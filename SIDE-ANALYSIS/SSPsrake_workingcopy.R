

# SET THE GROUPING VARIABLES HERE
GROUPING <- c("STATE", "COUNTY", "YEAR", "AGE", "RACE", "SEX")

test_year = 2015
launch_year = 2015
SIZE<-18
# NUMBER OF PROJECTION STEPS
STEPS<-17
# Forecast length
FORLEN<-(STEPS*5)
BASEANDSTEPS<-STEPS+1
years <- 0
years$YEAR <- seq(launch_year+5,launch_year+(STEPS*5), 5)
years$YEAR <- seq(launch_year+1,launch_year+STEPS,1)

# temp <- tempfile()
# download.file("https://seer.cancer.gov/popdata/yr1969_2016.19ages/us.1969_2016.19ages.adjusted.txt.gz",temp)
# data <- read.table(unz(temp, "a1.dat"))
# unlink(temp)
# # 
# # temp <- tempfile()
# # download.file("http://cbio.mskcc.org/microrna_data/human_predictions_S_C_aug2010.txt.gz", temp)
# gzfile(temp, 'rt')
# data <- read.table(temp)
# # 
# data<- read.fwf(temp, widths = c(4,2,2,3,2,1,1,1,2,8))
# unlink(temp)

# Load the base data in. Unfortunately, the Bridged Race data reads in as a single variable.
K05_pop<- read.table("DATA/us.1990_2016.19ages.adjusted.txt") 
K05_pop$V1 <- as.character(K05_pop$V1) # setting the first variable as a character.
K05_pop$YEAR <- as.numeric(substr(K05_pop$V1,1,4)) # making the year
K05_pop$STATEID <- substr(K05_pop$V1, 5,6) # Making the state FIPS
K05_pop$STATE <- substr(K05_pop$V1, 7,8) # naming the state
K05_pop$COUNTY <- substr(K05_pop$V1,9,11) # setting the county FIPS
K05_pop$REGISTRY <- substr(K05_pop$V1, 12,12)
K05_pop$RACE <- substr(K05_pop$V1, 14,14) # setting the race code
# Racecodes are 1 = White, 2 = Black, 3 = Am Indian/Ak Nat, 4 = API. These are recoded to 1 = White, 2 = Black, 3 = Other.
K05_pop$RACE <- ifelse(K05_pop$RACE == "3", "4", K05_pop$RACE)
# Origin codes are 0 = Non-hispanic, 1 = Hispanic.
K05_pop$ORIGIN <- substr(K05_pop$V1, 15,15)
# Setting the Hispanic race as RACE == 3.
K05_pop$RACE <- ifelse(K05_pop$ORIGIN == "1", "3", K05_pop$RACE)
K05_pop$SEX <- substr(K05_pop$V1, 16,16) # setting the sex code
K05_pop$AGE <- as.numeric(if_else(substr(K05_pop$V1, 17, 18) == "00","01",substr(K05_pop$V1, 17, 18))) # setting the age groups
K05_pop$POPULATION <- as.numeric(substr(K05_pop$V1, 19, 30)) # lastly, setting the population numbers

K05_pop <- K05_pop %>%
  group_by(.dots = GROUPING) %>%
  dplyr::summarise(POPULATION = sum(POPULATION))
K05_pop$GEOID <- paste0(K05_pop$STATE, K05_pop$COUNTY)
K05_pop$COUNTYRACE <- paste0(K05_pop$GEOID, "_", K05_pop$RACE)


K05_launch <- K05_pop[which(K05_pop$YEAR == launch_year),] %>%
  group_by(STATE, COUNTY, GEOID, YEAR, RACE, SEX) %>%
  dplyr::summarise(POPULATION = sum(POPULATION)) %>%
  ungroup()


fipslist <- read_csv(file="https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", col_names = FALSE) %>%
  mutate(GEOID = paste0(X2, X3)) %>%
  dplyr::rename(state = X1,
                STATEID = X2,
                CNTYID = X3,
                NAME = X4) %>%
  filter(!STATEID %in% c("60", "66", "69", "72", "74", "78"))

# Converting the fipslist into a unique list of 2-digit state ID's #
stateid = unlist(list(unique(fipslist$STATEID)))
# Converting the fipslist into a unique list of 5-digit county ID's #
GEOID = unlist(list(unique(fipslist$GEOID)))

statenames <- group_by(fipslist, STATEID, state) %>%
  dplyr::summarise() %>%
  dplyr::rename(STATE = STATEID,
                STATENAM = state)
countynames <- group_by(fipslist, GEOID, NAME, state) %>%
  dplyr::summarise()


files <- paste0("PROJECTIONS/PROJ//", list.files(path = "./PROJECTIONS/PROJ/",pattern = ".csv"))
temp <- lapply(files, fread, sep=" ")
z <- rbindlist( temp ) %>%
  dplyr::rename(YEAR = V3,
                SEX = V4,
                COUNTYRACE = V5,
                TYPE = V6,
                AGE = V7,
                A = V8,
                B = V9,
                C = V10,
                Var1 = V2) %>%
  mutate(STATE= substr(COUNTYRACE, 1,2),
         COUNTY = substr(COUNTYRACE, 3,5),
         GEOID = paste0(STATE, COUNTY),
         A = if_else(A<0, 0, A),
         B = if_else(B<0, 0, B),
         C = if_else(C<0,0, C),
         RACE = substr(COUNTYRACE, 7,7))
z[is.na(z)] <-0
basesum <-  K05_launch[which( K05_launch$YEAR == launch_year),] %>%
  dplyr::select(STATE, COUNTY, GEOID, POPULATION, RACE)

addsum <- z[which(z$TYPE=="ADD" & z$YEAR == (launch_year+FORLEN)),] %>%
  group_by(STATE, COUNTY, GEOID, RACE, TYPE) %>%
  dplyr::summarise(A = sum(A))

addmult <- left_join(addsum, basesum) %>%
  mutate(COMBINED = if_else(A>= POPULATION, "ADD" ,"Mult")) %>%
  dplyr::select(STATE, COUNTY, GEOID, RACE, COMBINED)


basesum2 <-  K05_launch[which( K05_launch$YEAR == launch_year),] %>%
  dplyr::select(STATE, COUNTY, GEOID, POPULATION, RACE) %>%
  group_by(GEOID, RACE) %>%
  dplyr::summarise(poptot = sum(POPULATION))

combined<- left_join(z, addmult) %>%
  filter(TYPE == COMBINED) %>%
  mutate(TYPE = "ADDMULT") %>%
  dplyr::select(-COMBINED)

z2<- rbind(z, combined) %>%
  dplyr::select(-V1)
z2<-  left_join(as.data.frame(z2), as.data.frame(K05_launch2))
z2<- left_join(z2, countynames)
z2[is.na(z2)] <-0
z2<- filter(z2,
           !GEOID %in% c("02900", "04910", "15900", "35910", "36910", "51910", "51911","51911", "51913", "51914", "51915", "51916", "51918"))
z3 <- filter(z2,
             TYPE == "ADDMULT")

totals <- z3 %>%
  group_by(AGE, SEX, YEAR) %>%
  dplyr::summarise(poptot = sum(A)) 
totals2 <- left_join(z3, totals) %>%
  mutate(percentage = (A/poptot))


# unzip(zipfile='DATA/SspDb_country_data_2013-06-12.csv.zip', exdir = "DATA")

SSPs <- read_csv("DATA/SspDb_country_data_2013-06-12.csv") %>%
  filter(REGION == "USA",
         grepl("Population",VARIABLE)) %>%
  separate(VARIABLE, c("VARIABLE", "VARIABLE1", "VARIABLE2", "VARIABLE3", "VARIABLE4"), by ="|")

SSPs2 <- SSPs %>%
  dplyr::select(-`1950`:-`2010`, -`2105`:-`2150`) %>%
  mutate(SEX = case_when(
     VARIABLE1 == "Female" ~ 2,
     VARIABLE1 == "Male" ~ 1),
    AGE = case_when(
      VARIABLE2 == "Aged0" ~ 1,
      VARIABLE2 == "Aged5" ~ 2,
      VARIABLE2 == "Aged10" ~ 3,
      VARIABLE2 == "Aged15" ~ 4,
      VARIABLE2 == "Aged20" ~ 5,
      VARIABLE2 == "Aged25" ~ 6,
      VARIABLE2 == "Aged30" ~ 7,
      VARIABLE2 == "Aged35" ~ 8,
      VARIABLE2 == "Aged40" ~ 9,
      VARIABLE2 == "Aged45" ~ 10,
      VARIABLE2 == "Aged50" ~ 11,
      VARIABLE2 == "Aged55" ~ 12,
      VARIABLE2 == "Aged60" ~ 13,
      VARIABLE2 == "Aged65" ~ 14,
      VARIABLE2 == "Aged70" ~ 15,
      VARIABLE2 == "Aged75" ~ 16,
      VARIABLE2 == "Aged80" ~ 17,
      VARIABLE2 == "Aged85" ~ 18,
      VARIABLE2 == "Aged90" ~ 18,
      VARIABLE2 == "Aged95" ~ 18,
      VARIABLE2 == "Aged100" ~ 18),
    SSP = case_when(
      grepl("SSP1", SCENARIO) ~ "SSP1",
      grepl("SSP2", SCENARIO) ~ "SSP2",
      grepl("SSP3", SCENARIO) ~ "SSP3",
      grepl("SSP4", SCENARIO) ~ "SSP4",
      grepl("SSP5", SCENARIO) ~ "SSP5"
    )) %>%
  filter(is.na(VARIABLE4),
         !is.na(VARIABLE2)) %>%
  dplyr::select(-MODEL:-UNIT) %>%
  na.omit %>%
  gather(YEAR, Population, `2015`:`2100`) %>%
  group_by(SEX, AGE, SSP, YEAR) %>%
  dplyr::summarise(Population = sum(Population)) %>%
  ungroup() %>%
  spread(SSP, Population) %>%
  mutate(YEAR = as.integer(YEAR),
         SEX = as.character(SEX))
  

test <- left_join(totals2, SSPs2) %>%
  mutate(SSP1 = SSP1*percentage*1000000,
         SSP2 = SSP2*percentage*1000000,
         SSP3 = SSP3*percentage*1000000,
         SSP4 = SSP4*percentage*1000000,
         SSP5 = SSP5*percentage*1000000) %>%
  select(YEAR, SEX, STATE, COUNTY, GEOID, RACE, AGE, SSP1:SSP5)

test2 <- test %>%
  # left_join(., statenames) %>%
  group_by(YEAR, SEX, STATE, RACE, AGE) %>%
  dplyr::summarise(SSP1 = sum(SSP1),
                   SSP2 = sum(SSP2),
                   SSP3 = sum(SSP3),
                   SSP4 = sum(SSP4),
                   SSP5 = sum(SSP5),
                   n = length(unique(GEOID)))


# write_csv(test, "DATA-PROCESSED/SSP_asrc.csv")
# write_csv(test2, "DATA-PROCESSED/SSP_asrstate.csv")

this.county <- "AK"

pdf(file = "totpop.pdf", width=11, height=8.5)
for(this.county in unique(test2$STATENAM)){
  tryCatch({
    KTH3 <- filter(test2, STATENAM == this.county) 
    
    #pdf(file = "totpop.pdf", width=11, height=8.5)
    print(
      ggplot() +
        geom_line(data = KTH3, aes(y=SSP1, x = YEAR), color = "green", lwd=1.5) +
        geom_line(data = KTH3, aes(y=SSP2, x = YEAR), color = "blue", lwd=1.5) +
        geom_line(data = KTH3, aes(y=SSP3, x = YEAR), color = "brown", lwd=1.5) +
        geom_line(data = KTH3, aes(y=SSP4, x = YEAR), color = "orange", lwd=1.5) +
        geom_line(data = KTH3, aes(y=SSP5, x = YEAR), color = "deeppink", lwd=1.5) +
        theme_bw() +
        scale_y_continuous(label=comma,
                           limits = c(0,max(KTH3$SSP5)*1.1),
                           expand = c(0,0)) +
        scale_x_continuous(limits = c(2020,max(KTH3$YEAR)+1),
                           expand = c(0, 0),
                           breaks = c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) +
        geom_text(aes(x = 2025, y = KTH3$SSP1[which.max(KTH3$YEAR)]*1.02, label = paste0("SSP1: ", format(round(KTH3$SSP1[which.max(KTH3$YEAR)], 0), nsmall=0, big.mark=","))), color="green") +
        geom_text(aes(x = 2025, y = KTH3$SSP2[which.max(KTH3$YEAR)]*1.02, label = paste0("SSP2: ", format(round(KTH3$SSP2[which.max(KTH3$YEAR)], 0), nsmall=0, big.mark=","))), color = "blue") +
        geom_text(aes(x = 2025, y = KTH3$SSP3[which.max(KTH3$YEAR)]*1.02, label = paste0("SSP3: ", format(round(KTH3$SSP3[which.max(KTH3$YEAR)], 0), nsmall=0, big.mark=","))), color = "brown") +
        geom_text(aes(x = 2025, y = KTH3$SSP4[which.max(KTH3$YEAR)]*1.02, label = paste0("SSP4: ", format(round(KTH3$SSP4[which.max(KTH3$YEAR)], 0), nsmall=0, big.mark=","))), color = "orange") +
        geom_text(aes(x = 2025, y = KTH3$SSP5[which.max(KTH3$YEAR)]*1.02, label = paste0("SSP5: ", format(round(KTH3$SSP5[which.max(KTH3$YEAR)], 0), nsmall=0, big.mark=","))), color = "deeppink") +
        theme(plot.caption = element_text(hjust = 0)) +
        labs(x='Year',
             y='Population',
             title = paste0(this.county,': TOTAL POPULATION for the five Shared Socioeconomic Pathways'),
             caption = paste0("SSP1: Sustainability\n SSP2: Middle of the road\n SSP3: Regional rivalry\n SSP4: Inequality\n SSP5: Fossil-fuel development"))
    )
  }
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}

dev.off()