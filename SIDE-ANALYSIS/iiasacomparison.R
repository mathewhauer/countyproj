files <- paste0("PROJECTIONS/EVAL//", list.files(path = "./PROJECTIONS/EVAL/",pattern = ".csv"))
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
  select(STATE, COUNTY, GEOID, POPULATION)

addsum <- z[which(z$TYPE=="ADD" & z$YEAR == (launch_year+FORLEN)),] %>%
  group_by(STATE, COUNTY, GEOID, TYPE) %>%
  dplyr::summarise(A = sum(A))

addmult <- left_join(addsum, basesum) %>%
  mutate(COMBINED = if_else(A>= POPULATION, "ADD" ,"Mult")) %>%
  select(STATE, COUNTY, GEOID, COMBINED)



combined<- left_join(z, addmult) %>%
  filter(TYPE == COMBINED) %>%
  mutate(TYPE = "ADDMULT") %>%
  select(-COMBINED)

z<- rbind(z, combined) %>%
  select(-V1)
z<-  left_join(as.data.frame(z), as.data.frame(K05_launch2))
z<- left_join(z, countynames)
z[is.na(z)] <-0
z<- filter(z,
           !GEOID %in% c("02900", "04910", "15900", "35910", "36910", "51910", "51911","51911", "51913", "51914", "51915", "51916", "51918"),
           YEAR == 2020,
           TYPE== "ADDMULT") %>%
  select(-B, -C)


z2 <- z %>%
  group_by(GEOID, AGE, YEAR) %>%
  dplyr::summarize(Population= sum(A))

write.csv(z2, "DATA-PROCESSED/iiasacomparison.csv")





