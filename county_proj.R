rm(list=ls())

gc(reset = TRUE) # Garbage Collection

#################### R Workspace Options ####################
options(scipen = 12) # Scientific Notation
options(digits = 6) # Specify Digits
options(java.parameters = "-Xmx1000m") # Increase Java Heap Size

######################################## Functions, Libraries, & Parallel Computing ########################################

#################### Functions ####################

########## Install and/or Load Packages ##########
packages <- function(x){
  
  x <- deparse(substitute(x))
  installed_packages <- as.character(installed.packages()[,1])
  
  if (length(intersect(x, installed_packages)) == 0){
    install.packages(pkgs = x, dependencies = TRUE, repos = "http://cran.r-project.org")
  }
  
  library(x, character.only = TRUE)
  rm(installed_packages) # Remove From Workspace
}

########## Specify Number of Digits (Forward) ##########
numb_digits_F <- function(x,y){
  numb_digits_F <- do.call("paste0", list(paste0(rep(0, y - nchar(x)), collapse = ""), x))
  numb_digits_F <- ifelse(nchar(x) < y, numb_digits_F, x)
}

########## Remove Double Space ##########
numb_spaces <- function(x) gsub("[[:space:]]{2,}", " ", x)

#################### Libraries ####################
packages(data.table) # Data Management/Manipulation
packages(doParallel) # Parallel Computing
packages(foreach) # Parallel Computing
packages(openxlsx) # Microsoft Excel Files
packages(plyr) # Data Management/Manipulation
packages(readxl) # Microsoft Excel Files
packages(reshape2) # Data Management/Manipulation
packages(stringi) # Character/String Editor
packages(stringr) # Character/String Editor
packages(zoo) # Time Series

packages(parallelsugar)
packages(tidyverse)
packages(scales)
packages(directlabels)
packages(data.table)
packages(tools)
packages(tidycensus)
packages(censusapi)

rm(packages)
library(tidyverse)
# library(devtools)
# install_github('nathanvan/parallelsugar')

start_time <-Sys.time()

set.seed(100)

key <- census_api_key("0206e3f2924a424be8722887fd0a49cea6308a7e")
key <- "0206e3f2924a424be8722887fd0a49cea6308a7e"


apis <- listCensusApis()

vintage_year = "2016" #<----------------------------------------------------------- INPUT VINTAGE ESTIMATE YEAR HERE
vintage_end = "9"     #<----------------------------------------------------------- INPUT VINTAGE DATE

list <- listCensusMetadata(name = "pep/charagegroups", vintage = "2015", type ="variables")
list <- makeVarlist(name = "pep/charagegroups", vintage = "2015", find = "total", output = "list")

# Gathering data for Census Years 2010-2016
e2010 <- getCensus(name="pep/charagegroups", 
                   vintage = vintage_year, 
                   key = key, 
                   vars =c("POP", "DATE", "SEX", "HISP", "RACE", "AGEGROUP", "GEONAME"), 
                   region="COUNTY",
                   regionin="state:10") %>% # State is set to Delaware
  # Creating the GEOID variable
  unite("GEOID", c("state", "county"), sep = "") %>%
  # Cleaning the data
  mutate(AGEGROUP = as.numeric(AGEGROUP),
         year = case_when(
           DATE == "9" ~ 2016,
           DATE == "8" ~ 2015,
           DATE == "7" ~ 2014,
           DATE == "6" ~ 2013,
           DATE == "5" ~ 2012,
           DATE == "4" ~ 2011,
           DATE == "3" ~ 2010),
         RACE = case_when(
           RACE == "0" | HISP == "0" ~ "TOTAL",
           RACE == "7" | HISP == "1" ~ "White, NH",
           RACE == "8" | HISP == "1" ~ "Black, NH",
           RACE %in% c("9","10","11") & HISP == "1" ~ "Other",
           HISP == 2 ~ "Other")) %>%
  filter(!is.na(RACE),
         !RACE == "TOTAL",
         !AGEGROUP == 0,
         !SEX == 0,
         DATE >= 3) %>%
  group_by(GEOID, GEONAME, year, RACE, AGEGROUP, SEX) %>%
  summarize(Population = sum(as.numeric(POP),na.rm = T)) %>%
  separate(GEONAME, c("county", "descript"), sep = " County") %>%
  ungroup() %>%
  #group_by(GEOID, RACE, SEX, AGEGROUP, year) %>%
  arrange(GEOID, RACE, SEX, AGEGROUP, year) %>%
  #mutate(pop.5 = lag(Population, 5)) %>%
  ungroup() %>%
  select(-descript)

#   e1990 <- getCensus(name="pep/int_charagegroups", 
#                  vintage = "1990", 
#                  key = key, 
#                  vars =c("POP", "YEAR", "RACE_SEX", "HISP", "AGEGRP"), 
#                  region="COUNTY"
#                  ,regionin="state:10") %>%
#     unite("GEOID", c("state", "county"), sep = "") %>%
#     mutate(AGEGROUP = as.numeric(AGEGRP),
#            year = case_when(
#              YEAR == "90" ~ 1990,
#              YEAR == "91" ~ 1991,
#              YEAR == "92" ~ 1992,
#              YEAR == "93" ~ 1993,
#              YEAR == "94" ~ 1994,
#              YEAR == "95" ~ 1995,
#              YEAR == "96" ~ 1996,
#              YEAR == "97" ~ 1997,
#              YEAR == "98" ~ 1998,
#              YEAR == "99" ~ 1999),
#            RACE = case_when(
#              RACE_SEX %in% c("01", "02") | HISP =="1" ~ "White, NH",
#              RACE_SEX %in% c("03", "04") | HISP == "1" ~ "Black, NH",
#              RACE_SEX %in% c("05", "06", "07", "08") | HISP %in% c("1", "2") ~ "Other"),
#            SEX = case_when(
#              RACE_SEX %in% c("01", "03", "05", "07") ~ "1",
#              RACE_SEX %in% c("02", "04", "06", "08") ~ "2")) %>%
#     filter(!is.na(RACE),
#            !RACE == "TOTAL",
#            !AGEGROUP == 0,
#            !SEX == 0) %>%
#     group_by(GEOID, year, RACE, AGEGROUP, SEX) %>%
#     summarize(Population = sum(as.numeric(POP),na.rm = T)) %>%
#     na.omit()
# e1990$county = "a"

e20001 <- getCensus(name="pep/int_charagegroups", 
                    vintage = "2000", 
                    key = key, 
                    vars =c("POP", "DATE", "SEX", "RACE", "AGEGROUP", "HISP", "GEONAME"), 
                    region = "county:1",
                    regionin = "state:10")
e20002 <- getCensus(name="pep/int_charagegroups", 
                    vintage = "2000", 
                    key = key, 
                    vars =c("POP", "DATE", "SEX", "RACE", "AGEGROUP", "HISP", "GEONAME"), 
                    region = "county:3",
                    regionin = "state:10")
e20003 <- getCensus(name="pep/int_charagegroups", 
                    vintage = "2000", 
                    key = key, 
                    vars =c("POP", "DATE", "SEX", "RACE", "AGEGROUP", "HISP", "GEONAME"), 
                    region = "county:5",
                    regionin = "state:10")
e2000 <-rbind(e20001, e20002) %>%
  rbind(., e20003) %>%
  unite("GEOID", c("state", "county"), sep = "") %>%
  mutate(AGEGROUP = as.numeric(AGEGROUP),
         year = case_when(
           DATE == "11" ~ 2009,
           DATE == "10" ~ 2008,
           DATE == "9" ~ 2007,
           DATE == "8" ~ 2006,
           DATE == "7" ~ 2005,
           DATE == "6" ~ 2004,
           DATE == "5" ~ 2003,
           DATE == "4" ~ 2002,
           DATE == "3" ~ 2001,
           DATE == "2" ~ 2000),
         RACE = case_when(
           RACE == "0" | HISP == "0" ~ "TOTAL",
           RACE == "1" | HISP == "1" ~ "White, NH",
           RACE == "2" | HISP == "1" ~ "Black, NH",
           RACE %in% c("9","10","11") & HISP == "1" ~ "Other",
           HISP == 2 ~ "Other")) %>%
  filter(!is.na(RACE),
         !RACE == "TOTAL",
         !AGEGROUP == 0,
         !SEX == 0) %>%
  group_by(GEOID, GEONAME, year, RACE, AGEGROUP, SEX) %>%
  summarize(Population = sum(as.numeric(POP),na.rm = T)) %>%
  separate(GEONAME, c("county", "descript"), sep = " County") %>%
  ungroup() %>%
  arrange(GEOID, RACE, SEX, AGEGROUP, year) %>%
  ungroup() %>%
  select(-descript) %>%
  na.omit

cendat <- rbind(e2010, e2000) %>%
  #bind_rows(., e1990) %>%
  mutate(countyrace = paste0(GEOID, "_", RACE))

K05_pop <- cendat %>%
  #mutate(AGEGROUP = if_else(AGEGROUP <18, AGEGROUP,17)) %>%
  #group_by(GEOID, year, RACE, AGEGROUP, SEX, countyrace) %>% 
  #summarize(Population = sum(as.numeric(Population),na.rm = T)) %>%
  #ungroup() %>%
  filter(year == 2016)

CCRs<- cendat %>%
  mutate(AGEGROUP = paste0("X", AGEGROUP)) %>%
  spread(AGEGROUP, Population) %>%
  arrange(GEOID,RACE, SEX,year) %>%
  mutate(ccr01 = X2 / lag(X1, 5),
         ccr02 = X3 / lag(X2, 5),
         ccr03 = X4 / lag(X3, 5),
         ccr04 = X5 / lag(X4, 5),
         ccr05 = X6 / lag(X5, 5),
         ccr06 = X7 / lag(X6, 5),
         ccr07 = X8 / lag(X7, 5),
         ccr08 = X9 / lag(X8, 5),
         ccr09 = X10 / lag(X9, 5),
         ccr10 = X11 / lag(X10, 5),
         ccr11 = X12 / lag(X11, 5),
         ccr12 = X13 / lag(X12, 5),
         ccr13 = X14 / lag(X13, 5),
         ccr14 = X15 / lag(X14, 5),
         ccr15 = X16 / lag(X15, 5),
         ccr16 = X17 / lag(X16, 5),
         ccr17 = X18 / (lag(X17, 5) + lag(X18, 5))) %>%
  filter(year >= 2005)

newborns <- cendat %>%
  ungroup() %>%
  filter(AGEGROUP == 1) %>%
  group_by(GEOID, countyrace, year, RACE)  %>%
  summarize(Newborns = sum(Population))

childbearing <-cendat %>%
  ungroup()%>%
  filter(AGEGROUP %in% c(1,3,4,5,6,7,8,9),
         SEX ==2 ) %>%
  group_by(GEOID, countyrace, year, RACE) %>%
  summarize(Women1550 = sum(Population)) %>%
  left_join(., newborns) %>%
  mutate(fertrat = Newborns/Women1550)


launch_year <- 2016
mylist<-unlist(list(unique(K05_pop$countyrace)))

ITER<-1000
##SIZE OF PROJECTION MATRIX
SIZE<-18
##NUMBER OF PROJECTION STEPS
STEPS<-10
BASEANDSTEPS<-STEPS+1
WINDOW <-20

# x = "10001_White, NH"
project = function(x){
  tryCatch({#print(this.county)
    print(x)
    K05 <- filter(K05_pop, countyrace == x)
    CCRm <- filter(CCRs, countyrace == x, SEX == 1)
    fertdat <- filter(childbearing, countyrace == x)
    BA16m<-BA15m<-BA14m<-BA13m<-BA12m<-BA11m<-BA10m<-BA09m<-BA08m<-BA07m<-BA06m<-BA05m<-BA04m<-BA03m<-BA02m<-BA01m<-BA00m<-array(0,c(BASEANDSTEPS,ITER))
    BA00m[1,]<-mean(filter(CCRm, year == (max(CCRm$year)))$ccr01, na.rm=T)
    BA01m[1,]<-mean(filter(CCRm, year == (max(CCRm$year)))$ccr02, na.rm=T)
    BA02m[1,]<-mean(filter(CCRm, year == (max(CCRm$year)))$ccr03, na.rm=T)
    BA03m[1,]<-mean(filter(CCRm, year == (max(CCRm$year)))$ccr04, na.rm=T)
    BA04m[1,]<-mean(filter(CCRm, year == (max(CCRm$year)))$ccr05, na.rm=T)
    BA05m[1,]<-mean(filter(CCRm, year == (max(CCRm$year)))$ccr06, na.rm=T)
    BA06m[1,]<-mean(filter(CCRm, year == (max(CCRm$year)))$ccr07, na.rm=T)
    BA07m[1,]<-mean(filter(CCRm, year == (max(CCRm$year)))$ccr08, na.rm=T)
    BA08m[1,]<-mean(filter(CCRm, year == (max(CCRm$year)))$ccr09, na.rm=T)
    BA09m[1,]<-mean(filter(CCRm, year == (max(CCRm$year)))$ccr10, na.rm=T)
    BA10m[1,]<-mean(filter(CCRm, year == (max(CCRm$year)))$ccr11, na.rm=T)
    BA11m[1,]<-mean(filter(CCRm, year == (max(CCRm$year)))$ccr12, na.rm=T)
    BA12m[1,]<-mean(filter(CCRm, year == (max(CCRm$year)))$ccr13, na.rm=T)
    BA13m[1,]<-mean(filter(CCRm, year == (max(CCRm$year)))$ccr14, na.rm=T)
    BA14m[1,]<-mean(filter(CCRm, year == (max(CCRm$year)))$ccr15, na.rm=T)
    BA15m[1,]<-mean(filter(CCRm, year == (max(CCRm$year)))$ccr16, na.rm=T)
    BA16m[1,]<-mean(filter(CCRm, year == (max(CCRm$year)))$ccr17, na.rm=T)
    
    for(i in 2:BASEANDSTEPS){
      BA00m[i,]<-BA00m[i-1,]+rnorm(ITER,0,sd(filter(CCRm, year >= (max(CCRm$year)-WINDOW))$ccr01, na.rm=T))
      BA01m[i,]<-BA01m[i-1,]+rnorm(ITER,0,sd(filter(CCRm, year >= (max(CCRm$year)-WINDOW))$ccr02, na.rm=T))
      BA02m[i,]<-BA02m[i-1,]+rnorm(ITER,0,sd(filter(CCRm, year >= (max(CCRm$year)-WINDOW))$ccr03, na.rm=T))
      BA03m[i,]<-BA03m[i-1,]+rnorm(ITER,0,sd(filter(CCRm, year >= (max(CCRm$year)-WINDOW))$ccr04, na.rm=T))
      BA04m[i,]<-BA04m[i-1,]+rnorm(ITER,0,sd(filter(CCRm, year >= (max(CCRm$year)-WINDOW))$ccr05, na.rm=T))
      BA05m[i,]<-BA05m[i-1,]+rnorm(ITER,0,sd(filter(CCRm, year >= (max(CCRm$year)-WINDOW))$ccr06, na.rm=T))
      BA06m[i,]<-BA06m[i-1,]+rnorm(ITER,0,sd(filter(CCRm, year >= (max(CCRm$year)-WINDOW))$ccr07, na.rm=T))
      BA07m[i,]<-BA07m[i-1,]+rnorm(ITER,0,sd(filter(CCRm, year >= (max(CCRm$year)-WINDOW))$ccr08, na.rm=T))
      BA08m[i,]<-BA08m[i-1,]+rnorm(ITER,0,sd(filter(CCRm, year >= (max(CCRm$year)-WINDOW))$ccr09, na.rm=T))
      BA09m[i,]<-BA09m[i-1,]+rnorm(ITER,0,sd(filter(CCRm, year >= (max(CCRm$year)-WINDOW))$ccr10, na.rm=T))
      BA10m[i,]<-BA10m[i-1,]+rnorm(ITER,0,sd(filter(CCRm, year >= (max(CCRm$year)-WINDOW))$ccr11, na.rm=T))
      BA11m[i,]<-BA11m[i-1,]+rnorm(ITER,0,sd(filter(CCRm, year >= (max(CCRm$year)-WINDOW))$ccr12, na.rm=T))
      BA12m[i,]<-BA12m[i-1,]+rnorm(ITER,0,sd(filter(CCRm, year >= (max(CCRm$year)-WINDOW))$ccr13, na.rm=T))
      BA13m[i,]<-BA13m[i-1,]+rnorm(ITER,0,sd(filter(CCRm, year >= (max(CCRm$year)-WINDOW))$ccr14, na.rm=T))
      BA14m[i,]<-BA14m[i-1,]+rnorm(ITER,0,sd(filter(CCRm, year >= (max(CCRm$year)-WINDOW))$ccr15, na.rm=T))
      BA15m[i,]<-BA15m[i-1,]+rnorm(ITER,0,sd(filter(CCRm, year >= (max(CCRm$year)-WINDOW))$ccr16, na.rm=T))
      BA16m[i,]<-BA16m[i-1,]+rnorm(ITER,0,sd(filter(CCRm, year >= (max(CCRm$year)-WINDOW))$ccr16, na.rm=T))
    }
    
    ###   Creating the diagonal for the survival rates
    lx01m<-rbind(BA00m[1,],BA01m[1,],BA02m[1,], BA03m[1,], BA04m[1,], BA05m[1,], BA06m[1,], BA07m[1,], BA08m[1,], BA09m[1,], BA10m[1,]
                 , BA11m[1,], BA12m[1,], BA13m[1,], BA14m[1,], BA15m[1,], BA16m[1,])
    lx02m<-rbind(BA00m[2,],BA01m[2,],BA02m[2,], BA03m[2,], BA04m[2,], BA05m[2,], BA06m[2,], BA07m[2,], BA08m[2,], BA09m[2,], BA10m[2,], BA11m[2,]
                 , BA12m[2,], BA13m[2,], BA14m[2,], BA15m[2,], BA16m[2,])
    lx03m<-rbind(BA00m[3,],BA01m[3,],BA02m[3,], BA03m[3,], BA04m[3,], BA05m[3,], BA06m[3,], BA07m[3,], BA08m[3,], BA09m[3,], BA10m[3,], BA11m[3,]
                 , BA12m[3,], BA13m[3,], BA14m[3,], BA15m[3,], BA16m[3,])
    lx04m<-rbind(BA00m[4,],BA01m[4,],BA02m[4,], BA03m[4,], BA04m[4,], BA05m[4,], BA06m[4,], BA07m[4,], BA08m[4,], BA09m[4,], BA10m[4,], BA11m[4,]
                 , BA12m[4,], BA13m[4,], BA14m[4,], BA15m[4,], BA16m[4,])
    lx05m<-rbind(BA00m[5,],BA01m[5,],BA02m[5,], BA03m[5,], BA04m[5,], BA05m[5,], BA06m[5,], BA07m[5,], BA08m[5,], BA09m[5,], BA10m[5,], BA11m[5,]
                 , BA12m[5,], BA13m[5,], BA14m[5,], BA15m[5,], BA16m[5,])
    lx06m<-rbind(BA00m[6,],BA01m[6,],BA02m[6,], BA03m[6,], BA04m[6,], BA05m[6,], BA06m[6,], BA07m[6,], BA08m[6,], BA09m[6,], BA10m[6,], BA11m[6,]
                 , BA12m[6,], BA13m[6,], BA14m[6,], BA15m[6,], BA16m[6,])
    lx07m<-rbind(BA00m[7,],BA01m[7,],BA02m[7,], BA03m[7,], BA04m[7,], BA05m[7,], BA06m[7,], BA07m[7,], BA08m[7,], BA09m[7,], BA10m[7,], BA11m[7,]
                 , BA12m[7,], BA13m[7,], BA14m[7,], BA15m[7,], BA16m[7,])
    lx08m<-rbind(BA00m[8,],BA01m[8,],BA02m[8,], BA03m[8,], BA04m[8,], BA05m[8,], BA06m[8,], BA07m[8,], BA08m[8,], BA09m[8,], BA10m[8,], BA11m[8,]
                 , BA12m[8,], BA13m[8,], BA14m[8,], BA15m[8,], BA16m[8,])
    lx09m<-rbind(BA00m[9,],BA01m[9,],BA02m[9,], BA03m[9,], BA04m[9,], BA05m[9,], BA06m[9,], BA07m[9,], BA08m[9,], BA09m[9,], BA10m[9,], BA11m[9,]
                 , BA12m[9,], BA13m[9,], BA14m[9,], BA15m[9,], BA16m[9,])
    lx10m<-rbind(BA00m[10,],BA01m[10,],BA02m[10,], BA03m[10,], BA04m[10,], BA05m[10,], BA06m[10,], BA07m[10,], BA08m[10,], BA09m[10,], BA10m[10,], BA11m[10,]
                 , BA12m[10,], BA13m[10,], BA14m[10,], BA15m[10,], BA16m[10,])
    
    ###   putting the survival rates in the diagonal of the leslise matrices  
    S10m<-S09m<-S08m<-S07m<-S06m<-S05m<-S04m<-S03m<-S02m<-S01m<-array(0,c(SIZE,SIZE,ITER))
    for (i in 1:ITER){
      S01m[,,i]<-rbind(0,cbind(diag(lx01m[,i]),0)) #setting the diagonal to the survival rate
      S01m[18,18,i]=lx01m[17,i] ##setting the the open-ended interval survival rate
      S02m[,,i]<-rbind(0,cbind(diag(lx02m[,i]),0))
      S02m[18,18,i]=lx02m[17,i]
      S03m[,,i]<-rbind(0,cbind(diag(lx03m[,i]),0))
      S03m[18,18,i]=lx03m[17,i]
      S04m[,,i]<-rbind(0,cbind(diag(lx04m[,i]),0))
      S04m[18,18,i]=lx04m[17,i]
      S05m[,,i]<-rbind(0,cbind(diag(lx05m[,i]),0))
      S05m[18,18,i]=lx05m[17,i]
      S06m[,,i]<-rbind(0,cbind(diag(lx06m[,i]),0))
      S06m[18,18,i]=lx06m[17,i]
      S07m[,,i]<-rbind(0,cbind(diag(lx07m[,i]),0))
      S07m[18,18,i]=lx07m[17,i]
      S08m[,,i]<-rbind(0,cbind(diag(lx08m[,i]),0))
      S08m[18,18,i]=lx08m[17,i]
      S09m[,,i]<-rbind(0,cbind(diag(lx09m[,i]),0))
      S09m[18,18,i]=lx09m[17,i]
      S10m[,,i]<-rbind(0,cbind(diag(lx10m[,i]),0))
      S10m[18,18,i]=lx10m[17,i]
    } 
    
    popm <- K05 %>%
      filter(SEX == 1)
    popm <- popm$Population
    Pbasem <-array(0,c(SIZE,1,ITER))
    for (i in 1:ITER){Pbasem[,,i] <- cbind(popm)}    
    
    CCRf <- filter(CCRs, countyrace == x, SEX == 1)
    BA16f<-BA15f<-BA14f<-BA13f<-BA12f<-BA11f<-BA10f<-BA09f<-BA08f<-BA07f<-BA06f<-BA05f<-BA04f<-BA03f<-BA02f<-BA01f<-BA00f<-array(0,c(BASEANDSTEPS,ITER))
    BA00f[1,]<-mean(filter(CCRf, year == (max(CCRf$year)))$ccr01, na.rm=T)
    BA01f[1,]<-mean(filter(CCRf, year == (max(CCRf$year)))$ccr02, na.rm=T)
    BA02f[1,]<-mean(filter(CCRf, year == (max(CCRf$year)))$ccr03, na.rm=T)
    BA03f[1,]<-mean(filter(CCRf, year == (max(CCRf$year)))$ccr04, na.rm=T)
    BA04f[1,]<-mean(filter(CCRf, year == (max(CCRf$year)))$ccr05, na.rm=T)
    BA05f[1,]<-mean(filter(CCRf, year == (max(CCRf$year)))$ccr06, na.rm=T)
    BA06f[1,]<-mean(filter(CCRf, year == (max(CCRf$year)))$ccr07, na.rm=T)
    BA07f[1,]<-mean(filter(CCRf, year == (max(CCRf$year)))$ccr08, na.rm=T)
    BA08f[1,]<-mean(filter(CCRf, year == (max(CCRf$year)))$ccr09, na.rm=T)
    BA09f[1,]<-mean(filter(CCRf, year == (max(CCRf$year)))$ccr10, na.rm=T)
    BA10f[1,]<-mean(filter(CCRf, year == (max(CCRf$year)))$ccr11, na.rm=T)
    BA11f[1,]<-mean(filter(CCRf, year == (max(CCRf$year)))$ccr12, na.rm=T)
    BA12f[1,]<-mean(filter(CCRf, year == (max(CCRf$year)))$ccr13, na.rm=T)
    BA13f[1,]<-mean(filter(CCRf, year == (max(CCRf$year)))$ccr14, na.rm=T)
    BA14f[1,]<-mean(filter(CCRf, year == (max(CCRf$year)))$ccr15, na.rm=T)
    BA15f[1,]<-mean(filter(CCRf, year == (max(CCRf$year)))$ccr16, na.rm=T)
    BA16f[1,]<-mean(filter(CCRf, year == (max(CCRf$year)))$ccr17, na.rm=T)
    
    for(i in 2:BASEANDSTEPS){
      BA00f[i,]<-BA00f[i-1,]+rnorm(ITER,0,sd(filter(CCRf, year >= (max(CCRf$year)-WINDOW))$ccr01, na.rm=T))
      BA01f[i,]<-BA01f[i-1,]+rnorm(ITER,0,sd(filter(CCRf, year >= (max(CCRf$year)-WINDOW))$ccr02, na.rm=T))
      BA02f[i,]<-BA02f[i-1,]+rnorm(ITER,0,sd(filter(CCRf, year >= (max(CCRf$year)-WINDOW))$ccr03, na.rm=T))
      BA03f[i,]<-BA03f[i-1,]+rnorm(ITER,0,sd(filter(CCRf, year >= (max(CCRf$year)-WINDOW))$ccr04, na.rm=T))
      BA04f[i,]<-BA04f[i-1,]+rnorm(ITER,0,sd(filter(CCRf, year >= (max(CCRf$year)-WINDOW))$ccr05, na.rm=T))
      BA05f[i,]<-BA05f[i-1,]+rnorm(ITER,0,sd(filter(CCRf, year >= (max(CCRf$year)-WINDOW))$ccr06, na.rm=T))
      BA06f[i,]<-BA06f[i-1,]+rnorm(ITER,0,sd(filter(CCRf, year >= (max(CCRf$year)-WINDOW))$ccr07, na.rm=T))
      BA07f[i,]<-BA07f[i-1,]+rnorm(ITER,0,sd(filter(CCRf, year >= (max(CCRf$year)-WINDOW))$ccr08, na.rm=T))
      BA08f[i,]<-BA08f[i-1,]+rnorm(ITER,0,sd(filter(CCRf, year >= (max(CCRf$year)-WINDOW))$ccr09, na.rm=T))
      BA09f[i,]<-BA09f[i-1,]+rnorm(ITER,0,sd(filter(CCRf, year >= (max(CCRf$year)-WINDOW))$ccr10, na.rm=T))
      BA10f[i,]<-BA10f[i-1,]+rnorm(ITER,0,sd(filter(CCRf, year >= (max(CCRf$year)-WINDOW))$ccr11, na.rm=T))
      BA11f[i,]<-BA11f[i-1,]+rnorm(ITER,0,sd(filter(CCRf, year >= (max(CCRf$year)-WINDOW))$ccr12, na.rm=T))
      BA12f[i,]<-BA12f[i-1,]+rnorm(ITER,0,sd(filter(CCRf, year >= (max(CCRf$year)-WINDOW))$ccr13, na.rm=T))
      BA13f[i,]<-BA13f[i-1,]+rnorm(ITER,0,sd(filter(CCRf, year >= (max(CCRf$year)-WINDOW))$ccr14, na.rm=T))
      BA14f[i,]<-BA14f[i-1,]+rnorm(ITER,0,sd(filter(CCRf, year >= (max(CCRf$year)-WINDOW))$ccr15, na.rm=T))
      BA15f[i,]<-BA15f[i-1,]+rnorm(ITER,0,sd(filter(CCRf, year >= (max(CCRf$year)-WINDOW))$ccr16, na.rm=T))
      BA16f[i,]<-BA16f[i-1,]+rnorm(ITER,0,sd(filter(CCRf, year >= (max(CCRf$year)-WINDOW))$ccr16, na.rm=T))
    }
    
    ###   Creating the diagonal for the survival rates
    lx01f<-rbind(BA00f[1,],BA01f[1,],BA02f[1,], BA03f[1,], BA04f[1,], BA05f[1,], BA06f[1,], BA07f[1,], BA08f[1,], BA09f[1,], BA10f[1,]
                 , BA11f[1,], BA12f[1,], BA13f[1,], BA14f[1,], BA15f[1,], BA16f[1,])
    lx02f<-rbind(BA00f[2,],BA01f[2,],BA02f[2,], BA03f[2,], BA04f[2,], BA05f[2,], BA06f[2,], BA07f[2,], BA08f[2,], BA09f[2,], BA10f[2,], BA11f[2,]
                 , BA12f[2,], BA13f[2,], BA14f[2,], BA15f[2,], BA16f[2,])
    lx03f<-rbind(BA00f[3,],BA01f[3,],BA02f[3,], BA03f[3,], BA04f[3,], BA05f[3,], BA06f[3,], BA07f[3,], BA08f[3,], BA09f[3,], BA10f[3,], BA11f[3,]
                 , BA12f[3,], BA13f[3,], BA14f[3,], BA15f[3,], BA16f[3,])
    lx04f<-rbind(BA00f[4,],BA01f[4,],BA02f[4,], BA03f[4,], BA04f[4,], BA05f[4,], BA06f[4,], BA07f[4,], BA08f[4,], BA09f[4,], BA10f[4,], BA11f[4,]
                 , BA12f[4,], BA13f[4,], BA14f[4,], BA15f[4,], BA16f[4,])
    lx05f<-rbind(BA00f[5,],BA01f[5,],BA02f[5,], BA03f[5,], BA04f[5,], BA05f[5,], BA06f[5,], BA07f[5,], BA08f[5,], BA09f[5,], BA10f[5,], BA11f[5,]
                 , BA12f[5,], BA13f[5,], BA14f[5,], BA15f[5,], BA16f[5,])
    lx06f<-rbind(BA00f[6,],BA01f[6,],BA02f[6,], BA03f[6,], BA04f[6,], BA05f[6,], BA06f[6,], BA07f[6,], BA08f[6,], BA09f[6,], BA10f[6,], BA11f[6,]
                 , BA12f[6,], BA13f[6,], BA14f[6,], BA15f[6,], BA16f[6,])
    lx07f<-rbind(BA00f[7,],BA01f[7,],BA02f[7,], BA03f[7,], BA04f[7,], BA05f[7,], BA06f[7,], BA07f[7,], BA08f[7,], BA09f[7,], BA10f[7,], BA11f[7,]
                 , BA12f[7,], BA13f[7,], BA14f[7,], BA15f[7,], BA16f[7,])
    lx08f<-rbind(BA00f[8,],BA01f[8,],BA02f[8,], BA03f[8,], BA04f[8,], BA05f[8,], BA06f[8,], BA07f[8,], BA08f[8,], BA09f[8,], BA10f[8,], BA11f[8,]
                 , BA12f[8,], BA13f[8,], BA14f[8,], BA15f[8,], BA16f[8,])
    lx09f<-rbind(BA00f[9,],BA01f[9,],BA02f[9,], BA03f[9,], BA04f[9,], BA05f[9,], BA06f[9,], BA07f[9,], BA08f[9,], BA09f[9,], BA10f[9,], BA11f[9,]
                 , BA12f[9,], BA13f[9,], BA14f[9,], BA15f[9,], BA16f[9,])
    lx10f<-rbind(BA00f[10,],BA01f[10,],BA02f[10,], BA03f[10,], BA04f[10,], BA05f[10,], BA06f[10,], BA07f[10,], BA08f[10,], BA09f[10,], BA10f[10,], BA11f[10,]
                 , BA12f[10,], BA13f[10,], BA14f[10,], BA15f[10,], BA16f[10,])
    
    ###   putting the survival rates in the diagonal of the leslise matrices  
    S10f<-S09f<-S08f<-S07f<-S06f<-S05f<-S04f<-S03f<-S02f<-S01f<-array(0,c(SIZE,SIZE,ITER))
    for (i in 1:ITER){
      S01f[,,i]<-rbind(0,cbind(diag(lx01f[,i]),0)) #setting the diagonal to the survival rate
      S01f[18,18,i]=lx01f[17,i] ##setting the the open-ended interval survival rate
      S02f[,,i]<-rbind(0,cbind(diag(lx02f[,i]),0))
      S02f[18,18,i]=lx02f[17,i]
      S03f[,,i]<-rbind(0,cbind(diag(lx03f[,i]),0))
      S03f[18,18,i]=lx03f[17,i]
      S04f[,,i]<-rbind(0,cbind(diag(lx04f[,i]),0))
      S04f[18,18,i]=lx04f[17,i]
      S05f[,,i]<-rbind(0,cbind(diag(lx05f[,i]),0))
      S05f[18,18,i]=lx05f[17,i]
      S06f[,,i]<-rbind(0,cbind(diag(lx06f[,i]),0))
      S06f[18,18,i]=lx06f[17,i]
      S07f[,,i]<-rbind(0,cbind(diag(lx07f[,i]),0))
      S07f[18,18,i]=lx07f[17,i]
      S08f[,,i]<-rbind(0,cbind(diag(lx08f[,i]),0))
      S08f[18,18,i]=lx08f[17,i]
      S09f[,,i]<-rbind(0,cbind(diag(lx09f[,i]),0))
      S09f[18,18,i]=lx09f[17,i]
      S10f[,,i]<-rbind(0,cbind(diag(lx10f[,i]),0))
      S10f[18,18,i]=lx10f[17,i]
    } 
    
    popf <- K05 %>%
      filter(SEX == 2)
    popf <- popf$Population
    Pbasef <-array(0,c(SIZE,1,ITER))
    for (i in 1:ITER){Pbasef[,,i] <- cbind(popf)}    
    
    n01<-array(0,c(1,ITER))
    for(i in 1:ITER){n01[i]<-mean(fertdat$fertrat, na.rm=T)
    n01[i]<-n01[i]+rnorm(ITER,0,sd(fertdat$fertrat, na.rm=T))
    }
    
    
    p10m<-p09m<-p08m<-p07m<-p06m<-p05m<-p04m<-p03m<-p02m<-p01m<-p10f<-p09f<-p08f<-p07f<-p06f<-p05f<-p04f<-p03f<-p02f<-p01f<-array(0,c(SIZE,1,ITER), dimnames = list(
      c("a00", "a05", "a10", "a15", "a20","a25", "a30", "a35", "a40", "a45", "a50", "a55", "a60", "a65", "a70", "a75", "a80", "a85")
    )) 
    
    for (i in 1:ITER){
      p01f[,,i]   <- (S01f[,,i] %*% Pbasef[,,i])
      p01m[,,i]   <- (S01m[,,i] %*% Pbasem[,,i]) 
      p01f[1,1,i] <- (n01[1,i] * sum(p01f[3:9,,i]))*.475
      p01m[1,1,i] <- (n01[1,i] * sum(p01f[3:9,,i]))*.525
      
      p02f[,,i]   <- (S02f[,,i] %*% p01m[,,i])
      p02m[,,i]   <- (S02m[,,i] %*% p01m[,,i]) 
      p02f[1,1,i] <- (n01[1,i] * sum(p02f[3:9,,i]))*.475
      p02m[1,1,i] <- (n01[1,i] * sum(p02f[3:9,,i]))*.525
      
      p03f[,,i]   <- (S03f[,,i] %*% p02m[,,i])
      p03m[,,i]   <- (S03m[,,i] %*% p02m[,,i]) 
      p03f[1,1,i] <- (n01[1,i] * sum(p03f[3:9,,i]))*.475
      p03m[1,1,i] <- (n01[1,i] * sum(p03f[3:9,,i]))*.525
      
      p04f[,,i]   <- (S04f[,,i] %*% p03m[,,i])
      p04m[,,i]   <- (S04m[,,i] %*% p03m[,,i]) 
      p04f[1,1,i] <- (n01[1,i] * sum(p04f[3:9,,i]))*.475
      p04m[1,1,i] <- (n01[1,i] * sum(p04f[3:9,,i]))*.525
      
      p05f[,,i]   <- (S05f[,,i] %*% p04m[,,i])
      p05m[,,i]   <- (S05m[,,i] %*% p04m[,,i]) 
      p05f[1,1,i] <- (n01[1,i] * sum(p05f[3:9,,i]))*.475
      p05m[1,1,i] <- (n01[1,i] * sum(p05f[3:9,,i]))*.525
      
      p06f[,,i]   <- (S06f[,,i] %*% p05m[,,i])
      p06m[,,i]   <- (S06m[,,i] %*% p05m[,,i]) 
      p06f[1,1,i] <- (n01[1,i] * sum(p06f[3:9,,i]))*.475
      p06m[1,1,i] <- (n01[1,i] * sum(p06f[3:9,,i]))*.525
      
      p07f[,,i]   <- (S07f[,,i] %*% p06m[,,i])
      p07m[,,i]   <- (S07m[,,i] %*% p06m[,,i]) 
      p07f[1,1,i] <- (n01[1,i] * sum(p07f[3:9,,i]))*.475
      p07m[1,1,i] <- (n01[1,i] * sum(p07f[3:9,,i]))*.525
      
      p08f[,,i]   <- (S08f[,,i] %*% p07m[,,i])
      p08m[,,i]   <- (S08m[,,i] %*% p07m[,,i]) 
      p08f[1,1,i] <- (n01[1,i] * sum(p08f[3:9,,i]))*.475
      p08m[1,1,i] <- (n01[1,i] * sum(p08f[3:9,,i]))*.525
      
      p09f[,,i]   <- (S09f[,,i] %*% p08m[,,i])
      p09m[,,i]   <- (S09m[,,i] %*% p08m[,,i]) 
      p09f[1,1,i] <- (n01[1,i] * sum(p09f[3:9,,i]))*.475
      p09m[1,1,i] <- (n01[1,i] * sum(p09f[3:9,,i]))*.525
      
      p10f[,,i]   <- (S10f[,,i] %*% p09m[,,i])
      p10m[,,i]   <- (S10m[,,i] %*% p09m[,,i]) 
      p10f[1,1,i] <- (n01[1,i] * sum(p10f[3:9,,i]))*.475
      p10m[1,1,i] <- (n01[1,i] * sum(p10f[3:9,,i]))*.525
    }
    
    KT10<-KT09<-KT08<-KT07<-KT06<-KT05<-KT04<-KT03<-KT02<-KT01<-KT00<-array(0,c(ITER)) 
    for (i in 1:ITER){
      KT00[i]<-sum(Pbasem[,,i]) + sum(Pbasef[,,i])
      KT01[i]<-sum(p01m[,,i]) + sum(p01f[,,i])
      KT02[i]<-sum(p02m[,,i]) + sum(p02f[,,i])
      KT03[i]<-sum(p03m[,,i]) + sum(p03f[,,i])
      KT04[i]<-sum(p04m[,,i]) + sum(p04f[,,i])
      KT05[i]<-sum(p05m[,,i]) + sum(p05f[,,i])
      KT06[i]<-sum(p06m[,,i]) + sum(p06f[,,i])
      KT07[i]<-sum(p07m[,,i]) + sum(p07f[,,i])
      KT08[i]<-sum(p08m[,,i]) + sum(p08f[,,i])
      KT09[i]<-sum(p09m[,,i]) + sum(p09f[,,i])
      KT10[i]<-sum(p10m[,,i]) + sum(p10f[,,i])
    }
    # zz <- gather(as.data.frame(p01m),  Iter, Population) %>%
    # rbind(., as.data.frame(p02m)) %>%
    #  rbind(., t(data.frame(p03m))) %>%
    #   rbind(., t(data.frame(p03m))) %>%
    #   rbind(., t(data.frame(p04m))) %>%
    #   rbind(., t(data.frame(p05m))) %>%
    #   rbind(., t(data.frame(p06m))) %>%
    #   rbind(., t(data.frame(p07m))) %>%
    #   rbind(., t(data.frame(p08m))) %>%
    #   rbind(., t(data.frame(p09m))) %>%
    #   rbind(., t(data.frame(p10m)))
    #   as.data.frame(zz$SEX = 1
    
    projm1 <- as.data.frame.table(p01m)
    projm2 <- as.data.frame.table(p02m)
    projm3 <- as.data.frame.table(p03m)
    projm4 <- as.data.frame.table(p04m)
    projm5 <- as.data.frame.table(p05m)
    projm6 <- as.data.frame.table(p06m)
    projm7 <- as.data.frame.table(p07m)
    projm8 <- as.data.frame.table(p08m)
    projm9 <- as.data.frame.table(p09m)
    projm10 <- as.data.frame.table(p10m)
    
    projm1$YEAR= launch_year+5
    projm2$YEAR= launch_year+10
    projm3$YEAR= launch_year+15
    projm4$YEAR= launch_year+20
    projm5$YEAR= launch_year+25
    projm6$YEAR= launch_year+30
    projm7$YEAR= launch_year+35
    projm8$YEAR= launch_year+40
    projm9$YEAR= launch_year+45
    projm10$YEAR= launch_year+50
    projmlist <- list(projm1, projm2, projm3, projm4, projm5, projm6, projm7, projm8, projm9, projm10)
    projm <- rbindlist(projmlist)
    projm$SEX = 1
    
    projf1 <- as.data.frame.table(p01f)
    projf2 <- as.data.frame.table(p02f)
    projf3 <- as.data.frame.table(p03f)
    projf4 <- as.data.frame.table(p04f)
    projf5 <- as.data.frame.table(p05f)
    projf6 <- as.data.frame.table(p06f)
    projf7 <- as.data.frame.table(p07f)
    projf8 <- as.data.frame.table(p08f)
    projf9 <- as.data.frame.table(p09f)
    projf10 <- as.data.frame.table(p10f)
    
    projf1$YEAR= launch_year+5
    projf2$YEAR= launch_year+10
    projf3$YEAR= launch_year+15
    projf4$YEAR= launch_year+20
    projf5$YEAR= launch_year+25
    projf6$YEAR= launch_year+30
    projf7$YEAR= launch_year+35
    projf8$YEAR= launch_year+40
    projf9$YEAR= launch_year+45
    projf10$YEAR= launch_year+50
    projflist <- list(projf1, projf2, projf3, projf4, projf5, projf6, projf7, projf8, projf9, projf10)
    projf <- rbindlist(projflist)
    projf$SEX = 2
    proj <-rbind(projm, projf)
    proj$countyrace <-x
    
    #z<- abind(p01m, p02m, p03m, p04m, p05m, p06m, p07m, p08m, p09m, p10m,along=2)
    
    # KTemp<-data.frame(array(c(KT00,KT01,KT02,KT03,KT04,KT05,KT06,KT07,KT08,KT09,KT10),c(ITER,BASEANDSTEPS)))
    #KTemp<- data.frame(array(c(p01m, p02m, p03m, p04m, p05m, p06m, p07m, p08m, p09m, p10m), c(ITER, BASEANDSTEPS)))
    #KTemp$countyrace <- x
    
    return(proj)
  }
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

d = mclapply(mylist, project, mc.cores = (detectCores() - 1))
KT <- rbindlist(d)
# end_time <- Sys.time()
# (time_elapsed <- (end_time - start_time))

cendat2 <-cendat %>%
  group_by(GEOID, year) %>%
  summarize(Population = sum(Population)) %>%
  mutate(Iter ="1") %>%
  rename(Year = year,
         county = GEOID)

Kpops <- KT %>%
  separate(countyrace, c("county", "Race"), sep = "_")%>%
  separate(Var1, c("drop", "agegrp"), sep = "a") %>%
  
  rename(Iter = Var3,
         Population = Freq) %>%
  select(-drop, Var2)


KTHA<-  setDT(KT2)[, Iter := sequence(.N), by = c("county", "Race")]
KTHB <- KTHA %>%
  mutate(county = trimws(county),
         Race = trimws(Race))

KTHD <- Kpops %>%
  # filter(county =="10001",
  #        Iter == "A", 
  #        YEAR == 2021)
  group_by(county, Iter, YEAR) %>%
  #ungroup() %>%
  summarise(Population = sum(Population, na.rm=T)) %>%
  rename(Year = YEAR)


this.county = "10005"

pdf(file = "totpop.pdf", width=11, height=8.5)  
for(this.county in unique(cendat$GEOID)){
  tryCatch({     
    KTH3 <- filter(KTHD, county == this.county) %>%
      group_by(Year) %>%
      summarize(Pop50 = quantile(as.numeric(Population), 0.5, na.rm=T),
                Pop10 = quantile(as.numeric(Population), 0.1, na.rm=T),
                Pop90 = quantile(as.numeric(Population), 0.9, na.rm=T))
    
    k_iters <- KTHD %>%
      filter(county == this.county) %>%
      subset(Iter %in% sample(levels(Iter), 20))
    
    
    
    
    #pdf(file = "totpop.pdf", width=11, height=8.5)
    print(
      ggplot(data = k_iters, aes(x=Year)) +
        geom_line( aes(y=Population, group = Iter),  color='gray') +
        geom_ribbon(data = KTH3, aes(ymin = Pop10, ymax= Pop90), fill="green", alpha=0.5, show.legend = FALSE) +
        geom_line(data = KTH3, aes(y = Pop50), lwd =1, color = "red") +
        geom_line(data = filter(cendat2, county == this.county), aes(y = Population), lwd =1, color="Black") +
        theme_bw() +
        scale_y_continuous(label=comma,
                           limits = c(min(KTH3$Pop10)/2,max(KTH3$Pop90)*1.5),
                           expand = c(0,0)) +
        scale_x_continuous(limits = c(1970,max(KTH3$Year)), 
                           expand = c(0, 0),
                           breaks = c(1970, 1980, 1990, 2000, 2010, 2020, 2030, 2040, 2050, 2060, 2070)) +
        geom_text(data = KTH3, aes(x = 1978, y = KTH3$Pop50[which.max(KTH3$Year)]*1.02, label = paste0("50 percentile: ", format(round(KTH3$Pop50[which.max(KTH3$Year)], 0), nsmall=0, big.mark=",")))) +
        geom_text(data = KTH3, aes(x = 1978, y = KTH3$Pop90[which.max(KTH3$Year)]*1.02, label = paste0("90 percentile: ", format(round(KTH3$Pop90[which.max(KTH3$Year)], 0), nsmall=0, big.mark=",")))) +
        geom_text(data = KTH3, aes(x = 1978, y = KTH3$Pop10[which.max(KTH3$Year)]*1.02, label = paste0("10 percentile: ", format(round(KTH3$Pop10[which.max(KTH3$Year)], 0), nsmall=0, big.mark=",")))) +
        labs(x='Year', 
             y='Population',
             title = paste0(this.county,': TOTAL POPULATION: HISTORICAL (BLACK) AND ',ITER , ' FORECASTS \n WITH 90% CONFIDENCE INTERVALS (GREEN)'))
    )
    # ggsave(file= paste("totpop_", this.county, ".pdf",sep=""),width=11, height=8.5)
    # dev.off()
    # Sys.sleep(5)
  }
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}

dev.off()

end_time <- Sys.time()
(time_elapsed <- (end_time - start_time))


