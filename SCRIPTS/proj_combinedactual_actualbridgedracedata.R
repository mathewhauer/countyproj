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
packages(rucm)
packages(pbmcapply)
packages(pbapply)
packages(RCurl)
packages(mipfp)
packages(rucm)
packages(cowplot)

packages(tidycensus)
packages(tmap)
packages(tmaptools)
packages(tigris)
packages(censusapi)
packages(pbmcapply)

rm(packages)
#library(tidyverse)
#library(devtools)
# install_github('nathanvan/parallelsugar')

#start_time <-Sys.time()

set.seed(100)

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

# SET THE GROUPING VARIABLES HERE
GROUPING <- c("STATE", "COUNTY", "YEAR", "AGE", "RACE", "SEX")

test_year = 2015
launch_year = test_year
SIZE<-18
# NUMBER OF PROJECTION STEPS
STEPS<-17
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

K05_pop<- read.table("DATA/us.1990_2016.19ages.adjusted.txt") 
K05_pop$V1 <- as.character(K05_pop$V1)
K05_pop$YEAR <- as.numeric(substr(K05_pop$V1,1,4))
K05_pop$STATEID <- substr(K05_pop$V1, 5,6)
K05_pop$STATE <- substr(K05_pop$V1, 7,8)
K05_pop$COUNTY <- substr(K05_pop$V1,9,11)
K05_pop$REGISTRY <- substr(K05_pop$V1, 12,12)
K05_pop$RACE <- substr(K05_pop$V1, 14,14)
K05_pop$RACE <- ifelse(K05_pop$RACE == "3", "4", K05_pop$RACE)
K05_pop$ORIGIN <- substr(K05_pop$V1, 15,15)
K05_pop$RACE <- ifelse(K05_pop$ORIGIN == "1", "3", K05_pop$RACE)
K05_pop$SEX <- substr(K05_pop$V1, 16,16)
K05_pop$AGE <- as.numeric(if_else(substr(K05_pop$V1, 17, 18) == "00","01",substr(K05_pop$V1, 17, 18)))
K05_pop$POPULATION <- as.numeric(substr(K05_pop$V1, 19, 30))

K05_pop <- K05_pop %>%
  group_by(.dots = GROUPING) %>%
  dplyr::summarise(POPULATION = sum(POPULATION))
K05_pop$GEOID <- paste0(K05_pop$STATE, K05_pop$COUNTY)
K05_pop$COUNTYRACE <- paste0(K05_pop$GEOID, "_", K05_pop$RACE)

# K05_pop <- read_csv("DATA/cendatbase.csv") %>%
#   mutate(RACE = case_when(
#     RACE == "BLACK, NH" ~ "BLACK",
#     RACE == "OTHER" ~ "OTHER",
#     RACE == "WHITE, NH" ~ "WHITE",
#     RACE == "HISPANIC" ~ "WHITE",
#     RACE == "OTHER, NH" ~ "OTHER"),
#     AGE = AGEGRP) %>%
#   group_by(.dots = GROUPING) %>%
#   summarise(POPULATION = sum(POPULATION))
# K05_pop$GEOID <- paste0(K05_pop$STATE, K05_pop$COUNTY)
# K05_pop$COUNTYRACE <- paste0(K05_pop$GEOID, "_", K05_pop$RACE)

# GQ2010.CSV IS THE RESULT OF 
gqpop <- read.csv("DATA/gQ2010.csv") %>%
  mutate(STATE = str_pad(STATE, 2, pad = "0"),
         COUNTY = str_pad(COUNTY, 3, pad="0"),
         GEOID = paste0(STATE, COUNTY),
         AGE = AGEGRP,
         SEX = case_when(
           SEX == "MALE" ~ "1",
           SEX == "FEMALE" ~ "2"),
         RACE = case_when(
           RACE == "BLACK, NH" ~ "2",
           RACE == "OTHER" ~ "4",
           RACE == "WHITE, NH" ~ "1",
           RACE == "HISPANIC" ~ "3",
           RACE == "OTHER, NH" ~ "4")) %>%
   group_by(.dots = GROUPING) %>%
  dplyr::summarise(group_quarters = sum(GQ, na.rm=T))

gqpop$GEOID <- paste0(gqpop$STATE, gqpop$COUNTY)
gqpop$COUNTYRACE <- paste0(gqpop$GEOID, "_", gqpop$RACE)


samp <- unique(K05_pop$COUNTYRACE)
# samp <- unique(K05_pop$COUNTYRACE[which(K05_pop$STATE == 15)])
# samp <- "01001_1"
x = unlist(list(paste0(samp)))


project = function(x){
  tryCatch({#print(x)
    ###   Prediction of the CCR function
    predccr = function(ccr, sex, x, DF){
      y <- as_data_frame(DF[[as.character(ccr)]][which(DF$COUNTYRACE== x & DF$SEX == sex )])/10
      num<- seq(1,FORLEN,5)
      pred<- tryCatch(predict(ucm(value~0, data = y, level = TRUE, slope = FALSE)$model, n.ahead = FORLEN)[c(num),]*10
                      , error=function(e) array(0, c(STEPS)))
      return(pred)
    }
    ###   Prediction of the CWR function. Maximum is set to 1% more than the max value in the time series. Minimum is set to a TFR of 1.0
    predcwr = function(ccr, sex, x, DF){
      hyndtran = function(ccr,DF){log((ccr - a) / (b - ccr))}
      b <- max(DF[[as.character(ccr)]][which(DF$COUNTYRACE== x)])*1.01
      a <- -0.00000001
      y <-as_data_frame(hyndtran(DF[[as.character(ccr)]][which(DF$COUNTYRACE== x & DF$SEX == sex)]))
      
      num <- seq(1,FORLEN,5)
      pred<- tryCatch(round(predict(ucm(value~0, data = y, level = TRUE, slope = FALSE)$model, n.ahead = FORLEN)[c(num),],5)
                      , error=function(e) array(hyndtran(DF$fertrat[which.max(DF$YEAR)]), c(STEPS)))
      pred2 <-(b-a)*exp(pred)/(1+exp(pred))+a
      #pred2<-ifelse(is.na(pred2), DF$fertrat[which.max(DF$YEAR)], pred2)
      return(round(pred2,6))#
    }
    ###################
    ### DATA PREP
    ##################
    ### Filtering the Census data based on the county/race combination
    K05 <- K05_pop[which(K05_pop$COUNTYRACE == x),] %>%
      group_by(YEAR,  STATE, COUNTY, RACE, SEX, AGE, COUNTYRACE) %>%
      dplyr::summarise(POPULATION = sum(POPULATION)) %>%
      ungroup()
    ###   Creating the Child/woman ratios.
    newborns <- K05 %>%
      filter(AGE == 1) %>% # AGE 1 = newborns.
      group_by(STATE, COUNTYRACE, YEAR)  %>%
      dplyr::summarise(Newborns = sum(POPULATION))
    childbearing <- K05 %>%
      filter(AGE %in% c(4,5,6,7,8,9,10), # women ages 15-49
             SEX == "2" ) %>%
      group_by(STATE, COUNTYRACE, YEAR) %>%
      dplyr::summarise(Women1550 = sum(POPULATION)) %>%
      left_join(., newborns) %>%
      mutate(fertrat = Newborns/Women1550) %>%
      filter(YEAR <= test_year)
    
    childbearing$SEX <- "2"
    childbearing[mapply(is.infinite, childbearing)] <- NA
    childbearing[mapply(is.nan, childbearing)] <- NA
    childbearing[is.na(childbearing)] <-0
    #childbearing$COUNTYRACE <- paste0(childbearing$GEOID, "_", childbearing$RACE)  
    
    ### Calculating the cohort-change differences (CCDs)
    CCDs<- K05 %>%
      ungroup() %>%
      mutate(AGE = paste0("X", str_pad(AGE, 2, pad ="0")),
             GEOID = paste0(STATE, COUNTY),
             POPULATION = as.numeric(POPULATION)) %>%
      spread(AGE, POPULATION)
    if(is.null(CCDs$X01)){CCDs$X01=0}else{CCDs$X01=CCDs$X01}
    if(is.null(CCDs$X02)){CCDs$X02=0}else{CCDs$X02=CCDs$X02}
    if(is.null(CCDs$X03)){CCDs$X03=0}else{CCDs$X03=CCDs$X03}
    if(is.null(CCDs$X04)){CCDs$X04=0}else{CCDs$X04=CCDs$X04}
    if(is.null(CCDs$X05)){CCDs$X05=0}else{CCDs$X05=CCDs$X05}
    if(is.null(CCDs$X06)){CCDs$X06=0}else{CCDs$X06=CCDs$X06}
    if(is.null(CCDs$X07)){CCDs$X07=0}else{CCDs$X07=CCDs$X07}
    if(is.null(CCDs$X08)){CCDs$X08=0}else{CCDs$X08=CCDs$X08}
    if(is.null(CCDs$X09)){CCDs$X09=0}else{CCDs$X09=CCDs$X09}
    if(is.null(CCDs$X10)){CCDs$X10=0}else{CCDs$X10=CCDs$X10}
    if(is.null(CCDs$X11)){CCDs$X11=0}else{CCDs$X11=CCDs$X11}
    if(is.null(CCDs$X12)){CCDs$X12=0}else{CCDs$X12=CCDs$X12}
    if(is.null(CCDs$X13)){CCDs$X13=0}else{CCDs$X13=CCDs$X13}
    if(is.null(CCDs$X14)){CCDs$X14=0}else{CCDs$X14=CCDs$X14}
    if(is.null(CCDs$X15)){CCDs$X15=0}else{CCDs$X15=CCDs$X15}
    if(is.null(CCDs$X16)){CCDs$X16=0}else{CCDs$X16=CCDs$X16}
    if(is.null(CCDs$X17)){CCDs$X17=0}else{CCDs$X17=CCDs$X17}
    if(is.null(CCDs$X18)){CCDs$X18=0}else{CCDs$X18=CCDs$X18}
    CCDs<- CCDs %>%
      arrange(GEOID, SEX, YEAR) %>%
      mutate(ccr1 = X02 - lag(X01, 5),
             ccr2 = X03 - lag(X02, 5),
             ccr3 = X04 - lag(X03, 5),
             ccr4 = X05 - lag(X04, 5),
             ccr5 = X06 - lag(X05, 5),
             ccr6 = X07 - lag(X06, 5),
             ccr7 = X08 - lag(X07, 5),
             ccr8 = X09 - lag(X08, 5),
             ccr9 = X10 - lag(X09, 5),
             ccr10 = X11 - lag(X10, 5),
             ccr11 = X12 - lag(X11, 5),
             ccr12 = X13 - lag(X12, 5),
             ccr13 = X14 - lag(X13, 5),
             ccr14 = X15 - lag(X14, 5),
             ccr15 = X16 - lag(X15, 5),
             ccr16 = X17 - lag(X16, 5),
             ccr17 = X18 - (lag(X17, 5) + lag(X18, 5))) %>%
      filter(YEAR >= min(YEAR +5, na.rm=T) & YEAR <= test_year)
    #CCDs$COUNTYRACE <- paste0(CCDs$GEOID, "_TOTAL")
    
    ### Calculating the CCRs
    CCRs<- K05 %>%
      ungroup() %>%
      mutate(AGE = paste0("X", str_pad(AGE, 2, pad ="0")),
             GEOID = paste0(STATE, COUNTY),
             POPULATION = as.numeric(POPULATION)) %>%
      spread(AGE, POPULATION)
      if(is.null(CCRs$X01)){CCRs$X01=0}else{CCRs$X01=CCRs$X01}
    if(is.null(CCRs$X02)){CCRs$X02=0}else{CCRs$X02=CCRs$X02}
    if(is.null(CCRs$X03)){CCRs$X03=0}else{CCRs$X03=CCRs$X03}
    if(is.null(CCRs$X04)){CCRs$X04=0}else{CCRs$X04=CCRs$X04}
    if(is.null(CCRs$X05)){CCRs$X05=0}else{CCRs$X05=CCRs$X05}
    if(is.null(CCRs$X06)){CCRs$X06=0}else{CCRs$X06=CCRs$X06}
    if(is.null(CCRs$X07)){CCRs$X07=0}else{CCRs$X07=CCRs$X07}
    if(is.null(CCRs$X08)){CCRs$X08=0}else{CCRs$X08=CCRs$X08}
    if(is.null(CCRs$X09)){CCRs$X09=0}else{CCRs$X09=CCRs$X09}
    if(is.null(CCRs$X10)){CCRs$X10=0}else{CCRs$X10=CCRs$X10}
    if(is.null(CCRs$X11)){CCRs$X11=0}else{CCRs$X11=CCRs$X11}
    if(is.null(CCRs$X12)){CCRs$X12=0}else{CCRs$X12=CCRs$X12}
    if(is.null(CCRs$X13)){CCRs$X13=0}else{CCRs$X13=CCRs$X13}
    if(is.null(CCRs$X14)){CCRs$X14=0}else{CCRs$X14=CCRs$X14}
    if(is.null(CCRs$X15)){CCRs$X15=0}else{CCRs$X15=CCRs$X15}
    if(is.null(CCRs$X16)){CCRs$X16=0}else{CCRs$X16=CCRs$X16}
    if(is.null(CCRs$X17)){CCRs$X17=0}else{CCRs$X17=CCRs$X17}
    if(is.null(CCRs$X18)){CCRs$X18=0}else{CCRs$X18=CCRs$X18}
    CCRs<- CCRs %>%
      arrange(GEOID, SEX, YEAR) %>%
      mutate(ccr1 = X02 / lag(X01, 5),
             ccr2 = X03 / lag(X02, 5),
             ccr3 = X04 / lag(X03, 5),
             ccr4 = X05 / lag(X04, 5),
             ccr5 = X06 / lag(X05, 5),
             ccr6 = X07 / lag(X06, 5),
             ccr7 = X08 / lag(X07, 5),
             ccr8 = X09 / lag(X08, 5),
             ccr9 = X10 / lag(X09, 5),
             ccr10 = X11 / lag(X10, 5),
             ccr11 = X12 / lag(X11, 5),
             ccr12 = X13 / lag(X12, 5),
             ccr13 = X14 / lag(X13, 5),
             ccr14 = X15 / lag(X14, 5),
             ccr15 = X16 / lag(X15, 5),
             ccr16 = X17 / lag(X16, 5),
             ccr17 = X18 / (lag(X17, 5) + lag(X18, 5))) %>%
      filter(YEAR >= min(YEAR +5, na.rm=T) & YEAR <= test_year)
    #CCRs$COUNTYRACE <- paste0(CCRs$GEOID, "_TOTAL")
    CCRs[mapply(is.infinite, CCRs)] <- NA
    CCRs[mapply(is.nan, CCRs)] <- NA
    CCRs[is.na(CCRs)] <-0
    CCDs[mapply(is.infinite, CCDs)] <- NA
    CCDs[mapply(is.nan, CCDs)] <- NA
    CCDs[is.na(CCDs)] <-0
    ##################################################
    ### Start of the Additive projections
    ##################################################
    
    ###  Calculating the UCM's of the CCD's for each age/sex group. The confidence interval is set to 80% (1.28*SD) 
    for (i in 1:(SIZE-1)){
      data_tablef <- cbind(predccr(paste0("ccr",i), "2", x, CCDs),0,0)
      data_tablem <- cbind(predccr(paste0("ccr",i), "1", x, CCDs),0,0)
      errf <- sd(CCDs[[as.character(paste0("ccr",i))]][which(CCDs$SEX == "2")])*1.28
      errm <- sd(CCDs[[as.character(paste0("ccr",i))]][which(CCDs$SEX == "1")])*1.28
      data_tablef[,2]<- data_tablef[,1]- ifelse(is.na(errf),0, errf)
      data_tablef[,3]<- data_tablef[,1]+ ifelse(is.na(errf),0, errf)
      data_tablem[,2]<- data_tablem[,1]- ifelse(is.na(errm),0, errm)
      data_tablem[,3]<- data_tablem[,1]+ ifelse(is.na(errm),0, errm)
      assign(paste0("BA",i,"f"), data_tablef[1:STEPS,1:3])
      assign(paste0("BA",i,"m"), data_tablem[1:STEPS,1:3])
      rm(data_tablef, data_tablem, errf, errm)
    }
    ### "Stacking" the CCDs into a single vector with a high/medium/low  
    for (i in 1:STEPS){
      namm<-paste0("lx", i, "m")
      namf<-paste0("lx",i,"f")
      assign(namm, rbind(BA1m[i,],BA2m[i,], BA3m[i,], BA4m[i,], BA5m[i,], BA6m[i,], BA7m[i,], BA8m[i,], BA9m[i,], BA10m[i,]
                         , BA11m[i,], BA12m[i,], BA13m[i,], BA14m[i,], BA15m[i,], BA16m[i,], BA17m[i,]))
      assign(namf, rbind(BA1f[i,],BA2f[i,], BA3f[i,], BA4f[i,], BA5f[i,], BA6f[i,], BA7f[i,], BA8f[i,], BA9f[i,], BA10f[i,]
                         , BA11f[i,], BA12f[i,], BA13f[i,], BA14f[i,], BA15f[i,], BA16f[i,], BA17f[i,]))}
    ###   Placing the CCD's into the subdiagonal of a leslie matrix.
    for (i in 1:STEPS){
      data_tablef <- get(paste0("lx",i,"f"))
      weird_dataf <- array(0,c(SIZE,SIZE,ncol(data_tablef)))
      data_tablem <- get(paste0("lx",i,"m"))
      weird_datam <- array(0,c(SIZE,SIZE,ncol(data_tablem)))
      for(j in 1:ncol(data_tablef)){
        weird_dataf[,,j] <- rbind(0,cbind(diag(data_tablef[,j]),0))
        weird_datam[,,j] <- rbind(0,cbind(diag(data_tablem[,j]),0))
        assign(paste0("S",i,"m"), weird_datam)
        assign(paste0("S",i,"f"), weird_dataf)
      }
      rm(data_tablef)
      rm(weird_dataf)
    }
    ### Formatting the base POPULATION data as equal to the total POPULATION minus the group quaters.
    popf <- array(0, c(SIZE))
    for(i in 1:SIZE){    popf[i] <- ifelse(length(K05$POPULATION[which(K05$SEX == "2" & K05$YEAR == launch_year & K05$AGE == i)])==0,
                                            0,
                                            K05$POPULATION[which(K05$SEX == "2" & K05$YEAR == launch_year & K05$AGE == i)])}
    gqf <-  if (length(gqpop$group_quarters[which(gqpop$SEX == "2" & gqpop$COUNTYRACE == x)]) > 0){
      gqpop$group_quarters[which(gqpop$SEX == "2" & gqpop$COUNTYRACE == x)]} else {
        0}
    popf <- popf - gqf
    
    
    popm <- array(0, c(SIZE))
    for(i in 1:SIZE){popm[i] <- ifelse(length(K05$POPULATION[which(K05$SEX == "1" & K05$YEAR == launch_year & K05$AGE == i)])==0,
                                        0,
                                        K05$POPULATION[which(K05$SEX == "1" & K05$YEAR == launch_year & K05$AGE == i)])}
    gqm <- if (length(gqpop$group_quarters[which(gqpop$SEX == "1" & gqpop$COUNTYRACE == x)]) > 0){
      gqpop$group_quarters[which(gqpop$SEX == "1" & gqpop$COUNTYRACE == x)]} else {
        0}
    popm <- popm - gqm 
    p0f <-array(0,c(SIZE,SIZE,ncol(lx1f)))
    p0m <-array(0,c(SIZE,SIZE,ncol(lx1f)))
    for (i in 1:ncol(lx1f)){
      p0f[,,i] <-  rbind(0,cbind(diag(popf),0))[1:18,1:18]
      p0f[18,18,i] = popf[18]
      p0m[,,i] <-  rbind(0,cbind(diag(popm),0))[1:18,1:18]
      p0m[18,18,i] = popm[18]
    }  
    ### Calculating the forecasted CWR's from the UCMs. Confidence interval is set at 80% (1.28*SD) 
    #n01<-array(0,c(STEPS,ncol(lx1f)))
    n01 <- cbind(predcwr("fertrat", "2", x, childbearing),0,0)
    n01[,2]<- ifelse(is.na(n01[,1] - sd(childbearing$fertrat)*1.28), childbearing$fertrat[which.max(childbearing$YEAR)], n01[,1] - sd(childbearing$fertrat)*1.28)
    n01[,3]<- ifelse(is.na(n01[,1] + sd(childbearing$fertrat)*1.28), childbearing$fertrat[which.max(childbearing$YEAR)], n01[,1] + sd(childbearing$fertrat)*1.28)
    
    ### PROJECTION ITSELF ###
    
    # Actually projecting with the additive model
    for (i in 1:STEPS){
      data_tablef <- get(paste0("S",i,"f"))
      data_tablem <- get(paste0("S",i,"m"))
      projm<-projf <- array(0,c(SIZE,ncol(lx1f)), dimnames = list(
        c("a1", "a2", "a3", "a4", "a5","a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16", "a17", "a18")))
      popdatf<- get(paste0("p",i-1,"f"))
      popdatm<- get(paste0("p",i-1,"m"))
      for(j in 1:ncol(lx1f)){  
        projf[,j] <- rowSums(data_tablef[,,j] + popdatf[,,j])
        projm[,j] <- rowSums(data_tablem[,,j] + popdatm[,,j])
        projf[1,j] <- (n01[i,j] * sum(projf[4:10,j]))*.487
        projm[1,j] <- (n01[i,j] * sum(projf[4:10,j]))*.512
        popdatf[,,j] <-rbind(0,cbind(diag(projf[,j]),0))[1:18,1:18]
        popdatm[,,j] <-rbind(0,cbind(diag(projm[,j]),0))[1:18,1:18]
        popdatf[18,18,j] <- projf[18,j]
        popdatm[18,18,j] <- projm[18,j]
        assign(paste0("p",i,"f"), popdatf)
        assign(paste0("p",i,"m"), popdatm)
        assign(paste0("proj",i,"f"),projf)
        assign(paste0("proj",i,"m"), projm)
      }
      rm(data_tablef, data_tablem, projf, projm, popdatf, popdatm)
    }
    ### Collecting the additive projections together.
    projm<-NULL
    projf<-NULL
    for (i in 1:STEPS){
      data_tablem <- as.data.frame.table(get(paste0("proj",i,"m")) + gqm)
      data_tablem$YEAR <- launch_year+ (i*5)
      data_tablem$SEX <- "1"
      data_tablef <- as.data.frame.table(get(paste0("proj",i,"f")) + gqf)
      data_tablef$YEAR <- launch_year+ (i*5)
      data_tablef$SEX <- "2"
      projm <- rbind(projm, data_tablem)
      projf <-rbind(projf, data_tablef)
      namm<- get(paste0("proj",i,"m"))
      rm(data_tablem)
    }
    ### Declaring several variables
    projadd <-rbind(projm, projf) %>%
      dplyr::rename(Scenario = Var2)
    projadd$COUNTYRACE <-x
    projadd$TYPE<- "ADD"
    
    ######################################
    ### PROJECTING THE CCRs
    
    ### Calculating the CCR UCMs for each individual age group
    for (i in 1:(SIZE-1)){
      data_tablef <- cbind(predccr(paste0("ccr",i), "2", x, CCRs),0,0)
      data_tablem <- cbind(predccr(paste0("ccr",i), "1", x, CCRs),0,0)
      errf <- sd(CCRs[[as.character(paste0("ccr",i))]][which(CCRs$SEX == "2")])*1.28
      errm <- sd(CCRs[[as.character(paste0("ccr",i))]][which(CCRs$SEX == "1")])*1.28
      data_tablef[,2]<- data_tablef[,1]- ifelse(is.na(errf),0, errf)
      data_tablef[,3]<- data_tablef[,1]+ ifelse(is.na(errf),0, errf)
      data_tablem[,2]<- data_tablem[,1]- ifelse(is.na(errm),0, errm)
      data_tablem[,3]<- data_tablem[,1]+ ifelse(is.na(errm),0, errm)
      assign(paste0("BA",i,"f"), data_tablef[1:STEPS,1:3])
      assign(paste0("BA",i,"m"), data_tablem[1:STEPS,1:3])
      rm(data_tablef, data_tablem, errf, errm)
    }
    ### Stacking the forecasted CCRs into single vectors.
    for (i in 1:STEPS){
      namm<-paste0("lx", i, "m")
      namf<-paste0("lx",i,"f")
      assign(namm, rbind(BA1m[i,],BA2m[i,], BA3m[i,], BA4m[i,], BA5m[i,], BA6m[i,], BA7m[i,], BA8m[i,], BA9m[i,], BA10m[i,]
                         , BA11m[i,], BA12m[i,], BA13m[i,], BA14m[i,], BA15m[i,], BA16m[i,], BA17m[i,]))
      assign(namf, rbind(BA1f[i,],BA2f[i,], BA3f[i,], BA4f[i,], BA5f[i,], BA6f[i,], BA7f[i,], BA8f[i,], BA9f[i,], BA10f[i,]
                         , BA11f[i,], BA12f[i,], BA13f[i,], BA14f[i,], BA15f[i,], BA16f[i,], BA17f[i,]))}
    ### Setting the sub-diagonal of a leslie matrix as equal to the projected CCRs
    for (i in 1:STEPS){
      data_tablef <- get(paste0("lx",i,"f"))
      data_tablem <- get(paste0("lx",i,"m"))
      weird_dataf <- array(0,c(SIZE,SIZE,ncol(data_tablef)))
      weird_datam <- array(0,c(SIZE,SIZE,ncol(data_tablem)))
      for(j in 1:ncol(data_tablef)){  
        weird_dataf[,,j] <- rbind(0,cbind(diag(data_tablef[,j]),0))
        weird_dataf[18,18,j]=data_tablef[17,j]
        weird_datam[,,j] <- rbind(0,cbind(diag(data_tablem[,j]),0))
        weird_datam[18,18,j]=data_tablem[17,j]
        assign(paste0("S",i,"f"), weird_dataf)
        assign(paste0("S",i,"m"), weird_datam)
      }
      rm(data_tablef, data_tablem, weird_dataf, weird_datam)
    }
    ### Formatting the base POPULATION data.
    p0f <-array(0,c(SIZE,1,ncol(lx1f)))
    p0m <-array(0,c(SIZE,1,ncol(lx1f)))
    for (i in 1:ncol(lx1f)){
      p0f[,,i] <-  cbind(popf)
      p0m[,,i] <-  cbind(popm)
      
    }  
    ### PROJECTING THE CCRs
    for (i in 1:STEPS){
      data_tablef <- get(paste0("S",i,"f"))
      data_tablem <- get(paste0("S",i,"m"))
      projm<-projf <- array(0,c(SIZE,1,ncol(lx1f)), dimnames = list(
        c("a1", "a2", "a3", "a4", "a5","a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16", "a17", "a18")))
      popdatf<- get(paste0("p",i-1,"f"))
      popdatm<- get(paste0("p",i-1,"m"))
      for(j in 1:ncol(lx1f)){  
        projf[,,j] <- data_tablef[,,j] %*% popdatf[,,j]
        projm[,,j] <- data_tablem[,,j] %*%  popdatm[,,j]
        projf[1,,j] <- (n01[i,j] * sum(projf[4:10,,j]))*.487
        projm[1,,j] <- (n01[i,j] * sum(projf[4:10,,j]))*.512
        assign(paste0("p",i,"f"), projf)
        assign(paste0("p",i,"m"), projm)
        assign(paste0("proj",i,"f"), projf)
        assign(paste0("proj",i,"m"), projm)
      }
    }
    ### Collecting the projection results
    projm<-NULL
    projf<-NULL
    for (i in 1:STEPS){
      data_tablem <- as.data.frame.table(get(paste0("proj",i,"m")) + gqm)
      data_tablem$YEAR <- launch_year+ (i*5)
      data_tablem$SEX <- "1"
      data_tablef <- as.data.frame.table(get(paste0("proj",i,"f")) + gqf)
      data_tablef$YEAR <- launch_year+ (i*5)
      data_tablef$SEX <- "2"
      
      projm <- rbind(projm, data_tablem)
      projf <-rbind(projf, data_tablef)
      namm<- get(paste0("proj",i,"m"))
      rm(data_tablem)
    }
    
    projmult <-rbind(projm, projf) %>%
      select(-Var2) %>%
      dplyr::rename(Scenario = Var3)
    projmult$COUNTYRACE <-x
    projmult$TYPE<- "Mult"
    ##################################
    #   Creating a launch POPULATION vectors
    # proj0A<-proj0B<-proj0C<- data.table()
    # proj0A = as_data_frame(K05[which(K05$YEAR == launch_year),]) %>%
    #   ungroup() %>%
    #   mutate(Var1 = paste0("a", AGE),
    #          Scenario = "A") %>%
    #   rename(Freq = POPULATION) %>%
    #   dplyr::select(Var1, Scenario, Freq, YEAR, SEX)
    # 
    # proj0B = as_data_frame(K05[which(K05$YEAR == launch_year),]) %>%
    #   ungroup() %>%
    #   mutate(Var1 = paste0("a", AGE),
    #          Scenario = "B") %>%
    #   rename(Freq = POPULATION) %>%
    #   dplyr::select(Var1, Scenario, Freq, YEAR, SEX)
    # 
    # proj0C <- as_data_frame(K05[which(K05$YEAR == launch_year),]) %>%
    #   ungroup() %>%
    #   mutate(Var1 = paste0("a", AGE),
    #          Scenario = "C") %>%
    #   rename(Freq = POPULATION) %>%
    #   dplyr::select(Var1, Scenario, Freq, YEAR, SEX)
    # 
    # proj0 <- rbind(proj0A, proj0B) %>%
    #   rbind(., proj0C)
    # proj0$TYPE ="BASE"
    # proj0$COUNTYRACE = x
    ##########################
    # Collecting all projections together
    proj <-rbind(projadd, projmult) #%>%
    #   rbind(., proj0)
    #projadd$COUNTYRACE <-x
    #projadd$TYPE<- "ADD"
    # addsum<-sum(proj$Freq[which(proj$TYPE=="ADD" & proj$YEAR == (launch_year+FORLEN) & proj$Scenario == "A")])
    # basepop<-sum(proj$Freq[which(proj$TYPE=="BASE" & proj$YEAR == (launch_year) & proj$Scenario == "A")])
    # 
    # if (addsum >= basepop){
    #   proj2 <- subset(proj, TYPE %in% c("ADD", "BASE"))
    # }else{
    #     proj2 <- subset(proj, TYPE %in% c("MULT", "BASE"))}
    return(proj)
  }
  , error=function(e){cat(x," ERROR :",conditionMessage(e), "\n")})
}

this.state="10"

# d= pblapply(x, project)
for(this.state in stateid){
# samp <- 
x = unlist(list(unique(K05_pop$COUNTYRACE[which(K05_pop$STATE==this.state)])))
KT = rbindlist(pbmclapply(x, project, mc.cores = detectCores()-1))
KT2 <- KT %>%
  mutate(AGE = as.numeric(substr(Var1, 2,3))) %>%
  group_by(YEAR, COUNTYRACE, SEX, AGE) %>%
  spread(Scenario, Freq)
# z<- K05_pop[which(K05_pop$YEAR >= test_year & K05_pop$STATE == this.state),] %>%
#   group_by(.dots = GROUPING) %>%
#   summarise(POPULATION = sum(POPULATION))
# z$COUNTYRACE <- paste0(z$STATE, z$COUNTY, "_", z$RACE)
# z$TYPE = "BASE"
# z<- rbind(z, KT2) %>%
#   na.omit
write.table(KT2, paste0("PROJECTIONS/BRIDGEDRACE/COUNTYnoslope_addmult",this.state,".csv"))
}


KT2 <- KT %>%
  mutate(AGE = as.numeric(substr(Var1, 2,3))) %>%
  group_by(YEAR, COUNTYRACE, SEX, AGE) %>%
  spread(Scenario, Freq)

z<- filter(K05_pop, YEAR>test_year, STATE== this.state)  %>%
z<- K05_pop[which(K05_pop$YEAR > test_year & K05_pop$STATE == this.state),] %>%
  group_by(.dots = GROUPING) %>%
  summarise(POPULATION = sum(POPULATION))
z$COUNTYRACE <- paste0(z$STATE, z$COUNTY, "_", z$RACE)
z<- left_join(z, KT2) %>%
  na.omit

tot <- z %>%
  ungroup() %>%
  #mutate(RACE = substr(countyrace, 7,25)) %>%
  group_by(STATE, COUNTY, YEAR, RACE, TYPE) %>%
  #group_by(YEAR, TYPE) %>%
  summarise(POPULATION = sum(POPULATION),
            A = sum(A),
            B = sum(B),
            C = sum(C)) %>%
  mutate(FLAG1 = if_else(is.na((A/POPULATION)-1), 0,(A/POPULATION)-1),
         FLAG2 = if_else(POPULATION>=B & POPULATION<=C,1,0)) %>%
  ungroup()

eval_ucm_statetotal <- z %>%
  #mutate(RACE = substr(countyrace, 7,25))
  group_by(YEAR, TYPE) %>%
  summarise(POPULATION = sum(POPULATION),
            A = sum(A),
            B = sum(B),
            C = sum(C),
            num = length(A)) %>%
  mutate(FLAG1 = if_else(is.na((A/POPULATION)-1), 0,(A/POPULATION)-1),
         FLAG2 = if_else(POPULATION>=B & POPULATION<=C,1,0),
         in90percentile = FLAG2/num)

eval_ucm_cntytotal <- z %>%
  group_by(STATE, COUNTY, YEAR, TYPE) %>%
  summarise(POPULATION = sum(POPULATION),
            A = sum(A),
            B = sum(B),
            C = sum(C),
            num = length(A)) %>%
  mutate(FLAG1 = if_else(is.na((A/POPULATION)-1), 0,(A/POPULATION)-1),
         FLAG2 = if_else(POPULATION>=B & POPULATION<=C,1,0),
         in90percentile = FLAG2/num) %>%
  ungroup() %>%
  group_by(YEAR, TYPE) %>%
  summarise(MAPE = quantile(abs(FLAG1), 0.5),
            in80percentile = sum(FLAG2)/length(FLAG2),
            num = length(FLAG2))

eval_ucm_racetotal <- z %>%
  group_by(STATE, RACE, YEAR, TYPE) %>%
  summarise(POPULATION = sum(POPULATION),
            A = sum(A),
            B = sum(B),
            C = sum(C),
            num = length(A)) %>%
  mutate(FLAG1 = if_else(is.na((A/POPULATION)-1), 0,(A/POPULATION)-1),
         FLAG2 = if_else(POPULATION>=B & POPULATION<=C,1,0),
         in90percentile = FLAG2/num) %>%
  ungroup() %>%
  group_by(YEAR, RACE, TYPE) %>%
  summarise(MAPE = quantile(abs(FLAG1), 0.5),
            in80percentile = sum(FLAG2)/length(FLAG2),
            num = length(FLAG2))

eval_ucm_agetotal <- z %>%
  group_by(STATE, AGE, YEAR, TYPE) %>%
  summarise(POPULATION = sum(POPULATION),
            A = sum(A),
            B = sum(B),
            C = sum(C),
            num = length(A)) %>%
  mutate(FLAG1 = if_else(is.na((A/POPULATION)-1), 0,(A/POPULATION)-1),
         FLAG2 = if_else(POPULATION>=B & POPULATION<=C,1,0),
         in90percentile = FLAG2/num) %>%
  ungroup() %>%
  group_by(AGE, TYPE) %>%
  summarise(MAPE = quantile(abs(FLAG1), 0.5),
            in80percentile = sum(FLAG2)/length(FLAG2),
            num = length(FLAG2))

