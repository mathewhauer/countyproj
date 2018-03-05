###   PREAMBLE   ###

startup = function(){
  
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
  
  rm(packages)
  library(tidyverse)
  library(devtools)
  # install_github('nathanvan/parallelsugar')
  
  start_time <-Sys.time()
  
  set.seed(100)
}
startup()
rm(list=ls())
#rm(list = ls()[!ls() %in% c("cendat", "K05_pop")])

launch_year = 2016
SIZE<-18
# NUMBER OF PROJECTION STEPS
STEPS<-10
FORLEN<-50
BASEANDSTEPS<-STEPS+1
cyc<-15
# SET THE PROJECTION INTERVALS PERCENTILE
setlevel <- 0.5
years <- 0
years$YEAR <- seq(launch_year+5,launch_year+(STEPS*5), 5)
years$YEAR <- seq(launch_year+1,launch_year+STEPS,1)



###  DATA DOWNLOAD/PREP   ###  
##  Downloading the list of FIPS codes from the Census bureua's website. ##
##  Also removing Guam, American Samoa, etc.  ##
fipslist <- read_csv(file="https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", col_names = FALSE) %>%
  mutate(GEOID = paste0(X2, X3)) %>%
  rename(state = X1,
         STATEID = X2,
         CNTYID = X3,
         NAME = X4) %>%
  filter(!STATEID %in% c("60", "66", "69", "72", "74", "78"))

# Converting the fipslist into a unique list of 2-digit state ID's #
stateid = unlist(list(unique(fipslist$STATEID)))
# Converting the fipslist into a unique list of 5-digit county ID's #
GEOID = unlist(list(unique(fipslist$GEOID)))

GROUPING <- c("STATE", "YEAR", "AGEGRP", "SEX")

# Importing the census base data population estimates for 1980 - 201X for all calculations
K05_pop <- read_csv("DATA/cendatbase.csv") %>%
  group_by(.dots = GROUPING) %>%
  summarise(population = sum(population))
K05_pop$GEOID <- paste0(K05_pop$STATE, "000")
K05_pop$countyrace <- paste0(K05_pop$GEOID, "_TOTAL")

# Importing the group quarters population from Census 2010.
gqpop <- read_csv("DATA/GQ2010.csv") %>%
  mutate(STATE = str_pad(STATE, 2, pad = "0"),
         COUNTY = str_pad(COUNTY, 3, pad="0"),
         GEOID = paste0(STATE, COUNTY)) %>%
  group_by(.dots = GROUPING) %>%
  summarise(group_quarters = sum(GQ, na.rm=T))
gqpop$GEOID <- paste0(gqpop$STATE, "000")
gqpop$countyrace <- paste0(gqpop$GEOID, "_TOTAL")

# Calculating the cohort-change ratios
CCRs<- K05_pop %>%
  ungroup() %>%
  mutate(AGEGRP = paste0("X", str_pad(AGEGRP, 2, pad ="0")),
         GEOID = paste0(STATE, "000")) %>%
  spread(AGEGRP, population) %>%
  arrange(GEOID, SEX, YEAR) %>%
  mutate(ccr01 = X02 / lag(X01, 5),
         ccr02 = X03 / lag(X02, 5),
         ccr03 = X04 / lag(X03, 5),
         ccr04 = X05 / lag(X04, 5),
         ccr05 = X06 / lag(X05, 5),
         ccr06 = X07 / lag(X06, 5),
         ccr07 = X08 / lag(X07, 5),
         ccr08 = X09 / lag(X08, 5),
         ccr09 = X10 / lag(X09, 5),
         ccr10 = X11 / lag(X10, 5),
         ccr11 = X12 / lag(X11, 5),
         ccr12 = X13 / lag(X12, 5),
         ccr13 = X14 / lag(X13, 5),
         ccr14 = X15 / lag(X14, 5),
         ccr15 = X16 / lag(X15, 5),
         ccr16 = X17 / lag(X16, 5),
         ccr17 = X18 / (lag(X17, 5) + lag(X18, 5))) %>%
  filter(YEAR >= min(YEAR +5) & YEAR <= launch_year) 
CCRs$GEOID <- paste0(CCRs$STATE, "000")
CCRs$countyrace <- paste0(CCRs$GEOID, "_TOTAL")

newborns <- K05_pop %>%
  filter(AGEGRP == 1) %>%
  group_by(STATE, YEAR)  %>%
  summarise(Newborns = sum(population))


##  THE WOMEN AGED 15-50 ARE SUBSET AND SUMMED. THEY ARE THEN JOINED WITH newborns
childbearing <-K05_pop %>%
  filter(AGEGRP %in% c(4,5,6,7,8,9,10),
         SEX == "FEMALE" ) %>%
  group_by(STATE, YEAR) %>%
  dplyr::summarise(Women1550 = sum(population)) %>%
  left_join(., newborns) %>%
  mutate(fertrat = Newborns/Women1550) %>%
  filter(YEAR >= min(YEAR+5) & YEAR <= launch_year)
childbearing$GEOID <- paste0(childbearing$STATE, "000")
childbearing$SEX <- "FEMALE"
childbearing$countyrace <- paste0(childbearing$GEOID, "_TOTAL")



samp <- unique(K05_pop$GEOID)
# this.county = "49033" # "38105" "12119
# samp <- "12000"
# x = this.county
x = unlist(list(paste0(samp, "_TOTAL")))

linmod = function(ccr, x, sex, df) {
  launchval=    df[[as.character(ccr)]][which(df$YEAR == launch_year & df$SEX == sex & df$countyrace ==x)]
  initpredval = predict(lm(as.formula(paste0(ccr, "~YEAR")), data=filter(df, countyrace == x)), years, interval = "prediction", level = setlevel)
  finpredval = (initpredval - launchval) + initpredval
  return(finpredval)
}

predccr = function(ccr, sex, x, DF){
  hyndtran = function(ccr, DF){((ccr-a)/2)+a}
  a <- quantile(DF[[as.character(ccr)]][which(DF$countyrace==x & DF$SEX == sex)], 0.5)
  #b <- max(DF[[as.character(ccr)]][which(DF$countyrace== x)])*1.01
  #a <- -0.00000000000001
  
  y <-as_data_frame(hyndtran(DF[[as.character(ccr)]][which(DF$countyrace== x & DF$SEX == sex)]))
  
  num<- seq(1,FORLEN,5)
  pred<- predict(ucm(value~0, data = y, level = TRUE, slope = FALSE)$model, n.ahead = FORLEN, interval = "prediction", level = setlevel)[c(num),]
  #pred2 <-(b-a)*exp(pred)/(1+exp(pred))+a
  return(pred)#, cycle = TRUE, cycle.period = cyc
}

predcwr = function(ccr, sex, x, DF){
  hyndtran = function(ccr,DF){log((ccr - a) / (b - ccr))}
  b <- max(DF[[as.character(ccr)]][which(DF$countyrace== x)])*1.01
  a <- 0
  y <-as_data_frame(hyndtran(DF[[as.character(ccr)]][which(DF$countyrace== x & DF$SEX == sex)]))
  
  num<- seq(1,FORLEN,5)
  pred<- predict(ucm(value~0, data = y, level = TRUE, slope = TRUE)$model, n.ahead = FORLEN, interval = "prediction", level = setlevel)[c(num),]
  pred2 <-(b-a)*exp(pred)/(1+exp(pred))+a
  return(pred2)#
}

z<-filter(CCRs, countyrace==x)

x<- x[1]
###   THE PROJECTION FUNCTION   ###
project = function(x){
  tryCatch({#print(this.county)
    #K05 <- filter(K05_pop , countyrace == x) 
    BA00m<- predccr("ccr01","MALE", x, CCRs)
    BA01m<- predccr("ccr02","MALE", x, CCRs)
    BA02m<- predccr("ccr03","MALE", x, CCRs)
    BA03m<- predccr("ccr04","MALE", x, CCRs)
    BA04m<- predccr("ccr05","MALE", x, CCRs)
    BA05m<- predccr("ccr06","MALE", x, CCRs)
    BA06m<- predccr("ccr07","MALE", x, CCRs)
    BA07m<- predccr("ccr08","MALE", x, CCRs)
    BA08m<- predccr("ccr09","MALE", x, CCRs)
    BA09m<- predccr("ccr10","MALE", x, CCRs)
    BA10m<- predccr("ccr11","MALE", x, CCRs)
    BA11m<- predccr("ccr12","MALE", x, CCRs)
    BA12m<- predccr("ccr13","MALE", x, CCRs)
    BA13m<- predccr("ccr14","MALE", x, CCRs)
    BA14m<- predccr("ccr15","MALE", x, CCRs)
    BA15m<- predccr("ccr16","MALE", x, CCRs)
    BA16m<- predccr("ccr17","MALE", x, CCRs)

    BA00f<- predccr("ccr01","FEMALE", x, CCRs)
    BA01f<- predccr("ccr02","FEMALE", x, CCRs)
    BA02f<- predccr("ccr03","FEMALE", x, CCRs)
    BA03f<- predccr("ccr04","FEMALE", x, CCRs)
    BA04f<- predccr("ccr05","FEMALE", x, CCRs)
    BA05f<- predccr("ccr06","FEMALE", x, CCRs)
    BA06f<- predccr("ccr07","FEMALE", x, CCRs)
    BA07f<- predccr("ccr08","FEMALE", x, CCRs)
    BA08f<- predccr("ccr09","FEMALE", x, CCRs)
    BA09f<- predccr("ccr10","FEMALE", x, CCRs)
    BA10f<- predccr("ccr11","FEMALE", x, CCRs)
    BA11f<- predccr("ccr12","FEMALE", x, CCRs)
    BA12f<- predccr("ccr13","FEMALE", x, CCRs)
    BA13f<- predccr("ccr14","FEMALE", x, CCRs)
    BA14f<- predccr("ccr15","FEMALE", x, CCRs)
    BA15f<- predccr("ccr16","FEMALE", x, CCRs)
    BA16f<- predccr("ccr17","FEMALE", x, CCRs)
    
    
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
    S10f<-S09f<-S08f<-S07f<-S06f<-S05f<-S04f<-S03f<-S02f<-S01f<-array(0,c(SIZE,SIZE,3))
    for (i in 1:3){
      S01f[,,i]<-rbind(0,cbind(diag(lx01f[,i]),0)) #setting the diagonal to the survival rate
      #S01f[18,18,i]=lx01f[17,i] ##setting the the open-ended interval survival rate
      S02f[,,i]<-rbind(0,cbind(diag(lx02f[,i]),0))
      #S02f[18,18,i]=lx02f[17,i]
      S03f[,,i]<-rbind(0,cbind(diag(lx03f[,i]),0))
      #S03f[18,18,i]=lx03f[17,i]
      S04f[,,i]<-rbind(0,cbind(diag(lx04f[,i]),0))
      #S04f[18,18,i]=lx04f[17,i]
      S05f[,,i]<-rbind(0,cbind(diag(lx05f[,i]),0))
      #S05f[18,18,i]=lx05f[17,i]
      S06f[,,i]<-rbind(0,cbind(diag(lx06f[,i]),0))
      #S06f[18,18,i]=lx06f[17,i]
      S07f[,,i]<-rbind(0,cbind(diag(lx07f[,i]),0))
      #S07f[18,18,i]=lx07f[17,i]
      S08f[,,i]<-rbind(0,cbind(diag(lx08f[,i]),0))
      #S08f[18,18,i]=lx08f[17,i]
      S09f[,,i]<-rbind(0,cbind(diag(lx09f[,i]),0))
      #S09f[18,18,i]=lx09f[17,i]
      S10f[,,i]<-rbind(0,cbind(diag(lx10f[,i]),0))
      #S10f[18,18,i]=lx10f[17,i]
    } 
    S10m<-S09m<-S08m<-S07m<-S06m<-S05m<-S04m<-S03m<-S02m<-S01m<-array(0,c(SIZE,SIZE,3))
    for (i in 1:3){
      S01m[,,i]<-rbind(0,cbind(diag(lx01m[,i]),0)) #setting the diagonal to the survival rate
      #S01m[18,18,i]=lx01m[17,i] ##setting the the open-ended interval survival rate
      S02m[,,i]<-rbind(0,cbind(diag(lx02m[,i]),0))
      #S02m[18,18,i]=lx02m[17,i]
      S03m[,,i]<-rbind(0,cbind(diag(lx03m[,i]),0))
      #S03m[18,18,i]=lx03m[17,i]
      S04m[,,i]<-rbind(0,cbind(diag(lx04m[,i]),0))
      #S04m[18,18,i]=lx04m[17,i]
      S05m[,,i]<-rbind(0,cbind(diag(lx05m[,i]),0))
      #S05m[18,18,i]=lx05m[17,i]
      S06m[,,i]<-rbind(0,cbind(diag(lx06m[,i]),0))
      #S06m[18,18,i]=lx06m[17,i]
      S07m[,,i]<-rbind(0,cbind(diag(lx07m[,i]),0))
      #S07m[18,18,i]=lx07m[17,i]
      S08m[,,i]<-rbind(0,cbind(diag(lx08m[,i]),0))
      #S08m[18,18,i]=lx08m[17,i]
      S09m[,,i]<-rbind(0,cbind(diag(lx09m[,i]),0))
      #S09m[18,18,i]=lx09m[17,i]
      S10m[,,i]<-rbind(0,cbind(diag(lx10m[,i]),0))
      #S10m[18,18,i]=lx10m[17,i]
    } 
    
    popf <- K05_pop$population[which(K05_pop$YEAR == launch_year & K05_pop$SEX == "FEMALE" & K05_pop$countyrace == x)]
    gqf <- gqpop$group_quarters[which(gqpop$SEX == "FEMALE" & gqpop$countyrace == x)]
    popf <- popf - gqf
    Pbasef <-array(0,c(SIZE,1,3))
    for (i in 1:3){Pbasef[,,i] <- cbind(popf)}  
    
    popm <- K05_pop$population[which(K05_pop$YEAR == launch_year & K05_pop$SEX == "MALE" & K05_pop$countyrace == x)]
    gqm <- gqpop$group_quarters[which(gqpop$SEX == "MALE" & gqpop$countyrace == x)]
        popm <- popm - gqm
    Pbasem <-array(0,c(SIZE,1,3))
    for (i in 1:3){Pbasem[,,i] <- cbind(popm)} 
    
    n01<--array(0,c(BASEANDSTEPS,3))
    for(i in 1:3){
      n01 <- predcwr("fertrat", "FEMALE", x, childbearing)
      
      #n01<-predccr_ucm("fertrat", fertdat)
      # n01<-if (sign(coef(lm(fertrat ~ YEAR, data=fertdat))[2]) < 0){exp(predict(lm(log(fertrat) ~ YEAR, data=fertdat), years,interval = "prediction", level = setlevel))} else{predict(lm(fertrat ~ YEAR, data = fertdat), years,interval = "prediction", level = setlevel)}
      #n01<-predcwr_ucm("fertraty", fertdat)
      #n01 <-predict(ucm(formula = fertrat~0, data = fertdat, level = TRUE, slope = TRUE)$model, n.ahead = BASEANDSTEPS, interval = "prediction", level= setlevel)
    }
    
    p10m<-p09m<-p08m<-p07m<-p06m<-p05m<-p04m<-p03m<-p02m<-p01m<-p10f<-p09f<-p08f<-p07f<-p06f<-p05f<-p04f<-p03f<-p02f<-p01f<-array(0,c(SIZE,1,3), dimnames = list(
      c("a1", "a2", "a3", "a4", "a5","a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16", "a17", "a18")
    )) 
    
    for (i in 1:3){
      p01f[,,i]   <- (S01f[,,i] %*% Pbasef[,,i])
      p01m[,,i]   <- (S01m[,,i] %*% Pbasem[,,i]) 
      p01f[1,1,i] <- (n01[1,i] * sum(p01f[4:10,,i]))*.487
      p01m[1,1,i] <- (n01[1,i] * sum(p01f[4:10,,i]))*.512
      
      p02f[,,i]   <- (S02f[,,i] %*% p01f[,,i])
      p02m[,,i]   <- (S02m[,,i] %*% p01m[,,i]) 
      p02f[1,1,i] <- (n01[1,i] * sum(p02f[4:10,,i]))*.487
      p02m[1,1,i] <- (n01[1,i] * sum(p02f[4:10,,i]))*.512
      
      p03f[,,i]   <- (S03f[,,i] %*% p02f[,,i])
      p03m[,,i]   <- (S03m[,,i] %*% p02m[,,i]) 
      p03f[1,1,i] <- (n01[1,i] * sum(p03f[4:10,,i]))*.487
      p03m[1,1,i] <- (n01[1,i] * sum(p03f[4:10,,i]))*.512
      
      p04f[,,i]   <- (S04f[,,i] %*% p03f[,,i])
      p04m[,,i]   <- (S04m[,,i] %*% p03m[,,i]) 
      p04f[1,1,i] <- (n01[1,i] * sum(p04f[4:10,,i]))*.487
      p04m[1,1,i] <- (n01[1,i] * sum(p04f[4:10,,i]))*.512
      
      p05f[,,i]   <- (S05f[,,i] %*% p04f[,,i])
      p05m[,,i]   <- (S05m[,,i] %*% p04m[,,i]) 
      p05f[1,1,i] <- (n01[1,i] * sum(p05f[4:10,,i]))*.487
      p05m[1,1,i] <- (n01[1,i] * sum(p05f[4:10,,i]))*.512
      
      p06f[,,i]   <- (S06f[,,i] %*% p05f[,,i])
      p06m[,,i]   <- (S06m[,,i] %*% p05m[,,i]) 
      p06f[1,1,i] <- (n01[1,i] * sum(p06f[4:10,,i]))*.487
      p06m[1,1,i] <- (n01[1,i] * sum(p06f[4:10,,i]))*.512
      
      p07f[,,i]   <- (S07f[,,i] %*% p06f[,,i])
      p07m[,,i]   <- (S07m[,,i] %*% p06m[,,i]) 
      p07f[1,1,i] <- (n01[1,i] * sum(p07f[4:10,,i]))*.487
      p07m[1,1,i] <- (n01[1,i] * sum(p07f[4:10,,i]))*.512
      
      p08f[,,i]   <- (S08f[,,i] %*% p07f[,,i])
      p08m[,,i]   <- (S08m[,,i] %*% p07m[,,i]) 
      p08f[1,1,i] <- (n01[1,i] * sum(p08f[4:10,,i]))*.487
      p08m[1,1,i] <- (n01[1,i] * sum(p08f[4:10,,i]))*.512
      
      p09f[,,i]   <- (S09f[,,i] %*% p08f[,,i])
      p09m[,,i]   <- (S09m[,,i] %*% p08m[,,i]) 
      p09f[1,1,i] <- (n01[1,i] * sum(p09f[4:10,,i]))*.487
      p09m[1,1,i] <- (n01[1,i] * sum(p09f[4:10,,i]))*.512
      
      p10f[,,i]   <- (S10f[,,i] %*% p09f[,,i])
      p10m[,,i]   <- (S10m[,,i] %*% p09m[,,i]) 
      p10f[1,1,i] <- (n01[1,i] * sum(p10f[4:10,,i]))*.475
      p10m[1,1,i] <- (n01[1,i] * sum(p10f[4:10,,i]))*.525
    }
    
    projm1 <- as.data.frame.table(p01m + gqm)
    projm2 <- as.data.frame.table(p02m + gqm)
    projm3 <- as.data.frame.table(p03m + gqm)
    projm4 <- as.data.frame.table(p04m + gqm)
    projm5 <- as.data.frame.table(p05m + gqm)
    projm6 <- as.data.frame.table(p06m + gqm)
    projm7 <- as.data.frame.table(p07m + gqm)
    projm8 <- as.data.frame.table(p08m + gqm)
    projm9 <- as.data.frame.table(p09m + gqm)
    projm10 <- as.data.frame.table(p10m + gqm)
    
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
    projm$SEX = "MALE"
    
    projf1 <- as.data.frame.table(p01f + gqf)
    projf2 <- as.data.frame.table(p02f + gqf)
    projf3 <- as.data.frame.table(p03f + gqf)
    projf4 <- as.data.frame.table(p04f + gqf)
    projf5 <- as.data.frame.table(p05f + gqf)
    projf6 <- as.data.frame.table(p06f + gqf)
    projf7 <- as.data.frame.table(p07f + gqf)
    projf8 <- as.data.frame.table(p08f + gqf)
    projf9 <- as.data.frame.table(p09f + gqf)
    projf10 <- as.data.frame.table(p10f + gqf)
    
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
    projf$SEX = "FEMALE"
    
    proj0A<-proj0B<-proj0C<- data.table()
    proj0A = as_data_frame(K05_pop[which(K05_pop$countyrace == x & K05_pop$YEAR == launch_year),]) %>%
      ungroup() %>%
      mutate(Var1 = paste0("a", AGEGRP),
             Var2 = "A",
             Var3 = "A") %>%
      rename(Freq = population) %>%
      select(Var1, Var2, Var3, Freq, YEAR, SEX)
    
    proj0B = as_data_frame(K05_pop[which(K05_pop$countyrace == x & K05_pop$YEAR == launch_year),]) %>%
      ungroup() %>%
      mutate(Var1 = paste0("a", AGEGRP),
             Var2 = "A",
             Var3 = "B") %>%
      rename(Freq = population) %>%
      select(Var1, Var2, Var3, Freq, YEAR, SEX)
    
    proj0C <- as_data_frame(K05_pop[which(K05_pop$countyrace == x & K05_pop$YEAR == launch_year),]) %>%
      ungroup() %>%
      mutate(Var1 = paste0("a", AGEGRP),
             Var2 = "A",
             Var3 = "C") %>%
      rename(Freq = population) %>%
      select(Var1, Var2, Var3, Freq, YEAR, SEX)
    proj0 <- rbind(proj0A, proj0B) %>%
      rbind(., proj0C)
    
    proj <-rbind(projm, projf) %>%
      rbind(., proj0)
    proj$countyrace <-x
    
    return(proj)
  }
  , error=function(e){cat(x," ERROR :",conditionMessage(e), "\n")})
}

    
d= pblapply(x, project)

KT <- rbindlist(d)
(end_time <- Sys.time() - start_time)

# write_csv(KT, "PROJECTIONS/stateucmnoslopesquished_02082018.csv")

###   VISUALIZING THE OUTPUT
# Aggregating the base historical data
cendat2 <- K05_pop %>%
  group_by(GEOID, YEAR) %>%
  dplyr::summarise(population = sum(population)) %>%
  mutate(Var3 = "D")

# Performing some QOL data improvements from the projection output
Kpops <- KT %>%
  separate(countyrace, c("GEOID", "RACE"), sep = "_")%>%
  separate(Var1, c("drop", "AGEGRP"), sep = "a")

# Summing the age/sex projections to the total population
KTHD <- Kpops %>%
  #filter(Var3 == "A") %>%
  group_by(GEOID, Var3, YEAR) %>%
  #ungroup() %>%
  summarise(population = sum(Freq, na.rm=T))

# Binding the historic data with the summed age/sex projections
KTH <- rbind(cendat2, KTHD)

# Calculating the total historic/projected population of the US 
TOTAL <- KTH %>%
  ungroup() %>%
  mutate(STATE = substr(GEOID, 1,2)) %>%
  group_by(Var3, YEAR) %>%
  dplyr::summarise(population = sum(population, na.rm=T)) %>%
  mutate(GEOID = "00000") %>%
  ungroup() 

# Binding the historic projections with the US totals
KTH <- rbind(as_data_frame(KTH), as_data_frame(TOTAL))

# This chunk of code merges names with the GEOIDs
states <- fipslist %>%
  select(state, STATEID)
states <- unique(states) %>%
  mutate(GEOID = paste0(STATEID, "000"))

KTH <- left_join(KTH, states)
KTH$state <- ifelse(KTH$GEOID == "00000", "USA", KTH$state)

# Adding the US Geoid (00000) to the list.
samp[[length(samp)+1]] <- "00000"
samp<-samp[order(samp)]





 this.county <- samp
KTH_4 <- filter(KTH, GEOID == this.county) %>%
  spread(Var3, population)
# ggplot(data = KTH3, aes(x=YEAR)) +
#   geom_line(data = KTH3, aes(y=D),  color='gray') +
#   geom_ribbon(data = KTH3, aes(ymin = B, ymax = C), fill = "green", alpha =0.5, show.legend = FALSE) +
#   geom_line(data = KTH3, aes(y = A), lwd =1, color = "red") +
#   geom_line(data = KTH3, aes(y = D), lwd =1, color="Black") +
#   theme_bw() +
#   scale_y_continuous(labels=scales::comma,
#                      limits = c(min(KTH3$A, KTH3$B, KTH3$C, KTH3$D, na.rm=T)/2,max(KTH3$A, KTH3$B, KTH3$C, KTH3$D, na.rm=T)*1.5),
#                      expand = c(0,0)) +
#   scale_x_continuous(limits = c(1970,max(KTH3$YEAR)),
#                      expand = c(0, 0),
#                      breaks = c(1970, 1980, 1990, 2000, 2010, 2020, 2030, 2040, 2050, 2060, 2070)) +
#   geom_text(data = KTH3, aes(x = 1978, y = KTH3$A[which.max(KTH3$YEAR)]*1.02, label = paste0("50 percentile: ", format(round(KTH3$A[which.max(KTH3$YEAR)], 0), nsmall=0, big.mark=",")))) +
#   geom_text(data = KTH3, aes(x = 1978, y = KTH3$B[which.max(KTH3$YEAR)]*1.02, label = paste0("90 percentile: ", format(round(KTH3$B[which.max(KTH3$YEAR)], 0), nsmall=0, big.mark=",")))) +
#   geom_text(data = KTH3, aes(x = 1978, y = KTH3$C[which.max(KTH3$YEAR)]*1.02, label = paste0("10 percentile: ", format(round(KTH3$C[which.max(KTH3$YEAR)], 0), nsmall=0, big.mark=",")))) +
#   geom_text(data = KTH3, aes(x = 1978, y = filter(KTH3, YEAR == 2016)$D*1.3, label = paste0("Launch population: ", format(filter(KTH3, YEAR == 2016)$D, nsmall=0, big.mark=",")))) +
#   labs(x='Year',
#        y='Population',
#        title = paste0(this.county,': TOTAL POPULATION: HISTORICAL (BLACK LINE) \n WITH ', setlevel*100,'% CONFIDENCE INTERVALS (GREEN)'))



# PRODUCING A SET OF PROJECTION FIGURES
pdf(file = paste0("FIGURES/totpopstates_ucmnoslopesquished.pdf"), width=11, height=8.5)
for(this.county in unique(samp)){
  tryCatch({
    KTH3 <- filter(KTH, GEOID == this.county) %>%
      spread(Var3, population)
    #cendat3 <- filter(cendat2, GEOID == this.county)
    print(
      ggplot(data = KTH3, aes(x=YEAR)) +
        geom_line(data = KTH3, aes(y=D),  color='gray') +
        geom_ribbon(data = KTH3, aes(ymin = B, ymax = C), fill = "green", alpha =0.5, show.legend = FALSE) +
        geom_line(data = KTH3, aes(y = A), lwd =1, color = "red") +
        geom_line(data = KTH3, aes(y = D), lwd =1, color="Black") +
        theme_bw() +
        scale_y_continuous(label=scales::comma,
                           limits = c(min(KTH3$A, KTH3$B, KTH3$C, KTH3$D, na.rm=T)/2,max(KTH3$A, KTH3$B, KTH3$C, KTH3$D, na.rm=T)*1.5),
                           expand = c(0,0)) +
        scale_x_continuous(limits = c(1970,max(KTH3$YEAR)), 
                           expand = c(0, 0),
                           breaks = c(1970, 1980, 1990, 2000, 2010, 2020, 2030, 2040, 2050, 2060, 2070)) +
        geom_text(data = KTH3, aes(x = 1980, y = KTH3$A[which.max(KTH3$YEAR)]*1.02, label = paste0("50th: ", format(round(KTH3$A[which.max(KTH3$YEAR)], 0), nsmall=0, big.mark=",")))) +
        geom_text(data = KTH3, aes(x = 1980, y = KTH3$B[which.max(KTH3$YEAR)]*1.02, label = paste0("80th: ", format(round(KTH3$B[which.max(KTH3$YEAR)], 0), nsmall=0, big.mark=",")))) +
        geom_text(data = KTH3, aes(x = 1980, y = KTH3$C[which.max(KTH3$YEAR)]*1.02, label = paste0("20th: ", format(round(KTH3$C[which.max(KTH3$YEAR)], 0), nsmall=0, big.mark=",")))) +
        geom_text(data = KTH3, aes(x = 1980, y = KTH3$D[which(KTH3$YEAR==2016 & KTH3$D > 0)],  label = paste0("Launch: ", format(KTH3$D[which(KTH3$YEAR==2016 & KTH3$D > 0)], nsmall=0, big.mark=",")))) +
        
        #geom_text(data = KTH3, aes(x = 2010, y = filter(KTH3, YEAR == 2016)$D*1.30, label = paste0("Launch population: ", format(filter(KTH3, YEAR == 2016)$D, nsmall=0, big.mark=",")))) +
        
        labs(x='Year', 
             y='Population',
             title = paste0(KTH3$state,': TOTAL POPULATION: HISTORICAL (BLACK LINE) \n WITH ', setlevel*100,'% CONFIDENCE INTERVALS (GREEN)'))
    )
  }
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}

dev.off()