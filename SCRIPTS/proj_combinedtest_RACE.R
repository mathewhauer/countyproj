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
}
startup()
rm(list=ls())
#rm(list = ls()[!ls() %in% c("cendat", "K05_pop")])

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

# SET THE GROUPING VARIABLES HERE
GROUPING <- c("STATE", "COUNTY", "YEAR", "AGEGRP", "RACE", "SEX")

test_year = 2000
launch_year = test_year
SIZE<-18
# NUMBER OF PROJECTION STEPS
STEPS<-3
FORLEN<-(STEPS*5)
BASEANDSTEPS<-STEPS+1
years <- 0
years$YEAR <- seq(launch_year+5,launch_year+(STEPS*5), 5)
years$YEAR <- seq(launch_year+1,launch_year+STEPS,1)

z<- K05_pop[which(K05_pop$RACE == "OTHER"),]
unique(K05_pop$RACE) [which(K05_pop$YEAR==1980)])

K05_pop <- read_csv("DATA/cendatbase.csv") %>%
  mutate(RACE = case_when(
    RACE == "BLACK, NH" ~ "BLACK",
    RACE == "OTHER" ~ "OTHER",
    RACE == "WHITE, NH" ~ "WHITE",
    RACE == "HISPANIC" ~ "OTHER",
    RACE == "OTHER, NH" ~ "OTHER")) %>%
  group_by(.dots = GROUPING) %>%
  summarise(population = sum(population)) 

K05_pop$GEOID <- paste0(K05_pop$STATE, K05_pop$COUNTY)
K05_pop$countyrace <- paste0(K05_pop$GEOID, "_", K05_pop$RACE)

gqpop <- read.csv("DATA/gq_2000.csv") %>%
  mutate(STATE = str_pad(STATE, 2, pad = "0"),
         COUNTY = str_pad(COUNTY, 3, pad="0"),
         GEOID = paste0(STATE, COUNTY),
         RACE = case_when(
           RACE == "BLACK, NH" ~ "BLACK",
           RACE == "OTHER" ~ "OTHER",
           RACE == "WHITE, NH" ~ "WHITE",
           RACE == "HISPANIC" ~ "OTHER",
           RACE == "OTHER, NH" ~ "OTHER")) %>%
  group_by(.dots = GROUPING) %>%
  summarise(group_quarters = sum(GQ, na.rm=T))

gqpop$GEOID <- paste0(gqpop$STATE, gqpop$COUNTY)
gqpop$countyrace <- paste0(gqpop$GEOID, "_", gqpop$RACE)

# samp <- filter(K05_pop, STATE == "13")
# samp <- unique(samp$countyrace)
# samp <- unique(K05_pop$countyrace[which(K05_pop$STATE == 12)])
samp <- unique(K05_pop$countyrace)
# samp <- "16049_BLACK" 

# this.county = "49033" # "38105" "12119 "38053"
# "02158" "36109" "13117" "48301"
# x = this.county
# x = unlist(list(paste0(samp, "_TOTAL")))
x = unlist(list(paste0(samp)))


# cendat2 <- K05_pop %>%
#   group_by(GEOID, YEAR) %>%
#   dplyr::summarise(population = sum(population)) %>%
#   mutate(Var3 = "D")

# rm(K05_pop, Kpops, KT, KTH, KTHD, newborns, stateproj, TOTAL, z)

# BA00m<- cbind(predccr("ccr1","MALE", x, CCDs),0,0)
# BA00m[,2]<- BA00m[,1] - sd(get(DF)[[as.character(ccr)]])*1.28
# BA00m[,3]<- BA00m[,1] + sd(get(DF)[[as.character(ccr)]])*1.28
# 
# for (i in 1:(SIZE-1)){
#   data_table <- cbind(predccr(paste0("ccr",i), "FEMALE", x, CCDs),0,0)
#   data_table[,2]<- data_table[,1]- sd(CCDs[[as.character(paste0("ccr",i))]][which(CCDs$SEX == sex)])*1.28
#   data_table[,3]<- data_table[,1]+ sd(CCDs[[as.character(paste0("ccr",i))]][which(CCDs$SEX == sex)])*1.28
#   assign(paste0("BA",i,"f"), data_table)
# }


project = function(x){
  tryCatch({#print(x)
    ###   Prediction of the CCR function
    predccr = function(ccr, sex, x, DF){
      y <-as_data_frame(DF[[as.character(ccr)]][which(DF$countyrace== x & DF$SEX == sex )])
      num<- seq(1,FORLEN,5)
      pred<- tryCatch(predict(ucm(value~0, data = y, level = TRUE, slope = FALSE)$model, n.ahead = FORLEN)[c(num),]
                      , error=function(e) array(0, c(STEPS, 3)))
      return(pred)
    }
    ###   Prediction of the CWR function. Maximum is set to 1% more than the max value in the time series. Minimum is set to a TFR of 1.0
    predcwr = function(ccr, sex, x, DF){
      hyndtran = function(ccr,DF){log((ccr - a) / (b - ccr))}
      b <- max(DF[[as.character(ccr)]][which(DF$countyrace== x)])*1.01
      a <- -0.00000001
      y <-as_data_frame(hyndtran(DF[[as.character(ccr)]][which(DF$countyrace== x & DF$SEX == sex)]))
      
      num<- seq(1,FORLEN,5)
      pred<- round(predict(ucm(value~0, data = y, level = TRUE, slope = FALSE)$model, n.ahead = FORLEN)[c(num),],5)
      pred2 <-(b-a)*exp(pred)/(1+exp(pred))+a
      return(round(pred2,6))#
    }
  ###################
    ### DATA PREP
    ##################
    ### Filtering the Census data based on the county/race combination
    K05 <- K05_pop[which(K05_pop$countyrace == x),]
    ###   Creating the Child/woman ratios.
    newborns <- K05 %>%
      filter(AGEGRP == 1) %>% # AGEGRP 1 = newborns.
      group_by(STATE, countyrace, YEAR)  %>%
      summarise(Newborns = sum(population))
    childbearing <-K05 %>%
      filter(AGEGRP %in% c(4,5,6,7,8,9,10), # women ages 15-49
             SEX == "FEMALE" ) %>%
      group_by(STATE, countyrace, YEAR) %>%
      dplyr::summarise(Women1550 = sum(population)) %>%
      left_join(., newborns) %>%
      mutate(fertrat = Newborns/Women1550) %>%
      filter(YEAR <= test_year)
      
    childbearing$SEX <- "FEMALE"
    childbearing[mapply(is.infinite, childbearing)] <- NA
    childbearing[mapply(is.nan, childbearing)] <- NA
    childbearing[is.na(childbearing)] <-0
    #childbearing$countyrace <- paste0(childbearing$GEOID, "_", childbearing$RACE)  
    
    ### Calculating the cohort-change differences (CCDs)
    CCDs<- K05 %>%
      ungroup() %>%
      mutate(AGEGRP = paste0("X", str_pad(AGEGRP, 2, pad ="0")),
             GEOID = paste0(STATE, COUNTY),
             population = as.numeric(population)) %>%
      spread(AGEGRP, population) %>%
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
    #CCDs$countyrace <- paste0(CCDs$GEOID, "_TOTAL")
    
    ### Calculating the CCRs
    CCRs<- K05 %>%
      ungroup() %>%
      mutate(AGEGRP = paste0("X", str_pad(AGEGRP, 2, pad ="0")),
             GEOID = paste0(STATE, COUNTY),
             population = as.numeric(population)) %>%
      spread(AGEGRP, population) %>%
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
      #CCRs$countyrace <- paste0(CCRs$GEOID, "_TOTAL")
      CCRs[mapply(is.infinite, CCRs)] <- NA
      CCRs[mapply(is.nan, CCRs)] <- NA
      CCRs[is.na(CCRs)] <-0
    ##################################################
    ### Start of the Additive projections
    ##################################################
    
    ###  Calculating the UCM's of the CCD's for each age/sex group. The confidence interval is set to 80% (1.28*SD) 
    for (i in 1:(SIZE-1)){
      data_tablef <- cbind(predccr(paste0("ccr",i), "FEMALE", x, CCDs),0,0)
      data_tablem <- cbind(predccr(paste0("ccr",i), "MALE", x, CCDs),0,0)
      data_tablef[,2]<- data_tablef[,1]- sd(CCDs[[as.character(paste0("ccr",i))]][which(CCDs$SEX == "FEMALE")])*1.28
      data_tablef[,3]<- data_tablef[,1]+ sd(CCDs[[as.character(paste0("ccr",i))]][which(CCDs$SEX == "FEMALE")])*1.28
      data_tablem[,2]<- data_tablem[,1]- sd(CCDs[[as.character(paste0("ccr",i))]][which(CCDs$SEX == "MALE")])*1.28
      data_tablem[,3]<- data_tablem[,1]+ sd(CCDs[[as.character(paste0("ccr",i))]][which(CCDs$SEX == "MALE")])*1.28
      assign(paste0("BA",i,"f"), data_tablef[1:3,1:3])
      assign(paste0("BA",i,"m"), data_tablem[1:3,1:3])
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
   ### Formatting the base population data as equal to the total population minus the group quaters.
    popf <- K05$population[which(K05$SEX == "FEMALE" & K05$YEAR == launch_year)]
    gqf <-  if (length(gqpop$group_quarters[which(gqpop$SEX == "FEMALE" & gqpop$countyrace == x)]) > 0){
      gqpop$group_quarters[which(gqpop$SEX == "FEMALE" & gqpop$countyrace == x)]} else {
        0}
    popf <- popf - gqf
    
    popm <- K05$population[which(K05$SEX == "MALE" & K05$YEAR == launch_year)]
    gqm <- if (length(gqpop$group_quarters[which(gqpop$SEX == "MALE" & gqpop$countyrace == x)]) > 0){
      gqpop$group_quarters[which(gqpop$SEX == "FEMALE" & gqpop$countyrace == x)]} else {
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
    n01 <- cbind(predcwr("fertrat", "FEMALE", x, childbearing),0,0)
      n01[,2]<- n01[,1] - sd(childbearing$fertrat)*1.28
      n01[,3]<- n01[,1] + sd(childbearing$fertrat)*1.28
    
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
      data_tablem$SEX <- "MALE"
      data_tablef <- as.data.frame.table(get(paste0("proj",i,"f")) + gqf)
      data_tablef$YEAR <- launch_year+ (i*5)
      data_tablef$SEX <- "FEMALE"
      projm <- rbind(projm, data_tablem)
      projf <-rbind(projf, data_tablef)
      namm<- get(paste0("proj",i,"m"))
      rm(data_tablem)
    }
    ### Declaring several variables
    projadd <-rbind(projm, projf) %>%
      rename(Scenario = Var2)
    projadd$countyrace <-x
    projadd$TYPE<- "ADD"
    
    ######################################
    ### PROJECTING THE CCRs
    
    ### Calculating the CCR UCMs for each individual age group
    for (i in 1:(SIZE-1)){
      data_tablef <- cbind(predccr(paste0("ccr",i), "FEMALE", x, CCRs),0,0)
      data_tablem <- cbind(predccr(paste0("ccr",i), "MALE", x, CCRs),0,0)
      data_tablef[,2]<- data_tablef[,1]- sd(CCRs[[as.character(paste0("ccr",i))]][which(CCRs$SEX == "FEMALE")])*1.28
      data_tablef[,3]<- data_tablef[,1]+ sd(CCRs[[as.character(paste0("ccr",i))]][which(CCRs$SEX == "FEMALE")])*1.28
      data_tablem[,2]<- data_tablem[,1]- sd(CCRs[[as.character(paste0("ccr",i))]][which(CCRs$SEX == "MALE")])*1.28
      data_tablem[,3]<- data_tablem[,1]+ sd(CCRs[[as.character(paste0("ccr",i))]][which(CCRs$SEX == "MALE")])*1.28
      assign(paste0("BA",i,"f"), data_tablef[1:3,1:3])
      assign(paste0("BA",i,"m"), data_tablem[1:3,1:3])
      rm(data_tablef, data_tablem)
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
    ### Formatting the base population data.
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
        data_tablem$SEX <- "MALE"
        data_tablef <- as.data.frame.table(get(paste0("proj",i,"f")) + gqf)
        data_tablef$YEAR <- launch_year+ (i*5)
        data_tablef$SEX <- "FEMALE"
        
        projm <- rbind(projm, data_tablem)
        projf <-rbind(projf, data_tablef)
        namm<- get(paste0("proj",i,"m"))
        rm(data_tablem)
      }
      
      projmult <-rbind(projm, projf) %>%
        select(-Var2) %>%
        rename(Scenario = Var3)
      projmult$countyrace <-x
      
      projmult$TYPE<- "Mult"
      ##################################
      #   Creating a launch population vectors
      proj0A<-proj0B<-proj0C<- data.table()
      proj0A = as_data_frame(K05[which(K05$YEAR == launch_year),]) %>%
        ungroup() %>%
        mutate(Var1 = paste0("a", AGEGRP),
               Scenario = "A") %>%
        rename(Freq = population) %>%
        dplyr::select(Var1, Scenario, Freq, YEAR, SEX)
      
      proj0B = as_data_frame(K05[which(K05$YEAR == launch_year),]) %>%
        ungroup() %>%
        mutate(Var1 = paste0("a", AGEGRP),
               Scenario = "B") %>%
        rename(Freq = population) %>%
        dplyr::select(Var1, Scenario, Freq, YEAR, SEX)
      
      proj0C <- as_data_frame(K05[which(K05$YEAR == launch_year),]) %>%
        ungroup() %>%
        mutate(Var1 = paste0("a", AGEGRP),
               Scenario = "C") %>%
        rename(Freq = population) %>%
        dplyr::select(Var1, Scenario, Freq, YEAR, SEX)
      
      proj0 <- rbind(proj0A, proj0B) %>%
        rbind(., proj0C)
      proj0$TYPE ="BASE"
      proj0$countyrace = x
      ##########################
      # Collecting all projections together
      proj <-rbind(projadd, projmult) %>%
        rbind(., proj0)
        #projadd$countyrace <-x
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


# d= pblapply(x, project)

KT = rbindlist(pbmclapply(x, project, mc.cores = detectCores()-1))
# KT <- rbindlist(d)

# write.table(KT, "EVAL/COUNTYnoslope_addmult.csv")

KT2 <- KT %>%
  mutate(AGEGRP = as.numeric(substr(Var1, 2,3))) %>%
  group_by(YEAR, countyrace, SEX, AGEGRP) %>%
  spread(Scenario, Freq)

z<- left_join(K05_pop, KT2) 
z<- z %>%
  filter(YEAR>2000) %>%
  na.omit

tot <- z %>%
  group_by(countyrace, YEAR, TYPE) %>%
  summarise(population = sum(population),
            A = sum(A),
            B = sum(B),
            C = sum(C)) %>%
  mutate(FLAG1 = if_else(is.na((A/population)-1), 0,(A/population)-1),
         FLAG2 = if_else(population>=B & population<=C,1,0)) %>%
  ungroup()

eval_ucm_tot <- tot %>%
  group_by(YEAR, TYPE) %>%
  #filter(YEAR == 2010) %>%
  summarise(MAPE = quantile(abs(FLAG1), 0.5),
            in90percentile = sum(FLAG2)/length(unique(z$countyrace)))
ages <- z %>%
  group_by(countyrace, YEAR, AGEGRP) %>%
  summarise(population = sum(population),
            A = sum(A),
            B = sum(B),
            C = sum(C)) %>%
  mutate(FLAG1 = if_else(is.na((A/population)-1), 0,(A/population)-1),
         FLAG2 = if_else(population>=B & population<=C,1,0)) %>%
  ungroup()

eval_ucm_ages <- ages %>%
  group_by(AGEGRP, YEAR) %>%
  #filter(YEAR == 2010) %>%
  summarise(#MAPE = median(abs(FLAG1))*100,
    MALPE = median(FLAG1)*100) %>%
  spread(YEAR, MALPE)

a<-ggplot(data=tot)+
  geom_point(aes(x=population, y =A), size=1, alpha=0.5,color='dark gray') +
  geom_abline(slope=1) +
  # geom_abline(slope=1.1, lty = 2, lwd = 1, col = 'black') +
  # geom_abline(slope=0.9, lty = 2, lwd = 1, col = 'black') +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(text=element_text(face='bold')) +
  labs(#caption='Source: UN Pop Division, 1950-2010', 
    #title='Actual TFR vs. iTFR',
    x='Observed Population, n', y='Projected Population, n')

b <- ggplot(data=tot, aes(x=population, y=FLAG1)) +
  #annotation_custom(my_grobd)+
  #geom_point(size=0.5, alpha=.50) +
  stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE) +
  geom_point(shape = '.', col = 'black', size =0.5) +
  scale_fill_gradientn(colours = gray(10:0 / 10)) +
  #scale_fill_gradientn(colours = terrain.colors(10)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = .10, lty=2, lwd=1)+
  geom_hline(yintercept = -.10, lty=2, lwd=1)+
  theme_bw() +
  # geom_hex(bins = 50) +
  # scale_fill_gradientn("", colours = rev(rainbow(10, end = 4/6))) +
  theme(text=element_text(face='bold')) +
  scale_y_continuous(expand= c(0,0),
                     limits= c(-.50,.5))+
  theme(legend.position="none") +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand = c(0,0)) +
  labs(x='Population, n', y='ALPE') 



c<- ggplot(data=tot) +
  #annotation_custom(my_grobj)+
  geom_density(aes(x=FLAG1), adjust=1.5,fill='gray',col='gray', lwd=1, alpha=.20) +
  #geom_density(data=join, aes(x=bayesTFR-lagTFR),adjust=1.5, fill='green',col='green', lwd=1, alpha=.20) +
  #geom_density(data=join, aes(x=xTFR-lagTFR), adjust=1.5, fill="blue", col="blue", lwd=1, alpha=.20)+
  geom_vline(xintercept=0) +
  theme_bw() +
  xlim(-0.5,0.5) + 
  geom_vline(aes(xintercept=quantile((FLAG1), 0.9, na.rm=T)), lty=2, lwd=0.5, color="black") +
  geom_vline(aes(xintercept=quantile((FLAG1), 0.1, na.rm=T)), lty=2, lwd=0.5, color="black") +
  #geom_vline(xintercept = 0.2, lty=2, lwd=1)+
  #geom_vline(xintercept = -0.2, lty=2, lwd=1)+
  theme(text=element_text(face='bold')) +
  labs(x='Algebraic Percent(error)',
       y='Density'
       # title='Estimation Errors: iTFR (red) versus Age-Structure-Adjusted iTFR (blue)',
       #caption='Human Fertility Database 1950+'
  )

d<- ggplot(data=tot) +
  #annotation_custom(my_grobk)+
  geom_density(aes(x=abs(FLAG1)), adjust=1.5,fill='gray',col='gray', lwd=1, alpha=.20) +
  
  geom_vline(xintercept=0) +
  theme_bw() +
  xlim(0,+0.50) + 
  geom_vline(aes(xintercept=quantile(abs(FLAG1), 0.5, na.rm=T)), lty=1, lwd=0.5, color="black") +
  geom_vline(aes(xintercept=quantile(abs(FLAG1), 0.9, na.rm=T)), lty=2, lwd=0.5, color="black") +
  
  #geom_vline(xintercept = 10, lty=2, lwd=1)+
  #geom_vline(xintercept = -10, lty=2, lwd=1)+
  theme(text=element_text(face='bold')) +
  labs(x='Absolute Percent (error)',
       y='Density'
       # title='Estimation Errors: iTFR (red) versus Age-Structure-Adjusted iTFR (blue)',
       #caption='Human Fertility Database 1950+'
  )

p<-plot_grid(a,b,c,d, labels = "AUTO")
title <- ggdraw() + draw_label("Error Rates for county-level population projections", fontface='bold')

p1<- plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
p2<-add_sub(p1, paste0("", length(unique(z$countyrace)), " counties based on CCRs from 1980-2000 evaluated at 2005, 2010, and 2015 \nusing Unobserved Component Models for each individual age/sex group.\n no slope CCAs, slope CWRs"))
ggdraw(p2)