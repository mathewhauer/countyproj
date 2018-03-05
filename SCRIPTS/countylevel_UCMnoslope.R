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
GROUPING <- c("STATE", "COUNTY", "YEAR", "AGEGRP", "SEX")

test_year = 2016
K05_pop <- read_csv("DATA/cendatbase.csv") %>%
  group_by(.dots = GROUPING) %>%
  summarise(population = sum(population))
K05_pop$GEOID <- paste0(K05_pop$STATE, K05_pop$COUNTY)
K05_pop$countyrace <- paste0(K05_pop$GEOID, "_TOTAL")

gqpop <- read.csv("DATA/gq_2000.csv") %>%
  mutate(STATE = str_pad(STATE, 2, pad = "0"),
         COUNTY = str_pad(COUNTY, 3, pad="0"),
         GEOID = paste0(STATE, COUNTY)) %>%
  group_by(.dots = GROUPING) %>%
  summarise(group_quarters = sum(GQ, na.rm=T))



gqpop$GEOID <- paste0(gqpop$STATE, gqpop$COUNTY)
gqpop$countyrace <- paste0(gqpop$GEOID, "_TOTAL")

CCRs<- K05_pop %>%
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
CCRs$countyrace <- paste0(CCRs$GEOID, "_TOTAL")

#z<- CCRs[!complete.cases(CCRs),]

CCRs[mapply(is.infinite, CCRs)] <- NA
CCRs[mapply(is.nan, CCRs)] <- NA
CCRs[is.na(CCRs)] <-0

# CCR2 <- CCRs %>%
#   gather(ccr0, ccr, ccr01:ccr17) %>%
#   select(STATE, YEAR, SEX, GEOID, ccr0, ccr) %>%
#   ungroup() %>%
#   group_by(STATE, SEX, GEOID, ccr0) %>%
#   summarise(p20 = ccr[which(YEAR == launch_year)] - sd(ccr)*1.28,
#             p50 = ccr[which(YEAR == launch_year)],
#             p80 = ccr[which(YEAR == launch_year)] + sd(ccr)*1.28)


newborns <- K05_pop %>%
  filter(AGEGRP == 1) %>%
  group_by(STATE, GEOID, YEAR)  %>%
  summarise(Newborns = sum(population))

childbearing <-K05_pop %>%
  filter(AGEGRP %in% c(4,5,6,7,8,9,10),
         SEX == "FEMALE" ) %>%
  group_by(STATE, GEOID, YEAR) %>%
  dplyr::summarise(Women1550 = sum(population)) %>%
  left_join(., newborns) %>%
  mutate(fertrat = Newborns/Women1550) %>%
  filter(YEAR <= test_year)
#childbearing$GEOID <- paste0(childbearing$STATE, "000")
childbearing$SEX <- "FEMALE"
childbearing$countyrace <- paste0(childbearing$GEOID, "_TOTAL")

#z<- childbearing[!complete.cases(childbearing),]

# childbearing2 <- childbearing %>%
#   
#   group_by(STATE, SEX, GEOID) %>%
#   summarise(p20 = fertrat[which(YEAR == launch_year)] - sd(fertrat)*1.28,
#             p50 = fertrat[which(YEAR == launch_year)],
#             p80 = fertrat[which(YEAR == launch_year)] + sd(fertrat)*1.28)


launch_year = 2016
SIZE<-18
# NUMBER OF PROJECTION STEPS
STEPS<-10
FORLEN<-(STEPS*5)
BASEANDSTEPS<-STEPS+1
cyc<-30
# SET THE PROJECTION INTERVALS PERCENTILE
setlevel <- 0.8
years <- 0
years$YEAR <- seq(launch_year+5,launch_year+(STEPS*5), 5)
years$YEAR <- seq(launch_year+1,launch_year+STEPS,1)

# samp <- filter(CCRs, STATE == "13")
# samp <- unique(samp$GEOID)
samp <- unique(K05_launch$GEOID)

# this.county = "49033" # "38105" "12119 "38053"
# samp <- "13009" "02158" "36109" "13117" "48301"
# x = this.county
x = unlist(list(paste0(samp, "_TOTAL")))

# z<-filter(CCRs, GEOID == samp, SEX == "FEMALE")
# predict(ucm(ccr01~ccr17, data = z)$model, n.ahead = FORLEN, interval = "prediction", level = setlevel)


predccr = function(ccr, sex, x, DF){
  hyndtran = function(ccr,DF){log((ccr - a) / (b - ccr))}
  #hyndtran = function(ccr, DF){((ccr-a)/1)+a}
  #a <- quantile(DF[[as.character(ccr)]][which(DF$countyrace==x & DF$SEX == sex)], .5)
  #a <- quantile(DF[[as.character(ccr)]][which(DF$YEAR==test_year & DF$SEX == sex)], .5)
  b <- max(DF[[as.character(ccr)]][which(DF$countyrace== x)])*1.01
  a <- -0.00000000000001
  
  y <-as_data_frame(hyndtran(DF[[as.character(ccr)]][which(DF$countyrace== x & DF$SEX == sex )]))
  
  num<- seq(1,FORLEN,5)
  pred<- tryCatch(predict(ucm(value~0, data = y, level = TRUE, slope = FALSE)$model, n.ahead = FORLEN, interval = "prediction", level = setlevel)[c(num),]
                  , error=function(e) array(0, c(STEPS, 3)))
  pred2 <-(b-a)*exp(pred)/(1+exp(pred))+a
  return(pred2)#, cycle = TRUE, cycle.period = cyc
}

predcwr = function(ccr, sex, x, DF){
  hyndtran = function(ccr,DF){log((ccr - a) / (b - ccr))}
  b <- max(DF[[as.character(ccr)]][which(DF$countyrace== x)])*1.01
  a <- 1/7
  y <-as_data_frame(hyndtran(DF[[as.character(ccr)]][which(DF$countyrace== x & DF$SEX == sex)]))
  
  num<- seq(1,FORLEN,5)
  pred<- round(predict(ucm(value~0, data = y, level = TRUE, slope = FALSE)$model, n.ahead = FORLEN, interval = "prediction", level = setlevel)[c(num),],5)
  pred2 <-(b-a)*exp(pred)/(1+exp(pred))+a
  return(pred2)#
}

# arimaccr = function(ccr, sex, x, DF){
#   num<- seq(1,FORLEN,5)
#   as.data.frame(forecast(auto.arima(DF[[as.character(ccr)]][which(DF$countyrace == x & DF$SEX== sex)], allowdrift=FALSE, lambda=0), h= FORLEN))[c(num),]
#   
# }


# z2<- filter(CCRs, countyrace ==x)
# z<- filter(K05_pop, countyrace == x)
# z<- filter(childbearing, countyrace ==x)
z<- filter(cendat2, GEOID==substr(x,1,5))

K05_launch <- K05_pop %>%
  filter(YEAR == launch_year)

cendat2 <- K05_pop %>%
  group_by(GEOID, YEAR) %>%
  dplyr::summarise(population = sum(population)) %>%
  mutate(Var3 = "D")

# rm(K05_pop, Kpops, KT, KTH, KTHD, newborns, stateproj, TOTAL, z)

project = function(x){
  tryCatch({#print(this.county)
    #K05 <- filter(K05_pop , countyrace == x) 
    
    for (i in 1:(SIZE-1)){
      namm<-paste0("BA", i-1, "m")
      namf<-paste0("BA",i-1,"f")
      assign(namm, predccr(paste0("ccr",i), "MALE", x, CCRs))
      assign(namf, predccr(paste0("ccr",i), "FEMALE", x, CCRs))}
    
    # BA00m<- predccr("ccr1","MALE", x, CCRs)
    # BA01m<- predccr("ccr2","MALE", x, CCRs)
    # BA02m<- predccr("ccr3","MALE", x, CCRs)
    # BA03m<- predccr("ccr4","MALE", x, CCRs)
    # BA04m<- predccr("ccr5","MALE", x, CCRs)
    # BA05m<- predccr("ccr6","MALE", x, CCRs)
    # BA06m<- predccr("ccr7","MALE", x, CCRs)
    # BA07m<- predccr("ccr8","MALE", x, CCRs)
    # BA08m<- predccr("ccr9","MALE", x, CCRs)
    # BA09m<- predccr("ccr10","MALE", x, CCRs)
    # BA10m<- predccr("ccr11","MALE", x, CCRs)
    # BA11m<- predccr("ccr12","MALE", x, CCRs)
    # BA12m<- predccr("ccr13","MALE", x, CCRs)
    # BA13m<- predccr("ccr14","MALE", x, CCRs)
    # BA14m<- predccr("ccr15","MALE", x, CCRs)
    # BA15m<- predccr("ccr16","MALE", x, CCRs)
    # BA16m<- predccr("ccr17","MALE", x, CCRs)
    # 
    # BA00f<- predccr("ccr1","FEMALE", x, CCRs)
    # BA01f<- predccr("ccr2","FEMALE", x, CCRs)
    # BA02f<- predccr("ccr3","FEMALE", x, CCRs)
    # BA03f<- predccr("ccr4","FEMALE", x, CCRs)
    # BA04f<- predccr("ccr5","FEMALE", x, CCRs)
    # BA05f<- predccr("ccr6","FEMALE", x, CCRs)
    # BA06f<- predccr("ccr7","FEMALE", x, CCRs)
    # BA07f<- predccr("ccr8","FEMALE", x, CCRs)
    # BA08f<- predccr("ccr9","FEMALE", x, CCRs)
    # BA09f<- predccr("ccr10","FEMALE", x, CCRs)
    # BA10f<- predccr("ccr11","FEMALE", x, CCRs)
    # BA11f<- predccr("ccr12","FEMALE", x, CCRs)
    # BA12f<- predccr("ccr13","FEMALE", x, CCRs)
    # BA13f<- predccr("ccr14","FEMALE", x, CCRs)
    # BA14f<- predccr("ccr15","FEMALE", x, CCRs)
    # BA15f<- predccr("ccr16","FEMALE", x, CCRs)
    # BA16f<- predccr("ccr17","FEMALE", x, CCRs)
    
    
    
    for (i in 1:STEPS){
      namm<-paste0("lx", i, "m")
      namf<-paste0("lx",i,"f")
      assign(namm, rbind(BA0m[i,],BA1m[i,],BA2m[i,], BA3m[i,], BA4m[i,], BA5m[i,], BA6m[i,], BA7m[i,], BA8m[i,], BA9m[i,], BA10m[i,]
                         , BA11m[i,], BA12m[i,], BA13m[i,], BA14m[i,], BA15m[i,], BA16m[i,]))
      assign(namf, rbind(BA0f[i,],BA1f[i,],BA2f[i,], BA3f[i,], BA4f[i,], BA5f[i,], BA6f[i,], BA7f[i,], BA8f[i,], BA0f[i,], BA10f[i,]
                         , BA11f[i,], BA12f[i,], BA13f[i,], BA14f[i,], BA15f[i,], BA16f[i,]))}
    
    ###   putting the survival rates in the diagonal of the leslise matrices  
    # S10f<-S09f<-S08f<-S07f<-S06f<-S05f<-S04f<-S03f<-S02f<-S01f<-array(0,c(SIZE,SIZE,3))
    for (i in 1:STEPS){
      data_table <- get(paste0("lx",i,"f"))
      weird_data <- array(0,c(SIZE,SIZE,ncol(data_table)))
      for(j in 1:ncol(data_table)){  
        weird_data[,,j] <- rbind(0,cbind(diag(data_table[,j]),0))
        weird_data[18,18,j]=data_table[17,j]
        assign(paste0("S",i,"f"), weird_data)
      }
      rm(data_table)
      rm(weird_data)  
    }
    for (i in 1:STEPS){
      data_table <- get(paste0("lx",i,"m"))
      weird_data <- array(0,c(SIZE,SIZE,ncol(data_table)))
      for(j in 1:ncol(data_table)){  
        weird_data[,,j] <- rbind(0,cbind(diag(data_table[,j]),0))
        assign(paste0("S",i,"m"), weird_data)
      }
      rm(data_table)
      rm(weird_data)  
    }
    
    popf <- K05_launch$population[which(K05_launch$SEX == "FEMALE" & K05_launch$countyrace == x)]
    gqf <-  if (length(gqpop$group_quarters[which(gqpop$SEX == "FEMALE" & gqpop$countyrace == x)]) > 0){
      gqpop$group_quarters[which(gqpop$SEX == "FEMALE" & gqpop$countyrace == x)]} else {
        0}
    popf <- popf - gqf
    
    popm <- K05_launch$population[which(K05_launch$SEX == "MALE" & K05_launch$countyrace == x)]
    gqm <- if (length(gqpop$group_quarters[which(gqpop$SEX == "MALE" & gqpop$countyrace == x)]) > 0){
      gqpop$group_quarters[which(gqpop$SEX == "FEMALE" & gqpop$countyrace == x)]} else {
        0}
    popm <- popm - gqm 
    p0f <-array(0,c(SIZE,1,ncol(lx1f)))
    p0m <-array(0,c(SIZE,1,ncol(lx1f)))
    for (i in 1:ncol(lx1f)){
      p0f[,,i] <- cbind(popf)
      p0m[,,i] <- cbind(popm)
    }  
    
    
    
    
    n01<--array(0,c(BASEANDSTEPS,ncol(lx1f)))
    for(i in 1:ncol(lx1f)){
      n01 <- predcwr("fertrat", "FEMALE", x, childbearing)
      #n01<- 
      #n01<-predccr_ucm("fertrat", fertdat)
      # n01<-if (sign(coef(lm(fertrat ~ YEAR, data=fertdat))[2]) < 0){exp(predict(lm(log(fertrat) ~ YEAR, data=fertdat), years,interval = "prediction", level = setlevel))} else{predict(lm(fertrat ~ YEAR, data = fertdat), years,interval = "prediction", level = setlevel)}
      #n01<-predcwr_ucm("fertraty", fertdat)
      #n01 <-predict(ucm(formula = fertrat~0, data = fertdat, level = TRUE, slope = TRUE)$model, n.ahead = BASEANDSTEPS, interval = "prediction", level= setlevel)
    }
    
    for (i in 1:STEPS){
      data_tablef <- get(paste0("S",i,"f"))
      data_tablem <- get(paste0("S",i,"m"))
      projm<-projf <- array(0,c(SIZE,1,ncol(lx1f)), dimnames = list(
        c("a1", "a2", "a3", "a4", "a5","a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16", "a17", "a18")))
      popdatf<- get(paste0("p",i-1,"f"))
      popdatm<- get(paste0("p",i-1,"m"))
      for(j in 1:ncol(lx1f)){  
        projf[,,j] <- data_tablef[,,j] %*% popdatf[,,j]
        projm[,,j] <- data_tablem[,,j] %*% popdatm[,,j]
        projf[1,1,j] <- (n01[i,j] * sum(projf[4:10,,j]))*.487
        projm[1,1,j] <- (n01[i,j] * sum(projf[4:10,,j]))*.512
        assign(paste0("p",i,"f"), projf)
        assign(paste0("p",i,"m"), projm)
      }
      rm(data_tablef, data_tablem, projf, projm, popdatf, popdatm)
    }
    
    projm<-NULL
    projf<-NULL
    for (i in 1:STEPS){
      data_tablem <- as.data.frame.table(get(paste0("p",i,"m")) + gqm)
      data_tablem$YEAR <- launch_year+ (i*5)
      data_tablem$SEX <- "MALE"
      data_tablef <- as.data.frame.table(get(paste0("p",i,"f")) + gqf)
      data_tablef$YEAR <- launch_year+ (i*5)
      data_tablef$SEX <- "FEMALE"
      
      projm <- rbind(projm, data_tablem)
      projf <-rbind(projf, data_tablef)
      
      rm(data_tablem)
    }
    
    proj0A<-proj0B<-proj0C<- data.table()
    proj0A = as_data_frame(K05_launch[which(K05_launch$countyrace == x),]) %>%
      ungroup() %>%
      mutate(Var1 = paste0("a", AGEGRP),
             Var2 = "A",
             Var3 = "A") %>%
      rename(Freq = population) %>%
      dplyr::select(Var1, Var2, Var3, Freq, YEAR, SEX)
    
    proj0B = as_data_frame(K05_launch[which(K05_launch$countyrace == x),]) %>%
      ungroup() %>%
      mutate(Var1 = paste0("a", AGEGRP),
             Var2 = "A",
             Var3 = "B") %>%
      rename(Freq = population) %>%
      dplyr::select(Var1, Var2, Var3, Freq, YEAR, SEX)
    
    proj0C <- as_data_frame(K05_launch[which(K05_launch$countyrace == x),]) %>%
      ungroup() %>%
      mutate(Var1 = paste0("a", AGEGRP),
             Var2 = "A",
             Var3 = "C") %>%
      rename(Freq = population) %>%
      dplyr::select(Var1, Var2, Var3, Freq, YEAR, SEX)
    proj0 <- rbind(proj0A, proj0B) %>%
      rbind(., proj0C)
    
    proj <-rbind(projm, projf) %>%
      rbind(., proj0)
    proj$countyrace <-x
    #if(!is.null(proj)){return(proj)}else {NULL}
    return(proj)
  }
  , error=function(e){cat(x," ERROR :",conditionMessage(e), "\n")})
}


d= pblapply(x, project)

#d = mclapply(x, project, mc.cores = 2)
KT <- rbindlist(d)
# KT2<-rbindlist(d2)
# KT<-rbind(KT, KT2)
#(end_time <- Sys.time() - start_time)
# KT2 <- KT %>%
#   mutate(AGEGRP = as.numeric(substr(Var1, 2,3))) %>%
#   group_by(YEAR, countyrace, SEX, AGEGRP) %>%
#   spread(Var3, Freq)

Kpops <- KT %>%
  separate(countyrace, c("GEOID", "RACE"), sep = "_")%>%
  separate(Var1, c("drop", "AGEGRP"), sep = "a")

KTHD <- Kpops %>%
  #filter(Var3 == "A") %>%
  group_by(GEOID, Var3, YEAR) %>%
  #ungroup() %>%
  summarise(population = sum(Freq, na.rm=T))

# z<- filter(KTHD, GEOID == "36001")

KTH <- rbind(cendat2, KTHD)

states <- as.data.frame(KTH) %>%
  ungroup() %>%
  mutate(STATE = substr(GEOID, 1,2)) %>%
  group_by(STATE, Var3, YEAR) %>%
  dplyr::summarise(population = sum(population, na.rm=T)) %>%
  mutate(GEOID = paste0(STATE, "000")) %>%
  ungroup() %>%
  select(-STATE)

TOTAL <- KTH %>%
  ungroup() %>%
  #mutate(STATE = substr(GEOID, 1,2)) %>%
  group_by(Var3, YEAR) %>%
  dplyr::summarise(population = sum(population, na.rm=T)) %>%
  mutate(population = population/2) %>%
  ungroup() %>%
  select(-STATE)

KTH <- rbind(as.data.frame(KTH), as.data.frame(states))