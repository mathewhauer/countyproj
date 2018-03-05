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
GROUPING <- c("STATE", "COUNTY", "YEAR", "AGEGRP", "SEX")

test_year = 2000
launch_year = test_year
SIZE<-18
# NUMBER OF PROJECTION STEPS
STEPS<-3
FORLEN<-(STEPS*5)
BASEANDSTEPS<-STEPS+1
cyc<-30
# SET THE PROJECTION INTERVALS PERCENTILE
setlevel <- 0.8
years <- 0
years$YEAR <- seq(launch_year+5,launch_year+(STEPS*5), 5)
years$YEAR <- seq(launch_year+1,launch_year+STEPS,1)


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

# samp <- filter(CCRs, STATE == "13")
# samp <- unique(samp$GEOID)
samp <- unique(K05_pop$GEOID)

# this.county = "49033" # "38105" "12119 "38053"
# samp <- "13117" "02158" "36109" "13117" "48301"
# x = this.county
x = unlist(list(paste0(samp, "_TOTAL")))

# cendat2 <- K05_pop %>%
#   group_by(GEOID, YEAR) %>%
#   dplyr::summarise(population = sum(population)) %>%
#   mutate(Var3 = "D")

# rm(K05_pop, Kpops, KT, KTH, KTHD, newborns, stateproj, TOTAL, z)

project = function(x){
  tryCatch({#print(x)
    K05 <- K05_pop[which(K05_pop$countyrace == x),]
    newborns <- K05 %>%
      filter(AGEGRP == 1) %>%
      group_by(STATE, GEOID, YEAR)  %>%
      summarise(Newborns = sum(population))
    childbearing <-K05 %>%
      filter(AGEGRP %in% c(4,5,6,7,8,9,10),
             SEX == "FEMALE" ) %>%
      group_by(STATE, GEOID, YEAR) %>%
      dplyr::summarise(Women1550 = sum(population)) %>%
      left_join(., newborns) %>%
      mutate(fertrat = Newborns/Women1550) %>%
      filter(YEAR <= test_year) %>%
      summarise(p20 = mean(fertrat[which(YEAR == launch_year)]) - sd(fertrat)*1.28,
                p50 = mean(fertrat[which(YEAR == launch_year)]),
                p80 = mean(fertrat[which(YEAR == launch_year)]) + sd(fertrat)*1.28)
    childbearing$SEX <- "FEMALE"
    childbearing$countyrace <- paste0(childbearing$GEOID, "_TOTAL")  
    
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
      filter(YEAR >= min(YEAR +5, na.rm=T) & YEAR <= test_year) %>%
      gather(ccr0, ccr, ccr1:ccr17) %>%
      select(STATE, YEAR, SEX, GEOID, ccr0, ccr) %>%
      ungroup() %>%
      group_by(STATE, SEX, GEOID, ccr0) %>%
      summarise(p20 = mean(ccr[which(YEAR == launch_year)]) - sd(ccr)*1.28,
                p50 = mean(ccr[which(YEAR == launch_year)]),
                p80 = mean(ccr[which(YEAR == launch_year)]) + sd(ccr)*1.28)
    CCDs$countyrace <- paste0(CCDs$GEOID, "_TOTAL")
    
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
      filter(YEAR >= min(YEAR +5, na.rm=T) & YEAR <= test_year) %>%
      gather(ccr0, ccr, ccr1:ccr17) %>%
      select(STATE, YEAR, SEX, GEOID, ccr0, ccr) %>%
      ungroup() %>%
      group_by(STATE, SEX, GEOID, ccr0) %>%
      summarise(p20 = mean(ccr[which(YEAR == launch_year)]) - sd(ccr)*1.28,
                p50 = mean(ccr[which(YEAR == launch_year)]),
                p80 = mean(ccr[which(YEAR == launch_year)]) + sd(ccr)*1.28)
    CCRs$countyrace <- paste0(CCRs$GEOID, "_TOTAL")
    
    lxf<-lxm <- array(0,c(SIZE-1,3))
    lxm[,1] <- CCDs$p50[which(CCDs$SEX=="MALE")]
    lxm[,2]<- CCDs$p20[which(CCDs$SEX=="MALE")]
    lxm[,3]<- CCDs$p80[which(CCDs$SEX=="MALE")]
    lxf[,1] <- CCDs$p50[which(CCDs$SEX=="FEMALE")]
    lxf[,2]<- CCDs$p20[which(CCDs$SEX=="FEMALE")]
    lxf[,3]<- CCDs$p80[which(CCDs$SEX=="FEMALE")]

  Sf<-Sm<-array(0,c(SIZE,SIZE,3))
   for (i in 1:3){
     Sf[,,i]<-rbind(0,cbind(diag(lxf[,i]),0))
     Sm[,,i]<-rbind(0,cbind(diag(lxm[,i]),0))
   } 
   
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
    p0f <-array(0,c(SIZE,SIZE,ncol(lxf)))
    p0m <-array(0,c(SIZE,SIZE,ncol(lxf)))
    for (i in 1:ncol(lxf)){
      p0f[,,i] <-  rbind(0,cbind(diag(popf),0))[1:18,1:18]
      p0f[18,18,i] = popf[18]
      p0m[,,i] <-  rbind(0,cbind(diag(popm),0))[1:18,1:18]
      p0m[18,18,i] = popm[18]
    }  
    
    n01<-array(0,c(1,3))
    n01[1]<- childbearing$p50
    n01[2]<- childbearing$p20
    n01[3]<- childbearing$p80

    for (i in 1:STEPS){
      data_tablef <- get(paste0("Sf"))
      data_tablem <- get(paste0("Sm"))
      projm<-projf <- array(0,c(SIZE,ncol(lxf)), dimnames = list(
        c("a1", "a2", "a3", "a4", "a5","a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16", "a17", "a18")))
      popdatf<- get(paste0("p",i-1,"f"))
      popdatm<- get(paste0("p",i-1,"m"))
      for(j in 1:ncol(lxf)){  
        projf[,j] <- rowSums(data_tablef[,,j] + popdatf[,,j])
        projm[,j] <- rowSums(data_tablem[,,j] + popdatm[,,j])
        projf[1,j] <- (n01[,j] * sum(projf[4:10,j]))*.487
        projm[1,j] <- (n01[,j] * sum(projf[4:10,j]))*.512
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
    
    projadd <-rbind(projm, projf) %>%
      rename(Scenario = Var2)
    projadd$countyrace <-x
    projadd$TYPE<- "ADD"
    
    
    lxf<-lxm <- array(0,c(SIZE-1,3))
    lxm[,1] <- CCRs$p50[which(CCRs$SEX=="MALE")]
    lxm[,2]<- CCRs$p20[which(CCRs$SEX=="MALE")]
    lxm[,3]<- CCRs$p80[which(CCRs$SEX=="MALE")]
    lxf[,1] <- CCRs$p50[which(CCRs$SEX=="FEMALE")]
    lxf[,2]<- CCRs$p20[which(CCRs$SEX=="FEMALE")]
    lxf[,3]<- CCRs$p80[which(CCRs$SEX=="FEMALE")]
    Sf<-Sm<-array(0,c(SIZE,SIZE,3))
    for (i in 1:3){
      Sf[,,i]<-rbind(0,cbind(diag(lxf[,i]),0)) #setting the diagonal to the survival rate
      Sf[18,18,i]=lxf[17,i] ##setting the the open-ended interval survival rate
      Sm[,,i]<-rbind(0,cbind(diag(lxm[,i]),0)) #setting the diagonal to the survival rate
      Sm[18,18,i]=lxm[17,i] ##setting the the open-ended interval survival rate
    }    
    p0f <-array(0,c(SIZE,1,ncol(lxf)))
    p0m <-array(0,c(SIZE,1,ncol(lxf)))
    for (i in 1:ncol(lxf)){
      p0f[,,i] <-  cbind(popf)
      p0m[,,i] <-  cbind(popm)
     
    }  
    
    for (i in 1:STEPS){
      data_tablef <- get(paste0("Sf"))
      data_tablem <- get(paste0("Sm"))
      projm<-projf <- array(0,c(SIZE,1,ncol(lxf)), dimnames = list(
        c("a1", "a2", "a3", "a4", "a5","a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16", "a17", "a18")))
      popdatf<- get(paste0("p",i-1,"f"))
      popdatm<- get(paste0("p",i-1,"m"))
      for(j in 1:ncol(lxf)){  
        projf[,,j] <- data_tablef[,,j] %*% popdatf[,,j]
        projm[,,j] <- data_tablem[,,j] %*%  popdatm[,,j]
        projf[1,,j] <- (n01[,j] * sum(projf[4:10,,j]))*.487
        projm[1,,j] <- (n01[,j] * sum(projf[4:10,,j]))*.512
        # popdatf[,j] <-rbind(0,cbind(diag(projf[,j]),0))[1:18,1:18]
        # popdatm[,j] <-rbind(0,cbind(diag(projm[,j]),0))[1:18,1:18]
        # popdatf[18,18,j] <- projf[18,j]
        # popdatm[18,18,j] <- projm[18,j]
        assign(paste0("p",i,"f"), projf)
        assign(paste0("p",i,"m"), projm)
        assign(paste0("proj",i,"f"), projf)
        assign(paste0("proj",i,"m"), projm)
      }
    }
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
      proj <-rbind(projadd, projmult) %>%
        rbind(., proj0)
        #projadd$countyrace <-x
      #projadd$TYPE<- "ADD"
      addsum<-sum(proj$Freq[which(proj$TYPE=="ADD" & proj$YEAR == (launch_year+FORLEN) & proj$Scenario == "A")])
      basepop<-sum(proj$Freq[which(proj$TYPE=="BASE" & proj$YEAR == (launch_year) & proj$Scenario == "A")])
      
      if (addsum >= basepop){
        proj2 <- subset(proj, TYPE %in% c("ADD", "BASE"))
      }else{
          proj2 <- subset(proj, TYPE %in% c("MULT", "BASE"))}
      
    
    #if(!is.null(proj)){return(proj)}else {NULL}
    return(proj2)
  }
  , error=function(e){cat(x," ERROR :",conditionMessage(e), "\n")})
}


#d= pblapply(x, project)

d = pbmclapply(x, project, mc.cores = detectCores()-1)
KT <- rbindlist(d)

#write.table(KT, "EVAL/COUNTYnoslope_addmult.csv")

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