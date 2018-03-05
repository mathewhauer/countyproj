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

test_year = 2000
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

# samp <- filter(CCRs, STATE == "13")
# samp <- unique(samp$GEOID)
samp <- unique(K05_launch$GEOID)

# this.county = "49033" # "38105" "12119 "38053"
# samp <- "04001" "02158" "36109" "13117" "48301"
# x = this.county
x = unlist(list(paste0(samp, "_TOTAL")))

# z<-filter(CCRs, GEOID == samp, SEX == "FEMALE")
# predict(ucm(ccr01~ccr17, data = z)$model, n.ahead = FORLEN, interval = "prediction", level = setlevel)


predccr = function(ccr, sex, x, DF){
  #hyndtran = function(ccr,DF){log((ccr - a) / (b - ccr))}
  hyndtran = function(ccr, DF){((ccr-a)/1)+a}
  #a <- quantile(DF[[as.character(ccr)]][which(DF$countyrace==x & DF$SEX == sex)], .5)
  a <- quantile(DF[[as.character(ccr)]][which(DF$YEAR==launch_year & DF$SEX == sex)], .5)
  #b <- max(DF[[as.character(ccr)]][which(DF$countyrace== x)])*1.01
  #a <- -0.00000000000001
  
  y <-as_data_frame(hyndtran(DF[[as.character(ccr)]][which(DF$countyrace== x & DF$SEX == sex )]))
  
  num<- seq(1,FORLEN,5)
  pred<- tryCatch(predict(ucm(value~0, data = y, level = TRUE, slope = FALSE)$model, n.ahead = FORLEN, interval = "prediction", level = setlevel)[c(num),]
                  , error=function(e) array(0, c(STEPS, 3)))
  #pred2 <-(b-a)*exp(pred)/(1+exp(pred))+a
  return(pred)#, cycle = TRUE, cycle.period = cyc
}

predcwr = function(ccr, sex, x, DF){
  hyndtran = function(ccr,DF){log((ccr - a) / (b - ccr))}
  b <- max(DF[[as.character(ccr)]][which(DF$countyrace== x)])*1.01
  a <- -0.0000000001
  y <-as_data_frame(hyndtran(DF[[as.character(ccr)]][which(DF$countyrace== x & DF$SEX == sex)]))
  
  num<- seq(1,FORLEN,5)
  pred<- predict(ucm(value~0, data = y, level = TRUE, slope = FALSE)$model, n.ahead = FORLEN, interval = "prediction", level = setlevel)[c(num),]
  pred2 <-(b-a)*exp(pred)/(1+exp(pred))+a
  return(pred2)#
}

arimaccr = function(ccr, sex, x, DF){
  num<- seq(1,FORLEN,5)
  as.data.frame(forecast(auto.arima(DF[[as.character(ccr)]][which(DF$countyrace == x & DF$SEX== sex)], allowdrift=FALSE, lambda=0), h= FORLEN))[c(num),]
  
}


# z2<- filter(CCRs, countyrace ==x)
# z<- filter(K05_pop, countyrace == x)
# z<- filter(childbearing, countyrace ==x)

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
    
    
    
    for (i in 1:STEPS){
      namm<-paste0("lx", i, "m")
      namf<-paste0("lx",i,"f")
      assign(namm, rbind(BA00m[i,],BA01m[i,],BA02m[i,], BA03m[i,], BA04m[i,], BA05m[i,], BA06m[i,], BA07m[i,], BA08m[i,], BA09m[i,], BA10m[i,]
                         , BA11m[i,], BA12m[i,], BA13m[i,], BA14m[i,], BA15m[i,], BA16m[i,]))
      assign(namf, rbind(BA00f[i,],BA01f[i,],BA02f[i,], BA03f[i,], BA04f[i,], BA05f[i,], BA06f[i,], BA07f[i,], BA08f[i,], BA09f[i,], BA10f[i,]
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
KT2 <- KT %>%
  mutate(AGEGRP = as.numeric(substr(Var1, 2,3))) %>%
  group_by(YEAR, countyrace, SEX, AGEGRP) %>%
  spread(Var3, Freq)

z<- left_join(K05_pop, KT2) 
z<- z %>%
  filter(YEAR>2000) %>%
  na.omit

write.table(z, "DATA/EVAL_countylevel_UCMnoslope.csv")

tot <- z %>%
  group_by(countyrace, YEAR) %>%
  summarise(population = sum(population),
            A = sum(A),
            B = sum(B),
            C = sum(C)) %>%
  mutate(FLAG1 = if_else(is.na((A/population)-1), 0,(A/population)-1),
         FLAG2 = if_else(population>=B & population<=C,1,0),
         DENS = densCols(population, FLAG1, colramp = colorRampPalette(rev(rainbow(10, end = 4/6))))) %>%
  ungroup()

eval_ucm_tot <- tot %>%
    group_by(YEAR) %>%
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
p2<-add_sub(p1, paste0("", length(unique(z$countyrace)), " counties based on CCRs from 1980-2000 evaluated at 2005, 2010, and 2015 \nusing Unobserved Component Models for each individual age/sex group.\n no slope CCRs, slope CWRs"))
ggdraw(p2)