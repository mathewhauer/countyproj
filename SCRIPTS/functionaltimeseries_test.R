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
library(tidyverse)
library(devtools)
library(ftsa)
library(rainbow)
# install_github('nathanvan/parallelsugar')

start_time <-Sys.time()

set.seed(100)


GROUPING <- c("STATE", "COUNTY", "YEAR", "AGEGRP", "SEX")

test_year = 2016
K05_pop <- read_csv("DATA/cendatbase.csv") %>%
  group_by(.dots = GROUPING) %>%
  summarise(population = sum(population))
K05_pop$GEOID <- paste0(K05_pop$STATE, K05_pop$COUNTY)
K05_pop$countyrace <- paste0(K05_pop$GEOID, "_TOTAL")

z<- K05_pop %>%
  filter(GEOID == "01001", SEX =="MALE")
hyndtran = function(ccr,DF){log((ccr - a) / (b - ccr))}
a=0
CCRs<- z %>%
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
  filter(YEAR >= min(YEAR +5, na.rm=T) & YEAR <= test_year) %>%
  dplyr::select(YEAR, ccr01:ccr17) %>%
  gather(age, ccr, ccr01:ccr17) %>%
  spread(YEAR, ccr)
zz<-t(CCRs)
CCRs$countyrace <- paste0(CCRs$GEOID, "_TOTAL")

y<- fts(x = 1:17, y = CCRs[,-1])
fts1<- rainbow::fts(x = 1:17, y = CCRs[,-1], xname = "Age", yname ="Year")




plot(forecast(ftsm(Australiasmoothfertility, order=2), h=20), "components")

y<-Australiasmoothfertility
yy<- y$y
plot(forecast(ftsm(y, order=2), h=20), "components")
colnames(fts1$y) <- seq_along(colnames(fts1$y))
plot(forecast(ftsm(fts1, order=2), h=20), "components")


plot(fts1, col = gray(0.8), xlab = "Age",
     ylab = "Count of live birth (per 1,000 females)",
     main = "Forecasted fertility rates (2007-2026)")
# Plot the forecasts in rainbow color for Fig. 4(a)
plot(forecast(ftsm(fts1, weight=TRUE, beta = 0.1), h = 10), add = TRUE)
legend("topright", c("2007", "2026"), col = c("red", "blue"), lty = 1)

aus = forecast(ftsm(fts1), h = 10)
plot(aus, ylim = c(0, 2))
# Plot the lower and upper bounds
lines(aus$lower, col = 2); lines(aus$upper, col = 2)

zz<-ftsmiterativeforecasts(object = fts1, components = 1, iteration = 50)

plot(ftsmiterativeforecasts(object = fts1, components = 1, iteration = 50))