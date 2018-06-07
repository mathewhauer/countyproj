###------DATA LOAD-----
## @knitr projbasedata

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

K05_launch <- K05_pop[which(K05_pop$YEAR == launch_year),] %>%
  group_by(STATE, COUNTY, GEOID, YEAR) %>%
  dplyr::summarise(POPULATION = sum(POPULATION)) %>%
  ungroup()

K05_launch2 <- K05_pop[which(K05_pop$YEAR %in% c(launch_year, launch_year+5, launch_year+10, launch_year+15)),]
K05_launch2$COUNTYRACE <- paste0(K05_launch2$GEOID, "_", K05_launch2$RACE)
K05_launch2$Var1 = paste0("a", K05_launch2$AGE)

counties <- counties(cb = TRUE)
counties <- spTransform(counties, CRS("+init=epsg:2163")) %>%
  subset(!(STATEFP %in% c("60", "64","66", "68", "69", "70", "74","72", "78")))
states <- states(cb=TRUE)
