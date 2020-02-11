#############################################################################################################
# Hierarchical Bayes Approach to Adjust for Selection Bias in Before-After Analyses of Vision Zero Policies #
#############################################################################################################
#AUTHORS: Jonathan Auerbach, Christopher Eshleman and Rob Trangucci
#CONTACT: jla2167@columbia.edu
#DATE:    January 2017

#This R code generates results and figures in the article: A Hierarchical Bayes Approach 
## to Adjust for Selection Bias in Before_After Analyses of Vision Zero Policies.
## It is divided into 12 steps, which must be run in order. This code was written using 
## R 3.3 and RStan 2.14.

#Large datasets, mostly shapefiles, cannot be included on GitHub, and instructions 
## are provided to download them in the comments. They can also be obtained by 
## contacting the authors at the email address above.

############
# 1. SETUP #
############

packages <- c("parallel", "data.table", "bit64", "dplyr", "plyr", "RCurl",
              "ggplot2", "ggmap","rstan", "rgdal", "maptools", "reshape2", 
              "rgeos", "scales", "raster", "magrittr")
lapply(packages, require, character.only = TRUE)

#NB as of 2018, the ggmap function get_map requires an API key from Google
## see README.md
## or see https://www.r-bloggers.com/geocoding-with-ggmap-and-the-google-api/

#####################################
# 2. REGRESSION TO THE MEAN EXAMPLE #
#####################################

#Download Vision Zero Priority Roads Shapefiles
#http://www.nyc.gov/html/dot/html/about/vz_datafeeds.shtml
#meta http://www.nyc.gov/html/dot/downloads/pdf/vision-zero-view-metadata.pdf
zones <- readOGR("data/Vision_Zero/vz_priority_zones_shapefile",
                 "vz_priority_zones")
zones@data$id <- rownames(zones@data)
zones_df <- fortify(zones)
corridors <- readOGR("data/Vision_Zero/vz_priority_corridors_shapefile",
                     "vz_priority_corridors")
corridors@data$id <- rownames(corridors@data)
corridors_df <- fortify(corridors)
intersections <- readOGR("data/Vision_Zero/vz_priority_intersections_shapefile",
                         "vz_priority_intersections")

###street shapefile
#http://gis.ny.gov/gisdata/inventories/details.cfm?DSID=932
#meta http://gis.ny.gov/gisdata/metadata/nysgis.streets_shp.xml
streets <- readOGR("data/Vision_Zero/Streets_shp/","StreetSegment")
streets <- spTransform(streets, corridors@proj4string)
pts_streets <- getSpatialLinesMidPoints(streets)
nyc_bbox <- corridors@bbox
nyc_locs <- pts_streets@coords[,1] > nyc_bbox[1,1] &
  pts_streets@coords[,1] < nyc_bbox[1,2] &
  pts_streets@coords[,2] > nyc_bbox[2,1] &
  pts_streets@coords[,2] < nyc_bbox[2,2]
nyc_streets <- streets[nyc_locs,]
nyc_streets <- nyc_streets[
  nyc_streets@data$LeftCounty %in% c("Bronx","Kings","New York","Queens","Richmond"),]
nyc_streets@data$RoadID <- 1:nrow(nyc_streets)
nyc_streets@data$id <- rownames(nyc_streets)
rm(streets)
nyc_streets_df <- fortify(nyc_streets)

nyc <- get_map("Empire State Building", zoom = 14, maptype ="terrain-lines")
bkn <- get_map("Atlantic Terminal Brooklyn", zoom = 14, maptype ="terrain-lines")
sid <- get_map("Staten Island", zoom = 14, maptype ="terrain-lines")
qns <- get_map("Jamaica Queens", zoom = 14, maptype ="terrain-lines")
gid <- get_map("Governor's Island New York City", zoom = 14, maptype ="terrain-lines")
ggmap(qns) + theme_minimal(14) +
  geom_path(aes(long,lat, group = group, color = factor(cos(as.numeric(id)))),
            data = nyc_streets_df) +
  theme(legend.position = "none")

pts_streets <- getSpatialLinesMidPoints(nyc_streets)
street_zones <- !is.na(over(pts_streets, zones)$id)
street_corridor <- apply(gDistance(pts_streets, corridors, byid=TRUE),2,min) < .0005
street_intersections <- apply(gDistance(pts_streets, intersections, byid=TRUE),2,min) < .0005
street_priority <- street_zones | street_corridor | street_intersections

deaths_nyc <- read.csv("http://www.nyc.gov/html/dot/downloads/misc/fatality_yearly.csv")
deaths_nyc <- deaths_nyc[deaths_nyc$PedFatalit>0 & deaths_nyc$YR < 2018,]

ggmap(gid, base_layer=ggplot(aes(x=long,y=lat), data=zones_df),
      extent = "normal", maprange = FALSE) + 
  coord_map(projection="mercator", 
            xlim=c(attr(gid, "bb")$ll.lon, attr(gid, "bb")$ur.lon),
            ylim=c(attr(gid, "bb")$ll.lat, attr(gid, "bb")$ur.lat)) + 
  theme_nothing() +
  geom_polygon(aes(long, lat, group = group), data = zones_df, alpha = .2, fill = "blue") +
  geom_point(aes(coords.x1,coords.x2), data = data.frame(intersections@coords),
             color = "blue", shape = 2) + 
  geom_path(aes(long,lat, group = group), color = "blue",
            data = corridors_df) +
  geom_point(aes(long,lat),data=data.frame(long = -74.0183945, lat = 40.68986),
             color = "red")

deaths_nyc$nodeX[is.na(deaths_nyc$nodeX) | is.na(deaths_nyc$nodeY)] <- 979148.8
deaths_nyc$nodeY[is.na(deaths_nyc$nodeX) | is.na(deaths_nyc$nodeY)] <- 190611.6

NAD83 <- "+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 
+lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80
+towgs84=0,0,0 "
pts_nyc <- SpatialPoints(cbind(deaths_nyc$nodeX,deaths_nyc$nodeY),
                         CRS(NAD83))
pts_nyc <- spTransform(pts_nyc, corridors@proj4string)
deaths_nyc$LONGITUD <- pts_nyc@coords[,1]
deaths_nyc$LATITUDE <- pts_nyc@coords[,2]

pts_nyc[deaths_nyc$LONGITUD < corridors@bbox[1,1] | 
          deaths_nyc$LONGITUD > corridors@bbox[1,2] |
          deaths_nyc$LATITUDE < corridors@bbox[2,1] | 
          deaths_nyc$LATITUDE > corridors@bbox[2,2],]@coords

ggmap(get_map(location = c(-73.90410,40.90686), zoom = 16)) +
  geom_point(aes(lon,lat),data = data.frame(lon = -73.90410, lat = 40.90686))
ggmap(get_map(location = c(-73.90322,40.91013), zoom = 16)) +
  geom_point(aes(lon,lat),data = data.frame(lon = -73.90322, lat = 40.91013))

deaths_nyc$Zones <- factor(as.numeric(is.na(over(pts_nyc,zones)[,4])),labels = c("Zone","No Zone"))
deaths_nyc$Intersection <-  ifelse(apply(gDistance(pts_nyc, intersections, byid=TRUE),2,min) < .0005,
                                   "Intersection","No Intersection")
deaths_nyc$Corridor <-  ifelse(apply(gDistance(pts_nyc, corridors, 
                                               byid=TRUE),2,min) < .0005,
                               "Corridor","No Corridor")
deaths_nyc$Priority <-  ifelse( deaths_nyc$Zones == "Zone" |
                                  deaths_nyc$Intersection == "Intersection" |
                                  deaths_nyc$Corridor == "Corridor", "Priority","Regular")
deaths_nyc$RoadID <- apply(gDistance(pts_nyc,  nyc_streets, byid=TRUE), 2, which.min)
deaths_nyc$MinDiff <- apply(gDistance(pts_nyc, nyc_streets, byid=TRUE), 2, min)

aggregate(PedFatalit ~ YR, deaths_nyc[deaths_nyc$Priority == "Priority",], sum)
aggregate(PedFatalit ~ YR, deaths_nyc, sum)
round(mean(c(99,93,92,101,114))) 
table(street_priority) 

nonprior <-
  c(158,153,142,151,184,140,139,145) -
  aggregate(PedFatalit ~ YR, deaths_nyc[deaths_nyc$Priority == "Priority",], sum)$Ped
(nonprior[8] - mean(nonprior[1:5]))/mean(nonprior[1:5])

(145 - mean(c(158,153,142,151,184)))/mean(c(158,153,142,151,184))

road_deaths <-
  plyr::join(data.frame(RoadID = unique(nyc_streets@data$RoadID)),
             aggregate(PedFatalit ~ RoadID,deaths_nyc[deaths_nyc$YR < 2014,
                                                      ], sum))
road_deaths$PedFatalit[is.na(road_deaths$PedFatalit)] <- 0
table(road_deaths$PedFatalit)

emp_bayes <- 
  (1:(length(table(road_deaths$PedFatalit)) - 1) * 
     table(road_deaths$PedFatalit)[-1])/
  table(road_deaths$PedFatalit)[-length(table(road_deaths$PedFatalit))]
names(emp_bayes) <- 0:(length(emp_bayes) - 1)
emp_bayes

sum(as.numeric(table(road_deaths$PedFatalit))[-length(table(road_deaths$PedFatalit))] * 
      emp_bayes) / length(2009:2013)
sum(deaths_nyc$PedFatalit[deaths_nyc$YR == 2016])
(128-145)/145

road_deaths <-
  plyr::join(data.frame(RoadID = unique(nyc_streets@data$RoadID[street_priority])),
             aggregate(PedFatalit ~ RoadID,deaths_nyc[deaths_nyc$YR < 2014 &
                                                        deaths_nyc$Priority == "Priority",], sum))
road_deaths$PedFatalit[is.na(road_deaths$PedFatalit)] <- 0
sum(as.numeric(table(road_deaths$PedFatalit))[-length(table(road_deaths$PedFatalit))] * 
      emp_bayes[1:4]) / length(2009:2013)
sum(deaths_nyc$PedFatalit[deaths_nyc$YR == 2016 & deaths_nyc$Priority == "Priority"])
(73-52)/52

deaths_nyc$Study_Period <- factor(deaths_nyc$YR > 2013, labels = c("2009-2013","2016"))
ggmap(nyc, base_layer=ggplot(aes(x=long,y=lat), data=zones_df),
      extent = "normal", maprange = FALSE) + 
  coord_map(projection="mercator", 
            xlim=c(attr(nyc, "bb")$ll.lon, attr(nyc, "bb")$ur.lon),
            ylim=c(attr(nyc, "bb")$ll.lat, attr(nyc, "bb")$ur.lat)) + 
  theme_minimal() +
  geom_polygon(aes(long, lat, group = group), data = zones_df, alpha = .2, fill = "blue") +
  geom_point(aes(coords.x1,coords.x2), data = data.frame(intersections@coords),
             color = "blue", shape = 2) + 
  geom_point(aes(LONGITUD,LATITUDE, color = Priority), 
             data = deaths_nyc[deaths_nyc$YR %in% c(2009:2013,2016), ]) +
  geom_path(aes(long,lat, group = group), color = "blue",
            data = corridors_df) +
  theme(legend.position = "none",
        axis.text = element_blank()) +
  labs(x="",y="") +
  scale_color_manual(values = c("dark blue", "red")) +
  facet_wrap(~Study_Period)

ggplot(rbind(aggregate(PedFatalit ~ YR + Priority, deaths_nyc, sum),
             data.frame(aggregate(PedFatalit ~ YR, deaths_nyc, sum),Priority = "Total"))) +
  theme_minimal() +
  aes(factor(YR),weight = PedFatalit,fill=Priority) +
  geom_bar() +
  theme(legend.position = "none") +
  labs(x="",y="") +
  scale_x_discrete(breaks = c("2009","2011","2013","2015")) +
  facet_grid(~factor(Priority,labels=c("Priority Roads","Non Priority Roads","All Roads")))

agg <- rbind(aggregate(PedFatalit ~ YR + Priority, deaths_nyc, sum),
             data.frame(aggregate(PedFatalit ~ YR, deaths_nyc, sum),Priority = "Total"))
before <- aggregate(PedFatalit ~ YR + Priority,
                    agg[agg$YR < 2014,], sum)
before <- aggregate(PedFatalit ~ Priority, before, mean)
before$YR <- 2008.5
after <- aggregate(PedFatalit ~ YR + Priority,
                   agg[agg$YR >= 2014,], sum)
after <- aggregate(PedFatalit ~ Priority, after, mean)
after$YR <- 2013.5

before2 <- aggregate(PedFatalit ~ YR + Priority,
                     agg[agg$YR < 2013,], sum)
before2 <- aggregate(PedFatalit ~ Priority, before2, mean)
before2$YR <- 2008.5

after2 <- aggregate(PedFatalit ~ YR + Priority,
                    agg[agg$YR == 2016,], sum)
after2 <- aggregate(PedFatalit ~ Priority, after2, mean)
after2$YR <- 2015.5

ggplot(agg) +
  theme_minimal() +
  aes(YR,weight = PedFatalit,fill=Priority) +
  geom_bar() +
  theme(legend.position = "none") +
  labs(x="",y="") +
  geom_segment(aes(y = PedFatalit, yend = PedFatalit, 
                   x = YR, xend = YR+5, group = Priority), data = before) +
  geom_segment(aes(y = PedFatalit, yend = PedFatalit, 
                   x = YR, xend = YR+1, group = Priority), data = after2) +
  scale_x_continuous(breaks = c(2009,2011,2013,2015)) +
  facet_grid(~factor(Priority,labels=c("Priority Roads","Non Priority Roads","All Roads"))) 

before$SE <- c(
  exp(summary(glm(agg$PedFatalit[
    agg$Priority == "Priority" & agg$YR < 2014]~1,
    family = "poisson"))$coefficients[, 2]),
  exp(summary(glm(agg$PedFatalit[
    agg$Priority == "Regular" & agg$YR < 2014]~1,
    family = "poisson"))$coefficients[, 2]),
  exp(summary(glm(agg$PedFatalit[
    agg$Priority == "Total" & agg$YR < 2014]~1,
    family = "poisson"))$coefficients[, 2]))

after2$SE <- c(
  exp(summary(glm(after2$PedFatalit[1]~1,family = "poisson"))$coefficients[, 2]),
  exp(summary(glm(after2$PedFatalit[2]~1,family = "poisson"))$coefficients[, 2]),
  exp(summary(glm(after2$PedFatalit[3]~1,family = "poisson"))$coefficients[, 2]))

ggplot(agg) +
  theme_minimal() +
  aes(YR,weight = PedFatalit,fill=Priority) +
  geom_bar() +
  theme(legend.position = "none") +
  labs(x="",y="") +
  geom_segment(aes(y = PedFatalit, yend = PedFatalit, 
                   x = YR, xend = YR+5, group = Priority), data = before) +
  geom_segment(aes(y = PedFatalit - 1.95 * SE, yend = PedFatalit - 1.95 * SE, 
                   x = YR, xend = YR+5, group = Priority), data = before,
               linetype = 2) +
  geom_segment(aes(y = PedFatalit + 1.95 * SE, yend = PedFatalit + 1.95 * SE, 
                   x = YR, xend = YR+5, group = Priority), data = before,
               linetype = 2) +
  geom_segment(aes(y = PedFatalit, yend = PedFatalit, 
                   x = YR, xend = YR+1, group = Priority), data = after2) +
  geom_segment(aes(y = PedFatalit - 1.95 * SE, yend = PedFatalit - 1.95 * SE, 
                   x = YR, xend = YR+1, group = Priority), data = after2,
               linetype = 2) +
  geom_segment(aes(y = PedFatalit + 1.95 * SE, yend = PedFatalit + 1.95 * SE, 
                   x = YR, xend = YR+1, group = Priority), data = after2,
               linetype = 2) +
  scale_x_continuous(breaks = c(2009,2011,2013,2015)) +
  facet_grid(~factor(Priority,labels=c("Priority Roads","Non Priority Roads","All Roads"))) 

(agg$PedFatalit[agg$Priority == "Priority" & agg$YR == 2016] -
    before$PedFatalit[before$Priority == "Priority"])/
  before$PedFatalit[before$Priority == "Priority"]

(after$PedFatalit[before$Priority == "Priority"] -
    before$PedFatalit[before$Priority == "Priority"])/
  before$PedFatalit[before$Priority == "Priority"]

(agg$PedFatalit[agg$Priority == "Regular" & agg$YR == 2016] -
    before$PedFatalit[before$Priority == "Regular"])/
  before$PedFatalit[before$Priority == "Regular"]

(after$PedFatalit[before$Priority == "Regular"] -
    before$PedFatalit[before$Priority == "Regular"])/
  before$PedFatalit[before$Priority == "Regular"]

(agg$PedFatalit[agg$Priority == "Total" & agg$YR == 2016] -
    before$PedFatalit[before$Priority == "Total"])/
  before$PedFatalit[before$Priority == "Total"]

(after$PedFatalit[before$Priority == "Total"] -
    before$PedFatalit[before$Priority == "Total"])/
  before$PedFatalit[before$Priority == "Total"]

deaths_nyc$Study_Period <- factor(deaths_nyc$YR > 2013, labels = c("2013","2016"))
ggmap(nyc, base_layer=ggplot(aes(x=long,y=lat), data=zones_df),
      extent = "normal", maprange = FALSE) + 
  coord_map(projection="mercator", 
            xlim=c(attr(nyc, "bb")$ll.lon, attr(nyc, "bb")$ur.lon),
            ylim=c(attr(nyc, "bb")$ll.lat, attr(nyc, "bb")$ur.lat)) + 
  theme_minimal() +
  geom_polygon(aes(long, lat, group = group), data = zones_df, alpha = .2, fill = "blue") +
  geom_point(aes(coords.x1,coords.x2), data = data.frame(intersections@coords),
             color = "blue", shape = 2) + 
  geom_point(aes(LONGITUD,LATITUDE, color = Priority), 
             data = deaths_nyc[deaths_nyc$YR %in% c(2013,2016), ]) +
  geom_path(aes(long,lat, group = group), color = "blue",
            data = corridors_df) +
  theme(legend.position = "none",
        axis.text = element_blank()) +
  labs(x="",y="") +
  scale_color_manual(values = c("dark blue", "red")) +
  facet_wrap(~Study_Period)

######################################################
# 3. JOIN FARS ACCIDENT, VEHICLE, AND PERSON RECORDS #
######################################################
#Download FARS SAS files from FTP site: 
## https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars
#  convert to .csv using SAS

accs = list()
years = 2010:2015
for (year_i in seq_along(years)) {
  year = years[year_i]
  accs[[year_i]] = fread(paste0("data/FARS_2010_2015/ACCIDENT_",
                                year, "_fars_sas_full.csv")) %>%
    data.frame() %>%
    select(ST_CASE,
           CITY, 
           STATE,
           MONTH, 
           YEAR, 
           DAY_WEEK, 
           HOUR, 
           WEATHER, 
           ROUTE, 
           TYP_INT, 
           LGT_COND,
           WRK_ZONE,
           NOT_HOUR,
           NOT_MIN,
           ARR_HOUR,
           ARR_MIN,
           HOSP_HR,
           HOSP_MN,
           WEATHER1,
           WEATHER2,
           LATITUDE,
           LONGITUD) %>%
    filter(CITY %in% c(1670,3290,4170,0120,1980,0330,
                       1650,1960,3340,3260,0600) | STATE == 11)
}
dts_acc <- bind_rows(accs) %>% mutate(
  unique_id = paste0(ST_CASE,YEAR))

dts_acc <- dts_acc[dts_acc$LONGITUD > -124.848974 & 
                   dts_acc$LONGITUD < -66.885444  &
                   dts_acc$LATITUDE >  24.396308  & 
                   dts_acc$LATITUDE <  49.384358, ]

vehs = list()
years = 2010:2015
for (year_i in seq_along(years)) {
  year = years[year_i]
  vehs[[year_i]] = fread(paste0("data/FARS_2010_2015/VEHICLE_",
                                year, "_fars_sas_full.csv")) %>%
    data.frame() %>%
    select(ST_CASE, 
           VEH_NO,
           VSPD_LIM,
           VTRAFWAY,
           VNUM_LAN,
           VALIGN,
           VPROFILE,
           VPAVETYP,
           VSURCOND,
           VTRAFCON,
           VTCONT_F,
           SPEEDREL) %>%
    mutate(
      YEAR = year,
      unique_id = paste0(ST_CASE,YEAR)
    ) %>%
    filter(unique_id %in% dts_acc$unique_id)
}
dts_veh <- bind_rows(vehs)

pers = list()
years = 2010:2015
for (year_i in seq_along(years)) {
  year = years[year_i]
  pers[[year_i]] = fread(paste0("data/FARS_2010_2015/PERSON_",
                                year, "_fars_sas_full.csv")) %>%
    data.frame() %>%
    select(ST_CASE, PER_TYP, INJ_SEV, VEH_NO, HISPANIC, 
           RACE, AGE, DRINKING, WORK_INJ) %>%
    mutate(
      YEAR = year,
      unique_id = paste0(ST_CASE,YEAR)
    ) %>%
    filter(unique_id %in% dts_acc$unique_id)
}
dts_per <- bind_rows(pers)
peds <- dts_per %>% filter(PER_TYP %in% c(5,6))

jnd <- peds %>% 
  left_join(dts_acc, by = c("unique_id","ST_CASE","YEAR"))

dts_veh_cln <- dts_veh %>% group_by(unique_id) %>% do(head(.,1))
jnd <- jnd %>% left_join(dts_veh_cln,by=c("ST_CASE","unique_id","YEAR"))
jnd <- plyr::join(jnd,
                  data.frame(REGION = c(2,4,1,1,4,3,4,4,4,4,3,4),
                            CITY = c(1670,3290,4170,120,1980,330,
                                     1650,1960,3340,3260,10,600)),
                  by = "CITY")

######################################################
# 4. ESTIMATE PEDESTRIAN POPULATION FROM CENSUS ACS  #
######################################################
# Download Census Planning Database file from FTP site (tract level): 
## https://www.census.gov/research/data/planning_database/2014/
# Download Census Transportation Planning Product (5YR_CTPP, tract level) 
## https://www.fhwa.dot.gov/planning/census_issues/ctpp/data_products/2006-2010_tract_flows/
## convert to .txt using Microsoft Access
# Tract is found using the Census Geocoder API:
## https://www.census.gov/geo/maps-data/data/geocoder.html
# Population is computed as described here:
## https://www.census.gov/topics/employment/commuting/guidance/calculations.html

pdb2014trv9_us <- fread("data/PDB_Tract_2014/pdb2014trv9_us.csv")
ctpp <- read.csv("data/CTPP_2006_2010/Tract-flows.txt")
ctpp_from <- aggregate(EST ~ Residence_State_FIPS_Code +
                             Residence_County_FIPS_Code +
                             Residence_Tract_FIPS_Code,
                       data = ctpp,
                       FUN = sum)

ctpp_to  <- aggregate(EST ~ Workplace_State_FIPS_Code  +
                            Workplace_County_FIPS_Code + 
                            Workplace_Tract_FIPS_Code,
                      data = ctpp,
                      FUN = sum)

colnames(ctpp_to) <- c("State","County","Tract","CTPP_TO")
colnames(ctpp_from) <- c("State","County","Tract","CTPP_FROM")
pdb2014trv9_us <- join(pdb2014trv9_us,ctpp_to)
pdb2014trv9_us <- join(pdb2014trv9_us,ctpp_from)
pdb2014trv9_us$CTPP_TO[is.na(pdb2014trv9_us$CTPP_TO)] <- 0
pdb2014trv9_us$CTPP_FROM[is.na(pdb2014trv9_us$CTPP_FROM)] <- 0

reverse_geocode <- function(lon,lat){
  block <- getURL(paste("http://data.fcc.gov/api/block/find?format=json&latitude=",
                        lat,"&longitude=",lon,"&showall=true",sep=""))
  block_id <- regmatches(strsplit(block,":")[[1]][3],
                         gregexpr("[0-9]+",strsplit(block,":")[[1]][3]))[[1]]
  if(length(block_id)==0){cbind(0,0,0)}else{
  pop  <- pdb2014trv9_us[pdb2014trv9_us$GIDTR == as.numeric(substr(block_id,1,11)),]$Tot_Population_CEN_2010
  to   <- pdb2014trv9_us[pdb2014trv9_us$GIDTR == as.numeric(substr(block_id,1,11)),]$CTPP_TO
  from <- pdb2014trv9_us[pdb2014trv9_us$GIDTR == as.numeric(substr(block_id,1,11)),]$CTPP_FROM
  na.omit(cbind(pop,to,from))}
}

population <- matrix(nrow=nrow(jnd),ncol=3)
for(i in 1:nrow(jnd)){
  population[i,] <- reverse_geocode(lat=jnd$LATITUDE[i],
                                    lon=jnd$LONGITUD[i])
}
population <- data.frame(population)
colnames(population) <- c("resident","work_to","work_from")
jnd <- cbind(jnd,population)
jnd$day_pop <-   jnd$resident - jnd$work_from + jnd$work_to 
jnd$night_pop <- jnd$resident
jnd$work <- 1
jnd$work[jnd$DAY_WEEK %in% c(1,7) | jnd$HOUR > 18 | jnd$HOUR < 6] <- 0
jnd$population[jnd$work == 0] <- jnd$night_pop[jnd$work == 0]
jnd$population[jnd$work == 1] <- jnd$day_pop[jnd$work == 1]

#############################################
# 5. ESTIMATE ANNUAL AVERAGE DAILTY TRAFFIC #
#############################################
# Download HPMS Data from FHWA website for each state
# HPMS Data:     http://www.fhwa.dot.gov/policyinformation/hpms/shapefiles.cfm
# Field Manual:  https://www.fhwa.dot.gov/policyinformation/hpms/fieldmanual/
## California 2014 used instead of 2015

city_codes <- data.frame(
  CITY   = c("Chicago","San Francisco","New York City","Boston","Los Angeles", 
             "Austin","Portland","Seattle","San Jose","San Diego",
             "Washington D.C.", "Denver"),
  STATE  = c("illinois","california","newyork","massachusetts","california",
             "texas","oregon","washington","california","california",
             "columbia","colorado"),
  CCode  = c(1670,3290,4170,120,1980,330,1650,1960,3340,3260,10,600),
  SCode  = c(17,6,36,25,6,48,41,53,6,6,11,8))

jnd <- jnd[!is.na(jnd$LATITUDE),]
jnd$aadt <- -1
jnd$slim <- -1

illinois      <- readOGR("data/HPMS_2015/illinois2015",      "Illinois_Sections")
california    <- readOGR("data/HPMS_2015/california2014",    "California")
newyork       <- readOGR("data/HPMS_2015/newyork2015",       "NewYork_Sections")
massachusetts <- readOGR("data/HPMS_2015/massachusetts2015", "Massachusetts_Sections")
texas         <- readOGR("data/HPMS_2015/texas2015",         "Texas_Sections")
oregon        <- readOGR("data/HPMS_2015/oregon2015",        "Oregon_Sections")
columbia      <- readOGR("data/HPMS_2015/district2015",      "District_Sections")
colorado      <- readOGR("data/HPMS_2015/colorado2015",      "Colorado_Sections")

for(city in city_codes$CCode){
  roads <- eval(parse(text=as.character(city_codes$STATE[city_codes$CCode == city  ])))
  points <- jnd[jnd$CITY==city,]
  spat_points <- SpatialPoints(cbind(points$LONGITUD,points$LATITUDE),
                               proj4string = roads@proj4string)
  rdist <- gDistance(roads,spat_points,byid=TRUE)
  aadt <- apply(rdist,1,function(i) roads@data$aadt[which.min(i)])
  slim <- apply(rdist,1,function(i) roads@data$speed_limi[which.min(i)])
  jnd$aadt[jnd$CITY==city] <- aadt
  jnd$slim2[jnd$CITY==city] <- slim
}
jnd$aadt[is.na(jnd$aadt)] <- 0

######################################
# 7. IMPUTE NYC SPEED LIMIT FROM DOT #
######################################
# Download from New York City Open Data Portal
## https://data.cityofnewyork.us/Transportation/VZV_Speed-Limits/7n5j-865y
#http://www.nyc.gov/html/dot/html/about/vz_datafeeds.shtml
#meta http://www.nyc.gov/html/dot/downloads/pdf/vision-zero-view-metadata.pdf

roads <- readOGR("data/Vision_Zero/VZV_Speed Limits/",
                "geo_export_ff4e4fcc-666d-4f91-890a-96472280081b")
points <- jnd[jnd$CITY== 4170, ]
spat_points <- SpatialPoints(cbind(points$LONGITUD,points$LATITUDE),
                             proj4string = slim@proj4string)
rdist <- gDistance(roads, spat_points, byid=TRUE)
slim <- apply(rdist,1,function(i) roads@data$postvz_sl[which.min(i)])
jnd$slim <- -1
jnd$slim[jnd$CITY== 4170] <- slim
jnd$slim[jnd$slim == 25 & jnd$YEAR < 2015] <- 30
jnd$VSPD_LIM_IMP <- ifelse(jnd$VSPD_LIM > 90 & jnd$CITY == 4170, 
                           jnd$slim, jnd$VSPD_LIM)

#################################################
# 7. JOIN GES ACCIDENT, VEHICLE AND PERSON FILE #
#################################################
#Download GES SAS files from FTP site: 
## ftp://ftp.nhtsa.dot.gov/ges/
#  convert to .csv using SAS

accs = list()
years = 2010:2015
for (year_i in seq_along(years)) {
  year = years[year_i]
  accs[[year_i]] = 
    read.csv(paste0("data/GES_2010_2015/ACCIDENT_",year,"_ges_sas_full.csv")) %>%
    data.frame %>%
    dplyr::select(CASENUM,
                  PSU,
                  PSUSTRAT,
                  REGION,
                  STRATUM,
                  WEIGHT,
                  MONTH, 
                  YEAR, 
                  DAY_WEEK, 
                  HOUR, 
                  WEATHER, 
                  TYP_INT, 
                  LGT_COND,
                  WRK_ZONE,
                  WEATHER1,
                  WEATHER2,
                  LAND_USE) %>%
    filter(LAND_USE == 3)
}
dts_acc <- bind_rows(accs)
dts_acc$unique_id = paste0(dts_acc$CASENUM, dts_acc$YEAR)

veh = list()
years = 2010:2015
for (year_i in seq_along(years)) {
  year = years[year_i]
  veh[[year_i]] = 
    read.csv(paste0("data/GES_2010_2015/VEHICLE_",year,"_ges_sas_full.csv")) %>%
    data.frame %>%
    dplyr::select(CASENUM, 
                  VSPD_LIM,
                  VTRAFWAY,
                  VNUM_LAN,
                  VALIGN,
                  VPROFILE,
                  VSURCOND,
                  VTRAFCON,
                  VTCONT_F,
                  SPEEDREL) %>%
    mutate(
      YEAR = year,
      unique_id = paste0(CASENUM,YEAR)
    ) %>%
    filter(unique_id %in% dts_acc$unique_id)
}
dts_veh <- bind_rows(veh)
dts_veh_cln <- dts_veh %>% group_by(unique_id) %>% do(head(.,1))

per = list()
years = 2010:2015
for (year_i in seq_along(years)) {
  year = years[year_i]
  per[[year_i]] = 
    read.csv(paste0("data/GES_2010_2015/PERSON_",year,"_ges_sas_full.csv")) %>%
    data.frame %>%
    dplyr::select(CASENUM, 
                  PSU,
                  PER_TYP, 
                  INJ_SEV) %>%
    mutate(
      YEAR = year,
      unique_id = paste0(CASENUM,YEAR)
    ) %>%
    filter(unique_id %in% dts_acc$unique_id)
}
dts_per <- bind_rows(per)

frm <- formula(WEIGHT ~ TYP_INT + VTRAFWAY + VNUM_LAN +
                 DAY_WEEK + VSURCOND + VTRAFCON + WEATHER + LGT_COND)
peds_num <- dts_per %>% filter(PER_TYP == 5, INJ_SEV == 4)
peds_den <- dts_per
wgt_den <- peds_den %>% left_join(dts_acc, by = c("unique_id","CASENUM","YEAR"))
wgt_den <- wgt_den %>% left_join(dts_veh_cln, by=c("unique_id","CASENUM","YEAR"))
wgt_den <- aggregate(frm, wgt_den, sum)
colnames(wgt_den)[ncol(wgt_den)] <- "den"

wgt_num <- peds_num %>% left_join(dts_acc, by = c("unique_id","CASENUM","YEAR"))
wgt_num <- wgt_num %>% left_join(dts_veh_cln,by=c("CASENUM","unique_id","YEAR"))
wgt_num <- aggregate(frm, wgt_num, sum)
colnames(wgt_num)[ncol(wgt_num)] <- "num"
wgt <- plyr::join(wgt_den, wgt_num)
wgt$num[is.na(wgt$num)] <- 1
jnd <- plyr::join(jnd, wgt)

jnd$COLL <- jnd$den
jnd$FATL <- jnd$num
jnd <- jnd[,!colnames(jnd) %in% c("den","num")]
jnd$COLL[is.na(jnd$COLL)] <- jnd$FATL[is.na(jnd$FATL)] <- 1

peds_num <- dts_per %>% filter(PER_TYP == 5, INJ_SEV == 4,
                               PSU %in% c(3,24,25))
peds_den <- dts_per %>% filter(PSU %in% c(3,24,25))
wgt_den <- peds_den %>% left_join(dts_acc, by = c("unique_id","CASENUM","YEAR"))
wgt_den <- wgt_den %>% left_join(dts_veh_cln,by=c("CASENUM","unique_id","YEAR"))
wgt_den <- aggregate(frm, wgt_den, sum)
colnames(wgt_den)[colnames(wgt_den) == "VSPD_LIM"] <- "VSPD_LIM_IMP"
colnames(wgt_den)[ncol(wgt_den)] <- "den"

wgt_num <- peds_num %>% left_join(dts_acc, by = c("unique_id","CASENUM","YEAR"))
wgt_num <- wgt_num %>% left_join(dts_veh_cln,by=c("CASENUM","unique_id","YEAR"))
wgt_num <- aggregate(frm,wgt_num, sum)
colnames(wgt_num)[colnames(wgt_num) == "VSPD_LIM"] <- "VSPD_LIM_IMP"
colnames(wgt_num)[ncol(wgt_num)] <- "num"

wgt <- plyr::join(wgt_den, wgt_num)
wgt$num[is.na(wgt$num)] <- 1
wgt$wgt_id <- 1:nrow(wgt)

jnd <- plyr::join(jnd, wgt)
jnd$den[is.na(jnd$den)] <- jnd$num[is.na(jnd$num)]  <- 1
jnd$COLL_NYC <- jnd$den
jnd$FATL_NYC <- jnd$num
jnd <- jnd[,!colnames(jnd) %in% c("den","num")]
jnd$COLL_NYC[is.na(jnd$COLL_NYC)] <- jnd$FATL_NYC[is.na(jnd$FATL_NYC)] <- 1

####################
# 8. DATA CLEANING #
####################

jnd %>% filter(
  !(HOUR %in% c(99)) &
    !(WEATHER %in% c(98,99)) &
    !(ROUTE %in% c(8,9)) &
    !(TYP_INT %in% c(8,9,98,99)) &
    !(LGT_COND %in% c(8,9)) &
    !(WRK_ZONE %in% c(8)) &
    !is.na(day_pop) &
    population != 0 &
    !(VSPD_LIM_IMP %in% c(98,99)) &
    !is.na(VSPD_LIM_IMP) &
    !(VTRAFCON %in% c(97,98,99)) &
    !(VTRAFWAY %in% c(8,9)) &
    !(VSURCOND %in% c(98,99)) &
    !(VNUM_LAN %in% c(8,9))
) -> jnd_cln

jnd_cln %>% filter(
  INJ_SEV %in% c(4)
) %>% mutate(
  ik = 1,
  hour_block = if_else(HOUR %in% c(6,7,8,9), 1, 
                       if_else(HOUR %in% c(10,11,12,13,14,15),2,
                               if_else(HOUR %in% c(16,17,18), 3, 
                                       if_else(HOUR %in% c(19,20,21,22),4,5)))),
  route_cust = if_else(ROUTE %in% c(1,2,3),1,2), # 1 if highway, 2 if not
  int_cust = as.integer(TYP_INT != 1), # 1 if intersection, 0 if not
  int_route = interaction(int_cust, route_cust, drop = TRUE),
  wrk_zone_cust = as.integer(WRK_ZONE != 0),
  wet_sur = as.integer(VSURCOND %in% c(2,3,4,6,10,11)),
  built_e = interaction(
    VTRAFWAY,
    VNUM_LAN, 
    drop=TRUE),
  incl_weath = WEATHER %in% c(2,3,4,5,6,8),
  wend = DAY_WEEK %in% c(1,7),
  lgt_time_wend = interaction(LGT_COND, wend, hour_block,drop=TRUE),
  wetsur_incl_weath = interaction(VSURCOND,WEATHER,drop=TRUE),
  sign_signal = interaction(VTRAFCON,wrk_zone_cust,drop=TRUE),
  aadt_disc = cut(pmin(aadt/(24*60),100), c(-1,0,1,10,100))
) %>%
  group_by(
    YEAR,
    CITY,
    VSPD_LIM_IMP,
    built_e,
    int_cust,
    route_cust,
    lgt_time_wend,
    wetsur_incl_weath,
    wend,
    sign_signal,
    hour_block,
    aadt_disc
  ) %>%
  dplyr::summarise(
    counts = n(),
    pops = sum(population),
    VSURCOND = first(VSURCOND),
    WEATHER = first(WEATHER),
    LGT_COND = first(LGT_COND),
    VTRAFWAY = first(VTRAFWAY),
    VNUM_LAN = first(VNUM_LAN),
    COLL = sum(COLL),
    FATL = sum(FATL),
    COLL_NYC = sum(COLL_NYC),
    FATL_NYC = sum(FATL_NYC),
    id = paste(unique(unique_id), collapse = ",")
  ) -> gpd

stan_dat <- gpd %>%
  ungroup() %>%
  mutate(
    sign_signal_ind = as.integer(factor(sign_signal)),
    city_ind = as.integer(factor(CITY)),
    spd_lim_ind = as.integer(factor(VSPD_LIM_IMP)),
    builte_ind = as.integer(factor(built_e)),
    year_ind = as.integer(factor(YEAR)),
    lgt_time_wend_ind = as.integer(factor(lgt_time_wend)),
    wetsur_incl_weath_ind = as.integer(factor(wetsur_incl_weath)),
    route_cust_ind = as.integer(route_cust == 1),
    aadt_disc = as.integer(factor(aadt_disc))
  ) %>% 
  dplyr::rename (
    CITY_NM = CITY,
    CITY = city_ind,
    COND_NM = wetsur_incl_weath,
    COND = wetsur_incl_weath_ind,
    YEAR_NM = YEAR,
    YEAR = year_ind,
    SLIM_NM = VSPD_LIM_IMP,
    SLIM = spd_lim_ind,
    SIGN_NM = sign_signal,
    SIGN = sign_signal_ind,
    LGHT_NM = lgt_time_wend,
    LGHT = lgt_time_wend_ind,
    BLTE_NM = built_e,
    BLTE = builte_ind,
    ROUTE_NM = route_cust,
    ROUTE = route_cust_ind,
    INT = int_cust,
    TFFC = aadt_disc,
    COLL = COLL,
    FATL = FATL,
    COLL_NYC = COLL_NYC,
    FATL_NYC = FATL_NYC,    
    ID = id
  )

hour_offset <- c(4,6,3,4,7)/24
wend_offset <- c(5,2)/7
wend_ind <- stan_dat$wend+1

wend_corr <- wend_offset[wend_ind]
hour_corr <- hour_offset[stan_dat$hour_block]
stan_dat <- stan_dat %>% mutate(EXPR = pops * wend_corr * hour_corr)

####################
# 9. MODEL FITTING #
####################

TEST <- stan_dat$YEAR == 6 
vrbls <- c("BLTE","CITY","COND","LGHT","SIGN","SLIM","TFFC","YEAR")
G <- length(vrbls)
J <- sapply(1:G, function(i)
  length(table(unique(stan_dat[,colnames(stan_dat) == vrbls[i]]))))
stan_dat_list <- with(stan_dat,
                      list(N_train = which.max(TEST)-1,
                           N = nrow(stan_dat),
                           J = J,
                           G = G,
                           BLTE = BLTE,
                           CITY = CITY,
                           COND = COND,
                           LGHT = LGHT,
                           SIGN = SIGN,
                           SLIM = SLIM,
                           YEAR = YEAR,
                           TFFC = TFFC,
                           count = counts,
                           EXPR = EXPR))

model1 <- stan_model(file = 'models/model1.stan')
fit1 <- sampling(model1, data = stan_dat_list, 
                iter = 2000, chains = 4, cores = 4,
                control = list(adapt_delta = 0.99, max_treedepth = 15))

crash_data_int <- stan_dat %>% arrange(YEAR_NM)
col_nms <- sort(Filter(function(x) !grepl('_NM|EXPR|COLL|FATL|COLL_NYC|FATL_NYC|ID',x) &
                         (stringr::str_to_upper(x) == x),names(crash_data_int)))
two_way <- combn(col_nms, 2)
three_way <- combn(col_nms, 3)
interaction_maker <- function(nms, df) {
  df_cp <- df
  interaction = apply(df_cp[,nms],1,function(x) paste(x, collapse='_'))
  interaction = as.integer(as.factor(interaction))
  int_nm = paste(nms,collapse='_')
  return(interaction)
}

tt_2way <- data.frame(apply(two_way, 2, function(x) interaction_maker(x, crash_data_int)))
tt_3way <- data.frame(apply(three_way, 2, function(x) interaction_maker(x, crash_data_int)))
names(tt_2way) <- apply(two_way, 2, function(x) paste(sort(x), collapse='_'))
names(tt_3way) <- apply(three_way, 2, function(x) paste(sort(x), collapse='_'))
df <- bind_cols(crash_data_int[,sort(names(crash_data_int))], tt_2way, tt_3way)
TEST <- df$YEAR == 6 

sign_tester <- function(x) {
  val <- sign(x[1]) == sign(x[2])
  return(val)
}

quantile_tester <- function(vec, probs = c(0.1,0.9)) {
  quants <- as.vector(quantile(vec, probs = probs))
  return(sign_tester(quants))
}

quantiles <- function(samps, probs = c(0.1,0.9)) {
  quants <- as.vector(quantile(vec, probs = probs))
  return(sign_tester(quants))
}

mod_nms <- Filter(function(x) {
  !(x %in% c('TEST','INT','ROUTE')) &
    !grepl('TFFC|_NM|EXPR|VSURCOND|LGT_COND|WEATHER|VTRAFWAY|VNUM_LAN|COLL|FATL|COLL_NYC|FATL_NYC|ID',x) & (stringr::str_to_upper(x) == x) &
    length(stringr::str_extract_all(x, '_')[[1]]) < 2
}, names(df))
mains <- Filter(function(x) !grepl('_',x), mod_nms)
ints <- Filter(function(x) grepl('_',x), mod_nms)
ints <- Filter(function(x) !grepl('YEAR',x),ints)
mod_nms <- c(mains,ints, 'BLTE_INT_ROUTE')
G_int <- length(mod_nms)
J_int <- apply(df[,mod_nms], 2, function(x) length(unique(x)))
stan_dat_int <- aggregate(cbind(counts,EXPR,COLL,FATL,COLL_NYC,FATL_NYC)~., 
                          df[,c('counts','EXPR','FATL','COLL','COLL_NYC','FATL_NYC',mod_nms)], sum) %>% arrange(YEAR)

stan_dat_int$WGHT <- (46/1195) * (1/1) * (1/4) * (1/2) * (stan_dat_int$FATL/stan_dat_int$COLL)
stan_dat_int$WNYC <- (3/1195) * (1/1) * (1/1) * (1/1) * (stan_dat_int$FATL_NYC/stan_dat_int$COLL_NYC)

stan_dat_list_int <- with(stan_dat_int,  
                          list(N_train = which.max(stan_dat_int$YEAR == 6)-1,
                               N = nrow(stan_dat_int),
                               J = J_int,
                               G = G_int,
                               BLTE = BLTE,
                               CITY = CITY,
                               COND = COND,
                               LGHT = LGHT,
                               SIGN = SIGN,
                               SLIM = SLIM,
                               YEAR = YEAR,
                               BLTExCITY = BLTE_CITY,
                               BLTExCOND = BLTE_COND,
                               BLTExINT = BLTE_INT,
                               BLTExLGHT = BLTE_LGHT,
                               BLTExROUTE = BLTE_ROUTE,
                               BLTExSIGN = BLTE_SIGN,
                               BLTExSLIM = BLTE_SLIM,
                               CITYxCOND = CITY_COND,
                               CITYxINT = CITY_INT,
                               CITYxLGHT = CITY_LGHT,
                               CITYxROUTE = CITY_ROUTE,
                               CITYxSIGN = CITY_SIGN,
                               CITYxSLIM = CITY_SLIM,
                               CONDxINT = COND_INT,
                               CONDxLGHT = COND_LGHT,
                               CONDxROUTE = COND_ROUTE,
                               CONDxSIGN = COND_SIGN,
                               CONDxSLIM = COND_SLIM,
                               INTxLGHT = INT_LGHT,
                               INTxROUTE = INT_ROUTE,
                               INTxSIGN = INT_SIGN,
                               INTxSLIM = INT_SLIM,
                               LGHTxROUTE = LGHT_ROUTE,
                               LGHTxSIGN = LGHT_SIGN,
                               LGHTxSLIM = LGHT_SLIM,
                               ROUTExSIGN = ROUTE_SIGN,
                               ROUTExSLIM = ROUTE_SLIM,
                               SIGNxSLIM = SIGN_SLIM,
                               BLTExINTxROUTE = BLTE_INT_ROUTE,
                               ints = as.matrix(stan_dat_int[,mod_nms]),
                               count = counts,
                               EXPR = EXPR))

stan_dat_list_int$n_main <- 7
stan_dat_list_int$n_inter <- G_int - stan_dat_list_int$n_main

model2 <- stan_model(file = 'models/model2.stan')
fit2 <- sampling(model2, data = stan_dat_list_int, 
                 iter = 2000, chains = 4, refresh = 200, cores = 4,
                 control = list(adapt_delta = 0.99, max_treedepth = 15))

samps_year_simple <- rstan::extract(fit_int)
sig_vals_year_simple <- sapply(1:length(J), function(x) {
  eval(parse(text = 
               paste0('which(apply(samps_year_simple$',
                      paste0('e_',x),
                      ',2,quantile_tester))')))
}
,simplify = F)

which(apply(samps_year_simple$cell_e,2,quantile_tester))

sums_year <- data.frame(sds = colMeans(samps_year_simple$sds),
                        se_sds = apply(samps_year_simple$sds,2,sd),
                        nm = c(names(J_int),'cell'),
                        ind = 1:(length(J_int) + 1)) %>%
  arrange(desc(sds))


####################
# 10. MODEL GRAPHS #
####################

output1 <- rstan::extract(fit1)
output2 <- rstan::extract(fit2)
sums_year <- data.frame(sds = colMeans(output2$sds),
                         se_sds = apply(output2$sds,2,sd),
                         nm = c(names(stan_dat_list_int$J),"cell"),
                         ind = 1:(length(stan_dat_list_int$J) + 1)) %>%
  arrange(desc(sds))

nm <- c(names(stan_dat_list_int$J), "cell")
btwn <- c("COND","CITY","SLIM","SIGN","LGHT","BLTE","YEAR",
          "cell","CITY_SLIM", "LGHT_SLIM","BLTE_SLIM",
          "COND_SLIM","COND_LGHT")
inds <- match(btwn, nm)
nms <- sapply(inds, function(x) ifelse(x == 37, "cell_sd", paste0("sd_",x)))
coefs <- data.frame(extract(output2,pars=nms))
coef_ggplot <- data.frame(coef_mean=apply(coefs,2,mean),
                          btwn = btwn)
coef_ggplot$upper50 <- apply(coefs,2,quantile,probs=.75)
coef_ggplot$lower50 <- apply(coefs,2,quantile,probs=.25)
coef_ggplot$upper95 <- apply(coefs,2,quantile,probs=.95)
coef_ggplot$lower95 <- apply(coefs,2,quantile,probs=.05)

ggplot(coef_ggplot, aes(btwn, coef_mean)) + 
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  geom_linerange(aes(ymin = lower50, ymax = upper50),size=2) +
  geom_linerange(aes(ymin = lower95, ymax = upper95)) +
  coord_flip() +
  scale_y_continuous(limits=c(0,1.1*max(coef_ggplot$upper95)),
                     expand = c(0, 0)) +
  labs(y=expression(hat(sigma)(.)),x="",
       title="Analysis of Variance, Between",
       caption = paste("This figure exhibits inner 50 and 90 percent intervals",
                       "for the finite-population standard deviations of each",
                       "batch of explanatory variables. "))

wthn <- c("SLIM","COND","BLTE","LGHT","CITY","BLTE_ROUTE","INT_ROUTE")
nms <- sapply(match(wthn, nm), function(x) paste0("e_",x))
coefs <- extract(output2,pars=nms)
coef_ggplot <- data.frame(wthn = character(),
                          numb = character(),
                          coef_mean=numeric(),
                          upper50 = numeric(),
                          lower50 = numeric(),
                          upper95 = numeric(),
                          lower95 = numeric())
for(var in seq_along(coefs)){
  coefs_temp <- coefs[[var]]
  coef_ggplot_temp <- data.frame(wthn = paste0(wthn[var],"_e"), 
                                 numb = paste0(wthn[var],"_e",1:ncol(coefs_temp)),
                                 coef_mean=apply(coefs_temp,2,mean))
  coef_ggplot_temp$upper50 <- apply(coefs_temp,2,quantile,probs=.75)
  coef_ggplot_temp$lower50 <- apply(coefs_temp,2,quantile,probs=.25)
  coef_ggplot_temp$upper95 <- apply(coefs_temp,2,quantile,probs=.95)
  coef_ggplot_temp$lower95 <- apply(coefs_temp,2,quantile,probs=.05)
  coef_ggplot <- rbind(coef_ggplot,coef_ggplot_temp)
}

ggplot(data.frame(coef_ggplot[coef_ggplot$wthn == "SLIM_e",][5:9,],
                  slim_names = c("20 MPH","25 MPH","30 MPH",
                                 "35 MPH","40 MPH"))) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  aes(numb, lower50 + (upper50 - lower50)/2) + 
  geom_linerange(aes(ymin = lower50, ymax = upper50),size=7) +
  geom_linerange(aes(ymin = lower95, ymax = upper95)) +
  geom_text(aes(label = slim_names),color="white",size=3) +
  coord_flip() +
  scale_x_discrete(breaks = NULL) +
  theme(legend.position = "none") +
  labs(title="Analysis of Variance, Within",
       y=expression(hat(mu)("SLIM")),x="",
       color = "",
       caption = paste("This figure exhibits inner 50 and 90 percent intervals for",
                       "selected speed limit effects. Effects are interpreted as the log",
                       "of the expected multiplicative \nincrease in the fatality rate",
                       "holding all else constant. With this model, slight",
                       "average speed limit effects are observable although \nthese",
                       "differences are likely too small to be meaningful."))

df_city <- unique(stan_dat[,c("CITY","CITY_NM")])
df_city$label <- c(
    "Washington D.C.","Boston","Austin","Denver","Portland","Chicago",
    "Seattle","Los Angeles","San Diego","San Francisco","San Jose",
    "New York City")
df_city <- df_city %>% arrange(CITY)

df_slim <- unique(stan_dat[,c("SLIM_NM","SLIM")]) %>% arrange(SLIM)

ggplot(data.frame(coef_ggplot[coef_ggplot$wthn == "CITY_e",],
                  slim_names = df_city$label)) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  aes(numb, lower50 + (upper50 - lower50)/2) + 
  geom_linerange(aes(ymin = lower50, ymax = upper50),size=7) +
  geom_linerange(aes(ymin = lower95, ymax = upper95)) +
  geom_text(aes(label = slim_names),color="white",size=3) +
  coord_flip() +
  scale_x_discrete(breaks = NULL) +
  theme(legend.position = "none") +
  labs(title="Analysis of Variance, Within",
       y=expression(hat(mu)("CITY")),x="",
       color = "",
       caption = paste("This figure exhibits inner 50 and 90 percent intervals for",
                       "city effects. Effects are interpreted as the log",
                       "of the expected multiplicative \nincrease in the fatality rate",
                       "holding all else constant"))

lght_df <- unique(stan_dat[,c("LGHT","LGT_COND","hour_block","wend")]) %>%
  arrange(LGHT)
lght_df$lgt_label <- dplyr::recode(lght_df$LGT_COND, 
                                   `1` = "Daylgt",
                                   `2` = "Dark No Lgt",
                                   `3` = "Dark Lgt",
                                   `4` = "Dawn",
                                   `5` = "Dusk",
                                   `6` = "Dark Uk Lgt",
                                   `7`= "Other",
                                   `8` = "Not reported",
                                   `9` = "Unknown")

lght_df$hour_label <- dplyr::recode(lght_df$hour_block, 
                                   `1` = "6am-9am",
                                   `2` = "10am-3pm",
                                   `3` = "4pm-6pm",
                                   `4` = "7pm-10pm",
                                   `5` = "11pm-5am")

lght_df$wend_label <- if_else(lght_df$wend, "WkEnd","Wk")
lght_df$label <- apply(lght_df[,c("lgt_label","hour_label","wend_label")],1,function(x) paste(x, collapse=" "))
  
cond_df[sig_vals_year_simple[[4]],]

cond_df <- unique(stan_dat[,c("COND","WEATHER","VSURCOND")]) %>% arrange(COND)
cond_df$weath_label <- dplyr::recode(cond_df$WEATHER,
                                     `1`="Clear",
                                     `2`="Rain",
                                     `3`="Sleet",
                                     `4`="Snow",
                                     `5`="Fog",
                                     `6`="Strong Wind",
                                     `8`="Other",
                                     `10`="Cloudy")
cond_df$sur_label <- dplyr::recode(cond_df$VSURCOND,
                                     `0`="NonTrafficWay",
                                     `1`="Dry",
                                     `2`="Wet",
                                     `3`="Snow",
                                     `4`="Ice",
                                     `5`="Sand",
                                     `6`="Water",
                                     `10`="Slush")
cond_df$label <- apply(cond_df[,c("weath_label","sur_label")],1,function(x) paste(x, collapse=" "))
                                     

blte_df <- unique(stan_dat[,c("BLTE","VTRAFWAY","VNUM_LAN")]) %>% arrange(BLTE)
blte_df$vtrafway_label <- dplyr::recode(blte_df$VTRAFWAY,
                                        `0`="NonTrafficWay",
                                        `1`="2-wayNoMedian",
                                        `2`="2-wayUnprotectedMedian",
                                        `3`="2-wayProtectedMedian",
                                        `4`="1-way",
                                        `5`="2-wayNoMedianLeftTurnLn",
                                        `6`="OnOffRamp")
blte_df$lan_label <- dplyr::recode(blte_df$VNUM_LAN,
                                        `0`="NonTrafficWay",
                                        `1`="1 Lane",
                                        `2`="2 Lanes",
                                        `3`="3 Lanes",
                                        `4`="4 Lanes",
                                        `5`="5 Lanes",
                                        `6`="6 Lanes",
                                        `7`="7+ Lanes")
blte_df$label <- apply(blte_df[,c("vtrafway_label","lan_label")],1,function(x) paste(x, collapse=" "))

blte_route_df <- unique(df[,c("BLTE","ROUTE","BLTE_ROUTE")]) %>% arrange(BLTE)
blte_route_df$BLTE_lab <- blte_df$label[blte_route_df$BLTE]
blte_route_df$ROUTE_lab <- dplyr::recode(blte_route_df$ROUTE,
                                          `0`="Local",
                                          `1`="Highway")
blte_route_df$label <- apply(blte_route_df[,c("BLTE_lab","ROUTE_lab")],1,function(x) paste(x, collapse=" "))

int_route_df <- unique(df[,c("INT","ROUTE","INT_ROUTE")]) %>% arrange(INT_ROUTE)
int_route_df$ROUTE_lab <- dplyr::recode(int_route_df$ROUTE,
                                          `0`="Local",
                                          `1`="Highway")
int_route_df$INT_lab <- dplyr::recode(int_route_df$INT,
                                          `0`="No Int",
                                          `1`="Int")

int_route_df$label <- apply(int_route_df[,c("INT_lab","ROUTE_lab")],1,function(x) paste(x, collapse=" "))
df_year <- unique(stan_dat[,c("YEAR","YEAR_NM")]) %>% arrange(YEAR)
stan_dat_int$city_lab <- df_city$label[stan_dat_int$CITY]
stan_dat_int$year_lab <- df_year$YEAR_NM[stan_dat_int$YEAR]

ggplot(data.frame(coef_ggplot[coef_ggplot$wthn == "LGHT_e",],
                  slim_names = lght_df$label)) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  aes(numb, lower50 + (upper50 - lower50)/2) + 
  geom_linerange(aes(ymin = lower50, ymax = upper50),size=7) +
  geom_linerange(aes(ymin = lower95, ymax = upper95)) +
  geom_text(aes(label = slim_names),color="white",size=3) +
  coord_flip() +
  scale_x_discrete(breaks = NULL) +
  theme(legend.position = "none") +
  labs(title="Analysis of Variance, Within",
       y=expression(hat(mu)("LGHT")),x="",
       color = "",
       caption = paste("This figure exhibits inner 50 and 90 percent intervals for",
                       "lighting effects. Effects are interpreted as the log",
                       "of the expected multiplicative \nincrease in the fatality rate",
                       "holding all else constant"))

ggplot(data.frame(coef_ggplot[coef_ggplot$wthn == "COND_e",],
                  slim_names = cond_df$label)) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  aes(numb, lower50 + (upper50 - lower50)/2) + 
  geom_linerange(aes(ymin = lower50, ymax = upper50),size=7) +
  geom_linerange(aes(ymin = lower95, ymax = upper95)) +
  geom_text(aes(label = slim_names),color="white",size=3) +
  coord_flip() +
  scale_x_discrete(breaks = NULL) +
  theme(legend.position = "none") +
  labs(title="Analysis of Variance, Within",
       y=expression(hat(mu)("COND")),x="",
       color = "",
       caption = paste("This figure exhibits inner 50 and 90 percent intervals for",
                       "the road condition effect. Effects are interpreted as the log",
                       "of the expected multiplicative \nincrease in the fatality rate",
                       "holding all else equal."))

ggplot(data.frame(coef_ggplot[coef_ggplot$wthn == "BLTE_e",],
                  slim_names = blte_df$label)) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  aes(numb, lower50 + (upper50 - lower50)/2) + 
  geom_linerange(aes(ymin = lower50, ymax = upper50),size=7) +
  geom_linerange(aes(ymin = lower95, ymax = upper95)) +
  geom_text(aes(label = slim_names),color="white",size=3) +
  coord_flip() +
  scale_x_discrete(breaks = NULL) +
  theme(legend.position = "none") +
  labs(title="Analysis of Variance, Within",
       y=expression(hat(mu)("BLTE")),x="",
       color = "",
       caption = paste("This figure exhibits inner 50 and 90 percent intervals for",
                       "the built environment effect. Effects are interpreted as the log",
                       "of the expected multiplicative \nincrease in the fatality rate",
                       "holding all else equal."))

ggplot(data.frame(coef_ggplot[coef_ggplot$wthn == "BLTE_ROUTE_e",],
                  slim_names = blte_route_df$label)) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  aes(numb, lower50 + (upper50 - lower50)/2) + 
  geom_linerange(aes(ymin = lower50, ymax = upper50),size=7) +
  geom_linerange(aes(ymin = lower95, ymax = upper95)) +
  geom_text(aes(label = slim_names),color="white",size=3) +
  coord_flip() +
  scale_x_discrete(breaks = NULL) +
  theme(legend.position = "none") +
  labs(title="Analysis of Variance, Within",
       y=expression(hat(mu)("BLTExROUTE")),x="",
       color = "",
       caption = paste("This figure exhibits inner 50 and 90 percent intervals for",
                       "the built environment interacted with road type. Effects are interpreted as the log",
                       "of the expected multiplicative \nincrease in the fatality rate",
                       "holding all else equal"))

ggplot(data.frame(coef_ggplot[coef_ggplot$wthn == "INT_ROUTE_e",],
                  slim_names = int_route_df$label)) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  aes(numb, lower50 + (upper50 - lower50)/2) + 
  geom_linerange(aes(ymin = lower50, ymax = upper50),size=7) +
  geom_linerange(aes(ymin = lower95, ymax = upper95)) +
  geom_text(aes(label = slim_names),color="white",size=3) +
  coord_flip() +
  scale_x_discrete(breaks = NULL) +
  theme(legend.position = "none") +
  labs(title="Analysis of Variance, Within",
       y=expression(hat(mu)("INTxROUTE")),x="",
       color = "",
       caption = paste("This figure exhibits inner 50 and 90 percent intervals for",
                       "intersection and route effects. Effects are interpreted as the log",
                       "of the expected multiplicative \nincrease in the fatality rate",
                       "holding all else constant. "))

#################
# 11. MODEL PPC #
#################

expose_stan_functions(fit2, "poisson_log_trunc_rng")
dat_2015 <- stan_dat_int %>% filter(year_lab == 2015)
pred30 <- apply(extract(output2,c("mu_indiv_pred30"))[[1]], c(1,2), poisson_log_trunc_rng)
city_30 <- apply(pred30[,dat_2015$SLIM == 7],1,sum)
deaths_30 <- sum(dat_2015$counts[dat_2015$SLIM == 7])
pvalue_30 <- paste("p-value:",round(sum(city_30 > deaths_30)/length(city_30),2))

qplot(city_30) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  geom_vline(xintercept = sum(stan_dat$counts[stan_dat$SLIM == 7 & 
                                          stan_dat$YEAR == 6]),
             linetype=2) +
  geom_text(aes(label=pvalue_30),x=80,y=750) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,50), limits = c(0,NA)) +
  labs(title=
"Figure 7.1: Model 1, Posterior Draws of Deaths in 2015 on 30 mph roads (unweighted)",
    x = "", y = "",
    caption = paste("This figure exhibits a histogram of draws from the posterior",
                    "predictive distribution of the total number of fatalities on the",
                    "30 mph road regions in \n2015 set aside as a test set. The dotted",
                    "line represents the observed number of fatalities on those road",
                    "regions. The p-value is the proportion \nof simulations exceeding",
                    "the number observed in 2015. The purpose of this figure is to",
                    "ensure that the model is predictive of the fatality rate \nof the",
                    "roads that did not have their posted speed limits reduced to 25",
                    "mph."))

pred25 <- apply(extract(output2,c("mu_indiv_pred25"))[[1]], c(1,2), poisson_log_trunc_rng)
city_25 <- apply(pred25[,dat_2015$SLIM == 6 &
                         dat_2015$city_lab == "New York City"],1,sum)
deaths_25 <- sum(dat_2015$counts[dat_2015$SLIM == 6 &
                                 dat_2015$city_lab == "New York City"])
pvalue_25 <- paste("p-value:",round(sum(city_25 > deaths_25)/length(city_25),2))

qplot(city_25) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  geom_vline(xintercept = sum(stan_dat$counts[stan_dat$SLIM_NM == 25 & 
                                              stan_dat$YEAR_NM == 2015 &
                                              stan_dat$CITY_NM == 4170 ]),
             linetype=2) +
  geom_text(aes(label=pvalue_25,x=200,y=500)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,50), limits = c(0,NA)) +
  labs(title= 
"Figure 7.2: Model 1, Posterior Draws of Deaths in 2015 on 25 mph roads (unweighted)",
    x = "", y = "",
    caption = paste("This figure exhibits a histogram of draws from the posterior",
                    "predictive distribution of the total number of fatalities on the",
                    "25 mph road regions in \n2015 set aside as a test set. The dotted",
                    "line represents the observed number of fatalities on those road",
                    "regions. The p-value is the proportion \nof simulations exceeding",
                    "the number observed in 2015. The purpose of this figure is to",
                    "ensure that the model is predictive of the fatality rate \nof the",
                    "roads that had their posted speed limits reduced to 25 mph."))

#########################################
# 12. REGRESSION TO THE MEAN CONCLUSION #
#########################################

stan_dat_int$fit <- NA
stan_dat_int$fit[stan_dat_int$YEAR!=6] <- 
  apply(exp(rstan::extract(fit_int,"mu_indiv")[[1]]),2,mean)

stan_ID <-  aggregate(cbind(ID)~.,df[,c("ID",mod_nms)], paste, collapse=",") %>% 
  arrange(YEAR) 
stan_dat_int$ID <- stan_ID$ID
jnd$ID <- jnd$unique_id

final_data <- stan_dat_int[0,]
for(obs in 1:nrow(stan_dat_int)){
  ids <- strsplit(stan_dat_int$ID[obs],",")[[1]]
  for(id in 1:length(ids)) {
    next_row <- stan_dat_int[obs,]
    next_row$ID <- ids[id]
    next_row$WGHT <- next_row$WGHT/length(ids)
    next_row$WNYC <- next_row$WNYC/length(ids)
    next_row$fit <- next_row$fit/length(ids)
    final_data <- rbind(final_data,next_row)
  }
}

results <- left_join(final_data, unique(jnd[,c("ID","LONGITUD","LATITUDE")]), by = "ID")

stan_draws <- exp(rstan::extract(fit_int,"mu_indiv")[[1]])
final_data_draws <- stan_draws[,0]

for(obs in 1:nrow(stan_dat_int[stan_dat_int$YEAR < 6,])){
  ids <- strsplit(stan_dat_int$ID[obs],",")[[1]]
  for(id in 1:length(ids)) {
    next_row <- stan_draws[,obs]
    next_row <- next_row/length(ids)
    final_data_draws <- cbind(final_data_draws,next_row)
  }
}

results_NYC <- results[results$CITY == 12,]
result_pts <- SpatialPoints(cbind(results_NYC$LONGITUD,results_NYC$LATITUDE), 
                            corridors@proj4string)

results_NYC$Zones <- factor(as.numeric(is.na(over(result_pts,zones)[,4])),
                            labels = c("Zone","No Zone"))
results_NYC$Intersection <-  ifelse(apply(gDistance(result_pts, intersections, byid=TRUE),2,min) < .0005,
                                    "Intersection","No Intersection")
results_NYC$Corridor <-  ifelse(apply(gDistance(result_pts, corridors, 
                                                byid=TRUE),2,min) < .0005,
                                "Corridor","No Corridor")
results_NYC$Priority <-  ifelse( results_NYC$Zones == "Zone" |
                                   results_NYC$Intersection == "Intersection" |
                                   results_NYC$Corridor == "Corridor", "Priority","Regular")

results_samples_NYC <- t(final_data_draws[,results$CITY[!is.na(results$fit)] == 12])
colnames(results_samples_NYC) <- paste0("Sample",1:4000)

samples <- apply(results_samples_NYC, 2, function(x) 
  (x * results_NYC$WGHT[!is.na(results_NYC$fit)]) *
    (1/results_NYC$WNYC[!is.na(results_NYC$fit)]))

samples <-
  rbind(
    data.frame(Priority = "Priority Roads",
               value = apply(samples[results_NYC$Priority[!is.na(results_NYC$fit)] == "Priority",],2, sum)),
    data.frame(Priority = "Non Priority Roads",
               value = apply(samples[results_NYC$Priority[!is.na(results_NYC$fit)] != "Priority",],2, sum)))

brh <- readOGR("data/nybb_17a","nybb")
pts_brh <- spTransform(pts_nyc,brh@proj4string)
deaths_brh <- data.frame(deaths_nyc,over(pts_brh,brh))
brh_agg <- aggregate(Fatalities ~ YR + BoroName + Priority, deaths_brh, sum) 

sum(brh_agg[brh_agg$YR < 2014 & 
              brh_agg$BoroName %in% c("Brooklyn", "Manhattan", "Queens"),]$Fatalities)/5

observ_bmq <-
  data.frame(
    prior_before =
      sum(brh_agg[brh_agg$YR < 2014 & 
                    brh_agg$BoroName %in% c("Brooklyn", "Manhattan", "Queens") &
                    brh_agg$Priority == "Priority",]$Fatalities)/5,
    
    not_prior_before =
      sum(brh_agg[brh_agg$YR < 2014 & 
                    brh_agg$BoroName %in% c("Brooklyn", "Manhattan", "Queens") &
                    brh_agg$Priority != "Priority",]$Fatalities)/5,
    
    prior_after =
      sum(brh_agg[brh_agg$YR == 2016 & 
                    brh_agg$BoroName %in% c("Brooklyn", "Manhattan", "Queens") &
                    brh_agg$Priority == "Priority",]$Fatalities),
    
    not_prior_after =
      sum(brh_agg[brh_agg$YR == 2016 & 
                    brh_agg$BoroName %in% c("Brooklyn", "Manhattan", "Queens") &
                    brh_agg$Priority != "Priority",]$Fatalities))


sample_qntl <-
  rbind(as.numeric(quantile(samples$value[samples$Priority == "Priority Roads"],
                            c(.05,.25,.75,.95))),
        as.numeric(quantile(samples$value[samples$Priority == "Non Priority Roads"],
                            c(.05,.25,.75,.95)))
  )
colnames(sample_qntl) <- c("L95","L50","U50","U95")
sample_qntl <- data.frame(sample_qntl)
sample_qntl$Priority <- c("Priority Roads", "Non Priority Roads")

ggplot()+
  theme_minimal() +
  geom_linerange(aes(x = Priority, ymin = L50, ymax = U50), data = sample_qntl,
                 size = 3) +
  geom_linerange(aes(x = Priority, ymin = L95, ymax = U95), data = sample_qntl,
                 size = 1) +
  labs(x = "", y= "Expected Number of Fatalities", color = "") +
  geom_point(aes(Priority, value, color = factor(Time,
                                                 levels = c("Before","After"))), 
             shape = 95, size = 10,
             data =  data.frame(melt(observ_bmq), 
                                Priority = c("Priority Roads","Non Priority Roads",
                                             "Priority Roads","Non Priority Roads"),
                                Time = c("Before","Before", "After","After"))) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1, size = 7),
        legend.position = "bottom") +
  labs(title= "",
       x = "", y = "")

bfr <- sapply(1:4000, function(i)
  sum((final_data_draws[i,] *
         (1/results$WNYC[!is.na(results$fit)]) *
         results$WGHT[!is.na(results$fit)])[results$CITY[!is.na(results$fit)] == 12])
)

summary(bfr)
quantile(samples$value[samples$Priority == "Priority Roads"], 
         c(.05,.25,.5,.75,.95))
quantile(samples$value[samples$Priority == "Non Priority Roads"], 
         c(.05,.25,.5,.75,.95))

(55 - 79.5)/79.5 
(55 - 64.5)/64.5 

(55 - 91.4)/91.4 
(55 - 55.6)/55.6 

table(samples$value[samples$Priority == "Priority Roads"] < 55)/4000


(55 - 71)/71
(55 - observ_bmq$prior_before)/observ_bmq$prior_before

samples2 <- apply(results_samples_NYC, 2, function(x) 
  (x * results_NYC$WGHT[!is.na(results_NYC$fit)]))

samples2 <-
  rbind(
    data.frame(Priority = "Priority Roads",
               value = apply(samples2[results_NYC$Priority[
                 !is.na(results_NYC$fit)] == "Priority",],2, sum)),
    data.frame(Priority = "Non Priority Roads",
               value = apply(samples2[results_NYC$Priority[
                 !is.na(results_NYC$fit)] != "Priority",],2, sum)))

ggplot(samples2) +
  theme_minimal() +
  aes(value, fill = Priority) +
  geom_density(color = NA, alpha = .75) + 
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1, size = 7),
        legend.position = "bottom") +
  labs(fill = "", x = "Expected Number of Fatalities") +
  labs(title= "", x = "", y = "")
