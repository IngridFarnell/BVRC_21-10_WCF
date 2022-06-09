# Ingrid Farnell
# Site selection for WCF 


setwd("E:/Ingrid/Borealis/WCF")
rm(list=ls())
library(data.table)
library(sf)
library(dplyr)

# In QGIS clipped RESULTS layer with AOI for community forest (there are some blocks that are not
# within the CF so will have to include client ID = WCF as a selector variable)

Results_all <- read_sf("E:/Ingrid/Borealis/WCF/RESULTS_WCF_PermitBlocks/RESULTS_WCF_PermitBlocks.shp", quiet=T)
BEC_allBC <- read_sf("E:/Ingrid/Borealis/WCF/BEC_BIOGEOCLIMATIC_POLY/BEC_POLY_polygon.shp", quiet=T)

# Keep only Results openings where "ClientID"=WCF
Client_incl <- "WETZIN'KWA COMMUNITY FOREST CORPORATION"
Results_WCF <- Results_all %>%
  dplyr::filter(CLIENT_NAM %in% Client_incl)

# Keep only BEC zones within the CF
BEC_WCF <- BEC_allBC %>%
  filter(BGC_LABEL=="ICH mc 1" | BGC_LABEL=="SBS mc 2" | BGC_LABEL=="ESSFmc" | BGC_LABEL=="SBS dk")

## HARVESTING AGE ##
# Have to make the results into a data table
Results_dt <- as.data.table(Results_WCF)

Sample_year <- 2021 #the year your are sampling the trees

# Add column for harvest age - use the denudation date
Results_dt[, HarvestAge := Sample_year - as.numeric(format(as.Date(DENUDATI_4, format="%Y-%m-%d"),"%Y"))]

# Classify the harvest age into 3 bins for stratification
Results_dt[,HarvestClss := ifelse(HarvestAge <7,1,
                                 ifelse(HarvestAge <10,2,
                                        ifelse(HarvestAge<13,3,4)))]


#range of cutblock ages:
hist(Results_dt[,HarvestClss])
table(Results_dt$HarvestClss)
#check if NAs
sum(is.na(Results_dt$HarvestClss))

# Add BEC data to results_WCF
Results_BEC <- st_intersection(st_buffer(st_as_sf(Results_dt),0), st_buffer(BEC_WCF,0))

#create Rep01 that finds which opening_ID is duplicated in Results_BEC
if(length(unique(Results_BEC$OPENING_ID))!= nrow(Results_BEC)){
  RepOI <- Results_BEC[which(duplicated(Results_BEC$OPENING_ID)),]$OPENING_ID
  Results_BEC_sf <- st_as_sf(Results_BEC)
  #st_as_sf converts object to spatial feature - converts Results_BEC
  for(r in 1:length(RepOI)){
    RepOI_sf <- Results_BEC_sf %>%
      filter(OPENING_ID == RepOI[r])
    u <- st_union(RepOI_sf)
    v <- as.data.table(RepOI_sf)[,geometry:=NULL]
    x <- merge(u,v)
    Results_BEC_sfu <- Results_BEC_sf %>%
      filter(OPENING_ID !=RepOI[r])
    Results_BEC_sfu <- rbind(Results_BEC_sfu,st_as_sf(x[1,]))
    Results_BEC_sf <- st_cast(Results_BEC_sfu,to="MULTIPOLYGON")
  }
  Results_BEC <- as.data.table(Results_BEC_sf)
} else {
  print("no repeated OI")
}

# Create a column that is a combo of HarvestClass and BECzone so that I can
# stratify the sample selection for both variables
Results_BEC <- Results_BEC %>%
  unite("HarvestClss_BEC", MAP_LABEL.1, HarvestClss, sep = "_", 
        remove = FALSE)
table(Results_BEC[,HarvestClss_BEC])
#have a look at the cutblocks
write_sf(st_as_sf(Results_BEC),"Results_BEC_WCF.shp", overwrite=TRUE)


#------------------------------STRATIFIED RANDOM SAMPLING------------------------------

library(spatialEco)
Results_BEC_WCF <- read_sf("Results_BEC_WCF.shp", 
                          quiet=T)


Sites2021_final <- as(Results_BEC_WCF,"Spatial")

WCF_sample <- stratified.random(Sites2021_final, 
                               strata='HrC_BEC', #weird, changed name - used to be "HarvestClss_BEC"
                               n = 1, # number of random samples
                               reps=3, # reps per strata
                               replace=FALSE)
WCF_Sites_2021 <- as(WCF_sample,"sf")
st_write(WCF_Sites_2021,paste0(getwd(), "/", "WCF_Sites_2021_10_14.shp"), overwrite=TRUE)



#------------------------------------------------------------------#
# Filter polygons by each HrC_BEC attribute and save as .kml 
sites <- read_sf("WCF_Sites_2021_10_14.shp", quiet = T)

#ESSFmc_1
ESSFmc_1 <- sites %>%
  filter(HrC_BEC == "ESSFmc_1")
write_sf(st_as_sf(ESSFmc_1), "WCF_Sites_ESSFmc_1.kml", overwrite=TRUE)

#ESSFmc_2
ESSFmc_2 <- sites %>%
  filter(HrC_BEC == "ESSFmc_2")
write_sf(st_as_sf(ESSFmc_2), "WCF_Sites_ESSFmc_2.kml", overwrite=TRUE)

#SBSdk_1
SBSdk_1 <- sites %>%
  filter(HrC_BEC == "SBSdk_1")
write_sf(st_as_sf(SBSdk_1), "WCF_Sites_SBSdk_1.kml", overwrite=TRUE)

#SBSdk_2
SBSdk_2 <- sites %>%
  filter(HrC_BEC == "SBSdk_2")
write_sf(st_as_sf(SBSdk_2), "WCF_Sites_SBSdk_2.kml", overwrite=TRUE)

#SBSdk_3
SBSdk_3 <- sites %>%
  filter(HrC_BEC == "SBSdk_3")
write_sf(st_as_sf(SBSdk_3), "WCF_Sites_SBSdk_3.kml", overwrite=TRUE)

#SBSmc2_1
SBSmc2_1 <- sites %>%
  filter(HrC_BEC == "SBSmc2_1")
write_sf(st_as_sf(SBSmc2_1), "WCF_Sites_SBSmc2_1.kml", overwrite=TRUE)

#SBSmc2_2
SBSmc2_2 <- sites %>%
  filter(HrC_BEC == "SBSmc2_2")
write_sf(st_as_sf(SBSmc2_2), "WCF_Sites_SBSmc2_2.kml", overwrite=TRUE)

#SBSmc2_3
SBSmc2_3 <- sites %>%
  filter(HrC_BEC == "SBSmc2_3")
write_sf(st_as_sf(SBSmc2_3), "WCF_Sites_SBSmc2_3.kml", overwrite=TRUE)

