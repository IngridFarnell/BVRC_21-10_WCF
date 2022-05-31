# Carbon Analysis
# Ingrid Farnell
# Jan 7, 2022

# This script converts all of the raw data into carbon

#------------------------ Load libraries---------------------------------------#
#--------------- Load libraries----------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("ggsci")) # figures

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)

source("./R/CarbonFunctions.R")

#------------------------- Load data-------------------------------------------#
# Plot
site <- fread("./Inputs/WCF_2021_SiteHistory.csv")

# Trees
B1trees <- fread("./Inputs/WCF_2021_B1Trees.csv")
A1trees <- fread("./Inputs/WCF_2021_A1Trees.csv")
Regen <- fread("./Inputs/WCF_2021_Regen.csv")

# Dead wood
CWD <- fread("./Inputs/WCF_2021_CWD.csv")
FWD <- fread("./Inputs/WCF_2021_FWD.csv")
transect <- fread("./Inputs/WCF_2021_TransectHorizontalDistance.csv")

# Soil, forest floor, litter, FWD
Soils <- fread("./Inputs/WCF_2021_Soils.csv")


# Cleaning
#Plot treatment cleaning
site[,PlotID:= as.factor(PlotID)][,BECzone:= as.factor(BECzone)][,BEC_zone_harvest_unit:= as.factor(BEC_zone_harvest_unit)]
B1trees[,PlotID:= as.factor(PlotID)][,SubPlot:= as.factor(SubPlot)][,Species:= as.factor(Species)]
A1trees[,PlotID:= as.factor(PlotID)][,SubPlot:= as.factor(SubPlot)][,Species:= as.factor(Species)]
Regen[,PlotID:= as.factor(PlotID)][,SubPlot:= as.factor(SubPlot)][,Species:= as.factor(Species)][,`Live/Dead`:= as.factor(`Live/Dead`)][,Height_class:= as.factor(Height_class)]
CWD[,PlotID:= as.factor(PlotID)][,Species:= as.factor(Species)]
FWD[,PlotID:= as.factor(PlotID)]
transect[,PlotID:= as.factor(PlotID)]
Soils[, PlotID:= as.factor(PlotID)]

#---------------------------- Calculate carbon pools---------------------------#
#---------------------------- Tree Carbon
# A1 plot = 5.64m radius = 100m2, B1 = 11.28m radius= 400 m2, A2 = 3.99m radius = 50m2

## Convert to Mg/ha: Kg/m2 x 10
#(Kg C in the tree)/plot are(m2) x (1Mg/1000kg) x (10000m2/ha) == (Kg C in the tree)/plot are(m2) x 10

table(A1trees[,Species])
table(A1trees[,Tree_class])
table(B1trees[,Species])
table(B1trees[,Tree_class])

# A1 trees carbon
t <- vector()
for(i in 1:nrow(A1trees)){
  t[i] <- TreeCarbonFN(Species = A1trees[i,Species], DBH= A1trees[i,DBH], 
                       HT= A1trees[i,Height], Tree_class=A1trees[i,Tree_class])
}

table(A1trees[,SubPlot])
A1trees[,AreaSearchM2 := ifelse(SubPlot=="A1",100, ifelse(SubPlot=="A2",50,400))]
A1trees[,MgPerHa_fac:=10/AreaSearchM2] #calculating the factor to mulitply to get Mg/Ha
A1trees[,CarbonPerHa:=t*MgPerHa_fac]
A1trees[is.na(CarbonPerHa)]#NO TREES, biomass==0
A1trees[is.na(CarbonPerHa), CarbonPerHa:=0]
A1trees[CarbonPerHa ==0]

# B1 trees carbon
table(B1trees[,SubPlot])
r <- vector()
for(i in 1:nrow(B1trees)){
  r[i] <- TreeCarbonFN(Species = B1trees[i,Species], DBH= B1trees[i,DBH], 
                       HT= B1trees[i,Height], Tree_class=B1trees[i,Tree_class])
}

B1trees[,AreaSearchM2 := ifelse(SubPlot=="A1",100, ifelse(SubPlot=="A2",50,400))]
B1trees[,MgPerHa_fac:=10/AreaSearchM2] #calculating the factor to mulitply to get Mg/Ha
B1trees[,CarbonPerHa:=r*MgPerHa_fac]
B1trees[is.na(CarbonPerHa)]#NO TREES, biomass==0
B1trees[is.na(CarbonPerHa), CarbonPerHa:=0]

# Merge A1 and B1 together
A1B1trees <- rbind(A1trees[,.(PlotID,SubPlot,Species,DBH,Height,Tree_class,AreaSearchM2,CarbonPerHa)], 
                   B1trees[,.(PlotID,SubPlot,Species,DBH,Height,Tree_class,AreaSearchM2,CarbonPerHa)])
A1B1trees[,PHF:=10000/AreaSearchM2] #accounting for smaller search areas


#Live trees by plot
LiveTree_plots <- merge(site[,.(PlotID)],A1B1trees[Tree_class<3, sum(CarbonPerHa),by="PlotID"],
                        by.x="PlotID", by.y="PlotID", all=TRUE)
LiveTree_plots$V1[is.na(LiveTree_plots$V1)] <-0
setnames(LiveTree_plots,"V1","LiveTreeCperHa")

#Dead trees by plot
DeadTree_plots <- merge(site[,.(PlotID)],A1B1trees[Tree_class>=3, sum(CarbonPerHa),by="PlotID"],
                        by.x="PlotID", by.y="PlotID", all=TRUE)
DeadTree_plots$V1[is.na(DeadTree_plots$V1)] <-0
setnames(DeadTree_plots,"V1","DeadTreeCperHa")

# Live and dead trees by plot
live_dead_CperHa <- merge(LiveTree_plots,DeadTree_plots,by="PlotID")
site <- merge(site,live_dead_CperHa,by="PlotID")


#--------------------------------- Regen carbon 
Regen$Diam_est <- 1 # for 31-130 cm height class
Regen$Diam_est[Regen$Height_class == "0-30"] <- 0.1
h <- vector()
for(i in 1:nrow(Regen)){
  h[i] <- RegenCarbonFN_Ung(Species = Regen[i,Species], Diam_est = 0.1, 
                            Height_class = Regen[i,Height_class], Health = Regen[i,`Live/Dead`] )
}
Regen[,AreaSearchM2:=ifelse(SubPlot=="A1|C1|C2|C3|C4",100,50)]
Regen[,MgPerHa_fac:=10/AreaSearchM2]
Regen[,CarbonPerHa := h*Tally*MgPerHa_fac] 
Regen[CarbonPerHa==0]
Regen[is.na(CarbonPerHa)]
PlotRegen <- Regen[,.(Regen_MGHa = sum(CarbonPerHa)),by=c("PlotID","Live/Dead")]
PlotRegen <- dcast(PlotRegen, PlotID ~ `Live/Dead`, value.var="Regen_MGHa")
setnames(PlotRegen, c("L","D"), c("Regen_L_MGHa","Regen_D_MGHa"))

site <- merge(site, PlotRegen, by.x="PlotID", by.y="PlotID")


#--------------------------------- Root carbon
#root biomass: - calculating this off of total live carbon below 0.22xlive stand biomas 
#Wang et al 2012 decay rate of dead roots. Only applied this to dead trees, but left dead roots with "non-decayed" carbon
#decided to remove dead roots from dead carbon pool

site[,LiveRootC:=((LiveTreeCperHa*0.222)+(Regen_L_MGHa*0.222)),by="PlotID"] #root =  0.222 x live biomass
site[,DeadRootC:=((DeadTreeCperHa*exp(-0.1044*(2021-Year_harvested)))+ #decaying the tree roots by time since fire
                             (Regen_D_MGHa*0.222)),by="PlotID"] #root =  0.222 x  dead biomass


#------------------------------- Mineral soil carbon
#### Mineral soil (T/ha) = volume(m3/ha)
# calculate the fine fraction BulkDensity
# fine fraction bulk density defined in Bulmer and Simpson 2010 (Soil Compaction Reduced the Growth of Lodgepole Pine and Douglas-fi r Seedlings in Raised Beds after Two Growing Seasons) Fine fraction
# "bulk density was determined after passing the sample through a 2-mm sieve and assuming a coarse fragment specific gravity of 2650 kg m−3 according to:
#  DBff = (Ws – Wcf )/[Vtot – (Wcf/2650)]
# where DBff is fine fraction bulk density,
# Ws is the total weight of the sample retrieved from the bulk density core,
# Wcf is the weight of coarse fragments retained on the 2-mm sieve,
# Vtot is the total volume of the sample core.
# The 500 is an estimate of the organic fragment particle density I usually just use the density of wood from a table like this (Density of wood in kg/m3, g/cm3, lb/ft3 - the ultimate guide - EngineeringClicks), from e-mail with Chuck Bulmer
# Can get a better esimate by putting roots and charcoal into graduated cylinder

# option 2 for bulk density:
#  BulkDens[,BDest_finefract_g_cm3 := -1.977+4.105 x (C_pro/0.47)- 1.229 x log(C_pro/0.47) - 0.103*log(C_pro/0.47)^2]

# Erica changed this so roots are not added in (C in roots is estimated from tree biomass)
# it would be interesting to look at the C in roots from our samples and compare with the estimates from tree biomass

# Change percent C from lab analysis to proportion C
Soils[,C_pro := MinSoil_C_PC/100]

# calculating bulk density
Soils[,TotalSample_wgt_kg := (MinSoil_FineWgt + MinSoil_CoarseWgt + 
                                MinSoil_WoodChunks + MinSoil_BlackC)/1000]
Soils[,CoarseFrag_wgt_kg := (MinSoil_CoarseWgt)/1000]
# Bulk density calculations:
Soils[,OrgFrag_wgt_kg := (MinSoil_WoodChunks + MinSoil_BlackC)/1000]
Soils[,FineFract_wgt_kg := (MinSoil_FineWgt)/1000]
Soils[,CoarseAndOrgFragM3M3 := (CoarseFrag_wgt_kg/2650 + OrgFrag_wgt_kg/500)/ (BulkDensity_ml/1000000) ]

#### Fine fraction BulkDensity ####
# fine fraction bulk density defined in Bulmer and Simpson 2010 (Soil Compaction Reduced the Growth of Lodgepole Pine and Douglas-fi r Seedlings in Raised Beds after Two Growing Seasons) Fine fraction
Soils[,BDcalc_finefract := FineFract_wgt_kg/((BulkDensity_ml/1000000)-
                                               (CoarseFrag_wgt_kg/2650)-(OrgFrag_wgt_kg/500))]#kg/m3
FR_minSOC_finefract_kg_m2 <- Min_SOC(Soc = Soils[,C_pro], BD = Soils[,BDcalc_finefract],
                                     depth = Soils[,BDDepth], CoarseFrags = Soils[,CoarseAndOrgFragM3M3])
# getting a negative BD.. 

#### Soil Organic Carbon ####
#using 0.5 biomass to C conversion for litter and roots as per Michelle equations
#using a 0.75 char to C mass conversion from Donato et al. 2009 (Quantifying char in postfire woody detritus inventories)
Soils[,FR_BlackC_kg := (MinSoil_BlackC*0.75)/1000] #removed roots, and added litter to litter
Soils[,FR_BlackC_kg_m2:= FR_BlackC_kg/(BulkDensity_ml/1000000) * BDDepth]

Soils[,FR_SOM:= C_pro/0.47]
Soils[,BDcalc_finefract_g_cm3 := BDcalc_finefract*1000/1e+06] #calculate finefraction

## WCF_9 seems a little wonky - ask Alana if she thinks would should estimate it instead


####### ADD TEXTURES ########
Soils[,CLAY := sum(MinSoil_Clay_PC_hyd,MinSoil_Clay_PC_h202_hyd,na.rm = TRUE), by="ID"] # what is it summing?
Soils[,SILT := sum(MinSoil_Silt_PC_hyd, MinSoil_Silt_PC_h202_hyd,na.rm = TRUE), by="ID"]
Soils[,SAND := sum(MinSoil_Sand_PC_hyd,MinSoil_Sand_PC_h202_hyd,na.rm = TRUE), by="ID"]
Soils[,TotalTexture := CLAY + SILT + SAND]



#--------------------------------- Litter carbon
# collected in 4 * 20cm x 20cm area = 0.16m2 = 1.6e-05 ha 

#add litter from mineral soil to litter - there is none so don't do
site[,Litter_C_g := Soils[,(Litter_DryWgt*0.46)]] # 0.46 = avg from frontiers litter carbon %
site[,Litter_C_MgHa := Litter_C_g/16]


#--------------------------------- Forest floor carbon
# area collection = 4 x 0.20 x 0.20 = 0.16m2 = 1.6e-05 ha 

#calculating the volume of forest floor (F and H layers)
site[, ForestFloor_C_g := Soils[, ForFloor_DryWgt*0.40]] # 0.40 = avg from frontiers forest floor carbon %
site[, ForestFl_MgHa := ForestFloor_C_g/16] #converting to Mg/Ha


#-------------------------------- V small FWD carbon
# area collection = 4 x 0.20 x 0.20 = 0.16m2 = 1.6e-05 ha 
site[, vFWD_C_g := Soils[, FWD_DryWgt*0.50]]
site[, vFWD_MgHa := vFWD_C_g/16] #converting to Mg/Ha


#-------------------------------------- CWD
# Transect line horizontal distances
transect.plot <- transect[,.(Hor.dist = sum(Horizontal_distance)), by = "PlotID"] # sum horizontal distance for each plot

##################################################################

# To convert volume (calculated using the VanWagner formula) to carbon you have to 
# calculate the volume for each decay class and species, then convert that to live biomass then dead biomass

# There are different conversion factors for each decay class and species. I will calculate the volume for
# each species and decay class in each plot - then convert that to biomass and carbon - then sum to 
# get the total plot carbon

# Volume equation
# CWD volume (m3/ha) = pi^2/8L  *  sum[D2]          

# Where: 	L = length of total transect (horizontal distance (HD) in m) 
#                                       HD = SD / Square root of [1 + (% slope / 100)2]
# D = diameter of each piece of CWD (cm)

################################################################

# Plot summary by Decay class
CWD.plot <- CWD[,.(D2 = sum(Diam_cm^2)), by = c("PlotID", "Decay_class", "Species")]
CWD.plot <- merge(CWD.plot, transect.plot, by = "PlotID") # merge with transect line

# Calculate plot volume(m3/ha) for each decay class
CWD.plot[, volume_ha := (pi^2/(8*Hor.dist) * D2)] 

# Assign carbon to new column
t <- vector()
for(i in 1:nrow(CWD.plot)){
  t[i] <- cwdCarbonFN(volume_ha = CWD.plot[i, volume_ha], 
                      Decay_class = CWD.plot[i, Decay_class], 
                      Species = CWD.plot[i,Species])
}

CWD.plot[, CWD_C := t]

# Sum total carbon in each plot
PlotCWD <- CWD.plot[, .(CWD_C =sum(CWD_C)), by = "PlotID"]
site <- merge(site, PlotCWD, by = "PlotID")


#--------------------------------- FWD
# fwd only measured on 10m of each line (=20m both lines), so subtract remaing 80 m from total length
transect.plot[, fwd.dist:= (Hor.dist - 80)]

# Sum tally for each diameter class for each plot
FWD.plot <- FWD[, .(Tally = sum(Tally)), by = c("PlotID", "Diam_class")]

# Merge line and fwd data
FWD.plot <- merge(FWD.plot, transect.plot, by = "PlotID")

##################################################################

# To convert volume (calculated using the VanWagner formula) to carbon you have to 
# calculate the volume for each decay class, then convert that to live biomass then dead biomass

# However, we only tallied the pieces by diameter class and did not record species or decay class
# Volume estimates will be multiplied by the bulk density of FWD and a decay-reduction factor to get biomass


# Volume equation for FWD
# CWD volume (m3/ha) = pi^2/8L  *  n * QMD^2          

# Where: 	L = length of total transect (horizontal distance (HD) in m) 
#                                       HD = SD / Square root of [1 + (% slope / 100)2]
# n = number of pieces in the diameter class
# QMD = quadratic mean diameter at the intersection aka. mean diameter of the diameter class

################################################################

# Create QMD column (mean of diameter class)
qmdFN <- function(Diam_class){
  if(is.na(Diam_class)){
    print(paste("Diam_class is not found"))
    Diam <- NA
  } else {
    if (Diam_class == "1.1-2.5"){
      Diam <- (2.5+1.1)/2
    } else if (Diam_class == "2.6-5"){
      Diam <- (5+2.6)/2
    } else if (Diam_class == "5.1-7.5"){
      Diam <- (7.5+5.1)/2
    } else {
      print(paste("Diam_class",Species,"not found"))
      Diam <- NA
    }
  }
  return(Diam)
}

# Assign QMD to new column
qmd <- vector()
for(i in 1:nrow(FWD.plot)){
  qmd[i] <- qmdFN(Diam_class = FWD.plot[i, Diam_class])
}
FWD.plot[, QMD:= qmd]


# Create volumn calculation function
fwdVolFN <- function(L, n, QMD){
  fwd_vol <- ((pi^2/(8*L)) * n *(QMD^2))
  return(fwd_vol)
}

# Calculate volume
m <- vector()
for (i in 1:nrow(FWD.plot)) {
  m[i] <- fwdVolFN(L = FWD.plot[i, fwd.dist],
                   n = FWD.plot[i, Tally],
                   QMD = FWD.plot[i, QMD])
}
FWD.plot[, volume_ha:= m]


# Assign carbon to new column
c <- vector()
for(i in 1:nrow(FWD.plot)){
  c[i] <- fwdCarbonFN(volume = FWD.plot[i,volume_ha], 
                      Diam_class = FWD.plot[i, Diam_class])
}
FWD.plot[, carbon:= c]

# Sum total carbon in each plot
PlotFWD <- FWD.plot[, .(FWD_C = sum(carbon)), by = "PlotID"]
site <- merge(site, PlotFWD, by = "PlotID")



#-------------------------- Data setup-----------------------------------------#
CarbonPlots <- melt(site, id.vars = c("PlotID", "BECzone", "Year_harvested","BEC_zone_harvest_unit"),
                    measure.vars = c("LiveTreeCperHa","Regen_L_MGHa","Regen_D_MGHa","DeadTreeCperHa",
                                     "Litter_C_MgHa","ForestFl_MgHa","vFWD_MgHa","CWD_C","FWD_C","LiveRootC"),
                    variable.name = "CarbonSource",
                    value.name = "CarbonMgHa")
site <- merge(site,CarbonPlots[,.(TotalCarbon = sum(na.omit(CarbonMgHa))), by="PlotID"],by="PlotID")
#FR_treatments <- merge(FR_treatments,CarbonPlots[CarbonSource!="MinSoilC_Mgha",
                                                 #.(TotalCarbon_NotMin = sum(na.omit(CarbonMgHa))), by="ID"],by="ID")
site <- merge(site,CarbonPlots[CarbonSource=="LiveTreeCperHa"|
                                 CarbonSource=="DeadTreeCperHa"|
                                 CarbonSource=="CWD_C"|
                                 CarbonSource=="FWD_C",
                               .(AboveGrCarbon = sum(na.omit(CarbonMgHa))),
                               by="PlotID"],by="PlotID")
site <- merge(site,CarbonPlots[#CarbonSource=="MinSoilC_Mgha"|
                                 CarbonSource=="Litter_C_MgHa"|
                                 CarbonSource=="ForestFl_MgHa"|
                                 CarbonSource=="vFWD_MgHA"|
                                 CarbonSource=="LiveRootC"|
                                 CarbonSource=="DeadRootC",
                                 .(BelowGrCarbon = sum(na.omit(CarbonMgHa))),
                                 by="PlotID"],by="PlotID")
site <- merge(site,CarbonPlots[CarbonSource=="DeadTreeCperHa"|
                                 CarbonSource=="Regen_D_MGHa"|
                                 CarbonSource=="CWD_C"|
                                 CarbonSource=="FWD_C",
                               #CarbonSource=="DeadRootC",
                               .(DeadCarbon = sum(na.omit(CarbonMgHa))),
                               by="PlotID"],by="PlotID")
site <- merge(site,CarbonPlots[CarbonSource=="CWD_C"|
                                 CarbonSource=="FWD_C",
                               .(WDCarbon = sum(na.omit(CarbonMgHa))),
                               by="PlotID"],by="PlotID")
site <- merge(site,CarbonPlots[CarbonSource=="LiveTreeCperHa"|
                                 CarbonSource=="Regen_L_MGHa"|
                                 CarbonSource=="LiveRootC",
                               .(LiveCarbon = sum(na.omit(CarbonMgHa))),
                               by="PlotID"],by="PlotID")
site <- merge(site,CarbonPlots[CarbonSource=="ForestFl_MgHa"|
                                 CarbonSource=="Litter_C_MgHa"|
                                 CarbonSource=="vFWD_MgHA",
                               .(ForestFloorTotC = sum(na.omit(CarbonMgHa))),
                               by="PlotID"],by="PlotID")
site <- merge(site,CarbonPlots[CarbonSource=="ForestFl_MgHa",
                                 #CarbonSource=="MinSoilC_Mgha",
                                 .(Min_FF = sum(na.omit(CarbonMgHa))),
                               by="PlotID"],by="PlotID")
site <- merge(site,CarbonPlots[CarbonSource=="MinSoilC_Mgha" |
                                 CarbonSource=="ForestFl_MgHa"|
                                 CarbonSource=="Litter_C_MgHa"|
                                 CarbonSource=="vFWD_MgHa",
                               .(Min_FF_L_vFWD = sum(na.omit(CarbonMgHa))),
                               by="PlotID"],by="PlotID")
site[,DeadConst:=(DeadCarbon+0.01)]
site[,FFConst:=(ForestFloorTotC+0.01)]

site_tables <- melt(site, id.vars = c("PlotID","Year_harvested","BECzone", "BEC_zone_harvest_unit"),
                    measure.vars = c("LiveTreeCperHa","Regen_L_MGHa","LiveRootC","DeadTreeCperHa","Regen_D_MGHa",
                                   "CWD_C","FWD_C"),
                    variable.name = "CarbonPool",
                    value.name = "CarbonMgHa")
site_tables[,harv_cat:=ifelse(Year_harvested >2014,2015,
                           ifelse(Year_harvested >2012,2013,
                                  ifelse(Year_harvested > 2008, 2009,
                                         0)))]
site_pool_summary <- site_tables[,.(mn_CarbonMgHa=mean(CarbonMgHa),sd_CarbonMgHa=sd(CarbonMgHa)),
                                 by=c("CarbonPool","BECzone", "harv_cat")]

site[,harv_cat:=ifelse(Year_harvested >2011, "2013-2016", "2009-2012")]


#---------------------------------- Figures------------------------------------#

pools <- site[,.(PlotID, BECzone, harv_cat, TotalCarbon, LiveCarbon, DeadCarbon, ForestFloorTotC)]


pools_m <- melt(pools,id.vars = c("PlotID","BECzone","harv_cat"),
                measure.vars = c("TotalCarbon","LiveCarbon","DeadCarbon","ForestFloorTotC"),
                variable.name = "CarbonPool",
                value.name = "CarbonMgHa")

table(pools_m[,.(BECzone,harv_cat)])
means <- pools_m[,.(mn_CarbonMgHa=mean(CarbonMgHa),sd_CarbonMgHa=sd(CarbonMgHa)),
                 by=c("CarbonPool","BECzone","harv_cat")]

supp.labs <- c("Total","Live","Dead","Forest floor")
names(supp.labs) <- c("TotalCarbon","LiveCarbon","DeadCarbon","ForestFloorTotC")

ggplot(pools_m,aes(x=harv_cat, y=CarbonMgHa))+
  geom_point(size=2,aes(colour=BECzone))+
  geom_smooth(method="lm",aes(colour=BECzone,fill=BECzone))+
  scale_color_npg()+
  scale_fill_npg()+
  theme_minimal() +
  ylab(expression("Carbon Mg" ~ ha^-1))+
  xlab("Time since harvest")+
  theme(legend.position = "bottom")+
  facet_wrap("CarbonPool",labeller=labeller(CarbonPool=supp.labs))+
  theme(strip.text.x = element_text(face="bold"))


