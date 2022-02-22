# Fuels Analysis
# Ingrid Farnell
# Feb 18, 2022

# This script converts all of the raw data into biomass and bins the fuels into categories based on...

#------------------------ Load libraries---------------------------------------#
#--------------- Load libraries----------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("ggsci")) # figures

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)

source("./R/BiomassFunctions.R")

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

# Forest floor, litter, FWD
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

#------------------------ Calculate Canopy Bulk Density------------------------#
#---------------------------- Tree
# A1 plot = 5.64m radius = 100m2, B1 = 11.28m radius= 400 m2, A2 = 3.99m radius = 50m2

## Calculate canopy fuel load (kg/m2), then divide by average crown length to get
# Canopy bulk density - Cruz et al. 2003 methods
# Using biomass of foliage and branches

table(A1trees[,Species])
table(A1trees[,Tree_class])
table(B1trees[,Species])
table(B1trees[,Tree_class])

# A1 trees canopy fuel load (kg/m2) and crown length (m)
m <- vector()
for(i in 1:nrow(A1trees)){
  m[i] <- TreeCanopyBiomassFN(Species = A1trees[i,Species], DBH= A1trees[i,DBH], 
                       HT= A1trees[i,Height], Tree_class=A1trees[i,Tree_class])
}

table(A1trees[,SubPlot])
A1trees[,AreaM2 := ifelse(SubPlot=="A1",100, ifelse(SubPlot=="A2",50,400))]
A1trees[,CanopyFuelsKgM2:=m/AreaM2]
A1trees[is.na(CanopyFuelsKgM2)]#NO TREES, canopy biomass==0
A1trees[is.na(CanopyFuelsKgM2), CanopyFuelsKgM2:=0]
A1trees[CanopyFuelsKgM2 ==0]

A1trees[,CrownLength := (Height - Crown_base_height)]
A1trees[is.na(CrownLength), CrownLength:=0]


# B1 trees canopy fuel load (kg/m2) and crown length (m)
table(B1trees[,SubPlot])
r <- vector()
for(i in 1:nrow(B1trees)){
  r[i] <- TreeCanopyBiomassFN(Species = B1trees[i,Species], DBH= B1trees[i,DBH], 
                       HT= B1trees[i,Height], Tree_class=B1trees[i,Tree_class])
}

B1trees[,AreaM2 := ifelse(SubPlot=="A1",100, ifelse(SubPlot=="A2",50,400))]
B1trees[,CanopyFuelsKgM2:=r/AreaM2]
B1trees[is.na(CanopyFuelsKgM2)]#NO TREES, canopy biomass==0
B1trees[is.na(CanopyFuelsKgM2), CanopyFuelsKgM2:=0]

B1trees[,CrownLength := (Height - Crown_base_height)]
B1trees[is.na(CrownLength), CrownLength:=0]


# Merge A1 and B1 together
A1B1trees <- rbind(A1trees[,.(PlotID,SubPlot,Species,DBH,Height,Tree_class,AreaM2,CanopyFuelsKgM2,CrownLength)], 
                   B1trees[,.(PlotID,SubPlot,Species,DBH,Height,Tree_class,AreaM2,CanopyFuelsKgM2,CrownLength)])


# Trees by plot
PlotTrees <- A1B1trees[,.(CanopyBulkDensity = sum(CanopyFuelsKgM2)/mean(CrownLength)), by="PlotID"]
PlotTrees[is.na(CanopyBulkDensity), CanopyBulkDensity:=0]

site <- merge(site,PlotTrees,by="PlotID")


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




#------------------------------- Mineral soil carbon
# Waiting on lab results


#--------------------------------- Litter carbon
# collected in 4 * 20cm x 20cm area = 0.16m2 = 1.6e-05 ha 

#add litter from mineral soil to litter - there is none so don't do
site[,Litter_C_g := Soils[,(Litter_DryWgt*0.46)]] # 0.46 = avg from frontiers litter carbon %
site[,Litter_C_MgHa := Litter_C_g/16]


#--------------------------------- Forest floor carbon
# area collection = 4 x 0.20 x 0.20 = 0.16m2

#calculating the volume of forest floor (F and H layers)
site[, ForestFloor_C_g := Soils[, ForFloor_DryWgt*0.40]] # 0.40 = avg from frontiers forest floor carbon %
site[, ForestFl_MgHa := ForestFloor_C_g/16] #converting to Mg/Ha


#-------------------------------- V small FWD carbon
# area collection = 4 x 0.20 x 0.20 = 0.16m2
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


