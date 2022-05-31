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
site[,PlotID:= as.factor(PlotID)][,BECzone:= as.factor(BECzone)][,BEC_zone_harvest_unit:= as.factor(BEC_zone_harvest_unit)][,YearsSinceHrvst := (2021-Year_harvested)]
B1trees[,PlotID:= as.factor(PlotID)][,SubPlot:= as.factor(SubPlot)][,Species:= as.factor(Species)]
A1trees[,PlotID:= as.factor(PlotID)][,SubPlot:= as.factor(SubPlot)][,Species:= as.factor(Species)]
Regen[,PlotID:= as.factor(PlotID)][,SubPlot:= as.factor(SubPlot)][,Species:= as.factor(Species)][,`Live/Dead`:= as.factor(`Live/Dead`)][,Height_class:= as.factor(Height_class)]
CWD[,PlotID:= as.factor(PlotID)][,Species:= as.factor(Species)]
FWD[,PlotID:= as.factor(PlotID)]
transect[,PlotID:= as.factor(PlotID)]
Soils[, PlotID:= as.factor(PlotID)]

#------------------------ Calculate Canopy Fuel Structure----------------------#
#---------------------------- Tree
# A1 plot = 5.64m radius = 100m2, B1 = 11.28m radius= 400 m2, A2 = 3.99m radius = 50m2

## Calculate crown foliage biomass(kg/m2), then divide by average crown length to get
# Canopy bulk density - Cruz et al. 2003 methods
# Using biomass of foliage and 0.5 1hr fuel (0-0.63 diam) branches

table(A1trees[,Species])
table(A1trees[,Tree_class])
table(B1trees[,Species])
table(B1trees[,Tree_class])

# A1 trees - individual trees
# Canopy Bulk Density (kg/m3) = Crown foliage biomass (kg/m2) / Canopy length (m)
# Crown foliage biomass (kg/m2)
fw <- vector()
for(i in 1:nrow(A1trees)){
  fw[i] <- CanopyBiomassFN(Species = A1trees[i,Species], DBH= A1trees[i,DBH], 
                       HT= A1trees[i,Height], Tree_class=A1trees[i,Tree_class])
}
# (there are 2 plots with no A1 trees, so should get 2 "species not found")

table(A1trees[,SubPlot])
A1trees[,AreaM2 := ifelse(SubPlot=="A1",100, ifelse(SubPlot=="A2",50,400))]
A1trees[,CanopyKgM2:=fw/AreaM2]
A1trees[is.na(CanopyKgM2)]#NO TREES, canopy biomass==0
A1trees[is.na(CanopyKgM2), CanopyKgM2:=0]
A1trees[CanopyKgM2 ==0]

# Crown length (m)
A1trees[,CrownLength := (Height - Crown_base_height)]
A1trees[is.na(CrownLength), CrownLength:=0]


# B1 trees - individual trees
# Crown foliage biomass (kg/m2)
table(B1trees[,SubPlot])
fwB1 <- vector()
for(i in 1:nrow(B1trees)){
  fwB1[i] <- CanopyBiomassFN(Species = B1trees[i,Species], DBH= B1trees[i,DBH], 
                       HT= B1trees[i,Height], Tree_class=B1trees[i,Tree_class])
}
# (there are 7 plots without large trees)

B1trees[,AreaM2 := 400]
B1trees[,CanopyKgM2:=fwB1/AreaM2]
B1trees[is.na(CanopyKgM2)]#NO TREES, canopy biomass==0
B1trees[is.na(CanopyKgM2), CanopyKgM2:=0]

# Crown length (m)
B1trees[,CrownLength := (Height - Crown_base_height)]
B1trees[is.na(CrownLength), CrownLength:=0]

# Stand level
# Merge A1 and B1 together
A1B1trees <- rbind(A1trees,B1trees, fill=TRUE)


# Trees by plot
# CFL (kg/m2) = sum foliage weight/plot
# CL (m) = average length crown length / plot
# CBD (kg/m3) = CFL / CL
# CBH (m) = average canopy base height
PlotCFL <- A1B1trees[,.(CFL = sum(CanopyKgM2)), by="PlotID"]
PlotCBD <- A1B1trees[,.(CBD = sum(CanopyKgM2)/mean(CrownLength)), by="PlotID"]
PlotCBH <- A1B1trees[,.(CBH = mean(Crown_base_height, na.rm=TRUE)), by="PlotID"]


site <- Reduce(merge, list(site,PlotCFL,PlotCBD,PlotCBH))




#-----------------------------------Surface fuels------------------------------#

#------------------------------------CWD (1000-hr fuel) (T/ha == Mg/ha)
# convert to kg/m2 at the end
##############################################################
#  Calculate volume then weight per load class (1-1000hr)
#     1-hr < 0.6 cm diam
#     10-hr 0.6 - 2.5 cm diam
#     100-hr 2.5 - 7.6 cm
#     1000 hr fuel - 7.7 - 20 cm: calculate sound (1-3) and rotten (4-5) load separately

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
# Transect line horizontal distances
transect.plot <- transect[,.(Hor.dist = sum(Horizontal_distance)), by = "PlotID"] # sum horizontal distance for each plot

# Make sure min diameter is 7.6 
min(CWD$Diam_cm)

# Plot summary by hourly fuel class (1000-hr) and sound/rotten class
CWD[,RottenSound:= ifelse(Decay_class >3, "rotten",
                        "sound")]


CWD.plot <- CWD[,.(D2 = sum(Diam_cm^2)), by = c("PlotID", "Decay_class", "Species", "RottenSound")]
CWD.plot <- merge(CWD.plot, transect.plot, by = "PlotID") # merge with transect line

# Calculate plot volume(m3/ha) for each decay class
CWD.plot[, volume_ha := (pi^2/(8*Hor.dist) * D2)] 

# Calculate plot biomass for each species and decay class
t <- vector()
for(i in 1:nrow(CWD.plot)){
  t[i] <- cwdBiomassFN(volume_ha = CWD.plot[i, volume_ha], 
                      Decay_class = CWD.plot[i, Decay_class], 
                      Species = CWD.plot[i,Species])
}
CWD.plot[, CWD_B:=t]


# Sum volume by hourly fuel (1000-hr_sound or 1000-hr_rotten)
CWDsound <- CWD.plot[,.('1000hr_sound'=sum(CWD_B[RottenSound=="sound"])), by = "PlotID"]
CWDrotten <- CWD.plot[,.('1000hr_rotten'=sum(CWD_B[RottenSound=="rotten"])), by = "PlotID"]
site <- Reduce(merge, list(site, CWDsound, CWDrotten)) # Mg/ha


#--------------------------------- FWD (100hr, 10hr fuels) (T/ha == Mg/ha)
# convert to kg/m2 at the end
# fwd only measured on 10m of each line (=20m both lines), so subtract remaing 80 m from total length
transect.plot[, fwd.dist:= (Hor.dist - 80)]

# Sum tally for each diameter class for each plot
FWD.plot <- FWD[, .(Tally = sum(Tally)), by = c("PlotID", "Diam_class")]

# Merge line and fwd data
FWD.plot <- merge(FWD.plot, transect.plot, by = "PlotID")

################################################################
# To convert volume (calculated using the VanWagner formula) to biomass you have to 
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


# Create volume calculation function
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


# Assign biomass to new column
c <- vector()
for(i in 1:nrow(FWD.plot)){
  c[i] <- fwdBiomassFN(volume = FWD.plot[i,volume_ha], 
                      Diam_class = FWD.plot[i, Diam_class])
}
FWD.plot[, biomass:= c]

# Hourly fuels per plot
Plot100hr <- FWD.plot[,.('100hr'=sum(biomass[Diam_class=="5.1-7.5"| Diam_class=="2.6-5"])), by = "PlotID"] # 2.5 - 7.6 cm
Plot10hr <- FWD.plot[,.('10hr'=sum(biomass[Diam_class=="1.1-2.5"])), by = "PlotID"] # 0.6 - 2.5
site <- Reduce(merge, list(site, Plot100hr, Plot10hr))

#-------------------------------- Fine fuel <1 cm diam = 1hr fuel
# area collection = 4 x 0.20 x 0.20 = 0.16m2
site[,'1hr':= Soils[, (FWD_DryWgt/16)]] #converting to Mg/Ha



#--------------------------------------Ground fuels---------------------------#
#--------------------------------- Litter
# Calculate weight
# collected in 4 * 20cm x 20cm area = 0.16m2 = 1.6e-05 ha 
site[,Litter:= Soils[,(Litter_DryWgt/16)]] # Mg/ha


#--------------------------------- Forest floor/duff
# area collection = 4 x 0.20 x 0.20 = 0.16m2
site[, Duff:= Soils[,(ForFloor_DryWgt/16)]] #converting to Mg/Ha


# Convert all surface and ground fuels from Mg/ha to kg/m2 by dividing by 10
mod_cols <- c("1000hr_sound","1000hr_rotten","100hr","10hr", "1hr","Litter", "Duff")
site[,(mod_cols) := lapply(.SD, "/",10), .SDcols=mod_cols]




#-------------------------- Data setup-----------------------------------------#
FuelPlots <- melt(site, id.vars = c("PlotID", "BECzone", "YearsSinceHrvst"),
                    measure.vars = c("CFL","CBD","CBH","1000hr_sound", "1000hr_rotten", "100hr", "10hr",
                                     "1hr","Litter","Duff"),
                    variable.name = "FuelType",
                    value.name = "kgM2")
site <- merge(site,FuelPlots[FuelType!="CBD"|
                               FuelType!="CBH",
                             .(TotalFuels = sum(na.omit(kgM2)))
                             , by="PlotID"],by="PlotID")
site <- merge(site,FuelPlots[FuelType=="1000hr_sound"|
                               FuelType=="1000hr_rotten"|
                               FuelType=="100hr"|
                               FuelType=="10hr"|
                               FuelType=="1hr",
                             .(DDWoodyFuels = sum(na.omit(kgM2))),
                             by="PlotID"],by="PlotID")
site <- merge(site,FuelPlots[FuelType=="Litter"|
                               FuelType=="Duff",
                             .(SurfaceFuels = sum(na.omit(kgM2))),
                             by="PlotID"],by="PlotID")

site_m <- melt(site, id.vars = c("PlotID", "BECzone", "YearsSinceHrvst"),
                    measure.vars = c("CFL","CBD","CBH","1000hr_sound", "1000hr_rotten", "100hr", "10hr",
                                     "1hr","Litter","Duff", "DDWoodyFuels", "SurfaceFuels", "TotalFuels"),
                    variable.name = "FuelType",
                    value.name = "kgM2")

site_summary <- site_tables[,.(mn_kgM2=mean(kgM2),sd_kgM2=sd(kgM2)),
                                 by=c("FuelType","BECzone", "YearsSinceHrvst")]


#---------------------------------- Figures------------------------------------#

# All fuel types
all.labs <- c("CBD","CFL","CBH", "1000hr_sound", "1000hr_rotten", "100hr", "10hr",
               "1hr","Litter","Duff", "DDWoodyFuels", "SurfaceFuels", "TotalFuels")
names(all.labs) <- c("CBD","CFL","CBH", "1000hr_sound", "1000hr_rotten", "100hr", "10hr",
                      "1hr","Litter","Duff", "DDWoodyFuels", "SurfaceFuels", "TotalFuels")

ggplot(site_m,aes(x=YearsSinceHrvst, y=kgM2))+
  geom_point(size=2,aes(colour=BECzone))+
  geom_smooth(method="lm",aes(colour=BECzone,fill=BECzone))+
  scale_color_npg()+
  scale_fill_npg()+
  theme_minimal() +
  ylab(expression("kg" ~ m^-2))+
  xlab("Years since harvest")+
  theme(legend.position = "bottom")+
  facet_wrap("FuelType",labeller=labeller(FuelType=all.labs))+
  theme(strip.text.x = element_text(face="bold"))


# Canopy bulk density
canopyBD <- site[,. (PlotID,BECzone,YearsSinceHrvst,CBD, CBH)]

canopyBD_m <- melt(canopyBD,id.vars = c("PlotID","BECzone","YearsSinceHrvst"),
                 measure.vars = c("CBD", "CBH"),
                 variable.name = "FuelType",
                 value.name = "kgM2")

canopy.labs <- c("Canopy Bulk Density", "Canopy Base Height")
names(canopy.labs) <- c("CBD", "CBH")

ggplot(canopyBD_m,aes(x=YearsSinceHrvst, y=kgM2, colour=FuelType))+
  geom_point()+
  scale_color_npg()+
  scale_fill_npg()+
  theme_minimal() +
  ylab(expression("kg" ~ m^-3))+
  xlab("Years since harvest")+
  theme(legend.position = "bottom")+
  facet_wrap("FuelType",labeller=labeller(FuelType=canopy.labs))+
  theme(strip.text.x = element_text(face="bold"))

# Hourly fuels
hourly <- site[,. (PlotID,BECzone,YearsSinceHrvst,`1000hr_sound`,`1000hr_rotten`,`100hr`,`10hr`,`1hr`)]

hourly_m <- melt(hourly,id.vars = c("PlotID","BECzone","YearsSinceHrvst"),
                 measure.vars = c("1000hr_sound", "1000hr_rotten", "100hr", "10hr", "1hr"),
                 variable.name = "FuelType",
                 value.name = "kgM2")

hr.labs <- c("1000hr sound", "1000hr rotten", "100hr", "10hr", "1hr")
names(hr.labs) <- c("1000hr_sound", "1000hr_rotten", "100hr", "10hr", "1hr")

ggplot(hourly_m, aes(x=YearsSinceHrvst, y=kgM2, fill=FuelType))+
  geom_col(position="stack")+
  scale_color_npg()+
  scale_fill_npg()+
  theme_minimal() +
  ylab(expression("kg" ~ m^-2))+
  xlab("Years since harvest")+
  theme(legend.position = "bottom")+
  theme(strip.text.x = element_text(face="bold"))

ggplot(hourly_m, aes(x=YearsSinceHrvst, y=kgM2, colour=FuelType))+
  geom_point(size=2,aes(colour=FuelType))+
  geom_smooth(method="lm",aes(colour=FuelType,fill=FuelType))+
  scale_color_npg()+
  scale_fill_npg()+
  theme_minimal() +
  ylab(expression("kg" ~ m^-2))+
  xlab("Years since harvest")+
  theme(legend.position = "bottom")+
  theme(strip.text.x = element_text(face="bold"))


# Surface fuels
surface <- site[,. (PlotID,BECzone,YearsSinceHrvst,Litter,Duff)]

surface_m <- melt(surface,id.vars = c("PlotID","BECzone","YearsSinceHrvst"),
                 measure.vars = c("Litter", "Duff"),
                 variable.name = "FuelType",
                 value.name = "kgM2")

surf.labs <- c("Litter", "Duff")
names(surf.labs) <- c("Litter", "Duff")

ggplot(surface_m, aes(x=YearsSinceHrvst, y=kgM2, fill=FuelType))+
  geom_bar(position="stack", stat="identity")+
  scale_color_npg()+
  scale_fill_npg()+
  theme_minimal() +
  ylab(expression("kg" ~ m^-2))+
  xlab("Years since harvest")+
  theme(legend.position = "bottom")+
  theme(strip.text.x = element_text(face="bold"))

ggplot(surface_m, aes(x=YearsSinceHrvst, y=kgM2, colour=FuelType))+
  geom_point(size=2,aes(colour=FuelType))+
  geom_smooth(method="lm",aes(colour=FuelType,fill=FuelType))+
  scale_color_npg()+
  scale_fill_npg()+
  theme_minimal() +
  ylab(expression("kg" ~ m^-2))+
  xlab("Years since harvest")+
  theme(legend.position = "bottom")+
  theme(strip.text.x = element_text(face="bold"))
