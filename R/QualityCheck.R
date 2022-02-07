# Quality checking raw data
# Ingrid Farnell
# Dec 6, 2021

rm(list=ls())   #cleans the workspace so all previous objects are deleted

#------------------------------- Load libraries--------------------------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
#ls <- append(ls, c("raster", "sf", "spatialEco")) # geo comp.

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#------------------------------Load data---------------------------------------#
plot <- fread("./Inputs/WCF_2021_plot.csv")
site <- fread("./Inputs/WCF_2021_SiteHistory.csv")
B1 <- fread("./Inputs/WCF_2021_B1Trees.csv")
A1 <- fread("./Inputs/WCF_2021_A1Trees.csv")
regen <- fread("./Inputs/WCF_2021_Regen.csv")
CWD <- fread("./Inputs/WCF_2021_CWD.csv")
FWD <- fread("./Inputs/WCF_2021_FWD.csv")
MinSoil <- fread("./Inputs/WCF_2021_MineralSoil.csv")
FrstFlr <- fread("./Inputs/WCF_2021_ForestFloor.csv")
litter <- fread("./Inputs/WCF_2021_Litter.csv")
vFWD <- fread("./Inputs/WCF_2021_FWDdestructive.csv")
transect <- fread("./Inputs/WCF_2021_TransectHorizontalDistance.csv")


######################
## Regen data 
######################

# Import data
plot<- read.xlsx("WCF_data_2021.xlsx",
                 sheetName = "Plot", 
                 header = T, 
                 stringsAsFactors = T) 

str(plot)

# Quality checking
unique(regen$PlotID)
unique(regen$Sub.Plot)
unique(regen$Species)
unique(regen$Live.Dead)
unique(regen$Height_class.cm.)
unique(regen$Tally)

max(regen$Tally)
min(regen$Tally)

# Some of the plots only had 1/2 or 1/4 of the plot measured so need to multiple some of the plots

# Some are A1 and A2 plots. These are different sizes. A1 = 5.64 m (100 ha) A2 = 3.99 m (200 ha) C = 5.64


##################
## A1 trees
##################

# Import data
a1<- read.xlsx("Raw_data/FireRehabData_v3.xlsx",
                  sheetName = "A1_trees", 
                  header = T, 
                  stringsAsFactors = T) 

str(a1)

# Change some factors to be numberic
a1$DBH <- as.numeric(as.character(a1$DBH))
a1$Height <- as.numeric(as.character(a1$Height))
a1$Crown_base_height <- as.numeric(as.character(a1$Crown_base_height))

# Quality checking
summary(a1)

unique(a1$PlotID)
unique(a1$Sub.plot)
unique(a1$Species)
unique(a1$Tree_class)
unique(a1$DBH)
unique(a1$Height_class)
unique(a1$Height)
unique(a1$Crown_base_height)
unique(a1$Insect)


hist(a1$DBH)
hist(a1$Height)
hist(a1$Crown_base_height)


####################
## B1 trees
####################

# Import data
b1<- read.xlsx("Raw_data/FireRehabData_v3.xlsx",
               sheetName = "B1_trees", 
               header = T, 
               stringsAsFactors = T) 

str(b1)

# Change some factors to be numberic
b1$DBH <- as.numeric(as.character(b1$DBH))
b1$Height <- as.numeric(as.character(b1$Height))
b1$Crown_base_height <- as.numeric(as.character(b1$Crown_base_height))

# Quality checking
summary(b1)

unique(b1$PlotID)
unique(b1$Sub.plot)
unique(b1$Species)
unique(b1$Tree_class)
unique(b1$DBH)
unique(b1$Height_class)
unique(b1$Height)
unique(b1$Crown_base_height)
unique(b1$Insect)
unique(b1$Comments)

hist(b1$DBH)
hist(b1$Height)
hist(b1$Crown_base_height)



######################
## CWD tally
######################

# Import data
cwd.tally<- read.xlsx("Raw_data/FireRehabData_v3.xlsx",
               sheetName = "CWD_tally", 
               header = T, 
               stringsAsFactors = T) 

str(cwd.tally)

# Quality checking
summary(cwd.tally)

unique(cwd.tally$PlotID)
unique(cwd.tally$Trasect_.)
unique(cwd.tally$Diam_class)
unique(cwd.tally$Location)
unique(cwd.tally$Tally)

max(cwd.tally$Tally)
hist(cwd.tally$Tally)



####################
## CWD
####################

# Import data
cwd<- read.xlsx("Raw_data/FireRehabData_v3.xlsx",
               sheetName = "CWD", 
               header = T, 
               stringsAsFactors = T) 

str(cwd)

# Change some factors to be numeric
cwd$Location_m <- as.numeric(as.character(cwd$Location_m))
cwd$Diam_cm <- as.numeric(as.character(cwd$Diam_cm))
cwd$char_depth_mm <- as.numeric(as.character(cwd$char_depth_mm))
cwd$Stump_height_cm <- as.numeric(as.character(cwd$Stump_height_cm))

# Quality check
summary(cwd)

unique(cwd$PlotID)
unique(cwd$Transect)
unique(cwd$Bearing_deg)
unique(cwd$Slope_.)
unique(cwd$Species)
unique(cwd$Location_m)
unique(cwd$Diam_cm)
unique(cwd$Decay_class)
unique(cwd$char_depth_mm)
unique(cwd$Stump_height_cm)

hist(cwd$Location_m)
hist(cwd$Diam_cm)
hist(cwd$char_depth_mm)
hist(cwd$Stump_height_cm)



####################################
## CWD transect horizontal distance
#####################################

# Import data
cwd.line<- read.xlsx("Raw_data/FireRehabData_v3.xlsx",
                sheetName = "CWD_line_slope", 
                header = T, 
                stringsAsFactors = T)

str(cwd.line)
