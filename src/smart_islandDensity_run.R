#####################################################################
###   For publication in Smart, Tingley & Phillips (2020)         ###
###   Estimating the benefit of quarantine:                       ###
###   eradicating invasive cane toads from islands                ###
###                                                               ###
###   Code author: Adam Smart                                     ###
###   Available from: https://github.com/assma1/islandDensity     ###
###                                                               ###
###   Data and scripts availbiel at:                              ###
###                                                               ###
###   Additional scripts: 'smart_islandDensity_functions.R'       ###
###                       'smart_islandDensity_analysis.R         ###
###                       'smart_islandDensity_figures.R'         ###
###                       'removal_model.txt'                     ###
###                       'removal_model_II.txt'                  ###
###   Required data:      'costData.xlsx'                         ###
###                       'removalData.xlsx'                      ###
###                       'toadCosting.xlsx'                      ###
###                                                               ###
###   Updated 18.05.2020                                          ###
#####################################################################


set.seed(1000)

#clear workspace
rm(list=ls())

#assign required packages
pkgs = c("rjags", "coda", "knitr", "ggplot2", "dplyr", "gridExtra", "scales", "readxl", "gdata", "RColorBrewer", "ggridges", "reshape2")

#check if required packages are installed, if not - install
new.pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new.pkgs)) install.packages(new.pkgs)

#load packages
inst = lapply(pkgs, library, character.only = TRUE)

#load in relevant datasets
removalData<- read_excel("dat/removalData.xlsx", sheet = "removal") #Number of individuals removed each night
costData<- read_excel("dat/costData.xlsx", sheet = "sumCost") #cost data from field removal
areaData<- read_excel("dat/costData.xlsx", sheet = "sumArea", col_types = c("text", "numeric", "numeric")) #island area data from goverment databases

#fit and run model
source("src/smart_islandDensity_functions.R")
#save.image("out/QVAL.RData")

#run analysis
source("src/smart_islandDensity_analysis.R")

#run figure code and save outputs
source("src/smart_islandDensity_figures.R")

#print tables
 summaryCost_kable
 summaryArea_kable
 summaryTable_kable
 summaryArea

