#assign mean vales from model output
bCombHoran<-do.call(what = "rbind", args = horanOutput)
bCombdf_horan<- as.data.frame(bCombHoran) #assign output from jags model to dataframe
bCombIndian<-do.call(what = "rbind", args = indianOutput)
bCombdf_indian<- as.data.frame(bCombIndian)

#number of days required to remove toads
horanCrit <- round((1/mean(bCombdf_horan$lambda)), 6)
horanDays<- ceiling(log(1/mean(bCombdf_horan$lambda))/log(1-mean(bCombdf_horan$p)))
temp<- summary(horanOutput)
horan95<- as.integer(c(temp$quantiles[1,1], temp$statistics[1,1], temp$quantiles[1,5])) #pull out 95% confint

indianCrit <- round((1/mean(bCombdf_indian$lambda)), 6)
indianDays<- ceiling(log(1/mean(bCombdf_indian$lambda))/log(1-mean(bCombdf_indian$p)))
temp<- summary(indianOutput)
indian95<- as.integer(c(temp$quantiles[1,1], temp$statistics[1,1], temp$quantiles[1,5])) #pull out 95 confint

#summary output
summary<- rbind(cbind(mean(bCombdf_horan$lambda), mean(bCombdf_indian$lambda)), cbind(mean(bCombdf_horan$p),mean(bCombdf_indian$p)), cbind(horanDays, indianDays) )
colnames(summary)<- c("Horan Island", "Indian Island")
row.names(summary)<- c("Population Estimate", "Detection Probability", "Mean Days to Remove")

#cost analysis
consumCost_H_mean<- sum(costData$cpu[1:4]*costData$upd[1:4]*horan95[2])
personCost_H_mean<- sum(costData$cpu[5:7]*costData$upd[5:7]*horan95[2])
travelCost_H_mean<- sum(costData$cpu[8:9]*costData$upd[8:9]*horan95[2])
tempA<- matrix(c(consumCost_H_mean, personCost_H_mean, travelCost_H_mean))

consumCost_H_95<- sum(costData$cpu[1:4]*costData$upd[1:4]*horan95[3])
personCost_H_95<- sum(costData$cpu[5:7]*costData$upd[5:7]*horan95[3])
travelCost_H_95<- sum(costData$cpu[8:9]*costData$upd[8:9]*horan95[3])
tempAa<- matrix(c(consumCost_H_95, personCost_H_95, travelCost_H_95))

consumCost_H_5<- sum(costData$cpu[1:4]*costData$upd[1:4]*horan95[1])
personCost_H_5<- sum(costData$cpu[5:7]*costData$upd[5:7]*horan95[1])
travelCost_H_5<- sum(costData$cpu[8:9]*costData$upd[8:9]*horan95[1])
tempAaa<- matrix(c(consumCost_H_5, personCost_H_5, travelCost_H_5))

consumCost_I_mean<- sum(costData$cpu[1:4]*costData$upd[1:4]*indian95[2])
personCost_I_mean<- sum(costData$cpu[5:7]*costData$upd[5:7]*indian95[2])
travelCost_I_mean<- sum(costData$cpu[8:9]*costData$upd[8:9]*indian95[2])
tempB<- matrix(c(consumCost_I_mean, personCost_I_mean, travelCost_I_mean))

consumCost_I_95<- sum(costData$cpu[1:4]*costData$upd[1:4]*indian95[3])
personCost_I_95<- sum(costData$cpu[5:7]*costData$upd[5:7]*indian95[3])
travelCost_I_95<- sum(costData$cpu[8:9]*costData$upd[8:9]*indian95[3])
tempBb<- matrix(c(consumCost_I_95, personCost_I_95, travelCost_I_95))

consumCost_I_5<- sum(costData$cpu[1:4]*costData$upd[1:4]*indian95[1])
personCost_I_5<- sum(costData$cpu[5:7]*costData$upd[5:7]*indian95[1])
travelCost_I_5<- sum(costData$cpu[8:9]*costData$upd[8:9]*indian95[1])
tempBbb<- matrix(c(consumCost_I_5, personCost_I_5, travelCost_I_5))

summaryCost<-cbind(tempA, tempAaa, tempAa, tempB, tempBbb, tempBb)
colnames(summaryCost)<- c("Horan Mean", "5% CI", "95% CI", "Indian Mean", "5% CI", "95% CI")
rownames(summaryCost) <- c("Net Consumable Cost", "Net Personal Cost", "Net Travel Cost")
summaryCost<- as.table(summaryCost)
sum(summaryCost[,1])
#Cost per area (summed km^2 cost over corresponding areas)
kiloCost_H<- sum(summaryCost[1:3,1])*(1/areaData[1,2])
periCost_H<- sum(summaryCost[1:3,1])/((areaData[1,3]))
horanIslandperi<- periCost_H*(areaData[1,3])
NSWabsent_H<- periCost_H*areaData[8,3]/1000000
WAabsent_H<- periCost_H*areaData[3,3]/1000000
NTabsent_H<- periCost_H*areaData[5,3]/1000000
NTpresent_H<- periCost_H*areaData[4,3]/1000000
QLDabsent_H<- periCost_H*areaData[7,3]/1000000
QLDpresent_H<-  periCost_H*areaData[6,3]/1000000
pilbarra_H<- periCost_H*areaData[9,3]/1000000
tempA<- matrix(c(kiloCost_H, periCost_H, horanIslandperi, NSWabsent_H, WAabsent_H, NTpresent_H, NTabsent_H, QLDpresent_H, QLDabsent_H, pilbarra_H))

kiloCost_I<- sum(summaryCost[1:3,4])/(areaData[2,2])
periCost_I<- sum(summaryCost[1:3,4])/((areaData[2,3]))
indianIslandAPeri<- periCost_I*(areaData[2,3])
NSWabsent_I<- periCost_I*areaData[8,3]/1000000
WAabsent_I<- periCost_I*areaData[3,3]/1000000
NTabsent_I<- periCost_I*areaData[5,3]/1000000
NTpresent_I<- periCost_I*areaData[4,3]/1000000
QLDabsent_I<- periCost_I*areaData[7,3]/1000000
QLDpresent_I<- periCost_I*areaData[6,3]/1000000
pilbarra_I<- periCost_I*areaData[9,3]/1000000
tempB<- matrix(c(kiloCost_I, periCost_I, indianIslandAPeri, NSWabsent_I,  WAabsent_I, NTpresent_I,NTabsent_I, QLDpresent_I, QLDabsent_I, pilbarra_I))

totalValueIndian_mean<- sum(NSWabsent_I,  WAabsent_I, NTpresent_I,NTabsent_I, QLDpresent_I, QLDabsent_I)
totalValueHoran_mean<- sum(NSWabsent_H, WAabsent_H, NTpresent_H, NTabsent_H, QLDpresent_H, QLDabsent_H)

# summaryArea_mean<- cbind(tempA, tempB)
# colnames(summaryArea_mean)<- c("Horan Island", " Indian Island")
# rownames(summaryArea_mean)<- c( "Cost per Km2", "Cost per Km of shoreline", "Island Eradication (Km shoreline)", "NSW island (toad free)","WA islands (toad free)", "NT islands (toads present)", "NT islands (toad free)", "QLD islands (toads present)", "QLD islands (toad free)", "Pilbarra waterbodies")

#------------------- LOWER 95 ---------------------------------

#Cost per area (summed km^2 cost over corresponding areas)
kiloCost_H<- sum(summaryCost[1:3,2])*(1/areaData[1,2])
periCost_H<- sum(summaryCost[1:3,2])/((areaData[1,3]))
horanIslandperi<- periCost_H*(areaData[1,3])
NSWabsent_H<- periCost_H*areaData[8,3]/1000000
WAabsent_H<- periCost_H*areaData[3,3]/1000000
NTabsent_H<- periCost_H*areaData[5,3]/1000000
NTpresent_H<- periCost_H*areaData[4,3]/1000000
QLDabsent_H<- periCost_H*areaData[7,3]/1000000
QLDpresent_H<-  periCost_H*areaData[6,3]/1000000
pilbarra_H<- periCost_H*areaData[9,3]/1000000
tempAa<- matrix(c(kiloCost_H, periCost_H, horanIslandperi, NSWabsent_H, WAabsent_H, NTpresent_H, NTabsent_H, QLDpresent_H, QLDabsent_H, pilbarra_H))

kiloCost_I<- sum(summaryCost[1:3,5])/(areaData[2,2])
periCost_I<- sum(summaryCost[1:3,5])/((areaData[2,3]))
indianIslandAPeri<- periCost_I*(areaData[2,3])
NSWabsent_I<- periCost_I*areaData[8,3]/1000000
WAabsent_I<- periCost_I*areaData[3,3]/1000000
NTabsent_I<- periCost_I*areaData[5,3]/1000000
NTpresent_I<- periCost_I*areaData[4,3]/1000000
QLDabsent_I<- periCost_I*areaData[7,3]/1000000
QLDpresent_I<- periCost_I*areaData[6,3]/1000000
pilbarra_I<- periCost_I*areaData[9,3]/1000000
tempBb<- matrix(c(kiloCost_I, periCost_I, indianIslandAPeri, NSWabsent_I,  WAabsent_I, NTpresent_I,NTabsent_I, QLDpresent_I, QLDabsent_I, pilbarra_I))

totalValueIndian_upper<- sum(NSWabsent_I,  WAabsent_I, NTpresent_I,NTabsent_I, QLDpresent_I, QLDabsent_I)
totalValueHoran_upper<- sum(NSWabsent_H, WAabsent_H, NTpresent_H, NTabsent_H, QLDpresent_H, QLDabsent_H)

# summaryArea_lower<- cbind(tempA, tempB)
# colnames(summaryArea_lower)<- c("Horan Island", " Indian Island")
# rownames(summaryArea_lower)<- c("Cost per Km2", "Cost per Km of shoreline", "Island Eradication (Km shoreline)", "NSW island (toad free)","WA islands (toad free)", "NT islands (toads present)", "NT islands (toad free)", "QLD islands (toads present)", "QLD islands (toad free)", "Pilbarra waterbodies")

#----------------------- UPPER 95 --------------------------------
#Cost per area (summed km^2 cost over corresponding areas)
kiloCost_H<- sum(summaryCost[1:3,3])*(1/areaData[1,2])
periCost_H<- sum(summaryCost[1:3,3])/((areaData[1,3]))
horanIslandperi<- periCost_H*(areaData[1,3])
NSWabsent_H<- periCost_H*areaData[8,3]/1000000
WAabsent_H<- periCost_H*areaData[3,3]/1000000
NTabsent_H<- periCost_H*areaData[5,3]/1000000
NTpresent_H<- periCost_H*areaData[4,3]/1000000
QLDabsent_H<- periCost_H*areaData[7,3]/1000000
QLDpresent_H<-  periCost_H*areaData[6,3]/1000000
pilbarra_H<- periCost_H*areaData[9,3]/1000000
tempAaa<- matrix(c(kiloCost_H, periCost_H, horanIslandperi, NSWabsent_H, WAabsent_H, NTpresent_H, NTabsent_H, QLDpresent_H, QLDabsent_H, pilbarra_H))

kiloCost_I<- sum(summaryCost[1:3,6])/(areaData[2,2])
periCost_I<- sum(summaryCost[1:3,6])/((areaData[2,3]))
indianIslandAPeri<- periCost_I*(areaData[2,3])
NSWabsent_I<- periCost_I*areaData[8,3]/1000000
WAabsent_I<- periCost_I*areaData[3,3]/1000000
NTabsent_I<- periCost_I*areaData[5,3]/1000000
NTpresent_I<- periCost_I*areaData[4,3]/1000000
QLDabsent_I<- periCost_I*areaData[7,3]/1000000
QLDpresent_I<- periCost_I*areaData[6,3]/1000000
pilbarra_I<- periCost_I*areaData[9,3]/1000000
tempBbb<- matrix(c(kiloCost_I, periCost_I, indianIslandAPeri, NSWabsent_I,  WAabsent_I, NTpresent_I,NTabsent_I, QLDpresent_I, QLDabsent_I, pilbarra_I))

totalValueIndian_lower<- sum(NSWabsent_I,  WAabsent_I, NTpresent_I,NTabsent_I, QLDpresent_I, QLDabsent_I)
totalValueHoran_lower<- sum(NSWabsent_H, WAabsent_H, NTpresent_H, NTabsent_H, QLDpresent_H, QLDabsent_H)

# summaryArea_upper<-cbind(tempA, tempB)
# colnames(summaryArea_upper)<- c("Horan Island", " Indian Island")
# rownames(summaryArea_upper)<- c( "Cost per Km2", "Cost per Km of shoreline", "Island Eradication (Km shoreline)", "NSW island (toad free)","WA islands (toad free)", "NT islands (toads present)", "NT islands (toad free)", "QLD islands (toads present)", "QLD islands (toad free)", "Pilbarra waterbodies")

summaryArea<- cbind(tempA, tempAa, tempAaa, tempB, tempBb, tempBbb)
colnames(summaryArea)<- c("Horan Mean", "5% CI", "95% CI", "Indian mean", "5% CI", "95% CI")
rownames(summaryArea)<- c( "Cost per Km2", "Cost per Km of shoreline", "Island Eradication (Km shoreline)", "NSW island (toad free)","WA islands (toad free)", "NT islands (toads present)", "NT islands (toad free)", "QLD islands (toads present)", "QLD islands (toad free)", "Pilbarra waterbodies")
