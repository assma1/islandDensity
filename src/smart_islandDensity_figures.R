
#raw number of removals per night
horan<- data.frame(x= removalData$removalNight, y = removalData$removalNumbers[1:6])
indian<- data.frame(x= removalData$removalNight, y = removalData$removalNumbers[7:12])

#---------------- plot posterior distribtion of horanOutput$Days ------------------------------
horanDays<- ggplot(bCombdf_horan, aes(x = days), show.legend = FALSE) + 
  geom_density(aes(), color="grey", fill = "lightgrey", show.legend = FALSE, size = 1) +
  theme(panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = "Days to eradication", y = "Density") +
  scale_y_continuous(labels = comma) + 
  scale_x_continuous(breaks = seq(from = 0, to = 200, by = 20))

#save plot to /out folder
ggsave("out/horanDays.pdf")
  
#---------------- plot posterior distribtion of indianOutput$Days --------------------------------
indianDays<- ggplot(bCombdf_indian, aes(x = days), show.legend = FALSE) + 
  geom_density(aes(), color="gray40", fill = "gray60",  show.legend = FALSE, size = 1) +
  theme(panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = "Days to eradication", y = "Density") +
  scale_y_continuous(labels = comma) + 
  scale_x_continuous(breaks = seq(from = 0, to = 40, by = 5))
  
#save plot to /out folder
ggsave("out/indianDays.pdf")

#---------------- plot raw removal numbers per night --------------------------------------
removalPanel<- ggplot(horan ,aes(x,y), show.legend = FALSE) + 
  geom_line(aes(), colour = "darkgrey", show.legend = FALSE) +
  geom_point(aes(), colour = "darkgrey", show.legend = FALSE, size = 2.3, shape = 15) +
  geom_line(data = indian, aes(), colour = "black", show.legend = FALSE) +
  geom_point(data = indian, aes(), colour = "black", show.legend = FALSE, size = 2.3, shape = 19) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_line("grey90"),
        panel.grid.minor.x = element_line("white"),
        axis.title = element_text(size = 12)) +
  labs(x = "Removal Night", y = "Individuals Removed") +
  scale_y_continuous(breaks = seq(from = 0, to = 300, by = 50)) + 
  scale_x_continuous(breaks = seq(from = 0, to = 6, by = 1))
 
#save plot to /out folder
ggsave("out/baseRemoval.pdf")

#---------------- plot estimated detection probability ----------------------------------
tempA<- bCombdf_horan
tempA$Area<- as.factor(rep("Horan Island", length(tempA$p)))

tempB<- bCombdf_indian
tempB$Area<- as.factor(rep("Indian Island", length(tempB$p)))
df<- as.data.frame(rbind(tempA, tempB))

detectionPanel<- ggplot(df, aes(x = p, y = Area)) +
  geom_density_ridges(aes(col = Area, fill = Area), scale = 5) +
  scale_colour_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("grey", "lightgrey")) +
  scale_y_discrete(name = "", limits = rev(levels(df$Area))) +
  scale_x_continuous(breaks = seq(from = 0, to = 0.4, by = 0.1), limits = c(0,0.4)) +
  theme(panel.border = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line("grey90"),
        panel.grid.minor.x = element_line("white"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.position = "none") +
  labs(x = "Estimated Detection Probability", y = "Treatment Area") 

#save plot to /out folder
ggsave("out/detEst.pdf")

#---------------- ridgleine plot of removal days ----------------------------
removalDaysPanel<- ggplot(df, aes(x = days, y = Area)) +
  geom_density_ridges(aes(col = Area, fill = Area), scale = 5) +
  scale_colour_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("grey", "lightgrey")) +
  scale_y_discrete(name = "", limits = rev(levels(df$Area))) +
  scale_x_continuous(breaks = seq(from = 0, to = 160, by = 20)) +
  theme(panel.border = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line("grey90"),
        panel.grid.minor.x = element_line("white"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.position = "none") +
  labs(x = "Days to remove") 

#save plot to /out folder
ggsave("out/rcritEst.pdf")

#---------------- plot estimated population size (Horan) ----------------------------------
horanPanel<- ggplot(bCombdf_horan, aes(x = lambda), show.legend = FALSE) + 
  geom_density(aes(), color="black", fill = "lightgrey", show.legend = FALSE, size = 1) +
  theme(panel.border = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line("grey90"),
        panel.grid.minor.x = element_line("white"),
        axis.line = element_line(colour = "black"),
        axis.line.x = element_blank(),
        axis.title = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(x= "", y = "Density") +
  xlim(100,4500)

#save plot to /out folder
#ggsave("out/popEst_horan.pdf")

#---------------- plot estimated population (indian) ----------------------------
indianPanel<- ggplot(bCombdf_indian,aes(x = lambda), show.legend = FALSE) + 
geom_density(data = bCombdf_indian, aes(), color="black", fill = "gray60",  show.legend = FALSE, size = 1) +
  theme(panel.border = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line("grey90"),
        panel.grid.minor.x = element_line("white"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = "Population Estimate", y = "Density") +
  xlim(100,4500)

#gsave("out/popEst_indian.pdf")

populationPanel<- arrangeGrob(horanPanel, indianPanel, nrow = 2, ncol = 1)

ggsave("out/popEstimatePanel.pdf", populationPanel)

#---------------- forest plot of cost over treatment areas ---------------------------
label<- c("New South Wales","Western Australia", "Northern Territory", "Northern Territory", "Queensland", "Queensland", "Pilbara Bioregion")
mean  <- as.numeric(cbind(summaryArea[4:10,1]))
lower <- as.numeric(cbind(summaryArea[4:10,2]))
upper <- as.numeric(cbind(summaryArea[4:10,3]))
Area <- c("Horan Island (HI)", "Horan Island (HI)", "HI - Toad Present", "Horan Island (HI)", "HI - Toad Present", "Horan Island (HI)", "Horan Island (HI)")
tempdf <- data.frame(label, mean, lower, upper, Area)

Area<- c("Indian Island (II)", "Indian Island (II)", "II - Toad Present", "Indian Island (II)", "II - Toad Present", "Indian Island (II)", "Indian Island (II)")
mean  <- as.numeric(cbind(summaryArea[4:10,4]))
lower <- as.numeric(cbind(summaryArea[4:10,5]))
upper <- as.numeric(cbind(summaryArea[4:10,6]))
tempdf2 <- data.frame(label, mean, lower, upper, Area)    

df<- interleave(tempdf, tempdf2)

df$label<- factor(df$label, levels = c("New South Wales", "Western Australia", "Queensland", "Northern Territory", "Pilbara Bioregion"))
df$Area<- factor(df$Area, levels = c("Indian Island (II)", "Horan Island (HI)", "II - Toad Present","HI - Toad Present"))
dodge <- position_dodge(width=0.5)  

  ggplot(data = df, aes(x = label, y = mean)) + 
  geom_point(aes(col = Area, shape = Area), position = dodge,  size = 2.3) +
  geom_errorbar(aes(ymax = upper, ymin = lower, col = Area), width = 0.2, position = dodge) +
    scale_colour_manual(values = c("gray0", "gray0", "gray60", "gray60","gray0", "gray0", "gray60", "gray60", "gray0", "gray0", "gray0", "gray0")) +
    scale_shape_manual(values=c(19, 15, 19, 15)) +
    theme(legend.position = c(0.9,0.9),
          legend.justification = c(0.9,0.9),
          legend.background = element_rect(fill = "white", colour = "black", linetype = "solid"),
          legend.key=element_blank(),
          legend.title = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"), 
          panel.grid.major.x = element_line("grey90"),
          panel.grid.minor.x = element_line("white"),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12)) +
  ylab("Cost (AU Millions)") + 
  xlab("Treatment Area") +
  coord_flip() +
  scale_x_discrete(name = "", limits = rev(levels(df$label))) + 
  scale_y_continuous(breaks = seq(from = 0, to = 250, by = 25), labels = c("0", "25", "50", "75", "100", "125", "150", "175", "200", "225", "250"))

ggsave("out/costTreatmentArea.pdf")

#---------------- forest plot to compare cost estimators ----------------------------------------
label<- c("Per square kilometer of landmass","Per kilometer of freshwater shoreline")
mean  <- as.numeric(cbind(summaryArea[1:2,1]))
lower <- as.numeric(cbind(summaryArea[1:2,2]))
upper <- as.numeric(cbind(summaryArea[1:2,3]))
Area <- rep("Horan Island", 2)
tempdf <- data.frame(label, mean, lower, upper, Area)

Area<- rep("Indian Island", 2)
mean  <- as.numeric(cbind(summaryArea[1:2,4]))
lower <- as.numeric(cbind(summaryArea[1:2,5]))
upper <- as.numeric(cbind(summaryArea[1:2,6]))
tempdf2 <- data.frame(label, mean, lower, upper, Area)    

df<- interleave(tempdf, tempdf2)

df$label<- factor(df$label, levels = c("Per kilometer of freshwater shoreline", "Per square kilometer of landmass"))
df$Area<- factor(df$Area, levels = c("Indian Island", "Horan Island"))

dodge <- position_dodge(width=0.5)  

ggplot(data = df, aes(x = label, y = mean)) + 
  geom_point(aes(col = Area, shape = Area), position = dodge,  size = 2.3) +
  geom_errorbar(aes(ymax = upper, ymin = lower, col = Area), width = 0.2, position = dodge) +
  scale_colour_manual(values = c("gray0", "gray60")) +
  scale_shape_manual(values=c(19, 15)) +
  theme(legend.position = c(0.95,0.95),
        legend.justification = c(0.95,0.95),
        legend.background = element_rect(fill = "white", colour = "black", linetype = "solid"),
        legend.key=element_blank(),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"), 
        panel.grid.major.x = element_line("grey90"),
        panel.grid.minor.x = element_line("white"),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  ylab("Cost '000s (AU)") + 
  xlab("Treatment Area") +
  coord_flip() +
  scale_x_discrete(name = "", limits = rev(levels(df$label))) +
  scale_y_continuous(breaks = seq(from = 0, to = 280000, by = 40000), labels = c("0", "40", "80", "120", "160", "200", "240", "280"))

ggsave("out/costEstimators.pdf")

#---------------- save out cost tables ---------------------------
summaryCost_kable<- kable(summaryCost, caption = "1. Estimated costs incurred from toad eradication programs on Horan and Indian Island", format = "pandoc") #%>%
summaryArea_kable<- kable(summaryArea, caption = "2. Value of Biosecurity measures in Northern Australia in AU. Estimates for each state are in millions", format = "pandoc")
summaryTable_kable<- kable(summary, caption = "3. Summary table of Indian Island and Horan Island removal efforts", format = "pandoc", digits = 2)

#---------------- save out density estimators ---------------------------
tempA<- summary(horanOutput)
tempB<- summary(indianOutput)

label<- as.character(c("Horan Island","Indian Island", "Horan Island","Indian Island"))
treatment<- as.character(c("Per km (areal)", "Per km (areal)", "Per km of shoreline (linear)","Per km of shoreline (linear)"))
mean<- as.numeric(c((mean(bCombdf_horan$lambda)*(1/areaData[1,2])), (mean(bCombdf_indian$lambda)/areaData[2,2]), (mean(bCombdf_horan$lambda)/areaData[1,3]), (mean(bCombdf_indian$lambda)/areaData[2,3])))
lower<- as.numeric(c(tempA$quantiles[2,1]*(1/areaData[1,2]), tempB$quantiles[2,1]/areaData[2,2], tempA$quantiles[2,1]/areaData[1,3], tempB$quantiles[2,1]/areaData[2,3]))
upper<- as.numeric(c(tempA$quantiles[2,5]*(1/areaData[1,2]), tempB$quantiles[2,5]/areaData[2,2], tempA$quantiles[2,5]/areaData[1,3], tempB$quantiles[2,5]/areaData[2,3]))

df <- data.frame(label, treatment, mean, lower, upper)

plot<- ggplot(df, aes(x = treatment, y = mean, fill = label)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.5) +
  scale_fill_manual(values = c("gray40", "gray80")) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.5)) +
  theme(legend.position = c(0.95,0.90),
        legend.justification = c(0.95,0.90),
        legend.background = element_rect(fill = "white", colour = "black", linetype = "solid"),
        legend.key=element_blank(),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"), 
        panel.grid.major.x = element_line("white"),
        panel.grid.minor.x = element_line("white"),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  ylab("Density of Individuals") + 
  xlab("Study Area") +
  scale_y_continuous(breaks = seq(from = 0, to = 3500, by = 500), labels = seq(from = 0, to = 3500, by =500))

ggsave("out/densityAreaEstimators.pdf")









