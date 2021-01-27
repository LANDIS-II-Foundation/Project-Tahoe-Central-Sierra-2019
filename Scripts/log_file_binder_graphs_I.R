####Call necessary packages###
library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
library(tidyr)
library(spatialEco)
library(ggpubr)
library(RColorBrewer)
library(stringr)
library(cowplot)

#####color schema####

mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 12), brewer.pal(name = "Accent", n = 8), brewer.pal(name = "Set3", n = 6))

######Set working directory####


p <- "I:/TCSI/Round2_outputs/"

setwd(p)

####iterate through different climates to join the data tables####

clim_dir <- c("CANESM85", "CNRM85", "GFDL85", "HADGEM85", "MIROC5_85")

nsl_data <- NULL
sl_data <- NULL
bdl_data <- NULL
snsl_data <- NULL
spl_data <- NULL 
h_data <- NULL
repro_data <- NULL

for(i in clim_dir){
  
  path <- paste0(p,i,"/")
  lf <- list.files(path = path, full.names = F, include.dirs = T)
  
  for(j in lf){
    
    tname <- unlist(strsplit(j, "_"))
    climate <- i
    ssp <- tname[1]
    scenario <- tname[2]
    rep <- tname[3]
    
    h <- read.csv(paste0(p,i,"/",j,"/harvest/summary-log.csv"))
    h <- cbind(h, climate, ssp, scenario, rep)
    h_data<- rbind(h, h_data)
    
    nsl <- read.csv(paste0(p,i,"/",j,"/NECN-succession-log.csv"))
    nsl <- cbind(nsl, climate, ssp, scenario, rep)
    nsl_data<- rbind(nsl, nsl_data)
    
    sl <- read.csv(paste0(p,i,"/",j,"/scrapple-events-log.csv"))
    sl <- cbind(sl, climate, ssp, scenario, rep)
    sl_data<- rbind(sl, sl_data)        
    
    bdl <- read.csv(paste0(p,i,"/",j,"/bda_log.csv"))
    bdl <- cbind(bdl, climate, ssp, scenario, rep)
    bdl_data<- rbind(bdl, bdl_data)        
  
    snsl <- read.csv(paste0(p,i,"/",j,"/NECN-succession-log-short.csv"))
    snsl <- cbind(snsl, climate, ssp, scenario, rep)
    snsl_data<- rbind(snsl, snsl_data)
    
    spl <- read.csv(paste0(p,i,"/",j,"/spp-biomass-log.csv"))
    spl <- cbind(spl, climate, ssp, scenario, rep)
    spl_data<- rbind(spl, spl_data)
    
    repro <- read.csv(paste0(p,i,"/",j,"/NECN-reproduction-log.csv"))
    repro <- cbind(repro, climate, ssp, scenario, rep)
    repro_data<- rbind(repro, repro_data)
    }
  }

write.csv(nsl_data, "I:/TCSI/Round2_outputs/nsl_data.csv")
write.csv(sl_data, "I:/TCSI/Round2_outputs/sl_data2.csv")
write.csv(bdl_data, "I:/TCSI/Round2_outputs/bdl_data.csv")
write.csv(snsl_data, "I:/TCSI/Round2_outputs/snsl_data.csv")
write.csv(spl_data, "I:/TCSI/Round2_outputs/spl_data.csv")
write.csv(h_data, "I:/TCSI/Round2_outputs/h_data.csv")
write.csv(repro_data, "I:/TCSI/Round2_outputs/repro_data.csv")

####full harvest log#########
h_all_data <- NULL

for(i in clim_dir){
  
  path <- paste0(p,i,"/")
  lf <- list.files(path = path, full.names = F, include.dirs = T)
  
  for(j in lf){
    
    tname <- unlist(strsplit(j, "_"))
    climate <- i
    ssp <- tname[1]
    scenario <- tname[2]
    rep <- tname[3]
    
    h_all <- read.csv(paste0(p,i,"/",j,"/harvest/log.csv"))
    h_all <- cbind(h_all, climate, ssp, scenario, rep)
    h_all_data<- rbind(h_all, h_all_data)
  }
}

h_all_data$new_id <- sub("(.*)_([A-Z]+)", h_all_data$Prescription)

h_all_short <- h_all_data %>%
  filter(MgBiomassRemoved > 0) %>%
  mutate(newid4 = str_extract(Prescription, "(.*)_([A-Z]+)"))


h_all_short2 <- h_all_short %>%
  group_by(newid4, Time) %>%
  summarise(meanArea = mean(NumberOfSites * 3.24), meanBiomRem = mean(MgBiomassRemoved), meanBioRemPDH = mean( MgBioRemovedPerDamagedHa))

h_all_short3 <- h_all_short %>%
  group_by(newid4, Time, climate, scenario, rep) %>%
  summarise(sumArea = sum(NumberOfSites) * 3.24, sumBiomRem = sum(MgBiomassRemoved))

write.csv(h_all_short2, "mean_area_biomrem_byprescription.csv")

####REPRODUCTION_DATA############
repro_data <- read.csv("I:/TCSI/Round2_outputs/repro_data.csv")

rep_data <- repro_data %>%
  mutate(Year = Time + 2019) %>%
  group_by(Climate, Species, Year) %>%
  summarise(meanTOT = mean(Total), sdTOT = sd(Total))

rdat <- ggplot(data = rep_data, aes(x = Year, y = meanTOT, fill = Species)) +
  geom_line() +
  geom_ribbon(aes(ymax = meanTOT + sdTOT, ymin = meanTOT - sdTOT)) +
  facet_wrap(~Climate, ncol = 5) +
  theme(legend.position="bottom") +
  labs(y = "Number of cohorts")

plot(rdat)

####NEEC#####
snsl_data <- read.csv("I:/TCSI/Round2_outputs/snsl_data.csv")

snsl_data <- snsl_data[,-1]
snsl_data <- snsl_data[,-8]

colnames(snsl_data) <- c("Time", "NEEC", "SOMTC", "AGB", "NPP", "MineralN", "C_Deadwood", "Climate","SSP", "Scenario", "Replicate")

snsl_data <- snsl_data %>%
  mutate(Year = Time + 2019) %>%
  filter(Time != 0)

snsl <- ggplot(snsl_data, aes(x = Year, y = NEEC, color = Scenario)) +
  geom_line() + 
  facet_wrap(~Climate)

plot(snsl)

sns_clim <- snsl_data %>%
  group_by(Year, Climate) %>%
  summarise(meanNEE = mean(NEEC), sdNEE = sd(NEEC), meanAGB = mean(AGB), sdAGB = sd(AGB), meanNPP = mean(NPP), sdNPP = sd(NPP))

snsc <- ggplot(sns_clim, aes(x = Year, y = meanNEE, color = Climate, fill = Climate)) +
  geom_line() + 
  geom_ribbon(aes(ymax = meanNEE + sdNEE, ymin = meanNEE - sdNEE), alpha = 0.5) +
  geom_smooth() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(y = "NEE (g C m^-2")

plot(snsc)

sns_scen <- snsl_data %>%
  group_by(Year, Scenario) %>%
  summarise(meanNEE = mean(NEEC), sdNEE = sd(NEEC), meanAGB = mean(AGB), sdAGB = sd(AGB), meanNPP = mean(NPP), sdNPP = sd(NPP))

snss <- ggplot(sns_scen, aes(x = Year, y = meanNEE, color = Scenario, fill = Scenario)) +
  geom_line() + 
  geom_ribbon(aes(ymax = meanNEE + sdNEE, ymin = meanNEE - sdNEE), alpha = 0.5) +
  geom_smooth() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(y = "NEE (g C m^-2")

plot(snss)

png(file = "NEEC_fig7_2.png", width = 4800, height = 3000, res=600)
ggarrange(snsc, snss, legend = "right")
dev.off()

sns_both <- snsl_data %>%
  group_by(Year, Climate, Scenario) %>%
  summarise(meanNEE = mean(NEEC), sdNEE = sd(NEEC), meanAGB = mean(AGB), sdAGB = sd(AGB), meanNPP = mean(NPP), sdNPP = sd(NPP))

snsboth <- ggplot(sns_both, aes(x = Year, y = meanNEE, color = Scenario, fill = Scenario)) +
  geom_line() + 
  geom_ribbon(aes(ymax = meanNEE + sdNEE, ymin = meanNEE - sdNEE), alpha = 0.5) +
  geom_smooth() +
  facet_wrap(~Climate) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(y = "NEE (g C m^-2")

plot(snsboth)

snsagb <- ggplot(sns_clim, aes(x = Year, y = meanAGB, color = Climate, linetype = Climate, fill = Climate)) +
  geom_line() + 
  geom_ribbon(aes(ymax = meanAGB + sdAGB, ymin = meanAGB - sdAGB)) +
  geom_smooth()

plot(snsagb)

sns_scen <- snsl_data %>%
  group_by(Year, Scenario) %>%
  summarise(meanNEE = mean(NEEC), sdNEE = sd(NEEC), meanAGB = mean(AGB), sdAGB = sd(AGB), meanNPP = mean(NPP), sdNPP = sd(NPP))

snss <- ggplot(sns_scen, aes(x = Year, y = meanNEE, color = Scenario, fill = Scenario)) +
  geom_line() + 
  geom_ribbon(aes(ymax = meanNEE + sdNEE, ymin = meanNEE - sdNEE)) +
  geom_smooth() +
  geom_hline(yintercept = 0)

plot(snss)

snssagb <- ggplot(sns_scen, aes(x = Year, y = meanAGB, color = Scenario, fill = Scenario)) +
  geom_line() + 
  geom_ribbon(aes(ymax = meanAGB + sdAGB, ymin = meanAGB - sdAGB), alpha = 0.5) +
  geom_smooth()
plot(snssagb)

####CARBON####
nsl_data <- read.csv( "I:/TCSI/Round2_outputs/nsl_data.csv")

nsl_data <- nsl_data %>%
  mutate(uTOTC = SOMTC + C_LiveLeaf + C_LiveFRoot + C_LiveWood + C_LiveCRoot + C_DeadWood + C_DeadCRoot + C_DeadLeaf_Struc + C_DeadLeaf_Meta) %>%
  mutate(wLive = (C_LiveLeaf + C_LiveFRoot + C_LiveWood + C_LiveCRoot) * NumSites / 275414) %>% 
  mutate(wSOMTC = (SOMTC * NumSites) / 275414) %>%
  mutate(wDead = ((C_DeadWood + C_DeadCRoot + C_DeadLeaf_Struc + C_DeadLeaf_Meta) * NumSites) / 275414) %>%
  mutate(wTOTC = (uTOTC * NumSites) / 275414) %>%
  mutate(Year = Time + 2019)

year_nsl_data <- nsl_data %>%
  group_by(Year, climate, ssp, scenario, rep) %>%
  summarise(TOTC = sum(wTOTC)/100, LIVEC = sum(wLive) / 100, SOILC = sum(wSOMTC)/100, DEADC = sum(wDead)/100)

mean_year_nsl_data <- year_nsl_data %>%
  group_by(Year, climate, scenario) %>%
  summarise(meanTOTC = mean(TOTC), sdTOTC = sd(TOTC), meanLiveC = mean(LIVEC), sdLiveC = sd(LIVEC), meanDEADC = mean(DEADC), sdDEADC = sd(DEADC), meanSOILC = mean(SOILC), sdSOILC = sd(SOILC) )
           
colnames(mean_year_nsl_data) <- c("Year", "Climate", "Scenario", "TotalC", "sdTOTC", "LiveC", "sdLIVEC",  "DeadC", "sdDEADC", "SoilC", "sdSOILC")

mean_year_nsl_climate <- year_nsl_data %>%
  group_by(Year, climate) %>%
  summarise(meanTOTC = mean(TOTC), sdTOTC = sd(TOTC), meanLiveC = mean(LIVEC), sdLiveC = sd(LIVEC), meanDEADC = mean(DEADC), sdDEADC = sd(DEADC), meanSOILC = mean(SOILC), sdSOILC = sd(SOILC) )

colnames(mean_year_nsl_climate) <- c("Year", "Climate", "TotalC", "sdTOTC", "LiveC", "sdLIVEC",  "DeadC", "sdDEADC", "SoilC", "sdSOILC")

nomiroc <- year_nsl_data %>%
  filter(climate != "MIROC5_85")

mean_year_nsl_scenario <- nomiroc %>%
  group_by(Year, scenario) %>%
  summarise(meanTOTC = mean(TOTC), sdTOTC = sd(TOTC), meanLiveC = mean(LIVEC), sdLiveC = sd(LIVEC), meanDEADC = mean(DEADC), sdDEADC = sd(DEADC), meanSOILC = mean(SOILC), sdSOILC = sd(SOILC) )

mean_year_nsl_both <- year_nsl_data %>%
  group_by(Year, scenario, climate) %>%
  summarise(meanTOTC = mean(TOTC), sdTOTC = sd(TOTC), meanLiveC = mean(LIVEC), sdLiveC = sd(LIVEC), meanDEADC = mean(DEADC), sdDEADC = sd(DEADC), meanSOILC = mean(SOILC), sdSOILC = sd(SOILC) )

colnames(mean_year_nsl_scenario) <- c("Year", "Scenario", "TotalC", "sdTOTC", "LiveC", "sdLIVEC",  "DeadC", "sdDEADC", "SoilC", "sdSOILC")

write.csv(mean_year_nsl_data, "F:/TCSI/Round2_outputs/mean_year_nsl_data.csv")

totc <- ggplot(data = mean_year_nsl_data, aes(x = Year, y = TotalC, fill = Scenario, color = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymax = TotalC + sdTOTC, ymin = TotalC - sdTOTC), alpha = 0.5) +
  facet_wrap(~Climate) +
  coord_cartesian(ylim = c(0, 310)) +
  labs(y = "Total C in Mg/ha")

plot(totc)  

climc <- ggplot(data = mean_year_nsl_climate, aes(x = Year, y = TotalC, fill = Climate, color = Climate)) +
  geom_line() +
  geom_ribbon(aes(ymax = TotalC + sdTOTC, ymin = TotalC - sdTOTC), alpha = 0.5) +
  coord_cartesian(ylim = c(0, 310)) +
  theme_bw() +
  labs(y = "Total C in Mg/ha")

plot(climc)  

scenc <- ggplot(data = mean_year_nsl_scenario, aes(x = Year, y = TotalC, fill = Scenario, color = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymax = TotalC + sdTOTC, ymin = TotalC - sdTOTC), alpha = 0.5) +
  coord_cartesian(ylim = c(0, 310)) +
  theme_bw() +
  labs(y = "Total C in Metric tons/ha")

plot(scenc)  

png(file = "TOTC_fig7_2.png", width = 4800, height = 3000, res=600)
ggarrange(climc, scenc, legend = "right")
dev.off()

bothc <- ggplot(data = mean_year_nsl_both, aes(x = Year, y = meanTOTC, fill = scenario, color = scenario)) +
  geom_line() +
  geom_ribbon(aes(ymax = meanTOTC + sdTOTC, ymin = meanTOTC - sdTOTC), alpha = 0.5) +
  coord_cartesian(ylim = c(0, 310)) +
  facet_wrap(~climate)+
  theme_bw() +
  labs(y = "Total C in Mg/ha")

plot(bothc)

####FIRE####

sl_data <- read.csv("I:/TCSI/Round2_outputs/sl_data.csv")

##
year_sl_data <- sl_data %>%
  mutate(type = case_when(
    IgnitionType %in% c(" Lightning", " Accidental") ~ "Wildfire",
    IgnitionType %in% " Rx" ~ "Rx")) %>%
  group_by(SimulationYear, type, climate, ssp, scenario, rep) %>%
  summarise(Low = sum(NumberCellsSeverity1) * 3.24, Moderate = sum(NumberCellsSeverity2) * 3.24, High = sum(NumberCellsSeverity3) * 3.24, Total = sum(TotalSitesBurned) *3.24)

ysl_data <- year_sl_data %>%
  pivot_longer(cols = Low:Total, names_to = "Severity")

ysl_rx <- ysl_data %>%
  filter(type == "Rx" & Severity == "Total") %>%
  mutate(Severity = case_when(
   Severity == "Total" ~ "Rx"
  ))


ysl_wild <- ysl_data %>%
  filter(type == "Wildfire")

ysl_final <- rbind(ysl_wild, ysl_rx) %>%
  mutate(Year = SimulationYear + 2019) 

colnames(ysl_final) <- c("Time", "Type", "Climate", "SSP", "Scenario", "Replicate", "Severity", "Area", "Year")

write.csv(ysl_final, "fire_for_Cumulative2.csv")

####cumulative within dplyr isn't working for me####

ysl_final <- ysl_final %>%
  filter(Severity == "High" & Area > 100) %>%
  group_by(Climate, Scenario, Severity) %>%
  mutate(cumulative = cumsum(Area))

ysl <- ggplot(ysl_final, aes( x = Year, y = cumulative, color = Severity)) + 
  geom_line() +
  facet_wrap(~Climate * Scenario) 
#  coord_cartesian(ylim = c(0,500000))

plot(ysl)

ysl_clim <- ysl_final %>%
  group_by(Climate, Year) %>%
  summarise(meanCum = mean(cumulative), sdCum = sd(cumulative, na.rm =T)) 
#%>%   filter(Severity != "Rx")

yslc <- ysl_clim %>%
#  filter(Severity == "High") %>%
  ggplot(aes( x = Year, y = meanCum, fill = Climate)) + 
  geom_line() +
  geom_ribbon(aes(ymax = meanCum + sdCum, ymin = meanCum - sdCum))

plot(yslc)

ysl_scen <- ysl_final %>%
  group_by(Scenario, Year) %>%
  summarise(meanCum = mean(cumulative), sdCum = sd(cumulative, na.rm =T)) 
#%>%  filter(Severity != "Rx")

ysls <- ysl_scen %>%
#  filter(Severity == "High") %>%
  ggplot(aes( x = Year, y = meanCum, fill = Scenario)) + 
  geom_line() +
  geom_ribbon(aes(ymax = meanCum + sdCum, ymin = meanCum - sdCum), alpha = 0.5) +
#  + facet_wrap(~Scenario)
  coord_cartesian(ylim = c(0,650000))

plot(ysls)

###
year_cum_fire <- read.csv("fire_for_Cumulative.csv")

ysl_clim <- year_cum_fire %>%
  filter(Severity == "High") %>%
  group_by(Climate, Year) %>%
  summarise(meanCum = mean(cumHSAREA), sdCum = sd(cumHSAREA, na.rm =T), meanLP = mean(cumLPHS), sdLP = sd(cumLPHS)) 
#%>%   filter(Severity != "Rx")

yslc <- ysl_clim %>%
  #  filter(Severity == "High") %>%
  ggplot(aes( x = Year, y = meanCum, fill = Climate)) + 
  geom_line() +
  geom_ribbon(aes(ymax = meanCum + sdCum, ymin = meanCum - sdCum))

plot(yslc)

yslc_lp <- ysl_clim %>%
  #  filter(Severity == "High") %>%
  ggplot(aes( x = Year, y = meanLP, fill = Climate)) + 
  geom_line() +
  geom_ribbon(aes(ymax = meanLP + sdLP, ymin = meanLP - sdLP)) +
  theme_bw() +
  labs(y = "Cumulative area of high severity fire in patches greater than 100 hectares")  

plot(yslc_lp)

ysl_scen <- year_cum_fire %>%
  filter(Severity == "High") %>%
  group_by(Scenario, Year) %>%
  summarise(meanCum = mean(cumHSAREA), sdCum = sd(cumHSAREA, na.rm =T), meanLP = mean(cumLPHS), sdLP = sd(cumLPHS)) 
#%>%  filter(Severity != "Rx")

ysls_LP <- ysl_scen %>%
  #  filter(Severity == "High") %>%
  ggplot(aes( x = Year, y = meanLP, fill = Scenario, color = Scenario)) + 
  geom_line() +
  geom_ribbon(aes(ymax = meanLP + sdLP, ymin = meanLP - sdLP), alpha = 0.5) +
  geom_smooth() +
  theme_bw() +
  labs(y = "")  

plot(ysls_LP)

png(file = "tcsi_figure6_cumarea_hslp.png", width = 4800, height = 3600, res=600)
ggarrange(yslc_lp, ysls_LP)
dev.off()


#####

sl_data2 <- sl_data %>%
  mutate(type = case_when(
    IgnitionType %in% c(" Lightning", " Accidental") ~ "Wildfire",
    IgnitionType %in% " Rx" ~ "Rx")) %>%
  mutate(HSfire = NumberCellsSeverity3 * 3.24) %>%
  mutate(MSfire = NumberCellsSeverity2 * 3.24) %>%
  mutate(HSMS = HSfire + MSfire) %>%
#  rename(Climate = i, Scenario = j) %>%
  mutate(Year = SimulationYear + 2019) %>%
  mutate(decade = case_when(SimulationYear <10 ~ "2020s",
                           SimulationYear <20 ~ "2030s",
                           SimulationYear <30 ~ "2040s",
                           SimulationYear <40 ~ "2050s",
                           SimulationYear <50 ~ "2060s",
                           SimulationYear <60 ~ "2070s",
                           SimulationYear <70 ~ "2080s",
                           SimulationYear <81 ~ "2090s",
                           )) 
  
write.csv(sl_data2, "fwi_vs_hsf.csv")

sl_data2 <- read.csv("fwi_vs_hsf.csv") 

sl_data2 <- sl_data2[,-c(1,2,28)]
#sl_data2 <- sl_data2[,-c(1,27)]

sl_data2$climate <- as.factor(sl_data2$climate)
sl_data2$scenario <- as.factor(sl_data2$scenario)

#####large fire occurrence####

megangmn <- sl_data2 %>%
  mutate(TSB  = TotalSitesBurned * 3.24) %>%
  filter(TSB > 10000)

sl_data2 <- 
  group_by(scenario, climate, rep) %>%
  summarise(across())
?across
write.csv(megangmn, "large_fires.csv")

###cumulative areas

cumarea <- sl_data2 %>%
  group_by(scenario, climate, Year, rep) %>%
  summarise(sumAREA = sum(HSMS))

cumarea2 <- cumarea %>%
  group_by(scenario, climate, Year) %>%
  summarise(meanAREA = mean(sumAREA), sdAREA = sd(sumAREA))

write.csv(cumarea, "HSMS_fire_area_notcumulative.csv")
tcumarea2 <- read.csv("HS_fire_area_notcumulative.csv")

cumarea_scen <- cumarea2 %>%
  group_by(scenario) %>%
  summarise(meanAREA = mean(cumAREA), sdAREA = sd(cumAREA))

cumarea_clim <- cumarea2 %>%
  group_by(climate) %>%
  summarise(meanAREA = mean(cumAREA), sdAREA = sd(cumAREA))

csplot <- ggplot(data = cumarea2, aes(x = Year, y = meanAREA, color = scenario, fill = scenario)) +
  geom_line() +
  geom_ribbon(aes(ymax = meanAREA + sdAREA, ymin = meanAREA)) +
  facet_wrap(~climate) +
  theme_bw() + 
  guides(color = F) +
  labs(y = "High Severity Fire Area (hectares)", fill = "Scenario")

plot(csplot)

tcsplot <- tcumarea2 %>%
  filter(climate == "MIROC5_85") %>%
  ggplot(aes(x = Year, y = cumAREA, color = scenario, fill = scenario)) +
  geom_line() +
  geom_ribbon(aes(ymax = cumAREA + sdAREA, ymin = cumAREA)) +
  facet_wrap(~climate) +
  theme_bw() + 
  guides(color = F) +
  labs(y = "High Severity Fire Area (hectares)", fill = "Scenario")

plot(tcsplot)

ccplot <- ggplot(data = cumarea_clim, aes(x = climate, y = meanAREA, fill = climate)) +
  geom_col() +
  geom_errorbar(aes(ymax = meanAREA + sdAREA, ymin = meanAREA)) +
  theme_bw() + 
  labs(x = "Climate Projection", y = "Cumulative High Severity Fire Area (hectares)", fill = "Climate Projection")

plot(ccplot)

###cumulative area through time

cumarea_t <- sl_data2 %>%
  group_by(scenario, climate, Year, rep) %>%
  summarise(sumHSarea = sum(HSfire), sumHSMSarea = sum(HSMS), sumMSarea = sum(MSfire))

write.csv(cumarea_t, "tobe_cumulative_fire.csv")
#passed out to excel to add cumulative through years
cumarea_t <- read.csv("tobe_cumulative_fire.csv")

cumarea_scen_t <- cumarea_t %>%
  group_by(scenario, Year) %>%
  summarise(meanHS = mean(cumHS), sdHS = sd(cumHS), meanHSMS = mean(cumHSMS), sdHSMS = sd(cumHSMS),  meanMS = mean(cumMS), sdMS = sd(cumMS))

cumarea_clim_t <- cumarea_t %>%
  group_by(climate, Year) %>%
  summarise(meanHS = mean(cumHS), sdHS = sd(cumHS), meanHSMS = mean(cumHSMS), sdHSMS = sd(cumHSMS),  meanMS = mean(cumMS), sdMS = sd(cumMS))

cast <- ggplot(data = cumarea_scen_t, aes(x = Year, y = meanHS, color = scenario)) +
  geom_line() +
  geom_ribbon(aes(ymax = meanHS + sdHS, ymin = meanHS)) +
  theme_bw() +
  labs()

plot(cast)

#####decade fire comparisons##############

decade_total <- sl_data2 %>%
  group_by(scenario, climate, rep, decade) %>%
  summarise(sumHS = sum(HSfire), sumHSMS = sum(HSMS))

decade_total$decade <- as.factor(decade_total$decade)

decade_mean <- decade_total %>%
  group_by(scenario, climate, decade) %>%
  summarise(HSmean = mean(sumHS), HSsd = sd(sumHS), HSMSmean = mean(sumHSMS), HSMSsd = sd(sumHSMS))

compare_means(HSmean ~ scenario, data = decade_mean, method = "anova")

decade_mean <- decade_total %>%
  group_by(scenario, decade) %>%
  summarise(HSmean = mean(sumHS), HSsd = sd(sumHS), HSMSmean = mean(sumHSMS), HSMSsd = sd(sumHSMS))

dec_HS <- ggplot(decade_mean, aes(x = decade, y = HSmean, fill = scenario)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymax = HSmean + HSsd, ymin = HSmean), position = "dodge") +
  theme_bw() +
  labs(x = "Decade", y = "High Severity Fire Area (hectares)", fill = "Scenario")

plot(dec_HS)

dec_HSMS <- ggplot(decade_mean, aes(x = decade, y = HSMSmean, fill = scenario)) +
  geom_errorbar(aes(ymax = HSMSmean + HSMSsd, ymin = HSMSmean), position = "dodge") +
  geom_col(position = "dodge") + 
  coord_cartesian(ylim = c(0,200000)) +
  theme_bw() +
  labs(x = "Decade", y = "Moderate and High Severity Fire Area (hectares)", fill = "Scenario")

plot(dec_HSMS)

#####total fire comparison##############
cent_total_rx <- sl_data2 %>%
  filter(IgnitionType == " Rx") %>%
  group_by(scenario, climate, Year, rep) %>%
  summarise(sumRx = sum(TotalSitesBurned)*3.24)
           
write.csv(cent_total_rx, "cent_total_rx.csv")

century_total <- sl_data2 %>%
  group_by(scenario, climate, rep) %>%
  summarise(sumHS = sum(HSfire), sumHSMS = sum(HSMS))

century_mean <- century_total %>%
  group_by(scenario, climate) %>%
  summarise(HSmean = mean(sumHS), HSsd = sd(sumHS), HSMSmean = mean(sumHSMS), HSMSsd = sd(sumHSMS))

cent_HSMS_facet <- ggplot(century_mean, aes(x = scenario, y = HSMSmean, fill = scenario)) +
  geom_col(position = "dodge") + 
  theme_bw() +
  geom_errorbar(aes(ymax = HSMSmean + HSMSsd, ymin = HSMSmean), position = "dodge") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  facet_wrap(~climate) +
  labs(x = "Scenario", y = "Moderate + High Severity Fire Area (hectares)", fill = "Scenario")

plot(cent_HSMS_facet)

cent_HS_facet <- ggplot(century_mean, aes(x = scenario, y = HSmean, fill = scenario)) +
  geom_col(position = "dodge") + 
  theme_bw() +
  geom_errorbar(aes(ymax = HSmean + HSsd, ymin = HSmean), position = "dodge") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  facet_wrap(~climate) +
  labs(x = "Scenario", y = "High Severity Fire Area (hectares)", fill = "Scenario")

plot(cent_HS_facet)

###large patch again

century_total <- sl_data2 %>%
  filter(HSfire > 100) %>%
  group_by(scenario, climate, rep) %>%
  summarise(sumHS = sum(HSfire))

century_mean <- century_total %>%
  group_by(scenario, climate) %>%
  summarise(HSmean = mean(sumHS), HSsd = sd(sumHS))

cent_HSLP_facet <- ggplot(century_mean, aes(x = scenario, y = HSmean, fill = scenario)) +
  geom_col(position = "dodge") + 
  theme_bw() +
  geom_errorbar(aes(ymax = HSmean + HSsd, ymin = HSmean), position = "dodge") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  facet_wrap(~climate) +
  labs(x = "Scenario", y = "High Severity Fire Area in Large Patches (hectares)", fill = "Scenario")

plot(cent_HSLP_facet)

century_mean$scenario <- as.factor(century_mean$scenario)
century_mean$climate <- as.factor(century_mean$climate)

compare_means(HSmean ~ climate, data = century_mean)
compare_means(HSmean ~ scenario, data = century_mean)

compare_means(HSMSmean ~ climate, data = century_mean)
compare_means(HSMSmean ~ scenario, data = century_mean)

century_mean_miroc <- century_total %>%
  filter(climate == "MIROC5_85") %>%
  group_by(scenario) %>%
  summarise(HSmean = mean(sumHS), HSsd = sd(sumHS), HSMSmean = mean(sumHSMS), HSMSsd = sd(sumHSMS))

cent_HSMS_miroc <- ggplot(century_mean_miroc, aes(x = scenario, y = HSMSmean, fill = scenario)) +
  geom_col(position = "dodge") + 
  theme_bw() +
  geom_errorbar(aes(ymax = HSMSmean + HSMSsd, ymin = HSMSmean), position = "dodge") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(title = "MIROC only", x = "Scenario", y = "Moderate + High Severity Fire Area (hectares)", fill = "Scenario")

plot(cent_HSMS_miroc)

century_mean_nomiroc <- century_total %>%
  filter(climate != "MIROC5_85") %>%
  group_by(scenario) %>%
  summarise(HSmean = mean(sumHS), HSsd = sd(sumHS), HSMSmean = mean(sumHSMS), HSMSsd = sd(sumHSMS))

cent_HSMS_nomiroc <- ggplot(century_mean_nomiroc, aes(x = scenario, y = HSMSmean, fill = scenario)) +
  theme_bw() +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymax = HSMSmean + HSMSsd, ymin = HSMSmean), position = "dodge") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(title = "Other Climates", x = "Scenario", y = "", fill = "Scenario")

plot(cent_HSMS_nomiroc)

png(filename = "HSMS_fire_comp.png", width = 4800, height = 3600, res = 600)
ggarrange(cent_HSMS_miroc, cent_HSMS_nomiroc, common.legend = T, legend = "bottom")
dev.off()

cent_HSMS <- ggplot(century_mean, aes(x = scenario, y = HSMSmean, fill = scenario)) +
  geom_errorbar(aes(ymax = HSMSmean + HSMSsd, ymin = HSMSmean), position = "dodge") +
  geom_col(position = "dodge") + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  theme_bw() +
  stat_compare_means(method = "anova") +
  labs(x = "Scenario", y = "Moderate and High Severity Fire Area (hectares)", fill = "Scenario")

plot(cent_HSMS)

compare_means(sumHSMS ~ scenario, data = century_total, ref.group = "Scenario1", method = "t.test")

ggbarplot(century_mean, x = "scenario", y = "HSMSmean", add = "mean_se")+
  stat_compare_means() +                                         # Global p-value
  stat_compare_means(ref.group = "Scenario1", label = "p.signif",
                     label.y = 400000)


#####big patch decade fire comparisons##############

decade_total <- sl_data2 %>%
  filter(HSfire > 100) %>%
  group_by(scenario, climate, rep, decade) %>%
  summarise(sumHS = sum(HSfire), sumHSMS = sum(HSMS))

decade_mean <- decade_total %>%
  group_by(scenario, decade) %>%
  summarise(HSmean = mean(sumHS), HSsd = sd(sumHS), HSMSmean = mean(sumHSMS), HSMSsd = sd(sumHSMS))

dec_HS <- ggplot(decade_mean, aes(x = decade, y = HSmean, fill = scenario)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymax = HSmean + HSsd, ymin = HSmean), position = "dodge") +
  theme_bw() +
  labs(x = "Decade", y = "Large Patch High Severity Fire Area (hectares)", fill = "Scenario")

plot(dec_HS)

#####big patch total fire comparison##############

century_total <- sl_data2 %>%
  filter(HSfire > 100) %>%
  group_by(scenario, climate, rep) %>%
  summarise(sumHS = sum(HSfire), sumHSMS = sum(HSMS))

century_mean <- century_total %>%
  group_by(scenario) %>%
  summarise(HSmean = mean(sumHS), HSsd = sd(sumHS), HSMSmean = mean(sumHSMS), HSMSsd = sd(sumHSMS))

cent_HS <- ggplot(century_mean, aes(x = scenario, y = HSmean, fill = scenario)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymax = HSmean + HSsd, ymin = HSmean), position = "dodge") +
  theme_bw() +
  stat_compare_means() +
  labs(x = "Scenario", y = "Large Patch High Severity Fire Area (hectares)", fill = "Scenario")

plot(cent_HS)

#####big patch decade fire comparisons MIROC5 only##############

decade_total <- sl_data2 %>%
  filter(HSfire > 100) %>%
  filter(climate == "MIROC5_85") %>%
  group_by(scenario, climate, rep, decade) %>%
  summarise(sumHS = sum(HSfire), sumHSMS = sum(HSMS))

decade_mean <- decade_total %>%
  group_by(scenario, decade) %>%
  summarise(HSmean = mean(sumHS), HSsd = sd(sumHS), HSMSmean = mean(sumHSMS), HSMSsd = sd(sumHSMS))

dec_HS <- ggplot(decade_mean, aes(x = decade, y = HSmean, fill = scenario)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymax = HSmean + HSsd, ymin = HSmean), position = "dodge") +
  theme_bw() +
  labs(title = "MIROC5 only", x = "Decade", y = "Large Patch High Severity Fire Area (hectares)", fill = "Scenario")

plot(dec_HS)

#####big patch total fire comparison MIROC5 only##############

century_total <- sl_data2 %>%
  filter(HSfire > 100) %>%
  #filter(climate != "MIROC5_85") %>%
  group_by(scenario, climate, rep) %>%
  summarise(sumHS = sum(HSfire), sumHSMS = sum(HSMS))

century_mean <- century_total %>%
  group_by(scenario) %>%
  summarise(HSmean = mean(sumHS), HSsd = sd(sumHS), HSMSmean = mean(sumHSMS), HSMSsd = sd(sumHSMS))

cent_HS <- ggplot(century_mean, aes(x = scenario, y = HSmean, fill = scenario)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymax = HSmean + HSsd, ymin = HSmean), position = "dodge") +
  theme_bw() +
  stat_compare_means() +
  labs(title = "all climates", x = "Scenario", y = "Large Patch High Severity Fire Area (hectares)", fill = "Scenario")

plot(cent_HS)

cent_HSMS <- ggplot(century_mean, aes(x = scenario, y = HSMSmean, fill = scenario)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymax = HSMSmean + HSMSsd, ymin = HSMSmean), position = "dodge") +
  theme_bw() +
  labs(title = "MIROC5 only", x = "Scenario", y = "Large Patch High Severity Fire Area (hectares)", fill = "Scenario")

plot(cent_HSMS)

#####big patch decade fire comparisons MORT5##############

decade_total <- sl_data2 %>%
  filter(HSfire > 100) %>%
  filter(climate != "MIROC5_85") %>%
  group_by(scenario, climate, rep, decade) %>%
  summarise(sumHS = sum(HSfire), sumHSMS = sum(HSMS))

decade_mean <- decade_total %>%
  group_by(scenario, decade) %>%
  summarise(HSmean = mean(sumHS), HSsd = sd(sumHS), HSMSmean = mean(sumHSMS), HSMSsd = sd(sumHSMS))

dec_HS <- ggplot(decade_mean, aes(x = decade, y = HSmean, fill = scenario)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymax = HSmean + HSsd, ymin = HSmean), position = "dodge") +
  theme_bw() +
  labs(title = "MIROC5 only", x = "Decade", y = "Large Patch High Severity Fire Area (hectares)", fill = "Scenario")

plot(dec_HS)

#####big patch total fire comparison MORT5 ##############

century_total <- sl_data2 %>%
  filter(HSfire > 100) %>%
  filter(climate != "MIROC5_85") %>%
  mutate(supp = paste0(scenario, "_", climate)) %>%
  group_by(scenario, climate, supp, rep) %>%
  summarise(sumHS = sum(HSfire), sumHSMS = sum(HSMS))

century_mean <- century_total %>%
  group_by(climate) %>%
  summarise(HSmean = mean(sumHS), HSsd = sd(sumHS), HSMSmean = mean(sumHSMS), HSMSsd = sd(sumHSMS))

cent_HS <- ggplot(century_mean, aes(x = climate, y = HSmean, fill = climate)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymax = HSmean + HSsd, ymin = HSmean), position = "dodge") +
  theme_bw() +
  stat_compare_means() +
  labs(title = "No MIROC5", x = "Scenario", y = "Large Patch High Severity Fire Area (hectares)", fill = "Scenario")

plot(cent_HS)

cent_HSMS <- ggplot(century_mean, aes(x = climate, y = HSMSmean, fill = climate)) +
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymax = HSMSmean + HSMSsd, ymin = HSMSmean), position = "dodge") +
  theme_bw() +
  stat_compare_means() +
  labs(title = "No MIROC5", x = "Scenario", y = "Large Patch High Severity Fire Area (hectares)", fill = "Scenario")

compare_means(sumHS ~ climate, data = century_total, method = "t.test")

plot(cent_HSMS)

#####fire v mgmt analysis #####

sl_data2 <- subset(sl_data2, MeanFWI > 40)

sl_data2$MeanFWI <- round(sl_data2$MeanFWI, 0)
sl_data2$MeanFWI <- as.character(sl_data2$MeanFWI)

sl_data2$MeanFWI <- as.numeric(sl_data2$MeanFWI)

sl_data3 <- sl_data2 %>%
  group_by(scenario, MeanFWI) %>%
  summarize(meanHSF = mean(HSfire))

sl_data4 <- sl_data3

sl_data4$MeanFWI <- as.numeric(sl_data4$MeanFWI)
#sl_data4$MeanFWI <- sl_data4$MeanFWI + 40
sl_plot <- ggplot(sl_data4, aes(x = MeanFWI, y = meanHSF, fill = scenario, color = scenario)) +
  geom_point() +
  geom_smooth(alpha = 0.2) +
  coord_cartesian(ylim = c(0,1500))

plot(sl_plot)

scen_fwi <- ggplot(sl_data2, aes(x = MeanFWI, y = HSfire)) +
  geom_point(aes(color = scenario)) +
  geom_smooth(aes(color = scenario))

plot(scen_fwi)
  

msa <- ggplot(sl_data2, aes(x = MeanEffectiveWindSpeed, y = MeanFWI)) +
  geom_point(aes(color = scenario)) 


plot(msa)  

msa <- ggplot(sl_data2, aes(x = MeanFWI, y = TotalSitesBurned)) +
  geom_point(aes(color = scenario))

plot(msa)  

msa <- ggplot(sl_data2, aes(x = MeanFWI, y = MeanSuppressionEffectiveness)) +
  geom_point(aes(color = scenario))

plot(msa)

msa <- scen_fwi <- ggplot(sl_data2, aes(x = MeanFWI, y = MaximumSpreadArea)) +
  geom_point(aes(color = scenario), position = "jitter")

scen_fwi <- ggplot(sl_data2, aes(x = MeanFWI, y = HSfire)) +
  geom_point(aes(color = scenario)) +
  geom_smooth(aes(color = scenario)) +
  labs(y = "Patch size of high severity fire by event in hectares", x = "Mean Fire Weather Index")
#+ facet_wrap(~decade)


year_sl_biom <- sl_data2 %>%
  group_by(climate, scenario, rep, type, Year) %>%
  summarise(totMg = sum(TotalBiomassMortality))
  
plot(scen_fwi)

###fire_analysis###

sl_data3 <- sl_data2[, c(14,17,29,33)]
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
sl_data3 <- as.data.frame(lapply(sl_data3[,c(1,2,4)], normalize))
sl_data3 <- cbind(sl_data3, sl_data2[,29])
colnames(sl_data3) <- c("MEWindSpeed", "MeanFWI", "HSMS", "Scenario")
train <- sample(nrow(sl_data3), 0.7*nrow(sl_data3), replace = FALSE)
TrainSet <- sl_data3[train,-4]
ValidSet <- sl_data3[-train,-4]
train_labels <- sl_data3[train,4]
valid_labesl <- sl_data3[-train,4]

library(class)
library(caret)

sqrt(277204)

knn.200 <- knn(train = TrainSet, test = ValidSet, cl = train_labels, k = 200)
confusionMatrix(table(knn.200, valid_labesl))

scen_fwi2 <- ggplot(sl_data3, aes(x = MeanFWI, y = HSfire, color = Scenario)) +
  geom_point() +
  labs(y = "Patch size of high severity fire by event in hectares", x = "Mean Fire Weather Index")
plot(scen_fwi2)

####BDA####

bdl_data  <- read.csv("I:/TCSI/Round2_outputs/bdl_data.csv")
bdl_data <- bdl_data %>%
  mutate(Area = DamagedSites * 3.24) %>%
  mutate(Year = Time + 2019)

bdl_data <- bdl_data[,-c(1,11)]

year_bdl_data <- bdl_data %>%
  group_by(Year, climate, scenario, rep) %>%
  summarise(TOT_AREA = sum(Area))

year_bdl_data  <- year_bdl_data %>%
  group_by(scenario) %>%
  summarise(meanAREA = mean(TOT_AREA), sdAREA = sd(TOT_AREA))

insect_total_area <- ggplot(year_bdl_data, aes(x = scenario, y = meanAREA, fill = scenario)) +
  geom_col() +
  geom_errorbar(aes(ymax = meanAREA + sdAREA, ymin = meanAREA)) +
  theme_bw() +
  labs(y = "Total area impacted by insects (hectares)")


plot(insect_total_area)                              

colnames(year_bdl_data) <- c("Climate", "Scenario", "Year", "Area", "SDArea")

year_bd_bm_data <- bdl_data %>%
  group_by(Year, climate, scenario, rep) %>%
  summarise(TOTBIOM = sum(TotalBiomassMortality))

insect_total_biomass <- ggplot(year_bd_bm_data, aes(x = scenario, y = TOTBIOM, fill = scenario)) +
  geom_col() +
  theme_bw() +
  labs(y = "Total biomass impacted by insects (Mg)")

plot(insect_total_biomass)

###bug mortality by scenario

year_bd_bm_data_scenario <- year_bd_bm_data %>%
  group_by(Year, scenario) %>%
  summarise(meanBIOM = mean(TOTBIOM), sdBIOM = sd(TOTBIOM))

insect_meanscen_biomass <- ggplot(year_bd_bm_data_scenario, aes(x = scenario, y = meanBIOM, fill = scenario)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = meanBIOM + sdBIOM, ymin = meanBIOM), position = "dodge") +
  theme_bw() +
  labs(y = "Total biomass impacted by insects (Mg)")

year_bd_bm_data_scenario2 <- year_bd_bm_data %>%
  group_by(Year, scenario, climate, rep) %>%
  summarise(meanBIOM = mean(TOTBIOM), sdBIOM = sd(TOTBIOM))

total_bd_bm_data_scenario <- year_bd_bm_data_scenario2 %>%
  group_by(scenario, climate, rep) %>%
  summarise(TOTBIOM = sum(meanBIOM))

total_bd_bm_data_scenario2 <- total_bd_bm_data_scenario %>%
  group_by(scenario) %>%
  summarise(meanBIOM = mean(TOTBIOM), sdBIOM = sd(TOTBIOM))

tbbds <- ggplot(total_bd_bm_data_scenario2, aes(x = scenario, y = meanBIOM, fill = scenario)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = meanBIOM + sdBIOM, ymin = meanBIOM), position = "dodge") +
  theme_bw() +
  labs(y = "Biomass killed by insects (Mg)", fill = "Scenario") +
  stat_compare_means()

plot(tbbds)

###bug mortality by climate

year_area_data_scenario2 <- year_bd_bm_data %>%
  group_by(Year, scenario, climate, rep) %>%
  summarise(meanBIOM = mean(TOTBIOM), sdBIOM = sd(TOTBIOM))

total_area_data_scenario <- year_area_data_scenario2 %>%
  group_by(scenario, climate, rep) %>%
  summarise(TOTBIOM = sum(meanBIOM))

total_area_data_scenario2 <- total_area_data_scenario %>%
  group_by(climate) %>%
  summarise(meanBIOM = mean(TOTBIOM), sdBIOM = sd(TOTBIOM))

tbareas <- ggplot(total_area_data_scenario2, aes(x = climate, y = meanBIOM, fill = climate)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = meanBIOM + sdBIOM, ymin = meanBIOM), position = "dodge") +
  theme_bw() +
  labs(y = "Biomass killed by insects (Mg)", fill = "Scenario") +
  stat_compare_means()

plot(tbareas)

###bug mortality by facet

year_facet_data_scenario2 <- year_bd_bm_data %>%
  group_by(Year, scenario, climate, rep) %>%
  summarise(meanBIOM = mean(TOTBIOM), sdBIOM = sd(TOTBIOM))

total_facet_data_scenario <- year_facet_data_scenario2 %>%
  group_by(scenario, climate, rep) %>%
  summarise(TOTBIOM = sum(meanBIOM))

total_facet_data_scenario2 <- total_facet_data_scenario %>%
  group_by(scenario, climate) %>%
  summarise(meanBIOM = mean(TOTBIOM), sdBIOM = sd(TOTBIOM))

tbfacet <- ggplot(total_facet_data_scenario2, aes(x = scenario, y = meanBIOM, fill = scenario)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = meanBIOM + sdBIOM, ymin = meanBIOM), position = "dodge") +
  theme_bw() +
#  stat_compare_means() +
  facet_wrap(~climate) +
  labs(y = "Biomass killed by insects (Mg)", fill = "Scenario")

plot(tbfacet)

#year_bd_bm_data  <- year_bd_bm_data %>%
#  group_by(climate, scenario, Year) %>%
#  summarise(meanTB = mean(TOTBIOM))

colnames(year_bd_bm_data) <- c("Year","Climate", "Scenario", "Replicate", "TOTBIOM")

year_bd_bm_data$Replicate <- as.integer(year_bd_bm_data$Replicate) 

write.csv(year_bdl_data, "I:/TCSI/Round2_outputs/bdl_data_2.csv")
write.csv(year_bd_bm_data, "I:/TCSI/Round2_outputs/bd_bm_data_2.csv")

###fixed_cumulative###

year_bdl_data <- read.csv("I:/TCSI/Round2_outputs/bdl_data2_cumulative.csv")

#colnames(year_bdl_data) <- c("Time", "Year", "Climate", "Scenario", "Replicate", "cumAREA")

year_bdl_scen <- year_bdl_data %>%
  group_by(Year, Scenario) %>%
  summarise(meanAREA = mean(cumAREA), sdAREA = sd(cumAREA))

bdas <- ggplot(data = year_bdl_scen, aes(x = Year, y = meanAREA, fill = Scenario, color = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymax = meanAREA + sdAREA, ymin = meanAREA - sdAREA), alpha = 0.5) +
#  geom_smooth() +
  labs(y = "Cumulative Outbreak Area in ha") +
  theme_bw()
  

plot(bdas)

year_bdl_clim <- year_bdl_data %>%
  group_by(Year, Climate) %>%
  summarise(meanAREA = mean(cumAREA), sdAREA = sd(cumAREA))

bdac <- ggplot(data = year_bdl_clim, aes(x = Year, y = meanAREA, fill = Climate, color = Climate)) +
  geom_line() +
  geom_ribbon(aes(ymax = meanAREA + sdAREA, ymin = meanAREA - sdAREA), alpha = 0.5) +
  theme_bw() +
#  geom_smooth() +
  labs(y = "Cumulative Outbreak Area in ha")

plot(bdac)

png(file = "tcsi_figure5_beetlearea.png", width = 4800, height = 3600, res=600)
ggarrange(bdac, bdas)
dev.off()

year_bdm_scen <- year_bdl_data %>%
  group_by(Year, Scenario) %>%
  summarise(meanMORT = mean(cumMORT), sdMORT = sd(cumMORT))

bdas_mg <- ggplot(data = year_bdm_scen, aes(x = Year, y = meanMORT, fill = Scenario, color = Scenario)) +
  geom_line() +
  geom_ribbon(aes(ymax = meanMORT + sdMORT, ymin = meanMORT - sdMORT), alpha = 0.5) +
  #  geom_smooth() +
  labs(y = "Cumulative Insect Mortality (Mg)")

plot(bdas_mg)

year_bdm_clim <- year_bdl_data %>%
  group_by(Year, Climate) %>%
  summarise(meanMORT = mean(cumMORT), sdMORT = sd(cumMORT))

bdac <- ggplot(data = year_bdm_clim, aes(x = Year, y = meanMORT, fill = Climate, color = Climate)) +
  geom_line() +
  geom_ribbon(aes(ymax = meanMORT + sdMORT, ymin = meanMORT - sdMORT), alpha = 0.5) +
  #  geom_smooth() +
  labs(y = "Cumulative Outbreak Area in ha")

plot(bdac)



#####HARVEST#####

h_data  <- read.csv("I:/TCSI/Round2_outputs/h_data.csv")
h_data <- h_data %>%
  mutate(Area = HarvestedSites * 3.24) %>%
  mutate(Acres = Area * 2.47) %>%
  mutate(Year = Time + 2019)

year_h_data <- h_data %>%
  group_by(climate, scenario, rep, Year) %>%
  summarise(totAcres = sum(Acres), totMg = sum(TotalBiomassHarvested))

colnames(year_h_data) <- c("Climate", "Scenario", "Replicate", "Year", "totAcres", "TOTBIOM")

year_h_data$Replicate <- as.integer(year_h_data$Replicate)

year_scen_data <- year_h_data %>%
  group_by(Scenario, Year) %>%
  summarise(meanAcres = mean(totAcres), sdAcres = sd(totAcres), meanMg = mean(TOTBIOM), sdMg = sd(TOTBIOM))

ysd <- ggplot(year_scen_data, aes(x = Year, y = meanAcres, color = Scenario, fill = Scenario)) +
  geom_line() + 
  geom_ribbon(aes(ymax = meanAcres + sdAcres, ymin = meanAcres - sdAcres)) +
  scale_color_discrete(guide = F) +
  labs(y = "Acres per year", fill = "Scenario") +
  theme_bw()

plot(ysd)

png(file = "tcsi_figure2_treatmentarea.png", width = 4800, height = 3600, res=600)
plot(ysd)
dev.off()


ysmg <- ggplot(year_scen_data, aes(x = Year, y = meanMg, color = Scenario, fill = Scenario)) +
  geom_line() + 
  geom_ribbon(aes(ymax = meanMg + sdMg, ymin = meanMg - sdMg), alpha = 0.5) +
  labs(y = "Biomass harvest", color = "Scenario")

plot(ysmg)

#####biomass damage by source#####
colnames(year_sl_biom) <- c("Climate", "Scenario", "Replicate", "Type", "Year", "TOTBIOM")
year_sl_biom
colnames(year_bd_bm_data) <- c("Year", "Climate", "Scenario", "Replicate", "TOTBIOM")
year_bd_bm_data
year_h_data

year_total <- inner_join(year_sl_biom, year_bd_bm_data, by = c("Climate", "Scenario", "Replicate", "Year"))
year_total_3 <- inner_join(year_total, year_h_data, by = c("Climate", "Scenario", "Replicate", "Year"))

colnames(year_total_3) <- c("Climate", "Scenario", "Replicate","Type", "Year", "BIOM_fire", "BIOM_insect", "AREA_insect", "BIOM_harvest")

year_total_3 <- year_total_3[,-8]

write.csv(year_total_3, "total_biom_mort.csv")
##passed out to lump rx into management
year_total_3 <- read.csv("total_biom_mort_v2.csv")

year_total_long <- year_total_3 %>%
  pivot_longer(cols = Fire:Management, names_to = "Biomass_mortality")

write.csv(year_total_long, "year_biomass_total_mort2.csv")


##all climates
year_total_climate <- year_total_long %>%
  group_by(Climate, Year, Biomass_mortality) %>%
  filter(Biomass_mortality != "Rx") %>%
  filter(Biomass_mortality != "Harvest") %>%
  summarise(meanMg = mean(value), sdMg = sd(value))

year_total_scenario <- year_total_long %>%
  group_by(Scenario, Year, Biomass_mortality) %>%
  filter(Biomass_mortality != "Rx") %>%
  filter(Biomass_mortality != "Harvest") %>%
  summarise(meanMg = mean(value), sdMg = sd(value))


ytc <- ggplot(year_total_climate, aes( x = Year, y = meanMg, fill = Biomass_mortality, color = Biomass_mortality)) +
  theme_bw() +
  geom_line() +
  geom_ribbon(aes(ymax = meanMg + sdMg, ymin = meanMg - sdMg)) +
  facet_wrap(~Climate, ncol = 5) +
  scale_color_discrete(guide = F) +
  theme(legend.position="bottom") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(y = "Mortality in Mg", fill = "Mortality type")

plot(ytc)

yts <- ggplot(year_total_scenario, aes( x = Year, y = meanMg, fill = Biomass_mortality, color = Biomass_mortality)) +
  theme_bw() +
  geom_line() +
  geom_ribbon(aes(ymax = meanMg + sdMg, ymin = meanMg - sdMg)) +
#  geom_smooth() +
  facet_wrap(~Scenario) +
  scale_color_discrete(guide = F) +
  coord_cartesian(ylim = c(0,250000000)) +
  theme(legend.position="bottom") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(y = "", fill = "Mortality type")

plot(yts)

combine_plot <- ggarrange(ytc, yts, common.legend = T, legend = "bottom")

png(file = "tcsi_figure4.png", width = 4800, height = 3600, res=600)
plot(combine_plot)
dev.off()

#####CUMULATIVE FIRE MORT####

ybtl <- read.csv("year_biomass_total_mort_v2_cumulative.csv")
ybtl$Climate <- as.factor(ybtl$Climate)
ybtl$Scenario <- as.factor(ybtl$Scenario)
ybtl$Biomass_mortality <- as.factor(ybtl$Biomass_mortality)
ybtl$value <- as.numeric(ybtl$value)
ybtl$Cumulative <- as.numeric(ybtl$Cumulative)
ybtl <- subset(ybtl, Biomass_mortality != "Rx")
ybtl <- subset(ybtl, Biomass_mortality != "Harvest")

##byscen by clim
year_total_climate <- ybtl %>%
  group_by(Climate, Year, Biomass_mortality) %>%
  summarise(meanMg = mean(Cumulative), sdMg = sd(Cumulative))

year_total_scenario <- ybtl %>%
  group_by(Scenario, Year, Biomass_mortality) %>%
  summarise(meanMg = mean(Cumulative), sdMg = sd(Cumulative))

ytc <- ggplot(year_total_climate, aes( x = Year, y = meanMg, fill = Biomass_mortality, color = Biomass_mortality)) +
  theme_bw() +
  geom_line() +
  geom_ribbon(aes(ymax = meanMg + sdMg, ymin = meanMg - sdMg), alpha = 0.7) +
  facet_wrap(~Climate, ncol = 5) +
  scale_color_discrete(guide = F) +
  theme(legend.position="right") +
  scale_y_continuous(labels = scales::comma ) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(y = "Mortality (Metric tons)", fill = "Mortality type")

plot(ytc)

yts <- ggplot(year_total_scenario, aes( x = Year, y = meanMg, fill = Biomass_mortality, color = Biomass_mortality)) +
  theme_bw() +
  geom_line() +
  geom_ribbon(aes(ymax = meanMg + sdMg, ymin = meanMg - sdMg), alpha = 0.6) +
  #  geom_smooth() +
  facet_wrap(~Scenario) +
  scale_color_discrete(guide = F) +
#  coord_cartesian(ylim = c(0,250000000)) +
  theme(legend.position="bottom") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(y = "", fill = "Mortality type")

plot(yts)

combine_plot <- ggarrange(ytc, yts, common.legend = T, legend = "bottom")

png(file = "tcsi_figure5A_v2.png", width = 4800, height = 3600, res=600)
plot(ytc)
dev.off()

##subset out miroc

ybtl_miroc <- subset(ybtl, Climate == "MIROC5_85")
#ybtl_miroc <- subset(ybtl_miroc, Scenario != "Scenario2" & Replicate != 4)

ybtl_miroc <- ybtl_miroc %>%
  group_by(Scenario, Year, Biomass_mortality) %>%
  summarise(meanCum = mean(Cumulative), sdCum = sd(Cumulative))

ybtl_nomiroc <- subset(ybtl, Climate != "MIROC5_85")

ybtl_nomiroc <- ybtl_nomiroc %>%
  group_by(Scenario, Year, Biomass_mortality) %>%
  summarise(meanCum = mean(Cumulative), sdCum = sd(Cumulative))


ybtls <- ggplot(ybtl_miroc, aes( x = Year, y = meanCum, fill = Biomass_mortality, color = Biomass_mortality)) +
  theme_bw() +
  geom_line() +
  geom_ribbon(aes(ymax = meanCum + sdCum, ymin = meanCum - sdCum)) +
  facet_wrap(~Scenario) +
  scale_color_discrete(guide = F) +
  scale_y_continuous(labels = scales::comma ) +
  #coord_cartesian(ylim = c(0,250000000)) +
  theme(legend.position="right") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(y = "Mortality (Metric tons)", fill = "Mortality type", title = "Hotter and driest projection only")

plot(ybtls)

ybtls_nm <- ggplot(ybtl_nomiroc, aes( x = Year, y = meanCum, fill = Biomass_mortality, color = Biomass_mortality)) +
  theme_bw() +
  geom_line() +
  geom_ribbon(aes(ymax = meanCum + sdCum, ymin = meanCum - sdCum)) +
  facet_wrap(~Scenario) +
  scale_color_discrete(guide = F) +
  #coord_cartesian(ylim = c(0,250000000)) +
  theme(legend.position="bottom") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(y = "Mortality (Mg)", fill = "Mortality type", title = "Other climate projections")

plot(ybtls_nm)

png(file = "tcsi_figure5B_v2.png", width = 4800, height = 3600, res=600)
plot(fmbc)
dev.off()

fmbc <- ggarrange(ybtls, ybtls_nm, legend = "bottom", common.legend = T)

ybamc <- read.csv("I:/TCSI/Round2_outputs/year_biomass_all_mort_v2_cumulative.csv")
ybamc_miroc <- subset(ybamc, Climate == "MIROC5_85")
ybamc_nomiroc <- subset(ybamc, Climate != "MIROC5_85")

btmpd <- read.csv("biomass_total_mort_perc_diff.csv")
btmpd$Scenario <- as.factor(btmpd$Scenario)
btmpd$Year <- as.factor(btmpd$Year)
btmpd <- btmpd %>%
  filter(Year != 2020)
btmpd_gg <- ggplot(btmpd, aes(x = Year, y = Percent.difference, fill = Scenario)) +
  geom_col(position = "dodge") +
  facet_wrap(~Climate) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(y = "Percent difference in cumulative mortality from Scenario 1")

plot(btmpd_gg)

btmpd$DD <- btmpd$Percent.difference - 1

btmpd_gg <- ggplot(btmpd, aes(x = Year, y = -(DD), fill = Scenario)) +
  geom_col(position = "dodge") +
  facet_wrap(~Climate2) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(y = "Percent savings in cumulative mortality from Scenario 1")

plot(btmpd_gg)


####species data#########

spl_data <- read.csv("I:/TCSI/Round2_outputs/species_biomass_annual2.csv")

spp_b <- spl_data %>%
  pivot_longer(cols = AbieConc:Shrubs, names_to = "Species") 

spp_c <- spp_b %>%
  group_by(Climate, Year, Species) %>%
  summarise(meanBIOM = mean(value), sdBIOM = sd(value))

spp_s <- spp_b %>%
  group_by(Scenario, Year, Species) %>%
  summarise(meanBIOM = mean(value), sdBIOM = sd(value))

colnames(spp_c) <- c("Climate", "Year", "Species", "Biomass")
colnames(spp_s) <- c("Scenario", "Year", "Species", "Biomass")

spp <- ggplot(spp_c, aes(x = Year, y = Biomass, color = Species, fill = Species)) +
  geom_col() +
  theme_bw() +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors, guide = F) +
  facet_wrap(~Climate, ncol = 5) +
  theme(axis.text.x = element_text(angle = 90), text= element_text(size = 12), legend.position = "bottom") +
  labs(y = "Biomass g/m2", x = "Year", fill = "Species")

plot(spp)  

spps <- ggplot(spp_s, aes(x = Year, y = Biomass, color = Species, fill = Species)) +
  geom_col() +
  theme_bw() +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors, guide = F) +
  facet_wrap(~Scenario, ncol = 6) +
  theme(axis.text.x = element_text(angle = 90), text= element_text(size = 12), legend.position = "bottom") +
  labs(y = "", x = "Year", fill = "Species")

plot(spps)  

png(file = "spp_biomass.png", width = 6400, height = 3600, res=600)
ggarrange(spp,spps, common.legend = T, legend = "bottom")
dev.off()
