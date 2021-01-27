##required packages
library(tidyverse)
library(dplyr)
#library(purrr)
library(tidyr)
library(raster)
#library(fuzzyjoin)
#library(data.table)
library(rgdal)

##Function to roundup ages to nearest 10
roundUp <- function(x,to=10){
  to*(x%/%to + as.logical(x%%to))
}

roundUp20 <- function(x,to=20){
  to*(x%/%to + as.logical(x%%to))
}

sierra_sp_long <- read.csv("H:/EPIC/Landis_species_codes.csv")

sierras_sp_only <- subset(sierra_sp_long, Location4 == "TCSI")

##FIA data source
dirFIA <- paste("C:/Users/cjmaxwe3/Downloads/FIA/") 

##lookup table linking FIA species codes and names
spec.codes <- read_csv("C:/Users/cjmaxwe3/Downloads/FIA/REF_SPECIES/REF_SPECIES.csv") %>%
  dplyr::select(SPCD, COMMON_NAME, SPECIES_SYMBOL, GENUS, SPECIES)
shrub.codes <- read_csv(paste0(dirFIA,"CA/landis_shrub_codes.csv")) ##for binning subplots into shrub groups

####CA data############
ca_plp <- read_csv(paste(dirFIA,"CA/CA_PLOTSNAP.csv",sep="")) %>%
  dplyr::select(INVYR,CN,EXPVOL,EXPCURR) %>%
  filter(!is.na(EXPVOL)) %>%
  dplyr::rename(PLT_CN=CN) %>%
  distinct(PLT_CN,.keep_all = T)

ca_plots <- read_csv(paste(dirFIA,"CA/CA_Plot.csv",sep="")) %>%
  dplyr::select(CN,ECOSUBCD,LAT,LON,ELEV) %>%
  dplyr::rename(PLT_CN=CN) 

ca_tb <- read_csv(paste(dirFIA,"CA/CA_TREE_REGIONAL_BIOMASS.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(TRE_CN,REGIONAL_DRYBIOT)

ca_cond <- read_csv(paste(dirFIA,"CA/CA_COND.csv",sep="")) %>%
  dplyr::select(CN, PLT_CN, PLOT, LIVE_CANOPY_CVR_PCT) %>%
  right_join(ca_plots,by="PLT_CN")

ca_trees <- read_csv(paste(dirFIA,"CA/CA_TREE.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(CN, PLT_CN, PLOT, TREE, BHAGE, TOTAGE, DIA, CARBON_AG, SPCD, TPA_UNADJ) %>%
  dplyr::rename(TRE_CN=CN) %>%
  right_join(ca_cond,by="PLT_CN") %>%
  #  left_join(ca_tb,by="TRE_CN") %>%
  left_join(spec.codes,by=c("SPCD")) %>%
  left_join(ca_plp,by="PLT_CN") %>%
  #  filter(!is.na(SpecCode)) %>%
  filter(!is.na(TRE_CN)) %>%
  filter(!is.na(DIA)) %>%
  filter(INVYR > 2005) %>%
  mutate(SpecCode2 = as.factor(SPECIES_SYMBOL)) %>%
  mutate(AG_biomass_gm2 = CARBON_AG * TPA_UNADJ* 2 * 0.1121) %>%
  mutate(TOTAGE2 = BHAGE + 10)

ca_shrubs <- read_csv(paste(dirFIA,"CA/CA_P2VEG_SUBPLOT_SPP.csv",sep="")) %>%
  dplyr::select(CN,PLT_CN,SUBP,VEG_SPCD,GROWTH_HABIT_CD,LAYER,COVER_PCT) %>%
  filter(GROWTH_HABIT_CD == "SH") %>%
  left_join(shrub.codes, by = "VEG_SPCD") %>%
  filter(!is.na(Type)) %>%
  mutate(growth_layer = case_when(LAYER == 1 ~ 0.25,
                                  LAYER == 2 ~ 0.5,
                                  LAYER == 3 ~ 0.75,
                                  LAYER == 4 ~ 1)) %>%
  mutate(biomass = (1.372014 * COVER_PCT + 2.576618)/10) %>%
  mutate(age = roundUp((COVER_PCT/100 * 40))) %>%
  distinct(Type, age, biomass, .keep_all = T) %>%
  right_join(ca_plots,by="PLT_CN") %>%
  dplyr::select(PLT_CN, LAT, LON, Type, biomass, age) %>%
  filter(!is.na(Type)) %>%
  group_by(PLT_CN, LAT, LON, Type, age) %>%
  summarise(biomass = sum(biomass))

####OR data##########
or_plp <- read_csv(paste(dirFIA,"OR/OR_PLOTSNAP.csv",sep="")) %>%
  dplyr::select(INVYR,CN,EXPVOL,EXPCURR) %>%
  filter(!is.na(EXPVOL)) %>%
  dplyr::rename(PLT_CN=CN) %>%
  distinct(PLT_CN,.keep_all = T)

or_plots <- read_csv(paste(dirFIA,"OR/OR_Plot.csv",sep="")) %>%
  dplyr::select(CN,ECOSUBCD,LAT,LON,ELEV) %>%
  dplyr::rename(PLT_CN=CN) 

or_tb <- read_csv(paste(dirFIA,"OR/OR_TREE_REGIONAL_BIOMASS.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(TRE_CN,REGIONAL_DRYBIOT)

or_cond <- read_csv(paste(dirFIA,"OR/OR_COND.csv",sep="")) %>%
  dplyr::select(CN, PLT_CN, PLOT, LIVE_CANOPY_CVR_PCT) %>%
  right_join(or_plots,by="PLT_CN")

or_trees <- read_csv(paste(dirFIA,"OR/OR_TREE.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(CN, PLT_CN, PLOT, TREE, BHAGE, TOTAGE, DIA, CARBON_AG, SPCD, TPA_UNADJ) %>%
  dplyr::rename(TRE_CN=CN) %>%
  right_join(or_cond,by="PLT_CN") %>%
  #  left_join(ca_tb,by="TRE_CN") %>%
  left_join(spec.codes,by=c("SPCD")) %>%
  left_join(or_plp,by="PLT_CN") %>%
  #  filter(!is.na(SpecCode)) %>%
  filter(!is.na(TRE_CN)) %>%
  filter(!is.na(DIA)) %>%
  filter(INVYR > 2005) %>%
  mutate(SpecCode2 = as.factor(SPECIES_SYMBOL)) %>%
  mutate(AG_biomass_gm2 = CARBON_AG * TPA_UNADJ *2 * 0.1121) %>%
  mutate(TOTAGE2 = BHAGE + 10)

###LINK up state tree tables#####  
trees.all <- rbind(ca_trees, or_trees)
trees.all <- merge(trees.all, sierras_sp_only, by.x = "SPCD" , by.y = "SPCD_FIA")

trees.all$SPECIES_SYMBOL <- as.factor(trees.all$SPECIES_SYMBOL)
uni_species <- unique(trees.all$SPECIES_SYMBOL)

all_data <- NULL

for(i in uni_species){
  curr_uni <- subset(trees.all, i == SPECIES_SYMBOL)
  curr_uni_not_missing <- subset(curr_uni, !is.na(TOTAGE2))
  if(nrow(curr_uni_not_missing) > 0){
    curr_uni_missing <- subset(curr_uni, is.na(TOTAGE2))
    ms <- lm(log(TOTAGE2) ~ log(DIA) + log(DIA)^2 + log(DIA)^3, data = curr_uni, na.action = na.exclude) ##follow angela's example of using the cubic of DIA to predict age
    summary(ms)
    actual_uni <- curr_uni$TOTAGE2[!is.na(curr_uni$TOTAGE2)]
    plot(actual_uni, exp(ms$fitted.values))
    uni_trees_predict <- as_tibble(exp(predict.lm(ms, curr_uni_missing)))
    uni_trees_fixed <- cbind(curr_uni_missing, uni_trees_predict)
    uni_trees_fixed <- uni_trees_fixed[-27]
    names(uni_trees_fixed)[names(uni_trees_fixed) == 'value'] <- 'TOTAGE2'
    uni_trees_total <- rbind(uni_trees_fixed, curr_uni_not_missing)
    all_data <- rbind(all_data, uni_trees_total)
  }
}

all_data$TOTAGE3 <- roundUp(all_data$TOTAGE2)

uni_species <- unique(all_data$SPECIES_SYMBOL)

conv_data <- NULL
full_list <- NULL

for(i in uni_species){
  curr_uni <- subset(trees.all, i == SPECIES_SYMBOL)
  md <- lm(log(DIA) ~ log(TOTAGE2) , data = curr_uni, na.action = na.exclude)   ##reverse the prediction going from dia back to age
  intercept <- md$coefficients[1]
  totage2_coeff <- md$coefficients[2]
  conv_data <- cbind(i, intercept, totage2_coeff)
  full_list <- rbind(conv_data, full_list)
}

full_list_int <- as_data_frame(full_list)
colnames(full_list_int) <- c("VEG_SPCD","INT", "COEFF")
full_list_int2 <- left_join(full_list_int, sierras_sp_only, by = "VEG_SPCD")
full_list_int3 <- full_list_int2[,c(4,2,3)]
full_list_int3$COEFF <- as.numeric(full_list_int3$COEFF)
full_list_int3$INT <- as.numeric(full_list_int3$INT)

all_data2_1 <- all_data %>%
  group_by(PLT_CN) %>%
  summarise(plot_bapa = sum(0.005454*(DIA^2)*TPA_UNADJ), plot_tpa = sum(TPA_UNADJ), plot_abg = sum(AG_biomass_gm2 * TPA_UNADJ), plot_qmd = sqrt((plot_bapa/plot_tpa)/0.005454), CC = mean(LIVE_CANOPY_CVR_PCT))  ##original plan was to associate LANDIS cohort records to their closest FIA record

all_data3 <- all_data %>%
  group_by(PLT_CN, INVYR, SPECIES_SYMBOL, TOTAGE3) %>%
  summarise(bapa = sum(0.005454*(DIA^2)*TPA_UNADJ), tpa = sum(TPA_UNADJ), abg = sum(AG_biomass_gm2 * TPA_UNADJ), qmd = sqrt((bapa/tpa)/0.005454))

all_data4 <- left_join(all_data3, all_data2_1, by = "PLT_CN")

all_data4$perc_abg <- (all_data4$abg / all_data4$plot_abg)
all_data4$perc_cc <- all_data4$perc_abg * all_data4$CC

species_LUT <- sierras_sp_only[,1:2]

colnames(species_LUT) <- c("SpeciesName", "SPCD")

i <- uni_species[1]

all_sp_z <- NULL

for(i in uni_species){
  uniz <- subset(all_data4, SPECIES_SYMBOL == i)
  uniz2 <- uniz %>%
    mutate(z_abg = (abg - mean(abg))/sd(abg), z_age = (TOTAGE3 - mean(TOTAGE3))/sd(TOTAGE3))
  all_sp_z <- rbind(uniz2, all_sp_z)
}

all_sp_z_cc <- na.omit(all_sp_z)

all_sp_z_cc$mean_z <- sqrt((all_sp_z_cc$z_abg^2) + (all_sp_z_cc$z_age^2))

colnames(all_sp_z_cc) <- c("PLT_CN", "INVYR", "SPCD", "AGE", "BAPA", "TPA" , "ABG", "QMD", "PLOT_BAPA", "PLOT_TPA", "PLOT_ABG", "PLOT_QMD", "CC", "PERC_ABG", "PERC_CC", "z_abg", "z_age")

rm(all_data, all_data2_1, all_data3, all_data4, ca_cond, ca_plots, ca_plp, ca_shrubs, ca_tb, ca_trees, conv_data, curr_uni, curr_uni_missing, curr_uni_not_missing, md, ms, or_cond, or_plots, or_plp, or_tb, or_trees, shrub.codes, sierra_sp_long, trees.all, uni_trees_fixed, uni_trees_predict, uni_trees_total, uniz, uniz2)

#######Moving onto LANDIS outputs###############

clim_dir <- "MIROC5_85"
scen_dir <- c("SSP2_Scenario1_3", "SSP2_Scenario1_4", "SSP2_Scenario1_5")
timesteps <- seq(from = 5, to = 80, by = 5)
wd <- "I:/TCSI/Round2_outputs/"

l <- clim_dir[1]
m <- scen_dir[1]
n <- timesteps[1]

for(l in clim_dir){
  for(m in scen_dir){
    for(n in timesteps){
        
        LANDIS_year0 <- read_csv(paste0(wd,l,"/",m,"/community-input-file-",n,".csv"))
        
        LANDIS_year0_plot <- LANDIS_year0 %>%  ##calculate plot biomass
          group_by(MapCode) %>%
          summarise(plot_biom = sum(CohortBiomass))
        
        LANDIS_year0$CohortAge <- roundUp20(LANDIS_year0$CohortAge)  ##bin the cohorts so that the closest age cohorts are lumped together--could do this better?  Since it's rounding up, it is exaggerating the DIA lookup.  I chose rounding up to avoid rounding down to zero but this could be handled by an IF statement
        
        LANDIS_year0_collapse <- LANDIS_year0 %>%
          group_by(MapCode, SpeciesName, CohortAge) %>%
          summarise(CohortBiomass = sum(CohortBiomass))
        
        LANDIS_year0_tot2 <- left_join(LANDIS_year0_collapse, LANDIS_year0_plot, by = "MapCode")
        LANDIS_year0_tot2$perc_biom <- LANDIS_year0_tot2$CohortBiomass / LANDIS_year0_tot2$plot_biom  ##calculate cohort's contribution to total biomass
        
        LANDIS_year0_tot2 <- left_join(LANDIS_year0_tot2, full_list_int3, by = c("SpeciesName" = "SpecCode"))
        LANDIS_year0_tot2$DIA <- exp(LANDIS_year0_tot2$INT + (log(LANDIS_year0_tot2$CohortAge) * LANDIS_year0_tot2$COEFF))  ##calc diameter from age
        
        LANDIS_year0_tot2$SpeciesName <- as.factor(LANDIS_year0_tot2$SpeciesName)
        
        landis_uni_sp <- unique(LANDIS_year0_tot2$SpeciesName)
        
        all_sp_z <- NULL
        
        for(i in landis_uni_sp){
          uniz <- subset(LANDIS_year0_tot2, SpeciesName == i)
          uniz2 <- uniz %>%
            filter(CohortBiomass > 100) %>%
            mutate(z_abg = (CohortBiomass - mean(CohortBiomass))/sd(CohortBiomass), z_age = (CohortAge - mean(CohortAge))/sd(CohortAge))  ##calculate z-scores for age and biomass
          all_sp_z <- rbind(uniz2, all_sp_z)
        }
        
        all_sp_z2 <- left_join(all_sp_z, species_LUT, by = "SpeciesName")
        
        all_sp_z2$mean_z <- sqrt((all_sp_z2$z_abg^2) + (all_sp_z2$z_age^2))
        
        
        #mapcodes <- unique(all_sp_z2$MapCode)  This code set was intended to join the LANDIS cohort to its closest FIA record.  Minimizing difference of the RMS of the z-score did provide a decent matching schema but couldn't handle the number of cohorts
        #i <- mapcodes[1]
        #king_all_table <- NULL
        #for(i in mapcodes){
        #  all_sp_z3 <- subset(all_sp_z2, MapCode == i)
        #  all_table <- all_sp_z3 %>%
        #    left_join(all_sp_z_cc, by = "SPCD") %>%
        #    group_by(SPCD) %>%
        #    slice(which.min(sqrt((z_abg.x - z_abg.y)^2 + (z_age.x - z_age.y)^2)))
        #  king_all_table <- rbind(all_table, king_all_table)
        #}
        
        all_sp_short2 <- all_sp_z2 %>%  ##choose cohort with the highest percentage contribution to plot biomass for each species present
          group_by(MapCode, SPCD) %>%
          slice(which.max(perc_biom))
        
        stand_perc_biom <- all_sp_short2 %>%  ##lump DIA into cwhr size classes
          mutate(CWHR_size = case_when(
            DIA < 1 ~ 1,
            DIA >= 1 & DIA < 6 ~ 2,
            DIA >= 6 & DIA < 11~ 3,
            DIA >= 11 & DIA < 24 ~ 4,
            DIA >= 24 ~ 5))
        
        stand_cwhr5 <- stand_perc_biom %>%  ##identify cwhr classes 5 that have a 3 or 4 component
          group_by(MapCode) %>%
          slice(which(CWHR_size == 5))
        
        stand_cwhr34 <- stand_perc_biom %>%
          group_by(MapCode) %>%
          slice(which(CWHR_size == 3 | CWHR_size == 4))
        
        stand_cwhr6 <- inner_join(stand_cwhr5, stand_cwhr34, by = "MapCode") %>%
          group_by(MapCode) %>%
          distinct(MapCode)
        
        stand_cwhr6$CWHR_size <- 8  ##8 is actually a 6...
        
        all_sp_short_maxage2 <- all_sp_short2 %>%  ##choose the oldest species with the highest perc contribution to plot biomass
          group_by(MapCode)%>%
          slice(which.max(CohortAge))
        
        stand_maxage2 <- all_sp_short_maxage2 %>%  ##recalculate cwhr size classes with other dia thresholds
          mutate(CWHR_size = case_when(
            DIA < 1 ~ 1,
            DIA >= 1 & DIA < 6 ~ 2,
            DIA >= 6 & DIA < 11~ 3,
            DIA >= 11 & DIA < 24 ~ 4,
            DIA >= 24 & DIA < 36 ~ 5,
            DIA >= 36 & DIA < 48 ~ 6,
            DIA >= 48 ~ 7))
        
        cwhr_seral2 <- stand_maxage2 %>%  ##not sure why i do this here
          mutate(CWHR_seral = case_when(
            CWHR_size >0 & CWHR_size < 4 ~ 1,
            CWHR_size == 4 ~ 2,
            CWHR_size > 4 ~ 3
          ))
        
        stand_maxage3 <- left_join(stand_maxage2, stand_cwhr6, by = "MapCode")  ##fold back in those stands that are the size class 5 with 3 or 4 understory
        stand_maxage3$CWHR_size.y[is.na(stand_maxage3$CWHR_size.y)] <- 0
        stand_maxage4 <-stand_maxage3 %>%
          mutate(CWHR_size = max(CWHR_size.x, CWHR_size.y))
        
        cwhr_seral2 <- stand_maxage4 %>%
          mutate(CWHR_seral = case_when(
            CWHR_size >0 & CWHR_size < 4 ~ 1,
            CWHR_size == 4 ~ 2,
            CWHR_size > 4 ~ 3
          ))
        
        #####data frame to map##########
        cic0 <- readGDAL(paste0(wd,l,"/",m,"/output-community-",n,".img"))
        
        map.dat <- as_tibble(cic0@data) %>% 
          mutate(MapCode = band1) %>% 
          left_join(cwhr_seral2, by="MapCode")
        
        cseralm <- cic0
        csizem <- cic0
        
        cseralm@data <- data.frame(map.dat$CWHR_seral)
        csizem@data <- data.frame(map.dat$CWHR_size)
        
        csm <- raster(cseralm)
        cssm <- raster(csizem)
        
        plot(csm)
        freq(csm)
        plot(cssm)
        freq(cssm)
        
        cseralnameout <- paste0(wd, "CWHR_maps/seral_",l,"_",m,"_",n,".tif")
        csizenameout <- paste0(wd, "CWHR_maps/size_",l,"_",m,"_",n,".tif")
        
        writeRaster(csm, cseralnameout)
        writeRaster(cssm, csizenameout)
    }
  }
}
