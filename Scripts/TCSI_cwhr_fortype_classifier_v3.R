####Call necessary packages###
library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
library(spatialEco)
library(RColorBrewer)
library(stringr)
library(cowplot)
######Set working directory####

mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 7))


######Set working directory####
p <- "I:/TCSI/Round2_outputs/"

setwd(p)
clim_dir <- c("CNRM85", "GFDL85", "HADGEM85", "MIROC5_85")
scen_dir <- c("Scenario1_1", "Scenario2_1", "Scenario3_1", "Scenario4_1", "Scenario5_1", "Scenario6_1")
timesteps <- seq(from = 0, to = 80, by = 20)

TOT_TCSI <- raster("I:/TCSI/Round2_outputs/CANESM85/SSP2_Scenario1_1/rx_equal1.tif")
plot(TOT_TCSI)

######################PROJECT RASTER SPATIALLY#########################################
rasterNoProj <- raster(nrow = 800, ncol = 650)
xMin = -1810455
yMin = -499545
res <- 180
xMax <- xMin + (rasterNoProj@ncols * res)
yMax <- yMin + (rasterNoProj@nrows * res)
rasExt <- extent(xMin,xMax,yMin,yMax)
rasterNoProj@extent <- rasExt
crs(rasterNoProj) <- "EPSG:2163"
rasterNoProj@crs
rasterNoProj@extent
rasterNoProj
#####################################################################################



j <- clim_dir[2]
k <- lf[1]
i <- timesteps[2]

all_data <- NULL

lf_existing <- list.files(pattern = ".*.tif", path = "I:/TCSI/Round2_outputs/CWHR_fortype/")

for(j in clim_dir){
  
  path <- paste0(p,j,"/")
  lf <- list.dirs(path = path, full.names = F, recursive = F)
  
  for(k in lf){
    
    path2 <- paste0(p,j,"/",k,"/biomass/")   
    setwd(path2)
    print(path2)
    
    totbiom80 <- paste0("TotalBiomass-80.img")
    tb_lf <- list.files()
    tb_skip <- grep(totbiom80, tb_lf)
    
    if(length(tb_skip != 0)){
    
    for(i in timesteps){

      rnameout <- paste0("I:/TCSI/Round2_outputs/CWHR_fortype/fortype_", j ,"_",k,"_",i,".tif")
      existing_name <- paste0("fortype_", j ,"_",k,"_",i,".tif")
      
      skipval <- grep(existing_name, lf_existing)
      
      if( length(skipval) == 0){

      
          rac <- raster(paste0("AbieConc-",i,".img"))
          ram <- raster(paste0("AbieMagn-",i,".img"))
          rcd <- raster(paste0("CaloDecu-",i,".img"))
          rjo <- raster(paste0("JuniOcci-",i,".img"))
          rpa <- raster(paste0("PinuAlbi-",i,".img"))
          rpc <- raster(paste0("PinuCont-",i,".img"))
          rpj <- raster(paste0("PinuJeff-",i,".img"))
          rpl <- raster(paste0("PinuLamb-",i,".img"))
          rpm <- raster(paste0("PinuMont-",i,".img"))
          rpp <- raster(paste0("PinuPond-",i,".img"))
          rps <- raster(paste0("PinuSabi-",i,".img"))
          rpw <- raster(paste0("PinuWash-",i,".img"))
          rpz <- raster(paste0("PseuMenz-",i,".img"))
          rtb <- raster(paste0("TaxuBrev-",i,".img"))
          rtc <- raster(paste0("TorrCali-",i,".img"))
          rtm <- raster(paste0("TsugMert-",i,".img"))
          
          racer <- raster(paste0("AcerMacr-",i,".img"))
          ralnu <- raster(paste0("AlnuRhom-",i,".img"))
          rarbu <- raster(paste0("ArbuMenz-",i,".img"))
          rlith <- raster(paste0("LithDens-",i,".img"))
          rumbe <- raster(paste0("UmbeCali-",i,".img"))
          rpopu <- raster(paste0("PopuTrem-",i,".img"))
          rquec <- raster(paste0("QuerChry-",i,".img"))
          rquek <- raster(paste0("QuerKell-",i,".img"))
          rquew <- raster(paste0("QuerWisl-",i,".img"))
          
          rfxr <- raster(paste0("FX_R_SEED-",i,".img"))
          rnfr <- raster(paste0("NOFX_R_SEED-",i,".img"))
          rnnr <- raster(paste0("NOFX_NOR_SEED-",i,".img"))
          
          total.biomass <- raster(paste0("TotalBiomass-",i,".img"))
          
          biomass.SMC = rac + rpc + rpj + rpl + rpp + rpw + rps + rcd + rpz + rtb + rtc
          biomass.SCN = ram + rpa + rpm + rtm
          
          biom.coni <- rac + ram + rcd + rpa + rpc + rpj + rpl + rpm + rpp + rps + rpw + rpz + rtb + rtm + rtc + rjo
          biom.hard <- racer + ralnu + rarbu + rlith  + rumbe + rpopu + rquec + rquek + rquew
          biom.shrub <- rfxr + rnfr + rnnr
          
          cover.coni <- biom.coni > biom.hard & biom.coni > biom.shrub
          cover.hard <- biom.hard > biom.coni & (biom.coni/total.biomass) < 0.25
          cover.mixed <- biom.hard > biom.coni & (biom.coni/total.biomass) > 0.25
          cover.shrub <- biom.shrub > biom.hard & biom.shrub > biom.coni
          
          #plot(cover.coni)
          #plot(cover.hard)
          #plot(cover.mixed)
          #plot(cover.shrub)
          
          mhc <- cover.mixed
          
          asp <- cover.hard == 1 & (rpopu / biom.hard) > 0.5
          
          mhw <- cover.hard == 1 & ((rquec + rquek + rquew + rarbu + rlith  + rumbe) / biom.hard) > 0.5
          
          mri <- cover.hard == 1 & ((racer + ralnu) / biom.hard) > 0.5
          
          wfr <- cover.coni == 1 & (rac/biom.coni) > 0.5
          
          rfr <- cover.coni == 1 & (ram/biom.coni) > 0.5
          
          jpn <- cover.coni == 1 & (rpj /biom.coni) > 0.5
          
          dfr <- cover.coni == 1 & (rpz/biom.coni) >0.5
          
          ppn <- cover.coni == 1 & ((rpp + rpw)/biom.coni) > 0.5
          
          lpn <- cover.coni == 1 & (rpc/biom.coni) > 0.5
          
          juo <- cover.coni == 1 & (rjo/biom.coni) > 0.5
          
          mch <- cover.shrub == 1
          
          smc <- cover.coni == 1 & ((rac/biom.coni) < 0.5) & ((ram/biom.coni) < 0.5) & ((rpa/biom.coni) < 0.5) & ((rpc/biom.coni) < 0.5) &
            ((rpj/biom.coni) < 0.5) & ((rpl/biom.coni) < 0.5) & ((rps/biom.coni) < 0.5) & (((rpp + rpw)/biom.coni) < 0.5) &
            ((rpz/biom.coni) < 0.5) & ((rtb/biom.coni) < 0.5) & ((rtm/biom.coni) < 0.5) & biomass.SMC > biomass.SCN
          
          scn <- cover.coni == 1 & ((rac/biom.coni) < 0.5) & ((ram/biom.coni) < 0.5) & ((rpa/biom.coni) < 0.5) & ((rpc/biom.coni) < 0.5) &
            ((rpj/biom.coni) < 0.5) & ((rpl/biom.coni) < 0.5) & ((rps/biom.coni) < 0.5) & (((rpp+rpw)/biom.coni) < 0.5) &
            ((rpz/biom.coni) < 0.5) & ((rtb/biom.coni) < 0.5) & ((rtm/biom.coni) < 0.5) & biomass.SCN > biomass.SMC
          
          yps <- cover.coni == 1 & ((rpj + rpp + rpw)/biom.coni) > 0.5
          wps <- cover.coni == 1 & ((rpl + rpm + rpa)/biom.coni) > 0.5
          
          asp[asp[] == 1] <- 1
          mhw[mhw[] == 1] <- 2
          mri[mri[] == 1] <- 3
          wfr[wfr[] == 1] <- 4
          rfr[rfr[] == 1] <- 5
          jpn[jpn[] == 1] <- 6
          ppn[ppn[] == 1] <- 7
          dfr[dfr[] == 1] <- 8
          mhc[mhc[] == 1] <- 9
          lpn[lpn[] == 1] <- 10
          smc[smc[] == 1] <- 11
          scn[scn[] == 1] <- 12
          juo[juo[] == 1] <- 13
          mch[mch[] == 1] <- 14
          
          r.fortype <- asp + mhw + mri + wfr + rfr + jpn + ppn + dfr + mhc + lpn + smc + scn + juo + mch
          #plot(r.fortype)
          r.fortype[r.fortype > 14] <- 9
          frf <- freq(r.fortype)
          
          scen_time_Data <- cbind(frf, j, k, i)
          all_data <- rbind(all_data, scen_time_Data)
          
          r.fortype@crs <- rasterNoProj@crs
          r.fortype@extent <- rasterNoProj@extent
          
          writeRaster(r.fortype, rnameout, datatype = "INT2S", options = "COMPRESS=LZW", overwrite = T)
          }
      }
    }
  }
}

write.csv(all_data, "I:/TCSI/Round2_outputs/CWHR_fortype/fortype_output_allv2.csv")
all_data <- read.csv("I:/TCSI/Round2_outputs/CWHR_fortype/fortype_output_all.csv")
all_data <- as.data.frame(all_data)


colnames(all_data) <- c("fortype", "cell", "climate", "scenario", "time")

all_data$cell <- as.numeric(all_data$cell)
all_data$fortype <- as.integer(all_data$fortype)
all_data$time <- as.integer(all_data$time)
all_data$forname <- as.character(all_data$fortype)



fort <- all_data %>%
#  mutate(Area = cell * 3.24) %>%
#  mutate(Year = time + 2019) %>%
  mutate(forname =
  case_when( forname == 0 ~ "Non-forested",
             forname == 1 ~ "Aspen",
             forname == 2 ~ "Montane Hardwood",
             forname == 3 ~ "Montane Riparian",
             forname == 4 ~ "White fir",
             forname == 5 ~ "Red fir",
             forname == 6 ~ "Jeffrey pine",
             forname == 7 ~ "Ponderosa pine",
             forname == 8 ~ "Douglas-fir",
             forname == 9 ~ "Mixed hardwood conifer",
             forname == 10 ~ "Lodgepole pine",
             forname == 11 ~ "Sierra mixed conifer",
             forname == 12 ~ "Sierra high elevation mixed conifer",
             forname == 13 ~ "Juniper",
             forname == 14 ~ "Chapparal"
             ))

write.csv(fort, "I:/TCSI/Round2_outputs/CWHR_fortype/fortype.csv")

fort <- read.csv("I:/TCSI/Round2_outputs/CWHR_fortype/fortype_output_allv2.csv")

fort_s <- fort %>%
  group_by(Scenario, Year, forname) %>%
  summarise(meanPERC = mean(Percent), meanAREA = mean(Area), sdArea = sd(Area))

fort_c <- fort %>%
  group_by(Climate, Year, forname) %>%
  summarise(meanPERC = mean(Percent), meanAREA = mean(Area), sdArea = sd(Area))


forty_c <- ggplot(fort_c, aes(x = Year, y = meanPERC, fill = forname)) +
  geom_col()+
  scale_fill_manual(values = mycolors) +
  facet_wrap(~Climate) +
  theme_bw() +
  labs(y = "Percent of Area", fill = "Forest Type")

plot(forty_c)

forty_s <- fort_s %>%
  filter(meanPERC > 0.02) %>% 
  ggplot(aes(x = Year, y = meanPERC, fill = forname)) +
  geom_col()+
  scale_fill_manual(values = mycolors) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  facet_wrap(~Scenario) +
  labs(y = "Percent of Area", fill = "Forest Type")

plot(forty_s)

fort1 <- read.csv("I:/TCSI/Round2_outputs/CWHR_fortype/fortype4.csv")

fort1$meanPERC <- round(fort1$meanPERC, 3)

fort1m <- fort1 %>%
  filter(Climate == "MIROC5_85")

forty_s1 <- fort1m %>%
  filter(meanPERC > 0.03) %>% 
  ggplot(aes(x = Year, y = meanPERC, fill = forname)) +
  geom_col()+
  scale_fill_manual(values = mycolors) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  facet_wrap(~Scenario) +
  labs(title = "MIROC5 projection only", y = "Percent of Area", fill = "Forest Type")

plot(forty_s1)

fort1nm <- fort1 %>%
  filter(Climate != "MIROC5_85") %>%
  group_by(Scenario, Year, forname) %>%
  summarise(meanPERC2 = mean(meanPERC), sdPERC2 = sd(sdPERC))

forty_s1nm <- fort1nm %>%
  filter(meanPERC2 > 0.03) %>% 
  ggplot(aes(x = Year, y = meanPERC2, fill = forname)) +
  geom_col()+
  scale_fill_manual(values = mycolors) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  facet_wrap(~Scenario) +
  labs(title = "Other climate projections", y = "Percent of Area", fill = "Forest Type")

plot(forty_s1nm)

png(file = "I:/TCSI/Round2_outputs/spp_biomass_perc.png", width = 6400, height = 3600, res=600)
plot(forty_s1)
dev.off()


fort_scen <- fort %>%
  group_by(Year, scenario, forname) %>%
  summarise(meanArea = mean(Area), sdArea = sd(Area))

fort_clim <- fort %>%
  group_by(Year, climate, forname) %>%
  summarise(meanArea = mean(Area), sdArea = sd(Area))

fortys <- ggplot(fort_scen, aes(x = Year, y = meanArea, fill = forname, color = forname)) +
  geom_line()+
  geom_ribbon(aes(ymax = meanArea + sdArea, ymin = meanArea - sdArea)) +
  facet_wrap(~scenario)

plot(fortys)

fortyc <- ggplot(fort_clim, aes(x = Year, y = meanArea, fill = forname, color = forname)) +
  geom_line()+
  geom_ribbon(aes(ymax = meanArea + sdArea, ymin = meanArea - sdArea)) +
  facet_wrap(~climate)

plot(fortyc)

fort_coll <- fort %>%
  group_by(Year, forname) %>%
  summarise(meanArea = mean(Area), sdArea = sd(Area))

forty_coll <- ggplot(fort_coll, aes(x = Year, y = meanArea, fill = forname)) +
  geom_line()+
  geom_ribbon(aes(ymax = meanArea + sdArea, ymin = meanArea - sdArea)) +
  labs(y = "Area in hectares", fill = "Forest type")

plot(forty_coll)

r1 <- raster("F:/TCSI/Round2_outputs/CWHR_fortype/fortype_HADGEM85_SSP5_Scenario2_1_1.tif")

r2 <- raster("F:/TCSI/Round2_outputs/CWHR_fortype/fortype_HADGEM85_SSP5_Scenario2_1_20.tif")

r3 <- raster("F:/TCSI/Round2_outputs/CWHR_fortype/fortype_HADGEM85_SSP5_Scenario2_1_40.tif")

r4 <- raster("F:/TCSI/Round2_outputs/CWHR_fortype/fortype_HADGEM85_SSP5_Scenario2_1_60.tif")

r5 <- raster("F:/TCSI/Round2_outputs/CWHR_fortype/fortype_HADGEM85_SSP5_Scenario2_1_80.tif")

fortype_stack <- stack(r1, r2, r3, r4, r5)
plot(fortype_stack)


####try to break out by percent by climate

fort <- read.csv("I:/TCSI/Round2_outputs/CWHR_fortype/fortype.csv")

fort2 <- fort %>% 
  group_by(Climate, Scenario, Year, forname) %>%
  summarise(perc = mean(Percent))

fort_miroc <- fort2 %>%
  filter(Climate == "MIROC5_85")
  