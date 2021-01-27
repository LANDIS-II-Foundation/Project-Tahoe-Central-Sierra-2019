####Call necessary packages###
library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
library(spatialEco)
library(Hmisc)


######Set working directory####
p <- "I:/TCSI/Round2_outputs/"

setwd(p)
clim_dir <- c("CNRM85", "GFDL85", "HADGEM85", "MIROC5_85")
scen_dir <- c("Scenario1_1", "Scenario2_1", "Scenario3_1", "Scenario4_1", "Scenario5_1", "Scenario6_1")
timesteps <- 1:30

TOT_TCSI <- raster("F:/TCSI/Round2_outputs/CANESM85/SSP2_Scenario1_1/rx_equal1.tif")
TOT_TCSI[TOT_TCSI == 0] <- NA
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

scen_data <- NULL
all_data <- NULL

i <- clim_dir[1]
j <- lf[1]
k <- scen_rep[1]
timesteps <-1:80


lf_existing <- list.files("I:/TCSI/Round2_outputs/fire_maps/")

r1 <- raster("I:/TCSI/Round2_outputs/CANESM85/SSP2_Scenario1_1/scrapple-fire/ignition-type-10.img")
plot(r1)
freq(r1)

j<-lf[1]

all_data <- NULL

for(i in clim_dir){

  path <- paste0(p,i,"/")
  lf <- list.files(path = path, full.names = F, include.dirs = T)
  #lf <- lf[-1]
  
  for(j in lf){
  
    path <- paste0(p,i,"/",j,"/scrapple-fire/")
    setwd(path)
    print(path)
    
    lf <- list.files(pattern = "fire-intensity-.*.img")
    lf2 <- list.files(pattern = "ignition-type-.*.img")
    #timesteps <- length(lf)
    #timesteps <- c(1:timesteps)
    rnameout <- paste0("I:/TCSI/Round2_outputs/ms_hs_fires/fire_mshs2050_",i,"_",j,".tif")
    #existing_name <- paste0("HS_fire_gt100ha_",i,"_",j,".tif")
    #skipval <- grep(existing_name, lf_existing)
    
    #if( length(skipval) == 0){
    
    binary.burn.map <- lapply(timesteps, function (timesteps){
        r <- raster (lf[timesteps])
        r3 <- r == 3
        r4 <- r == 4
        return(r4)
      })

      binary.burn.map2 <- Reduce (stack, binary.burn.map)
      reburn.times.map  <- sum (binary.burn.map2)
      reburn.times.map@crs <- rasterNoProj@crs
      reburn.times.map@extent <- rasterNoProj@extent
      
      z1 <- zonal(reburn.times.map, treatmentzones, "sum")
      scen_data <- cbind(z1, i, j)
      all_data <- rbind(scen_data, all_data)
      #print(rnameout)
      #writeRaster(reburn.times.map, rnameout, overwrite=T)
      
      
    }
  }
#}

treatmentzones <- raster("I:/TCSI/Round2_outputs/MIROC5_85/SSP2_Scenario4_5/tz1_20_ssp2_v3.tif")
treatmentzones[treatmentzones < 10] <- 0
treatmentzones[treatmentzones > 27] <- 0
treatmentzones[treatmentzones > 0] <- 1
freq(treatmentzones)
plot(treatmentzones)

##############################

wd <- "I:/TCSI/Round2_outputs/ms_hs_fires/"
setwd(wd)

lf <- list.files()
i <- lf[1]

all_data <- NULL

for(i in lf){
  r1 <- raster(i)
  print(i)
  rname <- unlist(strsplit(i, "_"))
  repname <- unlist(strsplit(rname[6], "\\."))[1]
  clim_name <- rname[3]
  scen_name <- rname[5]
  plot(r1)  
  z1 <- zonal(r1, treatmentzones, "sum")
  scen_data <- cbind(z1, clim_name, scen_name, repname)
  all_data <- rbind(scen_data, all_data)
}

write.csv(all_data, "I:/TCSI/Round2_outputs/mshs_wui_area_2050.csv")

all_data2 <- as.data.frame(all_data)



####################average across all replicates##########################
wd <- "I:/TCSI/Round2_outputs/ls_ms_fires/"
setwd(wd)
lf <- list.files(pattern = ".*.tif")
head(lf)
pattern <- "MIROC5"
miroc <- grep(pattern = pattern, lf, value = T)
no_miroc <- lf[lf %nin% miroc_only]


scen_dir <- c("Scenario1", "Scenario2", "Scenario3", "Scenario4", "Scenario5", "Scenario6")
i <- scen_dir[1]

for(i in scen_dir){
  lf1 <- grep( paste0(i), lf, value = T)
  print(lf1)
  scen_tot <- sum(stack(lf1))
  scen_tot <- raster::mask(scen_tot, TOT_TCSI)
  scen_tot@crs <- rasterNoProj@crs
  scen_tot@extent <- rasterNoProj@extent
  
  rnameouta <- paste0("total_fire_avg_", i, ".tif")
  scen_avg <- scen_tot / length(lf1)
  plot(scen_avg)
  writeRaster(scen_avg, rnameouta, overwrite = T)
    
  rnameoutp <- paste0("total_fire_pct_", i, ".tif")
  scen_pct <- (scen_avg / 80) * 100
  plot(scen_pct)
  writeRaster(scen_pct, rnameoutp, overwrite = T)
  
  rnameoutf <- paste0("total_fire_fri_", i, ".tif")
  scen_fri <- 80 / (scen_avg + 1)
  plot(scen_fri)
  writeRaster(scen_fri, rnameoutf, overwrite = T)
}

scen6 <- raster("total_fire_avg_Scenario6.tif")
scen5 <- raster("total_fire_avg_Scenario5.tif")
scen4 <- raster("total_fire_avg_Scenario4.tif")
scen3 <- raster("total_fire_avg_Scenario3.tif")
scen2 <- raster("total_fire_avg_Scenario2.tif")
scen1 <- raster("total_fire_avg_Scenario1.tif")

scen6 <- scen6 * 80
plot(scen6)
scen5 <- scen5 * 80
scen4 <- scen4 * 80
scen3 <- scen3 * 80
scen2 <- scen2 * 80
scen1 <- scen1 * 80

writeRaster(scen6, "total_fire_count_Scenario6.tif")
writeRaster(scen5, "total_fire_count_Scenario5.tif")
writeRaster(scen4, "total_fire_count_Scenario4.tif")
writeRaster(scen3, "total_fire_count_Scenario3.tif")
writeRaster(scen2, "total_fire_count_Scenario2.tif")
writeRaster(scen1, "total_fire_count_Scenario1.tif")




scen6 <- raster("fire_pct_Scenario6.tif")
scen5 <- raster("fire_pct_Scenario5.tif")
scen4 <- raster("fire_pct_Scenario4.tif")
scen3 <- raster("fire_pct_Scenario3.tif")
scen2 <- raster("fire_pct_Scenario2.tif")
scen1 <- raster("fire_pct_Scenario1.tif")

sta1 <- stack(scen1, scen2, scen3, scen4, scen5, scen6)
plot(sta1)

scen6 <- raster("H:/TCSI/hs_fire_maps/hs40_avg_Scenario6_1.tif")
scen5 <- raster("H:/TCSI/hs_fire_maps/hs40_avg_Scenario5_1.tif")
scen4 <- raster("H:/TCSI/hs_fire_maps/hs40_avg_Scenario4_1.tif")
scen3 <- raster("H:/TCSI/hs_fire_maps/hs40_avg_Scenario3_1.tif")
scen2 <- raster("H:/TCSI/hs_fire_maps/hs40_avg_Scenario2_1.tif")
scen1 <- raster("H:/TCSI/hs_fire_maps/hs40_avg_Scenario1_1.tif")

stacltw <- stack(scen1, scen2, scen3, scen4, scen5, scen6)
plot(stacltw)

freq(stacltw)

r1 <- raster("E:/SNPLMA3/hs_fire_maps/HS_fire_gt40ac_Scenario5_MIROC5_8.5_1.tif")
r2 <- raster("E:/SNPLMA3/hs_fire_maps/HS_fire_gt40ac_Scenario5_MIROC5_8.5_2.tif")
r3 <- raster("E:/SNPLMA3/hs_fire_maps/HS_fire_gt40ac_Scenario5_MIROC5_8.5_3.tif")
plot(r1)
plot(r2)
plot(r3)
