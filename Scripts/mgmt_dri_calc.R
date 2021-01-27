####Call necessary packages###
library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
library(spatialEco)


######Set working directory####
p <- "I:/TCSI/Round2_outputs/"

setwd(p)

clim_dir <- c("CANESM85", "CNRM85", "GFDL85", "HADGEM85", "MIROC5_85")
timesteps <- 1:80

TOT_TCSI <- raster("F:/TCSI/Round2_outputs/CANESM85/SSP2_Scenario1_1/rx_equal1.tif")
TOT_TCSI[TOT_TCSI == 0] <- NA
plot(TOT_TCSI)
######################PROJECT RASTER SPATIALLY#########################################
rasterNoProj <- raster(nrow = 800, ncol = 650)
newproj <-  "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs"
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

i <- clim_dir[1]
j <- lf[1]
l <- timesteps[1]

scen_data <- NULL
all_data <- NULL
stafire <- stack()
stahar <- stack()


for(i in clim_dir){
  
  path <- paste0(p,i,"/")
  lf <- list.files(path = path, full.names = F, include.dirs = T)
  
  for(j in lf){
  
    path2 <- paste0(p,i,"/",j,"/")   
    
    stafire <- stack()
    stahar <- stack()
    stabug <- stack()
    
    #ilist <- unlist(strsplit(i, "/"))
    #ilist2 <- ilist[2]
    path3 <- paste0(path2,"scrapple-fire/")
    setwd(path3)
    lf <- list.files(pattern = "ignition-type.*")
    
    for(k in lf){
      r1 <- raster(k)
      #plot(r1)
      r1[r1 < 4] = 0
      #r1[r1 == 4] <- 0
      r1[r1 == 4] <- 1
      stafire <- stack(stafire, r1)
    }
    
    rasfire <- sum(stafire)
    rasfire@crs <- rasterNoProj@crs
    rasfire@extent <- rasterNoProj@extent
    #plot(rasfire)
    
    path3 <- paste0(path2,"harvest/")
    setwd(path3)
    lfh <- list.files(pattern = "biomass-removed.*")
    for(k in lfh){
      r1 <- raster(k)
      r1[r1 == 1] <- 0
      r1[r1 > 1] <- 1
      stahar <- stack(stahar, r1)
    }
    
    rashar <- sum(stahar)
    rashar@crs <- rasterNoProj@crs
    rashar@extent <- rasterNoProj@extent
    #plot(rashar)
    

    totalras <- rasfire + rashar 
    totalDRI <- 80/(totalras + 1)
    
    rnameout <- paste0("I:/TCSI/Round2_outputs/mgmt_only/human_totdist_",i, "_",j,".tif")
    writeRaster(totalras, rnameout, overwrite = T)
    
    drnameout <- paste0("I:/TCSI/Round2_outputs/mgmt_only/human_DRI_",i, "_",j,".tif")
    writeRaster(totalDRI, drnameout, overwrite = T)
  }
}

wd <- "I:/TCSI/Round2_outputs/mgmt_only/"
setwd(wd)
lf <- list.files()
print(lf)

tcsi<- raster("E:/TCSI/TCSI_scenarios/low_spread/HADGEM_85_SSP2/Scenario6_1/accidental1.tif")
tcsi

scen_dir <- c("Scenario1", "Scenario2", "Scenario3", "Scenario4", "Scenario5", "Scenario6")
i <- scen_dir[6]

for(i in scen_dir){
  lf1 <- list.files(pattern = paste0("human_totdist_.*_",i,".*.tif"))

  scen_tot <- sum(stack(lf1))  
  scen_tot@crs <- TOT_TCSI@crs
  scen_tot@extent <- TOT_TCSI@extent
  
  rnameouta <- paste0("mgmt_avg_", i, ".tif")
  scen_avg <- scen_tot / length(lf1)
  scen_avg <- mask(scen_avg, TOT_TCSI)
  plot(scen_avg)
  writeRaster(scen_avg, rnameouta, overwrite = T)
  
  rnameoutp <- paste0("mgmt_pct_", i, ".tif")
  scen_pct <- (scen_avg / 80) * 100
  plot(scen_pct)
  writeRaster(scen_pct, rnameoutp, overwrite = T)
  
  rnameoutf <- paste0("mgmt_fri_", i, ".tif")
  scen_fri <- 80 / (scen_avg + 1)
  plot(scen_fri)
  writeRaster(scen_fri, rnameoutf, overwrite = T)
}

scen6 <- raster("F:/TCSI/Round2_outputs/DRI/hd_fri_Scenario6.tif")
scen5 <- raster("F:/TCSI/Round2_outputs/DRI/hd_fri_Scenario5.tif")
scen4 <- raster("F:/TCSI/Round2_outputs/DRI/hd_fri_Scenario4.tif")
scen3 <- raster("F:/TCSI/Round2_outputs/DRI/hd_fri_Scenario3.tif")
scen2 <- raster("F:/TCSI/Round2_outputs/DRI/hd_fri_Scenario2.tif")
scen1 <- raster("F:/TCSI/Round2_outputs/DRI/hd_fri_Scenario1.tif")

dri_hd <- stack(scen1, scen2, scen3, scen4, scen5, scen6)

plot(dri_hd)

scen6[scen6 == 80 ] <- NA
cellStats(scen6, mean)

scen5[scen5 == 80 ] <- NA
cellStats(scen5, mean)

scen4[scen4 == 80 ] <- NA
cellStats(scen4, mean)

scen3[scen3 == 80] <- NA
cellStats(scen3, mean)

scen2[scen2 == 80] <- NA
cellStats(scen2, mean)

scen1[scen1 == 80] <- NA
cellStats(scen1, mean)


plot(dri_hd)