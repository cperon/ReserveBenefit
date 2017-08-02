###########################################################################################################
# Create an animated image of primary productivity in the Balearic Sea (project ReserveBenefit)
#
# Author : Clara Peron, clara.peron22@gmail.com
# Date : 02-08-2017
# 
# R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
# Copyright (C) 2016 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
###########################################################################################################

setwd('C:/Users/Clara PERON/Documents/ReserveBenefit/')

library(fields)
library(raster)
library(ncdf4)
library(animation)
library(rgdal)

# Load data
  # coastlines
   #load('C:/Users/Clara PERON/Documents/ReserveBenefit/World_WGS84_Fine_Reso.Rdata')
   #coast <- crop(World, extent(-6, 6, 37, 45))
   #save(coast, file='WMed_coast.Rdata')
   load('data/WMed_coast.Rdata')
   barcelona <-  c(2.158889, 41.38861) 
   
  # mpas
   mpas <- readOGR('C:/Users/Clara PERON/Documents/ReserveBenefit/data/MPA_MedW.shp', 'MPA_MedW')
   mpas1 <- mpas[!mpas@data$IUCN_CAT %in% c('Not Reported', 'Not Applicable'),]

  # net Primary Production in sea water 
    chla <- nc_open('data/cmemsv02-med-ogs-bio-an-fc-d_1501683750533.nc')
    # data source : Mediterranean Sea Biogeochemistry Analysis and Forecast - 3DVAR-OGSTM-BFM model system
    # http://marine.copernicus.eu/services-portfolio/access-to-products/?option=com_csw&view=details&product_id=MEDSEA_ANALYSIS_FORECAST_BIO_006_006
    # tendency_of_mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water_due_to_net_primary_production 
    # unit : mol m-3 s-1 

      # Format ncdf
    # Read time
      t <- ncvar_get(chla,"time")
      reftime <- as.POSIXct(strptime('1970-01-01 12:00:00', format='%Y-%m-%d %H:%M:%S', tz='GMT'))
      vartime <- reftime+(t*3600*24)
      vartime
    # Read lon/lat  
      lon <- ncvar_get(chla, "longitude")
      lat <- ncvar_get(chla, "latitude")
      lon.vec  <- as.vector(lon)
      lat.vec  <- as.vector(lat)

    # Read chlorophyll data
      chla <- ncvar_get(chla,"ppn")

    # Map example
      i=1
      date <- vartime[i]
      chla.vec <- as.vector(chla[,,i])

      coord <- expand.grid(lon.vec, lat.vec)
      coord$chla <- chla.vec
      ras <- rasterFromXYZ(coord)
      ras <- ras*1000000
      ras[ras>0.04] <- 0.04
      
      plot(2,40, xlim=c(-2, 5), ylim=c(38,44), xlab='Longitude', ylab='Latitude', main=date)
      plot(log(ras+1), col=tim.colors(100), zlim=c(0,0.04), add=T)
      plot(coast, add=T, col='grey')
      plot(mpas, add=T)
      box()

    # Make an animation
      
    # Set animation parameters
    oopt = ani.options(ani.height = 800, ani.width = 1000, interval=1.5, verbose=F)
    ani.options(convert = 'C:/PROGRA~1/ImageMagick-7.0.5-Q16/magick.exe', ani.height = 800, ani.width = 1000, interval=1.5, verbose=F)
    
    # Make an Gif animation for total effort
    saveGIF({
      for (i in 1:length(vartime)){
        print(i)
        chla.vec <- as.vector(chla[,,i])
        #chla.vec <- ifelse(is.na(chla.vec)==T, 0, chla.vec)
        #chla.vec <- ifelse(chla.vec==0, NA, chla.vec)
        
        coord <- expand.grid(lon.vec, lat.vec)
        coord$chla <- chla.vec
        ras <- rasterFromXYZ(coord)
        
        ras <- ras*1000000
        ras[ras>0.04] <- 0.04
        
        date <- vartime[i]
        
        #ani.options(oopt)
        par(mar=c(5,5,9,12))
        plot(2,40, xlim=c(-2, 5), ylim=c(38,44), xlab='Longitude', ylab='Latitude', main=date)
        plot(log(ras+1), col=tim.colors(100), zlim=c(0,0.04), add=T, smallplot=c(0.85,0.88,0.2,0.8))
        plot(coast, add=T, col='grey')
        plot(mpas, add=T)
        plot(mpas1, add=T, lwd=2, border='red')
        plot(coast, add=T, col='grey')
        points(barcelona, pch=16, cex=2)
        text(c(2.158889, 41.55), 'Barcelona')
        box()
        
      }
    })
    #img.name= paste('Chla', substr(date, 1, 9), sep="_"), htmlfile = "Chla.html", title='Chla')

 

    