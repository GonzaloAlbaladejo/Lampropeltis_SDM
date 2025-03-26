rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # Set the working directory to the directory in which the code is stored
td <- tempdir()
#
#
#/\/\/\/\/\/\/\/\\/\/\/\/\/\/\/\/\/\/\/\/\/////\/\/\/\/\/\//\/\/\/>//\/\///\
##                                                                          \//\/|/      
#### Preparation of the environmental information and raster data for the 
##                (California or Eastern Kingsnake)  
##                                                                          \//\/\/\
#/\/\/\/\/\/\/\/\\/\/\/\/\/\/\//\/\/\/\/\/\/////\/\/\/\/\/\///\/\\//\/\/\///\
#
# 0. load the needed libraries----
list.of.packages<-c("tidyverse","rstudioapi","data.table","terra","sf","viridis","geodata")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages,require,character.only=TRUE)
rm(list.of.packages,new.packages)

# Load the needed functions
functions<-"./Functions" %>% list.files(recursive = FALSE,pattern = ".R$",full.names = TRUE)
lapply(functions,function(x) source(x)) #; lapply(functions,function(x)readLines(x) %>% length()) %>% unlist() %>% sum()

# 0. Some basic information for the study 
crs_p <- "EPSG:4326"

# 1. Building a raster----
# Raster data usually takes the shape of numerical or categorical matrices that contain spatial references that allow them to
# characterize an spatial area.
#
# a. Lets create a raster
  # First we create some fake data to populate our matrix. 
  # Since we want to populate a matrix with this data we need to make sure that we 
  # have enough information for each row and column
  r_dat <- rnorm(mean=0,sd=3,n=c(1000*1000)) 
  r_dat <- matrix(r_dat,ncol=1000,nrow=1000)
  
  r_dat <- terra::rast(r_dat)
  r_dat

# This is how our raster looks like!
  par(mfrow=c(1,1))
  r_dat %>% plot(col=viridis::inferno(n=200))

# b. We have a raster representing some random data, however there is no spatial information store in it.
  # We need to add more information for it to be useful. Lets try to coerce this raster to our study region
  
  # 1. Load some spatial information
  Global <- st_read("./Data/Process_data/General/Glob_map.shp")
  Sp_range <- st_read("./Data/Process_data/General/LampCaliforniaeRang.shp")
  study_t <- st_read("./Data/Other_ranges/Pro_BaseLayer.shp")
  
  study_t <- study_t %>% st_transform(crs=crs_p)
  
  # 2. Coerce our raster to two different spatial extends  
  glob_rast <- r_dat
  # range_rast <- r_dat
  
  ext(glob_rast) <- ext(Global)
  # ext(range_rast) <- ext(Sp_range)
  
# 3. The shape of our raster has changed according to the spatial dimension of our vector data. Not only that,
# we have changed the data stored in our raster, the extent and resolution fields are now different. For the extent and resolution
# to make sense, we need to project this data into space (we need to add a Coordinate Reference System)
  glob_rast
  crs(glob_rast) <- crs_p
  glob_rast <- terra::project(glob_rast,y=crs_p)
  glob_rast
  
  # a. Check the raster ----
  par(mfrow=c(2,1))
  glob_rast %>% plot(col=viridis::inferno(n=200)) # now our raster is compatible with the rest of spatial data
  Global %>% st_geometry() %>% plot(add=T,border="white",lwd=1.5) # see how everything line up
  mtext(side=3,adj=0,"a. Non-masked raster",cex=1.2)
  
  glob_mask <- glob_rast %>% mask(mask=Global %>% vect()) 
  glob_mask %>% plot(col=viridis::inferno(n=200)) # We don't want to have data into the ocean, so we are going to mask it
  mtext(side=3,adj=0,"b. Masked raster",cex=1.2)

  # b. Rasters can be projected and formatted just like vector data. ----
  crs_list <- list("+proj=fouc","+proj=lagrng","+proj=collg",
                   "epsg:3035","+proj=eck1") # These are some cool global projections
  
  lt <- layout(matrix(c(rep(rep(c(1,2,3),each=2),3),rep(rep(c(4,5),each=3),3)),ncol=6,nrow=6),respect = T)
  layout.show(lt)
  
  for(i in 1:length(crs_list)){
    plot(glob_mask %>% project(y=crs_list[[i]]),col=viridis::inferno(n=200))
    mtext(paste0(letters[i],")"),side=3,adj=0)
  }  
  
  # c. However, if we want to have multiple variables, we need to build a `stack` of raster layers, one for each variable ----
  # All layers need to have the same parameters (extension, resolution, and crs)
  # Var 2 
  r_2 <- rast(vals=rnorm(mean=4,sd=0.2,n=c(1000*1000))%>%order(), extent = ext(glob_mask),
                res=res(glob_mask),crs=crs(glob_mask)) %>% mask(mask=Global %>% vect())
  
  # Var 3
  r_3 <- rast(vals=0,res=res(glob_mask),crs=crs(glob_mask),extent = ext(glob_mask)) %>% 
                mask(mask=Global %>% vect(),inverse=T)
  

  # Combine all the variables into a stack and display the information
  cool_stack<-c(glob_mask,r_2,r_3)
  plot(cool_stack)  
    
# 4. Lets work with some real data ----
# We are going to use the geodata library to download some bioclimatic information from world-clim
# https://www.worldclim.org/

# a. Dowload the data ----
  bio_route <- "./Data/BioClim" ; bio_route %>% dir.create(showWarnings = F,recursive = T)
  bioClim <- geodata::worldclim_global(var="bio",res=5,path=bio_route)
  
# b. Each dataset consist of 19 different climatic variables with ecological potential to define species niches ---- 
  bioClim %>% plot()
  
# c. We don't need the full extension of the dataset, lets crop the raster----
# We can crop the raster stack using a vectorial object. In this case we are going to take the data from the united 
# states and mexico
  countries <- geodata::world(resolution=3,level=0,path="./Data/Global")
  countries <- countries %>% st_as_sf()

  countries <- countries %>% filter(GID_0 %in% c("USA","MEX"))  
  
  # The USA is made up of several different polygons, but we are only interested in the larger portion
  USA_split <- countries[2,] %>% st_cast("POLYGON")
  area_USA <- USA_split %>% st_area()
  
  countries[2,] <- USA_split[area_USA==max(area_USA),] #%>% st_geometry() %>% plot()
  # countries %>% st_geometry() %>% plot()
  
  c_list <- list()
  
  for(i in 1:nrow(countries)){
      c_list[[i]]<- bioClim %>% crop(y=countries[i,] %>% vect())
      names(c_list)[i]<-countries[i,"NAME_0"] %>% st_drop_geometry()
      }
  
  par(mfrow=c(2,1))
  c_list$`United States`$wc2.1_5m_bio_1 %>% plot(col=viridis::viridis(n=200)) ; mtext(side=3,adj=0,"b. United States of America")
  plot(Sp_range %>% st_geometry(),add=T,lwd=2)
  
  c_list$Mexico$wc2.1_5m_bio_1 %>% plot(col=viridis::viridis(n=200)) ; mtext(side=3,adj=0,"a. Mexico")
  plot(Sp_range %>% st_geometry(),add=T,lwd=2)

# d. To obtain a full picture of the study area we need to combine both tiles/layers----   
 full_rast <- terra::merge(c_list[[1]],c_list[[2]],na.rm=TRUE)
 
  # Crop and plot
  par(mfrow=c(1,2))
  plot(full_rast$wc2.1_5m_bio_1,col=viridis::viridis(n=200))
  plot(Sp_range %>% st_geometry(),add=T,lwd=2)
  mtext(side=3,adj=0,"a. Combined tiles")

# e. Crop the raster to the size of the study area----   
  crop_rast <- full_rast %>% crop(y=Sp_range %>% vect())
  plot(crop_rast$wc2.1_5m_bio_1,col=viridis::viridis(n=200))  
  plot(Sp_range %>% st_geometry(),add=T,lwd=2)
  mtext(side=3,adj=0,"a. Study area")
  
# 5. Extract data from a raster file
  # a. Load some point data
  Sp_points <- st_read("./Data/Process_data/Records/Gbif_rec.gpkg")  
  
  # b. extract the information from the raster
  Sp_data <- terra::extract(crop_rast,Sp_points %>% vect())
  head(Sp_data)
  
# 6. Operations with rasters----
  # a. Load the satellite data for the calculation of the NDVI metric----
  Sentinel_routes <- "./Data/Sentinel" %>% list.files(pattern=".tif$",recursive = TRUE,full.names = TRUE)
  sentinel_rast <- lapply(Sentinel_routes,rast)
  
  # b. Combine the rasters and harmonize with the rest of spatial information----
  strip1 <- merge(sentinel_rast[[1]],sentinel_rast[[2]]) 
  strip1 <- strip1 %>% project(y=crs_p) %>% crop(y=study_t %>% filter(Island=="Gran_Canaria") %>% st_geometry())
  
  strip1 <- strip1 %>% mask(mask=study_t %>% filter(Island=="Gran_Canaria") %>% vect())
  strip1 %>% plot(col=viridis::turbo(n=200))
  
  # c. Calculate the NDVI using the formula NDVI = B8-B4B8+B4
  NDVI_strip1 <- (strip1$`B08-1`-strip1$`B04-1`)/(sum(strip1)) ; names(NDVI_strip1) <- "NDVI"
  plot(NDVI_strip1,col=viridis(1000),box="n",axes=F,main="NDVI Gran Canaria")
  
