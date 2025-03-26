rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # Set the working directory to the directory in which the code is stored
td <- tempdir()
#
# maptools, rgdal, and rgeos, raster
#
#/\/\/\/\/\/\/\/\\/\/\/\/\/\/\/\/\/\/\/\/\/////\/\/\/\/\/\//\/\/\/>//\/\///\
##                                                                          \//\/|/      
#### Preparation of the point data from the Life+ Lampropeltis   
##                                                                          \//\/\/\
#/\/\/\/\/\/\/\/\\/\/\/\/\/\/\//\/\/\/\/\/\/////\/\/\/\/\/\///\/\\//\/\/\///\
#
# 0. load the needed libraries----
list.of.packages<-c("tidyverse","rstudioapi","data.table","pdftools","sf")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages,require,character.only=TRUE)
rm(list.of.packages,new.packages)

# Load the needed functions
functions<-"./Functions" %>% list.files(recursive = FALSE,pattern = ".R$",full.names = TRUE)
lapply(functions,function(x) source(x)) #; lapply(functions,function(x)readLines(x) %>% length()) %>% unlist() %>% sum()
sf::sf_use_s2(FALSE)

# 0. Load some general spatial information---
# Get some additional spatial information
# a. Download the spatial layout of the world land-masses
"./Data/Global" %>% dir.create(recursive = T,showWarnings = T)
global <- geodata::world(resolution = 1,level=0,path="./Data/Global")
global <- st_as_sf(global)

# b. Display the information
global <- global %>% st_make_valid() %>% st_union()

plot(global %>% st_geometry(),col="grey5")
g = st_graticule(global) ; plot(g[1], add = TRUE, col = 'grey')

# c. Spatial data need to be projected or referenced in order to be useful
# Different projections serve different purposes
  crs_list <- list("+proj=fouc","+proj=lagrng","+proj=collg",
                   "+proj=chamb +lat_1=10 +lon_1=30 +lon_2=40","+proj=eck1") # These are some cool global projections
  
  lt <- layout(matrix(c(rep(rep(c(1,2,3),each=2),3),rep(rep(c(4,5),each=3),3)),ncol=6,nrow=6),respect = T)
  layout.show(lt)
  par(par=c(0,0,0,0))
  
  for(i in 1:length(crs_list)){
    plot(global %>% st_transform(crs=crs_list[[i]]) %>% st_geometry(),col="gold3",border=NA)
              g = st_graticule(global %>% st_transform(crs=crs_list[[i]]))
              plot(g[1], add = TRUE, col = 'grey25',xpd=T)
              mtext(paste0(letters[i],")"),side=3,adj=0)
              }  

# This global data is going to help us put our data into context globally  
      
# 1. Lampropeltis Life+ data ----
  # We have some data from a project that aims to control an alien invasive species that was introduced into an island:
  # a. The data is presented as a csv file with 4 variables (type of records, date, X coordinate, and Y coordiante)
    data <- read.csv(paste("./Data/Life+ Lampropeltis","Raw_records.csv",sep="/"))
    head(data)
    
  # b. We need to transform this CSV into a projected spatial object that can interact with the rest of spatial data
    crs_life <- "EPSG:4326" # This data is formatted using the UTM reg-can 95 EPSG:4326
    PointsLife <- data %>% st_as_sf(coords=c("Coorde.nadaX","coorden.adaY"),crs=crs_life) 
    
  # c. Now that we have the points, we need some other spatial information to add context to this information
    LifeRANGE <- st_read("./Data/Other_ranges" %>% list.files(pattern=".shp$",full.names = T))
  
  # d. Plot the information
    par(mfrow=c(1,1))
    plot(LifeRANGE %>% st_geometry())
    plot(PointsLife %>% st_geometry(),pch=19,col="tomato3" %>% adjustcolor(alpha.f = 0.10),add=T)
    
    # The general map is ok but we can add more information, such as the name of the islands
    centroids <- st_centroid(LifeRANGE) %>% st_coordinates() # To allocate the labels we are going to take the centroids of the polygons as a guide
    LifeRANGE <- cbind(LifeRANGE,centroids) # Lets add this to the polygon information
    head(centroids) # X Y coordinates
    
    # Display the information
    plot(LifeRANGE %>% st_geometry()) # Now lets use the coordiantes of the centroids to allocate the labels
    
    text(x=LifeRANGE$X,y=LifeRANGE$Y,labels=LifeRANGE$Island,col="blue4") # the location is a bit off, lets add some space in the Y axis
    text(x=LifeRANGE$X,y=LifeRANGE$Y+40000,labels=LifeRANGE$Island,col="red",cex=0.75,xpd=T)
  
  # e. The map is way to general and offer litle detail about the distribution of the species
    plot(LifeRANGE %>% filter(Island=="Gran_Canaria") %>% st_geometry()) # subset the spatial information
    plot(PointsLife %>% st_geometry(),pch=19,col="tomato3" %>% adjustcolor(alpha.f = 0.10),add=T)

  # f. Put everything together in a fancy way
    tl<-layout(matrix(c(2,rep(1,times=8)),ncol = 3,nrow = 3,byrow = T))
    layout.show(tl)
    par(bg="white")
    
    # Study area map
    plot(LifeRANGE %>% filter(Island=="Gran_Canaria") %>% st_geometry()) # subset the spatial information
    plot(PointsLife %>% st_geometry(),pch=19,col="tomato3" %>% adjustcolor(alpha.f = 0.10),add=T) # Add the labels
    mtext(side=3,adj=1,"b. Gran Canaria Island") # Panel legend
    
    # Archipelago map
    par(bg=NA)
    plot(LifeRANGE %>% st_geometry(),col=ifelse(LifeRANGE$Island=="Gran_Canaria","tomato3","grey88"))
    text(x=LifeRANGE$X,y=LifeRANGE$Y+40000,labels=LifeRANGE$Island,col="black",cex=0.75,xpd=T) # Add the labels
    mtext(side=3,adj=0,"a. Canary Islands") # Panel legend
    par(bg="white")
  
# 2. Load the spatial data from the IUCN Red-List ----
  # Get the range of the species from the IUCN Red List : `https://www.iucnredlist.org/resources/spatial-data-download`
  pol_exit <- paste(td,"IUCN_pols",sep="/") # Create a temporal folder to host the data
  dir.create(pol_exit,recursive = TRUE,showWarnings = FALSE)

  # Lets unzip the REPTILE spatial data from the IUCN Red-List
  "./Data/IUCN_ranges" %>% list.files(pattern=".zip$",full.names = TRUE) %>% unzip(exdir=pol_exit)
  route_IUCN <- pol_exit %>% list.files(pattern=".shp$",full.names = TRUE)
  
  # Load the polygons
  IUCN_ranges <- rbind(route_IUCN[1] %>% st_read(),route_IUCN[2] %>% st_read())
  IUCN_Lamp <- IUCN_ranges %>% filter(genus=="Lampropeltis")
  
  SpeciesIUCN <- IUCN_Lamp$sci_name %>% unique()

# 3. Get the GBIF records (Skip this for the seminar!) ----
  crs_gbif <- "EPSG:4326"
  
  # a. Get he GBIF spatial records for our species ----
  "./Data/Gbif" %>% dir.create(recursive=T,showWarnings = F)
  
  # b. Get the taxonomic information for the species ----
  TaxInformation <- lapply(SpeciesIUCN,function(x) retrieve_syns(x,n_times=50))
  TaxData <- lapply(TaxInformation,function(x) x[["TaxDat"]]) ;  TaxData <- do.call("rbind",TaxData)
  
    names_sp <- list()
    for(i in 1:nrow(TaxData)){
      # Combine the names
      names_l <- TaxData[i,colnames(TaxData) %in% c("Or_name","IUCN_name","IUCN_syn","ITIS_name","ITIS_syn")] %>% paste(collapse=";")
      
      # Remove the NAs and create a list of names
      names_l <- strsplit(names_l,split=";") %>% unlist()
      
      # Remove the NAs and duplicated names
      names_l <- names_l[!names_l %in% "NA"]
      names_l <- unique(names_l)
      
      names_sp[[i]] <- list(Or_name=TaxData[i,"Or_name"],All_names=names_l)
      
      }
  
  # b.1 Export the information ----
    "./Data/Sp_info" %>% dir.create(recursive = TRUE,showWarnings = FALSE)
    
    write_rds(names_sp,file=paste("./Data/Sp_info","Species_names.rds",sep="/"))
    write.csv(TaxData,paste("./Data/Sp_info","Species_tax.csv",sep="/"))
  
  # c. With the species information, download the GBIF spatial records ----
    for(i in 1:length(names_sp)){
      print(paste("Getting the information for",names_sp[[i]][["Or_name"]]))
      try(Dowload_gbif(sp_list = names_sp[[i]][["All_names"]],
                       exit_route = "./Data/Gbif",
                       initial_date = 1970),
          silent=F)
      }
  # c.1 Load and unified the spatial and taxonomic information for the species (jump here directly) ----
    crs_gbif <- "EPSG:4326"
    GBIFrecords <- lapply("./Data/Gbif" %>% list.files(pattern=".csv",full.names = TRUE),read.csv)
    GBIFrecords <- GBIFrecords %>% rbindlist(fill=T)
  
    GBIFrecords <- st_as_sf(GBIFrecords,coords=c("decimalLongitude","decimalLatitude"),crs=crs_gbif)
    
  # d. Display all the records within the boundaries of the species ranges----
    # This is nice, but we are only interested in the area covered by the range of our species
    crop_ranges <- global %>% st_crop(st_bbox(IUCN_Lamp)) # We are going to use the bounding box of the range data to crop the global boundaries data
    
    # Create a color palette for the species ranges
    pal_species <- data.frame(species = unique(IUCN_Lamp$sci_name),sci_name = unique(IUCN_Lamp$sci_name),
                              col = viridis::turbo(n=length(unique(IUCN_Lamp$sci_name))))
    
    pal_species[pal_species$species=="Lampropeltis californiae","col"] <- "tomato3"
    
    IUCN_Lamp <- merge(IUCN_Lamp,pal_species,by="sci_name")
    GBIFrecords <- merge(GBIFrecords,pal_species,by="species")
    
  # d.2 Build the plot ----
  # d.2.a Global map ----
    # General polygon
    par(mfrow=c(1,1))
    plot(crop_ranges %>% st_geometry())
    
    # Polygon ranges
    plot(IUCN_Lamp %>% filter(sci_name == "Lampropeltis californiae") %>% st_geometry(),
         col="tomato3" %>% st_drop_geometry() %>% adjustcolor(alpha.f = 0.75),key.pos=2,
         cex.axis=0.5,key.length=0.4,add=T)
    
    plot(IUCN_Lamp["sci_name"] %>% filter(sci_name != "Lampropeltis californiae"),
         col=IUCN_Lamp$col[IUCN_Lamp$col!="tomato3"]%>%adjustcolor(alpha.f = 0.25),add=T)
    
    # Add legend to the plot
    legend("bottomleft",legend=pal_species$species,
           col=pal_species$col %>% adjustcolor(alpha.f = 0.75),
           pch=15,cex=ifelse(pal_species$species=="Lampropeltis californiae",0.75,0.5),ncol=2,
           pt.cex = 1.2,title="Species",bty="n")
    
  # d.2.b Species map ----
    # Select the polygons:
    # As we can see from the general map, the range of Lampropeltis californiae overlaps with other 
    # species of the same genus. This can lead to identification and classification errors 
    Over_ranges <- IUCN_Lamp %>% filter(sci_name!="Lampropeltis californiae")
    Sp_range <- IUCN_Lamp %>% filter(sci_name=="Lampropeltis californiae")
    
    intersect_pols <- st_intersects(Sp_range,Over_ranges,sparse = T)
    Over_ranges <- Over_ranges[intersect_pols[[1]],]
    
    # d.2.b.1 Check the spatial distribution ----
    Sp_range %>% st_geometry() %>% plot(col=Sp_range$col)
    Over_ranges %>% st_geometry() %>% plot(col=Over_ranges$col %>% adjustcolor(alpha.f = 0.5),add=T)
    plot(crop_ranges %>% st_geometry(),add=T)
    
    study_area <- rbind(Sp_range,Over_ranges)
    
    #
    legend("bottomleft",legend=study_area$sci_name %>% unique(),
           col=study_area$col %>% unique() %>% adjustcolor(alpha.f = 0.5),
           pch=15,cex=ifelse(study_area$sci_name=="Lampropeltis californiae",0.85,0.5),ncol=2,
           pt.cex = 1.2,title="Species",bty="n")

# 4. Clean the GBIF records---- 
# a. Which distribution points overlap with the species range?----
  records_range <- GBIFrecords[st_intersects(GBIFrecords,Sp_range,sparse = F),]
  all_recs <- xtabs(~records_range$species) # Number of records for each species within the species range
  sum(all_recs)
  
  # As the data shows, we have an overlap of several close related species within the range of Lampropeltis californiae.
  # This can cause erroneous identification of individuals and therefore erroneous point records. Therefore we need to filter
  # the spatial records to try to avoid these issues:
  
  # Which portions of the species range can be inhabited by a close related species?
  sub_ranges <- st_intersection(Over_ranges,Sp_range) # Crop the portions of the Species ranges that overlap with L. californiae
  
  mInt <- st_intersects(records_range,sub_ranges,sparse = F) # Which points overlap with the other species ranges
  head(mInt) # We have a column of indexes for each polygon/ sub-polygon
  mInt <- apply(mInt ,1,function(x) as.numeric(x)%>% sum()) # We are going to combine all the indexes into a single one
  mInt <- ifelse(mInt >= 1,TRUE,FALSE)
  sum(mInt) # Total number of observations withint the ranges
  
# b. However, since we don't trust the ability of herpetologist to correctly classified close-related species, we are going to add a buffer 
  # around the overlapping species ranges to exclude the points that fall into these areas.
  sub_ranges_buff <- sub_ranges %>% st_buffer(dist=0.15) %>% st_intersection(Sp_range) # Add a buffer around the overlapping areas to remove problematic records
 
  # Which distribution points are not in conflict areas (overlapping with other species)?---- 
  mInt <- st_intersects(records_range,sub_ranges_buff,sparse = F)
  mInt <- apply(mInt ,1,function(x) as.numeric(x)%>% sum()) # We are going to combine all the indexes into a single one
  mInt <- ifelse(mInt >= 1,TRUE,FALSE)
  sum(mInt)
  
  nrow(records_range)-(sum(mInt)) # Number of records within the range of the species and out of conflict areas
  clean_records <- records_range[!mInt,]
  
  # Check the spatial data
  # par(mfrow=c(1,2))
  lt<-layout(matrix(c(1,1,2,3),ncol=2,nrow=2),respect = T)
  # layout.show(lt)
  
  plot(Sp_range %>% st_geometry(),col="tomato3") ; mtext(side=3,adj=0,"a) Range of Lampropeltis californiae",cex=0.85)
  plot(sub_ranges %>% st_geometry(),col=sub_ranges$col %>% adjustcolor(alpha.f = 0.75),add=T)
  plot(sub_ranges_buff %>% st_geometry(),col=NA,border=sub_ranges$col %>% adjustcolor(alpha.f = 0.85),lty=3,add=T)
  
  legend("bottomleft",legend=sub_ranges$sci_name %>% unique(),pch=15,
         col=sub_ranges$col %>% unique() %>% adjustcolor(alpha.f = 0.75),
         title="Species Ranges",bty="n",pt.cex=1.5,cex=0.7)
  
  legend("left",legend="Buffered area",lty=3, col="tomato3" %>% adjustcolor(alpha.f = 1),bty="n",lwd=1.5,cex=0.7)

  # Plot the clean points
  plot(Sp_range %>% st_geometry(),col="grey70") ; mtext(side=3,adj=0,"b) `Clean records`",cex=0.85)
  plot(sub_ranges %>% st_union() %>% st_geometry(),border=NA,col="white" %>% adjustcolor(alpha.f = 0.65),add=T)
  
  plot(clean_records %>% st_geometry(),col=clean_records$col,add=T,pch=19,cex=0.5)
  
  # Plot the discarded points
  plot(Sp_range %>% st_geometry(),col="grey70") ; mtext(side=3,adj=0,"c) Discarded records",cex=0.85)
  plot(sub_ranges %>% st_union() %>% st_geometry(),border=NA,col="white" %>% adjustcolor(alpha.f = 0.65),add=T)
  
  plot(records_range[mInt,] %>% st_geometry(),col="black",add=T,pch=3,cex=0.2)
  
# c. Now we can trust herpetologists on correctly classifiying our snake species
  clean_records <- clean_records %>% filter(species=="Lampropeltis californiae")
  
# 5. Unified the spatial data
# Now that we have our Gbif general data and the specific data from our study, we need to make them compatible. This is 
# adjusting the their general format and the specific spatial parameters that define their spatial projection
# a. Check the spatial objects
  LifeRANGE %>% st_crs() == clean_records %>% st_crs() # CRS are different 
  
  LifeRANGE %>% st_crs()
  clean_records %>% st_crs()
    
# b. Transform the spatial information to the same CRS
  LifeRANGE <- LifeRANGE %>% st_transform(crs=crs_gbif) # We are going to use the CRS from the GBIf data
  
# c. We want a general polygon that includes both the native range of the species and the introduced area
  full_range <- c(IUCN_Lamp %>% st_geometry(),LifeRANGE %>% st_geometry())
  glob_lamp <- global %>% st_crop(st_bbox(full_range))      
  
  # Lets put all together
  par(mfrow=c(1,1))
  
  glob_lamp %>% st_geometry() %>% plot()
  Sp_range %>% st_geometry() %>% plot(col="tomato3",add=T)
  LifeRANGE %>% filter(Island=="Gran_Canaria") %>% st_geometry() %>% plot(col="tomato3",add=T)
  
  # Add some labels
  invasive_point <- LifeRANGE %>% filter(Island=="Gran_Canaria") %>% st_centroid() %>% st_coordinates()
  native_point <- Sp_range %>% st_centroid() %>% st_coordinates()  

  arrows(x0=invasive_point[1],y0=invasive_point[2]+1,x1=invasive_point[1]-5,y1=invasive_point[2]+5,lwd=2,code=0,col="tomato3")
  text(x=invasive_point[1]-5,y=invasive_point[2]+7,"Invasive range",col="tomato3")
  
  arrows(x0=native_point[1],y0=native_point[2]+1,x1=native_point[1]+7,y1=native_point[2]+7,lwd=2,code=0,col="tomato3")
  text(x=native_point[1]+8,y=native_point[2]+9,"Native range",col="tomato3")
   
  # Wouldn't be cool to calculate the distance between the  native and the invasive range?
  distance_sites <- st_distance(Sp_range %>% st_centroid(),
                                LifeRANGE %>% filter(Island=="Gran_Canaria") %>% st_centroid())
  units(distance_sites)<-"km"
  
  arrows(x0=native_point[1],y0=native_point[2],x1=invasive_point[1],
         y1=invasive_point[2],lwd=2,code=0,col="grey10",lty=2)
  
  text(x=mean(c(native_point[1],invasive_point[1])),y=mean(c(native_point[2],invasive_point[2]))+2, 
       label=paste(distance_sites %>% round(digits=2),"km"),srt=-3,font=2)
  
# 6. Export the spatial data
  # All our spatial information is following the same format and spatial projection
  exit_dir <- "./Data/Process_data" ; exit_dir %>% dir.create(recursive = T,showWarnings = F)
  
  # As with any other R object, if we want to preserve the processed spatial objects we need to export/ save them
  # General polygons
  paste(exit_dir,"General",sep="/") %>% dir.create(recursive = T,showWarnings = F)
  
  st_write(IUCN_Lamp,paste(exit_dir,"General",paste0("Lampropeltis_RANGES",".shp"),sep="/"),append=FALSE) # IUCN ranges for Lampropeltis Genus
  st_write(Sp_range,paste(exit_dir,"General",paste0("LampCaliforniaeRang",".shp"),sep="/"),append=FALSE) # Range of L. californiae
  st_write(glob_lamp,paste(exit_dir,"General",paste0("Glob_Dist",".shp"),sep="/"),append=FALSE) # Lampropeltis ranges
  st_write(global,paste(exit_dir,"General",paste0("Glob_map",".shp"),sep="/"),append=FALSE) # General Global polygon
  
  # Clean records
  paste(exit_dir,"Records",sep="/") %>% dir.create(recursive = T,showWarnings = F)
  
  st_write(clean_records,paste(exit_dir,"Records",paste0("Gbif_rec",".gpkg"),sep="/"),append=FALSE) # Gbif Clean Records
  st_write(PointsLife,paste(exit_dir,"Records",paste0("Study_rec",".shp"),sep="/"),append=FALSE) # Study Clean Records
  
#  
# End of the script
#  
