#
# /\/\/\/\/\/\/\/\////////\//\/\/\//\\\\\/\/\/\/\/\/\///\/\/\/\
# Function to download the information from the IUCN Red List
# /\/\/\/\/\/\/\/\////////\//\/\/\//\\\\\/\/\/\/\/\/\///\/\/\/\
#
IUCN_red_List<-function(x, # scientific name of the species
                        habitat_info=TRUE, # [character] does the IUCN species habitat information need to be downloaded
                        threats_info=TRUE, # [character] does the IUCN species threats information need to be downloaded
                        export=TRUE, # [Character] Should results be exported or stored on the working environment?
                        exit_route=getwd(), # Exit route, if export=TRUE
                        IUCN_api_token=NULL # API token to access the IUCN servers. Is better to set up this using options(iucn_redlist_key=IUCN_api_token) 
                                            # if no API is provided the function will assume is configure in the options. If an API is provided, the function
                                            # will configure the otherwise this is added to the session options. Notted that rredlist only supports API V.3 at 
                                            # the moment. More information here https://apiv3.iucnredlist.org/
                        ){
  
  # Needed packages ------------------------------------------------------------#
  if(!is.null(IUCN_api_token)){
    options(iucn_redlist_key=IUCN_api_token) # API 3
    } 
  
  list.of.packages<-c("tidyr","rredlist","data.table","stringr")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  lapply(list.of.packages,require,character.only=TRUE)
  rm(list.of.packages,new.packages)
  #-----------------------------------------------------------------------------#
  
  # Name of the specie
    print(x)
    sp_long <- strsplit(x,split=" ") %>% unlist()
  
  # Get the IUICN information
    b<-NULL         # This piece of code jump around the error 
    attempt <- 1    # and try to retrieve the information a "n" number of attemps
    
    while( is.null(b) && attempt <= 20 ) {
      try(b <- rl_species(genus=sp_long[1],species=sp_long[2],silent=TRUE),silent=T)
      attempt <- attempt + 1
    }
  
  # Species information
    tax <- b$taxon
    tax <- do.call("cbind",tax[!names(tax) %in% c("common_names","synonyms","description","ssc_groups")])  
  
  # Get the species habitat data
    if(habitat_info==TRUE){
      c<-NULL                                     
      attempt2 <- 1 
      
      while( is.null(c) && attempt2 <= 20 ) {
        
        try(c <- rl_assessment(id=b$assessments$assessment_id[length(b$assessments$assessment_id)],parse=TRUE)$habitats)
        attempt2 <- attempt2 + 1
      }
      c$sci_name<-x
    }
    
    c <- c %>% as.matrix() %>% as.data.frame()
  
  # Get the species threats
    if(threats_info==TRUE){
      d<-NULL                                     
      attempt2 <- 1 
      
      while( is.null(c) && attempt2 <= 20 ) {
        
        try(d <- rl_assessment(id=b$assessments$assessment_id[length(b$assessments$assessment_id)],parse=TRUE)$threats)
        attempt2 <- attempt2 + 1
      }
      d$sci_name<-x
    }
  
  # Export the results as individual files or save it into memory for future mergin
    if(is.null(b)){
      return(paste("No information for",x,":( !") %>% print())
    
      }else{
    if(export==TRUE){
      dir.create(paste(exit_route,"sp_info",sep="/"),showWarnings = FALSE)
      fwrite(tax,paste(exit_route,"sp_info",paste0(x,".csv"),sep="/"))
      
      if(habitat_info==TRUE){
        dir.create(paste(exit_route,"Threats_specie",sep="/"),showWarnings = FALSE)
        fwrite(c,paste(exit_route,"Threats_specie",paste0("T_",x,".csv"),sep="/"))
        }
      
      if(threats_info==TRUE){
        dir.create(paste(exit_route,"Habitat_specie",sep="/"),showWarnings = FALSE)
        fwrite(d,paste(exit_route,"Habitat_specie",paste0("H_",x,".csv"),sep="/"))
        }
      
      }else{
        # a
          if(habitat_info==FALSE & threats_info==FALSE){
            return(tax)
          }
        
        # b
          if(habitat_info==TRUE & threats_info==TRUE){
            return(list(sp_info=tax,Threats=d,Habitats=c))
          }
        
        #c
          if(habitat_info==FALSE & threats_info==TRUE){
            return(list(sp_info=tax,Threats=d))
          }
        
        # d
          if(habitat_info==TRUE & threats_info==FALSE){
            return(list(sp_info=tax,Habitats=c))
          }
        }
    }
   print("Something is happening here!")
  }

#
# End of the function
#

