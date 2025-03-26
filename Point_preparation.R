rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # Set the working directory to the directory in which the code is stored
td <- tempdir()
#
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

# 1. Lampropeltis data ----
# The data is included in a PDF file with 4 variables corresponding to their UTM coordinates, type or record and date of recording
text <- pdf_text("./Data/Life+ Lampropeltis" %>% list.files(pattern = ".pdf$",full.names = TRUE))
text[1] # The data is organized in 4 columns

# Function to format the decimal points
insert_point <- function(string, n) {
  if (n >= nchar(string)) {
    return(string)  # Return the original string if n is greater than string length
  }
  yP <- paste0(substring(string, 1, n), ".", substring(string, n + 1))
  return(yP)
  
  }

# 1.b Format the data ---- 
mPDF <- list()

for(i in 1:length(text)){
  # Load the sheet of the PDF
    xt <- text[i]
  
  # Create the matrix with the records
    xtlist <- xt %>% strsplit(split = ";") %>% unlist() %>% strsplit(split="\n") %>% unlist()
    m <- matrix(xtlist,ncol=4,byrow=T)
    
  # Format the different variables
    for(k in 1:nrow(m)){
      # Remove unwanted white spaces and punctuations
      while(m[k,3]%>%grepl(pattern = " ")){
        m[k,3] <- m[k,3] %>% gsub(pattern=" ",replacement="")
      }
      
      while(m[k,4]%>%grepl(pattern = " ")){
        m[k,4] <- m[k,4] %>% gsub(pattern=" ",replacement="")
      }
      
      # Remove unwanted punctuations
      m[k,3] <- m[k,3] %>% gsub(pattern="[[:punct:]]",replacement="")
      m[k,4] <- m[k,4] %>% gsub(pattern="[[:punct:]]",replacement="")
      
      # Format the decimals in the UTM coordinates (X coordinate has 6 numbers while the Y coordinates has 7)
      m[k,3] <- insert_point(m[k,3],n=6)
      m[k,4] <- insert_point(m[k,4],n=7)
    
      # print(k)
      }
    
    mPDF[[i]] <- m
    print(i)
}

# 1.c Combine the data, final format and exporting ----
data <- do.call("rbind",mPDF)
col.n <- data[1,]

# Remove some wrong formatting records
data<-data[-c(23137:23168),]

# Create a data.frame
data <- data[-1,]
data <- as.data.frame(data)

names(data) <- col.n # Include the variables names
str(data)

# 2. Format the variables ----
data$`Fecha hora contacto` <- as.Date(data$`Fecha hora contacto`,format="%d/%m/%Y %H:%M")
data$Coorde.nadaX <- data$Coorde.nadaX %>% as.numeric()
data$coorden.adaY <- data$coorden.adaY %>% as.numeric()

# 3. Export the information ----
data %>% write.csv(paste("./Data/Life+ Lampropeltis","Raw_records.csv",sep="/"),row.names = F)
data <- read.csv(paste("./Data/Life+ Lampropeltis","Raw_records.csv",sep="/"))

str(data)

# 4. Check the information ----
plot(data$Coorde.nadaX,data$coorden.adaY)

#~~~~~~~~~~~~~~~~~~#
# End of the script
#~~~~~~~~~~~~~~~~~~#