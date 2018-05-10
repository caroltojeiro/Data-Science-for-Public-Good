setwd("D:/ Project - 600") #Set your Working Directory

#loading merged file of Group A and Group B containing the block level addresses
#Note: The duplicate address values need to be removed before loading the data here.ny duplicate values
datamerged <- read.csv("data_merged.csv")
View(datamerged)

#adding latitude and longitude to the block level addresses

library(ggmap)

datamerged$Block.Address <- as.character(datamerged[,1]) #converting address data type to character

#Obtaining the corresponding latitude and longitude for the block level addresses of Group A & B merged dataset
for(i in 1:nrow(datamerged))
{
  lonlat_sample <- as.numeric(geocode(datamerged[i,1]))  #row containing the addresses. Note: Ensure the addresses are of the format: "xxx,Syracuse,NY". Else similar longitude latitude of different countries may get assigned
  datamerged[i,2] <- lonlat_sample[1] #assigning longitude co-ordinates
  datamerged[i,3] <- lonlat_sample[2] #assiging latitude co-ordinates
}

#calculating distance between two address points
#install.packages("spatstat")

write.csv(datamerged_NA,file=paste("datamerged.csv"))

###################################################################################################################

#loading the above obtained dataset containing the block level addresses(without duplicates), along with their corresponding latitude and longitude values
Data_AB <- read.csv("datamerged.csv") #Note: for consistency, the latitude and longitude have been switched. i.e. Latitude is in column 2 now and longitude is in column 3
View(Data_AB)

#loading group C data, containg only the latitude and longitude 
Data_C <- read.csv("GroupC_Data.csv")
View(Data_C)

#Creating 4 new columns to store the results
Data_C$Lat_AB <- 0
Data_C$Long_AB <- 0
Data_C$Min_len <- 0
Data_C$Address <- " "

str(Data_AB)

#converting factor to character data type
Data_AB$Block_Address <- as.character(Data_AB$Block_Address)

library(spatstat) #library containg the crossdist function to calculate euclidean distance

for(i in 1:nrow(Data_C))
{
  for(j in 1:nrow(Data_AB))
  {
    dist <- crossdist(Data_C[i,1],Data_C[i,2],Data_AB[j,2],Data_AB[j,3]) #function to calculate euclidean distance between latitude and longitude of Group C and lat, long of Group AB
    
    if(j==1)
    {
      min_len <- dist
      Data_C[i,3] <- Data_AB[j,2] #assiging latitude of GroupAB whose distance from Group C latitude is minimum
      Data_C[i,4] <- Data_AB[j,3] #assiging longitude of GroupAB whose distance from Group C longitude is minimum
      Data_C[i,5] <- min_len
      Data_C[i,6] <- Data_AB[j,1] #assiging block address of GroupAB for the corresponding lat,long
    }
    
    if(dist < min_len)
    {
      
      min_len <- dist
      Data_C[i,3] <- Data_AB[j,2] #assiging latitude of GroupAB whose distance from Group C latitude is minimum
      Data_C[i,4] <- Data_AB[j,3] #assiging longitude of GroupAB whose distance from Group C longitude is minimum
      Data_C[i,5] <- min_len
      Data_C[i,6] <- Data_AB[j,1] #assiging block address of GroupAB for the corresponding lat,long
    }
    
  }
}

#saving output file
write.csv(Data_C, file=paste("CrossDist.csv"))
