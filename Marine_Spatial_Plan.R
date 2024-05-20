#Read the fish survey data in
setwd("C:/Users/bensc/Downloads/Ben UCT/GIT folder/Marine-Spatial-PLanning")
FishSurvey <- read.csv("EstuaryFishSurveyData.csv")

#Load packages
library(vegan)
library(tidyverse)
library(ggplot2)

#Now get alpha diversity for each estuary 
FishSurvey[is.na(FishSurvey)] = 0
head(FishSurvey)

#Species richness: we will just tally up all the rows with a non-zero value

Alpha <- specnumber(FishSurvey[,4:148])
Alpha

FishSurvey$Alpha <- Alpha

#Let's order by the 20 most diverse estuaries: 
FishSurvey <- FishSurvey[order(FishSurvey$BZ, -FishSurvey$Alpha),]
FishSurvey[1:20,1]
#This gives us the estuaries with the highest species counts 
#Mlalazi is the most diverse 

Alphalist <- FishSurvey[1:20,1]

#Let's plot this though 

FishSurvey$kmEast <- (2947 - FishSurvey$kmWest)
ggplot(FishSurvey, aes(kmEast,Alpha)) + 
  geom_point()


ggplot(FishSurvey, aes(kmEast,Alpha)) + 
   geom_point(aes(colour = cut(Alpha, c(0,34,60))),
              size = 2) +
   scale_color_manual(name = "Alpha",
                      values = c("(0,34]" = "blue",
                                 "(34,60]" = "red"),
                      labels = c("< 34 species","Top 20"))

#Now let's try Colin's way: 
FishSurvey$Alphalist <- 0
FishSurvey$Alphalist[1:20] <- 1
#I don't know man, I think cutting the data is easier and more flexible 

#Now we have alpha diversity for each estuary

#How many species occur in these top 20 estuaries? 
specnumber(FishSurvey[4:148], FishSurvey$Alphalist)
0   1 
121 116 
#So the top 20 most diverse estuaries have 116 species protected whereas the others have 121 protected in total. So this indicates that if we were to protect these species we would be sorted for 116 species. That is 80% of all the estuarine species.  
116/145


#So the summed alpha is 760 for the 20 most diverse estuaries, but those will be many recounts of the same species 


#Now we should try and find the most biodiverse estuaries in each biogeographic zone 

#Let's subset for each zone: one for east, south and west 
#Find the most diverse East Coast Estuaries

FishSurvey$AlphaBZ <- 0
FishSurvey$AlphaBZ[1:7] <- 1
FishSurvey$AlphaBZ[90:96] <- 1
FishSurvey$AlphaBZ[206:211] <- 1



#Now I have selected seven estuaries from the east and south coasts, and six from the west coast. How many species have I protected? 

specnumber(FishSurvey[4:148], FishSurvey$AlphaBZ)
0   1 
123 119

#I've had diminishing returns it seems, only three more species are protected


#Shall we try Shannon diversity? 

Shannon <- diversity(FishSurvey[,4:148])
Shannon

FishSurvey$Shannon <- Shannon

#Now let's try for some Beta-diversity: just for the hell of it 

Simpson <- diversity(FishSurvey[,4:148],"simpson")
Simpson

FishSurvey$Simpson <- Simpson

#Now for Simpson's diversity, a low number = high diversity but a high number = low diversity, so should I invert it by subtracting it from 1? 

