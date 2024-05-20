#Read the fish survey data in
setwd("C:/Users/bensc/Downloads/Ben UCT/GIT folder/Marine-Spatial-PLanning")
FishSurvey <- read.csv("EstuaryFishSurveyData.csv")

#Load packages
library(vegan)
library(tidyverse)

#Now get alpha diversity for each estuary 
FishSurvey[is.na(FishSurvey)] = 0
head(FishSurvey)

#Species richness: we will just tally up all the rows with a non-zero value

Alpha <- specnumber(FishSurvey[,4:148])
Alpha

FishSurvey$Alpha <- Alpha

#Now we have alpha diversity for each estuary

#Shall we try Shannon diversity? 

Shannon <- diversity(FishSurvey[,4:148])
Shannon

FishSurvey$Shannon <- Shannon
