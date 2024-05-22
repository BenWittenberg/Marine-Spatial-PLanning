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
FishSurvey <- FishSurvey[order(FishSurvey$BZ,-FishSurvey$Alpha),]
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
FishSurvey$AlphaBZ[91:97] <- 1
FishSurvey$AlphaBZ[207:212] <- 1
FishSurvey$AlphaBZ <- as.character(FishSurvey$AlphaBZ)


#Now I have selected seven estuaries from the east and south coasts, and six from the west coast. How many species have I protected? 

specnumber(FishSurvey[4:148], FishSurvey$AlphaBZ)
0   1 
120 122

#I've had diminishing returns it seems, only four more species are protected

#So now I can plot it again: 

ggplot(FishSurvey, aes(kmEast,Alpha)) + 
  geom_point(aes(colour = AlphaBZ)) +
  scale_color_manual(name = "AlphaBZ",
                     values = c("0" = "blue",
                                "1" = "red"),
                      labels = c("Unprotected","Protected"))

#Now we need a list of the most diverse estuaries from each bioregion 
AlphaBZlist <- FishSurvey[FishSurvey$AlphaBZ == "1",1]
AlphaBZlist

#What's the problem of choosing the most diverse estuaries? Well it's probably in an overlap zone between multiple biogeographic zones 
#So we may be preserving marginal habitat rather than core habitat which is poor conservation
#One way to circumvent this is to include an abundance index to account for their core habitats. 
#So we need to calculate geographic range and the centre of their distribution 

ggplot(FishSurvey, aes(kmEast,Dusky.kob)) + 
  geom_point(aes(colour = AlphaBZ)) +
  scale_color_manual(name = "AlphaBZ",
                     values = c("0" = "blue",
                                "1" = "red"),
                     labels = c("Unprotected","Protected"))
#Let's determine the eastern and western boundaries of each species 

WL <- array(0,c(145))
WL
#We've made a blank vector which we will eventually fill

EL <- array(0,c(145))
EL
#Another blank vector of length 145

range <- array(0,c(145))
range
#Another vector for range 

ab <- array(0,c(145))
ab

centre <- array(0,c(145))
centre

max <- 0
#And a vector of abundance 
#Now I need to order by kmEast
ED <- FishSurvey[order(FishSurvey$kmWest),]
view(ED)
#So now we should run a loop to speed things up
for(i in 1:145){ #species loop
  for(j in 1:232){ #estuaries loop
    if(ED[j,i + 3] > 0) {EL[i] <- ED[j,2]} 
  }

  for(j in 232:1){ #estuaries loop
    if(ED[j,i + 3] > 0) {WL[i] <- ED[j,2]}
  }
}
  for(k in 1:145){
    range[i] <- (EL[i]-WL[i])
  }
  for(i in 1:145){#species loop
    max <- 0
    for(j in 1:232){ #estuary loop
      if(ED[j,i+3]>max){
        centre[i] <- ED[j,2]
        max <- ED[j,i+3]} 
    }
  }
  

    


#Now let's check what we've put in our Eastern Limit vector
Wlim <- EL 
Elim <- WL
#I did this because my EL was actually my Western Limit and vice versa 
#Seems to have worked
range

#Now we need to calculate the centre: 
centre

#Now can I plot a frequency histogram of centre ranges 
hist(centre, breaks = 232)
?hist
#Noticeable right-skew 
#And I need to know which 20 estuaries have the greatest frequency 

#How to list the estuaries with the most modes? 

levels(as.factor(centre))
mode_table <- table(as.factor(centre))

#Hey that seems to have worked! 
class(mode_table)
#Now I coerce to a data frame so I can roder it again
mode_df <- data.frame(rbind(mode_table))
class(mode_df)
View(mode_df)


library(tidyverse)
mode_df %>%
  pivot_longer(cols = starts_with("X"), names_to = "kmEast", values_to = "Mode frequency") -> mode_df
#Now I've made it a long dataset and saved them all as categorical variables 

mode_df <- mode_df[order(-mode_df$`Mode frequency`),]
#Now we have ordered our list so that we have the estuaries that represent the mode of the most species' distributions on top. I.e. the estuary at 267kmEast is the most abundant estuary for the most species (12 species have their peaks here). 

#Now we need a new variable called Modelist (and I'll do this to the FishSurvey and ED dataframes)



#so if we want the top twenty estuaries all we'd have to do is pick 

mod_est <- mode_df[1:20,]
#But I think this will quickly clog up my workspace 

#I have successfully listened to Colin and screwed up as a result 
#Ed is the untransformed data 
## add column modelist (top 20 estuaries by middle of ranges)
ED$modelist <- 0
# List of locations to set modelist to 1
locations <- c("St Lucia", "Mlalazi", "Matigulu/Nyoni", "Knysna", "Kariega", 
               "Bushmans", "Kosi", "Mfolozi/Msunduzi", "Manzimtoti", "Mkomazi", 
               "Kwelera", "Orange", "Mtentu", "Mngazana", "Ngqusi/Inxaxo", 
               "Great Kei", "Great Fish", "Zinkwasi", "Mzamba", "Msikaba")

ED <- ED %>%
  mutate(modelist = ifelse(ED$Estuary %in% locations, 1, 0))
ED$modelist <- as.character(ED$modelist)

#Now we plot this bad boy 
ggplot(ED, aes(kmEast,Alpha)) + 
  geom_point(aes(colour = modelist)) +
  scale_color_manual(name = "Modelist",
                     values = c("0" = "blue",
                                "1" = "red"),
                     labels = c("Unprotected","Protected"))
#Okay so there are 68 estuaries that are the most abundant for at least one species 

#Now I should figure out how many species are protected in this new form
specnumber(ED[4:148], ED$modelist)
0   1 
107 127
#So we're protecting quite a few more using this method 
127/145
#About 88% of all the fish 


#Now we should do an ordination to determine species composition 
#We'll do this using a dendrogram 
#We've loaded vegan with library(vegan) 

ED <- subset(ED, Alpha > 0)
#This removes all the estuaries where there are now species at all
nrow(ED)

ED_deco <- decostand(ED[,c(4:148)], method = "total")
#This noramlises the data by the total so that we have the proportions of the species for each estuary so we have comparable data rather than raw numbers 

#Next step: we need to use Bray-Curtis to calculate dissimilarity 

ED_dist <- vegdist(ED_deco,method = "bray")
#We're calculating Bray-Curtis dissimilarity 

ED_cluster <- hclust(ED_dist, method = "average")
plot(ED_cluster, hang = -1, ylab = "Dissimilarity")

slice <- cutree(ED_cluster, h = 0.8) #this is the dissimilarity value at which we're cutting
slice
#Seems we've divided this into ten groups - we woudl then have two protected estuaries per group

#Now we put these into the original data frame 

ED$cut80 <- slice

ED$cut80
#This has classed each estuary into different community types, and there are ten types (although there is only once example of 10, what's the bet it's the Orange river)

#Now we need to order our data by cut80 and then by Alpha 
ED <- ED[order(ED$cut80,-ED$Alpha),]

cutlist <- c("Kosi","Mngazana","Kwelera","Mlalazi","Matigulu/Nyoni","Mzimkulu","Bushmans","Great Kei","Nenga","Bakens","Kaaimans","Slang","Kaapsedrif","Olifants","Diep","Storms","Elsies","St Lucia","Orange","Kariega")
#I've also randomly added some others that do not occur in the cutlist but do coincide in the AlphaBZ list and the Modelist (the Orange, St Lucia and Kariega)
#I have also exchanged the Bree river for the diep river because that appears in the AlphaBZ list
ED[ED$cut80 == "8",1]
AlphaBZlist
ED[ED$modelist == "1",1]

library(dplyr)
library(tidyverse)
ED <- ED %>%
  mutate(cutlist = ifelse(ED$Estuary %in% cutlist, 1, 0))
ED$cutlist <- as.character(ED$cutlist)


#So how many species are we protecting? 
specnumber(ED[4:148], ED$cutlist)
0   1 
123 112
#Not doing so good 
#Aaaand suddenly my code is bugged out, why? It was literally working a few seconds ago 


#Now the issue is we are protecting too many of the same species 
Fish <- ED
Fish2 <- ED

Fish$complist <- 0


Fish[1,154] <- 1
#Now we're going to make a loop for this 
for(j in 1:20){
  Fish <- Fish[order(-Fish$Alpha),]
  Fish$complist[1] <- 1
  for(i in 1:145){
  if(Fish[1,i + 3]>0){Fish[,i + 3]<- 0}
  
  }
  Fish$Alpha <- specnumber(Fish[,4:148])
}
#This is basically ensuring that we preserve as great a variety of species as possible: it makes sure we aren't protecting the same species over and over again 
Fish$complist

Fish <- Fish[order(-Fish$complist),]
Fish$kmEast <- (2947 - Fish$kmWest)
Fish$complist <- as.character(Fish$complist)

ED <- ED[order(ED$kmWest),]
Fish <- Fish[order(Fish$kmWest),]
ED$complist <- Fish$complist

ED$kmEast <- (2947 - ED$kmWest)

ggplot(ED, aes(kmEast,Alpha)) + 
  geom_point(aes(colour = complist)) +
  scale_color_manual(name = "Turnover",
                     values = c("0" = "blue",
                                "1" = "red"),
                     labels = c("Unprotected","Protected"))

specnumber(ED[4:148], ED$complist)
0   1 
109 135
#This is the best method so far 
135/145
#We're protecting 93% of the fish species 
#But the problem is we are probably still protecting marginal habitat 

complist <- ED[ED$complist == "1",1] 
complist

#I'm curious as to how many estuaries we would need to protect to have full (145 species) protection


#What is the abundance for each species? 
# ab
# #Now I have a vector of abundances 
# 
# #That does not look right because each species should have a non-zero total 
# 
#Shall we try Shannon diversity? 
# 
Shannon <- diversity(ED[,4:148])
Shannon
 
ED$Shannon <- Shannon

ggplot(ED, aes(kmEast,Shannon)) + 
  geom_point(aes(colour = complist)) +
  scale_color_manual(name = "Turnover",
                     values = c("0" = "blue",
                                "1" = "red"),
                     labels = c("Unprotected","Protected"))



# 
#Now let's try for some Beta-diversity: just for the hell of it 
# 
Simpson <- diversity(ED[,4:148],"simpson")
Simpson
# 
ED$Simpson <- Simpson
ED$Invert.Simpson <- 1- Simpson

ggplot(ED, aes(kmEast,Simpson)) + 
  geom_point(aes(colour = complist)) +
  scale_color_manual(name = "Turnover",
                     values = c("0" = "blue",
                                "1" = "red"),
                     labels = c("Unprotected","Protected"))
# #Now for Simpson's diversity, a low number = high diversity but a high number = low diversity, so should I invert it by subtracting it from 1? 

