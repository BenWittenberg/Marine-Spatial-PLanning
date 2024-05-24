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
122/145
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
# ED <- ED %>%
#   mutate(cutlist = ifelse(ED$Estuary %in% cutlist, 1, 0))
# ED$cutlist <- as.character(ED$cutlist)

#Okay let's do this manually with the altered cutlist 




ED[ED$Estuary == "Kosi",153] <- 1
ED[ED$Estuary == "Mngazana",153] <- 1
ED[ED$Estuary == "Kwelera",153] <- 1
ED[ED$Estuary == "Mlalazi",153] <- 1
ED[ED$Estuary == "Matigulu/Nyoni",153] <- 1
ED[ED$Estuary == "Mzimkulu",153] <- 1
ED[ED$Estuary == "Bushmans",153] <- 1
ED[ED$Estuary == "Great Kei",153] <- 1
ED[ED$Estuary == "Nenga",153] <- 1
ED[ED$Estuary == "Bakens",153] <- 1
ED[ED$Estuary == "Kaaimans",153] <- 1
ED[ED$Estuary == "Slang",153] <- 1
ED[ED$Estuary == "Kaapsedrif",153] <- 1
ED[ED$Estuary == "Olifants",153] <- 1
ED[ED$Estuary == "Diep",153] <- 1
ED[ED$Estuary == "Storms",153] <- 1
ED[ED$Estuary == "Elsies",153] <- 1
ED[ED$Estuary == "St Lucia",153] <- 1
ED[ED$Estuary == "Orange",153] <- 1
ED[ED$Estuary == "Kariega",153] <- 1

#Make it a character
ED$cut80 <- as.character(ED$cut80)

#So how many species are we protecting? 
library(vegan)
specnumber(ED[4:148], ED$cutlist)
0   1 
121 110 
#Not doing so good 
110/145
145*0.78
#Is there a regional bias in the community divisions?
ggplot(ED, aes(kmEast,Alpha)) + 
  geom_point(aes(colour = cut80)) 




lm <- lm(cut80 ~ kmEast, data = ED) 
summary(lm)

#As it goes Westwards the number of community zones decreases 




#Now the issue is we are protecting too many of the same species 
Fish <- ED
Fish2 <- ED

Fish$complist <- 0


Fish[1,156] <- 1
#Now we're going to make a loop for this 
for(j in 1:40){
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

#Let's see how many estuaries we need to protect to boost the diversity count 
specnumber(ED[4:148], ED$complist)
0   1 
99 145

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
  scale_color_manual(name = "Complementary",
                     values = c("0" = "blue",
                                "1" = "red"),
                     labels = c("Unprotected","Protected"))
# #Now for Simpson's diversity, a low number = high diversity but a high number = low diversity, so should I invert it by subtracting it from 1? 


#Now let's read in the list of protected estuaries 
Protected <- read.csv("Protected_estuaries.csv")

Protected

#Now make a new variable in the ED dataframe 

ED$current <- 0

ED <- ED %>%
   mutate(current = ifelse(ED$Estuary %in% Protected[,1], 1, 0))
 ED$current <- as.character(ED$current)
 
#ED2 is the untampered data at this point 
 
ED2$alpha <- specnumber[,4:148]

Alpha <- specnumber(ED2[,4:148])
Alpha

ED2$Alpha <- Alpha

complist

127/145
 
#Now plot this: 
 ggplot(ED, aes(kmEast,Alpha)) + 
   geom_point(aes(colour = current)) +
   scale_color_manual(name = "Current status",
                      values = c("0" = "blue",
                                 "1" = "red"),
                      labels = c("Unprotected","Protected"))
 
#So how many species are now protected? 

specnumber(ED[4:148], ED$current)
0   1 
127 106

#So at the moment we're doing pretty poorly (only protecting 73%)
106/145

#Let's try the brute force method: basically a random selection with a loooot of permutations


ED2 <- read.csv("EstuaryFishSurveyData.csv")

ED3
# ED2$random <- 0
# ED2$dummy <- runif(n=232, min=1, max=232)

#Make a column for Alpha 
Alpha <- specnumber(ED2[,4:148])
Alpha

ED2$Alpha <- Alpha

randlist <- array(0,c(20))
randlist <- as.character(randlist)
class(randlist)
div <- 0
MaxSpp <- 0 
for(j in 1:1000000){
  div = 0
  ED3 <- ED2[sample(1:nrow(ED2),20,replace = FALSE),]
  for(i in 1:145){
    if(sum(ED3[,i+3])>0){div <- div + 1}
  }
    if(div > MaxSpp){
      MaxSpp <- div
      randlist <- ED3[1:20,1]
    }
    }
  MaxSpp/145 
  randlist
    
#First permutation protects 58%
  #Permutation of 100 000 protects 75% 
  #Permutation of 500 000 protects only 76%
  #Permutation of 1 mill prpotected 78%
  #Now the best randlist will be the randlist with the highest number on the end 
Randlist4 <- randlist



 
#After this I'm going to use the estuaries that occur in the highest number of lists (top 20 that occur in the most lists)
#Then add five to the list of existing MPAs, to best maximize the 


#Let me see how many estuaries are in each Biogeographic zone #Remember ED2 is the untransformed data
ED2[ED2$BZ == "W",1]
#26 in the Western region
#What proportion are we protecting? 
6/26
#23%
ED2[ED2$BZ == "S",1]
#116 in the South
7/116
#Only 6%
ED2[ED2$BZ == "E",1]
#90 in the East
7/90
#Only 8%
127/145


#Let's plot the data from the brute force method 

ED2$brute <- 0

ED2 <- ED2 %>%
  mutate(brute = ifelse(ED2$Estuary %in% Randlist4, 1, 0))
ED2$brute <- as.character(ED2$brute)

#Now plot this 
ED2$kmEast <- (2947 - ED2$kmWest)
ggplot(ED2, aes(kmEast,Alpha)) + 
  geom_point(aes(colour = brute)) +
  scale_color_manual(name = "Random",
                     values = c("0" = "blue",
                                "1" = "red"),
                     labels = c("Unprotected","Protected"))


#Touching up section

#Plot based off alpha only
FishSurvey <- read.csv("EstuaryFishSurveyData.csv")
FishSurvey[is.na(FishSurvey)] = 0
FishSurvey$Alpha <- specnumber(FishSurvey[,4:145])

FishSurvey <- FishSurvey[order(-FishSurvey$Alpha),]

FishSurvey$kmEast <- (2947 - FishSurvey$kmWest)
Alphalist <- FishSurvey[1:20,1]
Alphalist

FishSurvey$Alphalist <- 0

FishSurvey$Alphalist[1:20] <- 1

FishSurvey <- FishSurvey %>%
  mutate(Alphalist = ifelse(FishSurvey$Estuary %in% Alphalist, 1, 0))
FishSurvey$Alphalist <- as.character(FishSurvey$Alphalist)



ggplot(FishSurvey, aes(kmEast,Alpha)) + 
  geom_point(aes(colour = Alphalist)) +
  scale_color_manual(name = "Alpha list",
                     values = c("0" = "blue",
                                "1" = "red"),
                     labels = c("Unprotected","Protected"))

#Now I need to replot my cutlist 
cutlist
FishSurvey$cutlist <- 0

# FishSurvey <- FishSurvey %>%
#   mutate(cutlist = ifelse(FishSurvey$Estuary %in% cutlist, 1, 0))
# FishSurvey$cutlist <- as.character(FishSurvey$cutlist)

#Super weird that it doesn't do this, but only with the cutlist 

ED <- FishSurvey

ED[ED$Estuary == "Kosi",152] <- 1
ED[ED$Estuary == "Mngazana",152] <- 1
ED[ED$Estuary == "Kwelera",152] <- 1
ED[ED$Estuary == "Mlalazi",152] <- 1
ED[ED$Estuary == "Matigulu/Nyoni",152] <- 1
ED[ED$Estuary == "Mzimkulu",152] <- 1
ED[ED$Estuary == "Bushmans",152] <- 1
ED[ED$Estuary == "Great Kei",152] <- 1
ED[ED$Estuary == "Nenga",152] <- 1
ED[ED$Estuary == "Bakens",152] <- 1
ED[ED$Estuary == "Kaaimans",152] <- 1
ED[ED$Estuary == "Slang",152] <- 1
ED[ED$Estuary == "Kaapsedrif",152] <- 1
ED[ED$Estuary == "Olifants",152] <- 1
ED[ED$Estuary == "Diep",152] <- 1
ED[ED$Estuary == "Storms",152] <- 1
ED[ED$Estuary == "Elsies",152] <- 1
ED[ED$Estuary == "St Lucia",152] <- 1
ED[ED$Estuary == "Orange",152] <- 1
ED[ED$Estuary == "Kariega",152] <- 1
ED$cutlist <- as.character(ED$cutlist)




#Now plot this bad boiii
ggplot(ED, aes(kmEast,Alpha)) + 
  geom_point(aes(colour = randlist)) +
  scale_color_manual(name = "Brute force",
                     values = c("0" = "blue",
                                "1" = "red"),
                     labels = c("Unprotected","Protected")) +
  labs(x = "Km East", y = "Alpha diversity")+
  theme(
    panel.grid.major = element_blank(),              # Remove major gridlines
    panel.grid.minor = element_blank(),              # Remove minor gridlines
    panel.background = element_rect(fill = "white"), # Set panel background to white
    plot.background = element_rect(fill = "white"),  # Set plot background to white
    plot.title = element_blank(),                    # Remove plot title
    axis.ticks.length = unit(-0.2, "cm"),            # Make tick marks on the inside
    axis.ticks = element_line(color = "black"),      # Ensure ticks are visible
    axis.line = element_line(color = "black"))       # Add axis lines

#Now I'm doing for alphaBZ
ED$AlphaBZlist <- 0

AlphaBZlist

# ED <- ED %>%
#   mutate(Alphalist = ifelse(ED$Estuary %in% AlphaBZlist, 1, 0))
# ED$AlphaBZlist <- as.character(ED$AlphaBZlist)


#Didn't like that for some reason 
ED[ED$Estuary == "Mlalazi",153] <- 1
ED[ED$Estuary == "Matigulu/Nyoni",153] <- 1
ED[ED$Estuary == "St Lucia",153] <- 1
ED[ED$Estuary == "Mngazana",153] <- 1
ED[ED$Estuary == "Mntafufu",153] <- 1
ED[ED$Estuary == "Mzamba",153] <- 1
ED[ED$Estuary == "Mtamvuna",153] <- 1
ED[ED$Estuary == "Kariega",153] <- 1
ED[ED$Estuary == "Kwelera",153] <- 1
ED[ED$Estuary == "Knysna",153] <- 1
ED[ED$Estuary == "Tyolomnqa",153] <- 1
ED[ED$Estuary == "Kowie",153] <- 1
ED[ED$Estuary == "Gqunube",153] <- 1
ED[ED$Estuary == "Bushmans",153] <- 1
ED[ED$Estuary == "Berg",153] <- 1
ED[ED$Estuary == "Orange",153] <- 1
ED[ED$Estuary == "Olifants",153] <- 1
ED[ED$Estuary == "Diep",153] <- 1
ED[ED$Estuary == "Verlore",153] <- 1
ED[ED$Estuary == "Wildevoel",153] <- 1
ED$AlphaBZlist <- as.character(ED$AlphaBZlist)


Protected[,1] 

ED$protected <- 0
 ED <- ED %>%
   mutate(protected = ifelse(ED$Estuary %in% Protected[,1], 1, 0))
ED$protected <- as.character(ED$protected)


#Now we touch up the modelist 
ED$modelist <- 0
modelist <- c("St Lucia", "Mlalazi", "Matigulu/Nyoni", "Knysna", "Kariega", 
               "Bushmans", "Kosi", "Mfolozi/Msunduzi", "Manzimtoti", "Mkomazi", 
               "Kwelera", "Orange", "Mtentu", "Mngazana", "Ngqusi/Inxaxo", 
               "Great Kei", "Great Fish", "Zinkwasi", "Mzamba", "Msikaba")


# ED <- ED %>%
#   mutate(modelist = ifelse(ED$Estuary %in% modelist, 1, 0))
# ED$modelist <- as.character(ED$modelist)


#Again, didn't like that so we go manual 
#FOR MODELIST

ED[ED$Estuary == "St Lucia",155] <- 1
ED[ED$Estuary == "Mlalazi",155] <- 1
ED[ED$Estuary == "Matigulu/Nyoni",155] <- 1
ED[ED$Estuary == "Knysna",155] <- 1
ED[ED$Estuary == "Kariega",155] <- 1
ED[ED$Estuary == "Bushmans",155] <- 1
ED[ED$Estuary == "Kosi",155] <- 1
ED[ED$Estuary == "Mfolozi/Msunduzi",155] <- 1
ED[ED$Estuary == "Manzimtoti",155] <- 1
ED[ED$Estuary == "Mkomazi",155] <- 1
ED[ED$Estuary == "Kwelera",155] <- 1
ED[ED$Estuary == "Orange",155] <- 1
ED[ED$Estuary == "Mtentu",155] <- 1
ED[ED$Estuary == "Mngazana",155] <- 1
ED[ED$Estuary == "Ngqusi/Inxaxo",155] <- 1
ED[ED$Estuary == "Great Kei",155] <- 1
ED[ED$Estuary == "Great Fish",155] <- 1
ED[ED$Estuary == "Zinkwasi",155] <- 1
ED[ED$Estuary == "Mzamba",155] <- 1
ED[ED$Estuary == "Msikaba",155] <- 1
ED$modelist <- as.character(ED$modelist)


#Boom now complist 

complist

ED$complist <- 0
ED <- ED %>%
   mutate(complist = ifelse(ED$Estuary %in% complist, 1, 0)) 
ED$complist <- as.character(ED$complist)

#Now back to manual method 
#FOR COMPLEMENTARY METHOD
complist
ED[ED$Estuary == "Kosi",156] <- 1
ED[ED$Estuary == "St Lucia",156] <- 1
ED[ED$Estuary == "Mlalazi",156] <- 1
ED[ED$Estuary == "Matigulu/Nyoni",156] <- 1
ED[ED$Estuary == "Manzimtoti",156] <- 1
ED[ED$Estuary == "Mkomazi",156] <- 1
ED[ED$Estuary == "Mbizana",156] <- 1
ED[ED$Estuary == "Mtamvuna",156] <- 1
ED[ED$Estuary == "Mtentu",156] <- 1
ED[ED$Estuary == "Mngazana",156] <- 1
ED[ED$Estuary == "Qora",156] <- 1
ED[ED$Estuary == "Kwelera",156] <- 1
ED[ED$Estuary == "Tyolomnqa",156] <- 1
ED[ED$Estuary == "Great Fish",156] <- 1
ED[ED$Estuary == "Kariega",156] <- 1
ED[ED$Estuary == "Bushmans",156] <- 1
ED[ED$Estuary == "Knysna",156] <- 1
ED[ED$Estuary == "Bot",156] <- 1
ED[ED$Estuary == "Berg",156] <- 1
ED[ED$Estuary == "Orange",156] <- 1
ED$complist <- as.character(ED$complist)





#Boom now randlist 

Randlist4

#Will probably have to do this the manual way 

ED$randlist <- 0

ED[ED$Estuary == "Mpande",157] <- 1
ED[ED$Estuary == "Berg",157] <- 1
ED[ED$Estuary == "Jujura",157] <- 1
ED[ED$Estuary == "Sundays",157] <- 1
ED[ED$Estuary == "Matigulu/Nyoni",157] <- 1
ED[ED$Estuary == "Manzimtoti",157] <- 1
ED[ED$Estuary == "Knysna",157] <- 1
ED[ED$Estuary == "Ngqwara",157] <- 1
ED[ED$Estuary == "Xora",157] <- 1
ED[ED$Estuary == "Lovu",157] <- 1
ED[ED$Estuary == "Ntlupeni",157] <- 1
ED[ED$Estuary == "Mgwetyana",157] <- 1
ED[ED$Estuary == "Hickmans",157] <- 1
ED[ED$Estuary == "Orange",157] <- 1
ED[ED$Estuary == "Blind",157] <- 1
ED[ED$Estuary == "Ntlonyane",157] <- 1
ED[ED$Estuary == "Mtamvuna",157] <- 1
ED[ED$Estuary == "Sout R.",157] <- 1
ED[ED$Estuary == "St Lucia",157] <- 1
ED[ED$Estuary == "Ngqusi/Inxaxo",157] <- 1
ED$randlist <- as.character(ED$randlist)




#Remove the estuaries that don't have any species 
FishSurvey <- subset(FishSurvey, Alpha > 0)

FishSurvey[,1]


#What's the alpha diversity of each individual protected estuary? 
ED[ED$Estuary == "Kosi",149]
