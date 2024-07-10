library(tidyverse)
library(dplyr)
library(tidymodels)
library(readr)
library(kknn)
library(janitor)
library(ISLR)
library(discrim)
library(poissonreg)
library(glmnet)
library(corrr)
library(corrplot)
library(randomForest)
library(xgboost)
library(rpart.plot)
library(vip)
library(ranger)
library(tidytext)
library(ggplot2)

#read in the data
Lana <- read_csv("Lana Del Rey Spotify Data.csv", show_col_types = FALSE)

#check shape
dim(Lana)

#check number of albums
count(distinct(Lana, album_name))

#check number of tracks
count(distinct(Lana, track_name))

#check shape when keeping only the unique tracks in each unique album
Lana1<-Lana%>%
  distinct(album_name, track_name, .keep_all = TRUE)
dim(Lana)

#keep only unique tracks
Lana2 <- Lana %>%
  filter(!duplicated(track_name))

#check the albums
albums <- unique(Lana2$album_name)
print(albums)

#merge the different versions of the same album
Lana3 <- Lana2 %>%
  mutate(album_name = ifelse(str_detect(album_name, "Born To Die"), 
                             "Born to Die (all versions)", 
                             album_name)) %>% 
  mutate(album_name = ifelse(str_detect(album_name, "Ultraviolence"), 
                             "Ultraviolence (all versions)", 
                             album_name))
count(distinct(Lana3, album_name)) #count number of updated albums

#select only the variables of interest
Lana4 <- Lana3 %>% 
  select(c("track_name", "album_name", "danceability", "energy", "loudness",  
           "acousticness", "instrumentalness", "tempo", "speechiness", "liveness", "valence"))

#check for missingness
sum(is.na(Lana4))

#finalize clean data
write_csv(Lana4, 'Lana_final.csv')

#assign to the variable Lana_data
Lana_data <- read_csv('Lana_final.csv', show_col_types = FALSE)
head(Lana_data)