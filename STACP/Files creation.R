library(tidyverse)
library(lubridate)

#rm(list = ls())

base <- read.table("D:/UTS/MDSI/Sem 03/Social and Information Network Analysis/Assignment/Assignment 3/data/checkins.txt")

colnames(base) <- c("userID", "tweetID", "lat", "long", "date", "time", "placeID", "content")

nrow(base %>% distinct(userID)) #61413 unique users
nrow(base %>% distinct(placeID)) #42304 unique users
nrow(base %>% distinct(tweetID)) #973359 unique tweets

base$check_time <- as.POSIXct(ymd(base$date) + hms(base$time))

#Subset for top 1000 users

well_travelled <- base %>%
  group_by(userID) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  top_n(1000)

#Create sequential userID
well_travelled <- transform(well_travelled, 
                  user.rank = ave(userID, FUN = function(x) rank(x, ties.method = "first")))

#Join with original data to get sequential userIDs
base <- base %>%
  inner_join(well_travelled %>% select(-count), by = c("userID"))


nrow(base %>% distinct(userID)) #1014 unique users
nrow(base %>% distinct(placeID)) #22999 unique places
nrow(base %>% distinct(tweetID)) #170128 unique tweets

#Editing userID
base$userID <- base$user.rank
base$user.rank <- NULL


#Get size_file values
n_users = nrow(base %>% distinct(userID))
n_items = nrow(base %>% distinct(placeID))

#Create poi_coos file
poi_coos <- base %>%
  group_by(placeID, lat, long) %>%
  summarise(count = n()) %>%
  arrange(placeID, lat, long, -count) %>%
  ungroup() %>%
  group_by(placeID) %>%
  filter(row_number()==1)%>%
  select(-count)

poi_coos <- transform(poi_coos, 
                      place.rank = ave(placeID, FUN = function(x) rank(x, ties.method = "first")))

base <- base %>%
  inner_join(poi_coos %>% select(placeID,place.rank), by = c("placeID"))


#Edit placeIDs
poi_coos$placeID <- poi_coos$place.rank
poi_coos$place.rank <- NULL
poi_coos$placeID <- poi_coos$placeID - 1


base$placeID <- base$place.rank
base$place.rank <- NULL
base$placeID <- base$placeID - 1
base$userID <- base$userID - 1

#Create checkins file in the needed format
checkins <- base %>%
  mutate(check_unix = as.numeric(base$check_time)) %>%
  select(userID, placeID, check_unix)

#Get counts at a user, place level

freq <- base %>%
  group_by(userID, placeID) %>%
  summarise(count = n())

#Subset top 70% for train

train <- base %>%
  select(-check_time) %>%
  group_by(userID) %>%
  top_frac(0.7)


#Join train and frequency table to get counts
train <- train %>%
  distinct(userID, placeID) %>%
  left_join(freq, by = c("userID", "placeID"))


#Remove the combinations present in train
base <- base %>%
  left_join(train %>% select(userID, placeID, count), by = c("userID", "placeID")) %>%
  filter(is.na(count)) %>%
  select(-count)

#Subset next 70% of data for test

test <- base %>%
  select(-check_time) %>%
  group_by(userID) %>%
  top_frac(0.7)


#Join train and frequency table to get counts
test <- test %>%
  distinct(userID, placeID) %>%
  left_join(freq, by = c("userID", "placeID"))


#Remove the combinations present in train
base <- base %>%
  left_join(test %>% select(userID, placeID, count), by = c("userID", "placeID")) %>%
  filter(is.na(count)) %>%
  select(-count)

#Create tune data from whatever is left in baes
tune <- base %>%
  distinct(userID, placeID) %>%
  left_join(freq, by = c("userID", "placeID"))


#Check if combinations are present in multiple datasets
nrow(train %>% inner_join(test, by = c("userID", "placeID")))
nrow(train %>% inner_join(tune, by = c("userID", "placeID")))
nrow(tune %>% inner_join(test, by = c("userID", "placeID")))

#Export all files
write.table(checkins, "D:/UTS/MDSI/Sem 03/Social and Information Network Analysis/Assignment/Assignment 3/data/for_model/Foursquare_checkins.txt", sep = '\t', row.names = F) 

write.table(poi_coos, "D:/UTS/MDSI/Sem 03/Social and Information Network Analysis/Assignment/Assignment 3/data/for_model/Foursquare_poi_coos.txt", sep = '\t', row.names = F) 

write.table(test, "D:/UTS/MDSI/Sem 03/Social and Information Network Analysis/Assignment/Assignment 3/data/for_model/Foursquare_test.txt", sep = '\t', row.names = F) 

write.table(train, "D:/UTS/MDSI/Sem 03/Social and Information Network Analysis/Assignment/Assignment 3/data/for_model/Foursquare_train.txt", sep = '\t', row.names = F) 

write.table(tune, "D:/UTS/MDSI/Sem 03/Social and Information Network Analysis/Assignment/Assignment 3/data/for_model/Foursquare_tune.txt", sep = '\t', row.names = F) 