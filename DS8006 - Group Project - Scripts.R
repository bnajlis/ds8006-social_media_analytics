### installing/loading libraries
library(twitteR)
library(ggplot2)
library(httr)
library(rjson)
install.packages("tm")
library(tm)
library(gridExtra)
install.packages("lubridate")
library(lubridate)
install.packages("NLP")
library(NLP)
library(SnowballC)
library(dplyr)
library(scales)
library(stringr)
install.packages("network")
library(network)
install.packages("sna")
library(sna)
library(wordcloud)
library(ggthemes)

#####################################
# Data Collection and preprocessing #
#####################################

# Oauth authentication with API key and token
consumer_key <- "=== insert your customer_key here ==="
consumer_secret <- "=== insert your consumer_secret here ==="
access_token <- "=== insert your access_token here ==="
access_secret <- "=== insert your access_secret here ==="

current_date <- format(Sys.time(), "%d%b%Y")

# Twitter oauth
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

swTweets1 <- twListToDF(userTimeline(user="@SunwingAirIines", n=3200, includeRts = TRUE, retryOnRateLimit=2000))
write.csv(file = paste("sunwingairlines_usertimeline_", current_date, ".csv"), x= swTweets1, quote = TRUE, col.names = TRUE)
swTweets2 <- twListToDF(searchTwitter("SunWing", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("sunwingairlines_mentions_", current_date,".csv"), x= swTweets2, quote = TRUE, col.names = TRUE)
swTweets3 <- twListToDF(searchTwitter("#SunWing", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("sunwingairlines_hashtag_", current_date,".csv"), x= swTweets3, quote = TRUE, col.names = TRUE)
swTweets4 <- twListToDF(searchTwitter("@SunwingAirIines", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("sunwingairlines_atmention_", current_date,".csv"), x= swTweets4, quote = TRUE, col.names = TRUE)
swTweets5 <- twListToDF(searchTwitter("to:SunwingAirIines", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("sunwingairlines_touser_", current_date,".csv"), x= swTweets5, quote = TRUE, col.names = TRUE)
swTweets6 <- twListToDF(searchTwitter("from:SunwingAirIines", n=1000, retryOnRateLimit=2000))
write.csv(file = paste("sunwingairlines_fromuser_", current_date,".csv"), x= swTweets6, quote = TRUE, col.names = TRUE)
#swTweets <- rbind.data.frame(swTweets1, swTweets2, swTweets3, swTweets4, swTweets5, swTweets6)

# Load all sunwing files
sunwing1 <- read.csv("sunwingairlines_atmention_ 09Mar2017 .csv")
sunwing2 <- read.csv("sunwingairlines_atmention_ 11Mar2017 .csv")
sunwing3 <- read.csv("sunwingairlines_hashtag_ 09Mar2017 .csv")
sunwing4 <- read.csv("sunwingairlines_hashtag_ 11Mar2017 .csv")
sunwing5 <- read.csv("sunwingairlines_mentions_ 09Mar2017 .csv")
sunwing6 <- read.csv("sunwingairlines_mentions_ 11Mar2017 .csv")
sunwing7 <- read.csv("sunwingairlines_touser_ 09Mar2017 .csv")
sunwing8 <- read.csv("sunwingairlines_touser_ 11Mar2017 .csv")
sunwing9 <- read.csv("sunwingairlines_usertimeline_ 09Mar2017 .csv")
sunwing10 <- read.csv("sunwingairlines_usertimeline_ 11Mar2017 .csv")

sunwing11 <- read.csv("sunwingairlines_atmention_ 18Mar2017 .csv")
sunwing12 <- read.csv("sunwingairlines_hashtag_ 18Mar2017 .csv")
sunwing13 <- read.csv("sunwingairlines_mentions_ 18Mar2017 .csv")
sunwing14 <- read.csv("sunwingairlines_touser_ 18Mar2017 .csv")
sunwing15 <- read.csv("sunwingairlines_usertimeline_ 18Mar2017 .csv")

#bind all sunwing rows into one single data frame
sunwing_all <- rbind(sunwing1, sunwing2, sunwing3, sunwing4, sunwing5, sunwing6, sunwing7, sunwing8, sunwing9, sunwing10, sunwing11, sunwing12, sunwing13, sunwing14, sunwing15)
# remove surrogate key column for de-duplication
sunwing_all$X <- NULL
# add column that identifies duplicated rows
sunwing_all$isDuplicated <- duplicated(sunwing_all)
# subset rows not marked as duplicated
sunwing_all <- sunwing_all[!sunwing_all$isDuplicated,]

# date filtering
sunwing_all$created <- as.Date(sunwing_all$created, format= "%Y-%m-%d")
min(sunwing_all$created)  # check min date
sum(is.na(sunwing_all$created)) #check for any NA in dates
sunwing_all <- subset(sunwing_all, created > as.Date("2016-12-31"))
min(sunwing_all$created)  # check min date

current_date <- format(Sys.time(), "%d%b%Y")
write.csv(file = paste("sunwingairlines_all_", current_date,".csv"), x= sunwing_all, quote = TRUE, col.names = TRUE)


nltTweets1 <- twListToDF(userTimeline(user="@newleaftravel", n=3200, includeRts = TRUE, retryOnRateLimit=2000))
write.csv(file = paste("newleaftravel_usertimeline_", current_date,".csv"), x= nltTweets1, quote = TRUE, col.names = TRUE)
nltTweets2 <- twListToDF(searchTwitter("newleaftravel", n=3200, retryOnRateLimit=2000))
write.csv(file = paste("newleaftravel_mentions_", current_date,".csv"), x= nltTweets2, quote = TRUE, col.names = TRUE)
nltTweets3 <- twListToDF(searchTwitter("#newleaftravel", n=3200, retryOnRateLimit=2000))
write.csv(file = paste("newleaftravel_hashtag_", current_date,".csv"), x= nltTweets3, quote = TRUE, col.names = TRUE)
nltTweets4 <- twListToDF(searchTwitter("@newleaftravel", n=3200, retryOnRateLimit=2000))
write.csv(file = paste("newleaftravel_atmention_", current_date,".csv"), x= nltTweets4, quote = TRUE, col.names = TRUE)
nltTweets5 <- twListToDF(searchTwitter("to:newleaftravel", n=3200, retryOnRateLimit=2000))
write.csv(file = paste("newleaftravel_touser_", current_date,".csv"), x= nltTweets5, quote = TRUE, col.names = TRUE)
nltTweets6 <- twListToDF(searchTwitter("from:newleaftravel", n=3200, retryOnRateLimit=2000))

write.csv(file = paste("newleaftravel_fromuser_", current_date,".csv"), x= nltTweets6, quote = TRUE, col.names = TRUE)

# Load all sunwing files
newleaf1 <- read.csv("newleaftravel_atmention_ 09Mar2017 .csv")
newleaf2 <- read.csv("newleaftravel_atmention_ 11Mar2017 .csv")
#newleaf3 <- read.csv("newleaftravel_hashtag_ 09Mar2017 .csv")
#newleaf4 <- read.csv("newleaftravel_hashtag_ 11Mar2017 .csv")
newleaf5 <- read.csv("newleaftravel_mentions_ 09Mar2017 .csv")
newleaf6 <- read.csv("newleaftravel_mentions_ 11Mar2017 .csv")
newleaf7 <- read.csv("newleaftravel_touser_ 09Mar2017 .csv")
newleaf8 <- read.csv("newleaftravel_touser_ 11Mar2017 .csv")
newleaf9 <- read.csv("newleaftravel_usertimeline_ 09Mar2017 .csv")
newleaf10 <- read.csv("newleaftravel_usertimeline_ 11Mar2017 .csv")

newleaf11 <- read.csv("newleaftravel_atmention_ 18Mar2017 .csv")
#newleaf12 <- read.csv("newleaftravel_hashtag_ 18Mar2017 .csv")
newleaf13 <- read.csv("newleaftravel_mentions_ 18Mar2017 .csv")
newleaf14 <- read.csv("newleaftravel_touser_ 18Mar2017 .csv")
newleaf15 <- read.csv("newleaftravel_usertimeline_ 18Mar2017 .csv")


#bind all sunwing rows into one single data frame
newleaf_all <- rbind(newleaf1, newleaf2, newleaf5, newleaf6, newleaf7, newleaf8, newleaf9, newleaf10, newleaf11, newleaf13, newleaf14, newleaf15)
# remove surrogate key column for de-duplication
newleaf_all$X <- NULL
# add column that identifies duplicated rows
newleaf_all$isDuplicated <- duplicated(newleaf_all)
newleaf_all <- newleaf_all[!newleaf_all$isDuplicated,]

# date filtering
newleaf_all$created <- as.Date(newleaf_all$created, format= "%Y-%m-%d")
min(newleaf_all$created)  # check min date
sum(is.na(newleaf_all$created)) #check for any NA in dates
newleaf_all <- subset(newleaf_all, created > as.Date("2016-12-31"))
min(newleaf_all$created)  # check min date

write.csv(file = paste("newleaftravel_all_", current_date,".csv"), x= newleaf_all, quote = TRUE, col.names = TRUE)


# downloading data for WestJet
tweets_westjet <- searchTwitter("WestJet", n=3200)
tweets_westjet <- twListToDF(tweets_westjet)
write.csv(tweets_westjet, file = "tweets_westjet.csv")

tweets_hashwestjet <- searchTwitter("#WestJet", n=3200)
tweets_hashwestjet <- twListToDF(tweets_hashwestjet)
write.csv(tweets_hashwestjet, file = "tweets_hashwestjet.csv")

tweets_fromwestjet <- searchTwitter("from:WestJet", n=3200)
tweets_fromwestjet <- twListToDF(tweets_fromwestjet)
write.csv(tweets_fromwestjet, file = "tweets_fromwestjet.csv")

tweets_towestjet <- searchTwitter("to:WestJet", n=3200)
tweets_towestjet <- twListToDF(tweets_towestjet)
write.csv(tweets_towestjet, file = "tweets_towestjet.csv")

tweets_atwestjet <- searchTwitter("@WestJet", n=3200)
tweets_atwestjet <- twListToDF(tweets_atwestjet)
write.csv(tweets_atwestjet, file = "tweets_atwestjet.csv")

tweets_orwestjet <- searchTwitter("WestJet OR #Westjet OR @WestJet OR to:WestJet OR from:WestJet", n=3200)
tweets_orwestjet <- twListToDF(tweets_orwestjet)
write.csv(tweets_orwestjet, file = "tweets_orwestjet.csv")

# downloading data fro Jetlines
tweets_jetlines <- searchTwitter("Jetlines", n=3200)
tweets_jetlines <- twListToDF(tweets_jetlines)
write.csv(tweets_jetlines, file = "tweets_jetlines.csv")

tweets_hashjetlines <- searchTwitter("#Jetlines", n=3200)
tweets_hashjetlines <- twListToDF(tweets_hashjetlines)
write.csv(tweets_hashjetlines, file = "tweets_hashjetlines.csv")

tweets_canadajetlines <- searchTwitter("CanadaJetlines", n=3200)
tweets_canadajetlines <- twListToDF(tweets_canadajetlines)
write.csv(tweets_canadajetlines, file = "tweets_canadajetlines.csv")

tweets_hashcanadajetlines <- searchTwitter("#CanadaJetlines", n=3200)
tweets_hashcanadajetlines <- twListToDF(tweets_hashcanadajetlines)
write.csv(tweets_hashcanadajetlines, file = "tweets_hashcanadajetlines.csv")

tweets_fromjetlines <- searchTwitter("from:CanadaJetlines", n=3200)
tweets_fromjetlines <- twListToDF(tweets_fromjetlines)
write.csv(tweets_fromjetlines, file = "tweets_fromjetlines.csv")

tweets_tojetlines <- searchTwitter("to:CanadaJetlines", n=3200)
tweets_tojetlines <- twListToDF(tweets_tojetlines)
write.csv(tweets_tojetlines, file = "tweets_tojetlines.csv")

tweets_atjetlines <- searchTwitter("@CanadaJetlines", n=3200)
tweets_atjetlines <- twListToDF(tweets_atjetlines)
write.csv(tweets_atjetlines, file = "tweets_atjetlines.csv")

tweets_orjetlines <- searchTwitter("Jetlines OR #Jetlines OR CanadaJetlines OR #CanadaJetlines OR @CanadaJetlines OR to:CanadaJetlines OR from:CanadaJetlines", n=3200)
tweets_orjetlines <- twListToDF(tweets_orjetlines)
write.csv(tweets_orjetlines, file = "tweets_orjetlines.csv")


# Load all westjet files
westjet1 <- read.csv("tweets_westjet-03-09.csv")
westjet2 <- read.csv("tweets_hashwestjet-03-09.csv")
westjet3 <- read.csv("tweets_fromwestjet-03-09.csv")
westjet4 <- read.csv("tweets_towestjet-03-09.csv")
westjet5 <- read.csv("tweets_atwestjet-03-09.csv")
westjet6 <- read.csv("tweets_orwestjet-03-09.csv")

westjet7 <- read.csv("tweets_westjet-03-10.csv")
westjet8 <- read.csv("tweets_hashwestjet-03-10.csv")
westjet9 <- read.csv("tweets_fromwestjet-03-10.csv")
westjet10 <- read.csv("tweets_towestjet-03-10.csv")
westjet11 <- read.csv("tweets_atwestjet-03-10.csv")
westjet12 <- read.csv("tweets_orwestjet-03-10.csv")

westjet13 <- read.csv("tweets_westjet-03-14.csv")
westjet14 <- read.csv("tweets_hashwestjet-03-14.csv")
westjet15 <- read.csv("tweets_fromwestjet-03-14.csv")
westjet16 <- read.csv("tweets_towestjet-03-14.csv")
westjet17 <- read.csv("tweets_atwestjet-03-14.csv")
westjet18 <- read.csv("tweets_orwestjet-03-14.csv")

westjet19 <- read.csv("tweets_westjet-03-18.csv")
westjet20 <- read.csv("tweets_hashwestjet-03-18.csv")
westjet21 <- read.csv("tweets_fromwestjet-03-18.csv")
westjet22 <- read.csv("tweets_towestjet-03-18.csv")
westjet23 <- read.csv("tweets_atwestjet-03-18.csv")
westjet24 <- read.csv("tweets_orwestjet-03-18.csv")

#bind all westjet rows into one single data frame
westjet_all <- rbind(westjet1,
                     westjet2,
                     westjet3,
                     westjet4,
                     westjet5,
                     westjet6,
                     westjet7,
                     westjet8,
                     westjet9,
                     westjet10,
                     westjet11,
                     westjet12,
                     westjet13,
                     westjet14,
                     westjet15,
                     westjet16,
                     westjet17,
                     westjet18,
                     westjet19,
                     westjet20,
                     westjet21,
                     westjet22,
                     westjet23,
                     westjet24)


# remove surrogate key column for de-duplication
westjet_all$X <- NULL
# add column that identifies duplicated rows
westjet_all$isDuplicated <- duplicated(westjet_all)
westjet_all <- westjet_all[!westjet_all$isDuplicated,]

# Load all jetlines files
jetlines1 <- read.csv("tweets_jetlines-03-09.csv")
jetlines2 <- read.csv("tweets_hashjetlines-03-09.csv")
jetlines3 <- read.csv("tweets_canadajetlines-03-09.csv")
#jetlines4 <- read.csv("tweets_hashcanadajetlines-03-09.csv")
jetlines5 <- read.csv("tweets_fromjetlines-03-09.csv")
jetlines6 <- read.csv("tweets_tojetlines-03-09.csv")
jetlines7 <- read.csv("tweets_atjetlines-03-09.csv")
jetlines8 <- read.csv("tweets_orjetlines-03-09.csv")

jetlines9 <- read.csv("tweets_jetlines-03-10.csv")
jetlines10 <- read.csv("tweets_hashjetlines-03-10.csv")
jetlines11 <- read.csv("tweets_canadajetlines-03-10.csv")
#jetlines12 <- read.csv("tweets_hashcanadajetlines-03-10.csv")
jetlines13 <- read.csv("tweets_fromjetlines-03-10.csv")
jetlines14 <- read.csv("tweets_tojetlines-03-10.csv")
jetlines15 <- read.csv("tweets_atjetlines-03-10.csv")
jetlines16 <- read.csv("tweets_orjetlines-03-10.csv")

jetlines17 <- read.csv("tweets_jetlines-03-14.csv")
jetlines18 <- read.csv("tweets_hashjetlines-03-14.csv")
jetlines19 <- read.csv("tweets_canadajetlines-03-14.csv")
#jetlines20 <- read.csv("tweets_hashcanadajetlines-03-14.csv")
jetlines21 <- read.csv("tweets_fromjetlines-03-14.csv")
jetlines22 <- read.csv("tweets_tojetlines-03-14.csv")
jetlines23 <- read.csv("tweets_atjetlines-03-14.csv")
jetlines24 <- read.csv("tweets_orjetlines-03-14.csv")

jetlines25 <- read.csv("tweets_jetlines-03-14.csv")
jetlines26 <- read.csv("tweets_hashjetlines-03-14.csv")
jetlines27 <- read.csv("tweets_canadajetlines-03-14.csv")
#jetlines28 <- read.csv("tweets_hashcanadajetlines-03-14.csv")
jetlines29 <- read.csv("tweets_fromjetlines-03-14.csv")
jetlines30 <- read.csv("tweets_tojetlines-03-14.csv")
jetlines31 <- read.csv("tweets_atjetlines-03-14.csv")
jetlines32 <- read.csv("tweets_orjetlines-03-14.csv")

#bind all jetlines rows into one single data frame
jetlines_all <- rbind(jetlines1,
                      jetlines2,
                      jetlines3,
                      
                      jetlines5,
                      jetlines6,
                      jetlines7,
                      jetlines8,
                      jetlines9,
                      jetlines10,
                      jetlines11,
                      
                      jetlines13,
                      jetlines14,
                      jetlines15,
                      jetlines16,
                      jetlines17,
                      jetlines18,
                      jetlines19,
                      
                      jetlines21,
                      jetlines22,
                      jetlines23,
                      jetlines24,
                      jetlines25,
                      jetlines26,
                      jetlines27,
                      
                      jetlines29,
                      jetlines30,
                      jetlines31,
                      jetlines32)



# remove surrogate key column for de-duplication
jetlines_all$X <- NULL
# add column that identifies duplicated rows
jetlines_all$isDuplicated <- duplicated(jetlines_all)
jetlines_all <- jetlines_all[!jetlines_all$isDuplicated,]

# loading data for other airlines
Aircanada <- searchTwitter("AirCanada OR #AirCanada OR #aircanada OR aircanada OR @AirCanada OR "air canada" OR "Air Canada" OR to:AirCanada OR from:AirCanada ", n=3200)

#Put the tweets downloaded into a data.frame
Aircanada <- twListToDF(Aircanada)

aircanada_all <- read.csv("AirCanadarevised.csv")
aircanada_all$X.1 <- NULL
aircanada_all$X <- NULL


Porter <- searchTwitter("@porterairlines OR #flyporter OR "Porter Airlines" OR "porter airlines" OR "PORTER AIRLINES" OR to:porterairlines OR from:porterairlines ", n=3200)

#Put the tweets downloaded into a data.frame
Porter <- twListToDF(Porter)
porter3 <- read.csv("#flyporter-march11.csv")
porter4 <- read.csv("@porterairlines-march09.csv.csv")
porter5 <- read.csv("@porterairlines-march10.csv.csv")
porter6 <- read.csv("@porterairlines-march11.csv")
porter7 <- read.csv("allcaptalPorter Airlines-march09.csv")
porter8 <- read.csv("allcaptalPORTER AIRLINES-march10.csv")
porter9 <- read.csv("allcaptalPORTER AIRLINES-march11.csv")
porter10 <- read.csv("fromporterairlines-march09.csv")
porter11 <- read.csv("fromporterairlines-march10.csv")
porter12 <- read.csv("fromporterairlines-march11.csv")
porter13 <- read.csv("Porter Airlines-march09.csv")
porter14 <- read.csv("Porter Airlines-march10.csv")
porter15 <- read.csv("Porter Airlines-march11.csv")
porter16 <- read.csv("smallporter airlines-march09.csv")
porter17 <- read.csv("smallporter airlines-march10.csv")
porter18 <- read.csv("smallporter airlines-march11.csv")
porter19 <- read.csv("toporterairlines-march09.csv")
porter20 <- read.csv("toporterairlines-march10.csv")
porter21 <- read.csv("toporterairlines-march11.csv")
porter22 <- read.csv("wholeporterairlines-march10.csv")
porter23 <- read.csv("wholeporterairlines-march11.csv")
porter24 <- read.csv("C:\\Users\\Saeede\\Desktop\\newSocial media project\\Porter\\wholeporter-march09.csv")

#bind all sunwing rows into one single data frame
Porter <- rbind(porter1,porter2,porter3,porter4,porter5,porter6,porter7,porter8,porter9,porter10,porter11,porter12,porter13,porter14,porter15,porter16,porter17,porter18,porter19,porter20,porter21,porter22,porter23,porter24,porter25)

porter_all <- read.csv("PorterT 18Mar2017.csv")
porter_all$X <- NULL

newleaf_all <- read.csv("newleaftravel_all_ 18Mar2017.csv")
newleaf_all$X <- NULL

sunwing_all <- read.csv("sunwingairlines_all_ 18Mar2017.csv")
sunwing_all$X <- NULL


##############################  
# Twitter Metadata Analysis  #
##############################

### SUNWING
sunwing_tweets <- read.csv("sunwingairlines_all_ 18Mar2017 .csv")
sunwing_tweets$X <- NULL
sunwing_tweets$isOfficialAccount <- sunwing_tweets$screenName == "SunwingAirIines"
sunwing_tweets$isReply <- ifelse((!is.na(sunwing_tweets$replyToSID) |
                                    !is.na(sunwing_tweets$replyToSN) |
                                    !is.na(sunwing_tweets$replyToUID)), TRUE, FALSE)
sunwing_retweet_rate <- sum(sunwing_tweets$isRetweet) / nrow(sunwing_tweets)
sunwing_reply_rate <- sum(sunwing_tweets$isReply) / nrow(sunwing_tweets)
#sunwing_favorited_rate <- sum(sunwing_tweets$favorited) / nrow(sunwing_tweets)
sunwing_offacct_rate <- sum(sunwing_tweets$isOfficialAccount) / nrow(sunwing_tweets)



sunwing_metadata_summary <- data.frame(c("Retweets", "Replies","Official Account"), 
                                       c(sunwing_retweet_rate, sunwing_reply_rate, sunwing_offacct_rate))
colnames(sunwing_metadata_summary) <- c("Metric", "Value")
sunwing_metadata_summary$Airline = "Sunwing"

###  NEWLEAF
newleaf_tweets <- read.csv("newleaftravel_all_ 18Mar2017 .csv")
newleaf_tweets$X <- NULL
newleaf_tweets$isOfficialAccount <- newleaf_tweets$screenName == "newleaftravel"
newleaf_tweets$isReply <- ifelse((!is.na(newleaf_tweets$replyToSID) |
                                    !is.na(newleaf_tweets$replyToSN) |
                                    !is.na(newleaf_tweets$replyToUID)), TRUE, FALSE)
newleaf_retweet_rate <- sum(newleaf_tweets$isRetweet) / nrow(newleaf_tweets)
newleaf_reply_rate <- sum(newleaf_tweets$isReply) / nrow(newleaf_tweets)
#newleaf_favorited_rate <- sum(newleaf_tweets$favorited) / nrow(newleaf_tweets)
newleaf_offacct_rate <- sum(newleaf_tweets$isOfficialAccount) / nrow(newleaf_tweets)

newleaf_metadata_summary <- data.frame(c("Retweets", "Replies", "Official Account"), 
                                       c(newleaf_retweet_rate, newleaf_reply_rate, newleaf_offacct_rate))

colnames(newleaf_metadata_summary) <- c("Metric", "Value")
newleaf_metadata_summary$Airline = "NewLeafTravel"


#### AIR CANADA

aircanada_tweets <- read.csv("CanadaT 18Mar2017 .csv")
aircanada_tweets$X <- NULL
aircanada_tweets$isReply <- ifelse((!is.na(aircanada_tweets$replyToSID) |
                                      !is.na(aircanada_tweets$replyToSN) |
                                      !is.na(aircanada_tweets$replyToUID)), TRUE, FALSE)
aircanada_tweets$isOfficialAccount <- aircanada_tweets$screenName == "AirCanada"

aircanada_retweet_rate <- sum(aircanada_tweets$isRetweet) / nrow(aircanada_tweets)
aircanada_reply_rate <- sum(aircanada_tweets$isReply) / nrow(aircanada_tweets)
#aircanada_favorited_rate <- sum(aircanada_tweets$favorited) / nrow(aircanada_tweets)
aircanada_offacct_rate <- sum(aircanada_tweets$isOfficialAccount) / nrow(aircanada_tweets)

aircanada_metadata_summary <- data.frame(c("Retweets", "Replies", "Official Account"), 
                                         c(aircanada_retweet_rate, aircanada_reply_rate, aircanada_offacct_rate))

colnames(aircanada_metadata_summary) <- c("Metric", "Value")
aircanada_metadata_summary$Airline = "Air Canada"

##### PORTER


porter_tweets <- read.csv("PorterT 18Mar2017 .csv")
porter_tweets$X <- NULL
porter_tweets$isReply <- ifelse((!is.na(porter_tweets$replyToSID) |
                                   !is.na(porter_tweets$replyToSN) |
                                   !is.na(porter_tweets$replyToUID)), TRUE, FALSE)

porter_tweets$isOfficialAccount <- porter_tweets$screenName == "porterairlines"

porter_retweet_rate <- sum(porter_tweets$isRetweet) / nrow(porter_tweets)
porter_reply_rate <- sum(porter_tweets$isReply) / nrow(porter_tweets)
#porter_favorited_rate <- sum(porter_tweets$favorited) / nrow(porter_tweets)
porter_offacct_rate <- sum(porter_tweets$isOfficialAccount) / nrow(porter_tweets)

porter_metadata_summary <- data.frame(c("Retweets", "Replies", "Official Account"), 
                                      c(porter_retweet_rate, porter_reply_rate, porter_offacct_rate))

colnames(porter_metadata_summary) <- c("Metric", "Value")
porter_metadata_summary$Airline = "Porter"

#### JETLINES

jetlines_tweets <- read.csv("jetlines_allMar18.csv")
jetlines_tweets$X <- NULL
jetlines_tweets$group <- NULL
jetlines_tweets$isReply <- ifelse((!is.na(jetlines_tweets$replyToSID) |
                                     !is.na(jetlines_tweets$replyToSN) |
                                     !is.na(jetlines_tweets$replyToUID)), TRUE, FALSE)

jetlines_tweets$isOfficialAccount <- jetlines_tweets$screenName == "CanadaJetlines"

jetlines_retweet_rate <- sum(jetlines_tweets$isRetweet) / nrow(jetlines_tweets)
jetlines_reply_rate <- sum(jetlines_tweets$isReply) / nrow(jetlines_tweets)
#jetlines_favorited_rate <- sum(jetlines_tweets$favorited) / nrow(jetlines_tweets)
jetlines_offacct_rate <- sum(jetlines_tweets$isOfficialAccount) / nrow(jetlines_tweets)

jetlines_metadata_summary <- data.frame(c("Retweets", "Replies", "Official Account"), 
                                        c(jetlines_retweet_rate, jetlines_reply_rate, jetlines_offacct_rate))

colnames(jetlines_metadata_summary) <- c("Metric", "Value")
jetlines_metadata_summary$Airline = "JetLines"



#### WESTJET

westjet_tweets <- read.csv("westjet_allMar18.csv")
westjet_tweets$X <- NULL
westjet_tweets$group <- NULL
westjet_tweets$isReply <- ifelse((!is.na(westjet_tweets$replyToSID) |
                                    !is.na(westjet_tweets$replyToSN) |
                                    !is.na(westjet_tweets$replyToUID)), TRUE, FALSE)

westjet_tweets$isOfficialAccount <- westjet_tweets$screenName == "WestJet"

westjet_retweet_rate <- sum(westjet_tweets$isRetweet) / nrow(westjet_tweets)
westjet_reply_rate <- sum(westjet_tweets$isReply) / nrow(westjet_tweets)
#westjet_favorited_rate <- sum(westjet_tweets$favorited) / nrow(westjet_tweets)
westjet_offacct_rate <- sum(westjet_tweets$isOfficialAccount) / nrow(westjet_tweets)

westjet_metadata_summary <- data.frame(c("Retweets", "Replies", "Official Account"), 
                                       c(westjet_retweet_rate, westjet_reply_rate, westjet_offacct_rate))

colnames(westjet_metadata_summary) <- c("Metric", "Value")
westjet_metadata_summary$Airline = "WestJet"

### ALL AIRLINES

allairlines_tweets <- rbind(sunwing_tweets,
                            newleaf_tweets,
                            aircanada_tweets,
                            porter_tweets,
                            jetlines_tweets,
                            westjet_tweets)

allairlines_retweet_rate <- sum(allairlines_tweets$isRetweet) / nrow(allairlines_tweets)
allairlines_reply_rate <- sum(allairlines_tweets$isReply) / nrow(allairlines_tweets)
#westjet_favorited_rate <- sum(westjet_tweets$favorited) / nrow(westjet_tweets)
allairlines_offacct_rate <- sum(allairlines_tweets$isOfficialAccount) / nrow(allairlines_tweets)

allairlines_metadata_summary <- data.frame(c("Retweets", "Replies", "Official Account"), 
                                           c(allairlines_retweet_rate, allairlines_reply_rate, allairlines_offacct_rate))

colnames(allairlines_metadata_summary) <- c("Metric", "Value")
allairlines_metadata_summary$Airline = "All Airlines"

airlines_metadata_summary <- rbind(sunwing_metadata_summary,
                                   newleaf_metadata_summary, 
                                   aircanada_metadata_summary, 
                                   porter_metadata_summary,
                                   jetlines_metadata_summary,
                                   westjet_metadata_summary,
                                   allairlines_metadata_summary)

windowsFonts(ITC=windowsFont("ITC Officina Sans Std Book"))

ggplot(data=airlines_metadata_summary, aes(y=Value, x=Airline, label=Value)) + 
  geom_bar(aes(fill=Airline),data=airlines_metadata_summary, stat="identity")  +
  coord_flip() + 
  facet_grid(Metric ~ .) +
  scale_y_continuous(labels=percent, limits=c(0,1)) + 
  labs(title="Canadian Airlines - Twitter Metadata Analysis") + 
  geom_text(aes( y = Value, label = paste0(round(Value * 100,1),"%")), size=3, hjust=-0.1) + 
  theme_economist(base_family="ITC") + 
  scale_colour_economist() + 
  scale_fill_economist() + 
  theme(plot.title=element_text(family="ITC"),
        text=element_text(family="ITC"), legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) 




#################################
#   Exploratory Data Analysis   #
#################################

#SUNWING

# Load merged/de-duplicated data from csv file
sunwing_tweets <- read.csv("sunwingairlines_all_ 18Mar2017 .csv")
sunwing_tweets$X <- NULL
sunwing_tweets$isOfficialAccount <- sunwing_tweets$screenName == "SunwingAirIines"
sunwing_tweets$created <- ymd_hms(sunwing_tweets$created)
#tweets by time of the day
sunwing_tweets$timeonly <- strftime(sunwing_tweets$created, format="%H:%M:%S")
sunwing_tweets$timeonly <- as.POSIXct(sunwing_tweets$timeonly, format="%H:%M:%S")

# checking for N/A's
sunwing_tweets[(minute(sunwing_tweets$created) == 0 & second(sunwing_tweets$created) == 0),5] <- NA
mean(is.na(sunwing_tweets$timeonly))
#isReply
sunwing_tweets$isReply <- ifelse((!is.na(sunwing_tweets$replyToSID) |
                                    !is.na(sunwing_tweets$replyToSN) |
                                    !is.na(sunwing_tweets$replyToUID)), TRUE, FALSE)

sunwing_top10 <- sunwing_tweets %>% count(screenName) %>% top_n(n=10)

# number of tweets over time
qplot(created, data=sunwing_tweets, fill=isRetweet, facets = isReply~.)

ggplot(data = sunwing_tweets, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title="Number of Tweets over Time in Days - SunWing", x = "Time", y="Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4") +
  geom_density()

# number of tweets by week day
ggplot(data = sunwing_tweets, aes(x = wday(created, label = TRUE))) +
  stat_count(width = 1, aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title = "SunWing", x ="Day of the Week", y="Number of tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4") 

# Number of tweets in 3-hour time intervals
class(sunwing_tweets$timeonly) <- "POSIXct"
class(sunwing_tweets$created)

ggplot(data = sunwing_tweets, aes(x = timeonly)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  scale_x_datetime(breaks = date_breaks("3 hour")
                   ,labels = date_format("%H:00")) +
  labs(title = "SunWing", x = "Time", y="Number of Tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Late Night Tweets
latenight_sunwingtweets <- sunwing_tweets[(hour(sunwing_tweets$created) < 6),]
ggplot(data = latenight_sunwingtweets, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title = "Late Night Tweets - SunWing", x="Time", y = "Number of tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Retweets by count
ggplot(sunwing_tweets, aes(x= factor(isRetweet))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Number of tweets") + 
  ggtitle("Retweeted Tweets") +
  labs(title = "SunWing") + 
  scale_x_discrete(labels=c("Not retweeted", "Retweeted tweets"))

# Retweets by percentage
ggplot(sunwing_tweets, aes(x = sunwing_tweets$isRetweet)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Tweets by Type (Retweet vs. Other) - SunWing", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Other", "Retweets"))

# Replies by count
ggplot(sunwing_tweets, aes(sunwing_tweets$isReply)) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Number of tweets") + 
  ggtitle("Tweets by Type (Reply vs. Other) - SunWing") +
  scale_x_discrete(labels=c("Other", "Reply Tweets"))

# Replies by percentage
ggplot(sunwing_tweets, aes(x = sunwing_tweets$isReply)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Tweets by Type (Reply vs. Other) - SunWing", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Other", "Reply tweets"))

# Top 10 users
ggplot(data= sunwing_top10, aes(reorder(screenName, n),n)) +
  geom_bar(stat="identity", fill="midnightblue") +
  coord_flip() +
  labs(x = "Number of Tweets", y = "Screen Name", title = "Top 10 Users - SunWing")

sunwing_retweet_rate <- sum(sunwing_tweets$isRetweet) / nrow(sunwing_tweets)
sunwing_reply_rate <- sum(sunwing_tweets$isReply) / nrow(sunwing_tweets)
#sunwing_favorited_rate <- sum(sunwing_tweets$favorited) / nrow(sunwing_tweets)
sunwing_offacct_rate <- sum(sunwing_tweets$isOfficialAccount) / nrow(sunwing_tweets)



sunwing_metadata_summary <- data.frame(c("Retweets", "Replies","Official Account"), 
                                       c(sunwing_retweet_rate, sunwing_reply_rate, sunwing_offacct_rate))
colnames(sunwing_metadata_summary) <- c("Metric", "Value")
sunwing_metadata_summary$Airline = "Sunwing"



#### NEWLEAF

# Load merged/de-duplicated data from csv file
newleaf_tweets <- read.csv("newleaftravel_all_ 18Mar2017 .csv")
newleaf_tweets$X <- NULL
newleaf_tweets$isOfficialAccount <- newleaf_tweets$screenName == "newleaftravel"
newleaf_tweets$created <- ymd_hms(newleaf_tweets$created)
#tweets by time of the day
newleaf_tweets$timeonly <- strftime(newleaf_tweets$created, format="%H:%M:%S")
newleaf_tweets$timeonly <- as.POSIXct(newleaf_tweets$timeonly, format="%H:%M:%S")

# checking for N/A's
newleaf_tweets[(minute(newleaf_tweets$created) == 0 & second(newleaf_tweets$created) == 0),5] <- NA
mean(is.na(newleaf_tweets$timeonly))
#isReply
newleaf_tweets$isReply <- ifelse((!is.na(newleaf_tweets$replyToSID) |
                                    !is.na(newleaf_tweets$replyToSN) |
                                    !is.na(newleaf_tweets$replyToUID)), TRUE, FALSE)

newleaf_top10 <- newleaf_tweets %>% count(screenName) %>% top_n(n=10)

# number of tweets over time
qplot(created, data=newleaf_tweets, fill=isRetweet, facets = isReply~.)

ggplot(data = newleaf_tweets, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title="Number of Tweets over Time in Days - Newleaf", x = "Time", y="Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4") +
  geom_density()

# number of tweets by week day
ggplot(data = newleaf_tweets, aes(x = wday(created, label = TRUE))) +
  stat_count(width = 1, aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title = "Newleaf", x ="Day of the Week", y="Number of tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4") 

# Number of tweets in 3-hour time intervals
class(newleaf_tweets$timeonly) <- "POSIXct"

ggplot(data = newleaf_tweets, aes(x = timeonly)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  scale_x_datetime(breaks = date_breaks("3 hour")
                   ,labels = date_format("%H:00")) +
  labs(title = "Newleaf", x = "Time", y="Number of Tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Late Night Tweets
latenight_sunwingtweets <- newleaf_tweets[(hour(newleaf_tweets$created) < 6),]
ggplot(data = latenight_sunwingtweets, aes(x = created)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  labs(title = "Late Night Tweets - Newleaf", x="Time", y = "Number of tweets") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Retweets by count
ggplot(newleaf_tweets, aes(x= factor(isRetweet))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Number of tweets") + 
  ggtitle("Retweeted Tweets") +
  labs(title = "Newleaf") + 
  scale_x_discrete(labels=c("Not retweeted", "Retweeted tweets"))

# Retweets by percentage
ggplot(newleaf_tweets, aes(x = newleaf_tweets$isRetweet)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Tweets by Type (Retweet vs. Other) - Newleaf", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Other", "Retweets"))

# Replies by count
ggplot(newleaf_tweets, aes(newleaf_tweets$isReply)) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Number of tweets") + 
  ggtitle("Tweets by Type (Reply vs. Other) - Newleaf") +
  scale_x_discrete(labels=c("Other", "Reply Tweets"))

# Replies by percentage
ggplot(newleaf_tweets, aes(x = newleaf_tweets$isReply)) +
  geom_bar(aes(y=(..count..) / sum(..count..)), fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  scale_y_continuous(labels=percent) +
  labs(title = "Tweets by Type (Reply vs. Other) - Newleaf", y="Percentage of tweets") + 
  scale_x_discrete(labels=c("Other", "Reply tweets"))

# Top 10 users
ggplot(data= newleaf_top10, aes(reorder(screenName, n),n)) +
  geom_bar(stat="identity", fill="midnightblue") +
  coord_flip() +
  labs(x = "Number of Tweets", y = "Screen Name", title = "Top 10 Users - Newleaf")


newleaf_retweet_rate <- sum(newleaf_tweets$isRetweet) / nrow(newleaf_tweets)
newleaf_reply_rate <- sum(newleaf_tweets$isReply) / nrow(newleaf_tweets)
#newleaf_favorited_rate <- sum(newleaf_tweets$favorited) / nrow(newleaf_tweets)
newleaf_offacct_rate <- sum(newleaf_tweets$isOfficialAccount) / nrow(newleaf_tweets)

newleaf_metadata_summary <- data.frame(c("Retweets", "Replies", "Official Account"), 
                                       c(newleaf_retweet_rate, newleaf_reply_rate, newleaf_offacct_rate))

colnames(newleaf_metadata_summary) <- c("Metric", "Value")
newleaf_metadata_summary$Airline = "NewLeafTravel"


#### AIR CANADA

aircanada_tweets <- read.csv("CanadaT 18Mar2017 .csv")
aircanada_tweets$X <- NULL
aircanada_tweets$isReply <- ifelse((!is.na(aircanada_tweets$replyToSID) |
                                      !is.na(aircanada_tweets$replyToSN) |
                                      !is.na(aircanada_tweets$replyToUID)), TRUE, FALSE)
aircanada_tweets$isOfficialAccount <- aircanada_tweets$screenName == "AirCanada"

aircanada_retweet_rate <- sum(aircanada_tweets$isRetweet) / nrow(aircanada_tweets)
aircanada_reply_rate <- sum(aircanada_tweets$isReply) / nrow(aircanada_tweets)
#aircanada_favorited_rate <- sum(aircanada_tweets$favorited) / nrow(aircanada_tweets)
aircanada_offacct_rate <- sum(aircanada_tweets$isOfficialAccount) / nrow(aircanada_tweets)

aircanada_metadata_summary <- data.frame(c("Retweets", "Replies", "Official Account"), 
                                         c(aircanada_retweet_rate, aircanada_reply_rate, aircanada_offacct_rate))

colnames(aircanada_metadata_summary) <- c("Metric", "Value")
aircanada_metadata_summary$Airline = "Air Canada"

##### PORTER


porter_tweets <- read.csv("PorterT 18Mar2017 .csv")
porter_tweets$X <- NULL
porter_tweets$isReply <- ifelse((!is.na(porter_tweets$replyToSID) |
                                   !is.na(porter_tweets$replyToSN) |
                                   !is.na(porter_tweets$replyToUID)), TRUE, FALSE)

porter_tweets$isOfficialAccount <- porter_tweets$screenName == "porterairlines"

porter_retweet_rate <- sum(porter_tweets$isRetweet) / nrow(porter_tweets)
porter_reply_rate <- sum(porter_tweets$isReply) / nrow(porter_tweets)
#porter_favorited_rate <- sum(porter_tweets$favorited) / nrow(porter_tweets)
porter_offacct_rate <- sum(porter_tweets$isOfficialAccount) / nrow(porter_tweets)

porter_metadata_summary <- data.frame(c("Retweets", "Replies", "Official Account"), 
                                      c(porter_retweet_rate, porter_reply_rate, porter_offacct_rate))

colnames(porter_metadata_summary) <- c("Metric", "Value")
porter_metadata_summary$Airline = "Porter"

#### JETLINES

jetlines_tweets <- read.csv("jetlines_allMar18.csv")
jetlines_tweets$X <- NULL
jetlines_tweets$isReply <- ifelse((!is.na(jetlines_tweets$replyToSID) |
                                     !is.na(jetlines_tweets$replyToSN) |
                                     !is.na(jetlines_tweets$replyToUID)), TRUE, FALSE)

jetlines_tweets$isOfficialAccount <- jetlines_tweets$screenName == "CanadaJetlines"

jetlines_retweet_rate <- sum(jetlines_tweets$isRetweet) / nrow(jetlines_tweets)
jetlines_reply_rate <- sum(jetlines_tweets$isReply) / nrow(jetlines_tweets)
#jetlines_favorited_rate <- sum(jetlines_tweets$favorited) / nrow(jetlines_tweets)
jetlines_offacct_rate <- sum(jetlines_tweets$isOfficialAccount) / nrow(jetlines_tweets)

jetlines_metadata_summary <- data.frame(c("Retweets", "Replies", "Official Account"), 
                                        c(jetlines_retweet_rate, jetlines_reply_rate, jetlines_offacct_rate))

colnames(jetlines_metadata_summary) <- c("Metric", "Value")
jetlines_metadata_summary$Airline = "JetLines"



#### WESTJET

westjet_tweets <- read.csv("westjet_allMar18.csv")
westjet_tweets$X <- NULL
westjet_tweets$isReply <- ifelse((!is.na(westjet_tweets$replyToSID) |
                                    !is.na(westjet_tweets$replyToSN) |
                                    !is.na(westjet_tweets$replyToUID)), TRUE, FALSE)

westjet_tweets$isOfficialAccount <- westjet_tweets$screenName == "WestJet"

westjet_retweet_rate <- sum(westjet_tweets$isRetweet) / nrow(westjet_tweets)
westjet_reply_rate <- sum(westjet_tweets$isReply) / nrow(westjet_tweets)
#westjet_favorited_rate <- sum(westjet_tweets$favorited) / nrow(westjet_tweets)
westjet_offacct_rate <- sum(westjet_tweets$isOfficialAccount) / nrow(westjet_tweets)

westjet_metadata_summary <- data.frame(c("Retweets", "Replies", "Official Account"), 
                                       c(westjet_retweet_rate, westjet_reply_rate, westjet_offacct_rate))

colnames(westjet_metadata_summary) <- c("Metric", "Value")
westjet_metadata_summary$Airline = "WestJet"

airlines_metadata_summary <- rbind(sunwing_metadata_summary, 
                                   newleaf_metadata_summary, 
                                   aircanada_metadata_summary, 
                                   porter_metadata_summary,
                                   jetlines_metadata_summary,
                                   westjet_metadata_summary)

windowsFonts(ITC=windowsFont("ITC Officina Sans Std Book"))

ggplot(data=airlines_metadata_summary, aes(y=Value, x=Airline, label=Value)) + 
  geom_bar(aes(fill=Airline),data=airlines_metadata_summary, stat="identity")  +
  coord_flip() + 
  facet_grid(Metric ~ .) +
  scale_y_continuous(labels=percent, limits=c(0,1)) + 
  labs(title="Canadian Airlines - Twitter Metadata Analysis") + 
  geom_text(aes(x = Airline, y = Value, label = paste0(round(Value * 100,1),"%")), size=3, hjust=-0.1) + 
  theme_economist(base_family="ITC") + 
  scale_colour_economist() + 
  scale_fill_economist() + 
  theme(plot.title=element_text(family="ITC"),
        text=element_text(family="ITC"), legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) 


###### top users
library(dplyr)
library(ggplot2)
top10 <- westjet_all %>% count(screenName) %>% top_n(n=10)
plot31 <- ggplot(data= top10, aes(reorder(screenName, n),n)) +
  geom_bar(stat="identity", fill="midnightblue") +
  coord_flip() +
  labs(y="Number of Tweets", x="Screen Name",title="Most active Twitter Users WestJet")

top101 <- jetlines_all %>% count(screenName) %>% top_n(n=10)
plot32 <- ggplot(data= top101, aes(reorder(screenName, n),n)) +
  geom_bar(stat="identity", fill="midnightblue") +
  coord_flip() +
  labs(y="Number of Tweets", x="Screen Name",title="Most active Twitter Users Jetlines")

cowplot::plot_grid(plot31, plot32)

###### top hashtags

#keep only hashtags
westjet_hashtags <- lapply(as.character(westjet_all$text), function(x) strsplit(x, split = " "))
westjet_hashtags <- unlist(westjet_hashtags, recursive = TRUE)
westjet_hashtags <- westjet_hashtags[substr(westjet_hashtags,1,1) == "#"]
#westjet_hashtags <- gsub("[^0-9A-Za-z#\\\']", "" ,westjet_hashtags)

# creating a dataframe
westjet_hashtags <- data.frame(term = westjet_hashtags, freq=  1)

# summation over the freq
westjet_hashtags <- westjet_hashtags %>% group_by(term) %>% summarise(freq = n()) %>% top_n(n=20)

westjet_tophash <- ggplot(westjet_hashtags, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=10,face="bold")) + ggtitle("Westjet")


aircanada_hashtags <- lapply(as.character(aircanada_all$text), function(x) strsplit(x, split = " "))
aircanada_hashtags <- unlist(aircanada_hashtags, recursive = TRUE)
aircanada_hashtags <- aircanada_hashtags[substr(aircanada_hashtags,1,1) == "#"]
#aircanada_hashtags <- gsub("[^0-9A-Za-z#\\\']", "" ,aircanada_hashtags)

# creating a dataframe
aircanada_hashtags <- data.frame(term = aircanada_hashtags, freq=  1)

# summation over the freq
aircanada_hashtags <- aircanada_hashtags %>% group_by(term) %>% summarise(freq = n()) %>% top_n(n=20)

aircanada_tophash <- ggplot(aircanada_hashtags, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=10,face="bold")) + ggtitle("AirCanada")


jetlines_hashtags <- lapply(as.character(jetlines_all$text), function(x) strsplit(x, split = " "))
jetlines_hashtags <- unlist(jetlines_hashtags, recursive = TRUE)
jetlines_hashtags <- jetlines_hashtags[substr(jetlines_hashtags,1,1) == "#"]
#jetlines_hashtags <- gsub("[^0-9A-Za-z#\\\']", "" ,jetlines_hashtags)

# creating a dataframe
jetlines_hashtags <- data.frame(term = jetlines_hashtags, freq=  1)

# summation over the freq
jetlines_hashtags <- jetlines_hashtags %>% group_by(term) %>% summarise(freq = n()) %>% top_n(n=20)

jetlines_tophash <- ggplot(jetlines_hashtags, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=10,face="bold")) + ggtitle("Jetlines")


porter_hashtags <- lapply(as.character(porter_all$text), function(x) strsplit(x, split = " "))
porter_hashtags <- unlist(porter_hashtags, recursive = TRUE)
porter_hashtags <- porter_hashtags[substr(porter_hashtags,1,1) == "#"]
#porter_hashtags <- gsub("[^0-9A-Za-z#\\\']", "" ,porter_hashtags)

# creating a dataframe
porter_hashtags <- data.frame(term = porter_hashtags, freq=  1)

# summation over the freq
porter_hashtags <- porter_hashtags %>% group_by(term) %>% summarise(freq = n()) %>% top_n(n=20)

porter_tophash <- ggplot(porter_hashtags, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=10,face="bold")) + ggtitle("Porter")


newleaf_hashtags <- lapply(as.character(newleaf_all$text), function(x) strsplit(x, split = " "))
newleaf_hashtags <- unlist(newleaf_hashtags, recursive = TRUE)
newleaf_hashtags <- newleaf_hashtags[substr(newleaf_hashtags,1,1) == "#"]
#newleaf_hashtags <- gsub("[^0-9A-Za-z#\\\']", "" ,newleaf_hashtags)

# creating a dataframe
newleaf_hashtags <- data.frame(term = newleaf_hashtags, freq=  1)

# summation over the freq
newleaf_hashtags <- newleaf_hashtags %>% group_by(term) %>% summarise(freq = n()) %>% top_n(n=20)

newleaf_tophash <- ggplot(newleaf_hashtags, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=10,face="bold")) + ggtitle("Newleaf")


sunwing_hashtags <- lapply(as.character(sunwing_all$text), function(x) strsplit(x, split = " "))
sunwing_hashtags <- unlist(sunwing_hashtags, recursive = TRUE)
sunwing_hashtags <- sunwing_hashtags[substr(sunwing_hashtags,1,1) == "#"]
#sunwing_hashtags <- gsub("[^0-9A-Za-z#\\\']", "" ,sunwing_hashtags)

# creating a dataframe
sunwing_hashtags <- data.frame(term = sunwing_hashtags, freq=  1)

# summation over the freq
sunwing_hashtags <- sunwing_hashtags %>% group_by(term) %>% summarise(freq = n()) %>% top_n(n=20)

sunwing_tophash <- ggplot(sunwing_hashtags, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Most Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=10,face="bold")) + ggtitle("Sunwing")


cowplot::plot_grid(westjet_tophash, sunwing_tophash, newleaf_tophash, 
                   porter_tophash, aircanada_tophash, jetlines_tophash)


###### distribution of tweets in week days

westjet_all$group <- as.factor("WestJet")
jetlines_all$group <- as.factor("Jetlines")
aircanada_all$group <- as.factor("AirCanada")
porter_all$group <- as.factor("Porter")
sunwing_all$group <- as.factor("SunWing")
newleaf_all$group <- as.factor("Newleaf")


#Create one large data.frame for comparison
all_all <- rbind(westjet_all, jetlines_all, aircanada_all, porter_all, sunwing_all, newleaf_all)

#Use the lubridate package to make working with time easier.
all_all$date <- ymd_hms(all_all$created)


all_all$hour <- hour(all_all$date) + minute(all_all$date)/60
all_all$day <- weekdays(as.Date(all_all$date))
#rearrange the weekdays in chronological order
all_all$day <- factor(all_all$day, levels = c("Monday",
                                              "Tuesday", "Wednesday", "Thursday",
                                              "Friday", "Saturday", "Sunday"))
#This last re-factoring below just makes
#the colors in the plot correct.
#For your own groups you want to
#remove the line of code underneath.

all_all$group <- factor(all_all$group, levels = c("Jetlines", "WestJet", "Porter", 
                                                  "AirCanada", "SunWing", "Newleaf"))

ggplot(all_all, aes(hour, colour = group)) +
  geom_density()

ggplot(all_all, aes(hour, colour = group)) +
  geom_density() + facet_wrap(~day, nrow = 3)

ggplot(all_all, aes(hour, colour = group)) +
  geom_density() + facet_wrap(~day, nrow = 3, scales = "free_x")

#########################
#   Sentiment Analysis  #
#########################

#libraries
install.packages("qdap")
library(qdap)

### Emotional valence of tweets for westjet

# Split into retweets and original tweets
sp = split(westjet_all, westjet_all$isRetweet)
orig = sp[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt = mutate(sp[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

pol = 
  lapply(orig$text, function(txt) {
    # strip sentence senders so each tweet is analyzed as a sentence,
    # and +'s which muck up regex
    gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
      # strip URLs
      gsub(' http[^[:blank:]]+', '', .) %>%
      # calculate polarity
      polarity()
  })
orig$emotionalValence = sapply(pol, function(x) x$all$polarity)

orig$text[which.max(orig$emotionalValence)]

orig$text[which.min(orig$emotionalValence)]


# How does emotionalValence change over the day?
orig$created <- ymd_hms(orig$created)
westjet_plot1 <- ggplot(orig, aes(x=created, y=emotionalValence)) + 
  geom_smooth() + ggtitle("Westjet")

#westjet_plot <- ggplot(orig, aes(x = emotionalValence, y = retweetCount)) +
# geom_point(position = 'jitter') +
#geom_smooth() + ggtitle("Westjet")


# Emotional valence of tweets for Jetlines
sp_j = split(jetlines_all, jetlines_all$isRetweet)
orig_j = sp_j[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt_j = mutate(sp_j[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

pol_j = 
  lapply(orig_j$text, function(txt) {
    # strip sentence senders so each tweet is analyzed as a sentence,
    # and +'s which muck up regex
    gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
      # strip URLs
      gsub(' http[^[:blank:]]+', '', .) %>%
      # calculate polarity
      polarity()
  })
orig_j$emotionalValence = sapply(pol_j, function(x) x$all$polarity)

orig_j$text[which.max(orig_j$emotionalValence)]

orig_j$text[which.min(orig_j$emotionalValence)]

# How does emotionalValence change over the day?
orig_j$created <- ymd_hms(orig_j$created)
jetlines_plot1 <- ggplot(orig_j, aes(x=created, y=emotionalValence)) + 
  geom_smooth() + ggtitle("Jetlines")

#jetlines_plot <- ggplot(orig_j, aes(x = emotionalValence, y = retweetCount)) +
# geom_point(position = 'jitter') +
#geom_smooth() + ggtitle("Jetlines")


# Emotional valence of tweets for porter
sp_p = split(porter_all, porter_all$isRetweet)
orig_p = sp_p[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt_p = mutate(sp_p[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

pol_p = 
  lapply(orig_p$text, function(txt) {
    # strip sentence senders so each tweet is analyzed as a sentence,
    # and +'s which muck up regex
    gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
      # strip URLs
      gsub(' http[^[:blank:]]+', '', .) %>%
      # calculate polarity
      polarity()
  })
orig_p$emotionalValence = sapply(pol_p, function(x) x$all$polarity)
orig_p$text[which.max(orig_p$emotionalValence)]
orig_p$text[which.min(orig_p$emotionalValence)]

# How does emotionalValence change over the day?
orig_p$created <- ymd_hms(orig_p$created)
porter_plot1 <- ggplot(orig_p, aes(x=created, y=emotionalValence)) + 
  geom_smooth() + ggtitle("Porter")

#porter_plot <- ggplot(orig_p, aes(x = emotionalValence, y = retweetCount)) +
# geom_point(position = 'jitter') +
#geom_smooth() + ggtitle("Porter Airline")

# Emotional valence of tweets for newleaf
sp_n = split(newleaf_all, newleaf_all$isRetweet)
orig_n = sp_n[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt_n = mutate(sp_n[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

pol_n = 
  lapply(orig_n$text, function(txt) {
    # strip sentence senders so each tweet is analyzed as a sentence,
    # and +'s which muck up regex
    gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
      # strip URLs
      gsub(' http[^[:blank:]]+', '', .) %>%
      # calculate polarity
      polarity()
  })
orig_n$emotionalValence = sapply(pol_n, function(x) x$all$polarity)
orig_n$text[which.max(orig_n$emotionalValence)]
orig_n$text[which.min(orig_n$emotionalValence)]

# How does emotionalValence change over the day?
orig_n$created <- ymd_hms(orig_n$created)
newleaf_plot1 <- ggplot(orig_n, aes(x=created, y=emotionalValence)) + 
  geom_smooth() + ggtitle("Newleaf")

#newleaf_plot <- ggplot(orig_n, aes(x = emotionalValence, y = retweetCount)) +
# geom_point(position = 'jitter') +
#geom_smooth() + ggtitle("Newleaf Airline")


# Emotional valence of tweets for sunwing
sp_s = split(sunwing_all, sunwing_all$isRetweet)
orig_s = sp_s[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt_s = mutate(sp_s[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

pol_s = 
  lapply(orig_s$text, function(txt) {
    # strip sentence senders so each tweet is analyzed as a sentence,
    # and +'s which muck up regex
    gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
      # strip URLs
      gsub(' http[^[:blank:]]+', '', .) %>%
      # calculate polarity
      polarity()
  })
orig_s$emotionalValence = sapply(pol_s, function(x) x$all$polarity)
orig_s$text[which.max(orig_s$emotionalValence)]
orig_s$text[which.min(orig_s$emotionalValence)]

# How does emotionalValence change over the day?
orig_s$created <- ymd_hms(orig_s$created)
sunwing_plot1 <- ggplot(orig_s, aes(x=created, y=emotionalValence)) + 
  geom_smooth() + ggtitle("Sunwing")

#sunwing_plot <- ggplot(orig_s, aes(x = emotionalValence, y = retweetCount)) +
# geom_point(position = 'jitter') +
#geom_smooth() + ggtitle("Sunwing Airline")

# Emotional valence of tweets for AirCanada
sp_a = split(aircanada_all, aircanada_all$isRetweet)
orig_a = sp_a[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt_a = mutate(sp_a[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

pol_a = 
  lapply(orig_a$text, function(txt) {
    # strip sentence senders so each tweet is analyzed as a sentence,
    # and +'s which muck up regex
    gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
      # strip URLs
      gsub(' http[^[:blank:]]+', '', .) %>%
      # calculate polarity
      polarity()
  })
orig_a$emotionalValence = sapply(pol_a, function(x) x$all$polarity)
orig_a$text[which.max(orig_a$emotionalValence)]
orig_a$text[which.min(orig_a$emotionalValence)]

# How does emotionalValence change over the day?
orig_a$created <- ymd_hms(orig_a$created)
aircanada_plot1 <- ggplot(orig_a, aes(x=created, y=emotionalValence)) + 
  geom_smooth() + ggtitle("AirCanada")

#aircanada_plot <- ggplot(orig_a, aes(x = emotionalValence, y = retweetCount)) +
# geom_point(position = 'jitter') +
#geom_smooth() + ggtitle("AirCanada")


#cowplot::plot_grid(aircanada_plot, westjet_plot, jetlines_plot, porter_plot, 
#                  newleaf_plot, sunwing_plot)

cowplot::plot_grid(aircanada_plot1, westjet_plot1, jetlines_plot1, porter_plot1, 
                   newleaf_plot1, sunwing_plot1)




###### top frequent terms in top negative/positive tweets

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr); library(dplyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

#loading positive words
hu.liu.pos <- scan('positive-words.txt', what = 'character', comment.char = ';')

#loading negative words
hu.liu.neg <- scan('negative-words.txt', what = 'character', comment.char = ';')

# we can add industry specific lexicons
#pos.words <- c(hu.liu.pos, '')
#neg.words <- c(hu.liu.neg, '')

# calculating the setiment score
aircanada_score <- score.sentiment(aircanada_all$text, hu.liu.pos, hu.liu.neg)
westjet_score <- score.sentiment(westjet_all$text, hu.liu.pos, hu.liu.neg)
jetlines_score <- score.sentiment(jetlines_all$text, hu.liu.pos, hu.liu.neg)
porter_score <- score.sentiment(porter_all$text, hu.liu.pos, hu.liu.neg)
newleaf_score <- score.sentiment(newleaf_all$text, hu.liu.pos, hu.liu.neg)
sunwing_score <- score.sentiment(sunwing_all$text, hu.liu.pos, hu.liu.neg)

# adding airline labels to their datasets
westjet_score$group <- as.factor("WestJet")
jetlines_score$group <- as.factor("Jetlines")
aircanada_score$group <- as.factor("AirCanada")
porter_score$group <- as.factor("Porter")
sunwing_score$group <- as.factor("SunWing")
newleaf_score$group <- as.factor("Newleaf")

#combining all airlines together
score_all <- rbind(aircanada_score, westjet_score, newleaf_score, 
                   sunwing_score, porter_score, jetlines_score)


# sentiment score distribution
ggplot(data=score_all) + geom_bar(mapping = aes(x=score, fill=group), binwidth = 1) +
  facet_grid(group~.) + theme_bw() + scale_fill_brewer()

hist(porter_score$score)
ggplot(score_all, aes(score, colour = group)) + facet_grid(group~.) +
  geom_density() #+ facet_wrap(~day, nrow = 3, scales = "free_x")

# focusing on very negative and very positive tweets
score_all$very.pos <- as.numeric(score_all$score >= 2)
score_all$very.neg <- as.numeric(score_all$score <= -2)

# creating a summary of the count and score
twitter.df <- ddply(score_all, "group", summarise, pos.count = sum(very.pos), 
                    neg.count = sum(very.neg))

twitter.df$all.count <- twitter.df$pos.count + twitter.df$neg.count
twitter.df$score <- round(100 * twitter.df$pos.count / twitter.df$all.count)

# order by score
install.packages("doBy")
library(doBy)
orderBy(~-score, twitter.df)

# spliting very pos/neg datasets for airlines 
aircanada_pos <- subset(score_all, group == "AirCanada" & very.pos == 1)
aircanada_neg <- subset(score_all, group == "AirCanada" & very.neg == 1)

porter_pos <- subset(score_all, group == "Porter" & very.pos == 1)
porter_neg <- subset(score_all, group == "Porter" & very.neg == 1)

sunwing_pos <- subset(score_all, group == "SunWing" & very.pos == 1)
sunwing_neg <- subset(score_all, group == "SunWing" & very.neg == 1)

newleaf_pos <- subset(score_all, group == "Newleaf" & very.pos == 1)
newleaf_neg <- subset(score_all, group == "Newleaf" & very.neg == 1)

westjet_pos <- subset(score_all, group == "WestJet" & very.pos == 1)
westjet_neg <- subset(score_all, group == "WestJet" & very.neg == 1)

jetlines_pos <- subset(score_all, group == "Jetlines" & very.pos == 1)
jetlines_neg <- subset(score_all, group == "Jetlines" & very.neg == 1)


# top 10 word
textScrubber <- function(dataframe) {
  
  dataframe$text <- gsub("-", " ", dataframe$text)
  dataframe$text <- gsub("&", " ", dataframe$text)
  dataframe$text <- gsub("[[:punct:]]", " ", dataframe$text)
  dataframe$text <- gsub("[[:digit:]]", " ", dataframe$text)
  dataframe$text <- gsub("http\\w+", " ", dataframe$text)
  dataframe$text <- gsub("\n", " ", dataframe$text)
  dataframe$text <- gsub("[ \t]{2,}", " ", dataframe$text)
  dataframe$text <- gsub("^\\s+|\\s+$", " ", dataframe$text)
  #dataframe$text <- gsub("[^0-9A-Za-z#\\\']", "", dataframe$text)
  dataframe$text <- tolower(dataframe$text)
  
  return(dataframe)
}

# scrubbing
westjetp_scrubbed <- textScrubber(westjet_pos)
westjetn_scrubbed <- textScrubber(westjet_neg)

aircanadap_scrubbed <- textScrubber(aircanada_pos)
aircanadan_scrubbed <- textScrubber(aircanada_neg)

sunwingp_scrubbed <- textScrubber(sunwing_pos)
sunwingn_scrubbed <- textScrubber(sunwing_neg)

porterp_scrubbed <- textScrubber(porter_pos)
portern_scrubbed <- textScrubber(porter_neg)

jetlinesp_scrubbed <- textScrubber(jetlines_pos)
jetlinesn_scrubbed <- textScrubber(jetlines_neg)

newleafp_scrubbed <- textScrubber(newleaf_pos)
newleafn_scrubbed <- textScrubber(newleaf_neg)

tdmCreator <- function(dataframe, stemDoc = F, rmStopwords = T){
  
  tdm <- Corpus(VectorSource(dataframe$text))
  if (isTRUE(rmStopwords)) {
    tdm <- tm_map(tdm, removeWords, c(stopwords(),"can", "amp", "dfftkal", "uibepqmzwk", "cbcnb",
                                      "yyjvevpdos", "ixmlxsv", "newleaftravel", "sunwing",
                                      "maarten", "westjet", "aircanada", "canadajetlines", 
                                      "porterairlines", "hu...", "acjamiefox", "sunwingairiines",
                                      "mark", "jet", "lanaparrilla", "canada")) 
  }
  if (isTRUE(stemDoc)) {
    tdm <- tm_map(tdm, stemDocument)
  }
  tdm <- TermDocumentMatrix(tdm,
                            control = list(wordLengths = c(3, Inf))) 
  tdm <- rowSums(as.matrix(tdm))
  tdm <- sort(tdm, decreasing = T)
  df <- data.frame(term = names(tdm), freq = tdm)
  return(df)
}

#install.packages("SnowballC")
#library(SnowballC)

westjetp_cleaned <- tdmCreator(westjetp_scrubbed)
westjetp_top10 <- westjetp_cleaned[1:15,]
westjetn_cleaned <- tdmCreator(westjetn_scrubbed)
westjetn_top10 <- westjetn_cleaned[1:15,]

jetlinesp_cleaned <- tdmCreator(jetlinesp_scrubbed)
jetlinesp_top10 <- jetlinesp_cleaned[1:15,]
jetlinesn_cleaned <- tdmCreator(jetlinesn_scrubbed)
jetlinesn_top10 <- jetlinesn_cleaned[1:15,]

aircanadap_cleaned <- tdmCreator(aircanadap_scrubbed)
aircanadap_top10 <- aircanadap_cleaned[1:15,]
aircanadan_cleaned <- tdmCreator(aircanadan_scrubbed)
aircanadan_top10 <- aircanadan_cleaned[1:15,]

porterp_cleaned <- tdmCreator(porterp_scrubbed)
porterp_top10 <- porterp_cleaned[1:15,]
portern_cleaned <- tdmCreator(portern_scrubbed)
portern_top10 <- portern_cleaned[1:15,]

sunwingp_cleaned <- tdmCreator(sunwingp_scrubbed)
sunwingp_top10 <- sunwingp_cleaned[1:15,]
sunwingn_cleaned <- tdmCreator(sunwingn_scrubbed)
sunwingn_top10 <- sunwingn_cleaned[1:15,]

newleafp_cleaned <- tdmCreator(newleafp_scrubbed)
newleafp_top10 <- newleafp_cleaned[1:15,]
newleafn_cleaned <- tdmCreator(newleafn_scrubbed)
newleafn_top10 <- newleafn_cleaned[1:15,]

# graphs
westjetn_plot <- ggplot(westjetn_top10, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "red") +
  xlab("Most Negative Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold")) + ggtitle("WestJet")

westjetp_plot <- ggplot(westjetp_top10, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "green") +
  xlab("Most Positive Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold")) + ggtitle("WestJet")

grid.arrange(westjetn_plot, westjetp_plot, ncol=2)

aircanadan_plot <- ggplot(aircanadan_top10, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "red") +
  xlab("Most Negative Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold")) + ggtitle("AirCanada")

aircanadap_plot <- ggplot(aircanadap_top10, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "green") +
  xlab("Most Positive Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold")) + ggtitle("AirCanada")

grid.arrange(aircanadan_plot, aircanadap_plot, ncol=2)

portern_plot <- ggplot(portern_top10, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "red") +
  xlab("Most Negative Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold")) + ggtitle("Porter")

porterp_plot <- ggplot(porterp_top10, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "green") +
  xlab("Most Positive Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold")) + ggtitle("Porter")

grid.arrange(portern_plot, porterp_plot,ncol=2)

jetlinesn_plot <- ggplot(jetlinesn_top10, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "red") +
  xlab("Most Negative Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold")) + ggtitle("Jetlines")

jetlinesp_plot <- ggplot(jetlinesp_top10, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "green") +
  xlab("Most Positive Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold")) + ggtitle("Jetlines")

grid.arrange(jetlinesn_plot, jetlinesp_plot
             , ncol=2)

newleafn_plot <- ggplot(newleafn_top10, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "red") +
  xlab("Most Negative Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold")) + ggtitle("Newleaf")

newleafp_plot <- ggplot(newleafp_top10, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "green") +
  xlab("Most Positive Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold")) + ggtitle("Newleaf")

grid.arrange(newleafn_plot, newleafp_plot, ncol=2)

sunwingn_plot <- ggplot(sunwingn_top10, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "red") +
  xlab("Most Negative Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold")) + ggtitle("Sunwing")

sunwingp_plot <- ggplot(sunwingp_top10, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "green") +
  xlab("Most Positive Used") + ylab("How Often") +
  coord_flip() + theme(text=element_text(size=25,face="bold")) + ggtitle("Sunwing")

grid.arrange(sunwingn_plot, sunwingp_plot
             ,ncol=2)



####################
# Network Analysis # 
####################

# Static retweet network

######### westjet airlines #############

# Split into retweets and original tweets
sp_w = split(westjet_all, westjet_all$isRetweet)
orig_w = sp_w[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt_w = mutate(sp_w[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

# Adjust retweets to create an edgelist for network
el_w = as.data.frame(cbind(sender = tolower(rt_w$sender), 
                           receiver = tolower(rt_w$screenName)))
el_w = count(el_w, sender, receiver) 
rtnet_w = network(el_w, matrix.type = 'edgelist', directed = TRUE, 
                  ignore.eval = FALSE, names.eval = 'num')

# Get names of only those who were retweeted to keep labeling reasonable
vlabs_w = rtnet_w %v% 'vertex.names'
vlabs_w[degree(rtnet_w, cmode = 'outdegree') == 0] = NA

par(mar = c(0, 0, 3, 0))
plot(rtnet_w, label = vlabs_w, label.pos = 5, label.cex = .5, 
     vertex.cex = log(degree(rtnet_w)) + .5, vertex.col = 3,
     edge.lwd = 'num', edge.col = 'gray70', main = 'WestJet Retweet Network')

######### Porter Airlines #########

# Split into retweets and original tweets
sp_p = split(porter_all, porter_all$isRetweet)
orig_p = sp_p[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt_p = mutate(sp_p[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

# Adjust retweets to create an edgelist for network
el_p = as.data.frame(cbind(sender = tolower(rt_p$sender), 
                           receiver = tolower(rt_p$screenName)))
el_p = count(el_p, sender, receiver) 
rtnet_p = network(el_p, matrix.type = 'edgelist', directed = TRUE, 
                  ignore.eval = FALSE, names.eval = 'num')

# Get names of only those who were retweeted to keep labeling reasonable
vlabs_p = rtnet_p %v% 'vertex.names'
vlabs_p[degree(rtnet_p, cmode = 'outdegree') == 0] = NA

par(mar = c(0, 0, 3, 0))
plot(rtnet_p, label = vlabs_p, label.pos = 5, label.cex = .6, 
     vertex.cex = log(degree(rtnet_p)) + .2, vertex.col = 3,
     edge.lwd = 'num', edge.col = 'gray70', main = 'Porter Airlines Retweet Network')



######### newleaf Airlines #########

# Split into retweets and original tweets
sp_n = split(newleaf_all, newleaf_all$isRetweet)
orig_n = sp_n[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt_n = mutate(sp_n[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

# Adjust retweets to create an edgelist for network
el_n = as.data.frame(cbind(sender = tolower(rt_n$sender), 
                           receiver = tolower(rt_n$screenName)))
el_n = count(el_n, sender, receiver) 
rtnet_n = network(el_n, matrix.type = 'edgelist', directed = TRUE, 
                  ignore.eval = FALSE, names.eval = 'num')

# Get names of only those who were retweeted to keep labeling reasonable
vlabs_n = rtnet_n %v% 'vertex.names'
vlabs_n[degree(rtnet_n, cmode = 'outdegree') == 0] = NA

par(mar = c(0, 0, 3, 0))
plot(rtnet_n, label = vlabs_n, label.pos = 5, label.cex = .6, 
     vertex.cex = log(degree(rtnet_n)) + .2, vertex.col = 3,
     edge.lwd = 'num', edge.col = 'gray70', main = 'Newleaf Airlines Retweet Network')



######### sunwing Airlines #########

# Split into retweets and original tweets
sp_s = split(sunwing_all, sunwing_all$isRetweet)
orig_s = sp_s[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt_s = mutate(sp_s[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

# Adjust retweets to create an edgelist for network
el_s = as.data.frame(cbind(sender = tolower(rt_s$sender), 
                           receiver = tolower(rt_s$screenName)))
el_s = count(el_s, sender, receiver) 
rtnet_s = network(el_s, matrix.type = 'edgelist', directed = TRUE, 
                  ignore.eval = FALSE, names.eval = 'num')

# Get names of only those who were retweeted to keep labeling reasonable
vlabs_s = rtnet_s %v% 'vertex.names'
vlabs_s[degree(rtnet_s, cmode = 'outdegree') == 0] = NA

par(mar = c(0, 0, 3, 0))
plot(rtnet_s, label = vlabs_s, label.pos = 5, label.cex = .6, 
     vertex.cex = log(degree(rtnet_s)) + .2, vertex.col = 3,
     edge.lwd = 'num', edge.col = 'gray70', main = 'Sunwing Airlines Retweet Network')


######### sunwing Airlines #########

# Split into retweets and original tweets
sp_j = split(jetlines_all, jetlines_all$isRetweet)
orig_j = sp_s[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt_j = mutate(sp_j[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

# Adjust retweets to create an edgelist for network
el_j = as.data.frame(cbind(sender = tolower(rt_j$sender), 
                           receiver = tolower(rt_j$screenName)))
el_j = count(el_j, sender, receiver) 
rtnet_j = network(el_j, matrix.type = 'edgelist', directed = TRUE, 
                  ignore.eval = FALSE, names.eval = 'num')

# Get names of only those who were retweeted to keep labeling reasonable
vlabs_j = rtnet_j %v% 'vertex.names'
vlabs_j[degree(rtnet_j, cmode = 'outdegree') == 0] = NA

par(mar = c(0, 0, 3, 0))
plot(rtnet_j, label = vlabs_j, label.pos = 5, label.cex = 0.6, 
     vertex.cex = log(degree(rtnet_j)) + .2, vertex.col = 3,
     edge.lwd = 'num', edge.col = 'gray70', main = 'Jetlines Retweet Network')


######### AirCanada #########

# Split into retweets and original tweets
sp_a = split(aircanada_all, aircanada_all$isRetweet)
orig_a = sp_a[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt_a = mutate(sp_a[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

# Adjust retweets to create an edgelist for network
el_a = as.data.frame(cbind(sender = tolower(rt_a$sender), 
                           receiver = tolower(rt_a$screenName)))
el_a = count(el_a, sender, receiver) 
rtnet_a = network(el_a, matrix.type = 'edgelist', directed = TRUE, 
                  ignore.eval = FALSE, names.eval = 'num')

# Get names of only those who were retweeted to keep labeling reasonable
vlabs_a = rtnet_a %v% 'vertex.names'
vlabs_a[degree(rtnet_a, cmode = 'indegree') == 0] = NA

par(mar = c(0, 0, 3, 0))
plot(rtnet_a, label = vlabs_a, label.pos = 5, label.cex = .6, 
     vertex.cex = log(degree(rtnet_a)) + .2, vertex.col = 3,
     edge.lwd = 'num', edge.col = 'gray70', main = 'AirCanada Retweet Network')



##### interactive network graph

install.packages("igraph")
library(igraph)
library(magrittr)
install.packages("visNetwork")
library(visNetwork)
library(data.table)


airline <- aircanada_all
airline <- newleaf_all
airline <- jetlines_all
airline <- porter_all


# Split into retweets and original tweets
sp = split(airline, airline$isRetweet)
orig = sp[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt = mutate(sp[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

el = as.data.frame(cbind(sender = tolower(rt$sender), 
                         receiver = tolower(rt$screenName)))
el = count(el, sender, receiver) 

graph <- graph.data.frame(el, directed=T)

graph <- simplify(graph)

V(graph)$indegree <- centr_degree(graph, mode = "in")$res

nodes <- get.data.frame(graph, what="vertices")
nodes <- data.frame(id = nodes$name, title = nodes$name, group = nodes$indegree, indegree = nodes$indegree)
setnames(nodes, "indegree", "in-degree centrality")
nodes <- nodes[order(nodes$id, decreasing = F),]

edges <- get.data.frame(graph, what="edges")[1:2]

visNetwork(nodes, edges, height = "500px", width = "100%") %>%
  visOptions(selectedBy = "in-degree centrality", highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
  visPhysics(stabilization = FALSE) %>% visEdges(smooth = FALSE) #%>% visStabilize(iterations = 1)



airline <- aircanada_all
airline <- newleaf_all
airline <- jetlines_all
airline <- porter_all
airline <- westjet_all
airline <- sunwing_all


# Split into retweets and original tweets
sp = split(airline, airline$isRetweet)
orig = sp[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt = mutate(sp[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

el = as.data.frame(cbind(sender = tolower(rt$sender), 
                         receiver = tolower(rt$screenName)))
el = count(el, sender, receiver) 

graph <- graph.data.frame(el, directed=T)

graph <- simplify(graph)

V(graph)$outdegree <- centr_degree(graph, mode = "out")$res

nodes <- get.data.frame(graph, what="vertices")
nodes <- data.frame(id = nodes$name, title = nodes$name, group = nodes$outdegree, outdegree = nodes$outdegree)
setnames(nodes, "outdegree", "out-degree centrality")
nodes <- nodes[order(nodes$id, decreasing = F),]

edges <- get.data.frame(graph, what="edges")[1:2]

visNetwork(nodes, edges, height = "500px", width = "100%") %>%
  visOptions(selectedBy = "out-degree centrality", highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
  visPhysics(stabilization = FALSE) %>% visEdges(smooth = FALSE) #%>% visStabilize(iterations = 1)

