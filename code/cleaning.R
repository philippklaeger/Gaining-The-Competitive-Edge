###########################################################
#                 C L E A N I N G                         #
###########################################################



# 1. Set Working Directory / Workspace



###########################################################


# 2. Loading Packages
library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(tidytext)
library(textstem)
library(syuzhet)
library(lubridate)
library(stm)
library(stringr)
library(quanteda)


###########################################################


# 3. Loading Data
## 3.1 Political Marketing
AfD_raw <- read.csv(file = './data/AfD_raw.csv', sep = ",", fileEncoding = "UTF-8")
AfD_raw <- as.data.frame(AfD_raw)
CDU_raw <- read.csv(file = './data/CDU_raw.csv', sep = ",", fileEncoding = "UTF-8")
CDU_raw <- as.data.frame(CDU_raw)
FDP_raw <- read.csv(file = './data/FDP_raw.csv', sep = ",", fileEncoding = "UTF-8")
FDP_raw <- as.data.frame(FDP_raw)
Gruene_raw <- read.csv(file = './data/Gruene_raw.csv', sep = ",", fileEncoding = "UTF-8")
Gruene_raw <- as.data.frame(Gruene_raw)
Linke_raw <- read.csv(file = './data/Linke_raw.csv', sep = ",", fileEncoding = "UTF-8")
Linke_raw <- as.data.frame(Linke_raw)
SPD_raw <- read.csv(file = './data/SPD_raw.csv', sep = ",", fileEncoding = "UTF-8")
SPD_raw <- as.data.frame(SPD_raw)

## 3.2 User-Generated Discussions
Discussions_1_raw <- read.csv(file = './data/Discussions_1_raw.csv', sep = ",", fileEncoding = "UTF-8")
Discussions_1_raw <- as.data.frame(Discussions_1_raw)
Discussions_2_raw <- read.csv(file = './data/Discussions_2_raw.csv', sep = ",", fileEncoding = "UTF-8")
Discussions_2_raw <- as.data.frame(Discussions_2_raw)
Discussions_3_raw <- read.csv(file = './data/Discussions_3_raw.csv', sep = ",", fileEncoding = "UTF-8")
Discussions_3_raw <- as.data.frame(Discussions_3_raw)
Discussions_4_raw <- read.csv(file = './data/Discussions_4_raw.csv', sep = ",", fileEncoding = "UTF-8")
Discussions_4_raw <- as.data.frame(Discussions_4_raw)


###########################################################


# 4. Subset I - Columns
## Removing unnecessary columns and creating a variable to identify the party easily
AfD <- subset(AfD_raw, select = c(user_id, screen_name, created_at, text, display_text_width,
                                  reply_to_status_id, reply_to_user_id, reply_to_screen_name,
                                  is_quote, is_retweet, favorite_count, retweet_count,
                                  hashtags, urls_url, media_url, media_type, ext_media_url,
                                  mentions_user_id, mentions_screen_name, retweet_status_id,
                                  retweet_text, retweet_created_at, retweet_favorite_count,
                                  retweet_retweet_count, retweet_user_id, retweet_screen_name,
                                  retweet_name, retweet_followers_count, retweet_friends_count,
                                  retweet_location, retweet_verified, name, description,
                                  followers_count, friends_count, account_created_at, verified))
AfD <- mutate(AfD, party_id = "AfD")

CDU <- subset(CDU_raw, select = c(user_id, screen_name, created_at, text, display_text_width,
                                  reply_to_status_id, reply_to_user_id, reply_to_screen_name,
                                  is_quote, is_retweet, favorite_count, retweet_count,
                                  hashtags, urls_url, media_url, media_type, ext_media_url,
                                  mentions_user_id, mentions_screen_name, retweet_status_id,
                                  retweet_text, retweet_created_at, retweet_favorite_count,
                                  retweet_retweet_count, retweet_user_id, retweet_screen_name,
                                  retweet_name, retweet_followers_count, retweet_friends_count,
                                  retweet_location, retweet_verified, name, description,
                                  followers_count, friends_count, account_created_at, verified))
CDU <- mutate(CDU, party_id = "CDU")

FDP <- subset(FDP_raw, select = c(user_id, screen_name, created_at, text, display_text_width,
                                  reply_to_status_id, reply_to_user_id, reply_to_screen_name,
                                  is_quote, is_retweet, favorite_count, retweet_count,
                                  hashtags, urls_url, media_url, media_type, ext_media_url,
                                  mentions_user_id, mentions_screen_name, retweet_status_id,
                                  retweet_text, retweet_created_at, retweet_favorite_count,
                                  retweet_retweet_count, retweet_user_id, retweet_screen_name,
                                  retweet_name, retweet_followers_count, retweet_friends_count,
                                  retweet_location, retweet_verified, name, description,
                                  followers_count, friends_count, account_created_at, verified))
FDP <- mutate(FDP, party_id = "FDP")

Gruene <- subset(Gruene_raw, select = c(user_id, screen_name, created_at, text, display_text_width,
                                        reply_to_status_id, reply_to_user_id, reply_to_screen_name,
                                        is_quote, is_retweet, favorite_count, retweet_count,
                                        hashtags, urls_url, media_url, media_type, ext_media_url,
                                        mentions_user_id, mentions_screen_name, retweet_status_id,
                                        retweet_text, retweet_created_at, retweet_favorite_count,
                                        retweet_retweet_count, retweet_user_id, retweet_screen_name,
                                        retweet_name, retweet_followers_count, retweet_friends_count,
                                        retweet_location, retweet_verified, name, description,
                                        followers_count, friends_count, account_created_at, verified))
Gruene <- mutate(Gruene, party_id = "Gruene")

Linke <- subset(Linke_raw, select = c(user_id, screen_name, created_at, text, display_text_width,
                                      reply_to_status_id, reply_to_user_id, reply_to_screen_name,
                                      is_quote, is_retweet, favorite_count, retweet_count,
                                      hashtags, urls_url, media_url, media_type, ext_media_url,
                                      mentions_user_id, mentions_screen_name, retweet_status_id,
                                      retweet_text, retweet_created_at, retweet_favorite_count,
                                      retweet_retweet_count, retweet_user_id, retweet_screen_name,
                                      retweet_name, retweet_followers_count, retweet_friends_count,
                                      retweet_location, retweet_verified, name, description,
                                      followers_count, friends_count, account_created_at, verified))
Linke <- mutate(Linke, party_id = "Linke")

SPD <- subset(SPD_raw, select = c(user_id, screen_name, created_at, text, display_text_width,
                                  reply_to_status_id, reply_to_user_id, reply_to_screen_name,
                                  is_quote, is_retweet, favorite_count, retweet_count,
                                  hashtags, urls_url, media_url, media_type, ext_media_url,
                                  mentions_user_id, mentions_screen_name, retweet_status_id,
                                  retweet_text, retweet_created_at, retweet_favorite_count,
                                  retweet_retweet_count, retweet_user_id, retweet_screen_name,
                                  retweet_name, retweet_followers_count, retweet_friends_count,
                                  retweet_location, retweet_verified, name, description,
                                  followers_count, friends_count, account_created_at, verified))
SPD <- mutate(SPD, party_id = "SPD")

Discussions_1 <- subset(Discussions_1_raw, select = c(query,
                                                      user_id, screen_name, created_at, text, display_text_width,
                                                      reply_to_status_id, reply_to_user_id, reply_to_screen_name,
                                                      is_quote, is_retweet, favorite_count, retweet_count,
                                                      hashtags, urls_url, media_url, media_type, ext_media_url,
                                                      mentions_user_id, mentions_screen_name, retweet_status_id,
                                                      retweet_text, retweet_created_at, retweet_favorite_count,
                                                      retweet_retweet_count, retweet_user_id, retweet_screen_name,
                                                      retweet_name, retweet_followers_count, retweet_friends_count,
                                                      retweet_location, retweet_verified))

Discussions_2 <- subset(Discussions_2_raw, select = c(query,
                                                      user_id, screen_name, created_at, text, display_text_width,
                                                      reply_to_status_id, reply_to_user_id, reply_to_screen_name,
                                                      is_quote, is_retweet, favorite_count, retweet_count,
                                                      hashtags, urls_url, media_url, media_type, ext_media_url,
                                                      mentions_user_id, mentions_screen_name, retweet_status_id,
                                                      retweet_text, retweet_created_at, retweet_favorite_count,
                                                      retweet_retweet_count, retweet_user_id, retweet_screen_name,
                                                      retweet_name, retweet_followers_count, retweet_friends_count,
                                                      retweet_location, retweet_verified))

Discussions_3 <- subset(Discussions_3_raw, select = c(query,
                                                      user_id, screen_name, created_at, text, display_text_width,
                                                      reply_to_status_id, reply_to_user_id, reply_to_screen_name,
                                                      is_quote, is_retweet, favorite_count, retweet_count,
                                                      hashtags, urls_url, media_url, media_type, ext_media_url,
                                                      mentions_user_id, mentions_screen_name, retweet_status_id,
                                                      retweet_text, retweet_created_at, retweet_favorite_count,
                                                      retweet_retweet_count, retweet_user_id, retweet_screen_name,
                                                      retweet_name, retweet_followers_count, retweet_friends_count,
                                                      retweet_location, retweet_verified))

Discussions_4 <- subset(Discussions_4_raw, select = c(query,
                                                      user_id, screen_name, created_at, text, display_text_width,
                                                      reply_to_status_id, reply_to_user_id, reply_to_screen_name,
                                                      is_quote, is_retweet, favorite_count, retweet_count,
                                                      hashtags, urls_url, media_url, media_type, ext_media_url,
                                                      mentions_user_id, mentions_screen_name, retweet_status_id,
                                                      retweet_text, retweet_created_at, retweet_favorite_count,
                                                      retweet_retweet_count, retweet_user_id, retweet_screen_name,
                                                      retweet_name, retweet_followers_count, retweet_friends_count,
                                                      retweet_location, retweet_verified))


###########################################################


# 5. Merge
Polit_Marketing <- rbind(AfD, CDU, FDP, Gruene, Linke, SPD)
User_Generated_Disc <- rbind(Discussions_1, Discussions_2, Discussions_3, Discussions_4)


###########################################################


# 6. Subset II - Time
## Filtering for campaign term (Dec 8th 2020 - Sep 26th 2021) only
Political_Marketing <- Polit_Marketing[Polit_Marketing$created_at > "2020-12-08 00:00:00" 
                                       & Polit_Marketing$created_at < "2021-09-26 00:00:00", ]
User_Generated_Discussions <- User_Generated_Disc[User_Generated_Disc$created_at > "2020-12-08 00:00:00" 
                                                  & User_Generated_Disc$created_at < "2021-09-26 00:00:00", ]


###########################################################


# 7. Text Cleaning
Political_Marketing <-
  Political_Marketing %>%
  mutate(
    text = str_remove_all(text, "http\\S*"),
    text = str_remove_all(text, "@\\S*"),
    text = str_remove_all(text, "&S*"),
    text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"),
    text = str_replace_all(text, "<a(.*?)>", " "),
    text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),
    text = str_replace_all(text, "&#[:digit:]+;", " "),
    text = str_remove_all(text, "<[^>]*>"),
    text = str_remove_all(text, "[:digit:]"),
    text = str_squish(text),
    text = str_trim(text)
  )

User_Generated_Discussions <-
  User_Generated_Discussions %>%
  mutate(
    text = str_remove_all(text, "http\\S*"),
    text = str_remove_all(text, "@\\S*"),
    text = str_remove_all(text, "&S*"),
    text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"),
    text = str_replace_all(text, "<a(.*?)>", " "),
    text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),
    text = str_replace_all(text, "&#[:digit:]+;", " "),
    text = str_remove_all(text, "<[^>]*>"),
    text = str_remove_all(text, "[:digit:]"),
    text = str_squish(text),
    text = str_trim(text)
  )



###########################################################


# 8. ID Variables
## 8.1 Political Marketing
Political_Marketing$AfD <- ifelse(Political_Marketing$party_id == 'AfD', 1, 0)
Political_Marketing$CDU <- ifelse(Political_Marketing$party_id == 'CDU', 1, 0)
Political_Marketing$FDP <- ifelse(Political_Marketing$party_id == 'FDP', 1, 0)
Political_Marketing$Gruene <- ifelse(Political_Marketing$party_id == 'Gruene', 1, 0)
Political_Marketing$Linke <- ifelse(Political_Marketing$party_id == 'Linke', 1, 0)
Political_Marketing$SPD <- ifelse(Political_Marketing$party_id == 'SPD', 1, 0)

Political_Marketing <- mutate(Political_Marketing, election_id = case_when(
  party_id == "SPD" ~ 'Winner',
  party_id == "Gruene" ~ 'Winner',
  party_id == "FDP" ~ 'Winner',
  party_id == "CDU" ~ 'Loser',
  party_id == "AfD" ~ 'Loser',
  party_id == "Linke" ~ 'Loser'))

Political_Marketing <- mutate(Political_Marketing, campaign_id = case_when(
  party_id == "SPD" ~ 'gained',
  party_id == "Gruene" ~ 'lost',
  party_id == "FDP" ~ 'gained',
  party_id == "CDU" ~ 'lost',
  party_id == "AfD" ~ 'gained',
  party_id == "Linke" ~ 'lost'))

## 8.2 User-Generated Discussions
### 8.2.1 Party IDs
User_Generated_Discussions <- mutate(User_Generated_Discussions, party_id = case_when(
  str_detect(User_Generated_Discussions$text, "AfD|AlternativefürDeutschland|AlternativefuerDeutschland|AliceWeidel|Weidel|TinoChrupalla|Chrupalla") ~ 'AfD',
  str_detect(User_Generated_Discussions$hashtags, "AfD|AlternativefürDeutschland|AlternativefuerDeutschland|AliceWeidel|Weidel|TinoChrupalla|Chrupalla") ~ 'AfD',
  str_detect(User_Generated_Discussions$text, "CDU|CSU|CDU/CSU|ChristdemokratischeUnion|Christdemokraten|Union|ArminLaschet|Laschet|MarkusSöder|Söder|AlexanderDobrindt|Dobrindt") ~ 'CDU',
  str_detect(User_Generated_Discussions$hashtags, "CDU|CSU|CDU/CSU|ChristdemokratischeUnion|Christdemokraten|Union|ArminLaschet|Laschet|MarkusSöder|Söder|AlexanderDobrindt|Dobrindt") ~ 'CDU',
  str_detect(User_Generated_Discussions$text, "FDP|FreieDemokratischePartei|Liberalen|ChristianLindner|Lindner") ~ 'FDP',
  str_detect(User_Generated_Discussions$hashtags, "FDP|FreieDemokratischePartei|Liberalen|ChristianLindner|Lindner") ~ 'FDP',
  str_detect(User_Generated_Discussions$text, "Grünen|Bündnis90|Gruenen|Buendnis90|DieGrünen|DieGruenen|AnnalenaBaerbock|Baerbock|Baer|RobertHabeck|Habeck") ~ 'Gruene',
  str_detect(User_Generated_Discussions$hashtags, "Grünen|Bündnis90|Gruenen|Buendnis90|DieGrünen|DieGruenen|AnnalenaBaerbock|Baerbock|Baer|RobertHabeck|Habeck") ~ 'Gruene',
  str_detect(User_Generated_Discussions$text, "Linke|DieLinke|DIELINKE.|Linkspartei|JanineWissler|Wissler|DietmarBartsch|Bartsch") ~ 'Linke',
  str_detect(User_Generated_Discussions$hashtags, "Linke|DieLinke|DIELINKE.|Linkspartei|JanineWissler|Wissler|DietmarBartsch|Bartsch") ~ 'Linke',
  str_detect(User_Generated_Discussions$text, "SPD|SozialdemokratischePartei|Sozialdemokraten|Sozis|OlafScholz|Scholz|Scholzomat") ~ 'SPD',
  str_detect(User_Generated_Discussions$hashtags, "SPD|SozialdemokratischePartei|Sozialdemokraten|Sozis|OlafScholz|Scholz|Scholzomat") ~ 'SPD'))

User_Generated_Discussions$party_id[is.na(User_Generated_Discussions$party_id)] <- 'None/Other'
User_Generated_Discussions$AfD <- ifelse(User_Generated_Discussions$party_id == 'AfD', 1, 0)
User_Generated_Discussions$CDU <- ifelse(User_Generated_Discussions$party_id == 'CDU', 1, 0)
User_Generated_Discussions$FDP <- ifelse(User_Generated_Discussions$party_id == 'FDP', 1, 0)
User_Generated_Discussions$Gruene <- ifelse(User_Generated_Discussions$party_id == 'Gruene', 1, 0)
User_Generated_Discussions$Linke <- ifelse(User_Generated_Discussions$party_id == 'Linke', 1, 0)
User_Generated_Discussions$SPD <- ifelse(User_Generated_Discussions$party_id == 'SPD', 1, 0)
User_Generated_Discussions$None <- ifelse(User_Generated_Discussions$party_id == 'None', 1, 0)

### 8.2.2 Candidate IDs
User_Generated_Discussions <- mutate(User_Generated_Discussions, candidate_id = case_when(
  str_detect(User_Generated_Discussions$text, "AliceWeidel|Weidel") ~ 'Weidel',
  str_detect(User_Generated_Discussions$text, "TinoChrupalla|Chrupalla") ~ 'Chrupalla',
  str_detect(User_Generated_Discussions$hashtags, "AliceWeidel|Weidel") ~ 'Weidel',
  str_detect(User_Generated_Discussions$hashtags, "TinoChrupalla|Chrupalla") ~ 'Chrupalla',
  str_detect(User_Generated_Discussions$text, "ArminLaschet|Laschet") ~ 'Laschet',
  str_detect(User_Generated_Discussions$text, "AlexanderDobrindt|Dobrindt") ~ 'Dobrindt',
  str_detect(User_Generated_Discussions$hashtags, "ArminLaschet|Laschet") ~ 'Laschet',
  str_detect(User_Generated_Discussions$hashtags, "AlexanderDobrindt|Dobrindt") ~ 'Dobrindt',
  str_detect(User_Generated_Discussions$text, "ChristianLindner|Lindner") ~ 'Lindner',
  str_detect(User_Generated_Discussions$hashtags, "ChristianLindner|Lindner") ~ 'Lindner',
  str_detect(User_Generated_Discussions$text, "AnnalenaBaerbock|Baerbock|Baer") ~ 'Baerbock',
  str_detect(User_Generated_Discussions$text, "RobertHabeck|Habeck") ~ 'Habeck',
  str_detect(User_Generated_Discussions$hashtags, "AnnalenaBaerbock|Baerbock|Baer") ~ 'Baerbock',
  str_detect(User_Generated_Discussions$hashtags, "RobertHabeck|Habeck") ~ 'Habeck',
  str_detect(User_Generated_Discussions$text, "JanineWissler|Wissler") ~ 'Wissler',
  str_detect(User_Generated_Discussions$text, "DietmarBartsch|Bartsch") ~ 'Bartsch',
  str_detect(User_Generated_Discussions$hashtags, "JanineWissler|Wissler") ~ 'Wissler',
  str_detect(User_Generated_Discussions$hashtags, "DietmarBartsch|Bartsch") ~ 'Bartsch',
  str_detect(User_Generated_Discussions$text, "OlafScholz|Scholz|Scholzomat") ~ 'Scholz',
  str_detect(User_Generated_Discussions$hashtags, "OlafScholz|Scholz|Scholzomat") ~ 'Scholz'))

User_Generated_Discussions$candidate_id[is.na(User_Generated_Discussions$candidate_id)] <- 'None/Other'
User_Generated_Discussions$Weidel <- ifelse(User_Generated_Discussions$candidate_id == 'Weidel', 1, 0)
User_Generated_Discussions$Chrupalla <- ifelse(User_Generated_Discussions$candidate_id == 'Chrupalla', 1, 0)
User_Generated_Discussions$Laschet <- ifelse(User_Generated_Discussions$candidate_id == 'Laschet', 1, 0)
User_Generated_Discussions$Dobrindt <- ifelse(User_Generated_Discussions$candidate_id == 'Dobrindt', 1, 0)
User_Generated_Discussions$Lindner <- ifelse(User_Generated_Discussions$candidate_id == 'Lindner', 1, 0)
User_Generated_Discussions$Baerbock <- ifelse(User_Generated_Discussions$candidate_id == 'Baerbock', 1, 0)
User_Generated_Discussions$Habeck <- ifelse(User_Generated_Discussions$candidate_id == 'Habeck', 1, 0)
User_Generated_Discussions$Wissler <- ifelse(User_Generated_Discussions$candidate_id == 'Wissler', 1, 0)
User_Generated_Discussions$Bartsch <- ifelse(User_Generated_Discussions$candidate_id == 'Bartsch', 1, 0)
User_Generated_Discussions$Scholz <- ifelse(User_Generated_Discussions$candidate_id == 'Scholz', 1, 0)

## 8.3 Other IDs
Political_Marketing$photo <- ifelse(Political_Marketing$media_type == 'photo', 1, 0)


###########################################################


# 9. Handling NAs
## 9.1 Political Marketing
Political_Marketing$reply_to_status_id[Political_Marketing$reply_to_status_id == ""] <- 'None'
Political_Marketing$reply_to_user_id[Political_Marketing$reply_to_user_id == ""] <- 'None'
Political_Marketing$reply_to_screen_name[Political_Marketing$reply_to_screen_name == ""] <- 'None'
Political_Marketing$hashtags[Political_Marketing$hashtags == ""] <- 'None'
Political_Marketing$urls_url[Political_Marketing$urls_url == ""] <- 'None'
Political_Marketing$media_url[Political_Marketing$media_url == ""] <- 'None'
Political_Marketing$media_type[Political_Marketing$media_type == ""] <- 'None'
Political_Marketing$ext_media_url[Political_Marketing$ext_media_url == ""] <- 'None'
Political_Marketing$mentions_user_id[Political_Marketing$mentions_user_id == ""] <- 'None'
Political_Marketing$mentions_screen_name[Political_Marketing$mentions_screen_name == ""] <- 'None'
Political_Marketing$retweet_status_id[Political_Marketing$retweet_status_id == ""] <- 'None'
Political_Marketing$retweet_text[Political_Marketing$retweet_text == ""] <- 'None'
Political_Marketing$retweet_created_at[Political_Marketing$retweet_created_at == ""] <- 'None'
Political_Marketing$retweet_user_id[Political_Marketing$retweet_user_id == ""] <- 'None'
Political_Marketing$retweet_screen_name[Political_Marketing$retweet_screen_name == ""] <- 'None'
Political_Marketing$retweet_name[Political_Marketing$retweet_name == ""] <- 'None'
Political_Marketing$retweet_location[Political_Marketing$retweet_location == ""] <- 'None'
Political_Marketing$retweet_verified[Political_Marketing$retweet_verified == ""] <- 'None'
Political_Marketing$verified[Political_Marketing$verified == ""] <- 'None'
Political_Marketing$retweet_favorite_count[is.na(Political_Marketing$retweet_favorite_count)] <- 0
Political_Marketing$retweet_retweet_count[is.na(Political_Marketing$retweet_retweet_count)] <- 0
Political_Marketing$retweet_followers_count[is.na(Political_Marketing$retweet_followers_count)] <- 0
Political_Marketing$retweet_friends_count[is.na(Political_Marketing$retweet_friends_count)] <- 0

## 9.2 User-Generated Discussions
User_Generated_Discussions$reply_to_status_id[User_Generated_Discussions$reply_to_status_id == ""] <- 'None'
User_Generated_Discussions$reply_to_user_id[User_Generated_Discussions$reply_to_user_id == ""] <- 'None'
User_Generated_Discussions$reply_to_screen_name[User_Generated_Discussions$reply_to_screen_name == ""] <- 'None'
User_Generated_Discussions$hashtags[User_Generated_Discussions$hashtags == ""] <- 'None'
User_Generated_Discussions$urls_url[User_Generated_Discussions$urls_url == ""] <- 'None'
User_Generated_Discussions$media_url[User_Generated_Discussions$media_url == ""] <- 'None'
User_Generated_Discussions$media_type[User_Generated_Discussions$media_type == ""] <- 'None'
User_Generated_Discussions$ext_media_url[User_Generated_Discussions$ext_media_url == ""] <- 'None'
User_Generated_Discussions$mentions_user_id[User_Generated_Discussions$mentions_user_id == ""] <- 'None'
User_Generated_Discussions$mentions_screen_name[User_Generated_Discussions$mentions_screen_name == ""] <- 'None'
User_Generated_Discussions$retweet_status_id[User_Generated_Discussions$retweet_status_id == ""] <- 'None'
User_Generated_Discussions$retweet_text[User_Generated_Discussions$retweet_text == ""] <- 'None'
User_Generated_Discussions$retweet_created_at[User_Generated_Discussions$retweet_created_at == ""] <- 'None'
User_Generated_Discussions$retweet_user_id[User_Generated_Discussions$retweet_user_id == ""] <- 'None'
User_Generated_Discussions$retweet_screen_name[User_Generated_Discussions$retweet_screen_name == ""] <- 'None'
User_Generated_Discussions$retweet_name[User_Generated_Discussions$retweet_name == ""] <- 'None'
User_Generated_Discussions$retweet_location[User_Generated_Discussions$retweet_location == ""] <- 'None'
User_Generated_Discussions$retweet_verified[User_Generated_Discussions$retweet_verified == ""] <- 'None'
User_Generated_Discussions$retweet_favorite_count[is.na(User_Generated_Discussions$retweet_favorite_count)] <- 0
User_Generated_Discussions$retweet_retweet_count[is.na(User_Generated_Discussions$retweet_retweet_count)] <- 0
User_Generated_Discussions$retweet_followers_count[is.na(User_Generated_Discussions$retweet_followers_count)] <- 0
User_Generated_Discussions$retweet_friends_count[is.na(User_Generated_Discussions$retweet_friends_count)] <- 0


###########################################################


# 10. Exporting Data
write.csv(Political_Marketing, './data/Political_Marketing.csv', fileEncoding = "UTF-8")
write.csv(User_Generated_Discussions, './data/User_Generated_Discussions.csv', fileEncoding = "UTF-8")