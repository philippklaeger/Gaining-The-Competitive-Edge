###########################################################
#                 A N A L Y S I S                         #
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
library(tm)
library(wordcloud)
library(lubridate)
library(stm)
library(stringr)
library(jtools)
library(memisc)
library(ggplot2)
library(Rcpp)
library(wordcloud2)
library(Hmisc)
library(ggpubr)
library(modelsummary)
library(flextable)
library(manifestoR)
library(data.table)
require(quanteda)
require(quanteda.textmodels)
require(quanteda.textplots)
require(quanteda.textstats)
require(readtext)
library(ggdendro)
library(rtweet)
library(fmsb)


###########################################################


# 3. Loading Data
Political_Marketing <- read.csv(file = './data/Political_Marketing.csv', sep = ",", fileEncoding = "UTF-8")
User_Generated_Discussions <- read.csv(file = './data/User_Generated_Discussions.csv', sep = ",", fileEncoding = "UTF-8")
Experts_raw <- read.csv(file = '../data/CHES2019_experts.csv', sep = ",", fileEncoding = "UTF-8")

AfD <- Political_Marketing %>% filter(party_id == 'AfD')
CDU <- Political_Marketing %>% filter(party_id == 'CDU')
FDP <- Political_Marketing %>% filter(party_id == 'FDP')
Gruene <- Political_Marketing %>% filter(party_id == 'Gruene')
Linke <- Political_Marketing %>% filter(party_id == 'Linke')
SPD <- Political_Marketing %>% filter(party_id == 'SPD')


###########################################################


# 4. Summary Statistics
## 4.1 Summary Tables
M_Summary1A <- datasummary_skim(Political_Marketing, output = "default")
M_Summary1B <- datasummary_skim(Political_Marketing, type="categorical", output = "default")
M_Summary1A
M_Summary1B
U_Summary1A <- datasummary_skim(User_Generated_Discussions, output = "default")
U_Summary1B <- datasummary_skim(User_Generated_Discussions, type="categorical", output = "default")
U_Summary1A
U_Summary1B
U_Summary2A <- User_Generated_Discussions %>%
  group_by(party_id) %>%
  summarise("Observations" = n(),
            "Avg. Favorite Count" = mean(favorite_count),
            "Avg. Retweet Count" = mean(retweet_count),
            "Avg. Text Width" = mean(display_text_width))
U_Summary2A


## 4.2 WordClouds
### 4.2.1 Stopwords
data("stop_words")
G_Stop_Words <- c('ab', 'aber', 'als', 'am', 'an', 'auch', 'auf', 'aus', 'bei', 'beim', 'bin', 'bis', 'bist', 'da', 'dadurch',
                  'daher', 'darum', 'das', 'daß', 'dass', 'dazu', 'dein', 'deine', 'dem', 'den', 'der', 'des', 'dessen',
                  'deshalb', 'die', 'dies', 'diese', 'dieser', 'dieses', 'doch', 'dort', 'du', 'durch', 'ein', 'eine',
                  'einem', 'einen', 'einer', 'eines', 'er', 'es', 'euer', 'eure', 'für', 'habe', 'haben', 'hat', 'hatte', 'hatten', 'hattest',
                  'hattet', 'heute', 'hier', 'hinter', 'ich', 'ihr', 'ihre', 'ihrer', 'im', 'in', 'ins', 'ist', 'ja', 'jede', 'jedem', 'jeden',
                  'jeder', 'jedes', 'jener', 'jenes', 'jetzt', 'kann', 'kannst', 'können', 'könnt', 'man', 'machen', 'mehr', 'mein',
                  'meine', 'mich', 'mit', 'muss', 'musst', 'müssen', 'müsst','nach','nachdem','nein','nicht','nun', 'nur', 'noch', 'oder', 'of', 'seid',
                  'sein','seine','sich','sie','sind','so', 'sehr', 'soll','sollen','sollst','sollt','sonst','soweit','sowie', 'the', 'um', 'und','unser',
                  'uns', 'unsere','unter','vom','von','vor','wann','warum', 'war','was', 'we', 'weiter','weitere','wenn','wer','werde','werden',
                  'werdet','weshalb','wie','wieder','wieso','wir','wird','wirst','wo','woher','wohin','zu','zum','zur','über',
                  'this', 'ihn', "t.co", "amp", "https", "http", "gegen", "keine", "alle")
### Basis For Stopwords:
### https://github.com/stopwords-iso/stopwords-de/blob/master/raw/stop-words-german.txt

### 4.2.2 Word Corpuses
Marketing_WC <- Political_Marketing %>% 
  unnest_tokens(word, text)
Marketing_WC <- Marketing_WC %>%
  anti_join(stop_words)
Marketing_WC <- Marketing_WC %>%
  filter(!word %in% G_Stop_Words)
Marketing_WC <- Marketing_WC %>%
  count(word, sort = TRUE)
Users_WC <- User_Generated_Discussions %>% 
  unnest_tokens(word, text)
Users_WC <- Users_WC %>%
  anti_join(stop_words)
Users_WC <- Users_WC %>%
  filter(!word %in% G_Stop_Words)
Users_WC <- Users_WC %>%
  count(word, sort = TRUE)
AfD_WC <- AfD %>% 
  unnest_tokens(word, text)
AfD_WC <- AfD_WC %>%
  anti_join(stop_words)
AfD_WC <- AfD_WC %>%
  filter(!word %in% G_Stop_Words)
AfD_WC <- AfD_WC %>%
  count(word, sort = TRUE)
CDU_WC <- CDU %>% 
  unnest_tokens(word, text)
CDU_WC <- CDU_WC %>%
  anti_join(stop_words)
CDU_WC <- CDU_WC %>%
  filter(!word %in% G_Stop_Words)
CDU_WC <- CDU_WC %>%
  count(word, sort = TRUE)
FDP_WC <- FDP %>% 
  unnest_tokens(word, text)
FDP_WC <- FDP_WC %>%
  anti_join(stop_words)
FDP_WC <- FDP_WC %>%
  filter(!word %in% G_Stop_Words)
FDP_WC <- FDP_WC %>%
  count(word, sort = TRUE)
Gruene_WC <- Gruene %>% 
  unnest_tokens(word, text)
Gruene_WC <- Gruene_WC %>%
  anti_join(stop_words)
Gruene_WC <- Gruene_WC %>%
  filter(!word %in% G_Stop_Words)
Gruene_WC <- Gruene_WC %>%
  count(word, sort = TRUE)
Linke_WC <- Linke %>% 
  unnest_tokens(word, text)
Linke_WC <- Linke_WC %>%
  anti_join(stop_words)
Linke_WC <- Linke_WC %>%
  filter(!word %in% G_Stop_Words)
Linke_WC <- Linke_WC %>%
  count(word, sort = TRUE)
SPD_WC <- SPD %>% 
  unnest_tokens(word, text)
SPD_WC <- SPD_WC %>%
  anti_join(stop_words)
SPD_WC <- SPD_WC %>%
  filter(!word %in% G_Stop_Words)
SPD_WC <- SPD_WC %>%
  count(word, sort = TRUE)

# 4.2.3 Party Color Palettes
AfD_palette <- c("#00CAFF", "#0094C6", "#003965", "#354A54", "#98AFBA")
CDU_palette <- c("#090909", "#2A2C30", "#465057", "#3F2D3C", "#F5B061")
FDP_palette <- c("#e5e40D", "#494738", "#AFAC99", "#323300", "#00B8B4")
Gruene_palette <- c("#007D5C", "#007467", "#00696C", "#205364", "#E0EA87")
Linke_palette <- c("#8374C6", "#B36DAF", "#E775A3", "#50424F", "#B7A6B5")
SPD_palette <- c("#5c1010", "#6f0000", "#560d0d", "#c30101", "#940000")

# 4.2.4 Generating WordClouds
Marketing_WordCloud <- wordcloud2(Marketing_WC, size = 1)
Users_WordCloud <- wordcloud2(Users_WC, size = 1)
AfD_WordCloud <- wordcloud2(AfD_WC, size = 2.5, color = rep_len(AfD_palette, nrow(AfD_WC)))
CDU_WordCloud <- wordcloud2(CDU_WC, size = 1.2, color = rep_len(CDU_palette, nrow(CDU_WC)))
FDP_WordCloud <- wordcloud2(FDP_WC, size = 1.2, color = rep_len(FDP_palette, nrow(FDP_WC)))
Gruene_WordCloud <- wordcloud2(Gruene_WC, size = 1.2, color = rep_len(Gruene_palette, nrow(Gruene_WC)))
Linke_WordCloud <- wordcloud2(Linke_WC, size = 1.2, color = rep_len(Linke_palette, nrow(Linke_WC)))
SPD_WordCloud <- wordcloud2(SPD_WC, size = 1.2, color = rep_len(SPD_palette, nrow(SPD_WC)))

# 4.2.5 Visualizing WordClouds
Marketing_WordCloud
Users_WordCloud
AfD_WordCloud
CDU_WordCloud
FDP_WordCloud
Gruene_WordCloud
Linke_WordCloud
SPD_WordCloud


## 4.3 Reach & Interaction
### 4.3.1 Count Statistics
Reach <- Political_Marketing %>%
  group_by(party_id) %>%
  summarise("Observations" = n(),
            "Avg. Text Width" = mean(display_text_width),
            "Avg. Favorite Count" = mean(favorite_count),
            "Avg. Retweet Count" = mean(retweet_count),
            "Avg. Followers Count" = mean(followers_count),
            "Avg. Friends Count" = mean(friends_count))
Reach

### 4.3.2 Hashtag Usage
AfD_Hashtags <- AfD %>% unnest_tokens(word, hashtags)
AfD_Hashtags <- AfD_Hashtags %>%
  filter(!word %in% G_Stop_Words)
AfD_Hashtags <- AfD_Hashtags %>%
  count(word, sort = TRUE)
AfD_Hashtags <- AfD_Hashtags %>% filter(word != "none")
CDU_Hashtags <- CDU %>% unnest_tokens(word, hashtags)
CDU_Hashtags <- CDU_Hashtags %>%
  filter(!word %in% G_Stop_Words)
CDU_Hashtags <- CDU_Hashtags %>%
  count(word, sort = TRUE)
CDU_Hashtags <- CDU_Hashtags %>% filter(word != "none")
FDP_Hashtags <- FDP %>% unnest_tokens(word, hashtags)
FDP_Hashtags <- FDP_Hashtags %>%
  filter(!word %in% G_Stop_Words)
FDP_Hashtags <- FDP_Hashtags %>%
  count(word, sort = TRUE)
FDP_Hashtags <- FDP_Hashtags %>% filter(word != "none")
Gruene_Hashtags <- Gruene %>% unnest_tokens(word, hashtags)
Gruene_Hashtags <- Gruene_Hashtags %>%
  filter(!word %in% G_Stop_Words)
Gruene_Hashtags <- Gruene_Hashtags %>%
  count(word, sort = TRUE)
Gruene_Hashtags <- Gruene_Hashtags %>% filter(word != "none")
Linke_Hashtags <- Linke %>% unnest_tokens(word, hashtags)
Linke_Hashtags <- Linke_Hashtags %>%
  filter(!word %in% G_Stop_Words)
Linke_Hashtags <- Linke_Hashtags %>%
  count(word, sort = TRUE)
Linke_Hashtags <- Linke_Hashtags %>% filter(word != "none")
SPD_Hashtags <- SPD %>% unnest_tokens(word, hashtags)
SPD_Hashtags <- SPD_Hashtags %>%
  filter(!word %in% G_Stop_Words)
SPD_Hashtags <- SPD_Hashtags %>%
  count(word, sort = TRUE)
SPD_Hashtags <- SPD_Hashtags %>% filter(word != "none")

### 4.3.3 Generating Hashtag WordClouds
AfD_HashtagCloud <- wordcloud2(AfD_Hashtags, size = 4, color = rep_len(AfD_palette, nrow(AfD_Hashtags)))
CDU_HashtagCloud <- wordcloud2(CDU_Hashtags, size = 1.5, color = rep_len(CDU_palette, nrow(CDU_Hashtags)))
FDP_HashtagCloud <- wordcloud2(FDP_Hashtags, size = 1.5, color = rep_len(FDP_palette, nrow(FDP_Hashtags)))
Gruene_HashtagCloud <- wordcloud2(Gruene_Hashtags, size = 1.5, color = rep_len(Gruene_palette, nrow(Gruene_Hashtags)))
Linke_HashtagCloud <- wordcloud2(Linke_Hashtags, size = 1.5, color = rep_len(Linke_palette, nrow(Linke_Hashtags)))
SPD_HashtagCloud <- wordcloud2(SPD_Hashtags, size = 1.5, color = rep_len(SPD_palette, nrow(SPD_Hashtags)))

# 4.3.4 Visualizing Hashtag WordClouds
AfD_HashtagCloud
CDU_HashtagCloud
FDP_HashtagCloud
Gruene_HashtagCloud
Linke_HashtagCloud
SPD_HashtagCloud


###########################################################


# 5. Sentiment & Emotion Analysis
## 5.1 Sentiments
M_syuzhet <- get_sentiment(Political_Marketing$text, method="syuzhet")
M_bing <- get_sentiment(Political_Marketing$text, method="bing")
M_afinn <- get_sentiment(Political_Marketing$text, method="afinn")
M_nrc <- get_sentiment(Political_Marketing$text, method="nrc")
M_Sentiment_Index <- (M_syuzhet + M_bing + M_afinn + M_nrc) / 4
M_Sentiments <- data.frame(M_syuzhet, M_bing, M_afinn, M_nrc, M_Sentiment_Index)
write.csv(M_Sentiments, '../data/M_Sentiments.csv', fileEncoding = "UTF-8")

U_syuzhet <- get_sentiment(User_Generated_Discussions$text, method="syuzhet")
U_bing <- get_sentiment(User_Generated_Discussions$text, method="bing")
U_afinn <- get_sentiment(User_Generated_Discussions$text, method="afinn")
U_nrc <- get_sentiment(User_Generated_Discussions$text, method="nrc")
U_Sentiment_Index <- (U_syuzhet + U_bing + U_afinn + U_nrc) / 4
U_Sentiments <- data.frame(U_syuzhet, U_bing, U_afinn, U_nrc, U_Sentiment_Index)
write.csv(U_Sentiments, '../data/U_Sentiments.csv', fileEncoding = "UTF-8")


## 5.2 Emotions
M_Emotions <- get_nrc_sentiment(Political_Marketing$text, language = 'german')
M_Emotions$Polarity_Index <- (M_Emotions$positive + M_Emotions$negative) / 2
M_Emotions$Emotion_Index <- (M_Emotions$anger + M_Emotions$anticipation + M_Emotions$disgust + M_Emotions$fear +
                               M_Emotions$joy + M_Emotions$sadness + M_Emotions$surprise + M_Emotions$trust) / 8
M_Emotions$Intensity_Index <- (M_Emotions$anger + M_Emotions$anticipation + M_Emotions$disgust + M_Emotions$fear +
                                 M_Emotions$joy + M_Emotions$sadness + M_Emotions$surprise + M_Emotions$trust)
M_emo_bar = colSums(M_Emotions)
M_emo_sum = data.frame(count=M_emo_bar, emotion=names(M_emo_bar))
M_emo_sum$emotion = factor(M_emo_sum$emotion, levels=M_emo_sum$emotion[order(M_emo_sum$count, decreasing = TRUE)])
M_emo_sum <- M_emo_sum[-c(1), ]
write.csv(M_Emotions, '../data/M_Emotions.csv', fileEncoding = "UTF-8")

U_Emotions <- get_nrc_sentiment(User_Generated_Discussions$text, language = 'german')
U_Emotions$Polarity_Index <- (U_Emotions$positive + U_Emotions$negative) / 2
U_Emotions$Emotion_Index <- (U_Emotions$anger + U_Emotions$anticipation + U_Emotions$disgust + U_Emotions$fear +
                               U_Emotions$joy + U_Emotions$sadness + U_Emotions$surprise + U_Emotions$trust) / 8
U_Emotions$Intensity_Index <- (U_Emotions$anger + U_Emotions$anticipation + U_Emotions$disgust + U_Emotions$fear +
                                 U_Emotions$joy + U_Emotions$sadness + U_Emotions$surprise + U_Emotions$trust)
U_emo_bar = colSums(U_Emotions)
U_emo_sum = data.frame(count=U_emo_bar, emotion=names(U_emo_bar))
U_emo_sum$emotion = factor(U_emo_sum$emotion, levels=U_emo_sum$emotion[order(U_emo_sum$count, decreasing = TRUE)])
U_emo_sum <- U_emo_sum[-c(1), ]
write.csv(U_Emotions, '../data/U_Emotions.csv', fileEncoding = "UTF-8")

M_SandE <- ggplot(M_emo_sum, aes(x=emotion, y=count)) + 
  geom_bar(stat = "identity") +
  ggtitle("Sentiments & Emotions - Political Marketing Data") +
  xlab("Sentiments and Emotions") +
  ylab("Counts") +
  coord_flip()
M_SandE

U_SandE <- ggplot(U_emo_sum, aes(x=emotion, y=count)) + 
  geom_bar(stat = "identity") +
  ggtitle("Sentiments & Emotions -  User-Generated Discussions Data") +
  xlab("Sentiments and Emotions") +
  ylab("Counts") +
  coord_flip()
U_SandE

M_Parties_SnE1 <- Marketing %>%
  group_by(party_id) %>%
  summarise("Observations" = n(),
            "Avg. Sentiment Index" = mean(M_Sentiment_Index),
            "Avg. Syuzhet Score" = mean(M_syuzhet),
            "Avg. Bing Score" = mean(M_bing),
            "Avg. Afinn Score" = mean(M_afinn),
            "Avg. NRC Score" = mean(M_nrc),
            "Avg. Positive Score" = mean(positive),
            "Avg. Negative Score" = mean(negative),
            "Avg. Emotion Index" = mean(Emotion_Index),
            "Avg. Intensity Index" = mean(Intensity_Index))
M_Parties_SnE2 <- Marketing %>%
  group_by(party_id) %>%
  summarise("Observations" = n(),
            "Avg. Anger Score" = mean(anger),
            "Avg. Anticipation Score" = mean(anticipation),
            "Avg. Disgust Score" = mean(disgust),
            "Avg. Fear Score" = mean(fear),
            "Avg. Joy Score" = mean(joy),
            "Avg. Sadness Score" = mean(sadness),
            "Avg. Surprise Score" = mean(surprise),
            "Avg. Trust Score" = mean(trust))
M_Parties_SnE1
M_Parties_SnE2


## 5.4 Consolidating & Subsetting Data
Marketing <- cbind(Political_Marketing, M_Sentiments, M_Emotions)
Users <- cbind(User_Generated_Discussions, U_Sentiments, U_Emotions)
write.csv(Marketing, '../data/Marketing.csv', fileEncoding = "UTF-8")
write.csv(Users, '../data/Users.csv', fileEncoding = "UTF-8")



###########################################################


# 6. General Stance Analysis
## 6.1 Relative Positioning Based On The Overall Ideological Positioning (ManifestoR)
### Party Codes
### AfD:        41953
### CDU/CSU:    41521
### FDP:        41420
### Gruene:     41113
### Left:       41223
### SPD:        41320

### 6.1.2 ManifestoR API Key
mp_key <- mp_setapikey(file.choose())

### 6.1.2 Subset For Six Major Political Parties in Germany
all_manifestos <- mp_maindataset()
germany_manifestos <- all_manifestos[which(all_manifestos$countryname == "Germany"),]
big_parties <- germany_manifestos %>% filter(party == "41953" | party == "41521" | party == "41420" | party == "41113" | party == "41223" | party == "41320")
big_parties <- big_parties %>% filter(edate >= "1990-01-01")

### 6.1.3 Extracting Expert Evaluation Scores
big_parties$rile_index <- rile(big_parties)

### 6.1.4 Results
graph_rile <- big_parties %>%
  group_by(partyname) %>%
  ggplot(aes(x= as.Date(edate), y = rile_index, color=partyname)) +
  geom_point() + geom_line(size = 1.5) +
  geom_text(data=. %>%
              arrange(desc(edate)) %>%
              group_by(partyname) %>%
              slice(1),
            aes(label=c('Greens', 'AfD', 'CDU/CSU', 'FDP', 'SPD', 'Left')), position=position_jitter(height=2), stat='identity', hjust = 0, size = 3, angle=40) +
  theme(legend.position = "None")+
  ggtitle("Relative Right-Left Ideological Position of Major German Parties 1990 - 2021",
          subtitle = "Positive Values: Right-Wing Oriented; Negative Values: Left-Wing Oriented") +
  xlab("Year") + ylab("Right Left (RILE) Index")
graph_rile + theme_gray()
graph_rile + scale_color_manual(values=c("#00dd00", "#00CAFF", "#222222", "#eace09", "#dd0000", "#dd00dd"))
table_rile <- subset(big_parties,
                     select = c(edate, partyabbrev, planeco, markeco, welfare, intpeace, rile_index, datasetversion))
table_rile <- table_rile %>% filter(edate >= "2013-01-01")
summary(table_rile)


## 6.2 Relative Positioning Based On The Linguisitic Positioning
### Party IDs
### AfD:      310
### CDU/CSU:  301 & 308
### FDP:      303
### Gruene:   304
### Linke:    306
### SPD:      302

### 6.2.1 Extracting Expert Evaluation Scores
Experts <- Experts_raw %>% filter(party_id == "310" | party_id == "301" | party_id == "308" | party_id == "303" | party_id == "304" | party_id == "306" | party_id == '302')
Experts <- subset(Experts,
                  select = c(id, party_name, lrgen, lrecon, galtan, immigrate_policy, environment, spendvtax, nationalism, protectionism, civlib_laworder))
Experts$lrgen <- as.numeric(Experts$lrgen)
Experts$lrecon <- as.numeric(Experts$lrecon)
Experts$galtan <- as.numeric(Experts$galtan)
Experts$immigrate_policy <- as.numeric(Experts$immigrate_policy)
Experts$environment <- as.numeric(Experts$environment)
Experts$spendvtax <- as.numeric(Experts$spendvtax)
Experts$nationalism <- as.numeric(Experts$nationalism)
Experts$protectionism <- as.numeric(Experts$protectionism)
Experts$civlib_laworder <- as.numeric(Experts$civlib_laworder)
Experts_LR <- Experts %>%
  group_by(party_name) %>%
  summarise("Expert Evaluations" = n(),
            "Overall Ideological Stance" = mean(lrgen, na.rm = TRUE),
            "Economic Issues Score" = mean(lrecon, na.rm = TRUE),
            "Social & Cultural Values Score: Libertarian (lw) vs. Traditional (rw)" = mean(galtan, na.rm = TRUE),
            "Immigration Policy Score: liberal (lw) vs. restrictive (rw)" = mean(immigrate_policy, na.rm = TRUE),
            "Environmental Score: Environmental Sustainability (lw) vs. Economic Growth (rw)" = mean(environment, na.rm = TRUE),
            "State Spendings Score: Public Services Improvement (lw) vs. Tax Reduction (rw)" = mean(spendvtax, na.rm = TRUE),
            "Identity Score: Cosmopolitanism vs. Nationalism" = mean(nationalism, na.rm = TRUE),
            "Trade Orientation Score: Liberalization (lw) vs. Protectionism (rw)" = mean(protectionism, na.rm = TRUE),
            "Civil Score: Civil Liberties (lw) vs. Law & Order (rw)" = mean(civlib_laworder, na.rm = TRUE))
Experts_LR

### 6.2.2 Preparing Texts of Party Programs and Tweets
PP_AfD_2017 <- read.delim("../docs/PP AfD 2017.txt", encoding = "UTF-8", header = FALSE)
PP_AfD_2017 <- paste(PP_AfD_2017$V1, collapse = "")
PP_CDU_2017 <- read.delim("../docs/PP CDU 2017.txt", encoding = "UTF-8", header = FALSE)
PP_CDU_2017 <- paste(PP_CDU_2017$V1, collapse = "")
PP_FDP_2017 <- read.delim("../docs/PP FDP 2017.txt", encoding = "UTF-8", header = FALSE)
PP_FDP_2017 <- paste(PP_FDP_2017$V1, collapse = "")
PP_Gruene_2017 <- read.delim("../docs/PP Gruene 2017.txt", encoding = "UTF-8", header = FALSE)
PP_Gruene_2017 <- paste(PP_Gruene_2017$V1, collapse = "")
PP_Linke_2017 <- read.delim("../docs/PP Linke 2017.txt", encoding = "UTF-8", header = FALSE)
PP_Linke_2017 <- paste(PP_Linke_2017$V1, collapse = "")
PP_SPD_2017 <- read.delim("../docs/PP SPD 2017.txt", encoding = "UTF-8", header = FALSE)
PP_SPD_2017 <- paste(PP_SPD_2017$V1, collapse = "")
PP_AfD_2021 <- read.delim("../docs/PP AfD 2021.txt", encoding = "UTF-8", header = FALSE)
PP_AfD_2021 <- paste(PP_AfD_2021$V1, collapse = "")
PP_CDU_2021 <- read.delim("../docs/PP CDU 2021.txt", encoding = "UTF-8", header = FALSE)
PP_CDU_2021 <- paste(PP_CDU_2021$V1, collapse = "")
PP_FDP_2021 <- read.delim("../docs/PP FDP 2021.txt", encoding = "UTF-8", header = FALSE)
PP_FDP_2021 <- paste(PP_FDP_2021$V1, collapse = "")
PP_Gruene_2021 <- read.delim("../docs/PP Gruene 2021.txt", encoding = "UTF-8", header = FALSE)
PP_Gruene_2021 <- paste(PP_Gruene_2021$V1, collapse = "")
PP_Linke_2021 <- read.delim("../docs/PP Linke 2021.txt", encoding = "UTF-8", header = FALSE)
PP_Linke_2021 <- paste(PP_Linke_2021$V1, collapse = "")
PP_SPD_2021 <- read.delim("../docs/PP SPD 2021.txt", encoding = "UTF-8", header = FALSE)
PP_SPD_2021 <- paste(PP_SPD_2021$V1, collapse = "")
T_AfD <- paste0(AfD$text, collapse = "")
T_CDU <- paste0(CDU$text, collapse = "")
T_FDP <- paste0(FDP$text, collapse = "")
T_Gruene <- paste0(Gruene$text, collapse = "")
T_Linke <- paste0(Linke$text, collapse = "")
T_SPD <- paste0(SPD$text, collapse = "")

### 6.2.3 Creating Corpus
### Known scores are applied to the reference texts (2017 party programs)
### Virgin texts are scored based on the wordscores
### Initially each virgin text needs to be set up with "NA" as value.
### This will be overwritten by the actual scores.
Text <- c(PP_AfD_2017, PP_CDU_2017, PP_FDP_2017, PP_Gruene_2017, PP_Linke_2017, PP_SPD_2017,
          PP_AfD_2021, PP_CDU_2021, PP_FDP_2021, PP_Gruene_2021, PP_Linke_2021, PP_SPD_2021,
          T_AfD, T_CDU, T_FDP, T_Gruene, T_Linke, T_SPD)
Text <- data.frame(Text)
M_RefScore <- c("26.050", "3.495", "-21.037", "0.265", "-36.168", "-24.673",
                "NA", "NA", "NA", "NA", "NA", "NA",
                "NA", "NA", "NA", "NA", "NA", "NA")
CH_RefScore <- c("9.238", "6.524", "6.429", "3.238", "1.429", "3.619",
                 "NA", "NA", "NA", "NA", "NA", "NA",
                 "NA", "NA", "NA", "NA", "NA", "NA")
M_RefScore <- as.numeric(M_RefScore)
CH_RefScore <- as.numeric(CH_RefScore)
Election_Corpus <- corpus(Text$Text)
Election_Corpus$M_RefScore <- M_RefScore
Election_Corpus$CH_RefScore <- CH_RefScore
quanteda::docnames(Election_Corpus) <- c("PP AfD 2017", "PP CDU 2017", "PP FDP 2017", "PP Gruene 2017", "PP Linke 2017", "PP SPD 2017",
                                         "PP AfD 2021", "PP CDU 2021", "PP FDP 2021", "PP Gruene 2021", "PP Linke 2021", "PP SPD 2021",
                                         "Tweets AfD", "Tweets CDU", "Tweets FDP", "Tweets Gruene", "Tweets Linke", "Tweets SPD")

### 6.2.4 Generating Wordscores for 2021 Party Programs
#### Tokenization
Election_Toks <- tokens(Election_Corpus, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_remove(pattern = stopwords("de")) %>% 
  tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex")

#### Document-Feature Matrix
Election_DFM <- dfm(Election_Toks) %>%
  dfm_remove(pattern = stopwords("de")) %>%
  dfm_remove(pattern = G_Stop_Words)

#### Results
Election_WS <- textmodel_wordscores(Election_DFM, y = Election_Corpus$CH_RefScore, smooth = 1)
summary(Election_WS)
Election_Pred <- predict(Election_WS, se.fit = TRUE, newdata = Election_DFM)
textplot_scale1d(Election_Pred)


## 6.3 Similarity Between Authors (SBA)
### 6.3.1 Corpus for each party (distinct entity: candidate / "author")
AfD_Tweets_Corpus <- corpus(AfD)
CDU_Tweets_Corpus <- corpus(CDU)
FDP_Tweets_Corpus <- corpus(FDP)
Gruene_Tweets_Corpus <- corpus(Gruene)
Linke_Tweets_Corpus <- corpus(Linke)
SPD_Tweets_Corpus <- corpus(SPD)

### 6.3.2 Document-Feature Matrix for each party (distinct entity: candidate / "author")
AfD_Tweets_DFM <- AfD_Tweets_Corpus %>%
  tokens(remove_punct = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
  tokens_remove(pattern = stopwords("de")) %>% 
  tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex") %>%
  dfm() %>% 
  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*")) %>%
  dfm_remove(pattern = G_Stop_Words) %>%
  dfm_remove(pattern = stopwords("en"))
ndoc(AfD_Tweets_DFM)
topfeatures(AfD_Tweets_DFM)
AfD_Users_DFM <- dfm_group(AfD_Tweets_DFM, groups = screen_name)
ndoc(AfD_Users_DFM)

CDU_Tweets_DFM <- CDU_Tweets_Corpus %>%
  tokens(remove_punct = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
  tokens_remove(pattern = stopwords("de")) %>% 
  tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex") %>%
  dfm() %>% 
  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*")) %>%
  dfm_remove(pattern = G_Stop_Words) %>%
  dfm_remove(pattern = stopwords("en"))
ndoc(CDU_Tweets_DFM)
topfeatures(CDU_Tweets_DFM)
CDU_Users_DFM <- dfm_group(CDU_Tweets_DFM, groups = screen_name)
ndoc(CDU_Users_DFM)

FDP_Tweets_DFM <- FDP_Tweets_Corpus %>%
  tokens(remove_punct = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
  tokens_remove(pattern = stopwords("de")) %>% 
  tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex") %>%
  dfm() %>% 
  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*")) %>%
  dfm_remove(pattern = G_Stop_Words) %>%
  dfm_remove(pattern = stopwords("en"))
ndoc(FDP_Tweets_DFM)
topfeatures(FDP_Tweets_DFM)
FDP_Users_DFM <- dfm_group(FDP_Tweets_DFM, groups = screen_name)
ndoc(FDP_Users_DFM)

Gruene_Tweets_DFM <- Gruene_Tweets_Corpus %>%
  tokens(remove_punct = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
  tokens_remove(pattern = stopwords("de")) %>% 
  tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex") %>%
  dfm() %>% 
  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*")) %>%
  dfm_remove(pattern = G_Stop_Words) %>%
  dfm_remove(pattern = stopwords("en"))
ndoc(Gruene_Tweets_DFM)
topfeatures(Gruene_Tweets_DFM)
Gruene_Users_DFM <- dfm_group(Gruene_Tweets_DFM, groups = screen_name)
ndoc(Gruene_Users_DFM)

Linke_Tweets_DFM <- Linke_Tweets_Corpus %>%
  tokens(remove_punct = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
  tokens_remove(pattern = stopwords("de")) %>% 
  tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex") %>%
  dfm() %>% 
  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*")) %>%
  dfm_remove(pattern = G_Stop_Words) %>%
  dfm_remove(pattern = stopwords("en"))
ndoc(Linke_Tweets_DFM)
topfeatures(Linke_Tweets_DFM)
Linke_Users_DFM <- dfm_group(Linke_Tweets_DFM, groups = screen_name)
ndoc(Linke_Users_DFM)

SPD_Tweets_DFM <- SPD_Tweets_Corpus %>%
  tokens(remove_punct = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
  tokens_remove(pattern = stopwords("de")) %>% 
  tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex") %>%
  dfm() %>% 
  dfm_remove(pattern = c("*.tt", "*.uk", "*.com", "rt", "#*", "@*")) %>%
  dfm_remove(pattern = G_Stop_Words) %>%
  dfm_remove(pattern = stopwords("en"))
ndoc(SPD_Tweets_DFM)
topfeatures(SPD_Tweets_DFM)
SPD_Users_DFM <- dfm_group(SPD_Tweets_DFM, groups = screen_name)
ndoc(SPD_Users_DFM)

### 6.3.2 Computing SBA (Results)
AfD_Users_TextStat <- as.dist(textstat_dist(AfD_Users_DFM))
AfD_User_Cluster <- hclust(AfD_Users_TextStat, method = 'ward.D2')
CDU_Users_TextStat <- as.dist(textstat_dist(CDU_Users_DFM))
CDU_User_Cluster <- hclust(CDU_Users_TextStat, method = 'ward.D2')
FDP_Users_TextStat <- as.dist(textstat_dist(FDP_Users_DFM))
FDP_User_Cluster <- hclust(FDP_Users_TextStat, method = 'ward.D2')
Gruene_Users_TextStat <- as.dist(textstat_dist(Gruene_Users_DFM))
Gruene_User_Cluster <- hclust(Gruene_Users_TextStat, method = 'ward.D2')
Linke_Users_TextStat <- as.dist(textstat_dist(Linke_Users_DFM))
Linke_User_Cluster <- hclust(Linke_Users_TextStat, method = 'ward.D2')
SPD_Users_TextStat <- as.dist(textstat_dist(SPD_Users_DFM))
SPD_User_Cluster <- hclust(SPD_Users_TextStat, method = 'ward.D2')

summary(AfD_Users_TextStat)
summary(CDU_Users_TextStat)
summary(FDP_Users_TextStat)
summary(Gruene_Users_TextStat)
summary(Linke_Users_TextStat)
summary(SPD_Users_TextStat)

### 6.3.3 Visualization
ggdendrogram(AfD_User_Cluster, theme_dendro = FALSE) +
  geom_hline(yintercept = 300, color = "#C00000")
ggdendrogram(CDU_User_Cluster, theme_dendro = FALSE) +
  geom_hline(yintercept = 300, color = "#C00000")
ggdendrogram(FDP_User_Cluster, theme_dendro = FALSE) +
  geom_hline(yintercept = 300, color = "#C00000")
ggdendrogram(Gruene_User_Cluster, theme_dendro = FALSE) +
  geom_hline(yintercept = 300, color = "#C00000")
ggdendrogram(Linke_User_Cluster, theme_dendro = FALSE) +
  geom_hline(yintercept = 300, color = "#C00000")
ggdendrogram(SPD_User_Cluster, theme_dendro = FALSE) +
  geom_hline(yintercept = 300, color = "#C00000")


## 6.4 Lexical Diversity
Election_LexDiv <- textstat_lexdiv(Election_DFM)
plot(Election_LexDiv$TTR, type = "l", xaxt = "n", xlab = NULL, ylab = "TTR")
grid()
axis(1, at = seq_len(nrow(Election_LexDiv)), labels = Election_LexDiv$document)


###########################################################


# 7. Topic-Specific Stance Analysis
## 7.1 Keywords
Corona <- c("corona", "coronavirus", "covid-19", "sars-cov-2", "pandemie", "lockdown", "impfen", "impfung",
            "impfstoff", "geimpft", "ungeimpft", "impfdosen", "biontech", "pfizer", "astrazeneca", "moderna", 
            "johnson&johnson", "inzidenz", "mpk", "masken", "maskenskandal", "infektion", 
            "infektionsschutzgesetz", "impfpflicht")
Environment <- c("klimaschutz", "klimaschutzistjetzt", "klimakrise", "kohleausstieg", "verkehrswende",
                 "klimawandel", "erneuerbar", "erneuerbaren", "klimapolitik", "hochwasser", "sozialklimagerecht",
                 "nachhaltigkeit", "nachhaltig", "flutkatastrophe", "klimapolitik", "klimaänderung",
                 "klimaentwicklung")
Digitization <- c("digitalisierung", "digital", "digitalen", "digitaletransformation", "transformation",
                  "digitalisieren", "digitalisierungsstrategie", "modernisierungsjahrzehnt", "industrie4.0",
                  "digitalerevolution", "homeoffice", "videokonferenz", "videoconference", "zoom",
                  "skype", "microsoftteams", "hybridearbeit", "hybridesstudieren", "bürokratieabbau")

## 7.2 Topic 1 - Corona
### 7.2.1 Relevant Tweets
AfD_Corona_Tweets <- subset(AfD, select = c(screen_name, text, hashtags))
AfD_Corona_Tweets$text2 <- tolower(AfD_Corona_Tweets$text)
AfD_Corona_Tweets <- AfD_Corona_Tweets %>%
  filter(grepl('corona|coronavirus|covid-19|sars-cov-2|pandemie|lockdown|
                impfen|impfung|impfstoff|geimpft|ungeimpft|impfdosen|
                biontech|pfizer|astrazeneca|moderna|johnson&johnson|inzidenz|
                mpk|masken|maskenskandal|infektion|infektionsschutzgesetz|impfpflicht', text2))
T_AfD_Corona <- paste0(AfD_Corona_Tweets$text2, collapse = "")
CDU_Corona_Tweets <- subset(CDU, select = c(screen_name, text, hashtags))
CDU_Corona_Tweets$text2 <- tolower(CDU_Corona_Tweets$text)
CDU_Corona_Tweets <- CDU_Corona_Tweets %>%
  filter(grepl('corona|coronavirus|covid-19|sars-cov-2|pandemie|lockdown|
                impfen|impfung|impfstoff|geimpft|ungeimpft|impfdosen|
                biontech|pfizer|astrazeneca|moderna|johnson&johnson|inzidenz|
                mpk|masken|maskenskandal|infektion|infektionsschutzgesetz|impfpflicht', text2))
T_CDU_Corona <- paste0(CDU_Corona_Tweets$text2, collapse = "")
FDP_Corona_Tweets <- subset(FDP, select = c(screen_name, text, hashtags))
FDP_Corona_Tweets$text2 <- tolower(FDP_Corona_Tweets$text)
FDP_Corona_Tweets <- FDP_Corona_Tweets %>%
  filter(grepl('corona|coronavirus|covid-19|sars-cov-2|pandemie|lockdown|
                impfen|impfung|impfstoff|geimpft|ungeimpft|impfdosen|
                biontech|pfizer|astrazeneca|moderna|johnson&johnson|inzidenz|
                mpk|masken|maskenskandal|infektion|infektionsschutzgesetz|impfpflicht', text2))
T_FDP_Corona <- paste0(FDP_Corona_Tweets$text2, collapse = "")
Gruene_Corona_Tweets <- subset(Gruene, select = c(screen_name, text, hashtags))
Gruene_Corona_Tweets$text2 <- tolower(Gruene_Corona_Tweets$text)
Gruene_Corona_Tweets <- Gruene_Corona_Tweets %>%
  filter(grepl('corona|coronavirus|covid-19|sars-cov-2|pandemie|lockdown|
                impfen|impfung|impfstoff|geimpft|ungeimpft|impfdosen|
                biontech|pfizer|astrazeneca|moderna|johnson&johnson|inzidenz|
                mpk|masken|maskenskandal|infektion|infektionsschutzgesetz|impfpflicht', text2))
T_Gruene_Corona <- paste0(Gruene_Corona_Tweets$text2, collapse = "")
Linke_Corona_Tweets <- subset(Linke, select = c(screen_name, text, hashtags))
Linke_Corona_Tweets$text2 <- tolower(Linke_Corona_Tweets$text)
Linke_Corona_Tweets <- Linke_Corona_Tweets %>%
  filter(grepl('corona|coronavirus|covid-19|sars-cov-2|pandemie|lockdown|
                impfen|impfung|impfstoff|geimpft|ungeimpft|impfdosen|
                biontech|pfizer|astrazeneca|moderna|johnson&johnson|inzidenz|
                mpk|masken|maskenskandal|infektion|infektionsschutzgesetz|impfpflicht', text2))
T_Linke_Corona <- paste0(Linke_Corona_Tweets$text2, collapse = "")
SPD_Corona_Tweets <- subset(SPD, select = c(screen_name, text, hashtags))
SPD_Corona_Tweets$text2 <- tolower(SPD_Corona_Tweets$text)
SPD_Corona_Tweets <- SPD_Corona_Tweets %>%
  filter(grepl('corona|coronavirus|covid-19|sars-cov-2|pandemie|lockdown|
                impfen|impfung|impfstoff|geimpft|ungeimpft|impfdosen|
                biontech|pfizer|astrazeneca|moderna|johnson&johnson|inzidenz|
                mpk|masken|maskenskandal|infektion|infektionsschutzgesetz|impfpflicht', text2))
T_SPD_Corona <- paste0(SPD_Corona_Tweets$text2, collapse = "")

### 7.2.2 Wordscores
Corona_Texts <- c(T_AfD, T_CDU, T_FDP, T_Gruene, T_Linke, T_SPD,
                  T_AfD_Corona, T_CDU_Corona, T_FDP_Corona, T_Gruene_Corona,
                  T_Linke_Corona, T_SPD_Corona)
Corona_Texts <- data.frame(Corona_Texts)
Corona_RefScore <- c("4.854", "4.715", "4.885", "4.650", "4.739", "4.777",
                     "NA", "NA", "NA", "NA", "NA", "NA")
Corona_RefScore <- as.numeric(Corona_RefScore)
Corona_Corpus <- quanteda::corpus(Corona_Texts$Corona_Texts)
Corona_Corpus$RefScore <- Corona_RefScore
quanteda::docnames(Corona_Corpus) <- c("Tweets AfD", "Tweets CDU", "Tweets FDP",
                                       "Tweets Gruene", "Tweets Linke", "Tweets SPD",
                                       "Tweets AfD Corona", "Tweets CDU Corona", "Tweets FDP Corona",
                                       "Tweets Gruene Corona", "Tweets Linke Corona", "Tweets SPD Corona")
Corona_Toks <- quanteda::tokens(Corona_Corpus, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_remove(pattern = stopwords("de")) %>% 
  tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex")
Corona_DFM <- dfm(Corona_Toks) %>%
  dfm_remove(pattern = stopwords("de")) %>%
  dfm_remove(pattern = G_Stop_Words)
Corona_WS <- textmodel_wordscores(Corona_DFM, y = Corona_Corpus$RefScore, smooth = 1)
summary(Corona_WS)
Corona_Pred <- predict(Corona_WS, se.fit = TRUE, newdata = Corona_DFM)
textplot_scale1d(Corona_Pred)


## 7.3 Topic 2 - Environment
### 7.3.1 Relevant Tweets
AfD_Environment_Tweets <- subset(AfD, select = c(screen_name, text, hashtags))
AfD_Environment_Tweets$text2 <- tolower(AfD_Environment_Tweets$text)
AfD_Environment_Tweets <- AfD_Environment_Tweets %>%
  filter(grepl('klimaschutz|klimaschutzistjetzt|klimakrise|kohleausstieg|verkehrswende|
               klimawandel|erneuerbar|erneuerbaren|klimapolitik|hochwasser|sozialklimagerecht|
               nachhaltigkeit|nachhaltig|flutkatastrophe|klimapolitik|klimaänderung|klimaentwicklung', text2))
T_AfD_Environment <- paste0(AfD_Environment_Tweets$text2, collapse = "")
CDU_Environment_Tweets <- subset(CDU, select = c(screen_name, text, hashtags))
CDU_Environment_Tweets$text2 <- tolower(CDU_Environment_Tweets$text)
CDU_Environment_Tweets <- CDU_Environment_Tweets %>%
  filter(grepl('klimaschutz|klimaschutzistjetzt|klimakrise|kohleausstieg|verkehrswende|
               klimawandel|erneuerbar|erneuerbaren|klimapolitik|hochwasser|sozialklimagerecht|
               nachhaltigkeit|nachhaltig|flutkatastrophe|klimapolitik|klimaänderung|klimaentwicklung', text2))
T_CDU_Environment <- paste0(CDU_Environment_Tweets$text2, collapse = "")
FDP_Environment_Tweets <- subset(FDP, select = c(screen_name, text, hashtags))
FDP_Environment_Tweets$text2 <- tolower(FDP_Environment_Tweets$text)
FDP_Environment_Tweets <- FDP_Environment_Tweets %>%
  filter(grepl('klimaschutz|klimaschutzistjetzt|klimakrise|kohleausstieg|verkehrswende|
               klimawandel|erneuerbar|erneuerbaren|klimapolitik|hochwasser|sozialklimagerecht|
               nachhaltigkeit|nachhaltig|flutkatastrophe|klimapolitik|klimaänderung|klimaentwicklung', text2))
T_FDP_Environment <- paste0(FDP_Environment_Tweets$text2, collapse = "")
Gruene_Environment_Tweets <- subset(Gruene, select = c(screen_name, text, hashtags))
Gruene_Environment_Tweets$text2 <- tolower(Gruene_Environment_Tweets$text)
Gruene_Environment_Tweets <- Gruene_Environment_Tweets %>%
  filter(grepl('klimaschutz|klimaschutzistjetzt|klimakrise|kohleausstieg|verkehrswende|
               klimawandel|erneuerbar|erneuerbaren|klimapolitik|hochwasser|sozialklimagerecht|
               nachhaltigkeit|nachhaltig|flutkatastrophe|klimapolitik|klimaänderung|klimaentwicklung', text2))
T_Gruene_Environment <- paste0(Gruene_Environment_Tweets$text2, collapse = "")
Linke_Environment_Tweets <- subset(Linke, select = c(screen_name, text, hashtags))
Linke_Environment_Tweets$text2 <- tolower(Linke_Environment_Tweets$text)
Linke_Environment_Tweets <- Linke_Environment_Tweets %>%
  filter(grepl('klimaschutz|klimaschutzistjetzt|klimakrise|kohleausstieg|verkehrswende|
               klimawandel|erneuerbar|erneuerbaren|klimapolitik|hochwasser|sozialklimagerecht|
               nachhaltigkeit|nachhaltig|flutkatastrophe|klimapolitik|klimaänderung|klimaentwicklung', text2))
T_Linke_Environment <- paste0(Linke_Environment_Tweets$text2, collapse = "")
SPD_Environment_Tweets <- subset(SPD, select = c(screen_name, text, hashtags))
SPD_Environment_Tweets$text2 <- tolower(SPD_Environment_Tweets$text)
SPD_Environment_Tweets <- SPD_Environment_Tweets %>%
  filter(grepl('klimaschutz|klimaschutzistjetzt|klimakrise|kohleausstieg|verkehrswende|
               klimawandel|erneuerbar|erneuerbaren|klimapolitik|hochwasser|sozialklimagerecht|
               nachhaltigkeit|nachhaltig|flutkatastrophe|klimapolitik|klimaänderung|klimaentwicklung', text2))
T_SPD_Environment <- paste0(SPD_Environment_Tweets$text2, collapse = "")

### 7.3.2 Wordscores
Environment_Texts <- c(T_AfD, T_CDU, T_FDP, T_Gruene, T_Linke, T_SPD,
                       T_AfD_Environment, T_CDU_Environment, T_FDP_Environment, T_Gruene_Environment,
                       T_Linke_Environment, T_SPD_Environment)
Environment_Texts <- data.frame(Environment_Texts)
Environment_RefScore <- c("4.854", "4.715", "4.885", "4.650", "4.739", "4.777",
                          "NA", "NA", "NA", "NA", "NA", "NA")
Environment_RefScore <- as.numeric(Environment_RefScore)
Environment_Corpus <- quanteda::corpus(Environment_Texts$Environment_Texts)
Environment_Corpus$RefScore <- Environment_RefScore
quanteda::docnames(Environment_Corpus) <- c("Tweets AfD", "Tweets CDU", "Tweets FDP",
                                            "Tweets Gruene", "Tweets Linke", "Tweets SPD",
                                            "Tweets AfD Environment", "Tweets CDU Environment",
                                            "Tweets FDP Environment", "Tweets Gruene Environment",
                                            "Tweets Linke Environment", "Tweets SPD Environment")
Environment_Toks <- quanteda::tokens(Environment_Corpus, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_remove(pattern = stopwords("de")) %>% 
  tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex")
Environment_DFM <- dfm(Environment_Toks) %>%
  dfm_remove(pattern = stopwords("de")) %>%
  dfm_remove(pattern = G_Stop_Words)
Environment_WS <- textmodel_wordscores(Environment_DFM, y = Environment_Corpus$RefScore, smooth = 1)
summary(Environment_WS)
Environment_Pred <- predict(Environment_WS, se.fit = TRUE, newdata = Environment_DFM)
textplot_scale1d(Environment_Pred)


## 7.4 Topic 3 - Digitization
### 7.4.1 Relevant Tweets
AfD_Digitization_Tweets <- subset(AfD, select = c(screen_name, text, hashtags))
AfD_Digitization_Tweets$text2 <- tolower(AfD_Digitization_Tweets$text)
AfD_Digitization_Tweets <- AfD_Digitization_Tweets %>%
  filter(grepl('digitalisierung|digital|digitalen|digitaletransformation|transformation|
               digitalisieren|digitalisierungsstrategie|modernisierungsjahrzehnt|industrie4.0|
               digitalerevolution|homeoffice|videokonferenz|videoconference|zoom|
               skype|microsoftteams|hybridearbeit|hybridesstudieren|bürokratieabbau', text2))
T_AfD_Digitization <- paste0(AfD_Digitization_Tweets$text2, collapse = "")
CDU_Digitization_Tweets <- subset(CDU, select = c(screen_name, text, hashtags))
CDU_Digitization_Tweets$text2 <- tolower(CDU_Digitization_Tweets$text)
CDU_Digitization_Tweets <- CDU_Digitization_Tweets %>%
  filter(grepl('digitalisierung|digital|digitalen|digitaletransformation|transformation|
               digitalisieren|digitalisierungsstrategie|modernisierungsjahrzehnt|industrie4.0|
               digitalerevolution|homeoffice|videokonferenz|videoconference|zoom|
               skype|microsoftteams|hybridearbeit|hybridesstudieren|bürokratieabbau', text2))
T_CDU_Digitization <- paste0(CDU_Digitization_Tweets$text2, collapse = "")
FDP_Digitization_Tweets <- subset(FDP, select = c(screen_name, text, hashtags))
FDP_Digitization_Tweets$text2 <- tolower(FDP_Digitization_Tweets$text)
FDP_Digitization_Tweets <- FDP_Digitization_Tweets %>%
  filter(grepl('digitalisierung|digital|digitalen|digitaletransformation|transformation|
               digitalisieren|digitalisierungsstrategie|modernisierungsjahrzehnt|industrie4.0|
               digitalerevolution|homeoffice|videokonferenz|videoconference|zoom|
               skype|microsoftteams|hybridearbeit|hybridesstudieren|bürokratieabbau', text2))
T_FDP_Digitization <- paste0(FDP_Digitization_Tweets$text2, collapse = "")
Gruene_Digitization_Tweets <- subset(Gruene, select = c(screen_name, text, hashtags))
Gruene_Digitization_Tweets$text2 <- tolower(Gruene_Digitization_Tweets$text)
Gruene_Digitization_Tweets <- Gruene_Digitization_Tweets %>%
  filter(grepl('digitalisierung|digital|digitalen|digitaletransformation|transformation|
               digitalisierungsstrategie|modernisierungsjahrzehnt|industrie4.0|
               digitalerevolution|homeoffice|videokonferenz|videoconference|zoom|
               skype|microsoftteams|hybridearbeit|hybridesstudieren|bürokratieabbau', text2))
T_Gruene_Digitization <- paste0(Gruene_Digitization_Tweets$text2, collapse = "")
Linke_Digitization_Tweets <- subset(Linke, select = c(screen_name, text, hashtags))
Linke_Digitization_Tweets$text2 <- tolower(Linke_Digitization_Tweets$text)
Linke_Digitization_Tweets <- Linke_Digitization_Tweets %>%
  filter(grepl('digitalisierung|digital|digitalen|digitaletransformation|transformation|
               digitalisieren|digitalisierungsstrategie|modernisierungsjahrzehnt|industrie4.0|
               digitalerevolution|homeoffice|videokonferenz|videoconference|zoom|
               skype|microsoftteams|hybridearbeit|hybridesstudieren|bürokratieabbau', text2))
T_Linke_Digitization <- paste0(Linke_Digitization_Tweets$text2, collapse = "")
SPD_Digitization_Tweets <- subset(SPD, select = c(screen_name, text, hashtags))
SPD_Digitization_Tweets$text2 <- tolower(SPD_Digitization_Tweets$text)
SPD_Digitization_Tweets <- SPD_Digitization_Tweets %>%
  filter(grepl('digitalisierung|digital|digitalen|digitaletransformation|transformation|
               digitalisieren|digitalisierungsstrategie|modernisierungsjahrzehnt|industrie4.0|
               digitalerevolution|homeoffice|videokonferenz|videoconference|zoom|
               skype|microsoftteams|hybridearbeit|hybridesstudieren|bürokratieabbau', text2))
T_SPD_Digitization <- paste0(SPD_Digitization_Tweets$text2, collapse = "")


### 7.4.2 Wordscores
Digitization_Texts <- c(T_AfD, T_CDU, T_FDP, T_Gruene, T_Linke, T_SPD,
                        T_AfD_Digitization, T_CDU_Digitization, T_FDP_Digitization, T_Gruene_Digitization,
                        T_Linke_Digitization, T_SPD_Digitization)
Digitization_Texts <- data.frame(Digitization_Texts)
Digitization_RefScore <- c("4.854", "4.715", "4.885", "4.650", "4.739", "4.777",
                           "NA", "NA", "NA", "NA", "NA", "NA")
Digitization_RefScore <- as.numeric(Digitization_RefScore)
Digitization_Corpus <- quanteda::corpus(Digitization_Texts$Digitization_Texts)
Digitization_Corpus$RefScore <- Digitization_RefScore
quanteda::docnames(Digitization_Corpus) <- c("Tweets AfD", "Tweets CDU", "Tweets FDP",
                                             "Tweets Gruene", "Tweets Linke", "Tweets SPD",
                                             "Tweets AfD Digitization", "Tweets CDU Digitization",
                                             "Tweets FDP Digitization", "Tweets Gruene Digitization",
                                             "Tweets Linke Digitization", "Tweets SPD Digitization")
Digitization_Toks <- quanteda::tokens(Digitization_Corpus, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_remove(pattern = stopwords("de")) %>% 
  tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex")
Digitization_DFM <- dfm(Digitization_Toks) %>%
  dfm_remove(pattern = stopwords("de")) %>%
  dfm_remove(pattern = G_Stop_Words)
Digitization_WS <- textmodel_wordscores(Digitization_DFM, y = Digitization_Corpus$RefScore, smooth = 1)
summary(Digitization_WS)
Digitization_Pred <- predict(Digitization_WS, se.fit = TRUE, newdata = Digitization_DFM)
textplot_scale1d(Digitization_Pred)


###########################################################


# 8. User-Generated Discussions
## 8.1 Subset Data
Polit_Marketing <- Political_Marketing
Polit_Marketing$text2 <- tolower(Polit_Marketing$text)
Marketing_HotPhase <- Political_Marketing[Political_Marketing$created_at > "2021-08-25 00:00:00", ]
Marketing_HotPhase$text2 <- tolower(Marketing_HotPhase$text)
User_Discs <- User_Generated_Discussions
User_Discs$text2 <- tolower(User_Discs$text)

M_Corona_Discussions1 <- Polit_Marketing %>% filter(grepl('corona|coronavirus|covid-19|sars-cov-2|pandemie|lockdown|
                impfen|impfung|impfstoff|geimpft|ungeimpft|impfdosen|
                biontech|pfizer|astrazeneca|moderna|johnson&johnson|inzidenz|
                mpk|masken|maskenskandal|infektion|infektionsschutzgesetz|impfpflicht', text2))
M_Corona_Discussions2 <- Marketing_HotPhase %>% filter(grepl('corona|coronavirus|covid-19|sars-cov-2|pandemie|lockdown|
                impfen|impfung|impfstoff|geimpft|ungeimpft|impfdosen|
                biontech|pfizer|astrazeneca|moderna|johnson&johnson|inzidenz|
                mpk|masken|maskenskandal|infektion|infektionsschutzgesetz|impfpflicht', text2))
U_Corona_Discussions <- User_Discs %>% filter(grepl('corona|coronavirus|covid-19|sars-cov-2|pandemie|lockdown|
                impfen|impfung|impfstoff|geimpft|ungeimpft|impfdosen|
                biontech|pfizer|astrazeneca|moderna|johnson&johnson|inzidenz|
                mpk|masken|maskenskandal|infektion|infektionsschutzgesetz|impfpflicht', text2))
M_Environment_Discussions1 <- Polit_Marketing %>% filter(grepl('klimaschutz|klimaschutzistjetzt|klimakrise|kohleausstieg|verkehrswende|
               klimawandel|erneuerbar|erneuerbaren|klimapolitik|hochwasser|sozialklimagerecht|
               nachhaltigkeit|nachhaltig|flutkatastrophe|klimapolitik|klimaänderung|klimaentwicklung', text2))
M_Environment_Discussions2 <- Marketing_HotPhase %>% filter(grepl('klimaschutz|klimaschutzistjetzt|klimakrise|kohleausstieg|verkehrswende|
               klimawandel|erneuerbar|erneuerbaren|klimapolitik|hochwasser|sozialklimagerecht|
               nachhaltigkeit|nachhaltig|flutkatastrophe|klimapolitik|klimaänderung|klimaentwicklung', text2))
U_Environment_Discussions <- User_Discs %>% filter(grepl('klimaschutz|klimaschutzistjetzt|klimakrise|kohleausstieg|verkehrswende|
               klimawandel|erneuerbar|erneuerbaren|klimapolitik|hochwasser|sozialklimagerecht|
               nachhaltigkeit|nachhaltig|flutkatastrophe|klimapolitik|klimaänderung|klimaentwicklung', text2))
M_Digitization_Discussions1 <- Polit_Marketing %>% filter(grepl('digitalisierung|digital|digitalen|digitaletransformation|transformation|
               digitalisieren|digitalisierungsstrategie|modernisierungsjahrzehnt|industrie4.0|
               digitalerevolution|homeoffice|videokonferenz|videoconference|zoom|
               skype|microsoftteams|hybridearbeit|hybridesstudieren|bürokratieabbau', text2))
M_Digitization_Discussions2 <- Marketing_HotPhase %>% filter(grepl('digitalisierung|digital|digitalen|digitaletransformation|transformation|
               digitalisieren|digitalisierungsstrategie|modernisierungsjahrzehnt|industrie4.0|
               digitalerevolution|homeoffice|videokonferenz|videoconference|zoom|
               skype|microsoftteams|hybridearbeit|hybridesstudieren|bürokratieabbau', text2))
U_Digitization_Discussions <- User_Discs %>% filter(grepl('digitalisierung|digital|digitalen|digitaletransformation|transformation|
               digitalisieren|digitalisierungsstrategie|modernisierungsjahrzehnt|industrie4.0|
               digitalerevolution|homeoffice|videokonferenz|videoconference|zoom|
               skype|microsoftteams|hybridearbeit|hybridesstudieren|bürokratieabbau', text2))

# 8.2 Visualizations
## 8.2.1 Tweet Frequencies
Political_Marketing_TweetVolume <- ts_plot(dplyr::group_by(Polit_Marketing, party_id), "weeks") +
  labs(x = NULL, y = NULL,
       title = "Frequency of Tweets Regarding The Federal Elections",
       caption = "Political Marketing Data / Entire Campaign Term") +
  geom_line() +
  geom_point() + theme_gray() + scale_color_manual(values=c(
    "#00CAFF", "#222222", "#eace09", "#00dd00", "#dd00dd", "#dd0000"))
Political_Marketing_TweetVolume
Political_Marketing_TweetVolume2 <- ts_plot(dplyr::group_by(Marketing_HotPhase, party_id), "days") +
  labs(x = NULL, y = NULL,
       title = "Frequency of Tweets Regarding The Federal Elections",
       caption = "Political Marketing Data / Last 4 Weeks Before The Election") +
  geom_line() +
  geom_point() + theme_gray() + scale_color_manual(values=c(
    "#00CAFF", "#222222", "#eace09", "#00dd00", "#dd00dd", "#dd0000"))
Political_Marketing_TweetVolume2

## 8.2.2 Topic Discussions
M_Corona_Discussions_Plot1 <- ts_plot(dplyr::group_by(M_Corona_Discussions1, party_id), "weeks") +
  labs(x = NULL, y = NULL,
       title = "Tweets About The Covid-19 Pandemic",
       caption = "Political Marketing Data / Entire Campaign Term") +
  geom_line() +
  geom_point() + theme_gray() + scale_color_manual(values=c(
    "#00CAFF", "#222222", "#eace09", "#00dd00", "#dd00dd", "#dd0000"))
M_Corona_Discussions_Plot1

M_Corona_Discussions_Plot2 <- ts_plot(dplyr::group_by(M_Corona_Discussions2, party_id), "days") +
  labs(x = NULL, y = NULL,
       title = "Tweets About The Covid-19 Pandemic",
       caption = "Political Marketing Data / Last 4 Weeks Before The Election") +
  geom_line() +
  geom_point() + theme_gray() + scale_color_manual(values=c(
    "#00CAFF", "#222222", "#eace09", "#00dd00", "#dd00dd", "#dd0000"))
M_Corona_Discussions_Plot2

U_Corona_Discussions_Plot <- ts_plot(U_Corona_Discussions, "days") +
  labs(x = NULL, y = NULL,
       title = "Tweets About The Covid-19 Pandemic",
       caption = "User-Generated Discussions Data / Last 4 Weeks Before The Election") +
  geom_line() +
  geom_point() + theme_gray()
U_Corona_Discussions_Plot

M_Environment_Discussions_Plot1 <- ts_plot(dplyr::group_by(M_Environment_Discussions1, party_id), "weeks") +
  labs(x = NULL, y = NULL,
       title = "Tweets About The Environment",
       caption = "Political Marketing Data / Entire Campaign Term") +
  geom_line() +
  geom_point() + theme_gray() + scale_color_manual(values=c(
    "#00CAFF", "#222222", "#eace09", "#00dd00", "#dd00dd", "#dd0000"))
M_Environment_Discussions_Plot1

M_Environment_Discussions_Plot2 <- ts_plot(dplyr::group_by(M_Environment_Discussions2, party_id), "days") +
  labs(x = NULL, y = NULL,
       title = "Tweets About The Environment",
       caption = "Political Marketing Data / Last 4 Weeks Before The Election") +
  geom_line() +
  geom_point() + theme_gray() + scale_color_manual(values=c(
    "#00CAFF", "#222222", "#eace09", "#00dd00", "#dd00dd", "#dd0000"))
M_Environment_Discussions_Plot2

U_Environment_Discussions_Plot <- ts_plot(U_Environment_Discussions, "days") +
  labs(x = NULL, y = NULL,
       title = "Tweets About The Environment",
       caption = "User-Generated Discussions Data / Last 4 Weeks Before The Election") +
  geom_line() +
  geom_point() + theme_gray()
U_Environment_Discussions_Plot

M_Digitization_Discussions_Plot1 <- ts_plot(dplyr::group_by(M_Digitization_Discussions1, party_id), "weeks") +
  labs(x = NULL, y = NULL,
       title = "Tweets About The Digitization",
       caption = "Political Marketing Data / Entire Campaign Term") +
  geom_line() +
  geom_point() + theme_gray() + scale_color_manual(values=c(
    "#00CAFF", "#222222", "#eace09", "#00dd00", "#dd00dd", "#dd0000"))
M_Digitization_Discussions_Plot1

M_Digitization_Discussions_Plot2 <- ts_plot(dplyr::group_by(M_Digitization_Discussions2, party_id), "days") +
  labs(x = NULL, y = NULL,
       title = "Tweets About The Digitization",
       caption = "Political Marketing Data / Last 4 Weeks Before The Election") +
  geom_line() +
  geom_point() + theme_gray() + scale_color_manual(values=c(
    "#00CAFF", "#222222", "#eace09", "#00dd00", "#dd00dd", "#dd0000"))
M_Digitization_Discussions_Plot2

U_Digitization_Discussions_Plot <- ts_plot(U_Digitization_Discussions, "days") +
  labs(x = NULL, y = NULL,
       title = "Tweets About The Digitization",
       caption = "User-Generated Discussions Data / Last 4 Weeks Before The Election") +
  geom_line() +
  geom_point() + theme_gray()
U_Digitization_Discussions_Plot

## 8.2.3 Party Images
U_Parties_SnE1 <- Users %>%
  filter(party_id != 'None/Other') %>%
  group_by(party_id) %>%
  summarise("Observations" = n(),
            "Sentiment Index" = mean(U_Sentiment_Index),
            "Syuzhet Score" = mean(U_syuzhet),
            "Bing Score" = mean(U_bing),
            "Afinn Score" = mean(U_afinn),
            "NRC Score" = mean(U_nrc),
            "Positive Score" = mean(positive),
            "Negative Score" = mean(negative),
            "Intensity Index" = mean(Intensity_Index))
U_Parties_SnE2 <- Users %>%
  filter(party_id != 'None/Other') %>%
  group_by(party_id) %>%
  summarise("Observations" = n(),
            "Anger" = mean(anger),
            "Anticipation" = mean(anticipation),
            "Disgust" = mean(disgust),
            "Fear" = mean(fear),
            "Joy" = mean(joy),
            "Sadness" = mean(sadness),
            "Surprise" = mean(surprise),
            "Trust" = mean(trust))
U_Parties_SnE1
U_Parties_SnE2

#### Radar Charts
U_Parties_SnE3 <- U_Parties_SnE2
U_Parties_SnE3$party_id <- NULL
U_Parties_SnE3$Observations <- NULL
rownames(U_Parties_SnE3) <- c("AfD", "CDU", "FDP", "Greens", "Left", "SPD")
U_Parties_MaxMin <- data.frame(
  "Anger" = c(0.3, 0), "Anticipation" = c(0.3, 0), "Disgust" = c(0.3, 0),
  "Fear" = c(0.3, 0), "Joy" = c(0.3, 0), "Sadness" = c(0.3, 0),
  "Surprise" = c(0.3, 0), "Trust" = c(0.3, 0))
rownames(U_Parties_MaxMin) <- c("Max", "Min")
U_Parties_SnE4 <- rbind(U_Parties_MaxMin,U_Parties_SnE3)

#### Subsets
U_AfD_Radar <- U_Parties_SnE4[c("Max", "Min", "AfD"), ]
U_CDU_Radar <- U_Parties_SnE4[c("Max", "Min", "CDU"), ]
U_FDP_Radar <- U_Parties_SnE4[c("Max", "Min", "FDP"), ]
U_Greens_Radar <- U_Parties_SnE4[c("Max", "Min", "Greens"), ]
U_Left_Radar <- U_Parties_SnE4[c("Max", "Min", "Left"), ]
U_SPD_Radar <- U_Parties_SnE4[c("Max", "Min", "SPD"), ]

#### Function
beautiful_radarchart <- function(data, color = "#C00000", 
                                 vlabels = colnames(data), vlcex = 0.7,
                                 caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

#### Party Charts Plots
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart(U_AfD_Radar, caxislabels = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.30))
par(op)
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart(U_CDU_Radar, caxislabels = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.30))
par(op)
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart(U_FDP_Radar, caxislabels = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.30))
par(op)
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart(U_Greens_Radar, caxislabels = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.30))
par(op)
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart(U_Left_Radar, caxislabels = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.30))
par(op)
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart(U_SPD_Radar, caxislabels = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.30))
par(op)

## 8.2.4 Candidate Images
U_Candidates_SnE1 <- Users %>%
  filter(candidate_id != 'None/Other') %>%
  group_by(candidate_id) %>%
  summarise("Observations" = n(),
            "Sentiment Index" = mean(U_Sentiment_Index),
            "Syuzhet Score" = mean(U_syuzhet),
            "Bing Score" = mean(U_bing),
            "Afinn Score" = mean(U_afinn),
            "NRC Score" = mean(U_nrc),
            "Positive Score" = mean(positive),
            "Negative Score" = mean(negative),
            "Intensity Index" = mean(Intensity_Index))
U_Candidates_SnE2 <- Users %>%
  filter(candidate_id != 'None/Other') %>%
  group_by(candidate_id) %>%
  summarise("Observations" = n(),
            "Anger" = mean(anger),
            "Anticipation" = mean(anticipation),
            "Disgust" = mean(disgust),
            "Fear" = mean(fear),
            "Joy" = mean(joy),
            "Sadness" = mean(sadness),
            "Surprise" = mean(surprise),
            "Trust" = mean(trust))
U_Candidates_SnE1
U_Candidates_SnE2

#### Radar Charts
U_Candidates_SnE3 <- U_Candidates_SnE2
U_Candidates_SnE3$candidate_id <- NULL
U_Candidates_SnE3$Observations <- NULL
rownames(U_Candidates_SnE3) <- c("Baerbock", "Bartsch", "Chrupalla", "Dobrindt",
                                 "Habeck", "Laschet", "Lindner", "Scholz",
                                 "Weidel", "Wissler")
U_Candidates_MaxMin <- data.frame(
  "Anger" = c(0.4, 0), "Anticipation" = c(0.4, 0), "Disgust" = c(0.4, 0),
  "Fear" = c(0.4, 0), "Joy" = c(0.4, 0), "Sadness" = c(0.4, 0),
  "Surprise" = c(0.4, 0), "Trust" = c(0.4, 0))
rownames(U_Candidates_MaxMin) <- c("Max", "Min")
U_Candidates_SnE4 <- rbind(U_Candidates_MaxMin,U_Candidates_SnE3)

#### Subsets
U_AfD_Radar2 <- U_Candidates_SnE4[c("Max", "Min", "Chrupalla", "Weidel"), ]
U_CDU_Radar2 <- U_Candidates_SnE4[c("Max", "Min", "Dobrindt", "Laschet"), ]
U_FDP_Radar2 <- U_Candidates_SnE4[c("Max", "Min", "Lindner"), ]
U_Greens_Radar2 <- U_Candidates_SnE4[c("Max", "Min", "Baerbock", "Habeck"), ]
U_Left_Radar2 <- U_Candidates_SnE4[c("Max", "Min", "Bartsch", "Wissler"), ]
U_SPD_Radar2 <- U_Candidates_SnE4[c("Max", "Min", "Scholz"), ]

#### Function
beautiful_radarchart2 <- function(data, color = c("#C00000", "#FFC000"), 
                                  vlabels = colnames(data), vlcex = 0.7,
                                  caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

#### Candidates Charts Plots
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart2(U_AfD_Radar2, caxislabels = c(0, 0.1, 0.2, 0.3, 0.4))
par(op)
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart2(U_CDU_Radar2, caxislabels = c(0, 0.1, 0.2, 0.3, 0.4))
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart(U_FDP_Radar2, caxislabels = c(0, 0.1, 0.2, 0.3, 0.4))
par(op)
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart2(U_Greens_Radar2, caxislabels = c(0, 0.1, 0.2, 0.3, 0.4))
par(op)
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart2(U_Left_Radar2, caxislabels = c(0, 0.1, 0.2, 0.3, 0.4))
par(op)
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart(U_SPD_Radar2, caxislabels = c(0, 0.1, 0.2, 0.3, 0.4))
par(op)


###########################################################


# 9. Thesis Visualizations
## 9.1 Sentimental Strategies
SentStrat_Parties <- c(rep("AfD" , 2),
                       rep("CDU/CSU" , 2),
                       rep("FDP" , 2),
                       rep("Greens" , 2),
                       rep("Left" , 2),
                       rep("SPD" , 2))
SentStrat_Sents <- rep(c("positive" , "negative") , 6)
SentStrat_Value <- c("0.3461", "0.3189",
                     "0.4237", "0.2694",
                     "0.4134", "0.3128",
                     "0.4046", "0.2992",
                     "0.3835", "0.3268",
                     "0.4491", "0.2636")
SentStrat_Value <- as.numeric(SentStrat_Value)
SentStrat <- data.frame(SentStrat_Parties, SentStrat_Sents, SentStrat_Value)
SentStrat %>% 
  rename(
    SentStrat_Sents = Sentiments)

SentStrat_Plot <- ggplot(SentStrat, aes(fill=SentStrat_Sents, y=SentStrat_Value, x=SentStrat_Parties)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Parties") +
  ylab("Sentiments")
SentStrat_Plot + scale_fill_manual(name = "Sentiments", values=c("#C00000", "#00B050"))


## 9.2 Image Comparison: sentimental strategies
## Values from M_Parties_SnE_1 and M_Parties_SnE_2
SentImage_Parties <- c(rep("AfD" , 2),
                       rep("CDU/CSU" , 2),
                       rep("FDP" , 2),
                       rep("Greens" , 2),
                       rep("Left" , 2),
                       rep("SPD" , 2))
SentImage_Sents <- rep(c("positive" , "negative") , 6)
SentImage_Value <- c("0.3201", "0.3869",
                     "0.4194", "0.3768",
                     "0.4360", "0.4704",
                     "0.4604", "0.2802",
                     "0.4713", "0.2888",
                     "0.4254", "0.3547")
SentImage_Value <- as.numeric(SentImage_Value)
SentImage <- data.frame(SentImage_Parties, SentImage_Sents, SentImage_Value)
SentImage %>% 
  rename(
    SentImage_Sents = Sentiments)

SentImage_Plot <- ggplot(SentImage, aes(fill=SentImage_Sents, y=SentImage_Value, x=SentImage_Parties)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Parties") +
  ylab("Sentiments")
SentImage_Plot + scale_fill_manual(name = "Sentiments", values=c("#C00000", "#00B050"))


## 9.3 Image Comparison: emotive strategies vs. public perceptions
## Values from M_Parties_SnE_1 and M_Parties_SnE_2, and
## U_Parties_SnE_1 and U_Parties_SnE_2
ImageComp_Anger <- c("0.0866", "0.1036",
                     "0.0611", "0.1187",
                     "0.0699", "0.1206",
                     "0.0729", "0.0691",
                     "0.0764", "0.0583",
                     "0.0866", "0.1430")
ImageComp_Anticipation <- c("0.1370", "0.1402",
                            "0.1496", "0.1222",
                            "0.1379", "0.1434",
                            "0.1437", "0.1214",
                            "0.1565", "0.1374",
                            "0.1488", "0.1263")
ImageComp_Disgust <- c("0.0544", "0.0877",
                       "0.0369", "0.0781",
                       "0.0395", "0.0726",
                       "0.0418", "0.0389",
                       "0.0461", "0.0429",
                       "0.0376", "0.1109")
ImageComp_Fear <- c("0.1189", "0.1684",
                    "0.0989", "0.1433",
                    "0.1031", "0.1326",
                    "0.1056", "0.0783",
                    "0.1156", "0.0935",
                    "0.0969", "0.1445")
ImageComp_Joy <- c("0.0843", "0.0645",
                   "0.1107", "0.0815",
                   "0.0958", "0.0953",
                   "0.0969", "0.1264",
                   "0.1041", "0.0930",
                   "0.1041", "0.1109")
ImageComp_Sadness <- c("0.1705", "0.2327",
                       "0.1458", "0.1647",
                       "0.1574", "0.1973",
                       "0.1552", "0.1264",
                       "0.1744", "0.1410",
                       "0.1374", "0.1712")
ImageComp_Surprise <- c("0.0553", "0.0371",
                        "0.0666", "0.0541",
                        "0.0687", "0.0883",
                        "0.0679", "0.0409",
                        "0.0566", "0.0650",
                        "0.0728", "0.1254")
ImageComp_Trust <- c("0.1769", "0.1424",
                     "0.2174", "0.2054",
                     "0.1981", "0.2270",
                     "0.1940", "0.2236",
                     "0.1940", "0.2388",
                     "0.2168", "0.1996")
ImageComp0 <- data.frame(ImageComp_Anger, ImageComp_Anticipation, ImageComp_Disgust, ImageComp_Fear,
                         ImageComp_Joy, ImageComp_Sadness, ImageComp_Surprise, ImageComp_Trust)
ImageComp0$ImageComp_Anger <- as.numeric(ImageComp0$ImageComp_Anger)
ImageComp0$ImageComp_Anticipation <- as.numeric(ImageComp0$ImageComp_Anticipation)
ImageComp0$ImageComp_Disgust <- as.numeric(ImageComp0$ImageComp_Disgust)
ImageComp0$ImageComp_Fear <- as.numeric(ImageComp0$ImageComp_Fear)
ImageComp0$ImageComp_Joy <- as.numeric(ImageComp0$ImageComp_Joy)
ImageComp0$ImageComp_Sadness <- as.numeric(ImageComp0$ImageComp_Sadness)
ImageComp0$ImageComp_Surprise <- as.numeric(ImageComp0$ImageComp_Surprise)
ImageComp0$ImageComp_Trust <- as.numeric(ImageComp0$ImageComp_Trust)
colnames(ImageComp0) <- c("Anger","Anticipation","Disgust","Fear",
                          "Joy","Sadness","Surprise","Trust")
rownames(ImageComp0) <- c("AfD Emotive", "AfD Public",
                          "CDU/CSU Emotive", "CDU/CSU Public",
                          "FDP Emotive", "FDP Public",
                          "Greens Emotive", "Greens Public",
                          "Left Emotive", "Left Public",
                          "SPD Emotive", "SPD Public")
ImageComp_MaxMin <- data.frame(
  "Anger" = c(0.25, 0), "Anticipation" = c(0.25, 0), "Disgust" = c(0.25, 0),
  "Fear" = c(0.25, 0), "Joy" = c(0.25, 0), "Sadness" = c(0.25, 0),
  "Surprise" = c(0.25, 0), "Trust" = c(0.25, 0))
rownames(ImageComp_MaxMin) <- c("Max", "Min")
ImageComp <- rbind(ImageComp0, ImageComp_MaxMin)
ImageComp_AfD_Radar <- ImageComp[c("Max", "Min", "AfD Emotive", "AfD Public"), ]
ImageComp_CDU_Radar <- ImageComp[c("Max", "Min", "CDU/CSU Emotive", "CDU/CSU Public"), ]
ImageComp_FDP_Radar <- ImageComp[c("Max", "Min", "FDP Emotive", "FDP Public"), ]
ImageComp_Greens_Radar <- ImageComp[c("Max", "Min", "Greens Emotive", "Greens Public"), ]
ImageComp_Left_Radar <- ImageComp[c("Max", "Min", "Left Emotive", "Left Public"), ]
ImageComp_SPD_Radar <- ImageComp[c("Max", "Min", "SPD Emotive", "SPD Public"), ]
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart2(ImageComp_AfD_Radar, caxislabels = c(0, 0.05, 0.1, 0.15, 0.2))
par(op)
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart2(ImageComp_CDU_Radar, caxislabels = c(0, 0.05, 0.1, 0.15, 0.2))
par(op)
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart2(ImageComp_FDP_Radar, caxislabels = c(0, 0.05, 0.1, 0.15, 0.2))
par(op)
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart2(ImageComp_Greens_Radar, caxislabels = c(0, 0.05, 0.1, 0.15, 0.2))
par(op)
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart2(ImageComp_Left_Radar, caxislabels = c(0, 0.05, 0.1, 0.15, 0.2))
par(op)
op <- par(mar = c(1, 1, 1, 1))
beautiful_radarchart2(ImageComp_SPD_Radar, caxislabels = c(0, 0.05, 0.1, 0.15, 0.2))
par(op)



## 9.4 Stance Plots
### 9.4.1 General Stance Analysis
GSA_Party <- c("AfD", "CDU/CSU", "FDP", "Greens", "Left", "SPD",
               "AfD", "CDU/CSU", "FDP", "Greens", "Left", "SPD",
               "AfD", "CDU/CSU", "FDP", "Greens", "Left", "SPD")
GSA_Stances <- c("5.367561", "4.828087", "4.949745", "4.473521", "4.291900", "4.560059",
                 "5.015103", "4.715467", "4.884924", "4.650361", "4.555919", "4.605357",
                 "4.853591", "4.818023", "4.816554", "4.752792", "4.739039", "4.776970")
GSA_Types <- c("Party Program 2017", "Party Program 2017", "Party Program 2017",
               "Party Program 2017","Party Program 2017","Party Program 2017",
               "Party Program 2021", "Party Program 2021", "Party Program 2021",
               "Party Program 2021","Party Program 2021","Party Program 2021",
               "Tweets 2021", "Tweets 2021", "Tweets 2021", "Tweets 2021","Tweets 2021","Tweets 2021")
GSA <- data.frame(GSA_Party, GSA_Stances, GSA_Types)
GSA$GSA_Stances <- as.numeric(GSA$GSA_Stances)
colnames(GSA) <- c("Parties","Stances","Types")

ggplot(GSA, aes(x=Stances, y=Types, color=Parties, alpha=0.9)) + 
  xlab("Left-to-Right Positioning Document") +
  ylab("Document Type") +
  geom_point(shape=18, size=6, stroke=3) + scale_color_manual(values=c(
    "#00CAFF", "#222222", "#eace09", "#00dd00", "#dd00dd", "#dd0000")) +
  scale_fill_manual(name = "Parties")


### 9.4.2 Topic-Specific Stance Analysis
TSSA_Party <- c("AfD", "CDU/CSU", "FDP", "Greens", "Left", "SPD",
                "AfD", "CDU/CSU", "FDP", "Greens", "Left", "SPD",
                "AfD", "CDU/CSU", "FDP", "Greens", "Left", "SPD",
                "AfD", "CDU/CSU", "FDP", "Greens", "Left", "SPD")
TSSA_Stances <- c("4.777840", "4.764837", "4.775335", "4.758969", "4.764782", "4.766315",
                  "4.778739", "4.766199", "4.776989", "4.760684", "4.766164", "4.768595",
                  "4.773740", "4.762155", "4.773796", "4.754189", "4.760849", "4.761650",
                  "4.776430", "4.764508", "4.775597", "4.760672", "4.765364", "4.765578")
TSSA_Types <- c("Overall Tweets","Overall Tweets",
                "Overall Tweets","Overall Tweets",
                "Overall Tweets","Overall Tweets",
                "Corona Tweets","Corona Tweets","Corona Tweets",
                "Corona Tweets","Corona Tweets","Corona Tweets",
                "Environment Tweets","Environment Tweets","Environment Tweets",
                "Environment Tweets","Environment Tweets","Environment Tweets",
                "Digitization Tweets","Digitization Tweets","Digitization Tweets",
                "Digitization Tweets","Digitization Tweets","Digitization Tweets")
TSSA <- data.frame(TSSA_Party, TSSA_Stances, TSSA_Types)
TSSA$TSSA_Stances <- as.numeric(TSSA$TSSA_Stances)
colnames(TSSA) <- c("Parties","Stances","Types")

ggplot(TSSA, aes(x=Stances, y=Types, color=Parties, alpha=0.9)) + 
  xlab("Left-to-Right Positioning Document") +
  ylab("Document Type") +
  geom_point(shape=18, size=6, stroke=3) + scale_color_manual(values=c(
    "#00CAFF", "#222222", "#eace09", "#00dd00", "#dd00dd", "#dd0000")) +
  scale_fill_manual(name = "Parties")

# 9.5 Frequency Plots
Political_Marketing_TweetVolume <- ts_plot(dplyr::group_by(Polit_Marketing, party_id), "weeks") +
  labs(x = NULL, y= NULL,
       title = NULL,
       caption = "Political Marketing Data / Entire Campaign Term") +
  labs(fill = "Parties") +
  geom_line() +
  geom_point() +
  scale_color_manual(name = "Parties", values=c(
    "#00CAFF", "#222222", "#eace09", "#00dd00", "#dd00dd", "#dd0000"))
Political_Marketing_TweetVolume

M_Corona_Discussions_Plot1 <- ts_plot(dplyr::group_by(M_Corona_Discussions1, party_id), "weeks") +
  labs(x = NULL, y = NULL,
       title = NULL,
       caption = "Political Marketing Data / Entire Campaign Term") +
  geom_line() +
  geom_point() + scale_color_manual(name = "Parties", values=c(
    "#00CAFF", "#222222", "#eace09", "#00dd00", "#dd00dd", "#dd0000"))
M_Corona_Discussions_Plot1

M_Corona_Discussions_Plot2 <- ts_plot(dplyr::group_by(M_Corona_Discussions2, party_id), "days") +
  labs(x = NULL, y = NULL,
       title = NULL,
       caption = "Political Marketing Data / Last 4 Weeks Before The Election") +
  geom_line() +
  geom_point() + theme_gray() + scale_color_manual(name = "Parties", values=c(
    "#00CAFF", "#222222", "#eace09", "#00dd00", "#dd00dd", "#dd0000"))
M_Corona_Discussions_Plot2

M_Environment_Discussions_Plot1 <- ts_plot(dplyr::group_by(M_Environment_Discussions1, party_id), "weeks") +
  labs(x = NULL, y = NULL,
       title = NULL,
       caption = "Political Marketing Data / Entire Campaign Term") +
  geom_line() +
  geom_point() + scale_color_manual(name = "Parties", values=c(
    "#00CAFF", "#222222", "#eace09", "#00dd00", "#dd00dd", "#dd0000"))
M_Environment_Discussions_Plot1

M_Environment_Discussions_Plot2 <- ts_plot(dplyr::group_by(M_Environment_Discussions2, party_id), "days") +
  labs(x = NULL, y = NULL,
       title = NULL,
       caption = "Political Marketing Data / Last 4 Weeks Before The Election") +
  geom_line() +
  geom_point() + scale_color_manual(name = "Parties", values=c(
    "#00CAFF", "#222222", "#eace09", "#00dd00", "#dd00dd", "#dd0000"))
M_Environment_Discussions_Plot2

M_Digitization_Discussions_Plot1 <- ts_plot(dplyr::group_by(M_Digitization_Discussions1, party_id), "weeks") +
  labs(x = NULL, y = NULL,
       title = NULL,
       caption = "Political Marketing Data / Entire Campaign Term") +
  geom_line() +
  geom_point() + scale_color_manual(name = "Parties", values=c(
    "#00CAFF", "#222222", "#eace09", "#00dd00", "#dd00dd", "#dd0000"))
M_Digitization_Discussions_Plot1

M_Digitization_Discussions_Plot2 <- ts_plot(dplyr::group_by(M_Digitization_Discussions2, party_id), "days") +
  labs(x = NULL, y = NULL,
       title = NULL,
       caption = "Political Marketing Data / Last 4 Weeks Before The Election") +
  geom_line() +
  geom_point() + scale_color_manual(name = "Parties", values=c(
    "#00CAFF", "#222222", "#eace09", "#00dd00", "#dd00dd", "#dd0000"))
M_Digitization_Discussions_Plot2


# 9.6 RILE Index
graph_rile <- big_parties %>%
  group_by(partyname) %>%
  ggplot(aes(x= as.Date(edate), y = rile_index, color=partyname)) +
  geom_point() + geom_line(size = 1.2) +
  geom_text(data=. %>%
              arrange(desc(edate)) %>%
              group_by(partyname) %>%
              slice(1),
            aes(label=c('Greens', 'AfD', 'CDU/CSU', 'FDP', 'SPD', 'Left')), position=position_jitter(height=2), stat='identity', hjust = 0, size = 3, angle=40) +
  theme(legend.position = "None") +
  xlab("Year") + ylab("Right Left (RILE) Index")
graph_rile + scale_color_manual(values=c("#00dd00", "#00CAFF", "#222222", "#eace09", "#dd0000", "#dd00dd"))