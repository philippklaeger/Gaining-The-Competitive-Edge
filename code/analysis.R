###########################################################
#                 A N A L Y S I S                         #
###########################################################




################# TABLE OF CONTENTS #######################
##### I. Data & Packages
##### II. Party Manifesto Texts
##### III. Tweet Texts
##### IV. Wordscores
##### V. Difference Testing
##### VI. Visuals


###########################################################


################# I. DATA & PACKAGES ######################
setwd()

library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(tidytext)
library(textstem)
library(syuzhet)
library(plotly)
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
require(quanteda.corpora)
require(readtext)
library(ggdendro)
library(rtweet)
library(fmsb)
library(viridis)
library(ggpubr)
library(tidyverse)
library(car)


Political_Marketing <- 
  read.csv(file = './data/Political_Marketing.csv', sep = ",", fileEncoding = "UTF-8")


###########################################################


################# II. PARTY MANIFESTO TEXTS ###############
## 2017
PP_AfD_2017 <- read.delim("./docs/PP AfD 2017.txt", encoding = "UTF-8", header = FALSE)
PP_AfD_2017 <- paste(PP_AfD_2017$V1, collapse = " ")
PP_CDU_2017 <- read.delim("./docs/PP CDU 2017.txt", encoding = "UTF-8", header = FALSE)
PP_CDU_2017 <- paste(PP_CDU_2017$V1, collapse = " ")
PP_FDP_2017 <- read.delim("./docs/PP FDP 2017.txt", encoding = "UTF-8", header = FALSE)
PP_FDP_2017 <- paste(PP_FDP_2017$V1, collapse = " ")
PP_Gruene_2017 <- read.delim("./docs/PP Gruene 2017.txt", encoding = "UTF-8", header = FALSE)
PP_Gruene_2017 <- paste(PP_Gruene_2017$V1, collapse = " ")
PP_Linke_2017 <- read.delim("./docs/PP Linke 2017.txt", encoding = "UTF-8", header = FALSE)
PP_Linke_2017 <- paste(PP_Linke_2017$V1, collapse = " ")
PP_SPD_2017 <- read.delim("./docs/PP SPD 2017.txt", encoding = "UTF-8", header = FALSE)
PP_SPD_2017 <- paste(PP_SPD_2017$V1, collapse = " ")

## 2021
PP_AfD_2021 <- read.delim("./docs/PP AfD 2021.txt", encoding = "UTF-8", header = FALSE)
PP_AfD_2021 <- paste(PP_AfD_2021$V1, collapse = " ")
PP_CDU_2021 <- read.delim("./docs/PP CDU 2021.txt", encoding = "UTF-8", header = FALSE)
PP_CDU_2021 <- paste(PP_CDU_2021$V1, collapse = " ")
PP_FDP_2021 <- read.delim("./docs/PP FDP 2021.txt", encoding = "UTF-8", header = FALSE)
PP_FDP_2021 <- paste(PP_FDP_2021$V1, collapse = " ")
PP_Gruene_2021 <- read.delim("./docs/PP Gruene 2021.txt", encoding = "UTF-8", header = FALSE)
PP_Gruene_2021 <- paste(PP_Gruene_2021$V1, collapse = " ")
PP_Linke_2021 <- read.delim("./docs/PP Linke 2021.txt", encoding = "UTF-8", header = FALSE)
PP_Linke_2021 <- paste(PP_Linke_2021$V1, collapse = " ")
PP_SPD_2021 <- read.delim("./docs/PP SPD 2021.txt", encoding = "UTF-8", header = FALSE)
PP_SPD_2021 <- paste(PP_SPD_2021$V1, collapse = " ")


###########################################################


################# III. TWEET TEXTS ########################
Candidate_Tweets <- subset(Political_Marketing,
                           select = c(user_id, screen_name, text, party_id))


################# AFD TEXTS ###############################
##### Overall Tweets
AfD_Tweets = filter(Candidate_Tweets, party_id == "AfD")
length(unique(AfD_Tweets[["screen_name"]]))
unique(AfD_Tweets[["screen_name"]])
AfD_Tweets_1 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "AfD")
T_AfD_1 <- paste0(AfD_Tweets_1$text, collapse = " ")
AfD_Tweets_2 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "AfDimBundestag")
T_AfD_2 <- paste0(AfD_Tweets_2$text, collapse = " ")
AfD_Tweets_3 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Alice_Weidel")
T_AfD_3 <- paste0(AfD_Tweets_3$text, collapse = " ")
AfD_Tweets_4 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Tino_Chrupalla")
T_AfD_4 <- paste0(AfD_Tweets_4$text, collapse = " ")
AfD_Tweets_5 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Beatrix_vStorch")
T_AfD_5 <- paste0(AfD_Tweets_5$text, collapse = " ")
AfD_Tweets_6 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Frank_Pasemann")
T_AfD_6 <- paste0(AfD_Tweets_6$text, collapse = " ")
AfD_Tweets_7 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "M_Reichardt_AfD")
T_AfD_7 <- paste0(AfD_Tweets_7$text, collapse = " ")
AfD_Tweets_8 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Renner_AfD")
T_AfD_8 <- paste0(AfD_Tweets_8$text, collapse = " ")
AfD_Tweets_9 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "MdB_Lucassen")
T_AfD_9 <- paste0(AfD_Tweets_9$text, collapse = " ")
AfD_Tweets_10 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Th_Seitz_AfD")
T_AfD_10 <- paste0(AfD_Tweets_10$text, collapse = " ")
AfD_Tweets_11 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "JuergenBraunAfD")
T_AfD_11 <- paste0(AfD_Tweets_11$text, collapse = " ")
AfD_Tweets_12 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Leif_Erik_Holm")
T_AfD_12 <- paste0(AfD_Tweets_12$text, collapse = " ")
AfD_Tweets_13 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "PeterFelser")
T_AfD_13 <- paste0(AfD_Tweets_13$text, collapse = " ")
AfD_Tweets_14 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Thomas_Ehrhorn")
T_AfD_14 <- paste0(AfD_Tweets_14$text, collapse = " ")
AfD_Tweets_15 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Paul_Podolay")
T_AfD_15 <- paste0(AfD_Tweets_15$text, collapse = " ")
AfD_Tweets_16 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "DrHollnagel")
T_AfD_16 <- paste0(AfD_Tweets_16$text, collapse = " ")
AfD_Tweets_17 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "ulschzi")
T_AfD_17 <- paste0(AfD_Tweets_17$text, collapse = " ")
AfD_Tweets_18 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Marcus_Buehl")
T_AfD_18 <- paste0(AfD_Tweets_18$text, collapse = " ")
AfD_Tweets_19 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "WolfgangWiehle")
T_AfD_19 <- paste0(AfD_Tweets_19$text, collapse = " ")
AfD_Tweets_20 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "EnricoKomning")
T_AfD_20 <- paste0(AfD_Tweets_20$text, collapse = " ")
AfD_Tweets_20 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Witt_Uwe")
T_AfD_20 <- paste0(AfD_Tweets_20$text, collapse = " ")
AfD_Tweets_21 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "andreasbleckmdb")
T_AfD_21 <- paste0(AfD_Tweets_21$text, collapse = " ")
AfD_Tweets_22 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "DrFriesenMdB")
T_AfD_22 <- paste0(AfD_Tweets_22$text, collapse = " ")
AfD_Tweets_23 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Gerold_Otten")
T_AfD_23 <- paste0(AfD_Tweets_23$text, collapse = " ")
AfD_Tweets_24 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "M_HarderKuehnel")
T_AfD_24 <- paste0(AfD_Tweets_24$text, collapse = " ")
AfD_Tweets_25 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "CorinnaMiazga")
T_AfD_25 <- paste0(AfD_Tweets_25$text, collapse = " ")
AfD_Tweets_26 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "PetrBystronAfD")
T_AfD_26 <- paste0(AfD_Tweets_26$text, collapse = " ")
AfD_Tweets_27 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Nicole_Hoechst")
T_AfD_27 <- paste0(AfD_Tweets_27$text, collapse = " ")
AfD_Tweets_28 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "h_weyel")
T_AfD_28 <- paste0(AfD_Tweets_28$text, collapse = " ")
AfD_Tweets_29 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "DirkSpaniel")
T_AfD_29 <- paste0(AfD_Tweets_29$text, collapse = " ")
AfD_Tweets_30 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "SteffenKotre")
T_AfD_30 <- paste0(AfD_Tweets_30$text, collapse = " ")
AfD_Tweets_31 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "MarcBernhardAfD")
T_AfD_31 <- paste0(AfD_Tweets_31$text, collapse = " ")
AfD_Tweets_32 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Martin_Hess_AfD")
T_AfD_32 <- paste0(AfD_Tweets_32$text, collapse = " ")
AfD_Tweets_33 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "AfDProtschka")
T_AfD_33 <- paste0(AfD_Tweets_33$text, collapse = " ")
AfD_Tweets_34 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Buettner_MdB")
T_AfD_34 <- paste0(AfD_Tweets_34$text, collapse = " ")
AfD_Tweets_35 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "ChrWirthMdB")
T_AfD_35 <- paste0(AfD_Tweets_35$text, collapse = " ")
AfD_Tweets_36 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Pohl_MdB")
T_AfD_36 <- paste0(AfD_Tweets_36$text, collapse = " ")
AfD_Tweets_37 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Rene_Springer")
T_AfD_37 <- paste0(AfD_Tweets_37$text, collapse = " ")
AfD_Tweets_38 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "R_Hartwig_AfD")
T_AfD_38 <- paste0(AfD_Tweets_38$text, collapse = " ")
AfD_Tweets_39 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Schneider_AfD")
T_AfD_39 <- paste0(AfD_Tweets_39$text, collapse = " ")
AfD_Tweets_40 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "ElsnervonGronow")
T_AfD_40 <- paste0(AfD_Tweets_40$text, collapse = " ")
AfD_Tweets_41 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "Jochen_Haug")
T_AfD_41 <- paste0(AfD_Tweets_41$text, collapse = " ")
AfD_Tweets_42 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "mrosek1958")
T_AfD_42 <- paste0(AfD_Tweets_42$text, collapse = " ")
AfD_Tweets_43 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "NKleinwaechter")
T_AfD_43 <- paste0(AfD_Tweets_43$text, collapse = " ")
AfD_Tweets_44 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "KayGottschalk1")
T_AfD_44 <- paste0(AfD_Tweets_44$text, collapse = " ")
AfD_Tweets_45 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "GottfriedCurio")
T_AfD_45 <- paste0(AfD_Tweets_45$text, collapse = " ")
AfD_Tweets_46 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "MalteKaufmann")
T_AfD_46 <- paste0(AfD_Tweets_46$text, collapse = " ")
AfD_Tweets_47 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "StBrandner")
T_AfD_47 <- paste0(AfD_Tweets_47$text, collapse = " ")
AfD_Tweets_48 <- AfD_Tweets %>%
  filter(AfD_Tweets$screen_name == "JoanaCotar")
T_AfD_48 <- paste0(AfD_Tweets_48$text, collapse = " ")

T_AfD_Texts <- c(T_AfD_1,T_AfD_2,T_AfD_3,T_AfD_4,T_AfD_5,T_AfD_6,T_AfD_7,T_AfD_8,T_AfD_9,T_AfD_10,
                 T_AfD_11,T_AfD_12,T_AfD_13,T_AfD_14,T_AfD_15,T_AfD_16,T_AfD_17,T_AfD_18,T_AfD_19,T_AfD_20,
                 T_AfD_21,T_AfD_22,T_AfD_23,T_AfD_24,T_AfD_25,T_AfD_26,T_AfD_27,T_AfD_28,T_AfD_29,T_AfD_30,
                 T_AfD_31,T_AfD_32,T_AfD_33,T_AfD_34,T_AfD_35,T_AfD_36,T_AfD_37,T_AfD_38,T_AfD_39,T_AfD_40,
                 T_AfD_41,T_AfD_42,T_AfD_43,T_AfD_44,T_AfD_45,T_AfD_46,T_AfD_47,T_AfD_48)
T_AfD_Texts <- data.frame(T_AfD_Texts)


##### Corona Tweets
AfD_TweetsC <- AfD_Tweets %>% filter(grepl('corona|coronavirus|covid-19|sars-cov-2|pandemie|lockdown|
                impfen|impfung|impfstoff|geimpft|ungeimpft|impfdosen|
                biontech|pfizer|astrazeneca|moderna|johnson&johnson|inzidenz|
                mpk|masken|maskenskandal|infektion|infektionsschutzgesetz|impfpflicht', text))
AfD_Tweets_1C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "AfD")
T_AfD_1C <- paste0(AfD_Tweets_1C$text, collapse = " ")
AfD_Tweets_2C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "AfDimBundestag")
T_AfD_2C <- paste0(AfD_Tweets_2C$text, collapse = " ")
AfD_Tweets_3C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Alice_Weidel")
T_AfD_3C <- paste0(AfD_Tweets_3C$text, collapse = " ")
AfD_Tweets_4C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Tino_Chrupalla")
T_AfD_4C <- paste0(AfD_Tweets_4C$text, collapse = " ")
AfD_Tweets_5C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Beatrix_vStorch")
T_AfD_5C <- paste0(AfD_Tweets_5C$text, collapse = " ")
AfD_Tweets_6C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Frank_Pasemann")
T_AfD_6C <- paste0(AfD_Tweets_6C$text, collapse = " ")
AfD_Tweets_7C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "M_Reichardt_AfD")
T_AfD_7C <- paste0(AfD_Tweets_7C$text, collapse = " ")
AfD_Tweets_8C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Renner_AfD")
T_AfD_8C <- paste0(AfD_Tweets_8C$text, collapse = " ")
AfD_Tweets_9C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "MdB_Lucassen")
T_AfD_9C <- paste0(AfD_Tweets_9C$text, collapse = " ")
AfD_Tweets_10C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Th_Seitz_AfD")
T_AfD_10C <- paste0(AfD_Tweets_10C$text, collapse = " ")
AfD_Tweets_11C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "JuergenBraunAfD")
T_AfD_11C <- paste0(AfD_Tweets_11C$text, collapse = " ")
AfD_Tweets_12C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Leif_Erik_Holm")
T_AfD_12C <- paste0(AfD_Tweets_12C$text, collapse = " ")
AfD_Tweets_13C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "PeterFelser")
T_AfD_13C <- paste0(AfD_Tweets_13C$text, collapse = " ")
AfD_Tweets_14C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Thomas_Ehrhorn")
T_AfD_14C <- paste0(AfD_Tweets_14C$text, collapse = " ")
AfD_Tweets_15C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Paul_Podolay")
T_AfD_15C <- paste0(AfD_Tweets_15C$text, collapse = " ")
AfD_Tweets_16C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "DrHollnagel")
T_AfD_16C <- paste0(AfD_Tweets_16C$text, collapse = " ")
AfD_Tweets_17C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "ulschzi")
T_AfD_17C <- paste0(AfD_Tweets_17C$text, collapse = " ")
AfD_Tweets_18C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Marcus_Buehl")
T_AfD_18C <- paste0(AfD_Tweets_18C$text, collapse = " ")
AfD_Tweets_19C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "WolfgangWiehle")
T_AfD_19C <- paste0(AfD_Tweets_19C$text, collapse = " ")
AfD_Tweets_20C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "EnricoKomning")
T_AfD_20C <- paste0(AfD_Tweets_20C$text, collapse = " ")
AfD_Tweets_20C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Witt_Uwe")
T_AfD_20C <- paste0(AfD_TweetsC_20$text, collapse = " ")
AfD_Tweets_21C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "andreasbleckmdb")
T_AfD_21C <- paste0(AfD_Tweets_21C$text, collapse = " ")
AfD_Tweets_22C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "DrFriesenMdB")
T_AfD_22C <- paste0(AfD_Tweets_22C$text, collapse = " ")
AfD_Tweets_23C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Gerold_Otten")
T_AfD_23C <- paste0(AfD_Tweets_23C$text, collapse = " ")
AfD_Tweets_24C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "M_HarderKuehnel")
T_AfD_24C <- paste0(AfD_Tweets_24C$text, collapse = " ")
AfD_Tweets_25C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "CorinnaMiazga")
T_AfD_25C <- paste0(AfD_Tweets_25C$text, collapse = " ")
AfD_Tweets_26C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "PetrBystronAfD")
T_AfD_26C <- paste0(AfD_Tweets_26C$text, collapse = " ")
AfD_Tweets_27C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Nicole_Hoechst")
T_AfD_27C <- paste0(AfD_Tweets_27C$text, collapse = " ")
AfD_Tweets_28C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "h_weyel")
T_AfD_28C <- paste0(AfD_Tweets_28C$text, collapse = " ")
AfD_Tweets_29C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "DirkSpaniel")
T_AfD_29C <- paste0(AfD_Tweets_29C$text, collapse = " ")
AfD_Tweets_30C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "SteffenKotre")
T_AfD_30C <- paste0(AfD_Tweets_30C$text, collapse = " ")
AfD_Tweets_31C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "MarcBernhardAfD")
T_AfD_31C <- paste0(AfD_Tweets_31C$text, collapse = " ")
AfD_Tweets_32C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Martin_Hess_AfD")
T_AfD_32C <- paste0(AfD_Tweets_32C$text, collapse = " ")
AfD_Tweets_33C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "AfDProtschka")
T_AfD_33C <- paste0(AfD_Tweets_33C$text, collapse = " ")
AfD_Tweets_34C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Buettner_MdB")
T_AfD_34C <- paste0(AfD_Tweets_34C$text, collapse = " ")
AfD_Tweets_35C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "ChrWirthMdB")
T_AfD_35C <- paste0(AfD_Tweets_35C$text, collapse = " ")
AfD_Tweets_36C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Pohl_MdB")
T_AfD_36C <- paste0(AfD_Tweets_36C$text, collapse = " ")
AfD_Tweets_37C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Rene_Springer")
T_AfD_37C <- paste0(AfD_Tweets_37C$text, collapse = " ")
AfD_Tweets_38C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "R_Hartwig_AfD")
T_AfD_38C <- paste0(AfD_Tweets_38C$text, collapse = " ")
AfD_Tweets_39C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Schneider_AfD")
T_AfD_39C <- paste0(AfD_Tweets_39C$text, collapse = " ")
AfD_Tweets_40C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "ElsnervonGronow")
T_AfD_40C <- paste0(AfD_Tweets_40C$text, collapse = " ")
AfD_Tweets_41C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "Jochen_Haug")
T_AfD_41C <- paste0(AfD_Tweets_41C$text, collapse = " ")
AfD_Tweets_42C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "mrosek1958")
T_AfD_42C <- paste0(AfD_Tweets_42C$text, collapse = " ")
AfD_Tweets_43C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "NKleinwaechter")
T_AfD_43C <- paste0(AfD_Tweets_43C$text, collapse = " ")
AfD_Tweets_44C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "KayGottschalk1")
T_AfD_44C <- paste0(AfD_Tweets_44C$text, collapse = " ")
AfD_Tweets_45C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "GottfriedCurio")
T_AfD_45C <- paste0(AfD_Tweets_45C$text, collapse = " ")
AfD_Tweets_46C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "MalteKaufmann")
T_AfD_46C <- paste0(AfD_Tweets_46C$text, collapse = " ")
AfD_Tweets_47C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "StBrandner")
T_AfD_47C <- paste0(AfD_Tweets_47C$text, collapse = " ")
AfD_Tweets_48C <- AfD_TweetsC %>%
  filter(AfD_TweetsC$screen_name == "JoanaCotar")
T_AfD_48C <- paste0(AfD_Tweets_48C$text, collapse = " ")

T_AfD_TextsC <- c(T_AfD_1C,T_AfD_2C,T_AfD_3C,T_AfD_4C,T_AfD_5C,T_AfD_6C,T_AfD_7C,T_AfD_8C,T_AfD_9C,T_AfD_10C,
                  T_AfD_11C,T_AfD_12C,T_AfD_13C,T_AfD_14C,T_AfD_15C,T_AfD_16C,T_AfD_17C,T_AfD_18C,T_AfD_19C,T_AfD_20C,
                  T_AfD_21C,T_AfD_22C,T_AfD_23C,T_AfD_24C,T_AfD_25C,T_AfD_26C,T_AfD_27C,T_AfD_28C,T_AfD_29C,T_AfD_30C,
                  T_AfD_31C,T_AfD_32C,T_AfD_33C,T_AfD_34C,T_AfD_35C,T_AfD_36C,T_AfD_37C,T_AfD_38C,T_AfD_39C,T_AfD_40C,
                  T_AfD_41C,T_AfD_42C,T_AfD_43C,T_AfD_44C,T_AfD_45C,T_AfD_46C,T_AfD_47C,T_AfD_48C)
T_AfD_TextsC <- data.frame(T_AfD_TextsC)


##### Environment Tweets
AfD_TweetsE <- AfD_Tweets %>% filter(grepl('klimaschutz|klimaschutzistjetzt|klimakrise|kohleausstieg|verkehrswende|
               klimawandel|erneuerbar|erneuerbaren|klimapolitik|hochwasser|sozialklimagerecht|
               nachhaltigkeit|nachhaltig|flutkatastrophe|klimapolitik|klimaänderung|klimaentwicklung', text))
AfD_Tweets_1E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "AfD")
T_AfD_1E <- paste0(AfD_Tweets_1E$text, collapse = " ")
AfD_Tweets_2E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "AfDimBundestag")
T_AfD_2E <- paste0(AfD_Tweets_2E$text, collapse = " ")
AfD_Tweets_3E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Alice_Weidel")
T_AfD_3E <- paste0(AfD_Tweets_3E$text, collapse = " ")
AfD_Tweets_4E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Tino_Chrupalla")
T_AfD_4E <- paste0(AfD_Tweets_4E$text, collapse = " ")
AfD_Tweets_5E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Beatrix_vStorch")
T_AfD_5E <- paste0(AfD_Tweets_5E$text, collapse = " ")
AfD_Tweets_6E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Frank_Pasemann")
T_AfD_6E <- paste0(AfD_Tweets_6E$text, collapse = " ")
AfD_Tweets_7E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "M_Reichardt_AfD")
T_AfD_7E <- paste0(AfD_Tweets_7E$text, collapse = " ")
AfD_Tweets_8E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Renner_AfD")
T_AfD_8E <- paste0(AfD_Tweets_8E$text, collapse = " ")
AfD_Tweets_9E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "MdB_Lucassen")
T_AfD_9E <- paste0(AfD_Tweets_9E$text, collapse = " ")
AfD_Tweets_10E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Th_Seitz_AfD")
T_AfD_10E <- paste0(AfD_Tweets_10E$text, collapse = " ")
AfD_Tweets_11E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "JuergenBraunAfD")
T_AfD_11E <- paste0(AfD_Tweets_11E$text, collapse = " ")
AfD_Tweets_12E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Leif_Erik_Holm")
T_AfD_12E <- paste0(AfD_Tweets_12E$text, collapse = " ")
AfD_Tweets_13E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "PeterFelser")
T_AfD_13E <- paste0(AfD_Tweets_13E$text, collapse = " ")
AfD_Tweets_14E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Thomas_Ehrhorn")
T_AfD_14E <- paste0(AfD_Tweets_14E$text, collapse = " ")
AfD_Tweets_15E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Paul_Podolay")
T_AfD_15E <- paste0(AfD_Tweets_15E$text, collapse = " ")
AfD_Tweets_16E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "DrHollnagel")
T_AfD_16E <- paste0(AfD_Tweets_16E$text, collapse = " ")
AfD_Tweets_17E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "ulschzi")
T_AfD_17E <- paste0(AfD_Tweets_17E$text, collapse = " ")
AfD_Tweets_18E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Marcus_Buehl")
T_AfD_18E <- paste0(AfD_Tweets_18E$text, collapse = " ")
AfD_Tweets_19E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "WolfgangWiehle")
T_AfD_19E <- paste0(AfD_Tweets_19E$text, collapse = " ")
AfD_Tweets_20E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "EnricoKomning")
T_AfD_20E <- paste0(AfD_Tweets_20E$text, collapse = " ")
AfD_Tweets_20E <- AfD_TweetsE %>%
  filter(AfD_TweetsC$screen_name == "Witt_Uwe")
T_AfD_20E <- paste0(AfD_TweetsE_20$text, collapse = " ")
AfD_Tweets_21E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "andreasbleckmdb")
T_AfD_21E <- paste0(AfD_Tweets_21E$text, collapse = " ")
AfD_Tweets_22E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "DrFriesenMdB")
T_AfD_22E <- paste0(AfD_Tweets_22E$text, collapse = " ")
AfD_Tweets_23E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Gerold_Otten")
T_AfD_23E <- paste0(AfD_Tweets_23E$text, collapse = " ")
AfD_Tweets_24E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "M_HarderKuehnel")
T_AfD_24E <- paste0(AfD_Tweets_24E$text, collapse = " ")
AfD_Tweets_25E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "CorinnaMiazga")
T_AfD_25E <- paste0(AfD_Tweets_25E$text, collapse = " ")
AfD_Tweets_26E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "PetrBystronAfD")
T_AfD_26E <- paste0(AfD_Tweets_26E$text, collapse = " ")
AfD_Tweets_27E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Nicole_Hoechst")
T_AfD_27E <- paste0(AfD_Tweets_27E$text, collapse = " ")
AfD_Tweets_28E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "h_weyel")
T_AfD_28E <- paste0(AfD_Tweets_28E$text, collapse = " ")
AfD_Tweets_29E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "DirkSpaniel")
T_AfD_29E <- paste0(AfD_Tweets_29E$text, collapse = " ")
AfD_Tweets_30E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "SteffenKotre")
T_AfD_30E <- paste0(AfD_Tweets_30E$text, collapse = " ")
AfD_Tweets_31E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "MarcBernhardAfD")
T_AfD_31E <- paste0(AfD_Tweets_31E$text, collapse = " ")
AfD_Tweets_32E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Martin_Hess_AfD")
T_AfD_32E <- paste0(AfD_Tweets_32E$text, collapse = " ")
AfD_Tweets_33E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "AfDProtschka")
T_AfD_33E <- paste0(AfD_Tweets_33E$text, collapse = " ")
AfD_Tweets_34E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Buettner_MdB")
T_AfD_34E <- paste0(AfD_Tweets_34E$text, collapse = " ")
AfD_Tweets_35E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "ChrWirthMdB")
T_AfD_35E <- paste0(AfD_Tweets_35E$text, collapse = " ")
AfD_Tweets_36E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Pohl_MdB")
T_AfD_36E <- paste0(AfD_Tweets_36E$text, collapse = " ")
AfD_Tweets_37E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Rene_Springer")
T_AfD_37E <- paste0(AfD_Tweets_37E$text, collapse = " ")
AfD_Tweets_38E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "R_Hartwig_AfD")
T_AfD_38E <- paste0(AfD_Tweets_38E$text, collapse = " ")
AfD_Tweets_39E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Schneider_AfD")
T_AfD_39E <- paste0(AfD_Tweets_39E$text, collapse = " ")
AfD_Tweets_40E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "ElsnervonGronow")
T_AfD_40E <- paste0(AfD_Tweets_40E$text, collapse = " ")
AfD_Tweets_41E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "Jochen_Haug")
T_AfD_41E <- paste0(AfD_Tweets_41E$text, collapse = " ")
AfD_Tweets_42E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "mrosek1958")
T_AfD_42E <- paste0(AfD_Tweets_42E$text, collapse = " ")
AfD_Tweets_43E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "NKleinwaechter")
T_AfD_43E <- paste0(AfD_Tweets_43E$text, collapse = " ")
AfD_Tweets_44E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "KayGottschalk1")
T_AfD_44E <- paste0(AfD_Tweets_44E$text, collapse = " ")
AfD_Tweets_45E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "GottfriedCurio")
T_AfD_45E <- paste0(AfD_Tweets_45E$text, collapse = " ")
AfD_Tweets_46E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "MalteKaufmann")
T_AfD_46E <- paste0(AfD_Tweets_46E$text, collapse = " ")
AfD_Tweets_47E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "StBrandner")
T_AfD_47E <- paste0(AfD_Tweets_47E$text, collapse = " ")
AfD_Tweets_48E <- AfD_TweetsE %>%
  filter(AfD_TweetsE$screen_name == "JoanaCotar")
T_AfD_48E <- paste0(AfD_Tweets_48E$text, collapse = " ")

T_AfD_TextsE <- c(T_AfD_1E,T_AfD_2E,T_AfD_3E,T_AfD_4E,T_AfD_5E,T_AfD_6E,T_AfD_7E,T_AfD_8E,T_AfD_9E,T_AfD_10E,
                  T_AfD_11E,T_AfD_12E,T_AfD_13E,T_AfD_14E,T_AfD_15E,T_AfD_16E,T_AfD_17E,T_AfD_18E,T_AfD_19E,T_AfD_20E,
                  T_AfD_21E,T_AfD_22E,T_AfD_23E,T_AfD_24E,T_AfD_25E,T_AfD_26E,T_AfD_27E,T_AfD_28E,T_AfD_29E,T_AfD_30E,
                  T_AfD_31E,T_AfD_32E,T_AfD_33E,T_AfD_34E,T_AfD_35E,T_AfD_36E,T_AfD_37E,T_AfD_38E,T_AfD_39E,T_AfD_40E,
                  T_AfD_41E,T_AfD_42E,T_AfD_43E,T_AfD_44E,T_AfD_45E,T_AfD_46E,T_AfD_47E,T_AfD_48E)
T_AfD_TextsE <- data.frame(T_AfD_TextsE)


################# CDU/CSU TEXTS ###########################
##### Overall Tweets
CDU_Tweets = filter(Candidate_Tweets, party_id == "CDU")
length(unique(CDU_Tweets[["screen_name"]])) #87
unique(CDU_Tweets[["screen_name"]])
CDU_Tweets_1 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "cducsubt")
T_CDU_1 <- paste0(CDU_Tweets_1$text, collapse = " ")
CDU_Tweets_2 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "CDU")
T_CDU_2 <- paste0(CDU_Tweets_2$text, collapse = " ")
CDU_Tweets_3 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "csu_bt")
T_CDU_3 <- paste0(CDU_Tweets_3$text, collapse = " ")
CDU_Tweets_4 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "ArminLaschet")
T_CDU_4 <- paste0(CDU_Tweets_4$text, collapse = " ")
CDU_Tweets_5 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "Markus_Soeder")
T_CDU_5 <- paste0(CDU_Tweets_5$text, collapse = " ")
CDU_Tweets_6 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "peteraltmaier")
T_CDU_6 <- paste0(CDU_Tweets_6$text, collapse = " ")
CDU_Tweets_7 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "jensspahn")
T_CDU_7 <- paste0(CDU_Tweets_7$text, collapse = " ")
CDU_Tweets_8 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "_FriedrichMerz")
T_CDU_8 <- paste0(CDU_Tweets_8$text, collapse = " ")
CDU_Tweets_9 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "PaulZiemiak")
T_CDU_9 <- paste0(CDU_Tweets_9$text, collapse = " ")
CDU_Tweets_10 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "n_roettgen")
T_CDU_10 <- paste0(CDU_Tweets_10$text, collapse = " ")
CDU_Tweets_11 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "rbrinkhaus")
T_CDU_11 <- paste0(CDU_Tweets_11$text, collapse = " ")
CDU_Tweets_12 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "DaniLudwigMdB")
T_CDU_12 <- paste0(CDU_Tweets_12$text, collapse = " ")
CDU_Tweets_13 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "hahnflo")
T_CDU_13 <- paste0(CDU_Tweets_13$text, collapse = " ")
CDU_Tweets_14 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "JoWadephul")
T_CDU_14 <- paste0(CDU_Tweets_14$text, collapse = " ")
CDU_Tweets_15 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "DrAndreasNick")
T_CDU_15 <- paste0(CDU_Tweets_15$text, collapse = " ")
CDU_Tweets_16 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "PSchnieder")
T_CDU_16 <- paste0(CDU_Tweets_16$text, collapse = " ")
CDU_Tweets_17 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "HeikeBrehmerMdB")
T_CDU_17 <- paste0(CDU_Tweets_17$text, collapse = " ")
CDU_Tweets_18 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "c_bernstiel")
T_CDU_18 <- paste0(CDU_Tweets_18$text, collapse = " ")
CDU_Tweets_19 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "StephPilsinger")
T_CDU_19 <- paste0(CDU_Tweets_19$text, collapse = " ")
CDU_Tweets_20 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "StefingerMdB")
T_CDU_20 <- paste0(CDU_Tweets_20$text, collapse = " ")
CDU_Tweets_21 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "felixschreiner")
T_CDU_21 <- paste0(CDU_Tweets_21$text, collapse = " ")
CDU_Tweets_22 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "MichaelKuffer")
T_CDU_22 <- paste0(CDU_Tweets_22$text, collapse = " ")
CDU_Tweets_23 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "DerLenzMdB")
T_CDU_23 <- paste0(CDU_Tweets_23$text, collapse = " ")
CDU_Tweets_24 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "RonjaKemmer")
T_CDU_24 <- paste0(CDU_Tweets_24$text, collapse = " ")
CDU_Tweets_25 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "Peter_Beyer")
T_CDU_25 <- paste0(CDU_Tweets_25$text, collapse = " ")
CDU_Tweets_26 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "cdu_schweiger")
T_CDU_26 <- paste0(CDU_Tweets_26$text, collapse = " ")
CDU_Tweets_27 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "KartesMdB")
T_CDU_27 <- paste0(CDU_Tweets_27$text, collapse = " ")
CDU_Tweets_28 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "christophploss")
T_CDU_28 <- paste0(CDU_Tweets_28$text, collapse = " ")
CDU_Tweets_29 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "alexander_throm")
T_CDU_29 <- paste0(CDU_Tweets_29$text, collapse = " ")
CDU_Tweets_30 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "AntjeTillmann")
T_CDU_30 <- paste0(CDU_Tweets_30$text, collapse = " ")
CDU_Tweets_31 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "kdgroehler")
T_CDU_31 <- paste0(CDU_Tweets_31$text, collapse = " ")
CDU_Tweets_32 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "ChialoJoe")
T_CDU_32 <- paste0(CDU_Tweets_32$text, collapse = " ")
CDU_Tweets_33 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "RuedigerKruse")
T_CDU_33 <- paste0(CDU_Tweets_33$text, collapse = " ")
CDU_Tweets_34 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "BjoernSimon")
T_CDU_34 <- paste0(CDU_Tweets_34$text, collapse = " ")
CDU_Tweets_35 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "ninawarken")
T_CDU_35 <- paste0(CDU_Tweets_35$text, collapse = " ")
CDU_Tweets_36 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "groehe")
T_CDU_36 <- paste0(CDU_Tweets_36$text, collapse = " ")
CDU_Tweets_37 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "TKuban96")
T_CDU_37 <- paste0(CDU_Tweets_37$text, collapse = " ")
CDU_Tweets_38 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "thorsten_frei")
T_CDU_38 <- paste0(CDU_Tweets_38$text, collapse = " ")
CDU_Tweets_39 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "MdbWendt")
T_CDU_39 <- paste0(CDU_Tweets_39$text, collapse = " ")
CDU_Tweets_40 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "katrin_staffler")
T_CDU_40 <- paste0(CDU_Tweets_40$text, collapse = " ")
CDU_Tweets_41 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "MBiadaczMdB")
T_CDU_41 <- paste0(CDU_Tweets_41$text, collapse = " ")
CDU_Tweets_42 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "GruebelMdb")
T_CDU_42 <- paste0(CDU_Tweets_42$text, collapse = " ")
CDU_Tweets_43 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "Peter_Stein_CDU")
T_CDU_43 <- paste0(CDU_Tweets_43$text, collapse = " ")
CDU_Tweets_44 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "MarcHenrichmann")
T_CDU_44 <- paste0(CDU_Tweets_44$text, collapse = " ")
CDU_Tweets_45 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "TinoSorge")
T_CDU_45 <- paste0(CDU_Tweets_45$text, collapse = " ")
CDU_Tweets_46 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "mvabercron")
T_CDU_46 <- paste0(CDU_Tweets_46$text, collapse = " ")
CDU_Tweets_47 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "ManderlaGisela")
T_CDU_47 <- paste0(CDU_Tweets_47$text, collapse = " ")
CDU_Tweets_48 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "MaikBeermann")
T_CDU_48 <- paste0(CDU_Tweets_48$text, collapse = " ")
CDU_Tweets_49 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "SylviaPantel")
T_CDU_49 <- paste0(CDU_Tweets_49$text, collapse = " ")
CDU_Tweets_50 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "Thomas_Bareiss")
T_CDU_50 <- paste0(CDU_Tweets_50$text, collapse = " ")
CDU_Tweets_51 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "franksteffel")
T_CDU_51 <- paste0(CDU_Tweets_51$text, collapse = " ")
CDU_Tweets_52 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "MarcusWeinberg")
T_CDU_52 <- paste0(CDU_Tweets_52$text, collapse = " ")
CDU_Tweets_53 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "thomasgebhart")
T_CDU_53 <- paste0(CDU_Tweets_53$text, collapse = " ")
CDU_Tweets_54 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "PatrickSensburg")
T_CDU_54 <- paste0(CDU_Tweets_54$text, collapse = " ")
CDU_Tweets_55 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "SteffenBilger")
T_CDU_55 <- paste0(CDU_Tweets_55$text, collapse = " ")
CDU_Tweets_56 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "JoSteiniger")
T_CDU_56 <- paste0(CDU_Tweets_56$text, collapse = " ")
CDU_Tweets_57 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "Erwin_Rueddel")
T_CDU_57 <- paste0(CDU_Tweets_57$text, collapse = " ")
CDU_Tweets_58 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "TSchipanski")
T_CDU_58 <- paste0(CDU_Tweets_58$text, collapse = " ")
CDU_Tweets_59 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "MechthildHeil")
T_CDU_59 <- paste0(CDU_Tweets_59$text, collapse = " ")
CDU_Tweets_60 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "YvonneMagwas")
T_CDU_60 <- paste0(CDU_Tweets_60$text, collapse = " ")
CDU_Tweets_61 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "mueller_sepp")
T_CDU_61 <- paste0(CDU_Tweets_61$text, collapse = " ")
CDU_Tweets_62 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "gero_storjohann")
T_CDU_62 <- paste0(CDU_Tweets_62$text, collapse = " ")
CDU_Tweets_63 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "TErndl")
T_CDU_63 <- paste0(CDU_Tweets_63$text, collapse = " ")
CDU_Tweets_64 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "fritzfelgentreu")
T_CDU_64 <- paste0(CDU_Tweets_64$text, collapse = " ")
CDU_Tweets_65 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "Kai_Whittaker")
T_CDU_65 <- paste0(CDU_Tweets_65$text, collapse = " ")
CDU_Tweets_66 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "KarstenMoering")
T_CDU_66 <- paste0(CDU_Tweets_66$text, collapse = " ")
CDU_Tweets_67 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "HHirte")
T_CDU_67 <- paste0(CDU_Tweets_67$text, collapse = " ")
CDU_Tweets_68 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "anjaweisgerber")
T_CDU_68 <- paste0(CDU_Tweets_68$text, collapse = " ")
CDU_Tweets_69 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "VolkerUllrich")
T_CDU_69 <- paste0(CDU_Tweets_69$text, collapse = " ")
CDU_Tweets_70 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "smuellermdb")
T_CDU_70 <- paste0(CDU_Tweets_70$text, collapse = " ")
CDU_Tweets_71 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "KLeikert")
T_CDU_71 <- paste0(CDU_Tweets_71$text, collapse = " ")
CDU_Tweets_72 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "SteinekeCDU")
T_CDU_72 <- paste0(CDU_Tweets_72$text, collapse = " ")
CDU_Tweets_73 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "JanaSchimke")
T_CDU_73 <- paste0(CDU_Tweets_73$text, collapse = " ")
CDU_Tweets_74 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "Florian_Ossner")
T_CDU_74 <- paste0(CDU_Tweets_74$text, collapse = " ")
CDU_Tweets_75 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "JM_Luczak")
T_CDU_75 <- paste0(CDU_Tweets_75$text, collapse = " ")
CDU_Tweets_76 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "andreasscheuer")
T_CDU_76 <- paste0(CDU_Tweets_76$text, collapse = " ")
CDU_Tweets_77 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "DoroBaer")
T_CDU_77 <- paste0(CDU_Tweets_77$text, collapse = " ")
CDU_Tweets_78 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "MGrosseBroemer")
T_CDU_78 <- paste0(CDU_Tweets_78$text, collapse = " ")
CDU_Tweets_79 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "juergenhardt")
T_CDU_79 <- paste0(CDU_Tweets_79$text, collapse = " ")
CDU_Tweets_80 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "NadineSchoen")
T_CDU_80 <- paste0(CDU_Tweets_80$text, collapse = " ")
CDU_Tweets_81 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "ChristianHirte")
T_CDU_81 <- paste0(CDU_Tweets_81$text, collapse = " ")
CDU_Tweets_82 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "wanderwitz")
T_CDU_82 <- paste0(CDU_Tweets_82$text, collapse = " ")
CDU_Tweets_83 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "RKiesewetter")
T_CDU_83 <- paste0(CDU_Tweets_83$text, collapse = " ")
CDU_Tweets_84 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "HBraun")
T_CDU_84 <- paste0(CDU_Tweets_84$text, collapse = " ")
CDU_Tweets_85 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "JuliaKloeckner")
T_CDU_85 <- paste0(CDU_Tweets_85$text, collapse = " ")
CDU_Tweets_86 <- CDU_Tweets %>%
  filter(CDU_Tweets$screen_name == "akk")
T_CDU_86 <- paste0(CDU_Tweets_86$text, collapse = " ")

T_CDU_Texts <- c(T_CDU_1,T_CDU_2,T_CDU_3,T_CDU_4,T_CDU_5,T_CDU_6,T_CDU_7,T_CDU_8,T_CDU_9,T_CDU_10,
                 T_CDU_11,T_CDU_12,T_CDU_13,T_CDU_14,T_CDU_15,T_CDU_16,T_CDU_17,T_CDU_18,T_CDU_19,T_CDU_20,
                 T_CDU_21,T_CDU_22,T_CDU_23,T_CDU_24,T_CDU_25,T_CDU_26,T_CDU_27,T_CDU_28,T_CDU_29,T_CDU_30,
                 T_CDU_31,T_CDU_32,T_CDU_33,T_CDU_34,T_CDU_35,T_CDU_36,T_CDU_37,T_CDU_38,T_CDU_39,T_CDU_40,
                 T_CDU_41,T_CDU_42,T_CDU_43,T_CDU_44,T_CDU_45,T_CDU_46,T_CDU_47,T_CDU_48,T_CDU_49,T_CDU_50,
                 T_CDU_51,T_CDU_52,T_CDU_53,T_CDU_54,T_CDU_55,T_CDU_56,T_CDU_57,T_CDU_58,T_CDU_59,T_CDU_60,
                 T_CDU_61,T_CDU_62,T_CDU_63,T_CDU_64,T_CDU_65,T_CDU_66,T_CDU_67,T_CDU_68,T_CDU_69,T_CDU_70,
                 T_CDU_71,T_CDU_72,T_CDU_73,T_CDU_74,T_CDU_75,T_CDU_76,T_CDU_77,T_CDU_78,T_CDU_79,T_CDU_80,
                 T_CDU_81,T_CDU_82,T_CDU_83,T_CDU_84,T_CDU_85,T_CDU_86)
T_CDU_Texts <- data.frame(T_CDU_Texts)


##### Corona Tweets
CDU_TweetsC <- CDU_Tweets %>% filter(grepl('corona|coronavirus|covid-19|sars-cov-2|pandemie|lockdown|
                impfen|impfung|impfstoff|geimpft|ungeimpft|impfdosen|
                biontech|pfizer|astrazeneca|moderna|johnson&johnson|inzidenz|
                mpk|masken|maskenskandal|infektion|infektionsschutzgesetz|impfpflicht', text))

CDU_Tweets_1C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "cducsubt")
T_CDU_1C <- paste0(CDU_Tweets_1C$text, collapse = " ")
CDU_Tweets_2C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "CDU")
T_CDU_2C <- paste0(CDU_Tweets_2C$text, collapse = " ")
CDU_Tweets_3C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "csu_bt")
T_CDU_3C <- paste0(CDU_Tweets_3C$text, collapse = " ")
CDU_Tweets_4C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "ArminLaschet")
T_CDU_4C <- paste0(CDU_Tweets_4C$text, collapse = " ")
CDU_Tweets_5C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "Markus_Soeder")
T_CDU_5C <- paste0(CDU_Tweets_5C$text, collapse = " ")
CDU_Tweets_6C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "peteraltmaier")
T_CDU_6C <- paste0(CDU_Tweets_6C$text, collapse = " ")
CDU_Tweets_7C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "jensspahn")
T_CDU_7C <- paste0(CDU_Tweets_7C$text, collapse = " ")
CDU_Tweets_8C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "_FriedrichMerz")
T_CDU_8C <- paste0(CDU_Tweets_8C$text, collapse = " ")
CDU_Tweets_9C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "PaulZiemiak")
T_CDU_9C <- paste0(CDU_Tweets_9C$text, collapse = " ")
CDU_Tweets_10C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "n_roettgen")
T_CDU_10C <- paste0(CDU_Tweets_10C$text, collapse = " ")
CDU_Tweets_11C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "rbrinkhaus")
T_CDU_11C <- paste0(CDU_Tweets_11C$text, collapse = " ")
CDU_Tweets_12C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "DaniLudwigMdB")
T_CDU_12C <- paste0(CDU_Tweets_12C$text, collapse = " ")
CDU_Tweets_13C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "hahnflo")
T_CDU_13C <- paste0(CDU_Tweets_13C$text, collapse = " ")
CDU_Tweets_14C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "JoWadephul")
T_CDU_14C <- paste0(CDU_Tweets_14C$text, collapse = " ")
CDU_Tweets_15C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "DrAndreasNick")
T_CDU_15C <- paste0(CDU_Tweets_15C$text, collapse = " ")
CDU_Tweets_16C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "PSchnieder")
T_CDU_16C <- paste0(CDU_Tweets_16C$text, collapse = " ")
CDU_Tweets_17C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "HeikeBrehmerMdB")
T_CDU_17C <- paste0(CDU_Tweets_17C$text, collapse = " ")
CDU_Tweets_18C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "c_bernstiel")
T_CDU_18C <- paste0(CDU_Tweets_18C$text, collapse = " ")
CDU_Tweets_19C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "StephPilsinger")
T_CDU_19C <- paste0(CDU_Tweets_19C$text, collapse = " ")
CDU_Tweets_20C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "StefingerMdB")
T_CDU_20C <- paste0(CDU_Tweets_20C$text, collapse = " ")
CDU_Tweets_21C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "felixschreiner")
T_CDU_21C <- paste0(CDU_Tweets_21C$text, collapse = " ")
CDU_Tweets_22C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "MichaelKuffer")
T_CDU_22C <- paste0(CDU_Tweets_22C$text, collapse = " ")
CDU_Tweets_23C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "DerLenzMdB")
T_CDU_23C <- paste0(CDU_Tweets_23C$text, collapse = " ")
CDU_Tweets_24C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "RonjaKemmer")
T_CDU_24C <- paste0(CDU_Tweets_24C$text, collapse = " ")
CDU_Tweets_25C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "Peter_Beyer")
T_CDU_25C <- paste0(CDU_Tweets_25C$text, collapse = " ")
CDU_Tweets_26C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "cdu_schweiger")
T_CDU_26C <- paste0(CDU_Tweets_26C$text, collapse = " ")
CDU_Tweets_27C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "KartesMdB")
T_CDU_27C <- paste0(CDU_Tweets_27C$text, collapse = " ")
CDU_Tweets_28C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "christophploss")
T_CDU_28C <- paste0(CDU_Tweets_28C$text, collapse = " ")
CDU_Tweets_29C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "alexander_throm")
T_CDU_29C <- paste0(CDU_Tweets_29C$text, collapse = " ")
CDU_Tweets_30C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "AntjeTillmann")
T_CDU_30C <- paste0(CDU_Tweets_30C$text, collapse = " ")
CDU_Tweets_31C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "kdgroehler")
T_CDU_31C <- paste0(CDU_Tweets_31C$text, collapse = " ")
CDU_Tweets_32C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "ChialoJoe")
T_CDU_32C <- paste0(CDU_Tweets_32C$text, collapse = " ")
CDU_Tweets_33C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "RuedigerKruse")
T_CDU_33C <- paste0(CDU_Tweets_33C$text, collapse = " ")
CDU_Tweets_34C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "BjoernSimon")
T_CDU_34C <- paste0(CDU_Tweets_34C$text, collapse = " ")
CDU_Tweets_35C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "ninawarken")
T_CDU_35C <- paste0(CDU_Tweets_35C$text, collapse = " ")
CDU_Tweets_36C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "groehe")
T_CDU_36C <- paste0(CDU_Tweets_36C$text, collapse = " ")
CDU_Tweets_37C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "TKuban96")
T_CDU_37C <- paste0(CDU_Tweets_37C$text, collapse = " ")
CDU_Tweets_38C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "thorsten_frei")
T_CDU_38C <- paste0(CDU_Tweets_38C$text, collapse = " ")
CDU_Tweets_39C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "MdbWendt")
T_CDU_39C <- paste0(CDU_Tweets_39C$text, collapse = " ")
CDU_Tweets_40C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "katrin_staffler")
T_CDU_40C <- paste0(CDU_Tweets_40C$text, collapse = " ")
CDU_Tweets_41C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "MBiadaczMdB")
T_CDU_41C <- paste0(CDU_Tweets_41C$text, collapse = " ")
CDU_Tweets_42C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "GruebelMdb")
T_CDU_42C <- paste0(CDU_Tweets_42C$text, collapse = " ")
CDU_Tweets_43C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "Peter_Stein_CDU")
T_CDU_43C <- paste0(CDU_Tweets_43C$text, collapse = " ")
CDU_Tweets_44C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "MarcHenrichmann")
T_CDU_44C <- paste0(CDU_Tweets_44C$text, collapse = " ")
CDU_Tweets_45C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "TinoSorge")
T_CDU_45C <- paste0(CDU_Tweets_45C$text, collapse = " ")
CDU_Tweets_46C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "mvabercron")
T_CDU_46C <- paste0(CDU_Tweets_46C$text, collapse = " ")
CDU_Tweets_47C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "ManderlaGisela")
T_CDU_47C <- paste0(CDU_Tweets_47C$text, collapse = " ")
CDU_Tweets_48C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "MaikBeermann")
T_CDU_48C <- paste0(CDU_Tweets_48C$text, collapse = " ")
CDU_Tweets_49C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "SylviaPantel")
T_CDU_49C <- paste0(CDU_Tweets_49C$text, collapse = " ")
CDU_Tweets_50C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "Thomas_Bareiss")
T_CDU_50C <- paste0(CDU_Tweets_50C$text, collapse = " ")
CDU_Tweets_51C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "franksteffel")
T_CDU_51C <- paste0(CDU_Tweets_51C$text, collapse = " ")
CDU_Tweets_52C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "MarcusWeinberg")
T_CDU_52C <- paste0(CDU_Tweets_52C$text, collapse = " ")
CDU_Tweets_53C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "thomasgebhart")
T_CDU_53C <- paste0(CDU_Tweets_53C$text, collapse = " ")
CDU_Tweets_54C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "PatrickSensburg")
T_CDU_54C <- paste0(CDU_Tweets_54C$text, collapse = " ")
CDU_Tweets_55C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "SteffenBilger")
T_CDU_55C <- paste0(CDU_Tweets_55C$text, collapse = " ")
CDU_Tweets_56C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "JoSteiniger")
T_CDU_56C <- paste0(CDU_Tweets_56C$text, collapse = " ")
CDU_Tweets_57C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "Erwin_Rueddel")
T_CDU_57C <- paste0(CDU_Tweets_57C$text, collapse = " ")
CDU_Tweets_58C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "TSchipanski")
T_CDU_58C <- paste0(CDU_Tweets_58C$text, collapse = " ")
CDU_Tweets_59C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "MechthildHeil")
T_CDU_59C <- paste0(CDU_Tweets_59C$text, collapse = " ")
CDU_Tweets_60C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "YvonneMagwas")
T_CDU_60C <- paste0(CDU_Tweets_60C$text, collapse = " ")
CDU_Tweets_61C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "mueller_sepp")
T_CDU_61C <- paste0(CDU_Tweets_61C$text, collapse = " ")
CDU_Tweets_62C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "gero_storjohann")
T_CDU_62C <- paste0(CDU_Tweets_62C$text, collapse = " ")
CDU_Tweets_63C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "TErndl")
T_CDU_63C <- paste0(CDU_Tweets_63C$text, collapse = " ")
CDU_Tweets_64C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "fritzfelgentreu")
T_CDU_64C <- paste0(CDU_Tweets_64C$text, collapse = " ")
CDU_Tweets_65C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "Kai_Whittaker")
T_CDU_65C <- paste0(CDU_Tweets_65C$text, collapse = " ")
CDU_Tweets_66C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "KarstenMoering")
T_CDU_66C <- paste0(CDU_Tweets_66C$text, collapse = " ")
CDU_Tweets_67C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "HHirte")
T_CDU_67C <- paste0(CDU_Tweets_67C$text, collapse = " ")
CDU_Tweets_68C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "anjaweisgerber")
T_CDU_68C <- paste0(CDU_Tweets_68C$text, collapse = " ")
CDU_Tweets_69C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "VolkerUllrich")
T_CDU_69C <- paste0(CDU_Tweets_69C$text, collapse = " ")
CDU_Tweets_70C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "smuellermdb")
T_CDU_70C <- paste0(CDU_Tweets_70C$text, collapse = " ")
CDU_Tweets_71C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "KLeikert")
T_CDU_71C <- paste0(CDU_Tweets_71C$text, collapse = " ")
CDU_Tweets_72C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "SteinekeCDU")
T_CDU_72C <- paste0(CDU_Tweets_72C$text, collapse = " ")
CDU_Tweets_73C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "JanaSchimke")
T_CDU_73C <- paste0(CDU_Tweets_73C$text, collapse = " ")
CDU_Tweets_74C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "Florian_Ossner")
T_CDU_74C <- paste0(CDU_Tweets_74C$text, collapse = " ")
CDU_Tweets_75C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "JM_Luczak")
T_CDU_75C <- paste0(CDU_Tweets_75C$text, collapse = " ")
CDU_Tweets_76C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "andreasscheuer")
T_CDU_76C <- paste0(CDU_Tweets_76C$text, collapse = " ")
CDU_Tweets_77C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "DoroBaer")
T_CDU_77C <- paste0(CDU_Tweets_77C$text, collapse = " ")
CDU_Tweets_78C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "MGrosseBroemer")
T_CDU_78C <- paste0(CDU_Tweets_78C$text, collapse = " ")
CDU_Tweets_79C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "juergenhardt")
T_CDU_79C <- paste0(CDU_Tweets_79C$text, collapse = " ")
CDU_Tweets_80C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "NadineSchoen")
T_CDU_80C <- paste0(CDU_Tweets_80C$text, collapse = " ")
CDU_Tweets_81C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "ChristianHirte")
T_CDU_81C <- paste0(CDU_Tweets_81C$text, collapse = " ")
CDU_Tweets_82C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "wanderwitz")
T_CDU_82C <- paste0(CDU_Tweets_82C$text, collapse = " ")
CDU_Tweets_83C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "RKiesewetter")
T_CDU_83C <- paste0(CDU_Tweets_83C$text, collapse = " ")
CDU_Tweets_84C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "HBraun")
T_CDU_84C <- paste0(CDU_Tweets_84C$text, collapse = " ")
CDU_Tweets_85C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "JuliaKloeckner")
T_CDU_85C <- paste0(CDU_Tweets_85C$text, collapse = " ")
CDU_Tweets_86C <- CDU_TweetsC %>%
  filter(CDU_TweetsC$screen_name == "akk")
T_CDU_86C <- paste0(CDU_Tweets_86C$text, collapse = " ")

T_CDU_TextsC <- c(T_CDU_1C,T_CDU_2C,T_CDU_3C,T_CDU_4C,T_CDU_5C,T_CDU_6C,T_CDU_7C,T_CDU_8C,T_CDU_9C,T_CDU_10C,
                  T_CDU_11C,T_CDU_12C,T_CDU_13C,T_CDU_14C,T_CDU_15C,T_CDU_16C,T_CDU_17C,T_CDU_18C,T_CDU_19C,T_CDU_20C,
                  T_CDU_21C,T_CDU_22C,T_CDU_23C,T_CDU_24C,T_CDU_25C,T_CDU_26C,T_CDU_27C,T_CDU_28C,T_CDU_29C,T_CDU_30C,
                  T_CDU_31C,T_CDU_32C,T_CDU_33C,T_CDU_34C,T_CDU_35C,T_CDU_36C,T_CDU_37C,T_CDU_38C,T_CDU_39C,T_CDU_40C,
                  T_CDU_41C,T_CDU_42C,T_CDU_43C,T_CDU_44C,T_CDU_45C,T_CDU_46C,T_CDU_47C,T_CDU_48C,T_CDU_49C,T_CDU_50C,
                  T_CDU_51C,T_CDU_52C,T_CDU_53C,T_CDU_54C,T_CDU_55C,T_CDU_56C,T_CDU_57C,T_CDU_58C,T_CDU_59C,T_CDU_60C,
                  T_CDU_61C,T_CDU_62C,T_CDU_63C,T_CDU_64C,T_CDU_65C,T_CDU_66C,T_CDU_67C,T_CDU_68C,T_CDU_69C,T_CDU_70C,
                  T_CDU_71C,T_CDU_72C,T_CDU_73C,T_CDU_74C,T_CDU_75C,T_CDU_76C,T_CDU_77C,T_CDU_78C,T_CDU_79C,T_CDU_80C,
                  T_CDU_81C,T_CDU_82C,T_CDU_83C,T_CDU_84C,T_CDU_85C,T_CDU_86C)
T_CDU_TextsC <- data.frame(T_CDU_TextsC)


##### Environment Tweets
CDU_Unique <- unique(CDU_Tweets[["screen_name"]])
write.csv(CDU_Unique, "CDU_Unique.csv", row.names = FALSE)
CDU_TweetsE <- CDU_Tweets %>% filter(grepl('klimaschutz|klimaschutzistjetzt|klimakrise|kohleausstieg|verkehrswende|
               klimawandel|erneuerbar|erneuerbaren|klimapolitik|hochwasser|sozialklimagerecht|
               nachhaltigkeit|nachhaltig|flutkatastrophe|klimapolitik|klimaänderung|klimaentwicklung', text))

CDU_Tweets_1E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "cducsubt")
CDU_Tweets_2E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "CDU")
CDU_Tweets_3E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "csu_bt")
CDU_Tweets_4E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "ArminLaschet")
CDU_Tweets_5E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "Markus_Soeder")
CDU_Tweets_6E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "peteraltmaier")
CDU_Tweets_7E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "jensspahn")
CDU_Tweets_8E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "_FriedrichMerz")
CDU_Tweets_9E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "PaulZiemiak")
CDU_Tweets_10E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "n_roettgen")
CDU_Tweets_11E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "rbrinkhaus")
CDU_Tweets_12E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "DaniLudwigMdB")
CDU_Tweets_13E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "hahnflo")
CDU_Tweets_14E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "JoWadephul")
CDU_Tweets_15E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "DrAndreasNick")
CDU_Tweets_16E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "PSchnieder")
CDU_Tweets_17E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "HeikeBrehmerMdB")
CDU_Tweets_18E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "c_bernstiel")
CDU_Tweets_19E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "StephPilsinger")
CDU_Tweets_20E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "StefingerMdB")
CDU_Tweets_21E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "felixschreiner")
CDU_Tweets_22E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "MichaelKuffer")
CDU_Tweets_23E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "DerLenzMdB")
CDU_Tweets_24E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "RonjaKemmer")
CDU_Tweets_25E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "Peter_Beyer")
CDU_Tweets_26E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "cdu_schweiger")
CDU_Tweets_27E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "KartesMdB")
CDU_Tweets_28E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "christophploss")
CDU_Tweets_29E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "VriesChristoph")
CDU_Tweets_30E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "alexander_throm")
CDU_Tweets_31E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "AntjeTillmann")
CDU_Tweets_32E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "kdgroehler")
CDU_Tweets_33E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "ChialoJoe")
CDU_Tweets_34E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "RuedigerKruse")
CDU_Tweets_35E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "BjoernSimon")
CDU_Tweets_36E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "ninawarken")
CDU_Tweets_37E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "groehe")
CDU_Tweets_38E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "TKuban96")
CDU_Tweets_39E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "thorsten_frei")
CDU_Tweets_40E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "MdbWendt")
CDU_Tweets_41E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "katrin_staffler")
CDU_Tweets_42E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "MBiadaczMdB")
CDU_Tweets_43E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "GruebelMdb")
CDU_Tweets_44E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "Peter_Stein_CDU")
CDU_Tweets_45E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "MarcHenrichmann")
CDU_Tweets_46E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "TinoSorge")
CDU_Tweets_47E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "mvabercron")
CDU_Tweets_48E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "ManderlaGisela")
CDU_Tweets_49E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "MaikBeermann")
CDU_Tweets_50E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "SylviaPantel")
CDU_Tweets_51E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "Thomas_Bareiss")
CDU_Tweets_52E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "franksteffel")
CDU_Tweets_53E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "MarcusWeinberg")
CDU_Tweets_54E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "thomasgebhart")
CDU_Tweets_55E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "PatrickSensburg")
CDU_Tweets_56E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "SteffenBilger")
CDU_Tweets_57E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "JoSteiniger")
CDU_Tweets_58E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "Erwin_Rueddel")
CDU_Tweets_59E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "TSchipanski")
CDU_Tweets_60E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "MechthildHeil")
CDU_Tweets_61E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "YvonneMagwas")
CDU_Tweets_62E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "mueller_sepp")
CDU_Tweets_63E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "gero_storjohann")
CDU_Tweets_64E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "TErndl")
CDU_Tweets_65E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "fritzfelgentreu")
CDU_Tweets_66E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "Kai_Whittaker")
CDU_Tweets_67E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "KarstenMoering")
CDU_Tweets_68E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "HHirte")
CDU_Tweets_69E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "anjaweisgerber")
CDU_Tweets_70E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "VolkerUllrich")
CDU_Tweets_71E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "smuellermdb")
CDU_Tweets_72E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "KLeikert")
CDU_Tweets_73E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "SteinekeCDU")
CDU_Tweets_74E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "JanaSchimke")
CDU_Tweets_75E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "Florian_Ossner")
CDU_Tweets_76E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "JM_Luczak")
CDU_Tweets_77E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "andreasscheuer")
CDU_Tweets_78E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "DoroBaer")
CDU_Tweets_79E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "MGrosseBroemer")
CDU_Tweets_80E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "juergenhardt")
CDU_Tweets_81E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "NadineSchoen")
CDU_Tweets_82E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "ChristianHirte")
CDU_Tweets_83E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "wanderwitz")
CDU_Tweets_84E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "RKiesewetter")
CDU_Tweets_85E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "HBraun")
CDU_Tweets_86E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "JuliaKloeckner")
CDU_Tweets_87E<-CDU_TweetsE %>% filter(CDU_TweetsE$screen_name == "akk")

T_CDU_1E <- paste0(CDU_Tweets_1E$text, collapse = " ")
T_CDU_2E <- paste0(CDU_Tweets_2E$text, collapse = " ")
T_CDU_3E <- paste0(CDU_Tweets_3E$text, collapse = " ")
T_CDU_4E <- paste0(CDU_Tweets_4E$text, collapse = " ")
T_CDU_5E <- paste0(CDU_Tweets_5E$text, collapse = " ")
T_CDU_6E <- paste0(CDU_Tweets_6E$text, collapse = " ")
T_CDU_7E <- paste0(CDU_Tweets_7E$text, collapse = " ")
T_CDU_8E <- paste0(CDU_Tweets_8E$text, collapse = " ")
T_CDU_9E <- paste0(CDU_Tweets_9E$text, collapse = " ")
T_CDU_10E <- paste0(CDU_Tweets_10E$text, collapse = " ")
T_CDU_11E <- paste0(CDU_Tweets_11E$text, collapse = " ")
T_CDU_12E <- paste0(CDU_Tweets_12E$text, collapse = " ")
T_CDU_13E <- paste0(CDU_Tweets_13E$text, collapse = " ")
T_CDU_14E <- paste0(CDU_Tweets_14E$text, collapse = " ")
T_CDU_15E <- paste0(CDU_Tweets_15E$text, collapse = " ")
T_CDU_16E <- paste0(CDU_Tweets_16E$text, collapse = " ")
T_CDU_17E <- paste0(CDU_Tweets_17E$text, collapse = " ")
T_CDU_18E <- paste0(CDU_Tweets_18E$text, collapse = " ")
T_CDU_19E <- paste0(CDU_Tweets_19E$text, collapse = " ")
T_CDU_20E <- paste0(CDU_Tweets_20E$text, collapse = " ")
T_CDU_21E <- paste0(CDU_Tweets_21E$text, collapse = " ")
T_CDU_22E <- paste0(CDU_Tweets_22E$text, collapse = " ")
T_CDU_23E <- paste0(CDU_Tweets_23E$text, collapse = " ")
T_CDU_24E <- paste0(CDU_Tweets_24E$text, collapse = " ")
T_CDU_25E <- paste0(CDU_Tweets_25E$text, collapse = " ")
T_CDU_26E <- paste0(CDU_Tweets_26E$text, collapse = " ")
T_CDU_27E <- paste0(CDU_Tweets_27E$text, collapse = " ")
T_CDU_28E <- paste0(CDU_Tweets_28E$text, collapse = " ")
T_CDU_29E <- paste0(CDU_Tweets_29E$text, collapse = " ")
T_CDU_30E <- paste0(CDU_Tweets_30E$text, collapse = " ")
T_CDU_31E <- paste0(CDU_Tweets_31E$text, collapse = " ")
T_CDU_32E <- paste0(CDU_Tweets_32E$text, collapse = " ")
T_CDU_33E <- paste0(CDU_Tweets_33E$text, collapse = " ")
T_CDU_34E <- paste0(CDU_Tweets_34E$text, collapse = " ")
T_CDU_35E <- paste0(CDU_Tweets_35E$text, collapse = " ")
T_CDU_36E <- paste0(CDU_Tweets_36E$text, collapse = " ")
T_CDU_37E <- paste0(CDU_Tweets_37E$text, collapse = " ")
T_CDU_38E <- paste0(CDU_Tweets_38E$text, collapse = " ")
T_CDU_39E <- paste0(CDU_Tweets_39E$text, collapse = " ")
T_CDU_40E <- paste0(CDU_Tweets_40E$text, collapse = " ")
T_CDU_41E <- paste0(CDU_Tweets_41E$text, collapse = " ")
T_CDU_42E <- paste0(CDU_Tweets_42E$text, collapse = " ")
T_CDU_43E <- paste0(CDU_Tweets_43E$text, collapse = " ")
T_CDU_44E <- paste0(CDU_Tweets_44E$text, collapse = " ")
T_CDU_45E <- paste0(CDU_Tweets_45E$text, collapse = " ")
T_CDU_46E <- paste0(CDU_Tweets_46E$text, collapse = " ")
T_CDU_47E <- paste0(CDU_Tweets_47E$text, collapse = " ")
T_CDU_48E <- paste0(CDU_Tweets_48E$text, collapse = " ")
T_CDU_49E <- paste0(CDU_Tweets_49E$text, collapse = " ")
T_CDU_50E <- paste0(CDU_Tweets_50E$text, collapse = " ")
T_CDU_51E <- paste0(CDU_Tweets_51E$text, collapse = " ")
T_CDU_52E <- paste0(CDU_Tweets_52E$text, collapse = " ")
T_CDU_53E <- paste0(CDU_Tweets_53E$text, collapse = " ")
T_CDU_54E <- paste0(CDU_Tweets_54E$text, collapse = " ")
T_CDU_55E <- paste0(CDU_Tweets_55E$text, collapse = " ")
T_CDU_56E <- paste0(CDU_Tweets_56E$text, collapse = " ")
T_CDU_57E <- paste0(CDU_Tweets_57E$text, collapse = " ")
T_CDU_58E <- paste0(CDU_Tweets_58E$text, collapse = " ")
T_CDU_59E <- paste0(CDU_Tweets_59E$text, collapse = " ")
T_CDU_60E <- paste0(CDU_Tweets_60E$text, collapse = " ")
T_CDU_61E <- paste0(CDU_Tweets_61E$text, collapse = " ")
T_CDU_62E <- paste0(CDU_Tweets_62E$text, collapse = " ")
T_CDU_63E <- paste0(CDU_Tweets_63E$text, collapse = " ")
T_CDU_64E <- paste0(CDU_Tweets_64E$text, collapse = " ")
T_CDU_65E <- paste0(CDU_Tweets_65E$text, collapse = " ")
T_CDU_66E <- paste0(CDU_Tweets_66E$text, collapse = " ")
T_CDU_67E <- paste0(CDU_Tweets_67E$text, collapse = " ")
T_CDU_68E <- paste0(CDU_Tweets_68E$text, collapse = " ")
T_CDU_69E <- paste0(CDU_Tweets_69E$text, collapse = " ")
T_CDU_70E <- paste0(CDU_Tweets_70E$text, collapse = " ")
T_CDU_71E <- paste0(CDU_Tweets_71E$text, collapse = " ")
T_CDU_72E <- paste0(CDU_Tweets_72E$text, collapse = " ")
T_CDU_73E <- paste0(CDU_Tweets_73E$text, collapse = " ")
T_CDU_74E <- paste0(CDU_Tweets_74E$text, collapse = " ")
T_CDU_75E <- paste0(CDU_Tweets_75E$text, collapse = " ")
T_CDU_76E <- paste0(CDU_Tweets_76E$text, collapse = " ")
T_CDU_77E <- paste0(CDU_Tweets_77E$text, collapse = " ")
T_CDU_78E <- paste0(CDU_Tweets_78E$text, collapse = " ")
T_CDU_79E <- paste0(CDU_Tweets_79E$text, collapse = " ")
T_CDU_80E <- paste0(CDU_Tweets_80E$text, collapse = " ")
T_CDU_81E <- paste0(CDU_Tweets_81E$text, collapse = " ")
T_CDU_82E <- paste0(CDU_Tweets_82E$text, collapse = " ")
T_CDU_83E <- paste0(CDU_Tweets_83E$text, collapse = " ")
T_CDU_84E <- paste0(CDU_Tweets_84E$text, collapse = " ")
T_CDU_85E <- paste0(CDU_Tweets_85E$text, collapse = " ")
T_CDU_86E <- paste0(CDU_Tweets_86E$text, collapse = " ")
T_CDU_87E <- paste0(CDU_Tweets_87E$text, collapse = " ")

T_CDU_TextsE <- c(T_CDU_1E,T_CDU_2E,T_CDU_3E,T_CDU_4E,T_CDU_5E,T_CDU_6E,T_CDU_7E,T_CDU_8E,T_CDU_9E,T_CDU_10E,
                  T_CDU_11E,T_CDU_12E,T_CDU_13E,T_CDU_14E,T_CDU_15E,T_CDU_16E,T_CDU_17E,T_CDU_18E,T_CDU_19E,T_CDU_20E,
                  T_CDU_21E,T_CDU_22E,T_CDU_23E,T_CDU_24E,T_CDU_25E,T_CDU_26E,T_CDU_27E,T_CDU_28E,T_CDU_29E,T_CDU_30E,
                  T_CDU_31E,T_CDU_32E,T_CDU_33E,T_CDU_34E,T_CDU_35E,T_CDU_36E,T_CDU_37E,T_CDU_38E,T_CDU_39E,T_CDU_40E,
                  T_CDU_41E,T_CDU_42E,T_CDU_43E,T_CDU_44E,T_CDU_45E,T_CDU_46E,T_CDU_47E,T_CDU_48E,T_CDU_49E,T_CDU_50E,
                  T_CDU_51E,T_CDU_52E,T_CDU_53E,T_CDU_54E,T_CDU_55E,T_CDU_56E,T_CDU_57E,T_CDU_58E,T_CDU_59E,T_CDU_60E,
                  T_CDU_61E,T_CDU_62E,T_CDU_63E,T_CDU_64E,T_CDU_65E,T_CDU_66E,T_CDU_67E,T_CDU_68E,T_CDU_69E,T_CDU_70E,
                  T_CDU_71E,T_CDU_72E,T_CDU_73E,T_CDU_74E,T_CDU_75E,T_CDU_76E,T_CDU_77E,T_CDU_78E,T_CDU_79E,T_CDU_80E,
                  T_CDU_81E,T_CDU_82E,T_CDU_83E,T_CDU_84E,T_CDU_85E,T_CDU_86E)
T_CDU_TextsE <- data.frame(T_CDU_TextsE)


################# FDP TEXTS ###############################
##### Overall Tweets
FDP_Tweets = filter(Candidate_Tweets, party_id == "FDP")
length(unique(FDP_Tweets[["screen_name"]])) #67
FDP_Unique <- unique(FDP_Tweets[["screen_name"]])
FDP_Tweets_1<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "fdp")
FDP_Tweets_2<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "c_lindner")
FDP_Tweets_3<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "ria_schroeder")
FDP_Tweets_4<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "MAStrackZi")
FDP_Tweets_5<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "KatjaSuding")
FDP_Tweets_6<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "DFoest")
FDP_Tweets_7<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "reuther_bernd")
FDP_Tweets_8<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "_MartinHagen")
FDP_Tweets_9<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "DanielaKluckert")
FDP_Tweets_10<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "BraFDP")
FDP_Tweets_11<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "ManuelHoeferlin")
FDP_Tweets_12<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "MarcusFaber")
FDP_Tweets_13<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "f_schaeffler")
FDP_Tweets_14<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "MdBKlein")
FDP_Tweets_15<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "alexmuellerfdp")
FDP_Tweets_16<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "cad59")
FDP_Tweets_17<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "franksitta")
FDP_Tweets_18<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "theliberalfrank")
FDP_Tweets_19<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "Otto_Fricke")
FDP_Tweets_20<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "DjirSarai")
FDP_Tweets_21<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "florian_toncar")
FDP_Tweets_22<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "christianduerr")
FDP_Tweets_23<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "reinholdmdb")
FDP_Tweets_24<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "Meyer_FDP")
FDP_Tweets_25<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "EUTheurer")
FDP_Tweets_26<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "c_jung77")
FDP_Tweets_27<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "JBrandenburgFDP")
FDP_Tweets_28<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "torstenherbst")
FDP_Tweets_29<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "nicole_ae_bauer")
FDP_Tweets_30<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "bstrasser")
FDP_Tweets_31<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "OlliLuksic")
FDP_Tweets_32<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "johannesvogel")
FDP_Tweets_33<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "Lambsdorff")
FDP_Tweets_34<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "nicolabeerfdp")
FDP_Tweets_35<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "starkwatzinger")
FDP_Tweets_36<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "sandra_weeser")
FDP_Tweets_37<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "koehler_fdp")
FDP_Tweets_38<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "GydeJ")
FDP_Tweets_39<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "krusehamburg")
FDP_Tweets_40<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "LindaTeuteberg")
FDP_Tweets_41<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "MarcoBuschmann")
FDP_Tweets_42<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "noreenthiel")
FDP_Tweets_43<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "KonstantinKuhle")
FDP_Tweets_44<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "HartmutEbbing")
FDP_Tweets_45<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "TillMansmann")
FDP_Tweets_46<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "PeterHeidtFDP")
FDP_Tweets_47<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "RenataAlt_MdB")
FDP_Tweets_48<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "_MartinNeumann")
FDP_Tweets_49<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "michael_g_link")
FDP_Tweets_50<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "StephanThomae")
FDP_Tweets_51<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "BrittaDassler")
FDP_Tweets_52<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "carina_konrad")
FDP_Tweets_53<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "MTodtenhausen")
FDP_Tweets_54<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "HoubenReinhard")
FDP_Tweets_55<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "aggelidis_fdp")
FDP_Tweets_56<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "KatrinHelling")
FDP_Tweets_57<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "hacker_fdp")
FDP_Tweets_58<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "fdp_hessel")
FDP_Tweets_59<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "HoffmannForest")
FDP_Tweets_60<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "Matthiasnoelke")
FDP_Tweets_61<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "jensbeeck")
FDP_Tweets_62<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "HerbrandMarkus")
FDP_Tweets_63<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "PascalKober")
FDP_Tweets_64<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "ullaihnen")
FDP_Tweets_65<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "mseesternpauly")
FDP_Tweets_66<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "muellerboehm")
FDP_Tweets_67<- FDP_Tweets %>% filter(FDP_Tweets$screen_name == "k_willkomm")
T_FDP_1<- paste0(FDP_Tweets_1$text, collapse = " ")
T_FDP_2<- paste0(FDP_Tweets_2$text, collapse = " ")
T_FDP_3<- paste0(FDP_Tweets_3$text, collapse = " ")
T_FDP_4<- paste0(FDP_Tweets_4$text, collapse = " ")
T_FDP_5<- paste0(FDP_Tweets_5$text, collapse = " ")
T_FDP_6<- paste0(FDP_Tweets_6$text, collapse = " ")
T_FDP_7<- paste0(FDP_Tweets_7$text, collapse = " ")
T_FDP_8<- paste0(FDP_Tweets_8$text, collapse = " ")
T_FDP_9<- paste0(FDP_Tweets_9$text, collapse = " ")
T_FDP_10<- paste0(FDP_Tweets_10$text, collapse = " ")
T_FDP_11<- paste0(FDP_Tweets_11$text, collapse = " ")
T_FDP_12<- paste0(FDP_Tweets_12$text, collapse = " ")
T_FDP_13<- paste0(FDP_Tweets_13$text, collapse = " ")
T_FDP_14<- paste0(FDP_Tweets_14$text, collapse = " ")
T_FDP_15<- paste0(FDP_Tweets_15$text, collapse = " ")
T_FDP_16<- paste0(FDP_Tweets_16$text, collapse = " ")
T_FDP_17<- paste0(FDP_Tweets_17$text, collapse = " ")
T_FDP_18<- paste0(FDP_Tweets_18$text, collapse = " ")
T_FDP_19<- paste0(FDP_Tweets_19$text, collapse = " ")
T_FDP_20<- paste0(FDP_Tweets_20$text, collapse = " ")
T_FDP_21<- paste0(FDP_Tweets_21$text, collapse = " ")
T_FDP_22<- paste0(FDP_Tweets_22$text, collapse = " ")
T_FDP_23<- paste0(FDP_Tweets_23$text, collapse = " ")
T_FDP_24<- paste0(FDP_Tweets_24$text, collapse = " ")
T_FDP_25<- paste0(FDP_Tweets_25$text, collapse = " ")
T_FDP_26<- paste0(FDP_Tweets_26$text, collapse = " ")
T_FDP_27<- paste0(FDP_Tweets_27$text, collapse = " ")
T_FDP_28<- paste0(FDP_Tweets_28$text, collapse = " ")
T_FDP_29<- paste0(FDP_Tweets_29$text, collapse = " ")
T_FDP_30<- paste0(FDP_Tweets_30$text, collapse = " ")
T_FDP_31<- paste0(FDP_Tweets_31$text, collapse = " ")
T_FDP_32<- paste0(FDP_Tweets_32$text, collapse = " ")
T_FDP_33<- paste0(FDP_Tweets_33$text, collapse = " ")
T_FDP_34<- paste0(FDP_Tweets_34$text, collapse = " ")
T_FDP_35<- paste0(FDP_Tweets_35$text, collapse = " ")
T_FDP_36<- paste0(FDP_Tweets_36$text, collapse = " ")
T_FDP_37<- paste0(FDP_Tweets_37$text, collapse = " ")
T_FDP_38<- paste0(FDP_Tweets_38$text, collapse = " ")
T_FDP_39<- paste0(FDP_Tweets_39$text, collapse = " ")
T_FDP_40<- paste0(FDP_Tweets_40$text, collapse = " ")
T_FDP_41<- paste0(FDP_Tweets_41$text, collapse = " ")
T_FDP_42<- paste0(FDP_Tweets_42$text, collapse = " ")
T_FDP_43<- paste0(FDP_Tweets_43$text, collapse = " ")
T_FDP_44<- paste0(FDP_Tweets_44$text, collapse = " ")
T_FDP_45<- paste0(FDP_Tweets_45$text, collapse = " ")
T_FDP_46<- paste0(FDP_Tweets_46$text, collapse = " ")
T_FDP_47<- paste0(FDP_Tweets_47$text, collapse = " ")
T_FDP_48<- paste0(FDP_Tweets_48$text, collapse = " ")
T_FDP_49<- paste0(FDP_Tweets_49$text, collapse = " ")
T_FDP_50<- paste0(FDP_Tweets_50$text, collapse = " ")
T_FDP_51<- paste0(FDP_Tweets_51$text, collapse = " ")
T_FDP_52<- paste0(FDP_Tweets_52$text, collapse = " ")
T_FDP_53<- paste0(FDP_Tweets_53$text, collapse = " ")
T_FDP_54<- paste0(FDP_Tweets_54$text, collapse = " ")
T_FDP_55<- paste0(FDP_Tweets_55$text, collapse = " ")
T_FDP_56<- paste0(FDP_Tweets_56$text, collapse = " ")
T_FDP_57<- paste0(FDP_Tweets_57$text, collapse = " ")
T_FDP_58<- paste0(FDP_Tweets_58$text, collapse = " ")
T_FDP_59<- paste0(FDP_Tweets_59$text, collapse = " ")
T_FDP_60<- paste0(FDP_Tweets_60$text, collapse = " ")
T_FDP_61<- paste0(FDP_Tweets_61$text, collapse = " ")
T_FDP_62<- paste0(FDP_Tweets_62$text, collapse = " ")
T_FDP_63<- paste0(FDP_Tweets_63$text, collapse = " ")
T_FDP_64<- paste0(FDP_Tweets_64$text, collapse = " ")
T_FDP_65<- paste0(FDP_Tweets_65$text, collapse = " ")
T_FDP_66<- paste0(FDP_Tweets_66$text, collapse = " ")
T_FDP_67<- paste0(FDP_Tweets_67$text, collapse = " ")

T_FDP_Texts <- c(T_FDP_1,T_FDP_2,T_FDP_3,T_FDP_4,T_FDP_5,T_FDP_6,T_FDP_7,T_FDP_8,T_FDP_9,T_FDP_10,
                 T_FDP_11,T_FDP_12,T_FDP_13,T_FDP_14,T_FDP_15,T_FDP_16,T_FDP_17,T_FDP_18,T_FDP_19,T_FDP_20,
                 T_FDP_21,T_FDP_22,T_FDP_23,T_FDP_24,T_FDP_25,T_FDP_26,T_FDP_27,T_FDP_28,T_FDP_29,T_FDP_30,
                 T_FDP_31,T_FDP_32,T_FDP_33,T_FDP_34,T_FDP_35,T_FDP_36,T_FDP_37,T_FDP_38,T_FDP_39,T_FDP_40,
                 T_FDP_41,T_FDP_42,T_FDP_43,T_FDP_44,T_FDP_45,T_FDP_46,T_FDP_47,T_FDP_48,T_FDP_49,T_FDP_50,
                 T_FDP_51,T_FDP_52,T_FDP_53,T_FDP_54,T_FDP_55,T_FDP_56,T_FDP_57,T_FDP_58,T_FDP_59,T_FDP_60,
                 T_FDP_61,T_FDP_62,T_FDP_63,T_FDP_64,T_FDP_65,T_FDP_66,T_FDP_67)
T_FDP_Texts <- data.frame(T_FDP_Texts)


##### Corona Tweets
FDP_TweetsC <- FDP_Tweets %>% filter(grepl('corona|coronavirus|covid-19|sars-cov-2|pandemie|lockdown|
                impfen|impfung|impfstoff|geimpft|ungeimpft|impfdosen|
                biontech|pfizer|astrazeneca|moderna|johnson&johnson|inzidenz|
                mpk|masken|maskenskandal|infektion|infektionsschutzgesetz|impfpflicht', text))
FDP_Tweets_1C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "fdp")
FDP_Tweets_2C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "c_lindner")
FDP_Tweets_3C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "ria_schroeder")
FDP_Tweets_4C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "MAStrackZi")
FDP_Tweets_5C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "KatjaSuding")
FDP_Tweets_6C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "DFoest")
FDP_Tweets_7C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "reuther_bernd")
FDP_Tweets_8C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "_MartinHagen")
FDP_Tweets_9C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "DanielaKluckert")
FDP_Tweets_10C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "BraFDP")
FDP_Tweets_11C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "ManuelHoeferlin")
FDP_Tweets_12C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "MarcusFaber")
FDP_Tweets_13C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "f_schaeffler")
FDP_Tweets_14C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "MdBKlein")
FDP_Tweets_15C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "alexmuellerfdp")
FDP_Tweets_16C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "cad59")
FDP_Tweets_17C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "franksitta")
FDP_Tweets_18C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "theliberalfrank")
FDP_Tweets_19C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "Otto_Fricke")
FDP_Tweets_20C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "DjirSarai")
FDP_Tweets_21C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "florian_toncar")
FDP_Tweets_22C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "christianduerr")
FDP_Tweets_23C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "reinholdmdb")
FDP_Tweets_24C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "Meyer_FDP")
FDP_Tweets_25C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "EUTheurer")
FDP_Tweets_26C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "c_jung77")
FDP_Tweets_27C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "JBrandenburgFDP")
FDP_Tweets_28C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "torstenherbst")
FDP_Tweets_29C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "nicole_ae_bauer")
FDP_Tweets_30C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "bstrasser")
FDP_Tweets_31C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "OlliLuksic")
FDP_Tweets_32C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "johannesvogel")
FDP_Tweets_33C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "Lambsdorff")
FDP_Tweets_34C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "nicolabeerfdp")
FDP_Tweets_35C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "starkwatzinger")
FDP_Tweets_36C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "sandra_weeser")
FDP_Tweets_37C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "koehler_fdp")
FDP_Tweets_38C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "GydeJ")
FDP_Tweets_39C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "krusehamburg")
FDP_Tweets_40C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "LindaTeuteberg")
FDP_Tweets_41C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "MarcoBuschmann")
FDP_Tweets_42C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "noreenthiel")
FDP_Tweets_43C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "KonstantinKuhle")
FDP_Tweets_44C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "HartmutEbbing")
FDP_Tweets_45C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "TillMansmann")
FDP_Tweets_46C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "PeterHeidtFDP")
FDP_Tweets_47C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "RenataAlt_MdB")
FDP_Tweets_48C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "_MartinNeumann")
FDP_Tweets_49C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "michael_g_link")
FDP_Tweets_50C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "StephanThomae")
FDP_Tweets_51C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "BrittaDassler")
FDP_Tweets_52C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "carina_konrad")
FDP_Tweets_53C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "MTodtenhausen")
FDP_Tweets_54C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "HoubenReinhard")
FDP_Tweets_55C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "aggelidis_fdp")
FDP_Tweets_56C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "KatrinHelling")
FDP_Tweets_57C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "hacker_fdp")
FDP_Tweets_58C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "fdp_hessel")
FDP_Tweets_59C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "HoffmannForest")
FDP_Tweets_60C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "Matthiasnoelke")
FDP_Tweets_61C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "jensbeeck")
FDP_Tweets_62C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "HerbrandMarkus")
FDP_Tweets_63C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "PascalKober")
FDP_Tweets_64C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "ullaihnen")
FDP_Tweets_65C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "mseesternpauly")
FDP_Tweets_66C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "muellerboehm")
FDP_Tweets_67C<- FDP_TweetsC %>% filter(FDP_TweetsC$screen_name == "k_willkomm")
T_FDP_1C <- paste0(FDP_Tweets_1C$text, collapse = " ")
T_FDP_2C <- paste0(FDP_Tweets_2C$text, collapse = " ")
T_FDP_3C <- paste0(FDP_Tweets_3C$text, collapse = " ")
T_FDP_4C <- paste0(FDP_Tweets_4C$text, collapse = " ")
T_FDP_5C <- paste0(FDP_Tweets_5C$text, collapse = " ")
T_FDP_6C <- paste0(FDP_Tweets_6C$text, collapse = " ")
T_FDP_7C <- paste0(FDP_Tweets_7C$text, collapse = " ")
T_FDP_8C <- paste0(FDP_Tweets_8C$text, collapse = " ")
T_FDP_9C <- paste0(FDP_Tweets_9C$text, collapse = " ")
T_FDP_10C <- paste0(FDP_Tweets_10C$text, collapse = " ")
T_FDP_11C <- paste0(FDP_Tweets_11C$text, collapse = " ")
T_FDP_12C <- paste0(FDP_Tweets_12C$text, collapse = " ")
T_FDP_13C <- paste0(FDP_Tweets_13C$text, collapse = " ")
T_FDP_14C <- paste0(FDP_Tweets_14C$text, collapse = " ")
T_FDP_15C <- paste0(FDP_Tweets_15C$text, collapse = " ")
T_FDP_16C <- paste0(FDP_Tweets_16C$text, collapse = " ")
T_FDP_17C <- paste0(FDP_Tweets_17C$text, collapse = " ")
T_FDP_18C <- paste0(FDP_Tweets_18C$text, collapse = " ")
T_FDP_19C <- paste0(FDP_Tweets_19C$text, collapse = " ")
T_FDP_20C <- paste0(FDP_Tweets_20C$text, collapse = " ")
T_FDP_21C <- paste0(FDP_Tweets_21C$text, collapse = " ")
T_FDP_22C <- paste0(FDP_Tweets_22C$text, collapse = " ")
T_FDP_23C <- paste0(FDP_Tweets_23C$text, collapse = " ")
T_FDP_24C <- paste0(FDP_Tweets_24C$text, collapse = " ")
T_FDP_25C <- paste0(FDP_Tweets_25C$text, collapse = " ")
T_FDP_26C <- paste0(FDP_Tweets_26C$text, collapse = " ")
T_FDP_27C <- paste0(FDP_Tweets_27C$text, collapse = " ")
T_FDP_28C <- paste0(FDP_Tweets_28C$text, collapse = " ")
T_FDP_29C <- paste0(FDP_Tweets_29C$text, collapse = " ")
T_FDP_30C <- paste0(FDP_Tweets_30C$text, collapse = " ")
T_FDP_31C <- paste0(FDP_Tweets_31C$text, collapse = " ")
T_FDP_32C <- paste0(FDP_Tweets_32C$text, collapse = " ")
T_FDP_33C <- paste0(FDP_Tweets_33C$text, collapse = " ")
T_FDP_34C <- paste0(FDP_Tweets_34C$text, collapse = " ")
T_FDP_35C <- paste0(FDP_Tweets_35C$text, collapse = " ")
T_FDP_36C <- paste0(FDP_Tweets_36C$text, collapse = " ")
T_FDP_37C <- paste0(FDP_Tweets_37C$text, collapse = " ")
T_FDP_38C <- paste0(FDP_Tweets_38C$text, collapse = " ")
T_FDP_39C <- paste0(FDP_Tweets_39C$text, collapse = " ")
T_FDP_40C <- paste0(FDP_Tweets_40C$text, collapse = " ")
T_FDP_41C <- paste0(FDP_Tweets_41C$text, collapse = " ")
T_FDP_42C <- paste0(FDP_Tweets_42C$text, collapse = " ")
T_FDP_43C <- paste0(FDP_Tweets_43C$text, collapse = " ")
T_FDP_44C <- paste0(FDP_Tweets_44C$text, collapse = " ")
T_FDP_45C <- paste0(FDP_Tweets_45C$text, collapse = " ")
T_FDP_46C <- paste0(FDP_Tweets_46C$text, collapse = " ")
T_FDP_47C <- paste0(FDP_Tweets_47C$text, collapse = " ")
T_FDP_48C <- paste0(FDP_Tweets_48C$text, collapse = " ")
T_FDP_49C <- paste0(FDP_Tweets_49C$text, collapse = " ")
T_FDP_50C <- paste0(FDP_Tweets_50C$text, collapse = " ")
T_FDP_51C <- paste0(FDP_Tweets_51C$text, collapse = " ")
T_FDP_52C <- paste0(FDP_Tweets_52C$text, collapse = " ")
T_FDP_53C <- paste0(FDP_Tweets_53C$text, collapse = " ")
T_FDP_54C <- paste0(FDP_Tweets_54C$text, collapse = " ")
T_FDP_55C <- paste0(FDP_Tweets_55C$text, collapse = " ")
T_FDP_56C <- paste0(FDP_Tweets_56C$text, collapse = " ")
T_FDP_57C <- paste0(FDP_Tweets_57C$text, collapse = " ")
T_FDP_58C <- paste0(FDP_Tweets_58C$text, collapse = " ")
T_FDP_59C <- paste0(FDP_Tweets_59C$text, collapse = " ")
T_FDP_60C <- paste0(FDP_Tweets_60C$text, collapse = " ")
T_FDP_61C <- paste0(FDP_Tweets_61C$text, collapse = " ")
T_FDP_62C <- paste0(FDP_Tweets_62C$text, collapse = " ")
T_FDP_63C <- paste0(FDP_Tweets_63C$text, collapse = " ")
T_FDP_64C <- paste0(FDP_Tweets_64C$text, collapse = " ")
T_FDP_65C <- paste0(FDP_Tweets_65C$text, collapse = " ")
T_FDP_66C <- paste0(FDP_Tweets_66C$text, collapse = " ")
T_FDP_67C <- paste0(FDP_Tweets_67C$text, collapse = " ")

T_FDP_TextsC <- c(T_FDP_1C,T_FDP_2C,T_FDP_3C,T_FDP_4C,T_FDP_5C,T_FDP_6C,T_FDP_7C,T_FDP_8C,T_FDP_9C,T_FDP_10C,
                  T_FDP_11C,T_FDP_12C,T_FDP_13C,T_FDP_14C,T_FDP_15C,T_FDP_16C,T_FDP_17C,T_FDP_18C,T_FDP_19C,T_FDP_20C,
                  T_FDP_21C,T_FDP_22C,T_FDP_23C,T_FDP_24C,T_FDP_25C,T_FDP_26C,T_FDP_27C,T_FDP_28C,T_FDP_29C,T_FDP_30C,
                  T_FDP_31C,T_FDP_32C,T_FDP_33C,T_FDP_34C,T_FDP_35C,T_FDP_36C,T_FDP_37C,T_FDP_38C,T_FDP_39C,T_FDP_40C,
                  T_FDP_41C,T_FDP_42C,T_FDP_43C,T_FDP_44C,T_FDP_45C,T_FDP_46C,T_FDP_47C,T_FDP_48C,T_FDP_49C,T_FDP_50C,
                  T_FDP_51C,T_FDP_52C,T_FDP_53C,T_FDP_54C,T_FDP_55C,T_FDP_56C,T_FDP_57C,T_FDP_58C,T_FDP_59C,T_FDP_60C,
                  T_FDP_61C,T_FDP_62C,T_FDP_63C,T_FDP_64C,T_FDP_65C,T_FDP_66C,T_FDP_67C)
T_FDP_TextsC <- data.frame(T_FDP_TextsC)


##### Environment Tweets
FDP_TweetsE <- FDP_Tweets %>% filter(grepl('klimaschutz|klimaschutzistjetzt|klimakrise|kohleausstieg|verkehrswende|
               klimawandel|erneuerbar|erneuerbaren|klimapolitik|hochwasser|sozialklimagerecht|
               nachhaltigkeit|nachhaltig|flutkatastrophe|klimapolitik|klimaänderung|klimaentwicklung', text))

FDP_Tweets_1E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "fdp")
FDP_Tweets_2E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "c_lindner")
FDP_Tweets_3E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "ria_schroeder")
FDP_Tweets_4E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "MAStrackZi")
FDP_Tweets_5E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "KatjaSuding")
FDP_Tweets_6E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "DFoest")
FDP_Tweets_7E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "reuther_bernd")
FDP_Tweets_8E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "_MartinHagen")
FDP_Tweets_9E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "DanielaKluckert")
FDP_Tweets_10E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "BraFDP")
FDP_Tweets_11E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "ManuelHoeferlin")
FDP_Tweets_12E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "MarcusFaber")
FDP_Tweets_13E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "f_schaeffler")
FDP_Tweets_14E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "MdBKlein")
FDP_Tweets_15E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "alexmuellerfdp")
FDP_Tweets_16E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "cad59")
FDP_Tweets_17E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "franksitta")
FDP_Tweets_18E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "theliberalfrank")
FDP_Tweets_19E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "Otto_Fricke")
FDP_Tweets_20E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "DjirSarai")
FDP_Tweets_21E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "florian_toncar")
FDP_Tweets_22E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "christianduerr")
FDP_Tweets_23E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "reinholdmdb")
FDP_Tweets_24E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "Meyer_FDP")
FDP_Tweets_25E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "EUTheurer")
FDP_Tweets_26E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "c_jung77")
FDP_Tweets_27E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "JBrandenburgFDP")
FDP_Tweets_28E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "torstenherbst")
FDP_Tweets_29E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "nicole_ae_bauer")
FDP_Tweets_30E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "bstrasser")
FDP_Tweets_31E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "OlliLuksic")
FDP_Tweets_32E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "johannesvogel")
FDP_Tweets_33E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "Lambsdorff")
FDP_Tweets_34E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "nicolabeerfdp")
FDP_Tweets_35E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "starkwatzinger")
FDP_Tweets_36E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "sandra_weeser")
FDP_Tweets_37E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "koehler_fdp")
FDP_Tweets_38E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "GydeJ")
FDP_Tweets_39E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "krusehamburg")
FDP_Tweets_40E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "LindaTeuteberg")
FDP_Tweets_41E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "MarcoBuschmann")
FDP_Tweets_42E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "noreenthiel")
FDP_Tweets_43E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "KonstantinKuhle")
FDP_Tweets_44E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "HartmutEbbing")
FDP_Tweets_45E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "TillMansmann")
FDP_Tweets_46E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "PeterHeidtFDP")
FDP_Tweets_47E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "RenataAlt_MdB")
FDP_Tweets_48E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "_MartinNeumann")
FDP_Tweets_49E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "michael_g_link")
FDP_Tweets_50E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "StephanThomae")
FDP_Tweets_51E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "BrittaDassler")
FDP_Tweets_52E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "carina_konrad")
FDP_Tweets_53E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "MTodtenhausen")
FDP_Tweets_54E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "HoubenReinhard")
FDP_Tweets_55E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "aggelidis_fdp")
FDP_Tweets_56E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "KatrinHelling")
FDP_Tweets_57E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "hacker_fdp")
FDP_Tweets_58E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "fdp_hessel")
FDP_Tweets_59E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "HoffmannForest")
FDP_Tweets_60E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "Matthiasnoelke")
FDP_Tweets_61E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "jensbeeck")
FDP_Tweets_62E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "HerbrandMarkus")
FDP_Tweets_63E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "PascalKober")
FDP_Tweets_64E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "ullaihnen")
FDP_Tweets_65E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "mseesternpauly")
FDP_Tweets_66E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "muellerboehm")
FDP_Tweets_67E<- FDP_TweetsE %>% filter(FDP_TweetsE$screen_name == "k_willkomm")
T_FDP_1E <- paste0(FDP_Tweets_1E$text, collapse = " ")
T_FDP_2E <- paste0(FDP_Tweets_2E$text, collapse = " ")
T_FDP_3E <- paste0(FDP_Tweets_3E$text, collapse = " ")
T_FDP_4E <- paste0(FDP_Tweets_4E$text, collapse = " ")
T_FDP_5E <- paste0(FDP_Tweets_5E$text, collapse = " ")
T_FDP_6E <- paste0(FDP_Tweets_6E$text, collapse = " ")
T_FDP_7E <- paste0(FDP_Tweets_7E$text, collapse = " ")
T_FDP_8E <- paste0(FDP_Tweets_8E$text, collapse = " ")
T_FDP_9E <- paste0(FDP_Tweets_9E$text, collapse = " ")
T_FDP_10E <- paste0(FDP_Tweets_10E$text, collapse = " ")
T_FDP_11E <- paste0(FDP_Tweets_11E$text, collapse = " ")
T_FDP_12E <- paste0(FDP_Tweets_12E$text, collapse = " ")
T_FDP_13E <- paste0(FDP_Tweets_13E$text, collapse = " ")
T_FDP_14E <- paste0(FDP_Tweets_14E$text, collapse = " ")
T_FDP_15E <- paste0(FDP_Tweets_15E$text, collapse = " ")
T_FDP_16E <- paste0(FDP_Tweets_16E$text, collapse = " ")
T_FDP_17E <- paste0(FDP_Tweets_17E$text, collapse = " ")
T_FDP_18E <- paste0(FDP_Tweets_18E$text, collapse = " ")
T_FDP_19E <- paste0(FDP_Tweets_19E$text, collapse = " ")
T_FDP_20E <- paste0(FDP_Tweets_20E$text, collapse = " ")
T_FDP_21E <- paste0(FDP_Tweets_21E$text, collapse = " ")
T_FDP_22E <- paste0(FDP_Tweets_22E$text, collapse = " ")
T_FDP_23E <- paste0(FDP_Tweets_23E$text, collapse = " ")
T_FDP_24E <- paste0(FDP_Tweets_24E$text, collapse = " ")
T_FDP_25E <- paste0(FDP_Tweets_25E$text, collapse = " ")
T_FDP_26E <- paste0(FDP_Tweets_26E$text, collapse = " ")
T_FDP_27E <- paste0(FDP_Tweets_27E$text, collapse = " ")
T_FDP_28E <- paste0(FDP_Tweets_28E$text, collapse = " ")
T_FDP_29E <- paste0(FDP_Tweets_29E$text, collapse = " ")
T_FDP_30E <- paste0(FDP_Tweets_30E$text, collapse = " ")
T_FDP_31E <- paste0(FDP_Tweets_31E$text, collapse = " ")
T_FDP_32E <- paste0(FDP_Tweets_32E$text, collapse = " ")
T_FDP_33E <- paste0(FDP_Tweets_33E$text, collapse = " ")
T_FDP_34E <- paste0(FDP_Tweets_34E$text, collapse = " ")
T_FDP_35E <- paste0(FDP_Tweets_35E$text, collapse = " ")
T_FDP_36E <- paste0(FDP_Tweets_36E$text, collapse = " ")
T_FDP_37E <- paste0(FDP_Tweets_37E$text, collapse = " ")
T_FDP_38E <- paste0(FDP_Tweets_38E$text, collapse = " ")
T_FDP_39E <- paste0(FDP_Tweets_39E$text, collapse = " ")
T_FDP_40E <- paste0(FDP_Tweets_40E$text, collapse = " ")
T_FDP_41E <- paste0(FDP_Tweets_41E$text, collapse = " ")
T_FDP_42E <- paste0(FDP_Tweets_42E$text, collapse = " ")
T_FDP_43E <- paste0(FDP_Tweets_43E$text, collapse = " ")
T_FDP_44E <- paste0(FDP_Tweets_44E$text, collapse = " ")
T_FDP_45E <- paste0(FDP_Tweets_45E$text, collapse = " ")
T_FDP_46E <- paste0(FDP_Tweets_46E$text, collapse = " ")
T_FDP_47E <- paste0(FDP_Tweets_47E$text, collapse = " ")
T_FDP_48E <- paste0(FDP_Tweets_48E$text, collapse = " ")
T_FDP_49E <- paste0(FDP_Tweets_49E$text, collapse = " ")
T_FDP_50E <- paste0(FDP_Tweets_50E$text, collapse = " ")
T_FDP_51E <- paste0(FDP_Tweets_51E$text, collapse = " ")
T_FDP_52E <- paste0(FDP_Tweets_52E$text, collapse = " ")
T_FDP_53E <- paste0(FDP_Tweets_53E$text, collapse = " ")
T_FDP_54E <- paste0(FDP_Tweets_54E$text, collapse = " ")
T_FDP_55E <- paste0(FDP_Tweets_55E$text, collapse = " ")
T_FDP_56E <- paste0(FDP_Tweets_56E$text, collapse = " ")
T_FDP_57E <- paste0(FDP_Tweets_57E$text, collapse = " ")
T_FDP_58E <- paste0(FDP_Tweets_58E$text, collapse = " ")
T_FDP_59E <- paste0(FDP_Tweets_59E$text, collapse = " ")
T_FDP_60E <- paste0(FDP_Tweets_60E$text, collapse = " ")
T_FDP_61E <- paste0(FDP_Tweets_61E$text, collapse = " ")
T_FDP_62E <- paste0(FDP_Tweets_62E$text, collapse = " ")
T_FDP_63E <- paste0(FDP_Tweets_63E$text, collapse = " ")
T_FDP_64E <- paste0(FDP_Tweets_64E$text, collapse = " ")
T_FDP_65E <- paste0(FDP_Tweets_65E$text, collapse = " ")
T_FDP_66E <- paste0(FDP_Tweets_66E$text, collapse = " ")
T_FDP_67E <- paste0(FDP_Tweets_67E$text, collapse = " ")

T_FDP_TextsE <- c(T_FDP_1E,T_FDP_2E,T_FDP_3E,T_FDP_4E,T_FDP_5E,T_FDP_6E,T_FDP_7E,T_FDP_8E,T_FDP_9E,T_FDP_10E,
                  T_FDP_11E,T_FDP_12E,T_FDP_13E,T_FDP_14E,T_FDP_15E,T_FDP_16E,T_FDP_17E,T_FDP_18E,T_FDP_19E,T_FDP_20E,
                  T_FDP_21E,T_FDP_22E,T_FDP_23E,T_FDP_24E,T_FDP_25E,T_FDP_26E,T_FDP_27E,T_FDP_28E,T_FDP_29E,T_FDP_30E,
                  T_FDP_31E,T_FDP_32E,T_FDP_33E,T_FDP_34E,T_FDP_35E,T_FDP_36E,T_FDP_37E,T_FDP_38E,T_FDP_39E,T_FDP_40E,
                  T_FDP_41E,T_FDP_42E,T_FDP_43E,T_FDP_44E,T_FDP_45E,T_FDP_46E,T_FDP_47E,T_FDP_48E,T_FDP_49E,T_FDP_50E,
                  T_FDP_51E,T_FDP_52E,T_FDP_53E,T_FDP_54E,T_FDP_55E,T_FDP_56E,T_FDP_57E,T_FDP_58E,T_FDP_59E,T_FDP_60E,
                  T_FDP_61E,T_FDP_62E,T_FDP_63E,T_FDP_64E,T_FDP_65E,T_FDP_66E,T_FDP_67E)
T_FDP_TextsE <- data.frame(T_FDP_TextsE)


################# GREENS TEXTS ############################
##### Overall Tweets
Gruene_Tweets = filter(Candidate_Tweets, party_id == "Gruene")
length(unique(Gruene_Tweets[["screen_name"]])) #61
Gruene_Unique <- unique(Gruene_Tweets[["screen_name"]])
Gruene_Tweets_1<- Gruene_Tweets %>% filter(Gruene$screen_name == "Die_Gruenen")
Gruene_Tweets_2<- Gruene_Tweets %>% filter(Gruene$screen_name == "GrueneBundestag")
Gruene_Tweets_3<- Gruene_Tweets %>% filter(Gruene$screen_name == "ABaerbock")
Gruene_Tweets_4<- Gruene_Tweets %>% filter(Gruene$screen_name == "GoeringEckardt")
Gruene_Tweets_5<- Gruene_Tweets %>% filter(Gruene$screen_name == "RenateKuenast")
Gruene_Tweets_6<- Gruene_Tweets %>% filter(Gruene$screen_name == "JTrittin")
Gruene_Tweets_7<- Gruene_Tweets %>% filter(Gruene$screen_name == "Oliver_Krischer")
Gruene_Tweets_8<- Gruene_Tweets %>% filter(Gruene$screen_name == "ebner_sha")
Gruene_Tweets_9<- Gruene_Tweets %>% filter(Gruene$screen_name == "MdB_Stroebele")
Gruene_Tweets_10<- Gruene_Tweets %>% filter(Gruene$screen_name == "KonstantinNotz")
Gruene_Tweets_11<- Gruene_Tweets %>% filter(Gruene$screen_name == "BriHasselmann")
Gruene_Tweets_12<- Gruene_Tweets %>% filter(Gruene$screen_name == "cem_oezdemir")
Gruene_Tweets_13<- Gruene_Tweets %>% filter(Gruene$screen_name == "Ricarda_Lang")
Gruene_Tweets_14<- Gruene_Tweets %>% filter(Gruene$screen_name == "KathaSchulze")
Gruene_Tweets_15<- Gruene_Tweets %>% filter(Gruene$screen_name == "MargareteBause")
Gruene_Tweets_16<- Gruene_Tweets %>% filter(Gruene$screen_name == "KaiGehring")
Gruene_Tweets_17<- Gruene_Tweets %>% filter(Gruene$screen_name == "svenlehmann")
Gruene_Tweets_18<- Gruene_Tweets %>% filter(Gruene$screen_name == "lisapaus")
Gruene_Tweets_19<- Gruene_Tweets %>% filter(Gruene$screen_name == "K_SA")
Gruene_Tweets_20<- Gruene_Tweets %>% filter(Gruene$screen_name == "KirstenKappert")
Gruene_Tweets_21<- Gruene_Tweets %>% filter(Gruene$screen_name == "OWvonHoltz")
Gruene_Tweets_22<- Gruene_Tweets %>% filter(Gruene$screen_name == "DanyWagner_DA")
Gruene_Tweets_23<- Gruene_Tweets %>% filter(Gruene$screen_name == "UweKekeritz")
Gruene_Tweets_24<- Gruene_Tweets %>% filter(Gruene$screen_name == "SteffiLemke")
Gruene_Tweets_25<- Gruene_Tweets %>% filter(Gruene$screen_name == "Ingrid_Nestle")
Gruene_Tweets_26<- Gruene_Tweets %>% filter(Gruene$screen_name == "julia_verlinden")
Gruene_Tweets_27<- Gruene_Tweets %>% filter(Gruene$screen_name == "KottingUhl")
Gruene_Tweets_28<- Gruene_Tweets %>% filter(Gruene$screen_name == "MatthiasGastel")
Gruene_Tweets_29<- Gruene_Tweets %>% filter(Gruene$screen_name == "StefanGelbhaar")
Gruene_Tweets_30<- Gruene_Tweets %>% filter(Gruene$screen_name == "BeateWaRo")
Gruene_Tweets_31<- Gruene_Tweets %>% filter(Gruene$screen_name == "ManuelaRottman")
Gruene_Tweets_32<- Gruene_Tweets %>% filter(Gruene$screen_name == "katdro")
Gruene_Tweets_33<- Gruene_Tweets %>% filter(Gruene$screen_name == "tobiaslindner")
Gruene_Tweets_34<- Gruene_Tweets %>% filter(Gruene$screen_name == "GruenClaudia")
Gruene_Tweets_35<- Gruene_Tweets %>% filter(Gruene$screen_name == "KatjaKeul")
Gruene_Tweets_36<- Gruene_Tweets %>% filter(Gruene$screen_name == "MarkusTressel")
Gruene_Tweets_37<- Gruene_Tweets %>% filter(Gruene$screen_name == "ChrisKuehn_mdb")
Gruene_Tweets_38<- Gruene_Tweets %>% filter(Gruene$screen_name == "Schmidt_MdB")
Gruene_Tweets_39<- Gruene_Tweets %>% filter(Gruene$screen_name == "badulrichmartha")
Gruene_Tweets_40<- Gruene_Tweets %>% filter(Gruene$screen_name == "sven_kindler")
Gruene_Tweets_41<- Gruene_Tweets %>% filter(Gruene$screen_name == "bhoffmann_mdb")
Gruene_Tweets_42<- Gruene_Tweets %>% filter(Gruene$screen_name == "margit_stumpp")
Gruene_Tweets_43<- Gruene_Tweets %>% filter(Gruene$screen_name == "annachristmann")
Gruene_Tweets_44<- Gruene_Tweets %>% filter(Gruene$screen_name == "HajdukBundestag")
Gruene_Tweets_45<- Gruene_Tweets %>% filter(Gruene$screen_name == "Erhard_Grundl")
Gruene_Tweets_46<- Gruene_Tweets %>% filter(Gruene$screen_name == "monikalazar")
Gruene_Tweets_47<- Gruene_Tweets %>% filter(Gruene$screen_name == "MariaKlSchmeink")
Gruene_Tweets_48<- Gruene_Tweets %>% filter(Gruene$screen_name == "ulle_schauws")
Gruene_Tweets_49<- Gruene_Tweets %>% filter(Gruene$screen_name == "W_SK")
Gruene_Tweets_50<- Gruene_Tweets %>% filter(Gruene$screen_name == "fbrantner")
Gruene_Tweets_51<- Gruene_Tweets %>% filter(Gruene$screen_name == "nouripour")
Gruene_Tweets_52<- Gruene_Tweets %>% filter(Gruene$screen_name == "markuskurthmdb")
Gruene_Tweets_53<- Gruene_Tweets %>% filter(Gruene$screen_name == "IreneMihalic")
Gruene_Tweets_54<- Gruene_Tweets %>% filter(Gruene$screen_name == "ManuelSarrazin")
Gruene_Tweets_55<- Gruene_Tweets %>% filter(Gruene$screen_name == "filizgreen")
Gruene_Tweets_56<- Gruene_Tweets %>% filter(Gruene$screen_name == "DJanecek")
Gruene_Tweets_57<- Gruene_Tweets %>% filter(Gruene$screen_name == "TabeaRoessner")
Gruene_Tweets_58<- Gruene_Tweets %>% filter(Gruene$screen_name == "ekindeligoez")
Gruene_Tweets_59<- Gruene_Tweets %>% filter(Gruene$screen_name == "GrueneBeate")
Gruene_Tweets_60<- Gruene_Tweets %>% filter(Gruene$screen_name == "agnieszka_mdb")
Gruene_Tweets_61<- Gruene_Tweets %>% filter(Gruene$screen_name == "max_lucks")
T_Gruene_1<- paste0(Gruene_Tweets_1$text, collapse = " ")
T_Gruene_2<- paste0(Gruene_Tweets_2$text, collapse = " ")
T_Gruene_3<- paste0(Gruene_Tweets_3$text, collapse = " ")
T_Gruene_4<- paste0(Gruene_Tweets_4$text, collapse = " ")
T_Gruene_5<- paste0(Gruene_Tweets_5$text, collapse = " ")
T_Gruene_6<- paste0(Gruene_Tweets_6$text, collapse = " ")
T_Gruene_7<- paste0(Gruene_Tweets_7$text, collapse = " ")
T_Gruene_8<- paste0(Gruene_Tweets_8$text, collapse = " ")
T_Gruene_9<- paste0(Gruene_Tweets_9$text, collapse = " ")
T_Gruene_10<- paste0(Gruene_Tweets_10$text, collapse = " ")
T_Gruene_11<- paste0(Gruene_Tweets_11$text, collapse = " ")
T_Gruene_12<- paste0(Gruene_Tweets_12$text, collapse = " ")
T_Gruene_13<- paste0(Gruene_Tweets_13$text, collapse = " ")
T_Gruene_14<- paste0(Gruene_Tweets_14$text, collapse = " ")
T_Gruene_15<- paste0(Gruene_Tweets_15$text, collapse = " ")
T_Gruene_16<- paste0(Gruene_Tweets_16$text, collapse = " ")
T_Gruene_17<- paste0(Gruene_Tweets_17$text, collapse = " ")
T_Gruene_18<- paste0(Gruene_Tweets_18$text, collapse = " ")
T_Gruene_19<- paste0(Gruene_Tweets_19$text, collapse = " ")
T_Gruene_20<- paste0(Gruene_Tweets_20$text, collapse = " ")
T_Gruene_21<- paste0(Gruene_Tweets_21$text, collapse = " ")
T_Gruene_22<- paste0(Gruene_Tweets_22$text, collapse = " ")
T_Gruene_23<- paste0(Gruene_Tweets_23$text, collapse = " ")
T_Gruene_24<- paste0(Gruene_Tweets_24$text, collapse = " ")
T_Gruene_25<- paste0(Gruene_Tweets_25$text, collapse = " ")
T_Gruene_26<- paste0(Gruene_Tweets_26$text, collapse = " ")
T_Gruene_27<- paste0(Gruene_Tweets_27$text, collapse = " ")
T_Gruene_28<- paste0(Gruene_Tweets_28$text, collapse = " ")
T_Gruene_29<- paste0(Gruene_Tweets_29$text, collapse = " ")
T_Gruene_30<- paste0(Gruene_Tweets_30$text, collapse = " ")
T_Gruene_31<- paste0(Gruene_Tweets_31$text, collapse = " ")
T_Gruene_32<- paste0(Gruene_Tweets_32$text, collapse = " ")
T_Gruene_33<- paste0(Gruene_Tweets_33$text, collapse = " ")
T_Gruene_34<- paste0(Gruene_Tweets_34$text, collapse = " ")
T_Gruene_35<- paste0(Gruene_Tweets_35$text, collapse = " ")
T_Gruene_36<- paste0(Gruene_Tweets_36$text, collapse = " ")
T_Gruene_37<- paste0(Gruene_Tweets_37$text, collapse = " ")
T_Gruene_38<- paste0(Gruene_Tweets_38$text, collapse = " ")
T_Gruene_39<- paste0(Gruene_Tweets_39$text, collapse = " ")
T_Gruene_40<- paste0(Gruene_Tweets_40$text, collapse = " ")
T_Gruene_41<- paste0(Gruene_Tweets_41$text, collapse = " ")
T_Gruene_42<- paste0(Gruene_Tweets_42$text, collapse = " ")
T_Gruene_43<- paste0(Gruene_Tweets_43$text, collapse = " ")
T_Gruene_44<- paste0(Gruene_Tweets_44$text, collapse = " ")
T_Gruene_45<- paste0(Gruene_Tweets_45$text, collapse = " ")
T_Gruene_46<- paste0(Gruene_Tweets_46$text, collapse = " ")
T_Gruene_47<- paste0(Gruene_Tweets_47$text, collapse = " ")
T_Gruene_48<- paste0(Gruene_Tweets_48$text, collapse = " ")
T_Gruene_49<- paste0(Gruene_Tweets_49$text, collapse = " ")
T_Gruene_50<- paste0(Gruene_Tweets_50$text, collapse = " ")
T_Gruene_51<- paste0(Gruene_Tweets_51$text, collapse = " ")
T_Gruene_52<- paste0(Gruene_Tweets_52$text, collapse = " ")
T_Gruene_53<- paste0(Gruene_Tweets_53$text, collapse = " ")
T_Gruene_54<- paste0(Gruene_Tweets_54$text, collapse = " ")
T_Gruene_55<- paste0(Gruene_Tweets_55$text, collapse = " ")
T_Gruene_56<- paste0(Gruene_Tweets_56$text, collapse = " ")
T_Gruene_57<- paste0(Gruene_Tweets_57$text, collapse = " ")
T_Gruene_58<- paste0(Gruene_Tweets_58$text, collapse = " ")
T_Gruene_59<- paste0(Gruene_Tweets_59$text, collapse = " ")
T_Gruene_60<- paste0(Gruene_Tweets_60$text, collapse = " ")
T_Gruene_61<- paste0(Gruene_Tweets_61$text, collapse = " ")

T_Gruene_Texts <- c(T_Gruene_1,T_Gruene_2,T_Gruene_3,T_Gruene_4,T_Gruene_5,T_Gruene_6,T_Gruene_7,T_Gruene_8,T_Gruene_9,T_Gruene_10,
                    T_Gruene_11,T_Gruene_12,T_Gruene_13,T_Gruene_14,T_Gruene_15,T_Gruene_16,T_Gruene_17,T_Gruene_18,T_Gruene_19,T_Gruene_20,
                    T_Gruene_21,T_Gruene_22,T_Gruene_23,T_Gruene_24,T_Gruene_25,T_Gruene_26,T_Gruene_27,T_Gruene_28,T_Gruene_29,T_Gruene_30,
                    T_Gruene_31,T_Gruene_32,T_Gruene_33,T_Gruene_34,T_Gruene_35,T_Gruene_36,T_Gruene_37,T_Gruene_38,T_Gruene_39,T_Gruene_40,
                    T_Gruene_41,T_Gruene_42,T_Gruene_43,T_Gruene_44,T_Gruene_45,T_Gruene_46,T_Gruene_47,T_Gruene_48,T_Gruene_49,T_Gruene_50,
                    T_Gruene_51,T_Gruene_52,T_Gruene_53,T_Gruene_54,T_Gruene_55,T_Gruene_56,T_Gruene_57,T_Gruene_58,T_Gruene_59,T_Gruene_60,
                    T_Gruene_61)
T_Gruene_Texts <- data.frame(T_Gruene_Texts)


##### Corona Tweets
Gruene_TweetsC <- Gruene_Tweets %>% filter(grepl('corona|coronavirus|covid-19|sars-cov-2|pandemie|lockdown|
                impfen|impfung|impfstoff|geimpft|ungeimpft|impfdosen|
                biontech|pfizer|astrazeneca|moderna|johnson&johnson|inzidenz|
                mpk|masken|maskenskandal|infektion|infektionsschutzgesetz|impfpflicht', text))
Gruene_Tweets_1C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "Die_Gruenen")
Gruene_Tweets_2C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "GrueneBundestag")
Gruene_Tweets_3C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "ABaerbock")
Gruene_Tweets_4C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "GoeringEckardt")
Gruene_Tweets_5C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "RenateKuenast")
Gruene_Tweets_6C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "JTrittin")
Gruene_Tweets_7C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "Oliver_Krischer")
Gruene_Tweets_8C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "ebner_sha")
Gruene_Tweets_9C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "MdB_Stroebele")
Gruene_Tweets_10C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "KonstantinNotz")
Gruene_Tweets_11C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "BriHasselmann")
Gruene_Tweets_12C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "cem_oezdemir")
Gruene_Tweets_13C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "Ricarda_Lang")
Gruene_Tweets_14C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "KathaSchulze")
Gruene_Tweets_15C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "MargareteBause")
Gruene_Tweets_16C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "KaiGehring")
Gruene_Tweets_17C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "svenlehmann")
Gruene_Tweets_18C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "lisapaus")
Gruene_Tweets_19C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "K_SA")
Gruene_Tweets_20C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "KirstenKappert")
Gruene_Tweets_21C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "OWvonHoltz")
Gruene_Tweets_22C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "DanyWagner_DA")
Gruene_Tweets_23C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "UweKekeritz")
Gruene_Tweets_24C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "SteffiLemke")
Gruene_Tweets_25C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "Ingrid_Nestle")
Gruene_Tweets_26C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "julia_verlinden")
Gruene_Tweets_27C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "KottingUhl")
Gruene_Tweets_28C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "MatthiasGastel")
Gruene_Tweets_29C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "StefanGelbhaar")
Gruene_Tweets_30C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "BeateWaRo")
Gruene_Tweets_31C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "ManuelaRottman")
Gruene_Tweets_32C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "katdro")
Gruene_Tweets_33C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "tobiaslindner")
Gruene_Tweets_34C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "GruenClaudia")
Gruene_Tweets_35C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "KatjaKeul")
Gruene_Tweets_36C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "MarkusTressel")
Gruene_Tweets_37C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "ChrisKuehn_mdb")
Gruene_Tweets_38C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "Schmidt_MdB")
Gruene_Tweets_39C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "badulrichmartha")
Gruene_Tweets_40C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "sven_kindler")
Gruene_Tweets_41C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "bhoffmann_mdb")
Gruene_Tweets_42C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "margit_stumpp")
Gruene_Tweets_43C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "annachristmann")
Gruene_Tweets_44C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "HajdukBundestag")
Gruene_Tweets_45C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "Erhard_Grundl")
Gruene_Tweets_46C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "monikalazar")
Gruene_Tweets_47C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "MariaKlSchmeink")
Gruene_Tweets_48C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "ulle_schauws")
Gruene_Tweets_49C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "W_SK")
Gruene_Tweets_50C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "fbrantner")
Gruene_Tweets_51C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "nouripour")
Gruene_Tweets_52C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "markuskurthmdb")
Gruene_Tweets_53C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "IreneMihalic")
Gruene_Tweets_54C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "ManuelSarrazin")
Gruene_Tweets_55C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "filizgreen")
Gruene_Tweets_56C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "DJanecek")
Gruene_Tweets_57C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "TabeaRoessner")
Gruene_Tweets_58C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "ekindeligoez")
Gruene_Tweets_59C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "GrueneBeate")
Gruene_Tweets_60C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "agnieszka_mdb")
Gruene_Tweets_61C<- Gruene_TweetsC %>% filter(Gruene_TweetsC$screen_name == "max_lucks")
T_Gruene_1C <- paste0(Gruene_Tweets_1C$text, collapse = " ")
T_Gruene_2C <- paste0(Gruene_Tweets_2C$text, collapse = " ")
T_Gruene_3C <- paste0(Gruene_Tweets_3C$text, collapse = " ")
T_Gruene_4C <- paste0(Gruene_Tweets_4C$text, collapse = " ")
T_Gruene_5C <- paste0(Gruene_Tweets_5C$text, collapse = " ")
T_Gruene_6C <- paste0(Gruene_Tweets_6C$text, collapse = " ")
T_Gruene_7C <- paste0(Gruene_Tweets_7C$text, collapse = " ")
T_Gruene_8C <- paste0(Gruene_Tweets_8C$text, collapse = " ")
T_Gruene_9C <- paste0(Gruene_Tweets_9C$text, collapse = " ")
T_Gruene_10C <- paste0(Gruene_Tweets_10C$text, collapse = " ")
T_Gruene_11C <- paste0(Gruene_Tweets_11C$text, collapse = " ")
T_Gruene_12C <- paste0(Gruene_Tweets_12C$text, collapse = " ")
T_Gruene_13C <- paste0(Gruene_Tweets_13C$text, collapse = " ")
T_Gruene_14C <- paste0(Gruene_Tweets_14C$text, collapse = " ")
T_Gruene_15C <- paste0(Gruene_Tweets_15C$text, collapse = " ")
T_Gruene_16C <- paste0(Gruene_Tweets_16C$text, collapse = " ")
T_Gruene_17C <- paste0(Gruene_Tweets_17C$text, collapse = " ")
T_Gruene_18C <- paste0(Gruene_Tweets_18C$text, collapse = " ")
T_Gruene_19C <- paste0(Gruene_Tweets_19C$text, collapse = " ")
T_Gruene_20C <- paste0(Gruene_Tweets_20C$text, collapse = " ")
T_Gruene_21C <- paste0(Gruene_Tweets_21C$text, collapse = " ")
T_Gruene_22C <- paste0(Gruene_Tweets_22C$text, collapse = " ")
T_Gruene_23C <- paste0(Gruene_Tweets_23C$text, collapse = " ")
T_Gruene_24C <- paste0(Gruene_Tweets_24C$text, collapse = " ")
T_Gruene_25C <- paste0(Gruene_Tweets_25C$text, collapse = " ")
T_Gruene_26C <- paste0(Gruene_Tweets_26C$text, collapse = " ")
T_Gruene_27C <- paste0(Gruene_Tweets_27C$text, collapse = " ")
T_Gruene_28C <- paste0(Gruene_Tweets_28C$text, collapse = " ")
T_Gruene_29C <- paste0(Gruene_Tweets_29C$text, collapse = " ")
T_Gruene_30C <- paste0(Gruene_Tweets_30C$text, collapse = " ")
T_Gruene_31C <- paste0(Gruene_Tweets_31C$text, collapse = " ")
T_Gruene_32C <- paste0(Gruene_Tweets_32C$text, collapse = " ")
T_Gruene_33C <- paste0(Gruene_Tweets_33C$text, collapse = " ")
T_Gruene_34C <- paste0(Gruene_Tweets_34C$text, collapse = " ")
T_Gruene_35C <- paste0(Gruene_Tweets_35C$text, collapse = " ")
T_Gruene_36C <- paste0(Gruene_Tweets_36C$text, collapse = " ")
T_Gruene_37C <- paste0(Gruene_Tweets_37C$text, collapse = " ")
T_Gruene_38C <- paste0(Gruene_Tweets_38C$text, collapse = " ")
T_Gruene_39C <- paste0(Gruene_Tweets_39C$text, collapse = " ")
T_Gruene_40C <- paste0(Gruene_Tweets_40C$text, collapse = " ")
T_Gruene_41C <- paste0(Gruene_Tweets_41C$text, collapse = " ")
T_Gruene_42C <- paste0(Gruene_Tweets_42C$text, collapse = " ")
T_Gruene_43C <- paste0(Gruene_Tweets_43C$text, collapse = " ")
T_Gruene_44C <- paste0(Gruene_Tweets_44C$text, collapse = " ")
T_Gruene_45C <- paste0(Gruene_Tweets_45C$text, collapse = " ")
T_Gruene_46C <- paste0(Gruene_Tweets_46C$text, collapse = " ")
T_Gruene_47C <- paste0(Gruene_Tweets_47C$text, collapse = " ")
T_Gruene_48C <- paste0(Gruene_Tweets_48C$text, collapse = " ")
T_Gruene_49C <- paste0(Gruene_Tweets_49C$text, collapse = " ")
T_Gruene_50C <- paste0(Gruene_Tweets_50C$text, collapse = " ")
T_Gruene_51C <- paste0(Gruene_Tweets_51C$text, collapse = " ")
T_Gruene_52C <- paste0(Gruene_Tweets_52C$text, collapse = " ")
T_Gruene_53C <- paste0(Gruene_Tweets_53C$text, collapse = " ")
T_Gruene_54C <- paste0(Gruene_Tweets_54C$text, collapse = " ")
T_Gruene_55C <- paste0(Gruene_Tweets_55C$text, collapse = " ")
T_Gruene_56C <- paste0(Gruene_Tweets_56C$text, collapse = " ")
T_Gruene_57C <- paste0(Gruene_Tweets_57C$text, collapse = " ")
T_Gruene_58C <- paste0(Gruene_Tweets_58C$text, collapse = " ")
T_Gruene_59C <- paste0(Gruene_Tweets_59C$text, collapse = " ")
T_Gruene_60C <- paste0(Gruene_Tweets_60C$text, collapse = " ")
T_Gruene_61C <- paste0(Gruene_Tweets_61C$text, collapse = " ")

T_Gruene_TextsC <- c(T_Gruene_1C,T_Gruene_2C,T_Gruene_3C,T_Gruene_4C,T_Gruene_5C,T_Gruene_6C,T_Gruene_7C,T_Gruene_8C,T_Gruene_9C,T_Gruene_10C,
                     T_Gruene_11C,T_Gruene_12C,T_Gruene_13C,T_Gruene_14C,T_Gruene_15C,T_Gruene_16C,T_Gruene_17C,T_Gruene_18C,T_Gruene_19C,T_Gruene_20C,
                     T_Gruene_21C,T_Gruene_22C,T_Gruene_23C,T_Gruene_24C,T_Gruene_25C,T_Gruene_26C,T_Gruene_27C,T_Gruene_28C,T_Gruene_29C,T_Gruene_30C,
                     T_Gruene_31C,T_Gruene_32C,T_Gruene_33C,T_Gruene_34C,T_Gruene_35C,T_Gruene_36C,T_Gruene_37C,T_Gruene_38C,T_Gruene_39C,T_Gruene_40C,
                     T_Gruene_41C,T_Gruene_42C,T_Gruene_43C,T_Gruene_44C,T_Gruene_45C,T_Gruene_46C,T_Gruene_47C,T_Gruene_48C,T_Gruene_49C,T_Gruene_50C,
                     T_Gruene_51C,T_Gruene_52C,T_Gruene_53C,T_Gruene_54C,T_Gruene_55C,T_Gruene_56C,T_Gruene_57C,T_Gruene_58C,T_Gruene_59C,T_Gruene_60C,
                     T_Gruene_61C)
T_Gruene_TextsC <- data.frame(T_Gruene_TextsC)


##### Environment Tweets
Gruene_TweetsE <- Gruene_Tweets %>% filter(grepl('klimaschutz|klimaschutzistjetzt|klimakrise|kohleausstieg|verkehrswende|
               klimawandel|erneuerbar|erneuerbaren|klimapolitik|hochwasser|sozialklimagerecht|
               nachhaltigkeit|nachhaltig|flutkatastrophe|klimapolitik|klimaänderung|klimaentwicklung', text))

Gruene_Tweets_1E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "Die_Gruenen")
Gruene_Tweets_2E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "GrueneBundestag")
Gruene_Tweets_3E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "ABaerbock")
Gruene_Tweets_4E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "GoeringEckardt")
Gruene_Tweets_5E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "RenateKuenast")
Gruene_Tweets_6E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "JTrittin")
Gruene_Tweets_7E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "Oliver_Krischer")
Gruene_Tweets_8E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "ebner_sha")
Gruene_Tweets_9E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "MdB_Stroebele")
Gruene_Tweets_10E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "KonstantinNotz")
Gruene_Tweets_11E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "BriHasselmann")
Gruene_Tweets_12E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "cem_oezdemir")
Gruene_Tweets_13E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "Ricarda_Lang")
Gruene_Tweets_14E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "KathaSchulze")
Gruene_Tweets_15E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "MargareteBause")
Gruene_Tweets_16E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "KaiGehring")
Gruene_Tweets_17E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "svenlehmann")
Gruene_Tweets_18E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "lisapaus")
Gruene_Tweets_19E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "K_SA")
Gruene_Tweets_20E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "KirstenKappert")
Gruene_Tweets_21E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "OWvonHoltz")
Gruene_Tweets_22E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "DanyWagner_DA")
Gruene_Tweets_23E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "UweKekeritz")
Gruene_Tweets_24E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "SteffiLemke")
Gruene_Tweets_25E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "Ingrid_Nestle")
Gruene_Tweets_26E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "julia_verlinden")
Gruene_Tweets_27E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "KottingUhl")
Gruene_Tweets_28E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "MatthiasGastel")
Gruene_Tweets_29E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "StefanGelbhaar")
Gruene_Tweets_30E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "BeateWaRo")
Gruene_Tweets_31E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "ManuelaRottman")
Gruene_Tweets_32E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "katdro")
Gruene_Tweets_33E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "tobiaslindner")
Gruene_Tweets_34E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "GruenClaudia")
Gruene_Tweets_35E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "KatjaKeul")
Gruene_Tweets_36E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "MarkusTressel")
Gruene_Tweets_37E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "ChrisKuehn_mdb")
Gruene_Tweets_38E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "Schmidt_MdB")
Gruene_Tweets_39E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "badulrichmartha")
Gruene_Tweets_40E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "sven_kindler")
Gruene_Tweets_41E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "bhoffmann_mdb")
Gruene_Tweets_42E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "margit_stumpp")
Gruene_Tweets_43E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "annachristmann")
Gruene_Tweets_44E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "HajdukBundestag")
Gruene_Tweets_45E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "Erhard_Grundl")
Gruene_Tweets_46E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "monikalazar")
Gruene_Tweets_47E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "MariaKlSchmeink")
Gruene_Tweets_48E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "ulle_schauws")
Gruene_Tweets_49E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "W_SK")
Gruene_Tweets_50E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "fbrantner")
Gruene_Tweets_51E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "nouripour")
Gruene_Tweets_52E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "markuskurthmdb")
Gruene_Tweets_53E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "IreneMihalic")
Gruene_Tweets_54E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "ManuelSarrazin")
Gruene_Tweets_55E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "filizgreen")
Gruene_Tweets_56E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "DJanecek")
Gruene_Tweets_57E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "TabeaRoessner")
Gruene_Tweets_58E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "ekindeligoez")
Gruene_Tweets_59E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "GrueneBeate")
Gruene_Tweets_60E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "agnieszka_mdb")
Gruene_Tweets_61E<- Gruene_TweetsE %>% filter(Gruene_TweetsE$screen_name == "max_lucks")
T_Gruene_1E <- paste0(Gruene_Tweets_1E$text, collapse = " ")
T_Gruene_2E <- paste0(Gruene_Tweets_2E$text, collapse = " ")
T_Gruene_3E <- paste0(Gruene_Tweets_3E$text, collapse = " ")
T_Gruene_4E <- paste0(Gruene_Tweets_4E$text, collapse = " ")
T_Gruene_5E <- paste0(Gruene_Tweets_5E$text, collapse = " ")
T_Gruene_6E <- paste0(Gruene_Tweets_6E$text, collapse = " ")
T_Gruene_7E <- paste0(Gruene_Tweets_7E$text, collapse = " ")
T_Gruene_8E <- paste0(Gruene_Tweets_8E$text, collapse = " ")
T_Gruene_9E <- paste0(Gruene_Tweets_9E$text, collapse = " ")
T_Gruene_10E <- paste0(Gruene_Tweets_10E$text, collapse = " ")
T_Gruene_11E <- paste0(Gruene_Tweets_11E$text, collapse = " ")
T_Gruene_12E <- paste0(Gruene_Tweets_12E$text, collapse = " ")
T_Gruene_13E <- paste0(Gruene_Tweets_13E$text, collapse = " ")
T_Gruene_14E <- paste0(Gruene_Tweets_14E$text, collapse = " ")
T_Gruene_15E <- paste0(Gruene_Tweets_15E$text, collapse = " ")
T_Gruene_16E <- paste0(Gruene_Tweets_16E$text, collapse = " ")
T_Gruene_17E <- paste0(Gruene_Tweets_17E$text, collapse = " ")
T_Gruene_18E <- paste0(Gruene_Tweets_18E$text, collapse = " ")
T_Gruene_19E <- paste0(Gruene_Tweets_19E$text, collapse = " ")
T_Gruene_20E <- paste0(Gruene_Tweets_20E$text, collapse = " ")
T_Gruene_21E <- paste0(Gruene_Tweets_21E$text, collapse = " ")
T_Gruene_22E <- paste0(Gruene_Tweets_22E$text, collapse = " ")
T_Gruene_23E <- paste0(Gruene_Tweets_23E$text, collapse = " ")
T_Gruene_24E <- paste0(Gruene_Tweets_24E$text, collapse = " ")
T_Gruene_25E <- paste0(Gruene_Tweets_25E$text, collapse = " ")
T_Gruene_26E <- paste0(Gruene_Tweets_26E$text, collapse = " ")
T_Gruene_27E <- paste0(Gruene_Tweets_27E$text, collapse = " ")
T_Gruene_28E <- paste0(Gruene_Tweets_28E$text, collapse = " ")
T_Gruene_29E <- paste0(Gruene_Tweets_29E$text, collapse = " ")
T_Gruene_30E <- paste0(Gruene_Tweets_30E$text, collapse = " ")
T_Gruene_31E <- paste0(Gruene_Tweets_31E$text, collapse = " ")
T_Gruene_32E <- paste0(Gruene_Tweets_32E$text, collapse = " ")
T_Gruene_33E <- paste0(Gruene_Tweets_33E$text, collapse = " ")
T_Gruene_34E <- paste0(Gruene_Tweets_34E$text, collapse = " ")
T_Gruene_35E <- paste0(Gruene_Tweets_35E$text, collapse = " ")
T_Gruene_36E <- paste0(Gruene_Tweets_36E$text, collapse = " ")
T_Gruene_37E <- paste0(Gruene_Tweets_37E$text, collapse = " ")
T_Gruene_38E <- paste0(Gruene_Tweets_38E$text, collapse = " ")
T_Gruene_39E <- paste0(Gruene_Tweets_39E$text, collapse = " ")
T_Gruene_40E <- paste0(Gruene_Tweets_40E$text, collapse = " ")
T_Gruene_41E <- paste0(Gruene_Tweets_41E$text, collapse = " ")
T_Gruene_42E <- paste0(Gruene_Tweets_42E$text, collapse = " ")
T_Gruene_43E <- paste0(Gruene_Tweets_43E$text, collapse = " ")
T_Gruene_44E <- paste0(Gruene_Tweets_44E$text, collapse = " ")
T_Gruene_45E <- paste0(Gruene_Tweets_45E$text, collapse = " ")
T_Gruene_46E <- paste0(Gruene_Tweets_46E$text, collapse = " ")
T_Gruene_47E <- paste0(Gruene_Tweets_47E$text, collapse = " ")
T_Gruene_48E <- paste0(Gruene_Tweets_48E$text, collapse = " ")
T_Gruene_49E <- paste0(Gruene_Tweets_49E$text, collapse = " ")
T_Gruene_50E <- paste0(Gruene_Tweets_50E$text, collapse = " ")
T_Gruene_51E <- paste0(Gruene_Tweets_51E$text, collapse = " ")
T_Gruene_52E <- paste0(Gruene_Tweets_52E$text, collapse = " ")
T_Gruene_53E <- paste0(Gruene_Tweets_53E$text, collapse = " ")
T_Gruene_54E <- paste0(Gruene_Tweets_54E$text, collapse = " ")
T_Gruene_55E <- paste0(Gruene_Tweets_55E$text, collapse = " ")
T_Gruene_56E <- paste0(Gruene_Tweets_56E$text, collapse = " ")
T_Gruene_57E <- paste0(Gruene_Tweets_57E$text, collapse = " ")
T_Gruene_58E <- paste0(Gruene_Tweets_58E$text, collapse = " ")
T_Gruene_59E <- paste0(Gruene_Tweets_59E$text, collapse = " ")
T_Gruene_60E <- paste0(Gruene_Tweets_60E$text, collapse = " ")
T_Gruene_61E <- paste0(Gruene_Tweets_61E$text, collapse = " ")

T_Gruene_TextsE <- c(T_Gruene_1E,T_Gruene_2E,T_Gruene_3E,T_Gruene_4E,T_Gruene_5E,T_Gruene_6E,T_Gruene_7E,T_Gruene_8E,T_Gruene_9E,T_Gruene_10E,
                     T_Gruene_11E,T_Gruene_12E,T_Gruene_13E,T_Gruene_14E,T_Gruene_15E,T_Gruene_16E,T_Gruene_17E,T_Gruene_18E,T_Gruene_19E,T_Gruene_20E,
                     T_Gruene_21E,T_Gruene_22E,T_Gruene_23E,T_Gruene_24E,T_Gruene_25E,T_Gruene_26E,T_Gruene_27E,T_Gruene_28E,T_Gruene_29E,T_Gruene_30E,
                     T_Gruene_31E,T_Gruene_32E,T_Gruene_33E,T_Gruene_34E,T_Gruene_35E,T_Gruene_36E,T_Gruene_37E,T_Gruene_38E,T_Gruene_39E,T_Gruene_40E,
                     T_Gruene_41E,T_Gruene_42E,T_Gruene_43E,T_Gruene_44E,T_Gruene_45E,T_Gruene_46E,T_Gruene_47E,T_Gruene_48E,T_Gruene_49E,T_Gruene_50E,
                     T_Gruene_51E,T_Gruene_52E,T_Gruene_53E,T_Gruene_54E,T_Gruene_55E,T_Gruene_56E,T_Gruene_57E,T_Gruene_58E,T_Gruene_59E,T_Gruene_60E,
                     T_Gruene_61E)
T_Gruene_TextsE <- data.frame(T_Gruene_TextsE)


################# THE LEFT TEXTS ##########################
##### Overall Tweets
Linke_Tweets = filter(Candidate_Tweets, party_id == "Linke")
length(unique(Linke_Tweets[["screen_name"]])) #43
Linke_Unique <- unique(Linke_Tweets[["screen_name"]])
Linke_Tweets_1<- Linke_Tweets %>% filter(Linke$screen_name == "dieLinke")
Linke_Tweets_2<- Linke_Tweets %>% filter(Linke$screen_name == "Linksfraktion")
Linke_Tweets_3<- Linke_Tweets %>% filter(Linke$screen_name == "Janine_Wissler")
Linke_Tweets_4<- Linke_Tweets %>% filter(Linke$screen_name == "DietmarBartsch")
Linke_Tweets_5<- Linke_Tweets %>% filter(Linke$screen_name == "katjakipping")
Linke_Tweets_6<- Linke_Tweets %>% filter(Linke$screen_name == "b_riexinger")
Linke_Tweets_7<- Linke_Tweets %>% filter(Linke$screen_name == "GregorGysi")
Linke_Tweets_8<- Linke_Tweets %>% filter(Linke$screen_name == "CarenLay")
Linke_Tweets_9<- Linke_Tweets %>% filter(Linke$screen_name == "MdB_Freihold")
Linke_Tweets_10<- Linke_Tweets %>% filter(Linke$screen_name == "michel_brandt_")
Linke_Tweets_11<- Linke_Tweets %>% filter(Linke$screen_name == "HESommer")
Linke_Tweets_12<- Linke_Tweets %>% filter(Linke$screen_name == "AkbulutGokay")
Linke_Tweets_13<- Linke_Tweets %>% filter(Linke$screen_name == "MdB_Schreiber")
Linke_Tweets_14<- Linke_Tweets %>% filter(Linke$screen_name == "MartinaRenner")
Linke_Tweets_15<- Linke_Tweets %>% filter(Linke$screen_name == "FrStraetmanns")
Linke_Tweets_16<- Linke_Tweets %>% filter(Linke$screen_name == "pascalmeiser")
Linke_Tweets_17<- Linke_Tweets %>% filter(Linke$screen_name == "Norbert_MdB")
Linke_Tweets_18<- Linke_Tweets %>% filter(Linke$screen_name == "UllaJelpke")
Linke_Tweets_19<- Linke_Tweets %>% filter(Linke$screen_name == "SBarrientosK")
Linke_Tweets_20<- Linke_Tweets %>% filter(Linke$screen_name == "victorperli")
Linke_Tweets_21<- Linke_Tweets %>% filter(Linke$screen_name == "NicoleGohlke")
Linke_Tweets_22<- Linke_Tweets %>% filter(Linke$screen_name == "berlinliebich")
Linke_Tweets_23<- Linke_Tweets %>% filter(Linke$screen_name == "jankortemdb")
Linke_Tweets_24<- Linke_Tweets %>% filter(Linke$screen_name == "Amira_M_Ali")
Linke_Tweets_25<- Linke_Tweets %>% filter(Linke$screen_name == "HeikeHaensel")
Linke_Tweets_26<- Linke_Tweets %>% filter(Linke$screen_name == "LoetzschMdB")
Linke_Tweets_27<- Linke_Tweets %>% filter(Linke$screen_name == "ch_buchholz")
Linke_Tweets_28<- Linke_Tweets %>% filter(Linke$screen_name == "SevimDagdelen")
Linke_Tweets_29<- Linke_Tweets %>% filter(Linke$screen_name == "AndrejHunko")
Linke_Tweets_30<- Linke_Tweets %>% filter(Linke$screen_name == "AlexanderSNeu")
Linke_Tweets_31<- Linke_Tweets %>% filter(Linke$screen_name == "Diether_Dehm")
Linke_Tweets_32<- Linke_Tweets %>% filter(Linke$screen_name == "NiemaMovassat")
Linke_Tweets_33<- Linke_Tweets %>% filter(Linke$screen_name == "PetraPauMaHe")
Linke_Tweets_34<- Linke_Tweets %>% filter(Linke$screen_name == "voglerk")
Linke_Tweets_35<- Linke_Tweets %>% filter(Linke$screen_name == "DorisAchelwilm")
Linke_Tweets_36<- Linke_Tweets %>% filter(Linke$screen_name == "ZaklinNastic")
Linke_Tweets_37<- Linke_Tweets %>% filter(Linke$screen_name == "tpflueger")
Linke_Tweets_38<- Linke_Tweets %>% filter(Linke$screen_name == "ZdebelHubertus")
Linke_Tweets_39<- Linke_Tweets %>% filter(Linke$screen_name == "joerg_cezanne")
Linke_Tweets_40<- Linke_Tweets %>% filter(Linke$screen_name == "katrin_staffler")
Linke_Tweets_41<- Linke_Tweets %>% filter(Linke$screen_name == "SylviaGabelmann")
Linke_Tweets_42<- Linke_Tweets %>% filter(Linke$screen_name == "kerwolter")
Linke_Tweets_43<- Linke_Tweets %>% filter(Linke$screen_name == "MartinNeise")
T_Linke_1<- paste0(Linke_Tweets_1$text, collapse = " ")
T_Linke_2<- paste0(Linke_Tweets_2$text, collapse = " ")
T_Linke_3<- paste0(Linke_Tweets_3$text, collapse = " ")
T_Linke_4<- paste0(Linke_Tweets_4$text, collapse = " ")
T_Linke_5<- paste0(Linke_Tweets_5$text, collapse = " ")
T_Linke_6<- paste0(Linke_Tweets_6$text, collapse = " ")
T_Linke_7<- paste0(Linke_Tweets_7$text, collapse = " ")
T_Linke_8<- paste0(Linke_Tweets_8$text, collapse = " ")
T_Linke_9<- paste0(Linke_Tweets_9$text, collapse = " ")
T_Linke_10<- paste0(Linke_Tweets_10$text, collapse = " ")
T_Linke_11<- paste0(Linke_Tweets_11$text, collapse = " ")
T_Linke_12<- paste0(Linke_Tweets_12$text, collapse = " ")
T_Linke_13<- paste0(Linke_Tweets_13$text, collapse = " ")
T_Linke_14<- paste0(Linke_Tweets_14$text, collapse = " ")
T_Linke_15<- paste0(Linke_Tweets_15$text, collapse = " ")
T_Linke_16<- paste0(Linke_Tweets_16$text, collapse = " ")
T_Linke_17<- paste0(Linke_Tweets_17$text, collapse = " ")
T_Linke_18<- paste0(Linke_Tweets_18$text, collapse = " ")
T_Linke_19<- paste0(Linke_Tweets_19$text, collapse = " ")
T_Linke_20<- paste0(Linke_Tweets_20$text, collapse = " ")
T_Linke_21<- paste0(Linke_Tweets_21$text, collapse = " ")
T_Linke_22<- paste0(Linke_Tweets_22$text, collapse = " ")
T_Linke_23<- paste0(Linke_Tweets_23$text, collapse = " ")
T_Linke_24<- paste0(Linke_Tweets_24$text, collapse = " ")
T_Linke_25<- paste0(Linke_Tweets_25$text, collapse = " ")
T_Linke_26<- paste0(Linke_Tweets_26$text, collapse = " ")
T_Linke_27<- paste0(Linke_Tweets_27$text, collapse = " ")
T_Linke_28<- paste0(Linke_Tweets_28$text, collapse = " ")
T_Linke_29<- paste0(Linke_Tweets_29$text, collapse = " ")
T_Linke_30<- paste0(Linke_Tweets_30$text, collapse = " ")
T_Linke_31<- paste0(Linke_Tweets_31$text, collapse = " ")
T_Linke_32<- paste0(Linke_Tweets_32$text, collapse = " ")
T_Linke_33<- paste0(Linke_Tweets_33$text, collapse = " ")
T_Linke_34<- paste0(Linke_Tweets_34$text, collapse = " ")
T_Linke_35<- paste0(Linke_Tweets_35$text, collapse = " ")
T_Linke_36<- paste0(Linke_Tweets_36$text, collapse = " ")
T_Linke_37<- paste0(Linke_Tweets_37$text, collapse = " ")
T_Linke_38<- paste0(Linke_Tweets_38$text, collapse = " ")
T_Linke_39<- paste0(Linke_Tweets_39$text, collapse = " ")
T_Linke_40<- paste0(Linke_Tweets_40$text, collapse = " ")
T_Linke_41<- paste0(Linke_Tweets_41$text, collapse = " ")
T_Linke_42<- paste0(Linke_Tweets_42$text, collapse = " ")
T_Linke_43<- paste0(Linke_Tweets_43$text, collapse = " ")

T_Linke_Texts <- c(T_Linke_1,T_Linke_2,T_Linke_3,T_Linke_4,T_Linke_5,T_Linke_6,T_Linke_7,T_Linke_8,T_Linke_9,T_Linke_10,
                   T_Linke_11,T_Linke_12,T_Linke_13,T_Linke_14,T_Linke_15,T_Linke_16,T_Linke_17,T_Linke_18,T_Linke_19,T_Linke_20,
                   T_Linke_21,T_Linke_22,T_Linke_23,T_Linke_24,T_Linke_25,T_Linke_26,T_Linke_27,T_Linke_28,T_Linke_29,T_Linke_30,
                   T_Linke_31,T_Linke_32,T_Linke_33,T_Linke_34,T_Linke_35,T_Linke_36,T_Linke_37,T_Linke_38,T_Linke_39,T_Linke_40,
                   T_Linke_41,T_Linke_42,T_Linke_43)
T_Linke_Texts <- data.frame(T_Linke_Texts)


##### Corona Tweets
Linke_TweetsC <- Linke_Tweets %>% filter(grepl('corona|coronavirus|covid-19|sars-cov-2|pandemie|lockdown|
                impfen|impfung|impfstoff|geimpft|ungeimpft|impfdosen|
                biontech|pfizer|astrazeneca|moderna|johnson&johnson|inzidenz|
                mpk|masken|maskenskandal|infektion|infektionsschutzgesetz|impfpflicht', text))
Linke_Tweets_1C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "dieLinke")
Linke_Tweets_2C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "Linksfraktion")
Linke_Tweets_3C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "Janine_Wissler")
Linke_Tweets_4C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "DietmarBartsch")
Linke_Tweets_5C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "katjakipping")
Linke_Tweets_6C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "b_riexinger")
Linke_Tweets_7C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "GregorGysi")
Linke_Tweets_8C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "CarenLay")
Linke_Tweets_9C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "MdB_Freihold")
Linke_Tweets_10C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "michel_brandt_")
Linke_Tweets_11C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "HESommer")
Linke_Tweets_12C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "AkbulutGokay")
Linke_Tweets_13C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "MdB_Schreiber")
Linke_Tweets_14C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "MartinaRenner")
Linke_Tweets_15C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "FrStraetmanns")
Linke_Tweets_16C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "pascalmeiser")
Linke_Tweets_17C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "Norbert_MdB")
Linke_Tweets_18C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "UllaJelpke")
Linke_Tweets_19C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "SBarrientosK")
Linke_Tweets_20C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "victorperli")
Linke_Tweets_21C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "NicoleGohlke")
Linke_Tweets_22C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "berlinliebich")
Linke_Tweets_23C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "jankortemdb")
Linke_Tweets_24C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "Amira_M_Ali")
Linke_Tweets_25C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "HeikeHaensel")
Linke_Tweets_26C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "LoetzschMdB")
Linke_Tweets_27C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "ch_buchholz")
Linke_Tweets_28C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "SevimDagdelen")
Linke_Tweets_29C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "AndrejHunko")
Linke_Tweets_30C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "AlexanderSNeu")
Linke_Tweets_31C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "Diether_Dehm")
Linke_Tweets_32C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "NiemaMovassat")
Linke_Tweets_33C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "PetraPauMaHe")
Linke_Tweets_34C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "voglerk")
Linke_Tweets_35C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "DorisAchelwilm")
Linke_Tweets_36C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "ZaklinNastic")
Linke_Tweets_37C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "tpflueger")
Linke_Tweets_38C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "ZdebelHubertus")
Linke_Tweets_39C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "joerg_cezanne")
Linke_Tweets_40C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "katrin_staffler")
Linke_Tweets_41C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "SylviaGabelmann")
Linke_Tweets_42C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "kerwolter")
Linke_Tweets_43C<- Linke_TweetsC %>% filter(Linke_TweetsC$screen_name == "MartinNeise")
T_Linke_1C <- paste0(Linke_Tweets_1C$text, collapse = " ")
T_Linke_2C <- paste0(Linke_Tweets_2C$text, collapse = " ")
T_Linke_3C <- paste0(Linke_Tweets_3C$text, collapse = " ")
T_Linke_4C <- paste0(Linke_Tweets_4C$text, collapse = " ")
T_Linke_5C <- paste0(Linke_Tweets_5C$text, collapse = " ")
T_Linke_6C <- paste0(Linke_Tweets_6C$text, collapse = " ")
T_Linke_7C <- paste0(Linke_Tweets_7C$text, collapse = " ")
T_Linke_8C <- paste0(Linke_Tweets_8C$text, collapse = " ")
T_Linke_9C <- paste0(Linke_Tweets_9C$text, collapse = " ")
T_Linke_10C <- paste0(Linke_Tweets_10C$text, collapse = " ")
T_Linke_11C <- paste0(Linke_Tweets_11C$text, collapse = " ")
T_Linke_12C <- paste0(Linke_Tweets_12C$text, collapse = " ")
T_Linke_13C <- paste0(Linke_Tweets_13C$text, collapse = " ")
T_Linke_14C <- paste0(Linke_Tweets_14C$text, collapse = " ")
T_Linke_15C <- paste0(Linke_Tweets_15C$text, collapse = " ")
T_Linke_16C <- paste0(Linke_Tweets_16C$text, collapse = " ")
T_Linke_17C <- paste0(Linke_Tweets_17C$text, collapse = " ")
T_Linke_18C <- paste0(Linke_Tweets_18C$text, collapse = " ")
T_Linke_19C <- paste0(Linke_Tweets_19C$text, collapse = " ")
T_Linke_20C <- paste0(Linke_Tweets_20C$text, collapse = " ")
T_Linke_21C <- paste0(Linke_Tweets_21C$text, collapse = " ")
T_Linke_22C <- paste0(Linke_Tweets_22C$text, collapse = " ")
T_Linke_23C <- paste0(Linke_Tweets_23C$text, collapse = " ")
T_Linke_24C <- paste0(Linke_Tweets_24C$text, collapse = " ")
T_Linke_25C <- paste0(Linke_Tweets_25C$text, collapse = " ")
T_Linke_26C <- paste0(Linke_Tweets_26C$text, collapse = " ")
T_Linke_27C <- paste0(Linke_Tweets_27C$text, collapse = " ")
T_Linke_28C <- paste0(Linke_Tweets_28C$text, collapse = " ")
T_Linke_29C <- paste0(Linke_Tweets_29C$text, collapse = " ")
T_Linke_30C <- paste0(Linke_Tweets_30C$text, collapse = " ")
T_Linke_31C <- paste0(Linke_Tweets_31C$text, collapse = " ")
T_Linke_32C <- paste0(Linke_Tweets_32C$text, collapse = " ")
T_Linke_33C <- paste0(Linke_Tweets_33C$text, collapse = " ")
T_Linke_34C <- paste0(Linke_Tweets_34C$text, collapse = " ")
T_Linke_35C <- paste0(Linke_Tweets_35C$text, collapse = " ")
T_Linke_36C <- paste0(Linke_Tweets_36C$text, collapse = " ")
T_Linke_37C <- paste0(Linke_Tweets_37C$text, collapse = " ")
T_Linke_38C <- paste0(Linke_Tweets_38C$text, collapse = " ")
T_Linke_39C <- paste0(Linke_Tweets_39C$text, collapse = " ")
T_Linke_40C <- paste0(Linke_Tweets_40C$text, collapse = " ")
T_Linke_41C <- paste0(Linke_Tweets_41C$text, collapse = " ")
T_Linke_42C <- paste0(Linke_Tweets_42C$text, collapse = " ")
T_Linke_43C <- paste0(Linke_Tweets_43C$text, collapse = " ")


T_Linke_TextsC <- c(T_Linke_1C,T_Linke_2C,T_Linke_3C,T_Linke_4C,T_Linke_5C,T_Linke_6C,T_Linke_7C,T_Linke_8C,T_Linke_9C,T_Linke_10C,
                    T_Linke_11C,T_Linke_12C,T_Linke_13C,T_Linke_14C,T_Linke_15C,T_Linke_16C,T_Linke_17C,T_Linke_18C,T_Linke_19C,T_Linke_20C,
                    T_Linke_21C,T_Linke_22C,T_Linke_23C,T_Linke_24C,T_Linke_25C,T_Linke_26C,T_Linke_27C,T_Linke_28C,T_Linke_29C,T_Linke_30C,
                    T_Linke_31C,T_Linke_32C,T_Linke_33C,T_Linke_34C,T_Linke_35C,T_Linke_36C,T_Linke_37C,T_Linke_38C,T_Linke_39C,T_Linke_40C,
                    T_Linke_41C,T_Linke_42C,T_Linke_43C)
T_Linke_TextsC <- data.frame(T_Linke_TextsC)


##### Environment Tweets
Linke_TweetsE <- Linke_Tweets %>% filter(grepl('klimaschutz|klimaschutzistjetzt|klimakrise|kohleausstieg|verkehrswende|
               klimawandel|erneuerbar|erneuerbaren|klimapolitik|hochwasser|sozialklimagerecht|
               nachhaltigkeit|nachhaltig|flutkatastrophe|klimapolitik|klimaänderung|klimaentwicklung', text))
Linke_Tweets_1E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "dieLinke")
Linke_Tweets_2E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "Linksfraktion")
Linke_Tweets_3E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "Janine_Wissler")
Linke_Tweets_4E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "DietmarBartsch")
Linke_Tweets_5E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "katjakipping")
Linke_Tweets_6E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "b_riexinger")
Linke_Tweets_7E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "GregorGysi")
Linke_Tweets_8E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "CarenLay")
Linke_Tweets_9E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "MdB_Freihold")
Linke_Tweets_10E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "michel_brandt_")
Linke_Tweets_11E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "HESommer")
Linke_Tweets_12E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "AkbulutGokay")
Linke_Tweets_13E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "MdB_Schreiber")
Linke_Tweets_14E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "MartinaRenner")
Linke_Tweets_15E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "FrStraetmanns")
Linke_Tweets_16E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "pascalmeiser")
Linke_Tweets_17E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "Norbert_MdB")
Linke_Tweets_18E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "UllaJelpke")
Linke_Tweets_19E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "SBarrientosK")
Linke_Tweets_20E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "victorperli")
Linke_Tweets_21E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "NicoleGohlke")
Linke_Tweets_22E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "berlinliebich")
Linke_Tweets_23E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "jankortemdb")
Linke_Tweets_24E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "Amira_M_Ali")
Linke_Tweets_25E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "HeikeHaensel")
Linke_Tweets_26E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "LoetzschMdB")
Linke_Tweets_27E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "ch_buchholz")
Linke_Tweets_28E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "SevimDagdelen")
Linke_Tweets_29E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "AndrejHunko")
Linke_Tweets_30E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "AlexanderSNeu")
Linke_Tweets_31E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "Diether_Dehm")
Linke_Tweets_32E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "NiemaMovassat")
Linke_Tweets_33E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "PetraPauMaHe")
Linke_Tweets_34E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "voglerk")
Linke_Tweets_35E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "DorisAchelwilm")
Linke_Tweets_36E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "ZaklinNastic")
Linke_Tweets_37E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "tpflueger")
Linke_Tweets_38E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "ZdebelHubertus")
Linke_Tweets_39E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "joerg_cezanne")
Linke_Tweets_40E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "katrin_staffler")
Linke_Tweets_41E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "SylviaGabelmann")
Linke_Tweets_42E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "kerwolter")
Linke_Tweets_43E<- Linke_TweetsE %>% filter(Linke_TweetsE$screen_name == "MartinNeise")
T_Linke_1E <- paste0(Linke_Tweets_1E$text, collapse = " ")
T_Linke_2E <- paste0(Linke_Tweets_2E$text, collapse = " ")
T_Linke_3E <- paste0(Linke_Tweets_3E$text, collapse = " ")
T_Linke_4E <- paste0(Linke_Tweets_4E$text, collapse = " ")
T_Linke_5E <- paste0(Linke_Tweets_5E$text, collapse = " ")
T_Linke_6E <- paste0(Linke_Tweets_6E$text, collapse = " ")
T_Linke_7E <- paste0(Linke_Tweets_7E$text, collapse = " ")
T_Linke_8E <- paste0(Linke_Tweets_8E$text, collapse = " ")
T_Linke_9E <- paste0(Linke_Tweets_9E$text, collapse = " ")
T_Linke_10E <- paste0(Linke_Tweets_10E$text, collapse = " ")
T_Linke_11E <- paste0(Linke_Tweets_11E$text, collapse = " ")
T_Linke_12E <- paste0(Linke_Tweets_12E$text, collapse = " ")
T_Linke_13E <- paste0(Linke_Tweets_13E$text, collapse = " ")
T_Linke_14E <- paste0(Linke_Tweets_14E$text, collapse = " ")
T_Linke_15E <- paste0(Linke_Tweets_15E$text, collapse = " ")
T_Linke_16E <- paste0(Linke_Tweets_16E$text, collapse = " ")
T_Linke_17E <- paste0(Linke_Tweets_17E$text, collapse = " ")
T_Linke_18E <- paste0(Linke_Tweets_18E$text, collapse = " ")
T_Linke_19E <- paste0(Linke_Tweets_19E$text, collapse = " ")
T_Linke_20E <- paste0(Linke_Tweets_20E$text, collapse = " ")
T_Linke_21E <- paste0(Linke_Tweets_21E$text, collapse = " ")
T_Linke_22E <- paste0(Linke_Tweets_22E$text, collapse = " ")
T_Linke_23E <- paste0(Linke_Tweets_23E$text, collapse = " ")
T_Linke_24E <- paste0(Linke_Tweets_24E$text, collapse = " ")
T_Linke_25E <- paste0(Linke_Tweets_25E$text, collapse = " ")
T_Linke_26E <- paste0(Linke_Tweets_26E$text, collapse = " ")
T_Linke_27E <- paste0(Linke_Tweets_27E$text, collapse = " ")
T_Linke_28E <- paste0(Linke_Tweets_28E$text, collapse = " ")
T_Linke_29E <- paste0(Linke_Tweets_29E$text, collapse = " ")
T_Linke_30E <- paste0(Linke_Tweets_30E$text, collapse = " ")
T_Linke_31E <- paste0(Linke_Tweets_31E$text, collapse = " ")
T_Linke_32E <- paste0(Linke_Tweets_32E$text, collapse = " ")
T_Linke_33E <- paste0(Linke_Tweets_33E$text, collapse = " ")
T_Linke_34E <- paste0(Linke_Tweets_34E$text, collapse = " ")
T_Linke_35E <- paste0(Linke_Tweets_35E$text, collapse = " ")
T_Linke_36E <- paste0(Linke_Tweets_36E$text, collapse = " ")
T_Linke_37E <- paste0(Linke_Tweets_37E$text, collapse = " ")
T_Linke_38E <- paste0(Linke_Tweets_38E$text, collapse = " ")
T_Linke_39E <- paste0(Linke_Tweets_39E$text, collapse = " ")
T_Linke_40E <- paste0(Linke_Tweets_40E$text, collapse = " ")
T_Linke_41E <- paste0(Linke_Tweets_41E$text, collapse = " ")
T_Linke_42E <- paste0(Linke_Tweets_42E$text, collapse = " ")
T_Linke_43E <- paste0(Linke_Tweets_43E$text, collapse = " ")

T_Linke_TextsE <- c(T_Linke_1E,T_Linke_2E,T_Linke_3E,T_Linke_4E,T_Linke_5E,T_Linke_6E,T_Linke_7E,T_Linke_8E,T_Linke_9E,T_Linke_10E,
                    T_Linke_11E,T_Linke_12E,T_Linke_13E,T_Linke_14E,T_Linke_15E,T_Linke_16E,T_Linke_17E,T_Linke_18E,T_Linke_19E,T_Linke_20E,
                    T_Linke_21E,T_Linke_22E,T_Linke_23E,T_Linke_24E,T_Linke_25E,T_Linke_26E,T_Linke_27E,T_Linke_28E,T_Linke_29E,T_Linke_30E,
                    T_Linke_31E,T_Linke_32E,T_Linke_33E,T_Linke_34E,T_Linke_35E,T_Linke_36E,T_Linke_37E,T_Linke_38E,T_Linke_39E,T_Linke_40E,
                    T_Linke_41E,T_Linke_42E,T_Linke_43E)
T_Linke_TextsE <- data.frame(T_Linke_TextsE)


################# SPD TEXTS ###############################
##### Overall Tweets
SPD_Tweets = filter(Candidate_Tweets, party_id == "SPD")
length(unique(SPD_Tweets[["screen_name"]])) #79
SPD_Unique <- unique(SPD_Tweets[["screen_name"]])
SPD_Tweets_1<- SPD_Tweets %>% filter(SPD$screen_name == "spdde")
SPD_Tweets_2<- SPD_Tweets %>% filter(SPD$screen_name == "spdbt")
SPD_Tweets_3<- SPD_Tweets %>% filter(SPD$screen_name == "OlafScholz")
SPD_Tweets_4<- SPD_Tweets %>% filter(SPD$screen_name == "HeikoMaas")
SPD_Tweets_5<- SPD_Tweets %>% filter(SPD$screen_name == "hubertus_heil")
SPD_Tweets_6<- SPD_Tweets %>% filter(SPD$screen_name == "larsklingbeil")
SPD_Tweets_7<- SPD_Tweets %>% filter(SPD$screen_name == "KuehniKev")
SPD_Tweets_8<- SPD_Tweets %>% filter(SPD$screen_name == "EskenSaskia")
SPD_Tweets_9<- SPD_Tweets %>% filter(SPD$screen_name == "NowaboFM")
SPD_Tweets_10<- SPD_Tweets %>% filter(SPD$screen_name == "Ralf_Stegner")
SPD_Tweets_11<- SPD_Tweets %>% filter(SPD$screen_name == "Karl_Lauterbach")
SPD_Tweets_12<- SPD_Tweets %>% filter(SPD$screen_name == "Timon_Gremmels")
SPD_Tweets_13<- SPD_Tweets %>% filter(SPD$screen_name == "SiemtjeMdB")
SPD_Tweets_14<- SPD_Tweets %>% filter(SPD$screen_name == "AnneBressem")
SPD_Tweets_15<- SPD_Tweets %>% filter(SPD$screen_name == "HellmichMdB")
SPD_Tweets_16<- SPD_Tweets %>% filter(SPD$screen_name == "stadler_svenja")
SPD_Tweets_17<- SPD_Tweets %>% filter(SPD$screen_name == "Schwarz_MdB")
SPD_Tweets_18<- SPD_Tweets %>% filter(SPD$screen_name == "UliFreese")
SPD_Tweets_19<- SPD_Tweets %>% filter(SPD$screen_name == "DennisRohde")
SPD_Tweets_20<- SPD_Tweets %>% filter(SPD$screen_name == "ThomasHitschler")
SPD_Tweets_21<- SPD_Tweets %>% filter(SPD$screen_name == "BrunnerGanzOhr")
SPD_Tweets_22<- SPD_Tweets %>% filter(SPD$screen_name == "GabyKatzmarek")
SPD_Tweets_23<- SPD_Tweets %>% filter(SPD$screen_name == "Andreas_Rimkus")
SPD_Tweets_24<- SPD_Tweets %>% filter(SPD$screen_name == "A_Gloeckner")
SPD_Tweets_25<- SPD_Tweets %>% filter(SPD$screen_name == "Lothar_Binding")
SPD_Tweets_26<- SPD_Tweets %>% filter(SPD$screen_name == "matthiasbartke")
SPD_Tweets_27<- SPD_Tweets %>% filter(SPD$screen_name == "IsabelMackensen")
SPD_Tweets_28<- SPD_Tweets %>% filter(SPD$screen_name == "KatjaMast")
SPD_Tweets_29<- SPD_Tweets %>% filter(SPD$screen_name == "Kaiser_SPD")
SPD_Tweets_30<- SPD_Tweets %>% filter(SPD$screen_name == "KorkmazGT")
SPD_Tweets_31<- SPD_Tweets %>% filter(SPD$screen_name == "FalkoMohrs")
SPD_Tweets_32<- SPD_Tweets %>% filter(SPD$screen_name == "baldy_daniel")
SPD_Tweets_33<- SPD_Tweets %>% filter(SPD$screen_name == "nadjasthamer")
SPD_Tweets_34<- SPD_Tweets %>% filter(SPD$screen_name == "Krawallstein")
SPD_Tweets_35<- SPD_Tweets %>% filter(SPD$screen_name == "Lina_Seitzl")
SPD_Tweets_36<- SPD_Tweets %>% filter(SPD$screen_name == "jessi_rosenthal")
SPD_Tweets_37<- SPD_Tweets %>% filter(SPD$screen_name == "AnniKlose")
SPD_Tweets_38<- SPD_Tweets %>% filter(SPD$screen_name == "El_KaWeh_")
SPD_Tweets_39<- SPD_Tweets %>% filter(SPD$screen_name == "josephineortleb")
SPD_Tweets_40<- SPD_Tweets %>% filter(SPD$screen_name == "FrankeEdgar")
SPD_Tweets_41<- SPD_Tweets %>% filter(SPD$screen_name == "dieschmidt")
SPD_Tweets_42<- SPD_Tweets %>% filter(SPD$screen_name == "jungeinberlin")
SPD_Tweets_43<- SPD_Tweets %>% filter(SPD$screen_name == "CPetryMdB")
SPD_Tweets_44<- SPD_Tweets %>% filter(SPD$screen_name == "SteffenSonja")
SPD_Tweets_45<- SPD_Tweets %>% filter(SPD$screen_name == "fritzfelgentreu")
SPD_Tweets_46<- SPD_Tweets %>% filter(SPD$screen_name == "mischrodi")
SPD_Tweets_47<- SPD_Tweets %>% filter(SPD$screen_name == "UliGroetsch")
SPD_Tweets_48<- SPD_Tweets %>% filter(SPD$screen_name == "larscastellucci")
SPD_Tweets_49<- SPD_Tweets %>% filter(SPD$screen_name == "RitaHaglKehl")
SPD_Tweets_50<- SPD_Tweets %>% filter(SPD$screen_name == "KlausMindrup")
SPD_Tweets_51<- SPD_Tweets %>% filter(SPD$screen_name == "michael_thews")
SPD_Tweets_52<- SPD_Tweets %>% filter(SPD$screen_name == "MuellerChemnitz")
SPD_Tweets_53<- SPD_Tweets %>% filter(SPD$screen_name == "NielsAnnen")
SPD_Tweets_54<- SPD_Tweets %>% filter(SPD$screen_name == "stonie_kiel")
SPD_Tweets_55<- SPD_Tweets %>% filter(SPD$screen_name == "MarjaVoellers")
SPD_Tweets_56<- SPD_Tweets %>% filter(SPD$screen_name == "LeniBreymaier")
SPD_Tweets_57<- SPD_Tweets %>% filter(SPD$screen_name == "UlliNissen")
SPD_Tweets_58<- SPD_Tweets %>% filter(SPD$screen_name == "MetinHakverdi")
SPD_Tweets_59<- SPD_Tweets %>% filter(SPD$screen_name == "NinaScheer_SPD")
SPD_Tweets_60<- SPD_Tweets %>% filter(SPD$screen_name == "SoenkeRix")
SPD_Tweets_61<- SPD_Tweets %>% filter(SPD$screen_name == "HildeMattheis")
SPD_Tweets_62<- SPD_Tweets %>% filter(SPD$screen_name == "baerbelbas")
SPD_Tweets_63<- SPD_Tweets %>% filter(SPD$screen_name == "FrankSchwabe")
SPD_Tweets_64<- SPD_Tweets %>% filter(SPD$screen_name == "BaerbelKofler")
SPD_Tweets_65<- SPD_Tweets %>% filter(SPD$screen_name == "GaHeinrich")
SPD_Tweets_66<- SPD_Tweets %>% filter(SPD$screen_name == "danielakolbe")
SPD_Tweets_67<- SPD_Tweets %>% filter(SPD$screen_name == "CanselK")
SPD_Tweets_68<- SPD_Tweets %>% filter(SPD$screen_name == "zierke")
SPD_Tweets_69<- SPD_Tweets %>% filter(SPD$screen_name == "s_schwartze")
SPD_Tweets_70<- SPD_Tweets %>% filter(SPD$screen_name == "Achim_P")
SPD_Tweets_71<- SPD_Tweets %>% filter(SPD$screen_name == "soerenbartol")
SPD_Tweets_72<- SPD_Tweets %>% filter(SPD$screen_name == "MechthildRawert")
SPD_Tweets_73<- SPD_Tweets %>% filter(SPD$screen_name == "rischwasu")
SPD_Tweets_74<- SPD_Tweets %>% filter(SPD$screen_name == "FlorianPost")
SPD_Tweets_75<- SPD_Tweets %>% filter(SPD$screen_name == "sebast_hartmann")
SPD_Tweets_76<- SPD_Tweets %>% filter(SPD$screen_name == "oezdemir_spd")
SPD_Tweets_77<- SPD_Tweets %>% filter(SPD$screen_name == "DirkWieseSPD")
SPD_Tweets_78<- SPD_Tweets %>% filter(SPD$screen_name == "UteVogt")
SPD_Tweets_79<- SPD_Tweets %>% filter(SPD$screen_name == "NilsSchmid")
T_SPD_1<- paste0(SPD_Tweets_1$text, collapse = " ")
T_SPD_2<- paste0(SPD_Tweets_2$text, collapse = " ")
T_SPD_3<- paste0(SPD_Tweets_3$text, collapse = " ")
T_SPD_4<- paste0(SPD_Tweets_4$text, collapse = " ")
T_SPD_5<- paste0(SPD_Tweets_5$text, collapse = " ")
T_SPD_6<- paste0(SPD_Tweets_6$text, collapse = " ")
T_SPD_7<- paste0(SPD_Tweets_7$text, collapse = " ")
T_SPD_8<- paste0(SPD_Tweets_8$text, collapse = " ")
T_SPD_9<- paste0(SPD_Tweets_9$text, collapse = " ")
T_SPD_10<- paste0(SPD_Tweets_10$text, collapse = " ")
T_SPD_11<- paste0(SPD_Tweets_11$text, collapse = " ")
T_SPD_12<- paste0(SPD_Tweets_12$text, collapse = " ")
T_SPD_13<- paste0(SPD_Tweets_13$text, collapse = " ")
T_SPD_14<- paste0(SPD_Tweets_14$text, collapse = " ")
T_SPD_15<- paste0(SPD_Tweets_15$text, collapse = " ")
T_SPD_16<- paste0(SPD_Tweets_16$text, collapse = " ")
T_SPD_17<- paste0(SPD_Tweets_17$text, collapse = " ")
T_SPD_18<- paste0(SPD_Tweets_18$text, collapse = " ")
T_SPD_19<- paste0(SPD_Tweets_19$text, collapse = " ")
T_SPD_20<- paste0(SPD_Tweets_20$text, collapse = " ")
T_SPD_21<- paste0(SPD_Tweets_21$text, collapse = " ")
T_SPD_22<- paste0(SPD_Tweets_22$text, collapse = " ")
T_SPD_23<- paste0(SPD_Tweets_23$text, collapse = " ")
T_SPD_24<- paste0(SPD_Tweets_24$text, collapse = " ")
T_SPD_25<- paste0(SPD_Tweets_25$text, collapse = " ")
T_SPD_26<- paste0(SPD_Tweets_26$text, collapse = " ")
T_SPD_27<- paste0(SPD_Tweets_27$text, collapse = " ")
T_SPD_28<- paste0(SPD_Tweets_28$text, collapse = " ")
T_SPD_29<- paste0(SPD_Tweets_29$text, collapse = " ")
T_SPD_30<- paste0(SPD_Tweets_30$text, collapse = " ")
T_SPD_31<- paste0(SPD_Tweets_31$text, collapse = " ")
T_SPD_32<- paste0(SPD_Tweets_32$text, collapse = " ")
T_SPD_33<- paste0(SPD_Tweets_33$text, collapse = " ")
T_SPD_34<- paste0(SPD_Tweets_34$text, collapse = " ")
T_SPD_35<- paste0(SPD_Tweets_35$text, collapse = " ")
T_SPD_36<- paste0(SPD_Tweets_36$text, collapse = " ")
T_SPD_37<- paste0(SPD_Tweets_37$text, collapse = " ")
T_SPD_38<- paste0(SPD_Tweets_38$text, collapse = " ")
T_SPD_39<- paste0(SPD_Tweets_39$text, collapse = " ")
T_SPD_40<- paste0(SPD_Tweets_40$text, collapse = " ")
T_SPD_41<- paste0(SPD_Tweets_41$text, collapse = " ")
T_SPD_42<- paste0(SPD_Tweets_42$text, collapse = " ")
T_SPD_43<- paste0(SPD_Tweets_43$text, collapse = " ")
T_SPD_44<- paste0(SPD_Tweets_44$text, collapse = " ")
T_SPD_45<- paste0(SPD_Tweets_45$text, collapse = " ")
T_SPD_46<- paste0(SPD_Tweets_46$text, collapse = " ")
T_SPD_47<- paste0(SPD_Tweets_47$text, collapse = " ")
T_SPD_48<- paste0(SPD_Tweets_48$text, collapse = " ")
T_SPD_49<- paste0(SPD_Tweets_49$text, collapse = " ")
T_SPD_50<- paste0(SPD_Tweets_50$text, collapse = " ")
T_SPD_51<- paste0(SPD_Tweets_51$text, collapse = " ")
T_SPD_52<- paste0(SPD_Tweets_52$text, collapse = " ")
T_SPD_53<- paste0(SPD_Tweets_53$text, collapse = " ")
T_SPD_54<- paste0(SPD_Tweets_54$text, collapse = " ")
T_SPD_55<- paste0(SPD_Tweets_55$text, collapse = " ")
T_SPD_56<- paste0(SPD_Tweets_56$text, collapse = " ")
T_SPD_57<- paste0(SPD_Tweets_57$text, collapse = " ")
T_SPD_58<- paste0(SPD_Tweets_58$text, collapse = " ")
T_SPD_59<- paste0(SPD_Tweets_59$text, collapse = " ")
T_SPD_60<- paste0(SPD_Tweets_60$text, collapse = " ")
T_SPD_61<- paste0(SPD_Tweets_61$text, collapse = " ")
T_SPD_62<- paste0(SPD_Tweets_62$text, collapse = " ")
T_SPD_63<- paste0(SPD_Tweets_63$text, collapse = " ")
T_SPD_64<- paste0(SPD_Tweets_64$text, collapse = " ")
T_SPD_65<- paste0(SPD_Tweets_65$text, collapse = " ")
T_SPD_66<- paste0(SPD_Tweets_66$text, collapse = " ")
T_SPD_67<- paste0(SPD_Tweets_67$text, collapse = " ")
T_SPD_68<- paste0(SPD_Tweets_68$text, collapse = " ")
T_SPD_69<- paste0(SPD_Tweets_69$text, collapse = " ")
T_SPD_70<- paste0(SPD_Tweets_70$text, collapse = " ")
T_SPD_71<- paste0(SPD_Tweets_71$text, collapse = " ")
T_SPD_72<- paste0(SPD_Tweets_72$text, collapse = " ")
T_SPD_73<- paste0(SPD_Tweets_73$text, collapse = " ")
T_SPD_74<- paste0(SPD_Tweets_74$text, collapse = " ")
T_SPD_75<- paste0(SPD_Tweets_75$text, collapse = " ")
T_SPD_76<- paste0(SPD_Tweets_76$text, collapse = " ")
T_SPD_77<- paste0(SPD_Tweets_77$text, collapse = " ")
T_SPD_78<- paste0(SPD_Tweets_78$text, collapse = " ")
T_SPD_79<- paste0(SPD_Tweets_79$text, collapse = " ")

T_SPD_Texts <- c(T_SPD_1,T_SPD_2,T_SPD_3,T_SPD_4,T_SPD_5,T_SPD_6,T_SPD_7,T_SPD_8,T_SPD_9,T_SPD_10,
                 T_SPD_11,T_SPD_12,T_SPD_13,T_SPD_14,T_SPD_15,T_SPD_16,T_SPD_17,T_SPD_18,T_SPD_19,T_SPD_20,
                 T_SPD_21,T_SPD_22,T_SPD_23,T_SPD_24,T_SPD_25,T_SPD_26,T_SPD_27,T_SPD_28,T_SPD_29,T_SPD_30,
                 T_SPD_31,T_SPD_32,T_SPD_33,T_SPD_34,T_SPD_35,T_SPD_36,T_SPD_37,T_SPD_38,T_SPD_39,T_SPD_40,
                 T_SPD_41,T_SPD_42,T_SPD_43,T_SPD_44,T_SPD_45,T_SPD_46,T_SPD_47,T_SPD_48,T_SPD_49,T_SPD_50,
                 T_SPD_51,T_SPD_52,T_SPD_53,T_SPD_54,T_SPD_55,T_SPD_56,T_SPD_57,T_SPD_58,T_SPD_59,T_SPD_60,
                 T_SPD_61,T_SPD_62,T_SPD_63,T_SPD_64,T_SPD_65,T_SPD_66,T_SPD_67,T_SPD_68,T_SPD_69,T_SPD_70,
                 T_SPD_71,T_SPD_72,T_SPD_73,T_SPD_74,T_SPD_75,T_SPD_76,T_SPD_77,T_SPD_78,T_SPD_79)
T_SPD_Texts <- data.frame(T_SPD_Texts)


##### Corona Tweets
SPD_TweetsC <- SPD_Tweets %>% filter(grepl('corona|coronavirus|covid-19|sars-cov-2|pandemie|lockdown|
                impfen|impfung|impfstoff|geimpft|ungeimpft|impfdosen|
                biontech|pfizer|astrazeneca|moderna|johnson&johnson|inzidenz|
                mpk|masken|maskenskandal|infektion|infektionsschutzgesetz|impfpflicht', text))
SPD_Tweets_1C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "spdde")
SPD_Tweets_2C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "spdbt")
SPD_Tweets_3C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "OlafScholz")
SPD_Tweets_4C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "HeikoMaas")
SPD_Tweets_5C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "hubertus_heil")
SPD_Tweets_6C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "larsklingbeil")
SPD_Tweets_7C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "KuehniKev")
SPD_Tweets_8C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "EskenSaskia")
SPD_Tweets_9C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "NowaboFM")
SPD_Tweets_10C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "Ralf_Stegner")
SPD_Tweets_11C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "Karl_Lauterbach")
SPD_Tweets_12C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "Timon_Gremmels")
SPD_Tweets_13C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "SiemtjeMdB")
SPD_Tweets_14C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "AnneBressem")
SPD_Tweets_15C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "HellmichMdB")
SPD_Tweets_16C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "stadler_svenja")
SPD_Tweets_17C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "Schwarz_MdB")
SPD_Tweets_18C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "UliFreese")
SPD_Tweets_19C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "DennisRohde")
SPD_Tweets_20C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "ThomasHitschler")
SPD_Tweets_21C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "BrunnerGanzOhr")
SPD_Tweets_22C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "GabyKatzmarek")
SPD_Tweets_23C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "Andreas_Rimkus")
SPD_Tweets_24C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "A_Gloeckner")
SPD_Tweets_25C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "Lothar_Binding")
SPD_Tweets_26C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "matthiasbartke")
SPD_Tweets_27C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "IsabelMackensen")
SPD_Tweets_28C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "KatjaMast")
SPD_Tweets_29C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "Kaiser_SPD")
SPD_Tweets_30C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "KorkmazGT")
SPD_Tweets_31C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "FalkoMohrs")
SPD_Tweets_32C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "baldy_daniel")
SPD_Tweets_33C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "nadjasthamer")
SPD_Tweets_34C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "Krawallstein")
SPD_Tweets_35C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "Lina_Seitzl")
SPD_Tweets_36C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "jessi_rosenthal")
SPD_Tweets_37C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "AnniKlose")
SPD_Tweets_38C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "El_KaWeh_")
SPD_Tweets_39C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "josephineortleb")
SPD_Tweets_40C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "FrankeEdgar")
SPD_Tweets_41C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "dieschmidt")
SPD_Tweets_42C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "jungeinberlin")
SPD_Tweets_43C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "CPetryMdB")
SPD_Tweets_44C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "SteffenSonja")
SPD_Tweets_45C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "fritzfelgentreu")
SPD_Tweets_46C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "mischrodi")
SPD_Tweets_47C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "UliGroetsch")
SPD_Tweets_48C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "larscastellucci")
SPD_Tweets_49C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "RitaHaglKehl")
SPD_Tweets_50C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "KlausMindrup")
SPD_Tweets_51C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "michael_thews")
SPD_Tweets_52C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "MuellerChemnitz")
SPD_Tweets_53C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "NielsAnnen")
SPD_Tweets_54C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "stonie_kiel")
SPD_Tweets_55C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "MarjaVoellers")
SPD_Tweets_56C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "LeniBreymaier")
SPD_Tweets_57C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "UlliNissen")
SPD_Tweets_58C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "MetinHakverdi")
SPD_Tweets_59C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "NinaScheer_SPD")
SPD_Tweets_60C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "SoenkeRix")
SPD_Tweets_61C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "HildeMattheis")
SPD_Tweets_62C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "baerbelbas")
SPD_Tweets_63C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "FrankSchwabe")
SPD_Tweets_64C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "BaerbelKofler")
SPD_Tweets_65C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "GaHeinrich")
SPD_Tweets_66C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "danielakolbe")
SPD_Tweets_67C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "CanselK")
SPD_Tweets_68C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "zierke")
SPD_Tweets_69C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "s_schwartze")
SPD_Tweets_70C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "Achim_P")
SPD_Tweets_71C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "soerenbartol")
SPD_Tweets_72C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "MechthildRawert")
SPD_Tweets_73C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "rischwasu")
SPD_Tweets_74C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "FlorianPost")
SPD_Tweets_75C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "sebast_hartmann")
SPD_Tweets_76C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "oezdemir_spd")
SPD_Tweets_77C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "DirkWieseSPD")
SPD_Tweets_78C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "UteVogt")
SPD_Tweets_79C<- SPD_TweetsC %>% filter(SPD_TweetsC$screen_name == "NilsSchmid")
T_SPD_1C <- paste0(SPD_Tweets_1C$text, collapse = " ")
T_SPD_2C <- paste0(SPD_Tweets_2C$text, collapse = " ")
T_SPD_3C <- paste0(SPD_Tweets_3C$text, collapse = " ")
T_SPD_4C <- paste0(SPD_Tweets_4C$text, collapse = " ")
T_SPD_5C <- paste0(SPD_Tweets_5C$text, collapse = " ")
T_SPD_6C <- paste0(SPD_Tweets_6C$text, collapse = " ")
T_SPD_7C <- paste0(SPD_Tweets_7C$text, collapse = " ")
T_SPD_8C <- paste0(SPD_Tweets_8C$text, collapse = " ")
T_SPD_9C <- paste0(SPD_Tweets_9C$text, collapse = " ")
T_SPD_10C <- paste0(SPD_Tweets_10C$text, collapse = " ")
T_SPD_11C <- paste0(SPD_Tweets_11C$text, collapse = " ")
T_SPD_12C <- paste0(SPD_Tweets_12C$text, collapse = " ")
T_SPD_13C <- paste0(SPD_Tweets_13C$text, collapse = " ")
T_SPD_14C <- paste0(SPD_Tweets_14C$text, collapse = " ")
T_SPD_15C <- paste0(SPD_Tweets_15C$text, collapse = " ")
T_SPD_16C <- paste0(SPD_Tweets_16C$text, collapse = " ")
T_SPD_17C <- paste0(SPD_Tweets_17C$text, collapse = " ")
T_SPD_18C <- paste0(SPD_Tweets_18C$text, collapse = " ")
T_SPD_19C <- paste0(SPD_Tweets_19C$text, collapse = " ")
T_SPD_20C <- paste0(SPD_Tweets_20C$text, collapse = " ")
T_SPD_21C <- paste0(SPD_Tweets_21C$text, collapse = " ")
T_SPD_22C <- paste0(SPD_Tweets_22C$text, collapse = " ")
T_SPD_23C <- paste0(SPD_Tweets_23C$text, collapse = " ")
T_SPD_24C <- paste0(SPD_Tweets_24C$text, collapse = " ")
T_SPD_25C <- paste0(SPD_Tweets_25C$text, collapse = " ")
T_SPD_26C <- paste0(SPD_Tweets_26C$text, collapse = " ")
T_SPD_27C <- paste0(SPD_Tweets_27C$text, collapse = " ")
T_SPD_28C <- paste0(SPD_Tweets_28C$text, collapse = " ")
T_SPD_29C <- paste0(SPD_Tweets_29C$text, collapse = " ")
T_SPD_30C <- paste0(SPD_Tweets_30C$text, collapse = " ")
T_SPD_31C <- paste0(SPD_Tweets_31C$text, collapse = " ")
T_SPD_32C <- paste0(SPD_Tweets_32C$text, collapse = " ")
T_SPD_33C <- paste0(SPD_Tweets_33C$text, collapse = " ")
T_SPD_34C <- paste0(SPD_Tweets_34C$text, collapse = " ")
T_SPD_35C <- paste0(SPD_Tweets_35C$text, collapse = " ")
T_SPD_36C <- paste0(SPD_Tweets_36C$text, collapse = " ")
T_SPD_37C <- paste0(SPD_Tweets_37C$text, collapse = " ")
T_SPD_38C <- paste0(SPD_Tweets_38C$text, collapse = " ")
T_SPD_39C <- paste0(SPD_Tweets_39C$text, collapse = " ")
T_SPD_40C <- paste0(SPD_Tweets_40C$text, collapse = " ")
T_SPD_41C <- paste0(SPD_Tweets_41C$text, collapse = " ")
T_SPD_42C <- paste0(SPD_Tweets_42C$text, collapse = " ")
T_SPD_43C <- paste0(SPD_Tweets_43C$text, collapse = " ")
T_SPD_44C <- paste0(SPD_Tweets_44C$text, collapse = " ")
T_SPD_45C <- paste0(SPD_Tweets_45C$text, collapse = " ")
T_SPD_46C <- paste0(SPD_Tweets_46C$text, collapse = " ")
T_SPD_47C <- paste0(SPD_Tweets_47C$text, collapse = " ")
T_SPD_48C <- paste0(SPD_Tweets_48C$text, collapse = " ")
T_SPD_49C <- paste0(SPD_Tweets_49C$text, collapse = " ")
T_SPD_50C <- paste0(SPD_Tweets_50C$text, collapse = " ")
T_SPD_51C <- paste0(SPD_Tweets_51C$text, collapse = " ")
T_SPD_52C <- paste0(SPD_Tweets_52C$text, collapse = " ")
T_SPD_53C <- paste0(SPD_Tweets_53C$text, collapse = " ")
T_SPD_54C <- paste0(SPD_Tweets_54C$text, collapse = " ")
T_SPD_55C <- paste0(SPD_Tweets_55C$text, collapse = " ")
T_SPD_56C <- paste0(SPD_Tweets_56C$text, collapse = " ")
T_SPD_57C <- paste0(SPD_Tweets_57C$text, collapse = " ")
T_SPD_58C <- paste0(SPD_Tweets_58C$text, collapse = " ")
T_SPD_59C <- paste0(SPD_Tweets_59C$text, collapse = " ")
T_SPD_60C <- paste0(SPD_Tweets_60C$text, collapse = " ")
T_SPD_61C <- paste0(SPD_Tweets_61C$text, collapse = " ")
T_SPD_62C <- paste0(SPD_Tweets_62C$text, collapse = " ")
T_SPD_63C <- paste0(SPD_Tweets_63C$text, collapse = " ")
T_SPD_64C <- paste0(SPD_Tweets_64C$text, collapse = " ")
T_SPD_65C <- paste0(SPD_Tweets_65C$text, collapse = " ")
T_SPD_66C <- paste0(SPD_Tweets_66C$text, collapse = " ")
T_SPD_67C <- paste0(SPD_Tweets_67C$text, collapse = " ")
T_SPD_68C <- paste0(SPD_Tweets_68C$text, collapse = " ")
T_SPD_69C <- paste0(SPD_Tweets_69C$text, collapse = " ")
T_SPD_70C <- paste0(SPD_Tweets_70C$text, collapse = " ")
T_SPD_71C <- paste0(SPD_Tweets_71C$text, collapse = " ")
T_SPD_72C <- paste0(SPD_Tweets_72C$text, collapse = " ")
T_SPD_73C <- paste0(SPD_Tweets_73C$text, collapse = " ")
T_SPD_74C <- paste0(SPD_Tweets_74C$text, collapse = " ")
T_SPD_75C <- paste0(SPD_Tweets_75C$text, collapse = " ")
T_SPD_76C <- paste0(SPD_Tweets_76C$text, collapse = " ")
T_SPD_77C <- paste0(SPD_Tweets_77C$text, collapse = " ")
T_SPD_78C <- paste0(SPD_Tweets_78C$text, collapse = " ")
T_SPD_79C <- paste0(SPD_Tweets_79C$text, collapse = " ")

T_SPD_TextsC <- c(T_SPD_1C,T_SPD_2C,T_SPD_3C,T_SPD_4C,T_SPD_5C,T_SPD_6C,T_SPD_7C,T_SPD_8C,T_SPD_9C,T_SPD_10C,
                  T_SPD_11C,T_SPD_12C,T_SPD_13C,T_SPD_14C,T_SPD_15C,T_SPD_16C,T_SPD_17C,T_SPD_18C,T_SPD_19C,T_SPD_20C,
                  T_SPD_21C,T_SPD_22C,T_SPD_23C,T_SPD_24C,T_SPD_25C,T_SPD_26C,T_SPD_27C,T_SPD_28C,T_SPD_29C,T_SPD_30C,
                  T_SPD_31C,T_SPD_32C,T_SPD_33C,T_SPD_34C,T_SPD_35C,T_SPD_36C,T_SPD_37C,T_SPD_38C,T_SPD_39C,T_SPD_40C,
                  T_SPD_41C,T_SPD_42C,T_SPD_43C,T_SPD_44C,T_SPD_45C,T_SPD_46C,T_SPD_47C,T_SPD_48C,T_SPD_49C,T_SPD_50C,
                  T_SPD_51C,T_SPD_52C,T_SPD_53C,T_SPD_54C,T_SPD_55C,T_SPD_56C,T_SPD_57C,T_SPD_58C,T_SPD_59C,T_SPD_60C,
                  T_SPD_61C,T_SPD_62C,T_SPD_63C,T_SPD_64C,T_SPD_65C,T_SPD_66C,T_SPD_67C,T_SPD_68C,T_SPD_69C,T_SPD_70C,
                  T_SPD_71C,T_SPD_72C,T_SPD_73C,T_SPD_74C,T_SPD_75C,T_SPD_76C,T_SPD_77C,T_SPD_78C,T_SPD_79C)
T_SPD_TextsC <- data.frame(T_SPD_TextsC)


##### Environment Tweets
SPD_TweetsE <- SPD_Tweets %>% filter(grepl('klimaschutz|klimaschutzistjetzt|klimakrise|kohleausstieg|verkehrswende|
               klimawandel|erneuerbar|erneuerbaren|klimapolitik|hochwasser|sozialklimagerecht|
               nachhaltigkeit|nachhaltig|flutkatastrophe|klimapolitik|klimaänderung|klimaentwicklung', text))
SPD_Tweets_1E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "spdde")
SPD_Tweets_2E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "spdbt")
SPD_Tweets_3E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "OlafScholz")
SPD_Tweets_4E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "HeikoMaas")
SPD_Tweets_5E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "hubertus_heil")
SPD_Tweets_6E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "larsklingbeil")
SPD_Tweets_7E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "KuehniKev")
SPD_Tweets_8E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "EskenSaskia")
SPD_Tweets_9E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "NowaboFM")
SPD_Tweets_10E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "Ralf_Stegner")
SPD_Tweets_11E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "Karl_Lauterbach")
SPD_Tweets_12E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "Timon_Gremmels")
SPD_Tweets_13E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "SiemtjeMdB")
SPD_Tweets_14E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "AnneBressem")
SPD_Tweets_15E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "HellmichMdB")
SPD_Tweets_16E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "stadler_svenja")
SPD_Tweets_17E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "Schwarz_MdB")
SPD_Tweets_18E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "UliFreese")
SPD_Tweets_19E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "DennisRohde")
SPD_Tweets_20E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "ThomasHitschler")
SPD_Tweets_21E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "BrunnerGanzOhr")
SPD_Tweets_22E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "GabyKatzmarek")
SPD_Tweets_23E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "Andreas_Rimkus")
SPD_Tweets_24E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "A_Gloeckner")
SPD_Tweets_25E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "Lothar_Binding")
SPD_Tweets_26E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "matthiasbartke")
SPD_Tweets_27E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "IsabelMackensen")
SPD_Tweets_28E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "KatjaMast")
SPD_Tweets_29E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "Kaiser_SPD")
SPD_Tweets_30E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "KorkmazGT")
SPD_Tweets_31E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "FalkoMohrs")
SPD_Tweets_32E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "baldy_daniel")
SPD_Tweets_33E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "nadjasthamer")
SPD_Tweets_34E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "Krawallstein")
SPD_Tweets_35E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "Lina_Seitzl")
SPD_Tweets_36E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "jessi_rosenthal")
SPD_Tweets_37E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "AnniKlose")
SPD_Tweets_38E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "El_KaWeh_")
SPD_Tweets_39E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "josephineortleb")
SPD_Tweets_40E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "FrankeEdgar")
SPD_Tweets_41E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "dieschmidt")
SPD_Tweets_42E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "jungeinberlin")
SPD_Tweets_43E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "CPetryMdB")
SPD_Tweets_44E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "SteffenSonja")
SPD_Tweets_45E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "fritzfelgentreu")
SPD_Tweets_46E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "mischrodi")
SPD_Tweets_47E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "UliGroetsch")
SPD_Tweets_48E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "larscastellucci")
SPD_Tweets_49E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "RitaHaglKehl")
SPD_Tweets_50E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "KlausMindrup")
SPD_Tweets_51E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "michael_thews")
SPD_Tweets_52E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "MuellerChemnitz")
SPD_Tweets_53E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "NielsAnnen")
SPD_Tweets_54E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "stonie_kiel")
SPD_Tweets_55E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "MarjaVoellers")
SPD_Tweets_56E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "LeniBreymaier")
SPD_Tweets_57E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "UlliNissen")
SPD_Tweets_58E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "MetinHakverdi")
SPD_Tweets_59E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "NinaScheer_SPD")
SPD_Tweets_60E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "SoenkeRix")
SPD_Tweets_61E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "HildeMattheis")
SPD_Tweets_62E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "baerbelbas")
SPD_Tweets_63E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "FrankSchwabe")
SPD_Tweets_64E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "BaerbelKofler")
SPD_Tweets_65E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "GaHeinrich")
SPD_Tweets_66E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "danielakolbe")
SPD_Tweets_67E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "CanselK")
SPD_Tweets_68E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "zierke")
SPD_Tweets_69E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "s_schwartze")
SPD_Tweets_70E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "Achim_P")
SPD_Tweets_71E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "soerenbartol")
SPD_Tweets_72E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "MechthildRawert")
SPD_Tweets_73E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "rischwasu")
SPD_Tweets_74E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "FlorianPost")
SPD_Tweets_75E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "sebast_hartmann")
SPD_Tweets_76E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "oezdemir_spd")
SPD_Tweets_77E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "DirkWieseSPD")
SPD_Tweets_78E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "UteVogt")
SPD_Tweets_79E<- SPD_TweetsE %>% filter(SPD_TweetsE$screen_name == "NilsSchmid")
T_SPD_1E <- paste0(SPD_Tweets_1E$text, collapse = " ")
T_SPD_2E <- paste0(SPD_Tweets_2E$text, collapse = " ")
T_SPD_3E <- paste0(SPD_Tweets_3E$text, collapse = " ")
T_SPD_4E <- paste0(SPD_Tweets_4E$text, collapse = " ")
T_SPD_5E <- paste0(SPD_Tweets_5E$text, collapse = " ")
T_SPD_6E <- paste0(SPD_Tweets_6E$text, collapse = " ")
T_SPD_7E <- paste0(SPD_Tweets_7E$text, collapse = " ")
T_SPD_8E <- paste0(SPD_Tweets_8E$text, collapse = " ")
T_SPD_9E <- paste0(SPD_Tweets_9E$text, collapse = " ")
T_SPD_10E <- paste0(SPD_Tweets_10E$text, collapse = " ")
T_SPD_11E <- paste0(SPD_Tweets_11E$text, collapse = " ")
T_SPD_12E <- paste0(SPD_Tweets_12E$text, collapse = " ")
T_SPD_13E <- paste0(SPD_Tweets_13E$text, collapse = " ")
T_SPD_14E <- paste0(SPD_Tweets_14E$text, collapse = " ")
T_SPD_15E <- paste0(SPD_Tweets_15E$text, collapse = " ")
T_SPD_16E <- paste0(SPD_Tweets_16E$text, collapse = " ")
T_SPD_17E <- paste0(SPD_Tweets_17E$text, collapse = " ")
T_SPD_18E <- paste0(SPD_Tweets_18E$text, collapse = " ")
T_SPD_19E <- paste0(SPD_Tweets_19E$text, collapse = " ")
T_SPD_20E <- paste0(SPD_Tweets_20E$text, collapse = " ")
T_SPD_21E <- paste0(SPD_Tweets_21E$text, collapse = " ")
T_SPD_22E <- paste0(SPD_Tweets_22E$text, collapse = " ")
T_SPD_23E <- paste0(SPD_Tweets_23E$text, collapse = " ")
T_SPD_24E <- paste0(SPD_Tweets_24E$text, collapse = " ")
T_SPD_25E <- paste0(SPD_Tweets_25E$text, collapse = " ")
T_SPD_26E <- paste0(SPD_Tweets_26E$text, collapse = " ")
T_SPD_27E <- paste0(SPD_Tweets_27E$text, collapse = " ")
T_SPD_28E <- paste0(SPD_Tweets_28E$text, collapse = " ")
T_SPD_29E <- paste0(SPD_Tweets_29E$text, collapse = " ")
T_SPD_30E <- paste0(SPD_Tweets_30E$text, collapse = " ")
T_SPD_31E <- paste0(SPD_Tweets_31E$text, collapse = " ")
T_SPD_32E <- paste0(SPD_Tweets_32E$text, collapse = " ")
T_SPD_33E <- paste0(SPD_Tweets_33E$text, collapse = " ")
T_SPD_34E <- paste0(SPD_Tweets_34E$text, collapse = " ")
T_SPD_35E <- paste0(SPD_Tweets_35E$text, collapse = " ")
T_SPD_36E <- paste0(SPD_Tweets_36E$text, collapse = " ")
T_SPD_37E <- paste0(SPD_Tweets_37E$text, collapse = " ")
T_SPD_38E <- paste0(SPD_Tweets_38E$text, collapse = " ")
T_SPD_39E <- paste0(SPD_Tweets_39E$text, collapse = " ")
T_SPD_40E <- paste0(SPD_Tweets_40E$text, collapse = " ")
T_SPD_41E <- paste0(SPD_Tweets_41E$text, collapse = " ")
T_SPD_42E <- paste0(SPD_Tweets_42E$text, collapse = " ")
T_SPD_43E <- paste0(SPD_Tweets_43E$text, collapse = " ")
T_SPD_44E <- paste0(SPD_Tweets_44E$text, collapse = " ")
T_SPD_45E <- paste0(SPD_Tweets_45E$text, collapse = " ")
T_SPD_46E <- paste0(SPD_Tweets_46E$text, collapse = " ")
T_SPD_47E <- paste0(SPD_Tweets_47E$text, collapse = " ")
T_SPD_48E <- paste0(SPD_Tweets_48E$text, collapse = " ")
T_SPD_49E <- paste0(SPD_Tweets_49E$text, collapse = " ")
T_SPD_50E <- paste0(SPD_Tweets_50E$text, collapse = " ")
T_SPD_51E <- paste0(SPD_Tweets_51E$text, collapse = " ")
T_SPD_52E <- paste0(SPD_Tweets_52E$text, collapse = " ")
T_SPD_53E <- paste0(SPD_Tweets_53E$text, collapse = " ")
T_SPD_54E <- paste0(SPD_Tweets_54E$text, collapse = " ")
T_SPD_55E <- paste0(SPD_Tweets_55E$text, collapse = " ")
T_SPD_56E <- paste0(SPD_Tweets_56E$text, collapse = " ")
T_SPD_57E <- paste0(SPD_Tweets_57E$text, collapse = " ")
T_SPD_58E <- paste0(SPD_Tweets_58E$text, collapse = " ")
T_SPD_59E <- paste0(SPD_Tweets_59E$text, collapse = " ")
T_SPD_60E <- paste0(SPD_Tweets_60E$text, collapse = " ")
T_SPD_61E <- paste0(SPD_Tweets_61E$text, collapse = " ")
T_SPD_62E <- paste0(SPD_Tweets_62E$text, collapse = " ")
T_SPD_63E <- paste0(SPD_Tweets_63E$text, collapse = " ")
T_SPD_64E <- paste0(SPD_Tweets_64E$text, collapse = " ")
T_SPD_65E <- paste0(SPD_Tweets_65E$text, collapse = " ")
T_SPD_66E <- paste0(SPD_Tweets_66E$text, collapse = " ")
T_SPD_67E <- paste0(SPD_Tweets_67E$text, collapse = " ")
T_SPD_68E <- paste0(SPD_Tweets_68E$text, collapse = " ")
T_SPD_69E <- paste0(SPD_Tweets_69E$text, collapse = " ")
T_SPD_70E <- paste0(SPD_Tweets_70E$text, collapse = " ")
T_SPD_71E <- paste0(SPD_Tweets_71E$text, collapse = " ")
T_SPD_72E <- paste0(SPD_Tweets_72E$text, collapse = " ")
T_SPD_73E <- paste0(SPD_Tweets_73E$text, collapse = " ")
T_SPD_74E <- paste0(SPD_Tweets_74E$text, collapse = " ")
T_SPD_75E <- paste0(SPD_Tweets_75E$text, collapse = " ")
T_SPD_76E <- paste0(SPD_Tweets_76E$text, collapse = " ")
T_SPD_77E <- paste0(SPD_Tweets_77E$text, collapse = " ")
T_SPD_78E <- paste0(SPD_Tweets_78E$text, collapse = " ")
T_SPD_79E <- paste0(SPD_Tweets_79E$text, collapse = " ")

T_SPD_TextsE <- c(T_SPD_1E,T_SPD_2E,T_SPD_3E,T_SPD_4E,T_SPD_5E,T_SPD_6E,T_SPD_7E,T_SPD_8E,T_SPD_9E,T_SPD_10E,
                  T_SPD_11E,T_SPD_12E,T_SPD_13E,T_SPD_14E,T_SPD_15E,T_SPD_16E,T_SPD_17E,T_SPD_18E,T_SPD_19E,T_SPD_20E,
                  T_SPD_21E,T_SPD_22E,T_SPD_23E,T_SPD_24E,T_SPD_25E,T_SPD_26E,T_SPD_27E,T_SPD_28E,T_SPD_29E,T_SPD_30E,
                  T_SPD_31E,T_SPD_32E,T_SPD_33E,T_SPD_34E,T_SPD_35E,T_SPD_36E,T_SPD_37E,T_SPD_38E,T_SPD_39E,T_SPD_40E,
                  T_SPD_41E,T_SPD_42E,T_SPD_43E,T_SPD_44E,T_SPD_45E,T_SPD_46E,T_SPD_47E,T_SPD_48E,T_SPD_49E,T_SPD_50E,
                  T_SPD_51E,T_SPD_52E,T_SPD_53E,T_SPD_54E,T_SPD_55E,T_SPD_56E,T_SPD_57E,T_SPD_58E,T_SPD_59E,T_SPD_60E,
                  T_SPD_61E,T_SPD_62E,T_SPD_63E,T_SPD_64E,T_SPD_65E,T_SPD_66E,T_SPD_67E,T_SPD_68E,T_SPD_69E,T_SPD_70E,
                  T_SPD_71E,T_SPD_72E,T_SPD_73E,T_SPD_74E,T_SPD_75E,T_SPD_76E,T_SPD_77E,T_SPD_78E,T_SPD_79E)
T_SPD_TextsE <- data.frame(T_SPD_TextsE)


###########################################################


################# IV. WORDSCORES ##########################
##### CORPUS
TextC_All <- c(PP_AfD_2017, PP_CDU_2017, PP_FDP_2017, PP_Gruene_2017, PP_Linke_2017, PP_SPD_2017,
               PP_AfD_2021, PP_CDU_2021, PP_FDP_2021, PP_Gruene_2021, PP_Linke_2021, PP_SPD_2021,
               T_AfD_1,T_AfD_2,T_AfD_3,T_AfD_4,T_AfD_5,T_AfD_6,T_AfD_7,T_AfD_8,T_AfD_9,T_AfD_10,
               T_AfD_11,T_AfD_12,T_AfD_13,T_AfD_14,T_AfD_15,T_AfD_16,T_AfD_17,T_AfD_18,T_AfD_19,T_AfD_20,
               T_AfD_21,T_AfD_22,T_AfD_23,T_AfD_24,T_AfD_25,T_AfD_26,T_AfD_27,T_AfD_28,T_AfD_29,T_AfD_30,
               T_AfD_31,T_AfD_32,T_AfD_33,T_AfD_34,T_AfD_35,T_AfD_36,T_AfD_37,T_AfD_38,T_AfD_39,T_AfD_40,
               T_AfD_41,T_AfD_42,T_AfD_43,T_AfD_44,T_AfD_45,T_AfD_46,T_AfD_47,T_AfD_48,
               T_CDU_1,T_CDU_2,T_CDU_3,T_CDU_4,T_CDU_5,T_CDU_6,T_CDU_7,T_CDU_8,T_CDU_9,T_CDU_10,
               T_CDU_11,T_CDU_12,T_CDU_13,T_CDU_14,T_CDU_15,T_CDU_16,T_CDU_17,T_CDU_18,T_CDU_19,T_CDU_20,
               T_CDU_21,T_CDU_22,T_CDU_23,T_CDU_24,T_CDU_25,T_CDU_26,T_CDU_27,T_CDU_28,T_CDU_29,T_CDU_30,
               T_CDU_31,T_CDU_32,T_CDU_33,T_CDU_34,T_CDU_35,T_CDU_36,T_CDU_37,T_CDU_38,T_CDU_39,T_CDU_40,
               T_CDU_41,T_CDU_42,T_CDU_43,T_CDU_44,T_CDU_45,T_CDU_46,T_CDU_47,T_CDU_48,T_CDU_49,T_CDU_50,
               T_CDU_51,T_CDU_52,T_CDU_53,T_CDU_54,T_CDU_55,T_CDU_56,T_CDU_57,T_CDU_58,T_CDU_59,T_CDU_60,
               T_CDU_61,T_CDU_62,T_CDU_63,T_CDU_64,T_CDU_65,T_CDU_66,T_CDU_67,T_CDU_68,T_CDU_69,T_CDU_70,
               T_CDU_71,T_CDU_72,T_CDU_73,T_CDU_74,T_CDU_75,T_CDU_76,T_CDU_77,T_CDU_78,T_CDU_79,T_CDU_80,
               T_CDU_81,T_CDU_82,T_CDU_83,T_CDU_84,T_CDU_85,T_CDU_86,
               T_FDP_1,T_FDP_2,T_FDP_3,T_FDP_4,T_FDP_5,T_FDP_6,T_FDP_7,T_FDP_8,T_FDP_9,T_FDP_10,
               T_FDP_11,T_FDP_12,T_FDP_13,T_FDP_14,T_FDP_15,T_FDP_16,T_FDP_17,T_FDP_18,T_FDP_19,T_FDP_20,
               T_FDP_21,T_FDP_22,T_FDP_23,T_FDP_24,T_FDP_25,T_FDP_26,T_FDP_27,T_FDP_28,T_FDP_29,T_FDP_30,
               T_FDP_31,T_FDP_32,T_FDP_33,T_FDP_34,T_FDP_35,T_FDP_36,T_FDP_37,T_FDP_38,T_FDP_39,T_FDP_40,
               T_FDP_41,T_FDP_42,T_FDP_43,T_FDP_44,T_FDP_45,T_FDP_46,T_FDP_47,T_FDP_48,T_FDP_49,T_FDP_50,
               T_FDP_51,T_FDP_52,T_FDP_53,T_FDP_54,T_FDP_55,T_FDP_56,T_FDP_57,T_FDP_58,T_FDP_59,T_FDP_60,
               T_FDP_61,T_FDP_62,T_FDP_63,T_FDP_64,T_FDP_65,T_FDP_66,T_FDP_67,
               T_Gruene_1,T_Gruene_2,T_Gruene_3,T_Gruene_4,T_Gruene_5,T_Gruene_6,T_Gruene_7,T_Gruene_8,T_Gruene_9,T_Gruene_10,
               T_Gruene_11,T_Gruene_12,T_Gruene_13,T_Gruene_14,T_Gruene_15,T_Gruene_16,T_Gruene_17,T_Gruene_18,T_Gruene_19,T_Gruene_20,
               T_Gruene_21,T_Gruene_22,T_Gruene_23,T_Gruene_24,T_Gruene_25,T_Gruene_26,T_Gruene_27,T_Gruene_28,T_Gruene_29,T_Gruene_30,
               T_Gruene_31,T_Gruene_32,T_Gruene_33,T_Gruene_34,T_Gruene_35,T_Gruene_36,T_Gruene_37,T_Gruene_38,T_Gruene_39,T_Gruene_40,
               T_Gruene_41,T_Gruene_42,T_Gruene_43,T_Gruene_44,T_Gruene_45,T_Gruene_46,T_Gruene_47,T_Gruene_48,T_Gruene_49,T_Gruene_50,
               T_Gruene_51,T_Gruene_52,T_Gruene_53,T_Gruene_54,T_Gruene_55,T_Gruene_56,T_Gruene_57,T_Gruene_58,T_Gruene_59,T_Gruene_60,
               T_Gruene_61,
               T_Linke_1,T_Linke_2,T_Linke_3,T_Linke_4,T_Linke_5,T_Linke_6,T_Linke_7,T_Linke_8,T_Linke_9,T_Linke_10,
               T_Linke_11,T_Linke_12,T_Linke_13,T_Linke_14,T_Linke_15,T_Linke_16,T_Linke_17,T_Linke_18,T_Linke_19,T_Linke_20,
               T_Linke_21,T_Linke_22,T_Linke_23,T_Linke_24,T_Linke_25,T_Linke_26,T_Linke_27,T_Linke_28,T_Linke_29,T_Linke_30,
               T_Linke_31,T_Linke_32,T_Linke_33,T_Linke_34,T_Linke_35,T_Linke_36,T_Linke_37,T_Linke_38,T_Linke_39,T_Linke_40,
               T_Linke_41,T_Linke_42,T_Linke_43,
               T_SPD_1,T_SPD_2,T_SPD_3,T_SPD_4,T_SPD_5,T_SPD_6,T_SPD_7,T_SPD_8,T_SPD_9,T_SPD_10,
               T_SPD_11,T_SPD_12,T_SPD_13,T_SPD_14,T_SPD_15,T_SPD_16,T_SPD_17,T_SPD_18,T_SPD_19,T_SPD_20,
               T_SPD_21,T_SPD_22,T_SPD_23,T_SPD_24,T_SPD_25,T_SPD_26,T_SPD_27,T_SPD_28,T_SPD_29,T_SPD_30,
               T_SPD_31,T_SPD_32,T_SPD_33,T_SPD_34,T_SPD_35,T_SPD_36,T_SPD_37,T_SPD_38,T_SPD_39,T_SPD_40,
               T_SPD_41,T_SPD_42,T_SPD_43,T_SPD_44,T_SPD_45,T_SPD_46,T_SPD_47,T_SPD_48,T_SPD_49,T_SPD_50,
               T_SPD_51,T_SPD_52,T_SPD_53,T_SPD_54,T_SPD_55,T_SPD_56,T_SPD_57,T_SPD_58,T_SPD_59,T_SPD_60,
               T_SPD_61,T_SPD_62,T_SPD_63,T_SPD_64,T_SPD_65,T_SPD_66,T_SPD_67,T_SPD_68,T_SPD_69,T_SPD_70,
               T_SPD_71,T_SPD_72,T_SPD_73,T_SPD_74,T_SPD_75,T_SPD_76,T_SPD_77,T_SPD_78,T_SPD_79)


TextC_AllC <- c(PP_AfD_2017, PP_CDU_2017, PP_FDP_2017, PP_Gruene_2017, PP_Linke_2017, PP_SPD_2017,
                PP_AfD_2021, PP_CDU_2021, PP_FDP_2021, PP_Gruene_2021, PP_Linke_2021, PP_SPD_2021,
                T_AfD_1C,T_AfD_2C,T_AfD_3C,T_AfD_4C,T_AfD_5C,T_AfD_6C,T_AfD_7C,T_AfD_8C,T_AfD_9C,T_AfD_10C,
                T_AfD_11C,T_AfD_12C,T_AfD_13C,T_AfD_14C,T_AfD_15C,T_AfD_16C,T_AfD_17C,T_AfD_18C,T_AfD_19C,T_AfD_20C,
                T_AfD_21C,T_AfD_22C,T_AfD_23C,T_AfD_24C,T_AfD_25C,T_AfD_26C,T_AfD_27C,T_AfD_28C,T_AfD_29C,T_AfD_30C,
                T_AfD_31C,T_AfD_32C,T_AfD_33C,T_AfD_34C,T_AfD_35C,T_AfD_36C,T_AfD_37C,T_AfD_38C,T_AfD_39C,T_AfD_40C,
                T_AfD_41C,T_AfD_42C,T_AfD_43C,T_AfD_44C,T_AfD_45C,T_AfD_46C,T_AfD_47C,T_AfD_48C,
                T_CDU_1C,T_CDU_2C,T_CDU_3C,T_CDU_4C,T_CDU_5C,T_CDU_6C,T_CDU_7C,T_CDU_8C,T_CDU_9C,T_CDU_10C,
                T_CDU_11C,T_CDU_12C,T_CDU_13C,T_CDU_14C,T_CDU_15C,T_CDU_16C,T_CDU_17C,T_CDU_18C,T_CDU_19C,T_CDU_20C,
                T_CDU_21C,T_CDU_22C,T_CDU_23C,T_CDU_24C,T_CDU_25C,T_CDU_26C,T_CDU_27C,T_CDU_28C,T_CDU_29C,T_CDU_30C,
                T_CDU_31C,T_CDU_32C,T_CDU_33C,T_CDU_34C,T_CDU_35C,T_CDU_36C,T_CDU_37C,T_CDU_38C,T_CDU_39C,T_CDU_40C,
                T_CDU_41C,T_CDU_42C,T_CDU_43C,T_CDU_44C,T_CDU_45C,T_CDU_46C,T_CDU_47C,T_CDU_48C,T_CDU_49C,T_CDU_50C,
                T_CDU_51C,T_CDU_52C,T_CDU_53C,T_CDU_54C,T_CDU_55C,T_CDU_56C,T_CDU_57C,T_CDU_58C,T_CDU_59C,T_CDU_60C,
                T_CDU_61C,T_CDU_62C,T_CDU_63C,T_CDU_64C,T_CDU_65C,T_CDU_66C,T_CDU_67C,T_CDU_68C,T_CDU_69C,T_CDU_70C,
                T_CDU_71C,T_CDU_72C,T_CDU_73C,T_CDU_74C,T_CDU_75C,T_CDU_76C,T_CDU_77C,T_CDU_78C,T_CDU_79C,T_CDU_80C,
                T_CDU_81C,T_CDU_82C,T_CDU_83C,T_CDU_84C,T_CDU_85C,T_CDU_86C,
                T_FDP_1C,T_FDP_2C,T_FDP_3C,T_FDP_4C,T_FDP_5C,T_FDP_6C,T_FDP_7C,T_FDP_8C,T_FDP_9C,T_FDP_10C,
                T_FDP_11C,T_FDP_12C,T_FDP_13C,T_FDP_14C,T_FDP_15C,T_FDP_16C,T_FDP_17C,T_FDP_18C,T_FDP_19C,T_FDP_20C,
                T_FDP_21C,T_FDP_22C,T_FDP_23C,T_FDP_24C,T_FDP_25C,T_FDP_26C,T_FDP_27C,T_FDP_28C,T_FDP_29C,T_FDP_30C,
                T_FDP_31C,T_FDP_32C,T_FDP_33C,T_FDP_34C,T_FDP_35C,T_FDP_36C,T_FDP_37C,T_FDP_38C,T_FDP_39C,T_FDP_40C,
                T_FDP_41C,T_FDP_42C,T_FDP_43C,T_FDP_44C,T_FDP_45C,T_FDP_46C,T_FDP_47C,T_FDP_48C,T_FDP_49C,T_FDP_50C,
                T_FDP_51C,T_FDP_52C,T_FDP_53C,T_FDP_54C,T_FDP_55C,T_FDP_56C,T_FDP_57C,T_FDP_58C,T_FDP_59C,T_FDP_60C,
                T_FDP_61C,T_FDP_62C,T_FDP_63C,T_FDP_64C,T_FDP_65C,T_FDP_66C,T_FDP_67C,
                T_Gruene_1C,T_Gruene_2C,T_Gruene_3C,T_Gruene_4C,T_Gruene_5C,T_Gruene_6C,T_Gruene_7C,T_Gruene_8C,T_Gruene_9C,T_Gruene_10C,
                T_Gruene_11C,T_Gruene_12C,T_Gruene_13C,T_Gruene_14C,T_Gruene_15C,T_Gruene_16C,T_Gruene_17C,T_Gruene_18C,T_Gruene_19C,T_Gruene_20C,
                T_Gruene_21C,T_Gruene_22C,T_Gruene_23C,T_Gruene_24C,T_Gruene_25C,T_Gruene_26C,T_Gruene_27C,T_Gruene_28C,T_Gruene_29C,T_Gruene_30C,
                T_Gruene_31C,T_Gruene_32C,T_Gruene_33C,T_Gruene_34C,T_Gruene_35C,T_Gruene_36C,T_Gruene_37C,T_Gruene_38C,T_Gruene_39C,T_Gruene_40C,
                T_Gruene_41C,T_Gruene_42C,T_Gruene_43C,T_Gruene_44C,T_Gruene_45C,T_Gruene_46C,T_Gruene_47C,T_Gruene_48C,T_Gruene_49C,T_Gruene_50C,
                T_Gruene_51C,T_Gruene_52C,T_Gruene_53C,T_Gruene_54C,T_Gruene_55C,T_Gruene_56C,T_Gruene_57C,T_Gruene_58C,T_Gruene_59C,T_Gruene_60C,
                T_Gruene_61C,
                T_Linke_1C,T_Linke_2C,T_Linke_3C,T_Linke_4C,T_Linke_5C,T_Linke_6C,T_Linke_7C,T_Linke_8C,T_Linke_9C,T_Linke_10C,
                T_Linke_11C,T_Linke_12C,T_Linke_13C,T_Linke_14C,T_Linke_15C,T_Linke_16C,T_Linke_17C,T_Linke_18C,T_Linke_19C,T_Linke_20C,
                T_Linke_21C,T_Linke_22C,T_Linke_23C,T_Linke_24C,T_Linke_25C,T_Linke_26C,T_Linke_27C,T_Linke_28C,T_Linke_29C,T_Linke_30C,
                T_Linke_31C,T_Linke_32C,T_Linke_33C,T_Linke_34C,T_Linke_35C,T_Linke_36C,T_Linke_37C,T_Linke_38C,T_Linke_39C,T_Linke_40C,
                T_Linke_41C,T_Linke_42C,T_Linke_43C,
                T_SPD_1C,T_SPD_2C,T_SPD_3C,T_SPD_4C,T_SPD_5C,T_SPD_6C,T_SPD_7C,T_SPD_8C,T_SPD_9C,T_SPD_10C,
                T_SPD_11C,T_SPD_12C,T_SPD_13C,T_SPD_14C,T_SPD_15C,T_SPD_16C,T_SPD_17C,T_SPD_18C,T_SPD_19C,T_SPD_20C,
                T_SPD_21C,T_SPD_22C,T_SPD_23C,T_SPD_24C,T_SPD_25C,T_SPD_26C,T_SPD_27C,T_SPD_28C,T_SPD_29C,T_SPD_30C,
                T_SPD_31C,T_SPD_32C,T_SPD_33C,T_SPD_34C,T_SPD_35C,T_SPD_36C,T_SPD_37C,T_SPD_38C,T_SPD_39C,T_SPD_40C,
                T_SPD_41C,T_SPD_42C,T_SPD_43C,T_SPD_44C,T_SPD_45C,T_SPD_46C,T_SPD_47C,T_SPD_48C,T_SPD_49C,T_SPD_50C,
                T_SPD_51C,T_SPD_52C,T_SPD_53C,T_SPD_54C,T_SPD_55C,T_SPD_56C,T_SPD_57C,T_SPD_58C,T_SPD_59C,T_SPD_60C,
                T_SPD_61C,T_SPD_62C,T_SPD_63C,T_SPD_64C,T_SPD_65C,T_SPD_66C,T_SPD_67C,T_SPD_68C,T_SPD_69C,T_SPD_70C,
                T_SPD_71C,T_SPD_72C,T_SPD_73C,T_SPD_74C,T_SPD_75C,T_SPD_76C,T_SPD_77C,T_SPD_78C,T_SPD_79C)


TextC_AllE <- c(PP_AfD_2017, PP_CDU_2017, PP_FDP_2017, PP_Gruene_2017, PP_Linke_2017, PP_SPD_2017,
                PP_AfD_2021, PP_CDU_2021, PP_FDP_2021, PP_Gruene_2021, PP_Linke_2021, PP_SPD_2021,
                T_AfD_1E,T_AfD_2E,T_AfD_3E,T_AfD_4E,T_AfD_5E,T_AfD_6E,T_AfD_7E,T_AfD_8E,T_AfD_9E,T_AfD_10E,
                T_AfD_11E,T_AfD_12E,T_AfD_13E,T_AfD_14E,T_AfD_15E,T_AfD_16E,T_AfD_17E,T_AfD_18E,T_AfD_19E,T_AfD_20E,
                T_AfD_21E,T_AfD_22E,T_AfD_23E,T_AfD_24E,T_AfD_25E,T_AfD_26E,T_AfD_27E,T_AfD_28E,T_AfD_29E,T_AfD_30E,
                T_AfD_31E,T_AfD_32E,T_AfD_33E,T_AfD_34E,T_AfD_35E,T_AfD_36E,T_AfD_37E,T_AfD_38E,T_AfD_39E,T_AfD_40E,
                T_AfD_41E,T_AfD_42E,T_AfD_43E,T_AfD_44E,T_AfD_45E,T_AfD_46E,T_AfD_47E,T_AfD_48E,
                T_CDU_1E,T_CDU_2E,T_CDU_3E,T_CDU_4E,T_CDU_5E,T_CDU_6E,T_CDU_7E,T_CDU_8E,T_CDU_9E,T_CDU_10E,
                T_CDU_11E,T_CDU_12E,T_CDU_13E,T_CDU_14E,T_CDU_15E,T_CDU_16E,T_CDU_17E,T_CDU_18E,T_CDU_19E,T_CDU_20E,
                T_CDU_21E,T_CDU_22E,T_CDU_23E,T_CDU_24E,T_CDU_25E,T_CDU_26E,T_CDU_27E,T_CDU_28E,T_CDU_29E,T_CDU_30E,
                T_CDU_31E,T_CDU_32E,T_CDU_33E,T_CDU_34E,T_CDU_35E,T_CDU_36E,T_CDU_37E,T_CDU_38E,T_CDU_39E,T_CDU_40E,
                T_CDU_41E,T_CDU_42E,T_CDU_43E,T_CDU_44E,T_CDU_45E,T_CDU_46E,T_CDU_47E,T_CDU_48E,T_CDU_49E,T_CDU_50E,
                T_CDU_51E,T_CDU_52E,T_CDU_53E,T_CDU_54E,T_CDU_55E,T_CDU_56E,T_CDU_57E,T_CDU_58E,T_CDU_59E,T_CDU_60E,
                T_CDU_61E,T_CDU_62E,T_CDU_63E,T_CDU_64E,T_CDU_65E,T_CDU_66E,T_CDU_67E,T_CDU_68E,T_CDU_69E,T_CDU_70E,
                T_CDU_71E,T_CDU_72E,T_CDU_73E,T_CDU_74E,T_CDU_75E,T_CDU_76E,T_CDU_77E,T_CDU_78E,T_CDU_79E,T_CDU_80E,
                T_CDU_81E,T_CDU_82E,T_CDU_83E,T_CDU_84E,T_CDU_85E,T_CDU_86E,
                T_FDP_1E,T_FDP_2E,T_FDP_3E,T_FDP_4E,T_FDP_5E,T_FDP_6E,T_FDP_7E,T_FDP_8E,T_FDP_9E,T_FDP_10E,
                T_FDP_11E,T_FDP_12E,T_FDP_13E,T_FDP_14E,T_FDP_15E,T_FDP_16E,T_FDP_17E,T_FDP_18E,T_FDP_19E,T_FDP_20E,
                T_FDP_21E,T_FDP_22E,T_FDP_23E,T_FDP_24E,T_FDP_25E,T_FDP_26E,T_FDP_27E,T_FDP_28E,T_FDP_29E,T_FDP_30E,
                T_FDP_31E,T_FDP_32E,T_FDP_33E,T_FDP_34E,T_FDP_35E,T_FDP_36E,T_FDP_37E,T_FDP_38E,T_FDP_39E,T_FDP_40E,
                T_FDP_41E,T_FDP_42E,T_FDP_43E,T_FDP_44E,T_FDP_45E,T_FDP_46E,T_FDP_47E,T_FDP_48E,T_FDP_49E,T_FDP_50E,
                T_FDP_51E,T_FDP_52E,T_FDP_53E,T_FDP_54E,T_FDP_55E,T_FDP_56E,T_FDP_57E,T_FDP_58E,T_FDP_59E,T_FDP_60E,
                T_FDP_61E,T_FDP_62E,T_FDP_63E,T_FDP_64E,T_FDP_65E,T_FDP_66E,T_FDP_67E,
                T_Gruene_1E,T_Gruene_2E,T_Gruene_3E,T_Gruene_4E,T_Gruene_5E,T_Gruene_6E,T_Gruene_7E,T_Gruene_8E,T_Gruene_9E,T_Gruene_10E,
                T_Gruene_11E,T_Gruene_12E,T_Gruene_13E,T_Gruene_14E,T_Gruene_15E,T_Gruene_16E,T_Gruene_17E,T_Gruene_18E,T_Gruene_19E,T_Gruene_20E,
                T_Gruene_21E,T_Gruene_22E,T_Gruene_23E,T_Gruene_24E,T_Gruene_25E,T_Gruene_26E,T_Gruene_27E,T_Gruene_28E,T_Gruene_29E,T_Gruene_30E,
                T_Gruene_31E,T_Gruene_32E,T_Gruene_33E,T_Gruene_34E,T_Gruene_35E,T_Gruene_36E,T_Gruene_37E,T_Gruene_38E,T_Gruene_39E,T_Gruene_40E,
                T_Gruene_41E,T_Gruene_42E,T_Gruene_43E,T_Gruene_44E,T_Gruene_45E,T_Gruene_46E,T_Gruene_47E,T_Gruene_48E,T_Gruene_49E,T_Gruene_50E,
                T_Gruene_51E,T_Gruene_52E,T_Gruene_53E,T_Gruene_54E,T_Gruene_55E,T_Gruene_56E,T_Gruene_57E,T_Gruene_58E,T_Gruene_59E,T_Gruene_60E,
                T_Gruene_61E,
                T_Linke_1E,T_Linke_2E,T_Linke_3E,T_Linke_4E,T_Linke_5E,T_Linke_6E,T_Linke_7E,T_Linke_8E,T_Linke_9E,T_Linke_10E,
                T_Linke_11E,T_Linke_12E,T_Linke_13E,T_Linke_14E,T_Linke_15E,T_Linke_16E,T_Linke_17E,T_Linke_18E,T_Linke_19E,T_Linke_20E,
                T_Linke_21E,T_Linke_22E,T_Linke_23E,T_Linke_24E,T_Linke_25E,T_Linke_26E,T_Linke_27E,T_Linke_28E,T_Linke_29E,T_Linke_30E,
                T_Linke_31E,T_Linke_32E,T_Linke_33E,T_Linke_34E,T_Linke_35E,T_Linke_36E,T_Linke_37E,T_Linke_38E,T_Linke_39E,T_Linke_40E,
                T_Linke_41E,T_Linke_42E,T_Linke_43E,
                T_SPD_1E,T_SPD_2E,T_SPD_3E,T_SPD_4E,T_SPD_5E,T_SPD_6E,T_SPD_7E,T_SPD_8E,T_SPD_9E,T_SPD_10E,
                T_SPD_11E,T_SPD_12E,T_SPD_13E,T_SPD_14E,T_SPD_15E,T_SPD_16E,T_SPD_17E,T_SPD_18E,T_SPD_19E,T_SPD_20E,
                T_SPD_21E,T_SPD_22E,T_SPD_23E,T_SPD_24E,T_SPD_25E,T_SPD_26E,T_SPD_27E,T_SPD_28E,T_SPD_29E,T_SPD_30E,
                T_SPD_31E,T_SPD_32E,T_SPD_33E,T_SPD_34E,T_SPD_35E,T_SPD_36E,T_SPD_37E,T_SPD_38E,T_SPD_39E,T_SPD_40E,
                T_SPD_41E,T_SPD_42E,T_SPD_43E,T_SPD_44E,T_SPD_45E,T_SPD_46E,T_SPD_47E,T_SPD_48E,T_SPD_49E,T_SPD_50E,
                T_SPD_51E,T_SPD_52E,T_SPD_53E,T_SPD_54E,T_SPD_55E,T_SPD_56E,T_SPD_57E,T_SPD_58E,T_SPD_59E,T_SPD_60E,
                T_SPD_61E,T_SPD_62E,T_SPD_63E,T_SPD_64E,T_SPD_65E,T_SPD_66E,T_SPD_67E,T_SPD_68E,T_SPD_69E,T_SPD_70E,
                T_SPD_71E,T_SPD_72E,T_SPD_73E,T_SPD_74E,T_SPD_75E,T_SPD_76E,T_SPD_77E,T_SPD_78E,T_SPD_79E)


TextC_AllC <- data.frame(TextC_AllC)
TextC_AllE <- data.frame(TextC_AllE)


CH_RefScoreC_All <- c("9.238", "6.524", "6.429", "3.238", "1.429", "3.619",
                      "NA", "NA", "NA", "NA", "NA", "NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA","NA",
                      "NA","NA","NA","NA","NA","NA","NA","NA","NA")
CH_RefScoreC_All <- as.numeric(CH_RefScoreC_All)
Election_CorpusC_All <- corpus(TextC_All$TextC_All)
Election_CorpusC_All$CH_RefScore <- CH_RefScoreC_All
quanteda::docnames(Election_CorpusC_All)<- c(
  "PP AfD 2017", "PP CDU 2017", "PP FDP 2017", "PP Gruene 2017", "PP Linke 2017", "PP SPD 2017",
  "PP AfD 2021", "PP CDU 2021", "PP FDP 2021", "PP Gruene 2021", "PP Linke 2021", "PP SPD 2021",
  "T_AfD_1","T_AfD_2","T_AfD_3","T_AfD_4","T_AfD_5","T_AfD_6","T_AfD_7","T_AfD_8","T_AfD_9","T_AfD_10",
  "T_AfD_11","T_AfD_12","T_AfD_13","T_AfD_14","T_AfD_15","T_AfD_16","T_AfD_17","T_AfD_18","T_AfD_19","T_AfD_20",
  "T_AfD_21","T_AfD_22","T_AfD_23","T_AfD_24","T_AfD_25","T_AfD_26","T_AfD_27","T_AfD_28","T_AfD_29","T_AfD_30",
  "T_AfD_31","T_AfD_32","T_AfD_33","T_AfD_34","T_AfD_35","T_AfD_36","T_AfD_37","T_AfD_38","T_AfD_39","T_AfD_40",
  "T_AfD_41","T_AfD_42","T_AfD_43","T_AfD_44","T_AfD_45","T_AfD_46","T_AfD_47","T_AfD_48",
  "T_CDU_1","T_CDU_2","T_CDU_3","T_CDU_4","T_CDU_5","T_CDU_6","T_CDU_7","T_CDU_8","T_CDU_9","T_CDU_10",
  "T_CDU_11","T_CDU_12","T_CDU_13","T_CDU_14","T_CDU_15","T_CDU_16","T_CDU_17","T_CDU_18","T_CDU_19","T_CDU_20",
  "T_CDU_21","T_CDU_22","T_CDU_23","T_CDU_24","T_CDU_25","T_CDU_26","T_CDU_27","T_CDU_28","T_CDU_29","T_CDU_30",
  "T_CDU_31","T_CDU_32","T_CDU_33","T_CDU_34","T_CDU_35","T_CDU_36","T_CDU_37","T_CDU_38","T_CDU_39","T_CDU_40",
  "T_CDU_41","T_CDU_42","T_CDU_43","T_CDU_44","T_CDU_45","T_CDU_46","T_CDU_47","T_CDU_48","T_CDU_49","T_CDU_50",
  "T_CDU_51","T_CDU_52","T_CDU_53","T_CDU_54","T_CDU_55","T_CDU_56","T_CDU_57","T_CDU_58","T_CDU_59","T_CDU_60",
  "T_CDU_61","T_CDU_62","T_CDU_63","T_CDU_64","T_CDU_65","T_CDU_66","T_CDU_67","T_CDU_68","T_CDU_69","T_CDU_70",
  "T_CDU_71","T_CDU_72","T_CDU_73","T_CDU_74","T_CDU_75","T_CDU_76","T_CDU_77","T_CDU_78","T_CDU_79","T_CDU_80",
  "T_CDU_81","T_CDU_82","T_CDU_83","T_CDU_84","T_CDU_85","T_CDU_86",
  "T_FDP_1","T_FDP_2","T_FDP_3","T_FDP_4","T_FDP_5","T_FDP_6","T_FDP_7","T_FDP_8","T_FDP_9","T_FDP_10",
  "T_FDP_11","T_FDP_12","T_FDP_13","T_FDP_14","T_FDP_15","T_FDP_16","T_FDP_17","T_FDP_18","T_FDP_19","T_FDP_20",
  "T_FDP_21","T_FDP_22","T_FDP_23","T_FDP_24","T_FDP_25","T_FDP_26","T_FDP_27","T_FDP_28","T_FDP_29","T_FDP_30",
  "T_FDP_31","T_FDP_32","T_FDP_33","T_FDP_34","T_FDP_35","T_FDP_36","T_FDP_37","T_FDP_38","T_FDP_39","T_FDP_40",
  "T_FDP_41","T_FDP_42","T_FDP_43","T_FDP_44","T_FDP_45","T_FDP_46","T_FDP_47","T_FDP_48","T_FDP_49","T_FDP_50",
  "T_FDP_51","T_FDP_52","T_FDP_53","T_FDP_54","T_FDP_55","T_FDP_56","T_FDP_57","T_FDP_58","T_FDP_59","T_FDP_60",
  "T_FDP_61","T_FDP_62","T_FDP_63","T_FDP_64","T_FDP_65","T_FDP_66","T_FDP_67",
  "T_Gruene_1","T_Gruene_2","T_Gruene_3","T_Gruene_4","T_Gruene_5","T_Gruene_6","T_Gruene_7","T_Gruene_8","T_Gruene_9","T_Gruene_10",
  "T_Gruene_11","T_Gruene_12","T_Gruene_13","T_Gruene_14","T_Gruene_15","T_Gruene_16","T_Gruene_17","T_Gruene_18","T_Gruene_19","T_Gruene_20",
  "T_Gruene_21","T_Gruene_22","T_Gruene_23","T_Gruene_24","T_Gruene_25","T_Gruene_26","T_Gruene_27","T_Gruene_28","T_Gruene_29","T_Gruene_30",
  "T_Gruene_31","T_Gruene_32","T_Gruene_33","T_Gruene_34","T_Gruene_35","T_Gruene_36","T_Gruene_37","T_Gruene_38","T_Gruene_39","T_Gruene_40",
  "T_Gruene_41","T_Gruene_42","T_Gruene_43","T_Gruene_44","T_Gruene_45","T_Gruene_46","T_Gruene_47","T_Gruene_48","T_Gruene_49","T_Gruene_50",
  "T_Gruene_51","T_Gruene_52","T_Gruene_53","T_Gruene_54","T_Gruene_55","T_Gruene_56","T_Gruene_57","T_Gruene_58","T_Gruene_59","T_Gruene_60",
  "T_Gruene_61",
  "T_Linke_1","T_Linke_2","T_Linke_3","T_Linke_4","T_Linke_5","T_Linke_6","T_Linke_7","T_Linke_8","T_Linke_9","T_Linke_10",
  "T_Linke_11","T_Linke_12","T_Linke_13","T_Linke_14","T_Linke_15","T_Linke_16","T_Linke_17","T_Linke_18","T_Linke_19","T_Linke_20",
  "T_Linke_21","T_Linke_22","T_Linke_23","T_Linke_24","T_Linke_25","T_Linke_26","T_Linke_27","T_Linke_28","T_Linke_29","T_Linke_30",
  "T_Linke_31","T_Linke_32","T_Linke_33","T_Linke_34","T_Linke_35","T_Linke_36","T_Linke_37","T_Linke_38","T_Linke_39","T_Linke_40",
  "T_Linke_41","T_Linke_42","T_Linke_43",
  "T_SPD_1","T_SPD_2","T_SPD_3","T_SPD_4","T_SPD_5","T_SPD_6","T_SPD_7","T_SPD_8","T_SPD_9","T_SPD_10",
  "T_SPD_11","T_SPD_12","T_SPD_13","T_SPD_14","T_SPD_15","T_SPD_16","T_SPD_17","T_SPD_18","T_SPD_19","T_SPD_20",
  "T_SPD_21","T_SPD_22","T_SPD_23","T_SPD_24","T_SPD_25","T_SPD_26","T_SPD_27","T_SPD_28","T_SPD_29","T_SPD_30",
  "T_SPD_31","T_SPD_32","T_SPD_33","T_SPD_34","T_SPD_35","T_SPD_36","T_SPD_37","T_SPD_38","T_SPD_39","T_SPD_40",
  "T_SPD_41","T_SPD_42","T_SPD_43","T_SPD_44","T_SPD_45","T_SPD_46","T_SPD_47","T_SPD_48","T_SPD_49","T_SPD_50",
  "T_SPD_51","T_SPD_52","T_SPD_53","T_SPD_54","T_SPD_55","T_SPD_56","T_SPD_57","T_SPD_58","T_SPD_59","T_SPD_60",
  "T_SPD_61","T_SPD_62","T_SPD_63","T_SPD_64","T_SPD_65","T_SPD_66","T_SPD_67","T_SPD_68","T_SPD_69","T_SPD_70",
  "T_SPD_71","T_SPD_72","T_SPD_73","T_SPD_74","T_SPD_75","T_SPD_76","T_SPD_77","T_SPD_78","T_SPD_79")


Election_CorpusC_AllC <- corpus(TextC_AllC$TextC_AllC)
Election_CorpusC_AllC$CH_RefScore <- CH_RefScoreC_All
quanteda::docnames(Election_CorpusC_AllC)<- c(
  "PP AfD 2017", "PP CDU 2017", "PP FDP 2017", "PP Gruene 2017", "PP Linke 2017", "PP SPD 2017",
  "PP AfD 2021", "PP CDU 2021", "PP FDP 2021", "PP Gruene 2021", "PP Linke 2021", "PP SPD 2021",
  "T_AfD_1","T_AfD_2","T_AfD_3","T_AfD_4","T_AfD_5","T_AfD_6","T_AfD_7","T_AfD_8","T_AfD_9","T_AfD_10",
  "T_AfD_11","T_AfD_12","T_AfD_13","T_AfD_14","T_AfD_15","T_AfD_16","T_AfD_17","T_AfD_18","T_AfD_19","T_AfD_20",
  "T_AfD_21","T_AfD_22","T_AfD_23","T_AfD_24","T_AfD_25","T_AfD_26","T_AfD_27","T_AfD_28","T_AfD_29","T_AfD_30",
  "T_AfD_31","T_AfD_32","T_AfD_33","T_AfD_34","T_AfD_35","T_AfD_36","T_AfD_37","T_AfD_38","T_AfD_39","T_AfD_40",
  "T_AfD_41","T_AfD_42","T_AfD_43","T_AfD_44","T_AfD_45","T_AfD_46","T_AfD_47","T_AfD_48",
  "T_CDU_1","T_CDU_2","T_CDU_3","T_CDU_4","T_CDU_5","T_CDU_6","T_CDU_7","T_CDU_8","T_CDU_9","T_CDU_10",
  "T_CDU_11","T_CDU_12","T_CDU_13","T_CDU_14","T_CDU_15","T_CDU_16","T_CDU_17","T_CDU_18","T_CDU_19","T_CDU_20",
  "T_CDU_21","T_CDU_22","T_CDU_23","T_CDU_24","T_CDU_25","T_CDU_26","T_CDU_27","T_CDU_28","T_CDU_29","T_CDU_30",
  "T_CDU_31","T_CDU_32","T_CDU_33","T_CDU_34","T_CDU_35","T_CDU_36","T_CDU_37","T_CDU_38","T_CDU_39","T_CDU_40",
  "T_CDU_41","T_CDU_42","T_CDU_43","T_CDU_44","T_CDU_45","T_CDU_46","T_CDU_47","T_CDU_48","T_CDU_49","T_CDU_50",
  "T_CDU_51","T_CDU_52","T_CDU_53","T_CDU_54","T_CDU_55","T_CDU_56","T_CDU_57","T_CDU_58","T_CDU_59","T_CDU_60",
  "T_CDU_61","T_CDU_62","T_CDU_63","T_CDU_64","T_CDU_65","T_CDU_66","T_CDU_67","T_CDU_68","T_CDU_69","T_CDU_70",
  "T_CDU_71","T_CDU_72","T_CDU_73","T_CDU_74","T_CDU_75","T_CDU_76","T_CDU_77","T_CDU_78","T_CDU_79","T_CDU_80",
  "T_CDU_81","T_CDU_82","T_CDU_83","T_CDU_84","T_CDU_85","T_CDU_86",
  "T_FDP_1","T_FDP_2","T_FDP_3","T_FDP_4","T_FDP_5","T_FDP_6","T_FDP_7","T_FDP_8","T_FDP_9","T_FDP_10",
  "T_FDP_11","T_FDP_12","T_FDP_13","T_FDP_14","T_FDP_15","T_FDP_16","T_FDP_17","T_FDP_18","T_FDP_19","T_FDP_20",
  "T_FDP_21","T_FDP_22","T_FDP_23","T_FDP_24","T_FDP_25","T_FDP_26","T_FDP_27","T_FDP_28","T_FDP_29","T_FDP_30",
  "T_FDP_31","T_FDP_32","T_FDP_33","T_FDP_34","T_FDP_35","T_FDP_36","T_FDP_37","T_FDP_38","T_FDP_39","T_FDP_40",
  "T_FDP_41","T_FDP_42","T_FDP_43","T_FDP_44","T_FDP_45","T_FDP_46","T_FDP_47","T_FDP_48","T_FDP_49","T_FDP_50",
  "T_FDP_51","T_FDP_52","T_FDP_53","T_FDP_54","T_FDP_55","T_FDP_56","T_FDP_57","T_FDP_58","T_FDP_59","T_FDP_60",
  "T_FDP_61","T_FDP_62","T_FDP_63","T_FDP_64","T_FDP_65","T_FDP_66","T_FDP_67",
  "T_Gruene_1","T_Gruene_2","T_Gruene_3","T_Gruene_4","T_Gruene_5","T_Gruene_6","T_Gruene_7","T_Gruene_8","T_Gruene_9","T_Gruene_10",
  "T_Gruene_11","T_Gruene_12","T_Gruene_13","T_Gruene_14","T_Gruene_15","T_Gruene_16","T_Gruene_17","T_Gruene_18","T_Gruene_19","T_Gruene_20",
  "T_Gruene_21","T_Gruene_22","T_Gruene_23","T_Gruene_24","T_Gruene_25","T_Gruene_26","T_Gruene_27","T_Gruene_28","T_Gruene_29","T_Gruene_30",
  "T_Gruene_31","T_Gruene_32","T_Gruene_33","T_Gruene_34","T_Gruene_35","T_Gruene_36","T_Gruene_37","T_Gruene_38","T_Gruene_39","T_Gruene_40",
  "T_Gruene_41","T_Gruene_42","T_Gruene_43","T_Gruene_44","T_Gruene_45","T_Gruene_46","T_Gruene_47","T_Gruene_48","T_Gruene_49","T_Gruene_50",
  "T_Gruene_51","T_Gruene_52","T_Gruene_53","T_Gruene_54","T_Gruene_55","T_Gruene_56","T_Gruene_57","T_Gruene_58","T_Gruene_59","T_Gruene_60",
  "T_Gruene_61",
  "T_Linke_1","T_Linke_2","T_Linke_3","T_Linke_4","T_Linke_5","T_Linke_6","T_Linke_7","T_Linke_8","T_Linke_9","T_Linke_10",
  "T_Linke_11","T_Linke_12","T_Linke_13","T_Linke_14","T_Linke_15","T_Linke_16","T_Linke_17","T_Linke_18","T_Linke_19","T_Linke_20",
  "T_Linke_21","T_Linke_22","T_Linke_23","T_Linke_24","T_Linke_25","T_Linke_26","T_Linke_27","T_Linke_28","T_Linke_29","T_Linke_30",
  "T_Linke_31","T_Linke_32","T_Linke_33","T_Linke_34","T_Linke_35","T_Linke_36","T_Linke_37","T_Linke_38","T_Linke_39","T_Linke_40",
  "T_Linke_41","T_Linke_42","T_Linke_43",
  "T_SPD_1","T_SPD_2","T_SPD_3","T_SPD_4","T_SPD_5","T_SPD_6","T_SPD_7","T_SPD_8","T_SPD_9","T_SPD_10",
  "T_SPD_11","T_SPD_12","T_SPD_13","T_SPD_14","T_SPD_15","T_SPD_16","T_SPD_17","T_SPD_18","T_SPD_19","T_SPD_20",
  "T_SPD_21","T_SPD_22","T_SPD_23","T_SPD_24","T_SPD_25","T_SPD_26","T_SPD_27","T_SPD_28","T_SPD_29","T_SPD_30",
  "T_SPD_31","T_SPD_32","T_SPD_33","T_SPD_34","T_SPD_35","T_SPD_36","T_SPD_37","T_SPD_38","T_SPD_39","T_SPD_40",
  "T_SPD_41","T_SPD_42","T_SPD_43","T_SPD_44","T_SPD_45","T_SPD_46","T_SPD_47","T_SPD_48","T_SPD_49","T_SPD_50",
  "T_SPD_51","T_SPD_52","T_SPD_53","T_SPD_54","T_SPD_55","T_SPD_56","T_SPD_57","T_SPD_58","T_SPD_59","T_SPD_60",
  "T_SPD_61","T_SPD_62","T_SPD_63","T_SPD_64","T_SPD_65","T_SPD_66","T_SPD_67","T_SPD_68","T_SPD_69","T_SPD_70",
  "T_SPD_71","T_SPD_72","T_SPD_73","T_SPD_74","T_SPD_75","T_SPD_76","T_SPD_77","T_SPD_78","T_SPD_79")


Election_CorpusC_AllE <- corpus(TextC_AllE$TextC_AllE)
Election_CorpusC_AllE$CH_RefScore <- CH_RefScoreC_All
quanteda::docnames(Election_CorpusC_AllE)<- c(
  "PP AfD 2017", "PP CDU 2017", "PP FDP 2017", "PP Gruene 2017", "PP Linke 2017", "PP SPD 2017",
  "PP AfD 2021", "PP CDU 2021", "PP FDP 2021", "PP Gruene 2021", "PP Linke 2021", "PP SPD 2021",
  "T_AfD_1","T_AfD_2","T_AfD_3","T_AfD_4","T_AfD_5","T_AfD_6","T_AfD_7","T_AfD_8","T_AfD_9","T_AfD_10",
  "T_AfD_11","T_AfD_12","T_AfD_13","T_AfD_14","T_AfD_15","T_AfD_16","T_AfD_17","T_AfD_18","T_AfD_19","T_AfD_20",
  "T_AfD_21","T_AfD_22","T_AfD_23","T_AfD_24","T_AfD_25","T_AfD_26","T_AfD_27","T_AfD_28","T_AfD_29","T_AfD_30",
  "T_AfD_31","T_AfD_32","T_AfD_33","T_AfD_34","T_AfD_35","T_AfD_36","T_AfD_37","T_AfD_38","T_AfD_39","T_AfD_40",
  "T_AfD_41","T_AfD_42","T_AfD_43","T_AfD_44","T_AfD_45","T_AfD_46","T_AfD_47","T_AfD_48",
  "T_CDU_1","T_CDU_2","T_CDU_3","T_CDU_4","T_CDU_5","T_CDU_6","T_CDU_7","T_CDU_8","T_CDU_9","T_CDU_10",
  "T_CDU_11","T_CDU_12","T_CDU_13","T_CDU_14","T_CDU_15","T_CDU_16","T_CDU_17","T_CDU_18","T_CDU_19","T_CDU_20",
  "T_CDU_21","T_CDU_22","T_CDU_23","T_CDU_24","T_CDU_25","T_CDU_26","T_CDU_27","T_CDU_28","T_CDU_29","T_CDU_30",
  "T_CDU_31","T_CDU_32","T_CDU_33","T_CDU_34","T_CDU_35","T_CDU_36","T_CDU_37","T_CDU_38","T_CDU_39","T_CDU_40",
  "T_CDU_41","T_CDU_42","T_CDU_43","T_CDU_44","T_CDU_45","T_CDU_46","T_CDU_47","T_CDU_48","T_CDU_49","T_CDU_50",
  "T_CDU_51","T_CDU_52","T_CDU_53","T_CDU_54","T_CDU_55","T_CDU_56","T_CDU_57","T_CDU_58","T_CDU_59","T_CDU_60",
  "T_CDU_61","T_CDU_62","T_CDU_63","T_CDU_64","T_CDU_65","T_CDU_66","T_CDU_67","T_CDU_68","T_CDU_69","T_CDU_70",
  "T_CDU_71","T_CDU_72","T_CDU_73","T_CDU_74","T_CDU_75","T_CDU_76","T_CDU_77","T_CDU_78","T_CDU_79","T_CDU_80",
  "T_CDU_81","T_CDU_82","T_CDU_83","T_CDU_84","T_CDU_85","T_CDU_86",
  "T_FDP_1","T_FDP_2","T_FDP_3","T_FDP_4","T_FDP_5","T_FDP_6","T_FDP_7","T_FDP_8","T_FDP_9","T_FDP_10",
  "T_FDP_11","T_FDP_12","T_FDP_13","T_FDP_14","T_FDP_15","T_FDP_16","T_FDP_17","T_FDP_18","T_FDP_19","T_FDP_20",
  "T_FDP_21","T_FDP_22","T_FDP_23","T_FDP_24","T_FDP_25","T_FDP_26","T_FDP_27","T_FDP_28","T_FDP_29","T_FDP_30",
  "T_FDP_31","T_FDP_32","T_FDP_33","T_FDP_34","T_FDP_35","T_FDP_36","T_FDP_37","T_FDP_38","T_FDP_39","T_FDP_40",
  "T_FDP_41","T_FDP_42","T_FDP_43","T_FDP_44","T_FDP_45","T_FDP_46","T_FDP_47","T_FDP_48","T_FDP_49","T_FDP_50",
  "T_FDP_51","T_FDP_52","T_FDP_53","T_FDP_54","T_FDP_55","T_FDP_56","T_FDP_57","T_FDP_58","T_FDP_59","T_FDP_60",
  "T_FDP_61","T_FDP_62","T_FDP_63","T_FDP_64","T_FDP_65","T_FDP_66","T_FDP_67",
  "T_Gruene_1","T_Gruene_2","T_Gruene_3","T_Gruene_4","T_Gruene_5","T_Gruene_6","T_Gruene_7","T_Gruene_8","T_Gruene_9","T_Gruene_10",
  "T_Gruene_11","T_Gruene_12","T_Gruene_13","T_Gruene_14","T_Gruene_15","T_Gruene_16","T_Gruene_17","T_Gruene_18","T_Gruene_19","T_Gruene_20",
  "T_Gruene_21","T_Gruene_22","T_Gruene_23","T_Gruene_24","T_Gruene_25","T_Gruene_26","T_Gruene_27","T_Gruene_28","T_Gruene_29","T_Gruene_30",
  "T_Gruene_31","T_Gruene_32","T_Gruene_33","T_Gruene_34","T_Gruene_35","T_Gruene_36","T_Gruene_37","T_Gruene_38","T_Gruene_39","T_Gruene_40",
  "T_Gruene_41","T_Gruene_42","T_Gruene_43","T_Gruene_44","T_Gruene_45","T_Gruene_46","T_Gruene_47","T_Gruene_48","T_Gruene_49","T_Gruene_50",
  "T_Gruene_51","T_Gruene_52","T_Gruene_53","T_Gruene_54","T_Gruene_55","T_Gruene_56","T_Gruene_57","T_Gruene_58","T_Gruene_59","T_Gruene_60",
  "T_Gruene_61",
  "T_Linke_1","T_Linke_2","T_Linke_3","T_Linke_4","T_Linke_5","T_Linke_6","T_Linke_7","T_Linke_8","T_Linke_9","T_Linke_10",
  "T_Linke_11","T_Linke_12","T_Linke_13","T_Linke_14","T_Linke_15","T_Linke_16","T_Linke_17","T_Linke_18","T_Linke_19","T_Linke_20",
  "T_Linke_21","T_Linke_22","T_Linke_23","T_Linke_24","T_Linke_25","T_Linke_26","T_Linke_27","T_Linke_28","T_Linke_29","T_Linke_30",
  "T_Linke_31","T_Linke_32","T_Linke_33","T_Linke_34","T_Linke_35","T_Linke_36","T_Linke_37","T_Linke_38","T_Linke_39","T_Linke_40",
  "T_Linke_41","T_Linke_42","T_Linke_43",
  "T_SPD_1","T_SPD_2","T_SPD_3","T_SPD_4","T_SPD_5","T_SPD_6","T_SPD_7","T_SPD_8","T_SPD_9","T_SPD_10",
  "T_SPD_11","T_SPD_12","T_SPD_13","T_SPD_14","T_SPD_15","T_SPD_16","T_SPD_17","T_SPD_18","T_SPD_19","T_SPD_20",
  "T_SPD_21","T_SPD_22","T_SPD_23","T_SPD_24","T_SPD_25","T_SPD_26","T_SPD_27","T_SPD_28","T_SPD_29","T_SPD_30",
  "T_SPD_31","T_SPD_32","T_SPD_33","T_SPD_34","T_SPD_35","T_SPD_36","T_SPD_37","T_SPD_38","T_SPD_39","T_SPD_40",
  "T_SPD_41","T_SPD_42","T_SPD_43","T_SPD_44","T_SPD_45","T_SPD_46","T_SPD_47","T_SPD_48","T_SPD_49","T_SPD_50",
  "T_SPD_51","T_SPD_52","T_SPD_53","T_SPD_54","T_SPD_55","T_SPD_56","T_SPD_57","T_SPD_58","T_SPD_59","T_SPD_60",
  "T_SPD_61","T_SPD_62","T_SPD_63","T_SPD_64","T_SPD_65","T_SPD_66","T_SPD_67","T_SPD_68","T_SPD_69","T_SPD_70",
  "T_SPD_71","T_SPD_72","T_SPD_73","T_SPD_74","T_SPD_75","T_SPD_76","T_SPD_77","T_SPD_78","T_SPD_79")


##### DOCUMENT-FEATURE MATRIX (DFM)
Election_ToksC_All <- quanteda::tokens(Election_CorpusC_All, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_remove(pattern = stopwords("de")) %>% 
  tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex")
Election_DFMC_All <- dfm(Election_ToksC_All) %>%
  dfm_remove(pattern = stopwords("de")) %>%
  dfm_remove(pattern = G_Stop_Words)
Election_WSC_All <- textmodel_wordscores(Election_DFMC_All, y = Election_CorpusC_All$CH_RefScore, smooth = 1)


textstat_frequency(Election_DFMC_All)
summary(Election_WSC_All)
Election_WordscoresC_All <- Election_WSC_All$wordscores
sort(Election_WordscoresC_All, decreasing = TRUE)
itail(Election_WordscoresC_All)

topfeatures(Election_DFMC_All, n=20)

Election_PredC_All <- predict(Election_WSC_All, se.fit = TRUE, newdata = Election_DFMC_All)
textplot_scale1d(Election_PredC_All)


#####


Election_ToksC_AllC <- quanteda::tokens(Election_CorpusC_AllC, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_remove(pattern = stopwords("de")) %>% 
  tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex")
Election_DFMC_AllC <- dfm(Election_ToksC_AllC) %>%
  dfm_remove(pattern = stopwords("de")) %>%
  dfm_remove(pattern = G_Stop_Words)
Election_WSC_AllC <- textmodel_wordscores(Election_DFMC_AllC, y = Election_CorpusC_AllC$CH_RefScore, smooth = 1)

topfeatures(Election_DFMC_AllC, n=20)

Election_PredC_AllC <- predict(Election_WSC_AllC, se.fit = TRUE, newdata = Election_DFMC_AllC)
textplot_scale1d(Election_PredC_AllC)


#####


Election_ToksC_AllE <- quanteda::tokens(Election_CorpusC_AllE, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_remove(pattern = stopwords("de")) %>% 
  tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex")
Election_DFMC_AllE <- dfm(Election_ToksC_AllE) %>%
  dfm_remove(pattern = stopwords("de")) %>%
  dfm_remove(pattern = G_Stop_Words)
Election_WSC_AllE <- textmodel_wordscores(Election_DFMC_AllE, y = Election_CorpusC_AllE$CH_RefScore, smooth = 1)


topfeatures(Election_DFMC_AllE, n=20)

Election_PredC_AllE <- predict(Election_WSC_AllE, se.fit = TRUE, newdata = Election_DFMC_AllE)
textplot_scale1d(Election_PredC_AllE)


##### MV Transformation
Party_2021_RobustWSC <- predict(Election_WSC_All,
                                se.fit = TRUE,
                                interval = "confidence",
                                level = 0.95,
                                rescaling = "mv")
textplot_scale1d(Party_2021_RobustWSC, margin = "documents", sort = TRUE)


#####


Party_2021_RobustWSCC <- predict(Election_WSC_AllC,
                                 se.fit = TRUE,
                                 interval = "confidence",
                                 level = 0.95,
                                 rescaling = "mv")
textplot_scale1d(Party_2021_RobustWSCC, margin = "documents", sort = TRUE)
write.csv(Party_2021_RobustWSCC, "Party_2021_RobustWSCC.csv")
Textscore_DataC <- read.csv(file = 'Party_2021_RobustWSCC.csv', sep = ",", fileEncoding = "UTF-8")


#####


Party_2021_RobustWSCE <- predict(Election_WSC_AllE,
                                 se.fit = TRUE,
                                 interval = "confidence",
                                 level = 0.95,
                                 rescaling = "mv")
textplot_scale1d(Party_2021_RobustWSCE, margin = "documents", sort = TRUE)
write.csv(Party_2021_RobustWSCE, "Party_2021_RobustWSCE.csv")
Textscore_DataE <- read.csv(file = 'Party_2021_RobustWSCE.csv', sep = ",", fileEncoding = "UTF-8")


#####


write.csv(Party_2021_RobustWSC, "Party_2021_RobustWSC.csv")
Textscore_Data <- read.csv(file = 'Party_2021_RobustWSC.csv', sep = ",", fileEncoding = "UTF-8")
Textscore_Data <- as.data.frame(Textscore_Data)
Textscore_Data <- mutate(Textscore_Data, type = case_when(
  grepl("PP", Textscore_Data$X) ~ 'program',
  grepl("T_", Textscore_Data$X) ~ 'tweets'))
Textscore_Data <- mutate(Textscore_Data, type2 = case_when(
  grepl("2017", Textscore_Data$X) ~ 'reference',
  grepl("2021|T_", Textscore_Data$X) ~ 'virgin'))
Textscore_Data <- mutate(Textscore_Data, group = case_when(
  grepl("AfD", Textscore_Data$X) ~ 'AfD',
  grepl("CDU", Textscore_Data$X) ~ 'CDU/CSU',
  grepl("FDP", Textscore_Data$X) ~ 'FDP',
  grepl("Gruene", Textscore_Data$X) ~ 'Gruene',
  grepl("Linke", Textscore_Data$X) ~ 'Linke',
  grepl("SPD", Textscore_Data$X) ~ 'SPD'))
Textscore_Data <- mutate(Textscore_Data, group2 = case_when(
  grepl("FDP|SPD", Textscore_Data$X) ~ 'winner',
  grepl("AfD|Gruene", Textscore_Data$X) ~ 'inconclusive',
  grepl("CDU|Linke", Textscore_Data$X) ~ 'loser'))


Textscore_Means <- Textscore_Data %>%
  filter(type == "tweets" & type2 == "virgin") %>%
  group_by(group) %>%
  summarise("Observations" = n(),
            "Avg. Textscore" = mean(fit.fit),
            "Avg. Textscore (low)" = mean(fit.lwr),
            "Avg. Textscore (high)" = mean(fit.upr),
            "Avg. Standard Error" = mean(se.fit))
Textscore_Means
Textscore_Means2 <- Textscore_Data %>%
  filter(type == "tweets" & type2 == "virgin") %>%
  group_by(group2) %>%
  summarise("Observations" = n(),
            "Avg. Textscore" = mean(fit.fit),
            "Avg. Textscore (low)" = mean(fit.lwr),
            "Avg. Textscore (high)" = mean(fit.upr),
            "Avg. Standard Error" = mean(se.fit))
Textscore_Means2


Textscore_Data <- mutate(Textscore_Data, group_mean = case_when(
  grepl("AfD", Textscore_Data$X) ~ 6.370060,
  grepl("CDU", Textscore_Data$X) ~ 6.044256,
  grepl("FDP", Textscore_Data$X) ~ 6.062865,
  grepl("Gruene", Textscore_Data$X) ~ 5.625735,
  grepl("Linke", Textscore_Data$X) ~ 5.472004,
  grepl("SPD", Textscore_Data$X) ~ 5.803173))
Textscore_Data <- mutate(Textscore_Data, mean_diff = (fit.fit - group_mean)^2)
Textscore_Data$mean_diff <- as.numeric(Textscore_Data$mean_diff)


Textscore_Data <- mutate(Textscore_Data, group_prg = case_when(
  grepl("AfD", Textscore_Data$X) ~ 7.125377,
  grepl("CDU", Textscore_Data$X) ~ 5.085992,
  grepl("FDP", Textscore_Data$X) ~ 6.035960,
  grepl("Gruene", Textscore_Data$X) ~ 4.136916,
  grepl("Linke", Textscore_Data$X) ~ 2.810644,
  grepl("SPD", Textscore_Data$X) ~ 4.054489))
Textscore_Data <- mutate(Textscore_Data, prgtwe_diff = (fit.fit - group_prg)^2)
Textscore_Data$prgtwe_diff <- as.numeric(Textscore_Data$prgtwe_diff)


Textscore_Data_Merge <- merge(Textscore_Data, Textscore_DataC, by = "X")
Textscore_Data_Merge <- merge(Textscore_Data_Merge, Textscore_DataE, by = "X")

Textscore_Data_Merge <- mutate(Textscore_Data_Merge,
                               twecordiff.fit = (fit.fitC - fit.fit)^2, is.na = TRUE)
Textscore_Data_Merge <- mutate(Textscore_Data_Merge,
                               twecordiff.lwr = (fit.lwrC - fit.lwr)^2, is.na = TRUE)
Textscore_Data_Merge <- mutate(Textscore_Data_Merge,
                               twecordiff.upr = (fit.uprC - fit.upr)^2, is.na = TRUE)
Textscore_Data_Merge <- mutate(Textscore_Data_Merge,
                               tweenvdiff.fit = (fit.fitE - fit.fit)^2, is.na = TRUE)
Textscore_Data_Merge <- mutate(Textscore_Data_Merge,
                               tweenvdiff.lwr = (fit.lwrE - fit.lwr)^2, is.na = TRUE)
Textscore_Data_Merge <- mutate(Textscore_Data_Merge,
                               tweenvdiff.upr = (fit.uprE - fit.upr)^2, is.na = TRUE)


###########################################################


################# V. DIFFERENCE TESTING ###################
Textscore_Data2 <- Textscore_Data %>% filter(type == "tweets" & type2 == "virgin")
MeanDiff_Means <- Textscore_Data2 %>%
  group_by(group) %>%
  summarise("Observations" = n(),
            "Avg. Textscore" = mean(fit.fit),
            "Avg. Standard Error" = mean(se.fit),
            "Avg. Mean Diff" = mean(mean_diff))
MeanDiff_Means


##### Pre-Test
leveneTest(prgtwe_diff ~ group, data = Textscore_Data2)
aov1 <- aov(prgtwe_diff ~ group, data = Textscore_Data2)
summary(aov1)
aov1$coefficients
oneway.test(prgtwe_diff ~ group, data = Textscore_Data2, var.equal = FALSE)


##### Hypothesis 1
########## M1
leveneTest(prgtwe_diff ~ group2, data = Textscore_Data2)
aov2 <- aov(prgtwe_diff ~ group2, data = Textscore_Data2)
summary(aov2)
TukeyHSD(aov2)


########## M2
aov3 <- aov(mean_diff ~ group, data = Textscore_Data2)
summary(aov3)
aov3$coefficients
aov4 <- aov(mean_diff ~ group2, data = Textscore_Data2)
summary(aov4)
TukeyHSD(aov4)


######


Textscore_Data_Merge2 <- Textscore_Data_Merge %>% filter(type == "tweets" & type2 == "virgin")
MeanDiff_Means2 <- Textscore_Data_Merge2 %>%
  group_by(group) %>%
  drop_na(twecordiff.lwr) %>%
  summarise("Observations" = n(),
            "Avg. Textscore" = mean(fit.fit),
            "Avg. Textscore (lwr)" = mean(fit.lwr),
            "Avg. Textscore (upr)" = mean(fit.upr),
            "Avg. SE" = mean(se.fit),
            "Avg. Textscore (Cor)" = mean(fit.fitC),
            "Avg. Textscore (lwr) C" = mean(fit.lwrC),
            "Avg. Textscore (upr) C" = mean(fit.uprC),
            "Avg. SE C" = mean(se.fitC),
            "Avg. Textscore (Env)" = mean(fit.fitE),
            "Avg. Textscore (lwr) E" = mean(fit.lwrE),
            "Avg. Textscore (upr) E" = mean(fit.uprE),
            "Avg. SE E" = mean(se.fitE),
            "Avg. Mean Diff" = mean(mean_diff),
            "Avg. Ovr-Cor Diff" = mean(twecordiff.fit),
            "Avg. Ovr-Env Diff" = mean(tweenvdiff.fit))
view(MeanDiff_Means2)


##### Hypothesis 2
Textscore_Data_Merge3 <- Textscore_Data_Merge2 %>%
  drop_na(twecordiff.lwr)
Textscore_Data_Merge3b <- Textscore_Data_Merge2 %>%
  drop_na(twecordiff.lwr) %>%
  filter(group == "SPD" | group == "FDP" | group == "CDU/CSU" | group == "Linke")

leveneTest(tweenvdiff.fit ~ group2, data = Textscore_Data_Merge4)
aov5 <- aov(tweenvdiff.fit ~ group, data = Textscore_Data_Merge4)
summary(aov5)
TukeyHSD(aov5)
aov5$coefficients
aov6 <- aov(tweenvdiff.fit ~ group2, data = Textscore_Data_Merge4)
aov6$coefficients
summary(aov6)
TukeyHSD(aov6)

oneway.test(twecordiff.fit ~ group2, data = Textscore_Data_Merge3, var.equal = TRUE)
summary(aov6b)


##### Hypothesis 3
Textscore_Data_Merge4 <- Textscore_Data_Merge2 %>%
  drop_na(tweenvdiff.lwr)


## M1
aov9 <- aov(fit.fitE ~ group, data = Textscore_Data_Merge4)
summary(aov9)
aov9$coefficients
aov10 <- aov(fit.fitE ~ group2, data = Textscore_Data_Merge4)
summary(aov10)
TukeyHSD(aov10)


#####


# WORDS - LEFT TO RIGHT
Election_Wordscores <- Election_WS$wordscores
Election_Wordscores <- sort(Election_Wordscores, decreasing = TRUE)
Election_Wordscores_MostLeft <- tail(Election_Wordscores, n = 20L)
Election_Wordscores_MostRight <- head(Election_Wordscores, n = 20L)
Election_Wordscores_MostLeft
Election_Wordscores_MostRight


Election_Wordscores_Map <- textplot_scale1d(Election_WS, margin = "features", sort = TRUE,
                                            highlighted = c("linke", "menschen", "bundestag",
                                                            "afd", "bundestagswahl", "wahlprogramm"), 
                                            highlighted_color = "blue4")
Election_Wordscores_Map


Party_2021_DFM <- dfm_subset(Election_DFM,
                             c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                               TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                               TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))


Election_Features <- topfeatures(Party_2021_DFM,
                                 n = 20, decreasing = TRUE, scheme = c("count"))
Election_Features


Party_Features <- topfeatures(Party_2021_DFM,
                              n = 50, decreasing = TRUE, scheme = c("count"),
                              groups = c("AfD", "CDU/CSU", "FDP", "The Greens", "The Left", "SPD",
                                         "AfD", "CDU/CSU", "FDP", "The Greens", "The Left", "SPD"))
Party_Features


###########################################################


################# VI. VISUALS #############################
CandStance <- Textscore_Data_Merge2 %>%
  ggplot( aes(x=group, y=fit.fit, fill=group)) +
  geom_violin() +
  labs(x = NULL, y = NULL) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("") + theme_gray() +
  coord_flip()
CandStance


Textscore_Data_Merge4 %>%
  ggplot(aes(x=reorder(group, fit.fitE, FUN = median), y=fit.fitE, fill=group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#666666", "#666666", "#dddddd",
                               "#666666", "#666666", "#dddddd")) +
  theme_grey() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) + xlab("Party") + ylab("Left-to-Right Position") +
  geom_jitter(size=0.7,width=0.1,alpha=0.2) +
  coord_flip()


ggplot(Textscore_Data2, aes(group, fit.fit)) +
  geom_jitter() + theme_grey() +
  labs(x = NULL, y = NULL) +
  scale_fill_brewer(palette="Greys")