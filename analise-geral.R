setwd("~/Documents/analise-twitter")

# Install and load mongo package
#install.packages("mongolite")
#install.packages("abjutils")
#install.packages("wordcloud")
#install.packages("wesanderson") # Paleta de cores
library(mongolite)
library(stringr)
library(tibble)
library(tm)
library(abjutils)
library(tidytext)
library(dplyr)
library(readr)
library(wordcloud)
library(wesanderson)

# Conecting to mongoDB
m <- mongo(url = "mongodb://192.168.16.2/", options = ssl_options(weak_cert_validation = T), db="twitter", collection="tweets_geral")

#Getting tweets from database and saving on R variable
tweets <- m$find(
  fields = '{"_id": false, "text": true}',
  limit = 100
)

# stop words
stop_words <- read_delim("stopwords.csv", ";", escape_double = FALSE, trim_ws = TRUE)
stop_words <- add_row(stop_words, word = "to")
stop_words <- add_row(stop_words, word = "tô")
stop_words <- add_row(stop_words, word = "ta")
stop_words <- add_row(stop_words, word = "tá")
stop_words <- add_row(stop_words, word = "q")
stop_words <- add_row(stop_words, word = "pq")
stop_words <- add_row(stop_words, word = "ah")
stop_words <- add_row(stop_words, word = "ai")
stop_words <- add_row(stop_words, word = "s")
stop_words <- add_row(stop_words, word = "x")
stop_words <- add_row(stop_words, word = "tt")
stop_words <- add_row(stop_words, word = "tto")
stop_words <- add_row(stop_words, word = "tts")
stop_words <- add_row(stop_words, word = "vc")
stop_words <- add_row(stop_words, word = "2155")
stop_words <- add_row(stop_words, word = regex("[kkk*]"))
stop_words <- add_row(stop_words, word = "msm")
stop_words <- add_row(stop_words, word = "mto")
stop_words <- add_row(stop_words, word = "oq")
stop_words <- add_row(stop_words, word = "p")
stop_words <- add_row(stop_words, word = "811")
stop_words <- add_row(stop_words, word = "vai")
stop_words <- add_row(stop_words, word = "vem")
stop_words <- add_row(stop_words, word = "sei")
stop_words <- add_row(stop_words, word = "sai")
stop_words <- add_row(stop_words, word = "fala")
stop_words <- add_row(stop_words, word = "fez")




# Removing urls
patternRegexUrl <- 'https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*)'
tweets$text <- str_replace(tweets$text, patternRegexUrl, "") 

# Removing @users reference from text
tweets$text <- str_replace_all(tweets$text,  "@\\w+", "") %>%
  removePunctuation() %>% # REMOVE PONTUAÇÃO
  stripWhitespace() # REMOVE ESPAÇOS EM BRANCO E PALAVRAS ACENTUADAS

# Removing emojis
# tweets$text <- gsub("[^\x01-\x7F]", "", tweets$text)

# SEPARA POR PARLAVRA E REMOVE AS STOPWORDS
tokens_tweets <- tweets %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tokens_tweets$word = str_replace_all(tokens_tweets$word, " ", "")

# CONTA AS PALAVRAS
tokens_tweets_count <- tokens_tweets %>%
  count(word, sort = TRUE)

#Aumenta a paleta de cores
numRegister <- 15
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(numRegister)


# Cria o WordCount
png("graficos/wordcloud_geral.png")
wordcloud(tokens_tweets_count$word, 
          tokens_tweets_count$n, 
          max.words = 10, 
          scale = c(3 ,.2),
          rot.per=0.1,
          color=brewer.pal(8, "Dark2"))
dev.off()

