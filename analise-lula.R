# Author: Cesar Vinicius <cesarvinici@gmail.com>
# Data : 24/11/2019
# Código R realizado no trabalho final da disciplina de Análise de Textos com R
# Foram gerados alguns insights utilizando dados do Twitter
# As informações foram coletadas utilizando a linguagem Python e salvos em um banco de dados MongoDB
# E recuperadas usando R

setwd("~/Documents/analise-twitter")

#install.packages("mongolite")
#install.packages("abjutils")
#install.packages("wordcloud")
#install.packages("wesanderson") # Paleta de cores
#install.packages("RColorBrewer")
# install.packages("rmarkdown")


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
library(tidyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(RColorBrewer)


# Conecting to mongoDB
m <- mongo(url = "mongodb://192.168.16.3/", options = ssl_options(weak_cert_validation = T), db="twitter", collection="tweets_lula")

#Getting tweets from database and saving on R variable
tweets <- m$find(
  fields = '{"_id": false, "text": true}',
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
stop_words <- add_row(stop_words, word = "pode")
stop_words <- add_row(stop_words, word = "agr")
stop_words <- add_row(stop_words, word = "falar")
stop_words <- add_row(stop_words, word = "não")
stop_words <- add_row(stop_words, word = "vão")
stop_words <- add_row(stop_words, word = "vou")
stop_words <- add_row(stop_words, word = "dois")
stop_words <- add_row(stop_words, word = "quer")
stop_words <- add_row(stop_words, word = "né")
stop_words <- add_row(stop_words, word = "ver")
stop_words <- add_row(stop_words, word = "viu")
stop_words <- add_row(stop_words, word = "nao")
stop_words <- add_row(stop_words, word = "leon")
stop_words <- add_row(stop_words, word = "K k")
stop_words <- add_row(stop_words, word = "dia")
stop_words <- add_row(stop_words, word = "ficar")
stop_words <- add_row(stop_words, word = "quero")
stop_words <- add_row(stop_words, word = "hj")
stop_words <- add_row(stop_words, word = "faz")
stop_words <- add_row(stop_words, word = "coisa")
stop_words <- add_row(stop_words, word = "dia")
stop_words <- add_row(stop_words, word = "fazer")
stop_words <- add_row(stop_words, word = "perfil")
stop_words <- add_row(stop_words, word = "cara")
stop_words <- add_row(stop_words, word = "acho")
stop_words <- add_row(stop_words, word = "bom")
stop_words <- add_row(stop_words, word = "c")
stop_words <- add_row(stop_words, word = "dar")
stop_words <- add_row(stop_words, word = "k")
stop_words <- add_row(stop_words, word = "eh")
stop_words <- add_row(stop_words, word = "vcs")
stop_words <- add_row(stop_words, word = "cu")

# stop_words <- add_row(stop_words, word = "kk")
# stop_words <- add_row(stop_words, word = "kkk")
# stop_words <- add_row(stop_words, word = "kkkk")
# stop_words <- add_row(stop_words, word = "kkkkk")
# stop_words <- add_row(stop_words, word = "kkkkkk")
# stop_words <- add_row(stop_words, word = "kkkkkkk")
# stop_words <- add_row(stop_words, word = "kkkkkkkk")
# stop_words <- add_row(stop_words, word = "kkkkkkkkk")
# stop_words <- add_row(stop_words, word = "kkkkkkkkkk")
# stop_words <- add_row(stop_words, word = "kkkkkkkkkkk")
# stop_words <- add_row(stop_words, word = "kkkkkkkkkkkk")

affin_pt <- read_delim("affin_pt.csv",
                       ";",escape_double = FALSE, trim_ws = TRUE)




# Removing urls
patternRegexUrl <- 'https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*)'
tweets$text <- str_replace(tweets$text, patternRegexUrl, "") 
patternRegexKs <- 'k{2,}'
tweets$text <- str_replace(tweets$text, patternRegexKs, "")


# Removing @users reference from text
tweets$text <- str_replace_all(tweets$text,  "@\\w+", "") %>%
              removePunctuation() %>% # REMOVE PONTUAÇÃO
              stripWhitespace() # REMOVE ESPAÇOS EM BRANCO E PALAVRAS ACENTUADAS

# Removing emojis
# tweets$text <- gsub("[^\x01-\x7F]", "", tweets$text)

# SEPARA POR PARLAVRA E REMOVE AS STOPWORDS
tokens_tweets <- tweets %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tokens_tweets$word = str_replace_all(tokens_tweets$word, " ", "")
  
# CONTA AS PALAVRAS
tokens_tweets_count <- tokens_tweets %>%
  count(word, sort = TRUE)

#Aumenta a paleta de cores
nb.cols <- 20
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

# Cria um Wordcloud
png("graficos/wordcloud.png", width = 400, height = 400)
wordcloud(tokens_tweets_count$word, 
          tokens_tweets_count$n, 
          max.words =50, 
          scale = c(8,.3),
          rot.per=0.1,
          color=mycolors)
dev.off()

# Plotando palavras mais usadas
tokens_tweets_count %>%
  mutate(word = reorder(word,n)) %>%
  filter(word != "lula") %>%
  head(10) %>%
  ggplot(aes(word,n,fill=factor(word)))+
  scale_fill_manual(values = mycolors)+
  geom_col()+
  xlab("Palavras")+
  ylab("Quantidade") +
  coord_flip()


# CRIANDO UM BIGRAMA
bigrams <- tweets %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% # Separa as palavras de 2 em 2
  separate(bigram,c("word1","word2"),sep = " ") %>% # Divide entre palavra 1 e palavra 2
  filter(!word1 %in% as.vector(t(stop_words$word))) %>%
  filter(!word2 %in% as.vector(t(stop_words$word))) %>% # Filtra as palavras a partir das stop_words
  unite(bigram,word1,word2,sep = " ") %>%
  count(bigram, sort = TRUE)

bigrams %>%
  mutate(bigram = reorder(bigram,n)) %>%
  head(20) %>%
  ggplot(aes(bigram,n,fill=factor(bigram)))+
  scale_fill_manual(values = mycolors)+
  geom_col()+
  xlab(NULL)+
  coord_flip()

bigrams_graph <- bigrams %>%
  separate(bigram,c("word1","word2"), sep = " ") %>%
  filter(n>5) %>%
  graph_from_data_frame()

# CREATE TRIGAMS
trigrams <- tweets %>%
  unnest_tokens(trigrams, text, token = "ngrams", n = 3) %>% # Separa as palavras de 2 em 2
  separate(trigrams,c("word1","word2", "word3"),sep = " ") %>% # Divide entre palavra 1 e palavra 2
  filter(!word1 %in% as.vector(t(stop_words$word))) %>%
  #filter(!word2 %in% as.vector(t(stopwords_pt$word))) %>% # Filtra as palavras a partir das stop_words
  filter(!word3 %in% as.vector(t(stop_words$word))) %>%
  unite(trigrams,word1,word2, word3,sep = " ") %>%
  count(trigrams, sort = TRUE)

trigrams %>%
  mutate(trigrams = reorder(trigrams,n)) %>%
  head(9) %>%
  ggplot(aes(trigrams,n,fill=factor(trigrams)))+
  scale_fill_manual(values = mycolors) +
  geom_col()+
  xlab(NULL)+
  coord_flip()

# CREATE TRIGAMS
quadrams <- tweets %>%
  unnest_tokens(quadrams, text, token = "ngrams", n = 4) %>% # Separa as palavras de 2 em 2
  separate(quadrams,c("word1","word2", "word3", "word4"),sep = " ") %>% # Divide entre palavra 1 e palavra 2
  filter(!word1 %in% as.vector(t(stop_words$word))) %>%
  #filter(!word2 %in% as.vector(t(stopwords_pt$word))) %>% # Filtra as palavras a partir das stop_words
  filter(!word4 %in% as.vector(t(stop_words$word))) %>%
  unite(quadrams,word1,word2, word3, word4,sep = " ") %>%
  count(quadrams, sort = TRUE)

quadrams %>%
  mutate(quadrams = reorder(quadrams,n)) %>%
  head(9) %>%
  ggplot(aes(quadrams,n,fill=factor(quadrams)))+
  scale_fill_manual(values = mycolors) +
  geom_col()+
  xlab(NULL)+
  coord_flip()


# Analise de Sentimentos (verifica a diferença entre palavras negaticas e positivas)
affin <- tokens_tweets %>%
  inner_join(affin_pt) %>%
  count(index=linenumber %/% 25, sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  mutate(sentiment = positivo - negativo) #%>%

# Cria um gráfico
ggplot(affin, aes(index,sentiment))+
  geom_col(show.legend = TRUE)

positivo <- sum(affin['positivo'])
negativo <- sum(affin['negativo'])
dados <- c('Positivas' = positivo, 'Negativas' = negativo)
barplot(dados, 
        main='Diferença entre palavradas dadas como "Positivas" e "Negativas" ',
        ylab="Qtd. Palavras",
        xlab ="Sentimento" )


dados.frame <- data.frame(dados)


