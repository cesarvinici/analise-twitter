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
#install.packages("tm")
#install.packages("igraph")
#install.packages("ggraph")
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
library(scales)
library(tidyr)

# Conecting to mongoDB
tweets_lula <- mongo(url = "mongodb://172.18.0.3/", options = ssl_options(weak_cert_validation = T), db="twitter", collection="tweets_lula")
tweets_bolsonaro <- mongo(url = "mongodb://172.18.0.3/", options = ssl_options(weak_cert_validation = T), db="twitter", collection="tweets_bolsonaro")
tweets_geral <- mongo(url = "mongodb://172.18.0.3/", options = ssl_options(weak_cert_validation = T), db="twitter", collection="tweets_geral")

#Getting tweets from database and saving on R variable
tweets_lula <- tweets_lula$find(
  fields = '{"_id": false, "text": true}',
)

tweets_bolsonaro <- tweets_bolsonaro$find(
  fields = '{"_id": false, "text": true}',
)

tweets_geral <- tweets_geral$find(
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
stop_words <- add_row(stop_words, word = "fica")
stop_words <- add_row(stop_words, word = "deve")
stop_words <- add_row(stop_words, word = "falando")
stop_words <- add_row(stop_words, word = "diz")
stop_words <- add_row(stop_words, word = "tomar")
stop_words <- add_row(stop_words, word = "lado")
stop_words <- add_row(stop_words, word = "querem")
stop_words <- add_row(stop_words, word = "sendo")
stop_words <- add_row(stop_words, word = "dizer")
stop_words <- add_row(stop_words, word = "maior")
stop_words <- add_row(stop_words, word = "saber")
stop_words <- add_row(stop_words, word = "favor")
stop_words <- add_row(stop_words, word = "vamos")
stop_words <- add_row(stop_words, word = "sabe")
stop_words <- add_row(stop_words, word = "tempo")
stop_words <- add_row(stop_words, word = "via")
stop_words <- add_row(stop_words, word = "pau")
stop_words <- add_row(stop_words, word = "gente")
stop_words <- add_row(stop_words, word = "ir")
stop_words <- add_row(stop_words, word = "olha")
stop_words <- add_row(stop_words, word = "ia")
stop_words <- add_row(stop_words, word = "coisas")
stop_words <- add_row(stop_words, word = "visto")
stop_words <- add_row(stop_words, word = "dá")
stop_words <- add_row(stop_words, word = "so")
stop_words <- add_row(stop_words, word = "vi")
stop_words <- add_row(stop_words, word = "fico")
stop_words <- add_row(stop_words, word = "tava")
stop_words <- add_row(stop_words, word = "sair")
stop_words <- add_row(stop_words, word = "ja")
stop_words <- add_row(stop_words, word = "deu")
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
tweets_lula$text <- str_replace(tweets_lula$text, patternRegexUrl, "") 
tweets_bolsonaro$text <- str_replace(tweets_bolsonaro$text, patternRegexUrl, "") 
tweets_geral$text <- str_replace(tweets_geral$text, patternRegexUrl, "") 

patternRegexKs <- 'k{2,}'
tweets_lula$text <- str_replace(tweets_lula$text, patternRegexKs, "")
tweets_bolsonaro$text <- str_replace(tweets_bolsonaro$text, patternRegexKs, "")
tweets_geral$text <- str_replace(tweets_geral$text, patternRegexKs, "")


# Removing @users reference from text
tweets_lula$text <- str_replace_all(tweets_lula$text,  "@\\w+", "") %>%
  removePunctuation() %>% # REMOVE PONTUAÇÃO
  stripWhitespace() # REMOVE ESPAÇOS EM BRANCO E PALAVRAS ACENTUADAS

tweets_bolsonaro$text <- str_replace_all(tweets_bolsonaro$text,  "@\\w+", "") %>%
  removePunctuation() %>% # REMOVE PONTUAÇÃO
  stripWhitespace() # REMOVE ESPAÇOS EM BRANCO E PALAVRAS ACENTUADAS


tweets_geral$text <- str_replace_all(tweets_geral$text,  "@\\w+", "") %>%
  removePunctuation() %>% # REMOVE PONTUAÇÃO
  stripWhitespace() # REMOVE ESPAÇOS EM BRANCO E PALAVRAS ACENTUADAS

# Removing emojis
# tweets$text <- gsub("[^\x01-\x7F]", "", tweets$text)

# SEPARA POR PARLAVRA E REMOVE AS STOPWORDS
tokens_tweets_lula <- tweets_lula %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tokens_tweets_lula$word = str_replace_all(tokens_tweets_lula$word, " ", "")

# SEPARA POR PARLAVRA E REMOVE AS STOPWORDS
tokens_tweets_bolsonaro <- tweets_bolsonaro%>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tokens_tweets_bolsonaro$word = str_replace_all(tokens_tweets_bolsonaro$word, " ", "")

# SEPARA POR PARLAVRA E REMOVE AS STOPWORDS
tokens_tweets_geral <- tweets_geral%>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tokens_tweets_geral$word = str_replace_all(tokens_tweets_geral$word, " ", "")


# CONTA AS PALAVRAS
tokens_tweets_count_lula <- tokens_tweets_lula %>%
  count(word, sort = TRUE)

tokens_tweets_count_bolsonaro <- tokens_tweets_bolsonaro %>%
  count(word, sort = TRUE)

tokens_tweets_count_geral <- tokens_tweets_geral %>%
  count(word, sort = TRUE)

frequency <- bind_rows(mutate(tokens_tweets_count_lula, author = "Lula"),
                       mutate(tokens_tweets_bolsonaro, author = "Bolsonaro"),
                       mutate(tokens_tweets_geral, author = "Geral")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Lula`:`Bolsonaro`)

library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Geral`, color = abs(`Geral` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 1) +
  theme(legend.position="none") +
  labs(y = "Geral", x = NULL)
