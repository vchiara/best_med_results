library(pdftools)
library(tm)
library(quanteda)
library(quanteda.textplots)
library(wordcloud)
library(tidyverse)

directory <- getwd()

my_corpus <- VCorpus(DirSource(directory, pattern = ".pdf"), 
                     readerControl = list(reader = readPDF))


#cleaning
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
my_corpus <- tm_map(my_corpus, toSpace, "/")
my_corpus <- tm_map(my_corpus, toSpace, "@")
my_corpus <- tm_map(my_corpus, toSpace, "\\|")
my_corpus <- tm_map(my_corpus, content_transformer(tolower))
my_corpus <- tm_map(my_corpus, removeNumbers)
my_corpus <- tm_map(my_corpus, removeWords, stopwords("english"))

#removing common words
my_corpus <- tm_map(my_corpus, removeWords, c("MED","med", "S&C", "PATH", "Path", "paths",
                                              "BEST", "best", "seminar",
                                              "path", "route", "routes", "cultural",
                                              "sustainable", "sustainability",
                                              "Calabria", "Lazio", "Greece",
                                              "Spain", "Andalusia", "Slovenia", "slovenia",
                                              "please", "andalusia","calabria", "lazio",
                                              "Montenegro", "Italy", "tourism",
                                              "tourist", "tourists", "travel",
                                              "destination", "interreg", "Europe",
                                              "European Union", "EU", "model", "project",
                                              "regione", "montenegro")) 
# remove punctuation/white space
my_corpus <- tm_map(my_corpus, removePunctuation)
my_corpus <- tm_map(my_corpus, stripWhitespace)

#dtm
my_corpus_dtm <- TermDocumentMatrix(my_corpus)
dtm_m <- as.matrix(my_corpus_dtm)
dtm_v <- sort(rowSums(dtm_m), decreasing = T)
dtm_d <- data.frame(word = names(dtm_v), freq = dtm_v)

head(dtm_d, 10)

#term frequency plot
freq_plot <- ggplot(dtm_d[1:15,], aes(x = reorder(word, -freq), freq, text = freq)) +
  geom_bar(stat = "identity", fill = pal[2]) +
  labs(x = "Terms", y = "Frequency", title = "Frequent terms") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 15,
                       max.words = 100, random.order = F, rot.per = 0.45, 
                       colors = brewer.pal(5, "Blues"), scale = c(4, 0.5))
##associations
assocs <- findAssocs(my_corpus_dtm, 
           terms = dtm_d[1:5,1], corlimit = 0.75)

#word network
set.seed(12345)

quanteda_corpus <- corpus(my_corpus)

tokens <- quanteda_corpus %>%
  tokens(remove_punct = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(pattern = stopwords("english"), padding = FALSE)

fcmat <- fcm(tokens, context = "window", tri = FALSE)

feat <- names(topfeatures(fcmat, 30))

fcm <- fcm_select(fcmat, pattern = feat)

word_network <- textplot_network(fcm, 
                                 vertex_labelsize = 2 * rowSums(fcm) / min(rowSums(fcm)),
                                 min_freq = 0.5)