#Importing Libraries for Text Analysis
library(qdap)
library(readr)
library(tm)
library(wordcloud)
library(RWeka)
library(readtext)
library(stopwords)
library(wordcloud2)
library(dplyr)
library(ggplot2)
library(plotly)
library(twitteR)
library(text2vec)
library(tidytext)
library(radarchart)
library(shiny)
library(shinythemes)
library(extrafont)

#Importing cleaned tweet data
uber <- read.csv("Uber_cleaned.csv", row.names = 1)
lyft <- read.csv("Lyft_cleaned.csv", row.names = 1)

#Creating functions to use in cleaning procress
removeMentions <- function(x) gsub("@\\S+", "", x)
removeLinks <- function(x) gsub("http.*", "", x)

#Define preprocessing function and tokenization function
prep_fun  <- function(x){
  x %>%
    replace_abbreviation %>%
    removeLinks %>%
    removeMentions %>%
    tolower %>%
    removePunctuation %>%
    removeNumbers %>%
    stripWhitespace %>%
    removeWords(c("rt", "uber", "lyft", "Uber", "Lyft", "ubers", "lyfts", "Ubers", "Lyfts", "miguelt","ltlt","zoot","gtgt","u","dont","can")) %>%
    word_tokenizer
}

#Creating tokens  
uber_tokens <- prep_fun(uber$text)
lyft_tokens <- prep_fun(lyft$text)

#Getting stopwords list
stop_words <- stopwords("english")

uber_it <- itoken(uber_tokens)
lyft_it <- itoken(lyft_tokens)

#Getting word frequencies and number of documents they appear it
uber_vocab <- create_vocabulary(uber_it,  stopwords = stop_words)
lyft_vocab <- create_vocabulary(lyft_it,  stopwords = stop_words)

#Vectorization
uber_vectorizer <- vocab_vectorizer(uber_vocab)
lyft_vectorizer <- vocab_vectorizer(lyft_vocab)

#Creating the DTM
uber_dtm <- create_dtm(uber_it , uber_vectorizer, type = "dgTMatrix")
lyft_dtm <- create_dtm(lyft_it , lyft_vectorizer, type = "dgTMatrix")


#Creating TFIDF 
model_tfidf = TfIdf$new()
dtm_tfidf_uber = model_tfidf$fit_transform(uber_dtm)
dtm_tfidf_lyft = model_tfidf$fit_transform(lyft_dtm)

#Transforming to a matrix
dtm_uber_m = as.matrix(dtm_tfidf_uber)
dtm_lyft_m = as.matrix(dtm_tfidf_lyft)

#Sorting the term frequencies
sort1 = sort(colSums(dtm_uber_m),decreasing=T)
sort2 = sort(colSums(dtm_lyft_m),decreasing=T)

#Creating a dataframe of terms and frequency count
tfidf_freq_uber = data.frame(words=names(sort1),freq=sort1)
tfidf_freq_lyft = data.frame(words=names(sort2),freq=sort2)

#Creating Bigrams
bi_uber_it <- itoken(uber_tokens)
bi_lyft_it <- itoken(lyft_tokens)

bi_uber_vocab <- create_vocabulary(bi_uber_it,  stopwords = stop_words, ngram = c(ngram_min=2, ngram_max=2), sep_ngram = " ")
bi_lyft_vocab <- create_vocabulary(bi_lyft_it,  stopwords = stop_words, ngram = c(ngram_min=2, ngram_max=2), sep_ngram = " ")

bi_uber_vectorizer <- vocab_vectorizer(bi_uber_vocab)
bi_lyft_vectorizer <- vocab_vectorizer(bi_lyft_vocab)

bi_uber_dtm <- create_dtm(bi_uber_it , bi_uber_vectorizer, type = "dgTMatrix")
bi_lyft_dtm <- create_dtm(bi_lyft_it , bi_lyft_vectorizer, type = "dgTMatrix")

#Creating Trigrams
tri_uber_it <- itoken(uber_tokens)
tri_lyft_it <- itoken(lyft_tokens)

tri_uber_vocab <- create_vocabulary(tri_uber_it,  stopwords = stop_words, ngram = c(ngram_min=3, ngram_max=3), sep_ngram = " ")
tri_lyft_vocab <- create_vocabulary(tri_lyft_it,  stopwords = stop_words, ngram = c(ngram_min=3, ngram_max=3), sep_ngram = " ")

tri_uber_vectorizer <- vocab_vectorizer(tri_uber_vocab)
tri_lyft_vectorizer <- vocab_vectorizer(tri_lyft_vocab)

tri_uber_dtm <- create_dtm(tri_uber_it , tri_uber_vectorizer, type = "dgTMatrix")
tri_lyft_dtm <- create_dtm(tri_lyft_it , tri_lyft_vectorizer, type = "dgTMatrix")

#Sentiment Analysis
tidy_uber <- tidy(uber_dtm)
bing_lex <- get_sentiments("bing")

uber_bing <- inner_join(tidy_uber, bing_lex, by = c("column" = "word"))

uber_bing$sentiment_n <- ifelse(uber_bing$sentiment=="negative", -1, 1)

uber_bing$sentiment_score <- uber_bing$value*uber_bing$sentiment_n

sentiment_summary_uber <- uber_bing %>%
  group_by(row) %>%
  summarize(review_sentiment = sum(sentiment_score)) %>%
  arrange(desc(review_sentiment))

#Looking at most frequent positive and negative words
uber_pos <- filter(uber_bing, sentiment=="positive") %>%
  group_by(column)%>%
  summarise(freq=sum(value))


uber_neg <- filter(uber_bing, sentiment=="negative") %>%
  group_by(column)%>%
  summarise(freq=sum(value))

hist_uber <- ggplot(data = sentiment_summary_uber) +
  aes(x = review_sentiment) +
  geom_histogram(bins = 30, fill = "#0c4c8a") +
  labs(title = "Uber Sentiment Analysis using BING Lexicon",
       x = "Sentiment Score",
       y = "Frequency") +
  theme_gray()

ggplotly(hist_uber)


tidy_lyft <- tidy(lyft_dtm)
bing_lex <- get_sentiments("bing")

lyft_bing <- inner_join(tidy_lyft, bing_lex, by = c("column" = "word"))

lyft_bing$sentiment_n <- ifelse(lyft_bing$sentiment=="negative", -1, 1)

lyft_bing$sentiment_score <- lyft_bing$value*lyft_bing$sentiment_n

sentiment_summary_lyft <- lyft_bing %>%
  group_by(row) %>%
  summarize(review_sentiment = sum(sentiment_score)) %>%
  arrange(desc(review_sentiment))

#Looking at most frequent positive and negative words
lyft_pos <- filter(lyft_bing, sentiment=="positive") %>%
  group_by(column)%>%
  summarise(freq=sum(value))

lyft_neg <- filter(lyft_bing, sentiment=="negative") %>%
  group_by(column)%>%
  summarise(freq=sum(value))

hist_lyft <- ggplot(data = sentiment_summary_lyft) +
  aes(x = review_sentiment) +
  geom_histogram(bins = 30, fill = "#ef562d") +
  labs(title = "Lyft Sentiment Analysis using BING Lexicon",
       x = "Sentiment Score",
       y = "Frequency") +
  theme_gray()

ggplotly(hist_lyft)

#Emotion Analysis
nrc_lex <- get_sentiments("nrc")

uber_nrc <- inner_join(tidy_uber, nrc_lex, by = c("column" = "word"))

#Removing positive and negative sentiments
uber_nrc_noposneg <- uber_nrc[!(uber_nrc$sentiment %in% c("positive","negative")),]

uber_score <- uber_nrc_noposneg %>%
  group_by(sentiment) %>%
  summarise(n=n())

#Radar Chart
chartJSRadar(uber_score)


#Bar Chart
uber_nrc_plot <- ggplot(data=uber_score,aes(x=sentiment,y=n))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+ggtitle("Uber Emotion Analysis using NRC")

ggplotly(uber_nrc_plot)


lyft_nrc <- inner_join(tidy_lyft, nrc_lex, by = c("column" = "word"))

#Removing positive and negative sentiments
lyft_nrc_noposneg <- lyft_nrc[!(lyft_nrc$sentiment %in% c("positive","negative")),]

lyft_score <- lyft_nrc_noposneg %>%
  group_by(sentiment) %>%
  summarise(n=n())

#Radar Chart
chartJSRadar(lyft_score)

#Bar Chart
lyft_nrc_plot <- ggplot(data=lyft_score,aes(x=sentiment,y=n))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+ggtitle("Lyft Emotion Analysis using NRC")

ggplotly(lyft_nrc_plot)



#Extrating text and combining it together
uber_text = paste(uber$text, collapse=" ")
lyft_text = paste(lyft$text, collapse=" ")

#Put everything in a single vector
both_drivers <- c(uber_text, lyft_text)

#Making a corpus of a vector source 
both_corpus <- VCorpus(VectorSource(both_drivers))

#Cleaning corpus - pre_processing
clean_corpus2 <- function(corpus){
  cleaned_corpus2 <- tm_map(corpus, content_transformer(replace_abbreviation))
  cleaned_corpus2 <- tm_map(cleaned_corpus2, content_transformer(tolower))
  cleaned_corpus2 <- tm_map(cleaned_corpus2, removePunctuation)
  cleaned_corpus2 <- tm_map(cleaned_corpus2, removeNumbers)
  cleaned_corpus2 <- tm_map(cleaned_corpus2, removeWords, stopwords("english"))
  custom_stop_words <- c("lyft","uber", "Lyft","Uber")
  cleaned_corpus2 <- tm_map(cleaned_corpus2, removeWords, custom_stop_words)
  cleaned_corpus2 <- tm_map(cleaned_corpus2, stripWhitespace)
  return(cleaned_corpus2)
}

#Apply the clean_corpus function 
cleaned_both_corpus <- clean_corpus2(both_corpus)

#TDM
TDM_both <- TermDocumentMatrix(cleaned_both_corpus)
TDM_both_m <- as.matrix(TDM_both)

#Commonality Cloud
par(mar=rep(0,4))
commonality.cloud(TDM_both_m,colors=brewer.pal(8, "Dark2"),max.words = 500, random.order=FALSE)


#Comparison Cloud 
TDM_both <- TermDocumentMatrix(cleaned_both_corpus)
colnames(TDM_both) <- c("Uber","Lyft")
TDM_both_m <- as.matrix(TDM_both)
par(mar=rep(0,4))
comparison.cloud(TDM_both_m,colors=brewer.pal(8, "Dark2"),max.words = 500, random.order=FALSE)

#Other Things Needed for Shiny App
png("wordcloud_packages.png", width=12,height=8, units='in', res=800)

#Creating datasets for tweet tab
driving_companies <- list("Uber", "Lyft")

scores <- list("Uber" = uber_score$n,
               "Lyft" = lyft_score$n)

#Creating labels for radarchart
labels <- c("Anger", "Anticipation", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Trust")

#Creating colors for radarchart
color <- grDevices::col2rgb(c("royalblue","deeppink2"))

#Creating uber and lyft color system for wordclouds
ubercols <- colors()[c(563, 566, 490)]
lyftcols <- colors()[c(369, 370, 645)]

