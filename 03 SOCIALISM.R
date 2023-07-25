library(academictwitteR)
library(tidyverse)
library(dplyr)
library(writexl)
library(readxl)
library(qdapRegex)
library(tm)
library(qdap)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(usethis)
library(topicmodels)
library(usethis)
library(syuzhet)
library(viridis)

### DATA SCRAPING ### 
tweets <-
  get_all_tweets(
    query = c("socialism", "#socialism"),
    start_tweets = "2010-01-01T00:00:00Z",
    end_tweets = "2023-02-05T00:00:00Z",
    bearer_token = "AAAAAAAAAAAAAAAAAAAAAPrTkAEAAAAAmewz%2F7NyxcD4Qv4XC2g%2FQmOOIMo%3DhRQiEiLefa7pNtrRz2yHe5PyenbX46rBbT1E3B6XNcPCRzczUZ",
    file = "socialism",
    n = 10000,
    lang = "en"
  )

socialism <- data.frame(tweets)

write_xlsx(socialism, "D:/01 R Projects/04 SOCIALISM/socialism.xlsx")

### DATA PREPARATIONS ###

socialism_tweets <- read_excel("D:/01 R Projects/04 SOCIALISM/socialism.xlsx")
twt_txt <- socialism_tweets$text
head(twt_txt)
twt_txt_url <- rm_twitter_url(twt_txt)
twt_txt_chrs <- gsub("[^A-Za-z]", ' ', twt_txt_url)
twt_corpus <- twt_txt_chrs %>%
  VectorSource()%>%
  Corpus()
twt_corpus_lwr <- tm_map(twt_corpus, tolower)
twt_corpus_stpwd <- tm_map(twt_corpus_lwr, removeWords, stopwords("english"))
twt_corpus_final <- tm_map(twt_corpus_stpwd, stripWhitespace)
term_count <- freq_terms(twt_corpus_final, 60)
term_count
custom_stop <- c()
twt_corpus_refined <- tm_map(twt_corpus_final, removeWords, custom_stop)
term_count_clean <- freq_terms(twt_corpus_refined, 20)
term_count_clean
term50 <- subset(term_count_clean, FREQ > 50)

### PLOT ###

ggplot(term50, aes(x = reorder(WORD, -FREQ), y = FREQ)) +
  geom_bar(stat = "identity", width = 0.9, color = "black", alpha = 0.7)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### WORDCLOUD ###
my_palette <- brewer.pal(8, "Dark2")
wordcloud(twt_corpus_refined, max.words = 40, random.order = FALSE,
          colors = my_palette, scale = c(3, 0.8))

### TOPIC MODELING ###

dtm <- DocumentTermMatrix(twt_corpus_refined)
inspect(dtm)
rowTotals <- apply(dtm, 1, sum)
dtm <- DocumentTermMatrix(twt_corpus_refined)
inspect(dtm)
rowTotals <- apply(dtm, 1, sum)
tweet_dtm_new <- dtm[rowTotals > 0, ]
lda_5 <- LDA(tweet_dtm_new, k = 5)
top10terms <- terms(lda_5,10)
top10terms

### SENTIMENT ANALSYS ###

socialism_tweets <- read_excel("D:/01 R Projects/04 SOCIALISM/socialism.xlsx")
twt_txt <- socialism_tweets$text
socialism_value <- get_nrc_sentiment(twt_txt)
socialism_value[1:100, 1:10]
score <- colSums(socialism_value[,])
score_df <- data.frame(score)
score_df
socialism_score <- cbind(sentiment = row.names(score_df),
                         score_df, row.names= NULL)
print(socialism_score)
ggplot(data = socialism_score, aes(x = sentiment, y = score, fill = sentiment))+
  geom_bar(stat = "identity", width = 0.9) +
  scale_fill_brewer(palette = "Spectral") +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

