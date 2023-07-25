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
    query = c("libertarianism", "#libertarianism"),
    start_tweets = "2010-01-01T00:00:00Z",
    end_tweets = "2023-02-05T00:00:00Z",
    bearer_token = "yourtokenhere",
    n = 10000,
    lang = "en"
  )

libertarianism <- data.frame(tweets)

write_xlsx(libertarianism, "D:/01 R Projects/01 LIBERTARIANISM/libertarianism.xlsx")

### ANALSYS ###

# Extract tweet text from the pre-loaded dataset
anime_tweets <- read_excel("D:/anime.xlsx")
twt_txt <- anime_tweets$text
head(twt_txt)
twt_txt_url <- rm_twitter_url(twt_txt)
twt_txt_chrs <- gsub("[^A-Za-z]", ' ', twt_txt_url)
twt_corpus <- twt_txt_chrs %>%
  VectorSource()%>%
  Corpus()

# Convert the corpus to lowercase
twt_corpus_lwr <- tm_map(twt_corpus, content_transformer(tolower))
twt_corpus_lwr <- tm_filter(twt_corpus_lwr, FUN = function(x) length(trimws(x)) > 0)

# Original corpus
length(twt_corpus)

# Transformed corpus
length(twt_corpus_lwr)

# View the corpus after converting to lowercase
head(twt_corpus_lwr$content)

# Remove English stop words from the corpus and view the corpus
twt_corpus_stpwd <- tm_map(twt_corpus_lwr, removeWords, stopwords("english"))
# Original corpus
length(twt_corpus)

# Transformed corpus
length(twt_corpus_lwr)

# Remove sparse terms that occur in less than 5% of the documents
twt_dtm <- DocumentTermMatrix(twt_corpus_stpwd)

# Remove sparse terms that occur in less than 5% of the documents
twt_dtm_final <- removeSparseTerms(twt_dtm, 0.05)

# Convert the DocumentTermMatrix to a matrix
twt_matrix <- as.matrix(twt_dtm_final)
head(twt_corpus_stpwd$content)

# Remove additional spaces from the corpus
twt_corpus_final <- tm_map(twt_corpus_stpwd, stripWhitespace)

# Original corpus
length(twt_corpus)

# Transformed corpus
length(twt_corpus_lwr)

# View the text corpus after removing spaces
head(twt_corpus_final$text)

# Extract term frequencies for top 60 words and view output
termfreq  <-  freq_terms(twt_corpus, 60)
termfreq

# Create a vector of custom stop words
custom_stopwds <- c("telemedicine", " s", "amp", "can", "new", "medical", 
                    "will", "via", "way",  "today", "come", "t", "ways", 
                    "say", "ai", "get", "now")

# Remove custom stop words and create a refined corpus
corp_refined <- tm_map(twt_corpus,removeWords, custom_stopwds) 

# Original corpus
length(twt_corpus)

# Transformed corpus
length(twt_corpus_lwr)

# Extract term frequencies for the top 20 words
termfreq_clean <- freq_terms(corp_refined, 20)
termfreq_clean

# Extract term frequencies for the top 10 words
termfreq_10w <- freq_terms(corp_refined, 10)
termfreq_10w

# Identify terms with more than 60 counts from the top 10 list
term60 <- subset(termfreq_10w, FREQ > 60)

# Create a bar plot using terms with more than 60 counts
ggplot(term60, aes(x = reorder(WORD, -FREQ), y = FREQ)) + 
  geom_bar(stat = "identity", fill = "red") + 
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

# Create word cloud with 6 colors and max 50 words
wordcloud(corp_refined, max.words = 50, 
          colors = brewer.pal(6, "Dark2"), 
          scale=c(4,1), random.order = FALSE)

### TOPIC MODELING ###

# Create a document term matrix (DTM) from the pre-loaded corpus
dtm <- DocumentTermMatrix(corp_refined)
inspect(dtm)
rowTotals <- apply(dtm, 1, sum)
dtm <- DocumentTermMatrix(corp_refined)
inspect(dtm)
rowTotals <- apply(dtm, 1, sum)
tweet_dtm_new <- dtm[rowTotals > 0, ]
lda_5 <- LDA(tweet_dtm_new, k = 5)
top10terms <- terms(lda_5,10)
top10terms

### SENTIMENT ANALSYS ###

libertarianism_tweets <- read_excel("D:/01 R Projects/01 LIBERTARIANISM/libertarianism.xlsx")
twt_txt <- libertarianism_tweets$text
libertarianism_value <- get_nrc_sentiment(twt_txt)
libertarianism_value[1:100, 1:10]
score <- colSums(libertarianism_value[,])
score_df <- data.frame(score)
score_df
libertarianism_score <- cbind(sentiment = row.names(score_df),
                       score_df, row.names= NULL)
print(libertarianism_score)
ggplot(data = libertarianism_score, aes(x = sentiment, y = score, fill = sentiment))+
  geom_bar(stat = "identity", width = 0.9) +
  scale_fill_brewer(palette = "Spectral") +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

