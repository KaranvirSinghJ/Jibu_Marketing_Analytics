#EDA on Responses
#Libraries
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lattice)
library(caret)
library(e1071)
library(janitor)
library(lubridate)
library(cluster)
library(methods)
library(modelr)
library(dplyr)
library(corrplot)
library(e1071)
library(factoextra)
library(tm)
library(wordcloud)
library(readxl)
library(syuzhet)
library(topicmodels)
library(RColorBrewer)


#Import dataset
jibuempresponses <- read_excel("jibubojsurvey.xlsx", sheet = "Sheet1")
View(jibuempresponses)

#Reading column names
headers<-colnames(jibuempresponses)
headers


#Sentiment Analysis
summary(jibuempresponses$feedbackfromemployees)

# Text Preprocessing
additional_stopwords <- c("in", "is", "jibu", "'ve", "'s","the","bottles","water","na","franchisee","franchise","franchises","employee","sales","employees","employer")

corpus <- Corpus(VectorSource(jibuempresponses$feedbackfromemployees))
corpus_clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, c(stopwords("english"), additional_stopwords)) %>%
  tm_map(stripWhitespace)

# Ensure no empty documents proceed to DTM creation
corpus_clean <- corpus_clean[sapply(corpus_clean, function(doc) nchar(as.character(doc)) > 0)]

# Correct way to convert cleaned corpus to a text vector for the word cloud and sentiment analysis
text_vector <- sapply(corpus_clean, function(doc) as.character(doc))# Generate Wordcloud for EDA
set.seed(123) # For reproducibility
wordcloud(words = text_vector, max.words = 140, random.order = FALSE, scale = c(3, 0.5), colors=brewer.pal(8,"Dark2"))

# Prepare DTM and remove sparse terms
dtm <- DocumentTermMatrix(corpus_clean)
dtm <- removeSparseTerms(dtm, 0.995) 
# Remove terms that appear in less than 0.5% of the documents
# Filter out documents with no terms after preprocessing
valid_docs_index <- rowSums(as.matrix(dtm)) > 0
dtm_filtered <- dtm[valid_docs_index, ]

# Sentiment Analysis with two lexicons: AFINN and Bing
#get_sentiments(lexicon = "afinn")
sentiments_afinn <- sapply(sapply(corpus_clean, as.character), get_sentiment, method = "afinn")
#sentiments_afinn
#get_sentiments(lexicon = "bing")
sentiments_bing <- sapply(sapply(corpus_clean, as.character), get_sentiment, method = "bing")
#sentiments_bing

# Comparing Sentiments
compare_sentiments <- data.frame(AFINN=sentiments_afinn, Bing=sentiments_bing)
ggplot(compare_sentiments, aes(x=AFINN, y=Bing)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", col="red") +
  ggtitle("Comparison of Sentiment Scores: AFINN vs Bing") +
  xlab("AFINN Sentiment Score") + ylab("Bing Sentiment Score")

# Topic Modeling: LDA
dtm <- DocumentTermMatrix(corpus_clean)

#Compute the sum of words (non-zero entries) in each document
rowTotals<-apply(dtm,1,sum)

#Remove rows where the sum of words is zero
dtm_new<-dtm[rowTotals>0,]

lda_model <- LDA(dtm_new, k = 2, control = list(seed = 123))
topics <- topics(lda_model)
topic_terms_beta <- terms(lda_model, 6)
topic_terms_gamma <- lda_model@gamma
topic_terms_beta
topic_terms_gamma

# Visualizing Topics
beta <- lda_model@beta
barplot(beta[1,], names.arg = rownames(beta), main = "Topic 1 Beta Values")
barplot(beta[2,], names.arg = rownames(beta), main = "Topic 2 Beta Values")

# Words Common to Each Sentiment and Discriminating Words
findFreqTerms(dtm, lowfreq = 50) # Common words
findAssocs(dtm, "good", 0.3) # Words associated with positive sentiment
findAssocs(dtm, "poor", 0.3) # Words associated with negative sentiment
findAssocs(dtm, "okay", 0.3) # Words associated with neutral sentiment

