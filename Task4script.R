#ASDM Assignment Task 4: Sentiment Analysis
#Programmed by S. Mehran Ronaghi

#Setting working directory
mypath = 'C:/ASDM/Assignment/Task4'
setwd(mypath)
getwd()

#Loading necessary packages
install.packages('tm')
library(tm)

install.packages('SnowballC')
library(SnowballC)

install.packages('wordcloud')
library(wordcloud)

install.packages('RColorBrewer')
library(RColorBrewer)

#Importing datasets
reviews <- read.csv('tourist_accommodation_reviews.csv', header= TRUE)
positive_lexicon <- read.csv('positive-lexicon.txt')
negative_lexicon <- read.csv('negative-lexicon.txt')

#Inspecting tourist accommodation reviews dataset
dim(reviews)
names(reviews)
head(reviews)
tail(reviews)
summary(reviews)
str(reviews)

#Data Preparation

#Cleaning Variable Names
install.packages('janitor')
library(janitor)

reviews <- reviews %>% janitor::clean_names()
names(reviews)

#Missing Values
install.packages('skimr')
library(skimr)

skim(reviews)

text_mining <- function(text)
{
  text <- tolower(text)
  text <- gsub('http\\S+\\s*', '', text)
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:digit:]]', '', text)
  text <- gsub('^ ', '', text)
  text <- gsub(' $', '', text)
  text <- gsub('[\r\n]', '', text)
  text <- gsub('hotel', '', text)
  text <- gsub('restaurant', '', text)
  text <- gsub(hotel, '', text) #removing hotel name

  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, stripWhitespace)
  stem_corpus <- tm_map(corpus, stemDocument)
  
  return(stem_corpus)
}

sentiment_analysis <- function(stem_corpus)
{
  #Calculating the count of total positive and negative words in each review
  #Create variables and vectors
  total_pos_count <- 0
  total_neg_count <- 0
  #Calculating the size of the corpus
  size <- length(stem_corpus)
  for(i in 1:size)
  {
    #All the words in current review
    corpus_words <- list(strsplit(stem_corpus[i]$content, split = ' '))
    #positive words in current review
    pos_count <- length(intersect(unlist(corpus_words), unlist(positive_lexicon)))
    #negative words in current review
    neg_count <- length(intersect(unlist(corpus_words), unlist(negative_lexicon)))
    total_pos_count <- total_pos_count + pos_count ## overall positive count
    total_neg_count <- total_neg_count + neg_count ## overall negative count
  }
  #Calculating overall percentage of positive and negative words of all the reviews
  total_count <- total_pos_count + total_neg_count
  overall_positive_percentage <- (total_pos_count*100)/total_count
  overall_negative_percentage <- (total_neg_count*100)/total_count
  #Create a dataframe with all the positive and negative reviews
  df <- data.frame(Review_Type=c("Postive","Negitive"),
                   Count=c(total_pos_count ,total_neg_count ),
                   Percentage=c(round(overall_positive_percentage,2), round(overall_negative_percentage,2)))
  print(df)
  print(paste('Percentage of Positive Reviews for',
                                       hotel,'is',
                                       round(overall_positive_percentage,2),'%'))
  if (overall_positive_percentage>50 || overall_positive_percentage=='NaN')
    result <- 'Positive'
  else
    result <- 'Negative'
  return(result)
}

#Selecting 30 hotels
hotels<- unique(reviews$hotel_restaurant_name)
hotels
summary(hotels)

set.seed(123)
selected_hotels <- sample(hotels, size=30, replace = F)
selected_hotels

#main program
for (hotel in selected_hotels)
{
  hotel_reviews <- subset(reviews, hotel_restaurant_name==hotel)
  text <- hotel_reviews$review
  stem_corpus <- text_mining(text)
  
  #generate wordclouds
  wordcloud(stem_corpus,
            min.freq = 3,
            max.words = 30,
            colors = brewer.pal(8, 'Dark2'),
            random.color = TRUE,
            scale=c(4,.5),
  )
  
  sentiment_analysis(stem_corpus)
}

#Preparing the dataset for SAS
sentiment <- c()
selected_reviews <- subset(reviews, hotel_restaurant_name %in% selected_hotels)
for (i in 1:nrow(selected_reviews))
{
  text <- selected_reviews[i,]$review
  stem_corpus <- text_mining(text)
  result <- sentiment_analysis(stem_corpus)
  sentiment <- c(sentiment,result)
}
selected_reviews$id <- seq.int(nrow(selected_reviews))
selected_reviews$sentiment <- sentiment
write.csv(selected_reviews, 'tourist_accommodation_reviews_SAS.csv', row.names = FALSE)

dim(selected_reviews)
names(selected_reviews)