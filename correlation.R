#library(stringr)
#library(twitteR)
#library(purrr)
#library(tidytext)
#library(dplyr)
#library(tidyr)
#library(lubridate)
#library(scales)
#library(broom)
#library(ggplot2)
tweets.nodups.df <- read_csv("Tweets.csv")
#clean up dataframe a bit
tweets.nodups.df$text <- gsub('???', '', tweets.nodups.df$text) #remove ... at end of tweets
tweets.nodups.df <- plyr::rename(tweets.nodups.df, c("created" = "Date")) #rename created to Date
tweets.nodups.df$Date <- as.Date(tweets.nodups.df$Date,"%d/%m/%y") #convert from datetime to date format
tweets_nodups_text <- (tweets.nodups.df$text)

#Create tweet corpus
r_stats_text_corpus <- Corpus(VectorSource(tweets_nodups_text))
r_stats_text_corpus <- 
  r_stats_text_corpus %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(function(x) iconv(x, from='ASCII', 
                                               to='UTF-8', sub='byte'))) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(function(x) str_replace_all(x, "@\\w+", ""))) %>% # remove twitter handles
  tm_map(removeNumbers) %>%
  tm_map(stemDocument) %>%
  tm_map(stripWhitespace)

#Create color word cloud 
f<-wordcloud(r_stats_text_corpus, min.freq = 10, max.words = 150, colors=brewer.pal(8, "Dark2"))


score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    sentence = tolower(sentence)
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

anthem.scores <- score.sentiment(tweets_nodups_text, pos.words, neg.words, .progress='none')
anthem.score.merge <- merge(anthem.scores, tweets.nodups.df, by = 'text')
hist(anthem.score.merge$score,xlab=" ",main="Sentiment of tweets",
     border="black",col="skyblue")
plot(anthem.score.merge$Date, anthem.score.merge$score, xlab = "Date",
     ylab = "Sentiment Score", main = "Sentiment of tweets")

plot(anthem.score.merge$Date,anthem.score.merge$score)
#Read stock price CSV in 
stock_prices <- read.csv("AAPL.csv")
head(stock_prices)
#Format date so R knows this is a date field
stock_prices$Date <- as.Date(as.character(stock_prices$Date))
#library(base)
#Left join the sentiment analysis with the stock prices 
weekday_tweet_stock <- left_join(anthem.score.merge, stock_prices, by = "Date")
#View(tweet_stock)
View(weekday_tweet_stock)

#weekday_tweet_stock <- subset(tweet_stock,  !is.na(stock_change))
View(weekday_tweet_stock)
plot(jitter(weekday_tweet_stock$score), weekday_tweet_stock$Adj.Close,
     xlab = "Sentiment Score", ylab = "Daily Change in Stock Price")

#Create indicator fields to flag tweets as positive, negative or neutral based on sentiment score 
weekday_tweet_stock$pos <- as.numeric(weekday_tweet_stock$score >= 1)
weekday_tweet_stock$neg <- as.numeric(weekday_tweet_stock$score <= -1)
weekday_tweet_stock$neu <- as.numeric(weekday_tweet_stock$score == 0)

#Transform file from one row per tweet to one row per day summarizing the total positive, negative and netural tweets per day 
tweet_stock_df <- ddply(weekday_tweet_stock, c('Date'), plyr::summarise, pos.count = sum(pos), neg.count = sum(neg), neu.count = sum(neu))
tweet_stock_df$all.count <- tweet_stock_df$pos.count + tweet_stock_df$neg.count + tweet_stock_df$neu.count

#calculate the percent of tweets that were positive on each day 
tweet_stock_df$percent.pos <- round((tweet_stock_df$pos.count / tweet_stock_df$all.count) * 100)
tweet_stock_df <- left_join(stock_prices,tweet_stock_df, by = "Date")
View(tweet_stock_df)
#Simple correlation
cor(tweet_stock_df$percent.pos, tweet_stock_df$Adj.Close, use = "complete")
lm_model <- lm(tweet_stock_df$Adj.Close ~ tweet_stock_df$percent.pos)
summary(lm_model)
plot(tweet_stock_df$percent.pos, tweet_stock_df$Adj.Close, ylab = "Adjacent close of Stock Price",  xlab = "Percent of Tweets Positive", main = "% Positive Tweets vs Daily Stock Price Change")
abline(lm_model)
