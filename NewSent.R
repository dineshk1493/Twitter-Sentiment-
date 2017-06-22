library(tm)
library(twitteR)
library(ROAuth)
library(devtools)
library(methods)
library(base64enc)
library(ggplot2)
library(plyr)
library(stringr)
setup_twitter_oauth(consumer_key='XHZ4szNVgYWZDQy0khLuWlwsJ',consumer_secret='l3Q2wFiXsf8jZxyvzN8a52vKTd9RbNAjPPvBihd65OrQ1gzzlz', access_token= '221334395-z9UuCEtEeEBP1xuR6sAnEBHbYvcCyvtWAOU7COCq' , access_secret='dITJkVcOfHyrjvMOsvBaqZEeTZ1gj911OLzmUWN8o5Gb9')

cashless_tweets = searchTwitter('Cashless India', n=2000)  # keyword returns required no of tweets

digital_tweets = searchTwitter('digital India', n=2000) # keyword returns required no of tweets

Samsung_tweets = searchTwitter('Samsung Pay', n=2000) # keyword returns required no of tweets

#nfc_tweets = searchTwitter('NFC Payments', n=1000) #Keywords
posText <- read.delim("D:/DINESH/DATA SCIENCE/positive.txt", header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
negText <- read.delim("D:/DINESH/DATA SCIENCE/negative.txt", header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))
pos.words = c(posText, 'upgrade')
neg.words = c(negText, 'wtf', 'wait', 'waiting','epicfail', 'mechanical')

#make data frame
df <- do.call("rbind", lapply(cashless_tweets, as.data.frame))
df1 <- do.call("rbind", lapply(digital_tweets, as.data.frame))
df2 <- do.call("rbind", lapply(Samsung_tweets, as.data.frame))
# df3 = do.call("rbind",lapply(nfc_tweets, as.data.frame))

View(df)
View(df1)
View(df2)
# View(df3)

#write to csv file (or your RODBC code)
write.csv(df,file="cashless_tweets.csv")
write.csv(df1,file="digital_tweets.csv")
write.csv(df2,file="Samsung_tweets.csv")
# write.csv(df3,file = "nfc_tweets.csv")

cashless_txt = sapply(cashless_tweets, function(t) t$getText() )
digital_txt = sapply(digital_tweets, function(t) t$getText() )
Samsung_txt = sapply(Samsung_tweets, function(t) t$getText() )

# nfc_txt = sapply (nfc_tweets, function(t) t$getText())


noof_tweets = c(length(cashless_txt), length(digital_txt),length(Samsung_txt))
digital<- c(cashless_txt,digital_txt,Samsung_txt)

# noof_tweets = c(length(cashless_txt), length(digital_txt),length(Samsung_txt),length(nfc_txt))
# digital <- c (cashless_txt,digital_txt,Samsung_txt,nfc_txt)

 score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of positive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  # create a simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

scores = score.sentiment(digital,pos.words,neg.words,.progress = 'text')

scores$keyword = factor(rep(c("cashless", "digital","Samsung"), noof_tweets))
#scores$keyword = factor(rep(c("cashless","digital","Samsung","nfc")))

scores$positive <- as.numeric(scores$score >0)
scores$negative <- as.numeric(scores$score >0)
scores$neutral <- as.numeric(scores$score==0)

cashless_keyword <- subset(scores, scores$keyword=="cashless")
digital_keyword <- subset(scores,scores$keyword=="digital")
Samsung_keyword <- subset(scores,scores$keyword=="Samsung")
# nfc_keyword <- subset(scores,scores$keyword=="nfc")

cashless_keyword$polarity <- ifelse(cashless_keyword$score >0,"positive",ifelse(cashless_keyword$score < 0,"negative",ifelse(cashless_keyword$score==0,"Neutral",0)))
digital_keyword$polarity <- ifelse(digital_keyword$score >0,"positive",ifelse(digital_keyword$score < 0,"negative",ifelse(digital_keyword$score==0,"Neutral",0)))
Samsung_keyword$polarity <- ifelse(Samsung_keyword$score >0,"positive",ifelse(Samsung_keyword$score < 0,"negative",ifelse(Samsung_keyword$score==0,"Neutral",0)))

qplot(factor(polarity), data=cashless_keyword, geom="bar", fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle("Customer Sentiments - Cashless India")
qplot(factor(score), data=cashless_keyword, geom="bar", fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Customer Sentiment Scores - Cashless India")
qplot(factor(polarity), data=digital_keyword, geom="bar", fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle(" Customer Sentiments - Digital India ")
qplot(factor(score), data=digital_keyword, geom="bar", fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Customer Sentiment Scores - Digital India")
qplot(factor(polarity), data=Samsung_keyword, geom="bar", fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle("Customer Sentiments - Samsung Pay")
qplot(factor(score), data=Samsung_keyword, geom="bar", fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Customer Sentiment Scores - Samsung Pay ")
df = ddply(scores, c("keyword"), summarise,
           pos_count=sum( positive ),
           neg_count=sum( negative ),
           neu_count=sum(neutral))
df$total_count = df$pos_count +df$neg_count + df$neu_count
df$pos_prcnt_score = round( 100 * df$pos_count / df$total_count )
df$neg_prcnt_score = round( 100 * df$neg_count / df$total_count )
df$neu_prcnt_score = round( 100 * df$neu_count / df$total_count )

attach(df)
lbls <-paste(df$keyword,df$pos_prcnt_score)
lbls <- paste(lbls,"%",sep="")
pie(pos_prcnt_score, labels = lbls, col = rainbow(length(lbls)), main = "Positive Comparative Analysis - Payments")

lbls <-paste(df$keyword,df$neg_prcnt_score)
lbls <- paste(lbls,"%",sep="")
pie(neg_prcnt_score, labels = lbls, col = rainbow(length(lbls)), main = " Negative Comparative Analysis - Payments")

lbls <-paste(df$keyword,df$neu_prcnt_score)
lbls <- paste(lbls,"%",sep="")
pie(neu_prcnt_score, labels = lbls, col = rainbow(length(lbls)), main = "Neutral Comparative Analysis - Payments")