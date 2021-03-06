library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(wordcloud)
library("SnowballC")
library("RColorBrewer")
library(dplyr)
library("shinythemes")

setwd("C:/Users/AT003502/Documents/Emma/FB dashboard/facebook data with organic likes/Dashboard/")

Deskpro<-read.csv("DeskPro.csv", header=T)
# The list of valid services
service <- list(Deskpro$Service)
Deskpro2<-Deskpro[Deskpro$Referrer.URL!="n/a",]
str(Deskpro2)
URL_count<-aggregate(data.frame(count=Deskpro2$Referrer.URL), list(value=Deskpro2$Referrer.URL, Deskpro2$Service),length)
head(URL_count,10)
str(URL_count)
URL_count<-URL_count[order(-URL_count$count),]
names(URL_count)=c("URL", "Service","Count")
URL_count
URL_count=data.frame(URL_count$Service, URL_count$Count, URL_count$URL)
names(URL_count)=c("Service", "Reported issues","URL")
URL_count
# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(services) {
  # Carefluword not to let just any name slip in here; a
  # malicious user could manipulate this value.
  #if (!(service %in% service))
    #stop("Unknown service")
  
  docs <- Corpus(VectorSource(Deskpro$Message))
  inspect(docs)
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  docs <- tm_map(docs, toSpace, ":")
  docs <- tm_map(docs, toSpace, "\n")
  docs <- tm_map(docs, toSpace, "&#039;")
  
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  my_custom_stopwords<-c("What were you trying to do", 
                         "What were you doing",
                         "need", 
                         "help",
                         "password", 
                         "login",
                         "what do you need help with",
                         "tried",
                         "trying",
                         "log",
                         "tax"
  ) 
  # Remove punctuations)
  docs <- tm_map(docs, removeWords, my_custom_stopwords)
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  docs <- tm_map(docs, stemDocument)
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d,10 )
  set.seed(1234)

  par(bg="NA")
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=50,random.order=FALSE,
            colors=brewer.pal(8, "RdYlGn"))
  #rot.per=0.1,
  sort(rowSums(m),decreasing=TRUE)
  
})
  

