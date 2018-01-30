setwd("C:/Users/AT003502/Documents/Emma/FB dashboard/facebook data with organic likes/Dashboard/")
library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(wordcloud)
library("SnowballC")
library("RColorBrewer")

Deskpro<-read.csv("DeskPro.csv", header=T)
# The list of valid services
service <- list(Deskpro$Service)
URL_count<-aggregate(data.frame(count=Deskpro$Referrer.URL), list(value=Deskpro$Referrer.URL, Deskpro$Service),length)
head(URL_count,10)
str(URL_count)
URL_count<-URL_count[order(-URL_count$count),]
names(URL_count)=c("URL", "Service","Count")
URL_count


function(input, output, session) {
  
  getTermMatrix <- memoise(function(services) {
    data <-Deskpro 
    if (input$selection != "All") {
      data <- data[data$Service == input$selection,]
    }
    
    docs <- Corpus(VectorSource(data$Message))
    
    inspect(docs)
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    docs <- tm_map(docs, toSpace, ":")
    
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
    str(docs)
    set.seed(1234)
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=50, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "RdYlGn"))
    
    sort(rowSums(m),decreasing=TRUE)
  })

  #put session inside brackets
  # Define a reactive expression for the document term matrix
  terms <- reactive({
  # Change when the "update" button is pressed...
   input$update
  # ...but not for anything else
  isolate({
   withProgress({
    setProgress(message = "Processing corpus...")
    getTermMatrix(input$selection)
    })
  })
})
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v<-terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
              min.freq = input$freq, max.words=input$max,
              colors=brewer.pal(8, "RdYlGn"))
    })
  
  # output$words_Plot <- renderPlot({
  #    findFreqTerms(dtm, lowfreq = 50)
  #  words_Plot<-barplot(d[1:10,]$freq, 
  #                     las = 2, names.arg = d[1:10,]$word)
  # })
  
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <-URL_count
    if (input$Service != "All") {
      data <- data[data$Service == input$Service,]
    }
    data
  }))
  
}