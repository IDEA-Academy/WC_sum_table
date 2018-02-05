setwd("C:/Users/AT003502/Documents/Emma/FB dashboard/facebook data with organic likes/Dashboard/")
library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(wordcloud)
library("SnowballC")
library("RColorBrewer")
library("shinythemes")
library(DT)

Deskpro<-read.csv("DeskPro.csv", header=T)
# The list of valid services
service <- list(Deskpro$Service)
Deskpro2<-Deskpro[Deskpro$Referrer.URL!="n/a",]
#str(Deskpro2)
URL_count<-aggregate(data.frame(count=Deskpro2$Referrer.URL), list(value=Deskpro2$Referrer.URL, Deskpro2$Service),length)
head(URL_count,10)
str(URL_count)
URL_count<-URL_count[order(-URL_count$count),]
names(URL_count)=c("URL", "Service","Count")
##URL_count=URL_count[c("Service", "Count", "URL")]
URL_count=data.frame(URL_count$Service, URL_count$Count, URL_count$URL)
URL_count
names(URL_count)=c("Service", "Reported issues","URL")

function(input, output, session) {
  
  getTermMatrix <- memoise(function(services) {
    data <-Deskpro 
    if (input$selection != "All") {
      data <- data[data$Service == input$selection,]
    }
    #else if  (input$selection == "All") {
    #  data <- Deskpro
    #}
    docs <- Corpus(VectorSource(data$Message))
    
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
    my_custom_stopwords<-c("what were you trying to do", 
                           "what were you doing",
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
   # str(docs)
    set.seed(1234)
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=50, random.order=FALSE, 
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
    par(bg="NA", mar=rep(1,4))
    wordcloud_rep(names(v), v,scale=c(5,0.3),
              min.freq = input$freq, max.words=input$max,
              random.order=F,
              colors=brewer.pal(8, "RdYlGn"))#scale=c(3,0.),
    })
  
  # output$words_Plot <- renderPlot({
  #    findFreqTerms(dtm, lowfreq = 50)
  #  words_Plot<-barplot(d[1:10,]$freq, 
  #                     las = 2, names.arg = d[1:10,]$word)
  # })
  
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable(rownames=F,{
    data <-URL_count
    if (input$Service != "All") {
      data <- data[data$Service == input$Service,]
    }
    data
  }))
}
  