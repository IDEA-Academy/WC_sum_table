#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm) #text analysis
library(wordcloud)#word cloud
library(memoise) #updates word cloud when data refreshes
library("SnowballC")#used in text processing/ word cloud generation
library("RColorBrewer")#colour selection for word cloud
library("shinythemes")#themes for dashboard
library(DT)#text analysis
#install.packages("shinydashboard")
library(shinydashboard)#dashboard
library(plotly)

Deskpro<-read.csv("DeskPro.csv", header=T)#import table from wd
blue<- brewer.pal(9,"Blues")#set the blue palatte for use later
# The list of valid services
service <- list(Deskpro$Service)
Deskpro2<-Deskpro[Deskpro$Referrer.URL!="n/a",]#removing any URLs with NA
#count URLs and make into df
URL_count<-aggregate(data.frame(count=Deskpro2$Referrer.URL), list(value=Deskpro2$Referrer.URL, Deskpro2$Service),length)
#order descending from highest value
URL_count<-URL_count[order(-URL_count$count),]
str(URL_count)
#change column names
names(URL_count)=c("URL", "Service","Count")
##URL_count=URL_count[c("Service", "Count", "URL")]
#change order of columns
URL_count=data.frame(URL_count$Service, URL_count$Count, URL_count$URL)
#rename column names
names(URL_count)=c("Service", "Reported issues","URL")

#str(Deskpro2)
hourly_tick<-data.frame(Deskpro2$Timestamp, Deskpro2$Ticket.ID)
hourly_tick<-as.POSIXlt(hourly_tick$Deskpro2.Timestamp, format="%d/%m/%Y %H:%M")
#names(hourly_tick)
#head(tick2)
hourly_tick<-data.frame(hourly_tick,Deskpro2$Ticket.ID)
names(tick2)<-c("Date", "TicketID")
hour_count<-aggregate(list(TicketID=tick2$TicketID), list (hourofday=cut(tick2$Date, "1 hour")), length)
#str(hour_count)

hour_count$hourofday<-strptime(as.character(hour_count$hourofday), "%Y-%m-%d %H:%M:%S")
hour_count$hourofday<-format(hour_count$hourofday, "%d/%m/%y %H:%M")
#str(hour_count$hourofday)
# Define UI for application that draws a histogram
ui <- dashboardPage(
  header<-dashboardHeader(
    title="Deskpro Dashboard"),
  dashboardSidebar(disable = T),#no extra pages so have disabled this so far
body <- dashboardBody(
  fluidPage(
    
    theme = shinythemes::shinytheme("cerulean"),
    
    ## word cloud
    
    sidebarLayout(
      sidebarPanel(
        selectInput("selection", "Choose a service:",
                    c("All",
                      unique(as.character(Deskpro$Service)))),
        actionButton("update", "Update Service"),
        hr(),
        sliderInput("freq",
                    "Minimum Frequency:",
                    min = 1,  max = 400, value = 10),
        sliderInput("max",
                    "Maximum Number of Words:",
                    min = 1,  max = 100,  value = 50),
        mainPanel(column =1,position = "below",
                  plotOutput("plot",height= 400, width=400)
        )
        
      ),
      
      fluidRow(
        column(width=7,
               box(width=12,solidHeader=T,title="Number of Reported Issues by Service",
                   collapsible = T,collapsed = F,
                   selectInput("Service",
                               "Service:",
                               c("All",
                                 unique(as.character(URL_count$Service)))),       
                   DT::dataTableOutput("table")
                   
      
               ),
          fluidRow( 
            column(width=7,
            box(width=10, footer="Hover over bars for date, time and volume of raised tickets. Drag cusor to zoom in, double click to zoom out.", plotlyOutput("ticketChart"))))
               
        )
        
      )
    )
  )
)  
)
#adds header and sidebar options - to add extra pages to the sidebar

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #naming the function getTermMatrix for later 
  getTermMatrix <- memoise(function(services) {
    #renaming deskpro df to data to tie in with UI
    data <-Deskpro 
    #if all is not selected from drop-down menu, 
    #filter the table by the service selected and store it as data
    if (input$selection != "All") {
      data <- data[data$Service == input$selection,]
    }
    
    #create a corpus named docs from the messages
    docs <- Corpus(VectorSource(data$Message))
    #check the structure of the docs
    inspect(docs)
    #function to transform punctuation to whitespace
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    #removes the following characters and replaces with whitespace
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
    # Remove punctuation and stopwords
    docs <- tm_map(docs, removeWords, my_custom_stopwords)
    docs <- tm_map(docs, removePunctuation)
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)
    #store the data from the loop to this variable labelled dtm
    dtm <- TermDocumentMatrix(docs)
    #store dtm as a matrix
    m <- as.matrix(dtm)
    #sort the words, in descending order of frequency
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    head(d,10 )
    # str(docs)
    set.seed(1234)
    #producing the word cloud
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=50, random.order=FALSE, 
              colors=blue[5:9])
    
    sort(rowSums(m),decreasing=TRUE)
  })
  
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change the input when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Updating word cloud...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  #render the word cloud to an output plot
  output$plot <- renderPlot({
    v<-terms()
    par(bg="NA",mar=rep(1,4))#this line removes the background of the wordcloud
    wordcloud_rep(names(v), v,scale=c(5,0.3),
                  min.freq = input$freq, max.words=input$max,
                  random.order=F,
                  colors=blue[5:9])#adds the blue colour
  })
  #uses the data from earlier
  data <-URL_count
  #work out the mean for reported issues
  mean_issues<-mean(URL_count$`Reported issues`)
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable(rownames=F,options=list(
    lengthMenu = list(c(3, 10, 15, 20), c('3', '10','15', '20' )),
    pageLength = 3,
    # 
    initComplete = JS(
      
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#038fd2', 'color': '#fff'});",
      "}")
  ),
  #if all not selected, filter by the service                                             
  {
    data <-URL_count
    if (input$Service != "All") {
      data <- data[data$Service == input$Service,]
    }
    data
  })#format the table using reported issues column, any value above the mean
  #colour it in red
  %>%formatStyle(
    'Reported issues',
    backgroundColor = styleInterval(c(mean_issues), c('white', 'red'))
  )
  )
 
  output$ticketChart<-renderPlotly({
    plot_ly(hour_count,
            x=hour_count$hourofday,
            y=hour_count$TicketID,
            type="bar",
            showlegend = FALSE)%>%
      #hoverinfo="text",
      #text=paste(repo,"<br>",created_at,"<br> ",title),
      #marker=list(color=colStatus)) %>%
      
      layout(hovermode = "closest", barmode="stack",
             xaxis=list(title="",tickangle=55, ticklen=3, dtick=15, ticks="outside"), font=list(family="arial", size=10),
             yaxis=list(title="Volume of tickets raised per hour"), font=list(family="arial"),
             title="Deskpro Tickets",
             titlefont=list(size=16, family="arial"),
             margin=list(b=100,r=50)
      )
    
  }) 
}
# Run the application 
shinyApp(ui = ui, server = server)

