
#####actual set up of page

fluidPage(
  theme = shinythemes::shinytheme("cosmo"),
  titlePanel("Deskpro Issues"),
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
  

      
  #)
           
    #conditionalPanel(
    # 'input.dataset === "Deskpro"',
    # helpText("Click the column header to sort a column."),
    

    
   # for URL table
#  fluidRow(
#    column(3,
#           selectInput("Service",
#                       "Service:",
##                       c("All",
  #                       unique(as.character(URL_count$Service)))))
#  ),
  
   # mainPanel(
  #    column(1,
    #  plotOutput("plot", "500px","500px")
 # ),

fluidRow(
    column(2,
           selectInput("Service",
                       "Service:",
                       c("All",
                         unique(as.character(URL_count$Service)))),       
           DT::dataTableOutput("table")
           )
   # mainPanel(
   #   plotOutput("plot", "400px","600px")
    
    #fluidRow(
   #   column(1,
             #plotOutput("plot", "400px","600px")
     # )
      
  
    # mainPanel(
    #   plotOutput("words_Plot"),
   # mainPanel(
    #  tabsetPanel(
     #   id = 'URL_count',
      #  tabPanel("URL_count", DT::dataTableOutput("mytable1"))
        
        
         )
        )
      )
  # )  
 # )
#)
