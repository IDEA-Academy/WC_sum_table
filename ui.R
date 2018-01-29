
#####actual set up of page
fluidPage(
  
  titlePanel("Deskpro Issues"),
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(2,
           selectInput("Service",
                       "Service:",
                       c("All",
                         unique(as.character(URL_count$Service)))))
  ),
  #column(1,
  #       selectInput("Ticket",
  #                   "Type of request:",
  #                   c("All",
  #                     unique(as.character(URL_count$Ticket))))
  # )
  # ),
  # Create a new row for the table.
  fluidRow(
    DT::dataTableOutput("table")
  ),
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selection", "Choose a service:",
                  choices = Deskpro$Service),
      actionButton("update", "Update"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100)
    ),
  #conditionalPanel(
  # 'input.dataset === "Deskpro"',
  # helpText("Click the column header to sort a column."),
  
  mainPanel(
    plotOutput("plot", "800px", "600px"),
    # mainPanel(
    #   plotOutput("words_Plot"),
    mainPanel(
      tabsetPanel(
        id = 'URL_count',
        tabPanel("URL_count", DT::dataTableOutput("mytable1"))
        
        
        # )
         )
      )
    )  
  )
)
