
#####actual set up of page

header<-dashboardHeader(
  title="Deskpro Dashboard"
)
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
               collapsible = T,collapsed = T,
             selectInput("Service",
                         "Service:",
                         c("All",
                           unique(as.character(URL_count$Service)))),       
             DT::dataTableOutput("table")
           )
           
           )

         )
        )
      )
  )  
#adds header and sidebar options - to add extra pages to the sidebar
dashboardPage(
  header,
  dashboardSidebar(disable = T),#no extra pages so have disabled this so far
  body

)