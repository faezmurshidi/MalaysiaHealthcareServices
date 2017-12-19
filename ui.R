#Importing libraries
#install.packages("shinydashboard")
library(shinydashboard)
#install.packages("leaflet")
library(leaflet)
#install.packages("DT")
library(DT)

header<-dashboardHeader(title="Population vs Hospitals in Malaysia", titleWidth = 450)

body<-dashboardBody(
  fluidRow(
    column(width = 8,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("StateMap", height=540)
           )
    ),
# Design - version 1.1
        column(width=4,
           mainPanel(
             tabsetPanel(
               tabPanel("Pie Chart", plotOutput("Plot", height = "520", width = "400")),
               tabPanel("Bar Chart", plotOutput("Bar", height = "520", width = "400"))
             )
           )
# Design - version 1.0           
#           box(width=NULL, 
#               uiOutput("stateSelect")
#           ),
#           box(width = NULL, solidHeader = TRUE,
#               plotOutput("Plot", height=560)
#           ),
#           box(width = NULL, solidHeader = TRUE,
#               #plotOutput("Bar", height=265)
#           )
            ),
# Design - version 1.1
    absolutePanel(top = 80, right = 500,
                  width = 200, 
                  draggable = FALSE,
                  uiOutput("stateSelect"))
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)