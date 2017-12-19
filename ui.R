#Importing libraries
#install.packages("shinydashboard")
library(shinydashboard)
#install.packages("leaflet")
library(leaflet)
#install.packages("DT")
library(DT)

header<-dashboardHeader(title="Healthcare Services vs Population in Malaysia", titleWidth = 450)

body<-dashboardBody(
  fluidRow(
    column(width = 8,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("StateMap", height=800)
           )
    ),
    # Design - version 1.1
    column(width=4,
           mainPanel(
             selectInput("dataset", "Choose a dataset:", 
                         choices = c("Hospital", "Klinik")),
             
             uiOutput("stateSelect"),
             
             tabsetPanel(
               tabPanel("Pie Chart", plotOutput("Plot", height = "600", width = "100%")),
               tabPanel("Bar Chart", plotOutput("Bar", height = "600", width = "100%"))
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
    )
    
    
    
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)