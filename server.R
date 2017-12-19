# Importing libraries
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)
#install.packages("leaflet")
library(leaflet)
#install.packages("rgdal")
library(rgdal)
#install.packages("DT")
library(DT)
#install.packages("plotrix")
library(plotrix)

# Load map

MYMap <- readOGR(dsn="C:/Users/faez/Desktop/Population-Hospital/Population-Hospital", layer="MYS_adm2")



# Transform to WGS884 reference system 
MYMap <- spTransform(MYMap, CRS("+init=epsg:4326"))

# Map edges
bounds <- bbox(MYMap)

# Load datasets 
district_population <-read.csv("C:/Users/faez/Desktop/Population-Hospital/Population-Hospital/District_Population.csv")
Hospital_Kerajaan <- read.csv("C:/Users/faez/Desktop/Population-Hospital/Population-Hospital/Hospital_Kerajaan.csv")
Klinik_Kesihatan_Kerajaan <- read.csv("C:/Users/faez/Desktop/Population-Hospital/Population-Hospital/Klinik_Kesihatan_Kerajaan.csv")

colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMin <- function(data) sapply(data, min, na.rm = TRUE)
colAve <- function(data) sapply(data, mean, na.rm = TRUE)

state_selected = ""

# Function of getting subset of district based on state chosen by user
function(input, output, session){
  
  
  
  # Return the requested dataset
    getDataSet<-reactive({
    
    # Get a subset of the population based on the drop down selection
    dataSet <- district_population[district_population$NAME_1==input$dataState,]
    
    # Copy map
    joinedDataset <- MYMap
    
    # Join the two datasets together
    joinedDataset@data <- suppressWarnings(left_join(joinedDataset@data, dataSet, by="NAME_2"))
    
    joinedDataset
    
  })
    
    datasetInput <- reactive({
      switch(input$dataset,
             "Hospital" = Hospital_Kerajaan,
             "Klinik" =  Klinik_Kesihatan_Kerajaan)
    })
  
  # Due to use of leafletProxy below, this should only be called once during first load
  output$StateMap<-renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      # To fit Johor map during first load
      fitBounds(102.710688, 2.810081, 104.180109, 1.371868)
  })
  
  observe({
    
    
    theData<-getDataSet() 
    
    # Color palette mapped to population data on map
    pal <- colorQuantile("YlOrRd", theData$Total, n = 8)
    
    # Set text for the clickable popup labels
    # When user click on the map at certain district, the population information will be shown
    borough_popup <- paste0("<strong>District: </strong>", 
                            theData$NAME_2,
                            "<br><strong>",
                            "Population: </strong>", 
                            formatC(theData$Total, format="d", big.mark=','))
    
    
    # If the data changes, the polygons are cleared and redrawn, however, the map is not redrawn
    leafletProxy("StateMap", data = theData) %>%
      addProviderTiles("CartoDB.Positron") %>%
      clearShapes() %>%
      addPolygons(data = theData,
                  fillColor = pal(theData$Total), 
                  fillOpacity = 0.7,
                  smoothFactor = 0.1,
                  color = "grey",
                  stroke = TRUE,
                  weight = 1,
                  popup = borough_popup,
                  highlight = highlightOptions(
                    weight = 4,
                    color = "grey",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE))
  })

  
  
  
  
  # Observe on every state seection by user  
  observeEvent(input$dataState, {
  
    proxy <- leafletProxy("StateMap")
    
    data <- datasetInput()
    
    state_selected <- input$dataState
    
    Pop_State <- filter(district_population, NAME_1==state_selected)
    HK_PK_DATA <- filter(data, NAME_1==state_selected)
    
    pal <- colorQuantile("YlOrRd", Pop_State$Total, n = 8)
    icons <- makeAwesomeIcon(
      icon = 'medkit',
      iconColor = 'white',
      library = 'fa',
      markerColor = 'red'
    )
    
   proxy %>% fitBounds(lng1 = max(HK_PK_DATA$longitude),lat1 = max(HK_PK_DATA$latitude),
                       lng2 = min(HK_PK_DATA$longitude),lat2 = min(HK_PK_DATA$latitude))%>% clearMarkers() %>% clearControls() %>%
      addMarkers(data=HK_PK_DATA, lng=HK_PK_DATA$longitude, HK_PK_DATA$latitude, label=HK_PK_DATA$name, labelOptions = labelOptions(noHide = F, direction = 'top', offset=c(0,-45))) %>%
      #addMarkers(data=PK_State, lng=PK_State$longitude, PK_State$latitude, label=PK_State$name, labelOptions = labelOptions(noHide = F, direction = 'top', offset=c(0,-45))) %>%
      addLegend(
        pal = pal, 
        values = Pop_State$Total, 
        opacity = 0.7, 
        title = "Population",
        position = "bottomright")
    
    output$Plot <- renderPlot ({
      # Convertion to Percentage
      perLabel <- 0
      Total <- Pop_State$Total
      percentage <- round(Total/sum(Total) * 100)
      perLabel <- percentage
      perLabel <- paste(Pop_State$NAME_2,"\n", perLabel, "%", sep = "")
      
      pie3D(Pop_State$Total, labels = perLabel, main = "Population Fraction in Johor", explode=0.2, radius=0.8, labelcex = 0.9,  start= 0.6, theta=pi/4, shade = 0.5)
    })
    
    output$Bar <- renderPlot ({
      format(Pop_State$Total, scientific=FALSE)
      barplot(Pop_State$Total, main = "Population Fraction in Johor", ylim=c(0,1500000), xlab = "District", ylab = "Population", col = Pop_State$NAME_2, names.arg = Pop_State$NAME_2, horiz=FALSE)
    })
    
  })
  
  
  # year selecter; values based on those present in the dataset
  output$stateSelect<-renderUI({
    district_population <- district_population[-c(142,143,144,64),]
    stateRange <- sort(unique(as.character(district_population$NAME_1)), decreasing=FALSE)
    selectInput("dataState", "State", choices=stateRange, selected=stateRange[1])
  })
  
  
  
}