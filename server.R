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
MYMap <- readOGR(dsn="../Population-Hospital", layer="MYS_adm2")

# Transform to WGS884 reference system 
MYMap <- spTransform(MYMap, CRS("+init=epsg:4326"))

# Map edges
bounds <- bbox(MYMap)

# Load datasets 
district_population <-read.csv("District_Population.csv")
Hospital_Kerajaan <- read.csv("Hospital_Kerajaan.csv")
Klinik_Kesihatan_Kerajaan <- read.csv("Klinik_Kesihatan_Kerajaan.csv")

# Function of getting subset of district based on state chosen by user
function(input, output, session){
  
  getDataSet<-reactive({
    
    # Get a subset of the population based on the drop down selection
    dataSet <- district_population[district_population$NAME_1==input$dataState,]
    
    # Copy map
    joinedDataset <- MYMap
    
    # Join the two datasets together
    joinedDataset@data <- suppressWarnings(left_join(joinedDataset@data, dataSet, by="NAME_2"))
    
    joinedDataset
    
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
    if (input$dataState=="Johor") {
      Pop_State <- filter(district_population, NAME_1=="Johor")
      HK_State <- filter(Hospital_Kerajaan, NAME_1=="Johor")
      PK_State <- filter(Klinik_Kesihatan_Kerajaan, NAME_1=="Johor")
      pal <- colorQuantile("YlOrRd", Pop_State$Total, n = 8)
      icons <- makeAwesomeIcon(
        icon = 'medkit',
        iconColor = 'white',
        library = 'fa',
        markerColor = 'red'
      )
      proxy %>% fitBounds(102.710688, 2.810081, 104.180109, 1.371868)%>% clearMarkers() %>% clearControls() %>%
        addMarkers(data=HK_State, lng=HK_State$longitude, HK_State$latitude, label=HK_State$name, labelOptions = labelOptions(noHide = F, direction = 'top', offset=c(0,-45))) %>%
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
    }
  })
  
  observeEvent(input$dataState, {
    proxy <- leafletProxy("StateMap")
    if (input$dataState=="Kedah") {
      Pop_State <- filter(district_population, NAME_1=="Kedah")
      HK_State <- filter(Hospital_Kerajaan, NAME_1=="Kedah")
      PK_State <- filter(Klinik_Kesihatan_Kerajaan, NAME_1=="Kedah")
      pal <- colorQuantile("YlOrRd", Pop_State$Total, n = 8)
      proxy %>% fitBounds(100.389085, 6.469794, 100.551134, 5.130890)%>% clearMarkers() %>% clearControls() %>%
        addMarkers(data=HK_State, lng=HK_State$longitude, HK_State$latitude, label=HK_State$name, labelOptions = labelOptions(noHide = F, direction = 'top', offset=c(0,-45))) %>%
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
        
        pie3D(Pop_State$Total, labels = perLabel, main = "Population Fraction in Kedah", explode=0.2, radius=0.8, labelcex = 0.9,  start= 0.6, theta=pi/4, shade = 0.5)
      })
      
      output$Bar <- renderPlot ({
        format(Pop_State$Total, scientific=FALSE)
        barplot(Pop_State$Total, main = "Population Fraction in Kedah", ylim=c(0,500000), xlab = "District", ylab = "Population", col = Pop_State$NAME_2, names.arg = Pop_State$NAME_2, horiz=FALSE)
      })
    }
  })
  
  observeEvent(input$dataState, {
    proxy <- leafletProxy("StateMap")
    if (input$dataState=="Kelantan") {
      Pop_State <- filter(district_population, NAME_1=="Kelantan")
      HK_State <- filter(Hospital_Kerajaan, NAME_1=="Kelantan")
      PK_State <- filter(Klinik_Kesihatan_Kerajaan, NAME_1=="Kelantan")
      pal <- colorQuantile("YlOrRd", Pop_State$Total, n = 8)
      proxy %>% fitBounds(102.105699, 6.174968, 101.421800, 4.668415)%>% clearMarkers() %>% clearControls() %>%
        addMarkers(data=HK_State, lng=HK_State$longitude, HK_State$latitude, label=HK_State$name, labelOptions = labelOptions(noHide = F, direction = 'top', offset=c(0,-45))) %>%
      addLegend(
        pal = pal, 
        values = Pop_State$Total, 
        opacity = 0.7, 
        title = "Population",
        position = "bottomright")
      
      output$Plot <- renderPlot ({
        #Convertion to Percentage
        perLabel <- 0
        Total <- Pop_State$Total
        percentage <- round(Total/sum(Total) * 100)
        perLabel <- percentage
        perLabel <- paste(Pop_State$NAME_2,"\n", perLabel, "%", sep = "")
        
        pie3D(Pop_State$Total, labels = perLabel, main = "Population Fraction in Kelantan", explode=0.2, radius=0.8, labelcex = 0.9,  start= 0.4, theta=pi/4, shade = 0.5)
      })
      
      output$Bar <- renderPlot ({
        format(Pop_State$Total, scientific=FALSE)
        barplot(Pop_State$Total, main = "Population Fraction in Kelantan",xlab = "District", ylim=c(0,500000), ylab = "Population", col = Pop_State$NAME_2, names.arg = Pop_State$NAME_2, horiz=FALSE)
      })
    }
  })
  
  observeEvent(input$dataState, {
    proxy <- leafletProxy("StateMap")
    if (input$dataState=="Melaka") {
      Pop_State <- filter(district_population, NAME_1=="Melaka")
      HK_State <- filter(Hospital_Kerajaan, NAME_1=="Melaka")
      PK_State <- filter(Klinik_Kesihatan_Kerajaan, NAME_1=="Melaka")
      pal <- colorQuantile("YlOrRd", Pop_State$Total, n = 8) 
      proxy %>% fitBounds(102.234788, 2.502993, 102.492967, 2.096827)%>% clearMarkers() %>% clearControls() %>%
        addMarkers(data=HK_State, lng=HK_State$longitude, HK_State$latitude, label=HK_State$name, labelOptions = labelOptions(noHide = F, direction = 'top', offset=c(0,-45))) %>%
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
        
        pie3D(Pop_State$Total, labels = perLabel, main = "Population Fraction in Melaka", explode=0.2, radius=0.8, labelcex = 0.9,  start= 0.7, theta=pi/4, shade = 0.5)
      })
      
      output$Bar <- renderPlot ({
        format(Pop_State$Total, scientific=FALSE)
        barplot(Pop_State$Total, main = "Population Fraction in Melaka",xlab = "District", ylim=c(0,500000), ylab = "Population", col = Pop_State$NAME_2, names.arg = Pop_State$NAME_2, horiz=FALSE)
      })
    }
  })
  
  observeEvent(input$dataState, {
    proxy <- leafletProxy("StateMap")
    if (input$dataState=="Negeri Sembilan") {
      Pop_State <- filter(district_population, NAME_1=="Negeri Sembilan")
      HK_State <- filter(Hospital_Kerajaan, NAME_1=="Negeri Sembilan")
      PK_State <- filter(Klinik_Kesihatan_Kerajaan, NAME_1=="Negeri Sembilan")
      pal <- colorQuantile("YlOrRd", Pop_State$Total, n = 8)
      proxy %>% fitBounds(101.943651, 3.301217, 102.542406, 2.439881)%>% clearMarkers() %>% clearControls() %>%
        addMarkers(data=HK_State, lng=HK_State$longitude, HK_State$latitude, label=HK_State$name, labelOptions = labelOptions(noHide = F, direction = 'top', offset=c(0,-45))) %>%
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
        
        pie3D(Pop_State$Total, labels = perLabel, main = "Population Fraction in Negeri Sembilan", explode=0.2, radius=0.8, labelcex = 0.9,  start= 0.7, theta=pi/4, shade = 0.5)
      })
      
      output$Bar <- renderPlot ({
        format(Pop_State$Total, scientific=FALSE)
        barplot(Pop_State$Total, main = "Population Fraction in Negeri Sembilan",xlab = "District", ylim=c(0,550000), ylab = "Population", col = Pop_State$NAME_2, names.arg = Pop_State$NAME_2, horiz=FALSE)
      })
    }
  })
  
  observeEvent(input$dataState, {
    proxy <- leafletProxy("StateMap")
    if (input$dataState=="Pahang") {
      Pop_State <- filter(district_population, NAME_1=="Pahang")
      HK_State <- filter(Hospital_Kerajaan, NAME_1=="Pahang")
      PK_State <- filter(Klinik_Kesihatan_Kerajaan, NAME_1=="Pahang")
      pal <- colorQuantile("YlOrRd", Pop_State$Total, n = 8)
      proxy %>% fitBounds(103.437791, 2.656646, 102.020555, 4.684840)%>% clearMarkers() %>% clearControls() %>%
        addMarkers(data=HK_State, lng=HK_State$longitude, HK_State$latitude, label=HK_State$name, labelOptions = labelOptions(noHide = F, direction = 'top', offset=c(0,-45))) %>%
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
        
        pie3D(Pop_State$Total, labels = perLabel, main = "Population Fraction in Pahang", explode=0.2, radius=0.8, labelcex = 0.9,  start= 1.5, theta=pi/4, shade = 0.5)
      })
      
      output$Bar <- renderPlot ({
        format(Pop_State$Total, scientific=FALSE)
        barplot(Pop_State$Total, main = "Population Fraction in Pahang",xlab = "District", ylim=c(0,500000), ylab = "Population", col = Pop_State$NAME_2, names.arg = Pop_State$NAME_2, horiz=FALSE)
      })
    }
  })
  
  observeEvent(input$dataState, {
    proxy <- leafletProxy("StateMap")
    if (input$dataState=="Perak") {
      Pop_State <- filter(district_population, NAME_1=="Perak")
      HK_State <- filter(Hospital_Kerajaan, NAME_1=="Perak")
      PK_State <- filter(Klinik_Kesihatan_Kerajaan, NAME_1=="Perak")
      pal <- colorQuantile("YlOrRd", Pop_State$Total, n = 8)
      proxy %>% fitBounds(101.278978, 3.742581, 101.482225, 5.828064)%>% clearMarkers() %>% clearControls() %>%
        addMarkers(data=HK_State, lng=HK_State$longitude, HK_State$latitude, label=HK_State$name, labelOptions = labelOptions(noHide = F, direction = 'top', offset=c(0,-45))) %>%
      addLegend(
        pal = pal, 
        values = Pop_State$Total, 
        opacity = 0.7, 
        title = "Population",
        position = "bottomright")
      
      output$Plot <- renderPlot ({
        #Convertion to Percentage
        perLabel <- 0
        Total <- Pop_State$Total
        percentage <- round(Total/sum(Total) * 100)
        perLabel <- percentage
        perLabel <- paste(Pop_State$NAME_2,"\n", perLabel, "%", sep = "")
        
        pie3D(Pop_State$Total, labels = perLabel, main = "Population Fraction in Perak", explode=0.2, radius=0.8, labelcex = 0.9,  start= 0.9, theta=pi/4, shade = 0.5)
      })
      
      output$Bar <- renderPlot ({
        format(Pop_State$Total, scientific=FALSE)
        barplot(Pop_State$Total, main = "Population Fraction in Perak",xlab = "District", ylim=c(0,800000), ylab = "Population", col = Pop_State$NAME_2, names.arg = Pop_State$NAME_2, horiz=FALSE)
      })
    }
  })
  
  observeEvent(input$dataState, {
    proxy <- leafletProxy("StateMap")
    if (input$dataState=="Pulau Pinang") {
      Pop_State <- filter(district_population, NAME_1=="Pulau Pinang")
      HK_State <- filter(Hospital_Kerajaan, NAME_1=="Pulau Pinang")
      PK_State <- filter(Klinik_Kesihatan_Kerajaan, NAME_1=="Pulau Pinang")
      pal <- colorQuantile("YlOrRd", Pop_State$Total, n = 8)
      proxy %>% fitBounds(100.339647, 5.112425, 100.320421, 5.600547)%>% clearMarkers() %>% clearControls() %>%
        addMarkers(data=HK_State, lng=HK_State$longitude, HK_State$latitude, label=HK_State$name, labelOptions = labelOptions(noHide = F, direction = 'top', offset=c(0,-45))) %>%
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
        
        pie3D(Pop_State$Total, labels = perLabel, main = "Population Fraction in Pulau Pinang", explode=0.2, radius=0.8, labelcex = 0.9,  start= 0.4, theta=pi/4, shade = 0.5)
      })
      
      output$Bar <- renderPlot ({
        format(Pop_State$Total, scientific=FALSE)
        barplot(Pop_State$Total, main = "Population Fraction in Pulau Pinang",xlab = "District", ylab = "Population", col = Pop_State$NAME_2, names.arg = Pop_State$NAME_2, horiz=FALSE)
      })
    }
  })
  
  observeEvent(input$dataState, {
    proxy <- leafletProxy("StateMap")
    if (input$dataState=="Sabah") {
      Pop_State <- filter(district_population, NAME_1=="Sabah")
      HK_State <- filter(Hospital_Kerajaan, NAME_1=="Sabah")
      PK_State <- filter(Klinik_Kesihatan_Kerajaan, NAME_1=="Sabah")
      pal <- colorQuantile("YlOrRd", Pop_State$Total, n = 8)
      proxy %>% fitBounds(116.972948, 7.438328, 117.500291, 4.280275)%>% clearMarkers() %>% clearControls() %>%
        addMarkers(data=HK_State, lng=HK_State$longitude, HK_State$latitude, label=HK_State$name, labelOptions = labelOptions(noHide = F, direction = 'top', offset=c(0,-45))) %>%
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
        
        label_sabah <- c("", "Beluran", "Keningau", "Kinabatangan", "", "Kota Kinabalu", "", "", "", "", "Lahad Datu", "", "Papar", "Penampang", "", "", "", "Sandakan", "Semporna", "", "", "Tawau", "", "", "Tuaran")
        pie3D(Pop_State$Total, labels = label_sabah, main = "Population Fraction in Sabah", explode=0.1, radius=0.8, labelcex = 0.9,  start= 0.0, theta=pi/4, shade = 0.5)
      })
      
      output$Bar <- renderPlot ({
        format(Pop_State$Total, scientific=FALSE)
        barplot(Pop_State$Total, main = "Population Fraction in Sabah",ylim=c(0,500000), xlab = "District", ylab = "Population", col = Pop_State$NAME_2, names.arg = Pop_State$NAME_2, horiz=FALSE)
      })
    }
  })
  
  observeEvent(input$dataState, {
    proxy <- leafletProxy("StateMap")
    if (input$dataState=="Sarawak") {
      Pop_State <- filter(district_population, NAME_1=="Sarawak")
      HK_State <- filter(Hospital_Kerajaan, NAME_1=="Sarawak")
      PK_State <- filter(Klinik_Kesihatan_Kerajaan, NAME_1=="Sarawak")
      pal <- colorQuantile("YlOrRd", Pop_State$Total, n = 8)
      proxy %>% fitBounds(114.231859, 1.345295, 114.682298, 5.227312)%>% clearMarkers() %>% clearControls() %>%
        addMarkers(data=HK_State, lng=HK_State$longitude, HK_State$latitude, label=HK_State$name, labelOptions = labelOptions(noHide = F, direction = 'top', offset=c(0,-45))) %>%
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
        
        label_sarawak <- c("", "", "", "", "Bintulu", "", "", "", "", "", "Kuching", "", "", "", "", "Marudi", "", "", "Miri", "", "", "Samarahan", "", "Sarikei", "", "Serian", "Sibu", "", "", "Sri Aman", "")
        pie3D(Pop_State$Total, labels = label_sarawak, main = "Population Fraction in Sarawak", explode=0.1, radius=0.9, labelcex=0.9,  start=0.9, theta=pi/pi, shade = 0.0)
      })
      
      output$Bar <- renderPlot ({
        format(Pop_State$Total, scientific=FALSE)
        barplot(Pop_State$Total, main = "Population Fraction in Sarawak", ylim=c(0,600000), xlab = "District", ylab = "Population", col = Pop_State$NAME_2, names.arg = Pop_State$NAME_2, horiz=FALSE)
      })
    }
  })
  
  observeEvent(input$dataState, {
    proxy <- leafletProxy("StateMap")
    if (input$dataState=="Selangor") {
      Pop_State <- filter(district_population, NAME_1=="Selangor")
      HK_State <- filter(Hospital_Kerajaan, NAME_1=="Selangor")
      PK_State <- filter(Klinik_Kesihatan_Kerajaan, NAME_1=="Selangor")
      pal <- colorQuantile("YlOrRd", Pop_State$Total, n = 8)
      proxy %>% fitBounds(100.839525, 3.828224, 101.693712, 2.643614)%>% clearMarkers() %>% clearControls() %>%
        addMarkers(data=HK_State, lng=HK_State$longitude, HK_State$latitude, label=HK_State$name, labelOptions = labelOptions(noHide = F, direction = 'top', offset=c(0,-45))) %>%
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
        
        pie3D(Pop_State$Total, labels = perLabel, main = "Population Fraction in Selangor", explode=0.2, radius=0.8, labelcex = 0.9,  start= 0.2, theta=pi/4, shade = 0.5)
      })
      
      output$Bar <- renderPlot ({
        format(Pop_State$Total, scientific=FALSE)
        barplot(Pop_State$Total, main = "Population Fraction in Selangor",xlab = "District", ylim=c(0,1800000), ylab = "Population", col = Pop_State$NAME_2, names.arg = Pop_State$NAME_2, horiz=FALSE)
      })
    }
  })
  
  observeEvent(input$dataState, {
    proxy <- leafletProxy("StateMap")
    if (input$dataState=="Terengganu") {
      Pop_State <- filter(district_population, NAME_1=="Terengganu")
      HK_State <- filter(Hospital_Kerajaan, NAME_1=="Terengganu")
      PK_State <- filter(Klinik_Kesihatan_Kerajaan, NAME_1=="Terengganu")
      pal <- colorQuantile("YlOrRd", Pop_State$Total, n = 8)
      proxy %>% fitBounds(102.528673, 5.806888, 103.253770, 4.058391)%>% clearMarkers() %>% clearControls() %>%
        addMarkers(data=HK_State, lng=HK_State$longitude, HK_State$latitude, label=HK_State$name, labelOptions = labelOptions(noHide = F, direction = 'top', offset=c(0,-45))) %>%
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
        
        pie3D(Pop_State$Total, labels = perLabel, main = "Population Fraction in Terengganu", explode=0.2, radius=0.8, labelcex = 0.9,  start= 0.7, theta=pi/4, shade = 0.5)
      })
      
      output$Bar <- renderPlot ({
        format(Pop_State$Total, scientific=FALSE)
        barplot(Pop_State$Total, main = "Population Fraction in Terengganu",ylim=c(0,400000), xlab = "District", ylab = "Population", col = Pop_State$NAME_2, names.arg = Pop_State$NAME_2, horiz=FALSE)
      })
    }
  })
  
  
  # year selecter; values based on those present in the dataset
  output$stateSelect<-renderUI({
    district_population <- district_population[-c(142,143,144,64),]
    stateRange <- sort(unique(as.character(district_population$NAME_1)), decreasing=FALSE)
    selectInput("dataState", "State", choices=stateRange, selected=stateRange[1])
  })
  
}