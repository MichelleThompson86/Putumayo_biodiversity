

library(shiny)
library(leaflet)
library(tidyverse)
library(rgbif)
library(rgdal)
library(viridis)

#load data and change column names

Herps_Put <-read.csv(file ="Herps_Put.csv")
Herps_Put_map <-read.csv(file ="Herps_Put_map.csv")
Putbasin <- readOGR("Geo/Putmayo_WaterShed.shp")



#Herps_Put <- dplyr::rename(Herps_Put, latitude = decimalLatitude, 
 #                        longitude = decimalLongitude)

ui <- fluidPage(
  titlePanel("Putumayo Watersehd GBIF Data"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("Herps_output"),
      br(),
      downloadButton("download_filtered", "csv Download Filtered"),   
      downloadButton("downloadData", "csv Download All"),
      br(),
      br()

    ),
    mainPanel(width = 8, # maximum of 12
              tabsetPanel(type = "tabs",
                          tabPanel("Map", leafletOutput("mymap")),
                          tabPanel("Summary", plotOutput("summary"))
                          
              )
    )
  )
)


# server controls

server <- function(input, output) {
  output$Herps_output <- renderUI({
    selectInput(inputId = "Class_input", "Class", sort(unique(Herps_Put$class)), 
                selected = "Amphibia")
  })
  # use renderLeaflet for elements of the map that don't change, note setting
  # default sizes
  #leafletmap
  #convert forleaflet
  shapeData <- spTransform(Putbasin, CRS("+proj=longlat +datum=WGS84"))
  
  output$mymap <- renderLeaflet({
    leaflet(Herps_Put_map,options = leafletOptions(preferCanvas = TRUE)) %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
  #  addCircles(~longitude, ~latitude, popup = Herps_Put$species)%>%
    addPolygons(data =shapeData, weight=5,col = 'red')
})
  
  # Use leafletProxy for elements that change
  
  observe({
    set <- Herps_Put_map %>% filter(Herps_Put_map$class %in% input$Class_input)
    
    leafletProxy("mymap") %>% clearMarkers() %>% addCircleMarkers(lng = set$longitude, 
                                                                  lat = set$latitude, radius=1, weight=3,
                                                                    opacity = 0.5, fill = TRUE, fillOpacity = 0.2)
  })
  
  
  output$summary <- renderPlot({
    ggplot(Herps_Put, aes(x=factor(class)))+
     geom_bar(stat="count", fill="blue1")+
      geom_text(stat='count', aes(label=..count..), vjust=-0.2, size=6)+
      theme_minimal()+
      labs(x="Class", y="Records")+
      theme_bw()+
      theme(
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.x=element_text(size = 16),
        axis.text.y=element_text(size = 13),
        axis.title.y=element_text(size = 16),
        axis.title.x=element_text(size = 16))+
      theme(plot.title = element_text(size = 18))+
      ggtitle("Number of records")
  
  })
  
  ggplot(mtcars, aes(x=factor(cyl)))+
    geom_bar(stat="bin", width=0.7, fill="steelblue")+
    theme_minimal() 
  

    # Add the download file details
  output$downloadData <- downloadHandler(
  filename = function() {
    paste("Put_records", ".csv", sep = "")
  },
  
  content = function(file) {
    write.csv(Herps_Put, file, row.names = FALSE)
  }
  )
  
  ##filteres
  # Reactive function based on input
  react_df <- eventReactive(input$Class_input, {
    
    return(Herps_Put %>% filter(class %in% input$Class_input))
    
  })
  
  output$displayer <- renderDataTable(react_df())
  
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste('GBIFdata-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      output_d <-react_df()
      write.csv(output_d, file, row.names=FALSE)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

