

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


ui <- fluidPage(
  titlePanel("Putumayo Watersehd GBIF Data"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("Herps_output"),
      br(),
      downloadButton("download_filtered", "csv Download Filtered Data"),   
      downloadButton("downloadData", "csv Download All Data"),
      br(),
      br()

    ),
    mainPanel(width = 8, # maximum of 12
              tabsetPanel(type = "tabs",
                          tabPanel("Map", leafletOutput("mymap")),
                          tabPanel("Summary", plotOutput("summary")),
                          tabPanel("Metadata", verbatimTextOutput("text"))
                          
              )
    )
  )
)


# server controls

server <- function(input, output) {
  output$Herps_output <- renderUI({
    selectInput(inputId = "Class_input", "Group", sort(unique(Herps_Put$group)), 
                selected = "Amphibians")
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
    set <- Herps_Put_map %>% filter(Herps_Put_map$group %in% input$Class_input)
    
    leafletProxy("mymap") %>% clearMarkers() %>% addCircleMarkers(lng = set$longitude, 
                                                                   lat = set$latitude, radius=1, weight=3,
                                                                   opacity = 0.5, fill = TRUE, fillOpacity = 0.2)
  })
  
  
  output$summary <- renderPlot({
    ggplot(Herps_Put, aes(x=factor(group)))+
     geom_bar(stat="count", fill="blue1")+
      geom_text(stat='count', aes(label=..count..), vjust=-0.2, size=6)+
      theme_minimal()+
      labs(x="Group", y="Records")+
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
  

    # Add the download file details
  output$downloadData <- downloadHandler(
  filename = function() {
    paste("Putumayo_all", ".csv", sep = "")
  },
  
  content = function(file) {
    write.csv(Herps_Put, file, row.names = FALSE)
  }
  )
  
  ##filteres
  # Reactive function based on input
  react_df <- eventReactive(input$Class_input, {
    
    return(Herps_Put %>% filter(group %in% input$Class_input))
    
  })
  
  output$displayer <- renderDataTable(react_df())
  
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste('Putumayo_filtered', '.csv', sep='')
    },
    content = function(file) {
      output_d <-react_df()
      write.csv(output_d, file, row.names=FALSE)
    }
  )
  
  output$text <- renderText({paste("Data filtered from the following: 
                                    Amphibians:https://doi.org/10.15468/dl.jzabhy
                                    Reptiles: https://doi.org/10.15468/dl.je6utk
                                    Fishes: https://doi.org/10.15468/dl.fu5bj3
                                    Birds: https://doi.org/10.15468/dl.a3pv39 
                                    Mammals: https://doi.org/10.15468/dl.abn8s4")})

  
}

# Run the application 
shinyApp(ui = ui, server = server)

