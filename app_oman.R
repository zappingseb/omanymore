# ideas:

# * get geos work to search for points in the map
# Try to get the routes between places by order in the table
# Make uploading a table possible
# Get rid of KML, instead manage the routes with the app itself

library(shiny)
library(leaflet)
library(RColorBrewer)
library(XML)
library(maptools)
library(rhandsontable)
library(stringr)
#library(rgdal); library(XML)
#
## SET WORKING DIRECTORY FIRST!!
#dir <- getwd()
#
#kmlfilelist <- list.files(dir, pattern =".kml$", full.names=TRUE, recursive=FALSE)
#
#ImportKml <- function (kmlfile) {
# doc0 <- xmlTreeParse(kmlfile, useInternal = TRUE)
# rootNode0 <- xmlRoot(doc0)
# rootName0 <- xmlName(rootNode0)
# element1Name0 <- names(rootNode0)
# 
# kmlNodeNames <- unname(names(rootNode0[1][[1]]))
# kmlFolderNodeNum <- which(kmlNodeNames == "Folder")
# kmlFolderNodeName <- xmlValue(rootNode0[[1]][[kmlFolderNodeNum]][[1]])
# 
# kmlIn <- readOGR(dsn=kmlfile, layer = kmlFolderNodeName)
#}
# ImportedKmls <- lapply(kmlfilelist, ImportKml)
#addTiles = function (map, urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
#   attribution = NULL, layerId = NULL, group = NULL, options = tileOptions())
#{
# options$attribution = attribution
# if (missing(urlTemplate) && is.null(options$attribution))
#   options$attribution = paste("© <a href='http://openstreetmap.org'>OpenStreetMap",
#       "contributors, <a href='http://creativecommons.org/licenses/by-sa/2.0/\'>CC-BY-SA")
# invokeMethod(map, getMapData(map), "addTiles", urlTemplate,
#     layerId, group, options)
#}
# quakes data
#lat   long depth mag stations
#1 -20.42 181.62   562 4.8       41
#2 -20.62 181.03   650 4.2       15
#3 -26.00 184.10    42 5.4       43
#4 -17.97 181.66   626 4.1       19
#5 -20.42 181.96   649 4.0       11
#6 -19.68 184.31   195 4.0       12


#source("test_googlesheets.R")
    
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "70%", height = "100%"),
    absolutePanel(top = 10, right = 10,
        htmlOutput("range"),        
        checkboxInput("legend", "Show legend", FALSE),
        #rHandsontableOutput("table"),
        verbatimTextOutput('selected')
    )
)

server <- function(input, output, session) {
  
  map_data_reactive <- 
        kml_to_coords("oman_route.kml",TRUE)[1][[1]]
map_data_reactive <- as.data.frame(apply(map_data_reactive,2,as.numeric))   
  view_data <- data.frame(list_of_sheets[["Oman_info"]])

  # Reactive expression for the data subsetted to what the user selected
  filteredData_by_table <- reactive({
        if(!is.null(input$table_select$select$r)){
          quakes <- view_data
          quakes[
              input$table_select$select$r,
              ]
        }else{
          NULL
        }
      })
  
  output$map <- renderLeaflet({
        #quakes <- map_data_reactive
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
#       leaflet(quakes) %>% addTiles() %>%
        #%>% setView("map",
              # lng = ~mean(long), lat=~mean(lat), zoom = 12)
#           fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
        leaflet(data = map_data_reactive, width=300) %>% addTiles() %>% setView(
              lng = mean(map_data_reactive$long,na.rm=T),
			  lat=  mean(map_data_reactive$lat,na.rm=T),
			  zoom = 17) %>%
			addPolylines(data = map_data_reactive, lng = ~long, lat = ~lat)
	
      })
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
#  observe({
#        pal <- colorpal()
#        
#        leafletProxy("map", data = filteredData()) %>%
#            clearShapes() %>%
#            addCircleMarkers(lat=~lat,lng=~long, radius = ~stations*5, color = "#777777",
#                fillColor =  ~pal(stations), fillOpacity = 0.9, popup = ~paste(stations)
#            )
#      })
#  
#  # Use a separate observer to recreate the legend as needed.
#  observe({
#        proxy <- leafletProxy("map", data = quakes_reactive())
#        
#        # Remove any existing legend, and only if the legend is
#        # enabled, create a new one.
#        proxy %>% clearControls()
#        if (input$legend) {
#          pal <- colorpal()
#          proxy %>% addLegend(position = "bottomright",
#              pal = pal, values = ~stations
#          )
#        }
#      })
 # output$table=NULL
#  renderRHandsontable(
#      rhandsontable(view_data,selectCallback = TRUE,readOnly = TRUE)
#  )
  output$selected=renderPrint({
        cat('Selected Row:',input$table_select$select$r)
        cat('\nSelected Column:',input$table_select$select$c)
        cat('\nSelected Cell Value:',
            input$table_select$data[[
                input$table_select$select$r]][[input$table_select$select$c]])
        cat('\nSelected Range: R',input$table_select$select$r,
            'C',input$table_select$select$c,':R',input$table_select$select$r2,
            'C',input$table_select$select$c2,sep="")
        cat('\nChanged Cell Row Column:',input$table$changes$changes[[1]][[1]],
            input$table$changes$changes[[1]][[2]])    
        cat('\nChanged Cell Old Value:',input$table$changes$changes[[1]][[3]])
        cat('\nChanged Cell New Value:',input$table$changes$changes[[1]][[4]])
      })
  
#  observe({
#        pal <- colorpal()   
#        if(!is.null(filteredData_by_table())){
#          
#          leafletProxy("map", data = filteredData_by_table()) %>%
#            clearShapes() %>%
#            removeMarkerFromCluster(layerId = "11_red",clusterId="1") %>%
#            addCircleMarkers(lat=~lat,lng=~long, radius = ~stations*20, color = "#777777",
#                fillColor = "red", layerId="11_red", clusterId="1",fillOpacity = 0.9, popup = ~paste(stations)
#            )
#        }
#      })
#
#  output$range <- renderUI({ isolate({
#              
#        sliderInput("range", "Magnitudes", 
#            min(quakes_reactive()$stations),
#            max(quakes_reactive()$stations),
#            value = range(quakes_reactive()$stations), step = 1
#        )})
#            })
  
}

shinyApp(ui, server)