# ideas:

# * get geos work to search for points in the map
# Try to get the routes between places by order in the table
# Make uploading a table possible
# Get rid of KML, instead manage the routes with the app itself
library(here)
library(R6)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(XML)
library(maptools)
library(rhandsontable)
library(stringr)
library(shinyjs)

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
#   options$attribution = paste("? <a href='http://openstreetmap.org'>OpenStreetMap",
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


source("test_googlesheets.R")
source("kml_handling.R")
source("switchButton.R")

ui <- bootstrapPage(
  useShinyjs(),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$head(includeCSS(here("www/css/button.css"))),
  fluidPage(
    fluidRow(
      column(12, div(style = "width: 100%; max-height: 300px;position:fixed;top:0px;z-index:8",
                     leafletOutput("leaflet_map",width="100%",height="100%")),style="z-index:2"),
                  
      column(12, div(style="width:100%;margin-left:10px;margin-right:10px;margin-top:10px;z-index:2",
                     uiOutput("table_container")),style="z-index:1")
      
    )
  ),
  absolutePanel(top = 0, id="menu",right = 0,width="200px",fixed=T,style="z-index:10;background-color:white;display:none;padding:20px;padding-top:70px",
                fluidPage(
                  fluidRow(
                    column(12,
                           switchButton(inputId = "map_switch",
                                        label = "Show on Map?",
                                        value = FALSE, col = "GB", type = "TF")),
                    column(12,
                           switchButton(inputId = "route_switch",
                                        label = "Show whole Route?",
                                        value = TRUE, col = "GB", type = "TF")),
                    column(12,div(
                      tags$label("Reset to the whole Trip?"),
                      tags$br(),
                      actionButton("map_reset","Reset")
                      ))
                  )
                  
                )
                # ,verbatimTextOutput('selected')
                
                
  ),
  absolutePanel(
    top = 0,
    right = 0,
    fixed=T,
    style="z-index:10;margin:20px;",
    actionButton("menu_in_out","Menu"))
)

server <- function(input, output, session) {
  
  kml_file <- KMLDocument$new(filename=here("/www/kmls/oman_route.kml"),ignore_altitude=TRUE)
  
  map_data_reactive <- kml_file$xml_data[1][[1]]
  
  map_data_reactive <- as.data.frame(apply(map_data_reactive,2,as.numeric))   
  
 # sheets <- GoogleTable$new("https://docs.google.com/spreadsheets/d/16O1MSjRnD5epMqHIpKFQvp2o7jsrH1kYfBo3a2hj6T8/edit?usp=sharing")
  
  view_data <- read.csv(here("www","oman_info.csv"),header=T,stringsAsFactors = F,na.strings = "")
               #         data.frame(sheets$list_of_sheets[["Oman_info"]])
  
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
  
  filteredData_by_table_upper_lower <- reactive({
    if(!is.null(input$table_select$select$r)){
      quakes <- view_data
      
      rows_with_info <- which(!is.na(quakes$GPS.lat))
      
      first_row_above <- max(rows_with_info[which(rows_with_info < input$table_select$select$r)],na.rm=T)
                             
      first_row_behind <- min(rows_with_info[which(rows_with_info > input$table_select$select$r)],na.rm=T)
      
      if(first_row_above==-Inf || first_row_behind==-Inf || first_row_behind==Inf || first_row_above==Inf){
        NULL
      }else{
        
        quakes[c(first_row_above,first_row_behind),]
      }
    }else{
      NULL
    }
    
    
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
  output$table= 
    
    renderRHandsontable(
    rhandsontable(data=view_data,
                  selectCallback = TRUE,
                  readOnly = TRUE,
                  width="100%",
                  height="100%")%>%hot_table(stretchH = "all",zindex="1")%>%
      hot_cols(colWidths = rep(70,ncol(view_data)))
  )
  # output$selected=renderPrint({
  #   cat('Selected Row:',input$table_select$select$r)
  #   cat('\nSelected Column:',input$table_select$select$c)
  #   cat('\nSelected Cell Value:',
  #       input$table_select$data[[
  #         input$table_select$select$r]][[input$table_select$select$c]])
  #   cat('\nSelected Range: R',input$table_select$select$r,
  #       'C',input$table_select$select$c,':R',input$table_select$select$r2,
  #       'C',input$table_select$select$c2,sep="")
  #   cat('\nChanged Cell Row Column:',input$table$changes$changes[[1]][[1]],
  #       input$table$changes$changes[[1]][[2]])    
  #   cat('\nChanged Cell Old Value:',input$table$changes$changes[[1]][[3]])
  #   cat('\nChanged Cell New Value:',input$table$changes$changes[[1]][[4]])
  # })
  
  output$table_container <- renderUI({
    div(rHandsontableOutput("table"))
  })
  
  output$leaflet_map <- renderLeaflet({
    leaflet(data = map_data_reactive,height="300px") %>% addTiles() %>% 
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      addPolylines(data = map_data_reactive, lng = ~long, lat = ~lat,layerId="overall")
  }) 
    
  
  # Map Switch Observer
  observe({
    if(input$map_switch){
      js_code <-"$( '#leaflet_map' ).css('height','300px');"
      
      shinyjs::runjs(js_code)
      shinyjs::runjs("$('#table_container').css('margin-top','300px');")
      
      
      
      if(input$route_switch){
        if(!is.null(input$table_select$select$r)){
          leafletProxy("leaflet_map", data = map_data_reactive) %>%
            addPolylines(data = map_data_reactive, lng = ~long, lat = ~lat,layerId="overall")
        
        }else{
        output$leaflet_map <-
          renderLeaflet({
            leaflet(data = map_data_reactive,height="300px") %>% addTiles() %>%
              #fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
              addPolylines(data = map_data_reactive, lng = ~long, lat = ~lat,layerId="overall")
          })
        }
      }else{
          leafletProxy("leaflet_map", data = map_data_reactive) %>%
            removeShape("overall")
          # output$leaflet_map <- 
          #   renderLeaflet({
          #     leaflet(data = map_data_reactive,height="300px") %>% addTiles() %>% 
          #       fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
          #   })
      }
      
    }else{
      output$leaflet_map <- renderLeaflet({
        NULL
      }) 
      shinyjs::runjs("$('#table_container').css('margin-top','0px');")
    }
  })
  
  # Table click observer
  observeEvent({input$table_select$select$r},{
    # Get a selected row
    if(!is.null(filteredData_by_table())){
      
      # If the row contains Geo Data highlight it on the map
      if(!is.na(filteredData_by_table()$GPS.lat)){
        print(filteredData_by_table()$GPS.lat)
        leafletProxy("leaflet_map", data = filteredData_by_table()) %>%
          removeMarkerFromCluster(layerId = "11_red",clusterId="1") %>%
          addMarkers(lat=~as.numeric(GPS.lat),lng=~as.numeric(GPS.long),layerId="11_red", clusterId="1")
        #, radius = 50, color = "#777777",
        #                   fillColor = "red",fillOpacity = 0.9, popup = "test"
        #  )
      }else{
        
        if(!is.na(filteredData_by_table()$Wohin)){
          
          geo_data <- data.frame(lon=NA,lat=NA)#ggmap::geocode(filteredData_by_table()$Wohin)
          
          if(!is.na(geo_data$lon)){
            leafletProxy("leaflet_map", data = geo_data) %>%
              removeMarkerFromCluster(layerId = "11_red",clusterId="1") %>%
              addCircleMarkers(lat=~as.numeric(lat),lng=~as.numeric(lon), radius = 50, color = "#777777",
                               fillColor = "red", layerId="11_red", clusterId="1",fillOpacity = 0.9, popup = "test"
              )
          }else{
            
            showNotification("Finding Route between points...",duration=5,type="error",id="waiter")
            
            # Get the point on the route before and after the selected row
            routing_points_table <- filteredData_by_table_upper_lower()

            # Derive the route between the point before and after the selected row in the table
            if(!is.null(routing_points_table)){
              
              routing_table <- kml_file$get_route_between_two_points(kml_file,
                                                                     as.numeric(routing_points_table$GPS.long[1]),
                                                                     as.numeric(routing_points_table$GPS.lat[1]),
                                                                     as.numeric(routing_points_table$GPS.long[2]),
                                                                     as.numeric(routing_points_table$GPS.lat[2]))
              
              routing_table <- as.data.frame(apply(routing_table[[1]],2,as.numeric)) 
              names(routing_table)<-c("long","lat")
              
              # Update the lealet map to the route found and
              # Display just the route
              if(input$route_switch){
                output$leaflet_map <- renderLeaflet({
                  leaflet(data = routing_table,height="300px") %>% addTiles() %>% 
                    fitBounds(~min(long)-0.001, ~min(lat)-0.001, ~max(long)+0.001, ~max(lat)+0.001) %>%
                    clearShapes() %>%
                    addPolylines(data = routing_table, lng = ~long, lat = ~lat,label="high_route",color="#F00",layerId="route") %>%
                    addPolylines(data = map_data_reactive, lng = ~long, lat = ~lat,layerId="overall")
                }) 
              }else{
                output$leaflet_map <- renderLeaflet({
                  leaflet(data = routing_table,height="300px") %>% addTiles() %>% 
                    fitBounds(~min(long)-0.001, ~min(lat)-0.001, ~max(long)+0.001, ~max(lat)+0.001) %>%
                    clearShapes() %>%
                    removeShape("overall") %>%
                    addPolylines(data = routing_table, lng = ~long, lat = ~lat,label="high_route",color="#F00",layerId="route")
                })
              }
              
              removeNotification(id="waiter")
            }else{
              showNotification("No route could be found",duration=5,type="error")
            }
            
           
            
            
            
          }#if geodata is na
        }# if Wohin is na
      }# if GPS.lat is na
    }# if is NULL Table
    
    
})
  observeEvent({input$map_reset},{
    if(input$map_switch){
      leafletProxy("leaflet_map", data = map_data_reactive) %>%
        fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    }
  })
  observeEvent({
    input$menu_in_out
  },{
    shinyjs::runjs('
 $("#menu").toggle();
      '
    )
  })
  #
  
}
shinyApp(ui,server)
