
require(XML)

#' KML Document
#'
#' An R6 class handling all properties of an kml file
#' @section Usage:
#' \preformatted{kml_file <- KMLDocument$new(filename = "test.kml",ignore_altitude=TRUE)}
#' 
#' this calls \code{\link{kml_to_koords}} with the specific parameters and generates the
#' \preformatted{kml_file$xml_data} list
#' 
#' @section Arguments:
#' \describe{
#'   \item{filename}{A \code{character} describing where the kml file is located.}
#'    \item{ignore_altitude}{A \code{logical} describing wether the kml altitudes shall be read}
#'    }
#' @section Methods:
#' \describe{
#'   \item{kml_to_coords} \code{\link{kml_to_koords}}
#' }
#' @importFrom R6 R6Class
#' @export
KMLDocument <- R6Class("KMLDocument",
                       public = list(
                         filename = NULL,
                         xml_data = NULL,
                         ignore_altitude = FALSE,
                         initialize = function(filename = NA, ignore_altitude=FALSE) {
                           self$filename <- filename
                           self$ignore_altitude <- ignore_altitude
                           kml_to_coords(self)
                         },
                         #-----------------------------------------------------------------------------------
                         # Function to get the closest point to all routes inside the 
                         # kml file.
                         # By adding a point as input this calculates the closest route point
                         # and returns a table with the closest route points for all routes
                         # inside this kml file
                         #-----------------------------------------------------------------------------------
                          get_closest_route_point = function(self,long,lat,alt=0){
                            
                            if(self$ignore_altitude){
                              min_points <- data.frame(long=c(),lat=c())
                            }else{
                              min_points <- data.frame(long=c(),lat=c(),alt=c())
                            }
                            
                            min_indexes <- self$get_closest_route_point_index(self,long,lat,alt)
                            for(i in 1:length(self$xml_data)){
                              if(is.data.frame(self$xml_data[i][[1]])){
                                
                                  min_points <- rbind(min_points,self$xml_data[i][[1]][min_indexes,])
                              }
                            }
                            return(min_points)
                          },
                         get_closest_route_point_index=function(self,long,lat,alt=0){
                           if(self$ignore_altitude){
                             vector_to_compare <- c(long,lat)
                           }else{
                             vector_to_compare <- c(long,lat,alt)
                             
                           }
                           min_points <- c()
                           for(kml_element in self$xml_data){
                             if(is.data.frame(kml_element)){
                               kml_element <- apply(kml_element,2,as.numeric)
                               distances <- sqrt(colSums((t(kml_element) - vector_to_compare)**2))
                               #   apply(kml_element,1,function(row_to_take){
                               #   dist(rbind(as.numeric(row_to_take),vector_to_compare))
                               # })
                             }
                             min_points <- c(min_points, which.min(distances))
                           }
                           return(min_points)
                         },
                       get_route_between_two_points = function(self, long_1, lat_1, long_2, lat_2){
                         
                         # Get the nearest point of route long1,lat1
                         point1 <- self$get_closest_route_point_index(self,long_1,lat_1)
                         
                         # Get the nearest point index of long2, lat2
                         point2 <- self$get_closest_route_point_index(self,long_2,lat_2)
                         
                         routes <- list()
                         
                         for(i in 1:length(self$xml_data)){
                           if(is.data.frame(self$xml_data[i][[1]])){
                             # Append the route elements between the two points to the list of routes
                             routes <- append(routes,
                                              list(as.data.frame(
                                                self$xml_data[i][[1]][
                                                  point1[i]:point2[i],])))
                           }
                         }
                         return(routes)
                       })
                         
)


#' Function to read in kml file
#' @param self \code{\link{KMLDocument}}
#' @author Sebastian WOlf sebastian@@mail-wolf.de
#' @export
kml_to_coords <- function(self) {
  
  ignoreAltitude <- self$ignore_altitude
  
  data <- xmlRoot(xmlTreeParse(file=self$filename))
  
  xml_data <- lapply(xmlChildren(data[["Document"]]),
                     function(x){
                       if(x$name=="Placemark"){
                         if("LineString" %in% names(xmlChildren(x))){
                           
                           
                           data_table <- as.data.frame(
                             stringr::str_split_fixed(
                               
                               as.data.frame(strsplit(
                                 xmlValue(xmlChildren(x)$LineString[["coordinates"]]),
                                 " "
                               ),stringsAsFactors=F)[,1], ",", 3),stringsAsFactors=F)
                           
                           colnames(data_table) <- c("long","lat","alt")
                           if(ignoreAltitude){
                             data_table[,1:2]
                           }else{
                             data_table
                           }		
                           
                         }else if("Point" %in% names(xmlChildren(x))){
                           
                           data <- strsplit(
                             xmlValue(xmlChildren(x)$Point[["coordinates"]]),
                             ",")
                           
                           if("name" %in% names(xmlChildren(x))){
                             if(ignoreAltitude){
                               select <- 1:2
                             }else{
                               select <- 1:3
                             }
                             values <- list(
                               "name"=as.numeric(data[[1]][select]))
                             
                             names(values) = xmlValue(xmlChildren(x)$name)
                             
                             values
                           }else{
                             if(ignoreAltitude){
                               list(as.numeric(data[[1]][1:2]))
                             }else{
                               list(as.numeric(data[[1]]))
                             }
                             
                           }
                           
                         }
                       }
                     })
  self$xml_data <- xml_data
  invisible(self)
}

