library(googlesheets)

#"https://docs.google.com/spreadsheets/d/16O1MSjRnD5epMqHIpKFQvp2o7jsrH1kYfBo3a2hj6T8/edit?usp=sharing"

#' Google Table
#'
#' An R6 class handling all properties of an kml file
#' @section Usage:
#' \preformatted{sheets <- GoogleTable$new(
#' url = "https://docs.google.com/spreadsheets/d/16O1MSjRnD5epMqHIpKFQvp2o7jsrH1kYfBo3a2hj6T8/edit?usp=sharing")}
#' 
#' this calls \code{\link[omanymore]{get_sheets}}with this url to return
#' \preformatted{sheets$list_of_sheets} list
#' 
#' @section Arguments:
#' \describe{
#'   \item{url}{A \code{character} URL of a publically available google sheet}
#'    }
#' @section Methods:
#' \describe{
#'   \item{get_sheets} \code{\link{get_sheets}}
#' }
#' @export
GoogleTable <- R6Class("GoogleTable",
                       public = list(
                         url = NULL,
                         list_of_sheets = NULL,
                         initialize = function(url = NA) {
                           self$url <- url
                           get_sheets(self)
                         })
)

# Function to derive go
get_sheets <- function(self){
  
  gs_ls()
  # 
  gap <- gs_url(self$url)
  
  africa <- as.data.frame(gs_read(gap),stringsAsFactors=F)
  
  list_of_sheets <- list()
  for(i in 4:nrow(africa)){
    
    # Get the sheet name from very first sheet
    tag <- africa[i,1]
    
    if(grepl("info",tag)){
      
      # Get the travel info into a table
      list_of_sheets[[tag]] <- gs_read(gap,ws=i+1,col_names=T)
      
    }else{
      
      # Add the sheet as reading in as.character
      list_of_sheets[[tag]] <- as.character(gs_read(gap,ws=i+1,col_names=F))
    }
    
    # If the table shows an xml file write it to disk
    if(any(grepl("?xml",list_of_sheets[[tag]]))){
      write.table(gs_read(gap,ws=i+1,col_names=F,na=""),
                  file = paste0(tag,".kml"), sep = '\t', row.names = FALSE,na="",col.names=F)
      
      list_of_sheets[[tag]] <- paste0(tag,".kml")
    }
  }
  # Append the list_of_sheets to the object
  self$list_of_sheets <- list_of_sheets
  
  invisible(self)
}

