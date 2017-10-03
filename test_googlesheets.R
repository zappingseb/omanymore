library(googlesheets)
gs_gap()

gap <- gs_title("Reise1")

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

require(XML)

kml_to_coords <- function(kml_file,ignoreAltitude = FALSE) {
	
	data <- xmlRoot(xmlTreeParse(file=kml_file))

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
		return(xml_data)
}
xml_data

