#' Create a spatial joint between a flowdata table and a map background
#' Performs an attribute spatial join between a flow data table and XY map background
#'
#' @param tab is the input flowdata table in the form of a list
#' @param fdc the map background file, ie. a shapefile
#' @param code is the IDs of the spatial units in the map background
#' @return Resulting jointure table between flowdata and map background
#' @export
#' @importFrom maptools readShapeSpatial
#' @importFrom rlang .data
#' @import sp
#' @examples
#' ## Example 1 : from a matrix
#' #library(cartograflow)
#' #tab<- read.csv2("mflowdataset.csv",header=TRUE,sep=";",stringsAsFactors=FALSE,
#' #encoding="UTF-8",dec=".",check.names=FALSE)
#' #tabflow<-flowstructmat(tab) #paste and transposes the columns ID to the rows ID
#' #tabflow<-flowtabmat(tabflow,matlist="L") #transform the flowdataset from
#' #matrix to list format
#' #tab<-flowjointure(tabflow,"file.shp","ID_map")
#' ## Example 2 : from a list
#' #library(cartograflow)
#' #tabflow<- read.csv2("flowdataset.csv",header=TRUE,sep=";",stringsAsFactors=FALSE,
#' #encoding="UTF-8",dec=".",check.names=FALSE)
#' #tab<-flowjointure(tabflow,"file.shp","ID_map")
#'

flowjointure<-function(tab,fdc,code){

  carte <- readOGR(fdc,verbose = FALSE)

  pt <- cbind(carte@data[, code], as.data.frame(coordinates(carte)))

  colnames(pt) <- c(code, "X", "Y")

  tab = data.frame(tab, pt[match(tab[, "i"], pt[, code]), 2:3])
  tab = data.frame(tab, pt[match(tab[, "j"], pt[, code]), 2:3])
  colnames(tab) <- c("i", "j", "ydata", "X1", "Y1", "X2", "Y2")
  tab.na<-tab %>%
          mutate_at(vars(.data$X1:.data$Y2), ~case_when(
          rowSums(is.na(tab)) > 0 ~ 0,
          TRUE ~ .))

return(tab.na)
}
