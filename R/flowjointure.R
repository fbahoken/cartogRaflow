#' @title Create a spatial join with flow
#' @description Create an attribute spatial join between a flow dataset table and a map background
#' @param tab the input flow dataset table in long format
#' @param fdc the map background file, ie. a shapefile
#' @param code the ID of the spatial units in the map background
#' @return Resulting jointure table between flow dataset and map background
#' @export
#' @importFrom maptools readShapeSpatial
#' @importFrom rlang .data
#' @import sp
#' @examples
#' library(cartograflow)
#' data(flowdata)
#' bkg<- system.file("shape/MGP_TER.shp", package="cartograflow",
#'                    lib.loc = NULL, mustWork = TRUE)
#' tabflow<-flowjointure(flows,bkg,"EPT_NUM")
#'@export


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
