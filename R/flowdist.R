#' @title Builds a continuous distance matrices from a spatial features background
#' @description
#' From a layer of areal spatial features, compute and threshold a continuous distance matrix.
#' The result is either a matrice of distances between ODs, or a flow matrix based on the distance travelled between ODs ; both can be used for filtering flow before flow mapping (\link{flowmap})
#' @param tab the input flow dataset
#' @param dist.method distance calculation algorithm, default is euclidian calculation
#' @param result Choose Building a "flowdist" or a simple "dist" matrice. See Details 
#' @return (1) A flowdata set with continuous  distances calculations. See dist.method parameter
#' @return (2) A flowdata set with movement from euclidian distances calculations
#' @details
#' -- result = "dist" is the simple resulting distance matrice.\cr
#' -- result = "flowdist" is the resulting distance matrice with additional calculated parameters.\cr
#' -- It is also possible to filter flow by a level of distance travelled.
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom utils tail
#' @examples
#' library(cartograflow)
#' library(sf)
#' data(flowdata)
#' map <- st_read(system.file("shape/MGP_TER.shp", package = "cartograflow"))
#' tabflow <- flowjointure(
#'   geom = "area", bkg = map, DF.flow = flows, origin = "i",
#'   destination = "j", id = "EPT_NUM", x = "X", y = "Y"
#' )
#'
#' # Format long with only origin, destination and distance parameters:
#' tab.distance <- flowdist(tabflow, dist.method = "euclidian", result = "dist")
#' # Format long with with all parameters: coordinates, distance, mouvement
#' tab.distance <- flowdist(tabflow, dist.method = "euclidian", result = "flowdist")
#' @export

flowdist <- function(tab, dist.method, result) {
  if (dist.method == "euclidian") {
                tabflow <- tab %>%
                          mutate(distance = sqrt((.data$Xi - .data$Xj)^2 + (.data$Yi - .data$Yj)^2)) %>%
                          mutate(mouvement = .data$ydata * .data$distance)

                if (result == "flowdist") {
                            return(tabflow)
                }
                
                if (result == "dist") {
                             tab.reduction<-tabflow %>% select( .data$i,.data$j,.data$distance)
                             return(tab.reduction)
                }
  }

  if (dist.method == "manhattan") {
               tabflow <- tab %>%
                          mutate(distance = abs(.data$Xj- .data$Xi)+ abs(.data$Yj - .data$Yi)) %>%
                          mutate(mouvement = .data$ydata * .data$distance) 
              
                if (result == "flowdist") {
                            return(tabflow)
                }
               
               if (result == "dist") {
                          tab.reduction<-tabflow %>% select( .data$i,.data$j,.data$distance)          
                          return(tab.reduction)
               }
    
  }
}
