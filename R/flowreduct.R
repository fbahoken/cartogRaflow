#' @title Flow matrix reduction according to another matrix
#' @description
#' Reduces a flow dataset according to an external matrix, eg. a matrix of travelled distance.\cr 
#' Builds geographical movements,  by weighting a flow dataset according to a distance criterion.
#' @param tab is the input flowdata set
#' @param tab.metric the distance dataset
#' @param metric select "continuous" or "ordinal" metric. See Details
#' @param d.criteria is for selecting "dmin" or "dmax" distance criteria for "continuous" metric. See Details.
#' @param d is the value of the selected "dmin" or "dmax". see Details
#' @return A flow dataset with distances computations and flow reduction
#' @details
#' The involved metric can be continous or not.\cr
#' 
#' (1) Metric is 'continous" for distance as euclidian, maximum, manhattan, etc.\cr
#' See \link{flowdist} \cr
#' - Metric is 'ordinal" for computing neighbourhood ordinal distance matrix.
#' -- Select ="dmin" for reducing flow dataset to flow values that are up or equal to the dmin distance parameter  (Fij>=dmin);\cr
#' -- select ="dmax" for reducing flow dataset to values that are less or equal to the dmax distance parameter(Fij=<dmin).\cr \cr
#'
#' - Metric is 'ordinal' for computing neighbourhood ordinal distance with k contiguity.\cr
#' See \link{flowcontig} for computing ordinal distance matrix \cr
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom utils tail
#' @examples
#' library(cartograflow)
#' library(sf)
#' library(dplyr)
#' data(flowdata)
#' map <- st_read(system.file("shape/MGP_TER.shp", package = "cartograflow"))
#'
#' tab <- flowjointure(
#'   geom = "area", bkg = map, DF.flow = flows, origin = "i", destination = "j",
#'   id = "EPT_NUM", x = "X", y = "Y"
#' )
#'
#' # Example for reducing a flow matrice with a distance matrice, in long format (i,j, distance)
#' ## 1/2: Computes the matrice distances
#' tab.distance <- flowdist(tab, dist.method = "euclidian", result = "dist")
#' tab.distance <- tab.distance %>% select(i, j, distance)
#' ## 2/2: Reduce the flow matrice
#' tab.flow <- flowreduct(flows, tab.distance,
#'   metric = "continous",
#'   d.criteria = "dmax", d = 8567
#' )
#' @export

flowreduct <- function(tab, tab.metric, metric, d.criteria, d) {
  if (ncol(tab) > 3 || ncol(tab.metric) > 3) {
    message("your dataset must be have three columns : origine, destination and flow or another data")
  }

  if (metric == "ordinal") {
                            tabflow <- merge(tab, tab.metric, by = c("i", "j"), all.X = TRUE, all.Y = TRUE)
                            colnames(tabflow) <- c("i", "j", "flow", "ordre.c")
                            return(tabflow)
  }

  if (metric == "continous") {
                              if (missing(d.criteria)) {
                                message("put the distance selection : dmax or dmin")
                              }
    
                              if (missing(d)) {
                                message("put the distance variable")
                              }
    
                              colnames(tab) <- c("i", "j", "ydata")
                              tabflow <- merge(tab, tab.metric, by = c("i", "j"), all.X = TRUE, all.Y = TRUE)
                          
                              if (d.criteria == "dmin") {
                                                        tabreduct <- tabflow %>%
                                                                     mutate(flowfilter = ifelse(.data$distance >= d, .data$ydata, 0)) %>%
                                                                     filter(.data$flowfilter != 0 , .data$distance !=0)
                                                        return(tabreduct)
                              }
                              
                              if (d.criteria == "dmax") {
                                                        tabreduct <- tabflow %>%
                                                                     mutate(flowfilter = ifelse(.data$distance < d, .data$ydata, 0)) %>%
                                                                     filter(.data$flowfilter != 0 , .data$distance !=0)
                                                        return(tabreduct)
                              }
  }
}
