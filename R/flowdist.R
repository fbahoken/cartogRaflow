#' @title Compute continous distance matrix from geographical background
#' @description
#' From a geographical background computes (and threshold) a distance matrix.
#' @param tab the input flow dataset.
#' @param dist.method euclidian calculation
#' @param result take the vallue "flowdist" or "dist" allows to parameter the resulting distance dataset (flows filtered by a distance criterion or not)
#' @return (1) A flowdata set with continuous euclidian distances calculations, see dist.method parameter
#' @return (2) A flowdata set with movement from euclidian distances calculations.
#' @return (3) A flowmap filtered by a global distance criterion.
#' @details
#' -- result = "dist" is the resulting tab of the distance\cr
#' -- result = "flowdist" with all the calculated parameters
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom utils tail
#' @examples
#' library(cartograflow)
#' data(flowdata)
#' bkg<- system.file("shape/MGP_TER.shp", package="cartograflow",
#'                   lib.loc = NULL, mustWork = TRUE)
#' tab<-flowjointure(flows,bkg,"EPT_NUM")
#' #Format long with only origin, destination and distance parameters:
#' tab.distance<-flowdist(tab, dist.method = "euclidian",result = "dist")
#' #Format long with with all parameters: coordinates, distance, mouvement
#' tab.distance<-flowdist(tab, dist.method = "euclidian",result = "flowdist")
#' @export

flowdist<-function(tab,dist.method,result){

if (dist.method == "euclidian"){
       tabflow<-tab %>%
           mutate(distance=sqrt((.data$X1-.data$X2)^2+(.data$Y1-.data$Y2)^2)) %>%
           mutate(mouvement =.data$ydata*.data$distance)

       if (result == "flowdist"){
             return(tabflow)}
       if (result == "dist"){
          tab.reduction<-select(tabflow,-.data$ydata,-.data$X1,-.data$Y1,-.data$X2,-.data$Y2,-.data$mouvement)
          return(tab.reduction)
       }
  }

if (dist.method == "xxx"){}

}




