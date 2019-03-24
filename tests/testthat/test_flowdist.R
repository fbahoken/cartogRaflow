#' Compute the distance
#'
#' Calculates and threshold a distance matrix in order to filter a flowdata set before flowmapping. Compute also geographic movement.
#' @param tab is the input flowdata set.
#' @param dist.method euclidian calculation
#' @param result take the vallue "flowdist" or "dist"allows to parameter the resulting distance dataset (flows filtered by a distance criterion or not)
#' @return (1) A flowdata set with continuous euclidian distances calculations, see dist parameter
#' @return (2) A flowdata set with movement from euclidian distances calculations.
#' @return (3) A flow map filtered by a global distance criterion.
#' @details
#' This function allows to create a continous distance (see \link{flowcontig} for ordinal distance).
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom utils tail
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




