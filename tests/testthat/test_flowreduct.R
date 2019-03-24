#' Flow matrice reduction according to another matrice
#'
#' Reduces a flow dataset according to an external matrice (eg. distance travelled)
#' Calculates geographical movements - weights a flow dataset according to a distance criterion
#' @param tab is the input flowdata set.
#' @param tab.metric is the table of distance (continuous dataset) or contiguity (ordinal dataset)
#' @param metric  See Details.
#' @param select is the continuous distance thresholding parameter. See Details.
#' @param d is the map background IDs.
#' @return A flow dataset with continuous euclidian distances calculations
#' @details
#' This function allows to reduce a flow dataset from a matrice distance.
#' - Metric is 'continous" for distance as euclidian, maximum, manhattan, etc.
#' See \link{flowdist} for computing neighbourhood ordinal distance matrix.
#'
#' select ="dmin" is for reducing flow dataset to values that are up or equal to the dmin distance parameter.
#' select ="dmax" is for reducing flow dataset to values that are less or equal to the dmax distance.
#'
#' - Metric is 'ordinal' for neighbourhood ordinal distance so-called k contiguity.
#' See \link{flowcontig} for computing continuous distance matrix
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom utils tail
#' @export

flowreduct<-function(tab,tab.metric,metric,select,d){
  if(ncol(tab)>3 || ncol(tab.metric)>3){message("your dataset must be have three columns : origine, destination and flow or another data")}
  if(metric == "ordinal"){
  tabflow<-merge(tab,tab.metric,by=c("i","j"),all.X=TRUE,all.Y=TRUE)
  colnames(tabflow)=c("i","j","flux","ordre.c")
  return(tabflow)
  }

  if(metric =="continous"){
      if (missing(select)){message("put the distance selection : dmax or dmin")}
      if (missing(d)){message("put the distance variable")}
      tabflow<-merge(tab,tab.metric,by=c("i","j"),all.X=TRUE,all.Y=TRUE)

      if(select =="dmin"){
               tabreduct<-tabflow %>%
               mutate(flowfilter = ifelse(.data$distance>=d,.data$ydata,0)) %>%
               filter(.data$distance !=0)
              return(tabreduct)}
      if(select == "dmax"){
              tabreduct<-tabflow %>%
              mutate(flowfilter = ifelse(.data$distance<d,.data$ydata,0)) %>%
              filter(.data$distance !=0)
              return(tabreduct)}
  }
}




