#' @title Flow matrice reduction according to another matrice
#' @description
#' Reduces a flow dataset according to an external matrice (eg. distance travelled)
#' Computes geographical movements (by weighting a flow dataset according to a distance criterion)
#' @param tab is the input flowdata set.
#' @param tab.metric is the table of distance (continuous dataset) or contiguity (ordinal dataset)
#' @param metric  See Details.
#' @param select is the continuous distance thresholding parameter. See Details.
#' @param d distance thresholds criterion.
#' @return A flow dataset with continuous euclidian distances calculations
#' @details
#' -- Metric is 'continous" for distance as euclidian, maximum, manhattan, etc.\cr
#' See \link{flowdist} for computing neighbourhood ordinal distance matrix.
#'
#' select ="dmin" for reducing flow dataset to values that are up or equal to the dmin distance parameter ;\cr
#' select ="dmax" for reducing flow dataset to values that are less or equal to the dmax distance.
#'
#' -- Metric is 'ordinal' for neighbourhood ordinal distance so-called k contiguity.\cr
#' See \link{flowcontig} for computing continuous distance matrix
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom utils tail
#' @examples
#' library(cartograflow)
#' data(flowdata)
#' bkg<- system.file("shape/MGP_TER.shp", package="cartograflow",
#'                   lib.loc = NULL, mustWork = TRUE)
#' tab<-flowjointure(MOBPRO_ETP,bkg,"EPT_NUM")
#'
#' #Example for reducing a flow matrice with a distance matrice, in long format (i,j, distance)
#' ##1/2: Computes the matrice distances
#' tab.distance<-flowdist(tab, dist.method = "euclidian",result = "dist")
#' ##2/2/: Reduce the flow matrice
#' tab.flow<-flowreduct(tab,tab.distance, metric = "continous",
#'                      select = "dmax", #maximum distance travelled criterion
#'                      d = 8567) #maximum distance value
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




