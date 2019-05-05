#' @title Structuring a matrice
#' @description Fixes an ID shift in the flow  matrice (to bo use with \link{flowjointure} if necessary and \link{flowtabmat})
#' @param z The flow dataset is in the matrice format where the first column is filled with the ID
#' @return A flowdataset with an usable format
#' @export
#' @examples
#' library(cartograflow)
#' data(flowdata)
#'
#' dim(mat_ex) # dimension fo the original matrice
#' ### 10 11  # first colum is fill with the ID
#'
#' tab<-flowstructmat(mat_ex)
#' dim(tab)
#' ## 10 10 # dimension fo the resulting matrice

flowstructmat<-function(z){
  m <- as.matrix(z[,-1])
  row.names(m) <- z[,1]
  class(m)
  resul<-m
}




