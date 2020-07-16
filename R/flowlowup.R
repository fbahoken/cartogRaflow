#' @title Extracts the upper or lower sub-matrix
#' @description Extracts the upper or lower triangular part of a matrix
#' @param tab is the input flow dataset
#' @param format specify the flow dataset format, "M " for square matrix [n*n] or "L" for long [i,j,data]
#' @param x enter the Enter the triangular part to be extracted: "low", "up". See Details.
#' @details 
#' This function compute for all pairs or origin-destination places (i,j)
#' a lower "low" or upper "up" triangular sub-portion of the original matrix
#' - x = "up"  for the part above the main diagonal \cr
#' - x = "low" for the part below the main diagonal\cr
#' @import dplyr
#' @importFrom rlang .data
#' @examples
#' library(cartograflow)
#' data(flowdata)
#' 
#' Extract the upper part of the matrix : Long format
#' tab_up <- flowlowup(tab, format="L", lowup="up")
#' 
#' @export

flowlowup <- function(tab,origin=NULL,destination=NULL,fij=NULL,lowup, format, x) {
  
  
  if (format == "L") {
    
    tab_up<-flowtabmat(tab,matlist = "M")
    temp_up<-lower.tri(tab_up, diag = FALSE)
    
    tab_low<-flowtabmat(tab,matlist = "M")
    temp_low<-upper.tri(tab_low,diag=FALSE)
    
    nbi<-dim(tab_up)[1]
    nbj<-dim(tab_up)[2]
    
    for (i in 1:nbi){
      for (j in 1:nbj){
        if (temp_up[i,j] == TRUE){tab_up[i,j]<-0 }
        if (temp_low[i,j] == TRUE){tab_low[i,j]<-0 }
      }}
    
    tab_low<-flowtabmat(tab_low,matlist = "L")
    tab_up<-flowtabmat(tab_up,matlist = "L")
    
  }
  
  if (lowup == "low") {
       return(tab_low)
  }
    
  if
  (lowup == "up") {
    return(tab_up)
  }
  
  if (missing(lowup)) {
    return(tab_up)
  
  }
}




