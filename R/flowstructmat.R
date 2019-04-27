#' @title structure matrice
#' @description Fixes an ID shift in the flow dataset matrice (to bo use with \link{flowjointure} and \link{flowtabmat})
#' @param z is the flowdataset in the matrix format where the first column is filled with the IDs
#' @return A flowdataset with an usable format
#' @export
#' @examples
#' \dontrun{
#' preparing a matrice
#' library(cartograflow)
#' tab<- read.csv2("flowdataset.csv",header=TRUE,sep=";",stringsAsFactors=FALSE,
#' encoding="UTF-8",dec=".",check.names=FALSE)
#' #Transforms the first colunm (which contain the IDs code of the background map)
#' #into matrix ID colunm.
#' tabflow<-flowstructmat(tab)
#' ##transform the flowdataset from matrix to list format
#' tabflow<-flowtabmat(tabflow,matlist="L")
#'}

flowstructmat<-function(z){
  m <- as.matrix(z[,-1])
  row.names(m) <- z[,1]
  class(m)
  resul<-m
}
