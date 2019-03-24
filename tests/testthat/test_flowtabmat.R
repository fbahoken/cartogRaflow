#' Transform a matrice format to a long format and vice versa
#'
#' @param tab dasaset matrice or list
#' @param matlist choise of matice or list
#' @return matrice or list
#' @export
#' @details
#' From list to matrice: matlist="M" ;
#' From matrice to list: matlist="L".
#' @examples
#' ## Example 1 : from list to matrix (n*m)
#' #library(cartograflow)
#' #tabFlow<-read.csv2("flowdataset.csv",header=TRUE,sep=";",stringsAsFactors=FALSE,
#' #encoding="UTF-8",dec=".",check.names=FALSE)
#' #matFlow<-flowtabmat(tabFlow,matlist="M")
#' ## Example 2 : from matrix to list [i,j,Fij]
#' #library(cartograflow)
#' #matFlow<-read.csv2("flowdataset.csv",header=TRUE,sep=";",stringsAsFactors=FALSE,
#' #encoding="UTF-8",dec=".",check.names=FALSE)
#' #tabFlow<-flowtabmat(matFlow,matlist="L")
#' @importFrom reshape2 melt
#' @importFrom reshape2 acast
#'

flowtabmat<-function(tab,matlist){
if (matlist == "L"){
  if(nrow(tab)!=ncol(tab)){
    message("your matrix is not usable, please use the function cartograflow::flowstructmat()")
    l.liste <- melt(tab)}
  if(nrow(tab) == ncol(tab)){l.liste <- melt(tab,na.rm=TRUE)}
  names(l.liste) = c("i", "j", "ydata")
  return(l.liste)
  }
if (matlist == "M"){
  m.mat<-acast(tab, i~j)
  for (i in 1:nrow(m.mat))
    for (j in 1:ncol(m.mat))
    {if (is.na.data.frame(m.mat[i,j])==TRUE) {m.mat[i,j]<-0}}
  if(nrow(m.mat)!=ncol(m.mat)){message("warning:your matrix is not square!")}
  if(nrow(m.mat)==ncol(m.mat)){message("great:your matrix is square!")}
  return(m.mat)}
}
