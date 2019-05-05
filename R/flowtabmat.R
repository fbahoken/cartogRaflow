#' @title Changing the format of a flow dataset
#' @description Transform a flow dataset from long to matrice format, and vice versa
#' @param tab flow dasaset, in matrice or long format
#' @param matlist choose matrice or long as the result format. See details.
#' @return matrice or long
#' @export
#' @details
#' -- From long to matrice format : matlist="M";\cr
#' -- From matrice to long format : matlist="L".
#' @examples
#' library(cartograflow)
#' data(flowdata)
#' #1: From long to matrix format (n*m)
#' matFlow<-flowtabmat(MOBPRO_ETP,matlist="M")
#' #2: From matrix to long format [i,j,Fij]
#' listflow<-flowtabmat(matFlow,matlist="L")
#' @importFrom reshape2 melt
#' @importFrom reshape2 acast

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
