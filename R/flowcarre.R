#' Create a square matrice from IDs
#'
#' Create a closed square matrice from a list of geographic IDs (CODE)
#' @param liste list or all the spatial units IDs, on a single column
#' @param tab is the non squared input flowdata table with three column : origin, destination, flow)
#' @param origin identify the origin nodes column (which contains IDs)
#' @param dest identify the destination nodes column (which contains IDs)
#' @param valflow identify tne flow value column
#' @param format indicates the squared flow data output format : "M" or "L"for matrice (n*n) or "L" for list
#' @param diagonale tttt
#' @param empty.sq TRUE : to allows to have an matrice empty with only the ID of background map ; FALSE or missing
#' @details
#' The table listing all the spatial geometry IDs involved (origin and/or destination)
#' must have been prepared in advance.
#' For a matrice (n*n) output form: format ="M"
#' For a list output form: format ="L"
#' @examples
#' ## Example 1 : transform a non squared flow dataset to a closed and squared matrice
#' #library(cartograflow)
#' #code<-read.csv2("file1.csv",header=TRUE,sep=";",stringsAsFactors=FALSE,
#' #encoding="UTF-8",dec=".",check.names=FALSE)
#' #flow<-read.csv2("file2.csv",header=TRUE,sep=";",stringsAsFactors=FALSE,
#' #encoding="UTF-8",dec=".",check.names=FALSE)
#' #mat<-flowcarre(tab="file1",tab="file2",origin="COMMUNE_O",dest="COMMUNE_D",
#' #valflow="COUNT",format="L")
#' @export
#' @importFrom utils str

flowcarre<-function(liste, tab, origin, dest,valflow,format,diagonale,empty.sq){
    str(liste)
    nbi<-dim(liste)[1]
    matrice<-matrix(data = c(0),nrow = nbi,ncol = nbi,
        dimnames = list(c(as.matrix(liste)), c(as.matrix(liste))))
    class(matrice)
    if (empty.sq == TRUE){return(matrice)}
    if (empty.sq == FALSE | missing(empty.sq)){
      tabflow <- flowtabmat(matrice, matlist = "L")
      tabflow$ID_link <- paste(tabflow$i, tabflow$j, sep = "_")
      tab$ID_link <- paste(tab[, origin], tab[, dest], sep = "_")
      tabflow <- data.frame(tabflow, tab[match(tabflow[, "ID_link"], tab[, "ID_link"]), 2:3])
      tabmap <- data.frame(tabflow$i, tabflow$j, tabflow[, valflow], stringsAsFactors = FALSE)
      names(tabmap) = c("i", "j", "ydata")
      result <- replace(tabmap, is.na(tabmap), 0)

      if (format == "L"){return(result)}

      if (format == "M"){
        matflow <- flowtabmat(result, matlist = "M")
        if (missing(diagonale)){
          message("warning : the variable diagonale is missing so the diagonal of your matrix will be not empty!")
          return(matflow)}
        if (diagonale == TRUE){return(matflow)}
        else if (diagonale == FALSE){
          diag(matflow) <- 0
          return(matflow)}
      }
    }
  }
