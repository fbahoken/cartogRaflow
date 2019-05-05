#' @title Create a square matrice from geographical ID
#' @description Create a closed and square matrice from a list of geographic ID (CODE)
#' @param liste list or all the geographical ID as a single vector flow dataset
#' @param tab the non squared input flow dataset with three column : origin, destination, flow value
#' @param origin origin place ID
#' @param dest destination place ID
#' @param valflow flow value
#' @param empty.sq TRUE : to allows to have an matrice empty with only the ID of background map ; FALSE or missing
#' @param format the squared flow dataset output format : see details
#' @param diagonale see details
#' @details
#' - format is "M" for matrice format
#'
#' - format is "L" for long format
#'
#' - diagonal is "TRUE" to zero the main diagonal
#' @examples
#' library(cartograflow)
#' data(flowdata)
#' var1<-COD_GEO_EPT
#' var2<-MOBPRO_ETP
#'
#' #1/2 Compute an empty square matrice with ID code, and sets the value to zero
#' #Example for matrice format (same procedure for the long format)
#'
#' mat<-flowcarre(var1,var2,origin="i",dest="j",valflow="Fij",
#'                format="M",empty.sq=TRUE)
#'
#' #2/2 Fill in the matrice with external flow values
#' mat<-flowcarre(var1,var2,origin="i",dest="j",valflow="Fij",
#'                format="M",empty.sq=FALSE)
#'
#' #Square a matrice and zero the main diagonal
#' mat<-flowcarre(var1,var2,origin="i",dest="j",valflow="Fij",format="M",
#'                empty.sq=FALSE,diagonale = FALSE)
#' @export
#' @importFrom utils str

flowcarre<-function(liste, tab, origin, dest,valflow,empty.sq,format,diagonale){
    str(liste)
    nbi<-dim(liste)[1]
    matrice<-matrix(data = c(0),nrow = nbi,ncol = nbi,
        dimnames = list(c(as.matrix(liste)), c(as.matrix(liste))))
    class(matrice)

    if (empty.sq == TRUE){return(matrice)}
    if (missing(empty.sq) || empty.sq == FALSE){
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
