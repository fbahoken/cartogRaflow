#' @title Builds a square matrice from geographical nodes
#' @description Builds a square and closed matrice from a dataframe of spatial nodes
#' @param liste list of all the spatial codes as a single dataframe
#' @param tab the non squared input flow dataset with three column : origin, destination, flow value
#' @param origin node / place of origin of the flow
#' @param dest node / place of destination of the flow
#' @param valflow is the flow value between origin and destination places
#' @param empty.sq Builds an empty matrix or not. See Details.
#' @param format is the desired squared flow dataset output format. See Details.
#' @param diagonale to zero or not the main diagonal. See Details.
#' @details
#' - empty.sq is "TRUE" builds an empty matrix ; else is "FALSE" or missing\cr \cr
#' - format is "M" for matrice format\cr
#' - format is "L" for long format, as three column dataframe\cr\cr
#' - diagonal is "TRUE" to zero the main diagonal
#' @examples
#' library(cartograflow)
#' data(flowdata)
#' var1 <- geoid
#' var2 <- flows
#'
#' # 1/2 Compute an empty square matrice with ID code, and sets the value to zero
#' # Example for matrice format (same procedure for the long format)
#'
#' mat <- flowcarre(var1, var2,
#'   origin = "i", dest = "j", valflow = "Fij",
#'   format = "M", empty.sq = TRUE
#' )
#'
#' # 2/2 Fill in the matrice with external flow values
#' mat <- flowcarre(var1, var2,
#'   origin = "i", dest = "j", valflow = "Fij",
#'   format = "M", empty.sq = FALSE
#' )
#'
#' # Square a matrice and zero the main diagonal
#' mat <- flowcarre(var1, var2,
#'   origin = "i", dest = "j", valflow = "Fij", format = "M",
#'   empty.sq = FALSE, diagonale = FALSE
#' )
#' @export
#' @importFrom utils str

flowcarre <- function(liste, tab, origin, dest, valflow, empty.sq, format, diagonale) {
  str(liste)
  nbi <- dim(liste)[1]
  matrice <- matrix(
    data = c(0), nrow = nbi, ncol = nbi,
    dimnames = list(c(as.matrix(liste)), c(as.matrix(liste)))
  )
  class(matrice)

  if (empty.sq == TRUE) {
    return(matrice)
  }
  if (missing(empty.sq) || empty.sq == FALSE) {
    tabflow <- flowtabmat(matrice, matlist = "L")

    tabflow$ID_link <- paste(tabflow$i, tabflow$j, sep = "_")

    tab$ID_link <- paste(tab[, origin], tab[, dest], sep = "_")

    tabflow <- data.frame(tabflow, tab[match(tabflow[, "ID_link"], tab[, "ID_link"]), 2:3])

    tabmap <- data.frame(tabflow$i, tabflow$j, tabflow[, valflow], stringsAsFactors = FALSE)

    names(tabmap) <- c("i", "j", "ydata")

    result <- replace(tabmap, is.na(tabmap), 0)

    if (format == "L") {
      return(result)
    }

    if (format == "M") {
      matflow <- flowtabmat(result, matlist = "M")

      if (missing(diagonale)) {
        message("warning : the variable diagonale is missing so the diagonal of your matrix will be not empty!")
        return(matflow)
      }

      if (diagonale == TRUE) {
        return(matflow)
      }
      else if (diagonale == FALSE) {
        diag(matflow) <- 0
        return(matflow)
      }
    }
  }
}
