#' @title Compute flowdata types (volum, balance)
#' @description Compute gross (volumn) and net (balance) flows from initial asymetric flow values
#' @param tab is the input flow dataset
#' @param format specify the flow dataset format, "M " for square matrix [n*n] or L for long [i,j,data]
#' @param x enter the computation type : "flux", "transpose", "bivolum" and "bisold".
#' @details The matrice must be squared (if not, see \link{flowcarre}).
#' This function compute for all pairs or origin-destination places (i,j)
#' involved in an asymetric flow matrix (Fij<> Fji) several matrix :\cr
#' - x = "flux" for remaining initial flow (Fij)\cr
#' - x = "transpose" for reverse flow value (Fji)\cr
#' - x = "bivolum" for bilateral gross flow Vij=(Fij+Fji)\cr
#' - x = "bisold" for bilateral net flow Sij=(Fij-Fji) \cr
#' @examples
#' library(cartograflow)
#' data(flowdata)
#' bkg <- system.file("shape/MGP_TER.shp",
#'   package = "cartograflow",
#'   lib.loc = NULL, mustWork = TRUE
#' )
#'
#' ## 1a:Computes flowtypes: Matrice format
#' matflow <- flowtabmat(flows, matlist = "M")
#' m <- flowtype(matflow, format = "M", x = "flux")
#' m <- flowtype(matflow, format = "M", x = "transpose")
#' m <- flowtype(matflow, format = "M", x = "bivolum")
#' m <- flowtype(matflow, format = "M", x = "bisold")
#'
#' ## 1b:Computes flowtypes: Long format
#' list <- flowtabmat(matflow, matlist = "L")
#' colnames(list) <- c("i", "j", "Fij")
#' l_all <- flowtype(list, format = "L", x = "all")
#' l_sold <- flowtype(list, format = "L", x = "bisold")
#' \donttest{
#' # 2:flowmapping: example of bisold
#' flowmap(l_sold,
#'   format = "L", bkg, code = "EPT_NUM",
#'   filter = TRUE, threshold = 20, taille = 5
#' )
#' }
#' @references
#' Bahoken Francoise, 2016, L'approche cartographique de la décomposition des matrices de flux,
#' Mappemonde, Revue sur l'image géographique et les formes du territoire,
#' number 116, URL : https://mappemonde-archive.mgm.fr/num44/articles/art14404.html
#' @export

flowtype <- function(tab, format, x) {
  if (format == "M") {
    if (nrow(tab) != ncol(tab)) {
      warning("your matrix is not square")
    }

    if (x == "flux") {
      return(tab)
    }

    if (x == "transpose") {
      fji <- t(tab)
      return(fji)
    }

    if (x == "bivolum") {
      fji <- t(tab)
      vij <- tab + fji
      return(vij)
    }

    if (x == "bisold") {
      fji <- t(tab)
      sij <- tab - fji
      return(sij)
    }
  }
  if (format == "L") {
    f1 <- data.frame(tab$i, tab$j, tab$Fij)
    names(f1) <- c("i", "j", "Fij")
    f2 <- data.frame(tab$j, tab$i, tab$Fij)
    names(f2) <- c("i", "j", "Fji")
    tabflow <- merge(f1, f2, by = c("i", "j"), all.X = TRUE, all.Y = TRUE)
    tabflow$FSij <- tabflow$Fij + tabflow$Fji
    tabflow$FDij <- tabflow$Fji - tabflow$Fij

    if (missing(x)) {
      message("renseigner un choix de calcul : all, flux, transpose, solde, volume")
    }
    if (x == "all") {
      return(tabflow)
    }

    if (x == "flux") {
      result <- data.frame(tabflow$i, tabflow$j, tabflow$Fij)
      names(result) <- c("i", "j", "Fij")
      return(result)
    }

    if (x == "transpose") {
      result <- data.frame(tabflow$i, tabflow$j, tabflow$Fji)
      names(result) <- c("i", "j", "Fji")
      return(result)
    }

    if (x == "bivolum") {
      result <- data.frame(tabflow$i, tabflow$j, tabflow$FSij)
      names(result) <- c("i", "j", "FSij")
      return(result)
    }

    if (x == "bisold") {
      result <- data.frame(tabflow$i, tabflow$j, tabflow$FDij)
      names(result) <- c("i", "j", "FDij")
      return(result)
    }
  }
}
