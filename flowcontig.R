#' @title Builds an ordinal distance matrices from a spatial features background
#' @description
#' From a layer of areal spatial features, compute an ordinal distance matrice
#' based on a k order criterion of adjacency or contiguity between origin and destination places . \cr
#' The result is a neighbourhood graph that can be used for filtering flow values before flow mapping (\link{flowmap})
#' @param bkg a layer of areal spatial features (eg. the map background)
#' @param code spatial areal features code
#' @param k order of adjacency or contiguity between two areal spatial features
#' @param algo algorithm to use for ordinal distance calculation. Default is "Dijkstra's"  algorithm. See Details.
#' @return a contiguity matrice with the k orders of adjacency
#' @details
#' The (k=1,2,...,k) order of adjacency or contiguity, of an areal spatial features
#' background, is the number of spatial boundaries to be crossed between
#' a couple of origin-destination (ODs) places. The k number can be assimilated to a shortest path between two pair of nodes
#' Argument `k` is to enter the number k of the contiguity matrix to be constructed ;
#' -k{ordre=1 : ODs places are adjacent, ie the flow have to cross only 1 boundary.}\cr
#' -k{ordre=2 : ODs places are distant from 2 borders}\cr
#' -k{ordre=k : ODs places are distant from k borders}\cr
#' The function returns also the (k) number of the layer
#' @examples
#' library(cartograflow)
#' library(sf)
#' data(flowdata)
#' map <- st_read(system.file("shape/MGP_TER.shp", package = "cartograflow"))
#' graph_ckij_1 <- flowcontig(bkg = map, code = "EPT_NUM", k = 1, algo = "automatic")
#' \donttest{
#' flowmap(
#'   tab = graph_ckij_1,
#'   fij = "ordre", origin.f = "i", destination.f = "j",
#'   bkg = map, code = "EPT_NUM", nodes.X = "X", nodes.Y = "Y",
#'   filter = FALSE
#' )
#' }
#' @importFrom rgeos gIntersects
#' @importFrom sf as_Spatial
#' @importFrom igraph V
#' @export

flowcontig <- function(bkg, code, k, algo) {
  if (missing(algo)) {
    algo <- "automatic"
  }
  else {
    algo
  }

  ordre1 <- function(bkg, code) {
    carte <- as_Spatial(bkg)
    contig <- gIntersects(carte, byid = TRUE, prepared = TRUE)
    row.names(contig) <- carte@data[, code]
    colnames(contig) <- carte@data[, code]

    for (i in 1:nrow(contig)) {
      for (j in 1:ncol(contig))
      {
        if (contig[i, j] == TRUE) {
          contig[i, j] <- 1
        }
        if (contig[i, i] != 0) {
          contig[i, i] <- 0
        }
      }
    }
    tab <- flowtabmat(contig, matlist = "L")
    colnames(tab) <- c("CODE_i", "CODE_j", "cij")
    ordre_1 <- tab[tab[, "cij"] != 0, ]
  }

  contig_1 <- ordre1(bkg, code)

  graph_voisinage <- igraph::graph.data.frame(contig_1)

  contig_k <- igraph::distances(graph_voisinage,
    v = V(graph_voisinage), to = V(graph_voisinage), mode = c("all", "out", "in"),
    weights = NULL, algorithm = algo
  )

  tabcontig_k <- flowtabmat(contig_k, matlist = "L")
  colnames(tabcontig_k) <- c("i", "j", "ordre")

  tabcontig_k <- tabcontig_k %>%
    filter(.data$ordre != 0) %>%
    filter(.data$ordre != "Inf")
  max <- paste("ordre max =", max(tabcontig_k$ordre))
  print(max)

  tabcontig_k <- tabcontig_k %>%
    filter(.data$ordre <= k)
  return(tabcontig_k)
}
