#' @title Builds an ordinal distance matrice from a territorial grid
#' @description
#' From a territorial background, compute an ordinal matrice based on k-contiguity.
#' The result is a neighbourhood graph that can be used to filter the flow values to be mapped (\link{flowmap} and \link{flowreduct})
#' @param bkg is the territorial background file - ie. a spatial shape of polygons
#' @param code geographical code of the spatial entities
#' @param k contiguity order level
#' @param algo network distance calculation algorithm. See Details.
#' @return a contiguity matrice with the k level of contiguity measures
#' @details
#' Contiguity is in terms of the (k=1,2,4, ... k) number of spatial boundaries to be crossed between
#' an "i" place of origin and a "j" place of destination.\cr
#' k=1 means that "i" and "j" are adjacent to each other.
#' 
#' algo : the network distance calculation algorithm available are : \cr
#' "automatic", "unweighted","dijkstra", "bellman-ford", "johnson"
#' @examples
#' library(cartograflow)
#' library(sf)
#' data(flowdata)
#' map<- st_read(system.file("shape/MGP_TER.shp", package="cartograflow"))
#' graph_ckij_1<-flowcontig(bkg = map,code ="EPT_NUM",k=1,algo="automatic")
#' \donttest{
#' flowmap(tab=graph_ckij_1,
#'         fij="ordre",origin.f = "i",destination.f = "j",
#'         bkg = map,code="EPT_NUM",nodes.X="X",nodes.Y = "Y",
#'         filter=FALSE)
#'   }
#' @importFrom rgeos gIntersects
#' @importFrom sf as_Spatial
#' @importFrom igraph V
#' @export

flowcontig<-function(bkg,code,k,algo){
  
  if (missing(algo)){algo<-"automatic"}
  else algo
  
            ordre1<-function(bkg,code){
                                      carte<- as_Spatial(bkg)
                                      contig<-gIntersects(carte, byid = TRUE, prepared=TRUE)
                                      row.names(contig)<-carte@data[,code]
                                      colnames(contig)<-carte@data[,code]
                                      
                                      for (i in 1:nrow(contig))
                                        for (j in 1:ncol(contig))
                                        {if (contig[i,j]==TRUE) {contig[i,j]<-1}
                                          if (contig[i,i]!=0)  {contig[i,i]<-0}
                                        }
                                      tab <-flowtabmat(contig,matlist = "L")
                                      colnames(tab)<-c("CODE_i","CODE_j","cij")
                                      ordre_1<-tab[tab[,"cij"]!=0,]
    }

    contig_1<-ordre1(bkg,code)
    
    graph_voisinage<-igraph::graph.data.frame(contig_1)
            
    contig_k<-igraph::distances(graph_voisinage, v = V(graph_voisinage), to = V(graph_voisinage), mode = c("all", "out", "in"),
                                 weights = NULL, algorithm = algo)
            
    tabcontig_k<-flowtabmat(contig_k,matlist = "L")
    colnames(tabcontig_k)<-c("i","j","ordre")
    
    tabcontig_k<-tabcontig_k %>%
                 filter(.data$ordre!=0) %>% 
                 filter(.data$ordre!="Inf")
    max<-paste("Maximum adjacency order =",max(tabcontig_k$ordre))
    print(max) 
    
    tabcontig_k<-tabcontig_k %>%
                 filter(.data$ordre<=k)
    return(tabcontig_k)
 
}
