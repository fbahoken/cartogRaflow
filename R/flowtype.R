#' @title Compute bilateral several flow types
#' @description Compute bilateral flow type: volumn (gross), balance (net), asymetry, min/max ... from an initial asymetric matrix
#' @param tab is the input flow dataset
#' @param origin the place of origin code
#' @param destination the place of destination code
#' @param fij the flow value between origin and destination places
#' @param format specify the flow dataset format, "M " for square matrix [n*n] or L for long [i,j,data]
#' @param x enter the flow indicator type : "alltypes", "flux", "transpose", "bivolum", "bibal","biasym","bimin", "bimax","birange" and "bidisym"
#' @param lowup for extracting the lower or upper triangular sub-portion of the bilateral volum matrix. See Details.
#' @param net for extracting the "positive" or the "negative" flow values of the bilateral balance) matrix
#' @details The matrice must be squared (if not, see \link{flowcarre}).
#' This function compute for all pairs or origin-destination places (i,j)
#' involved in an asymetric flow matrix (Fij<> Fji) several matrix :\cr
#' Param x:
#' - x = "flux" for the initial flow: (Fij)\cr
#' - x = "transpose" for the reverse flow value: (Fji) =t(Fij)\cr
#' - x = "bivolum" for the bilateral volum or gross flow: FSij=(Fij+Fji)\cr
#' - x = "bibal" for the bilateral balance or net flow: FBij=(Fij-Fji) \cr
#' - x = "biasym" for asymetry of bilateral flow: FAij=(FBij/FSij)\cr
#' - x = "bimin" for the minimum of bilateral flow: minFij=(Fij, Fji)\cr
#' - x = "bimax" for the maximum of bilateral flow: Fij(Fij, Fji)\cr
#' - x = "birange" for the amplitude of bilateral flows: rangeFij=(maxFij - minFij)\cr
#' - x = "bidisym" for the bilateral disymetry: FDij=(rangeFij/FSij)
#' - x = "alltypes" for computing all the available types of flows \cr
#' Param lowup is for reducing the matrix:\cr
#' - lowup ="up" for triangular part above the main diagonal \cr
#' - lowup = "low" for triangular part below the main diagonal\cr
#' Param net is for extracting positive or negative flow values of the bilateral balance (bibalà matrix:\cr 
#' - net ="negative" values\cr
#' - net ="positive" values\cr
#' @import dplyr
#' @importFrom rlang .data
#' @examples
#' library(cartograflow)
#' data(flowdata)
#' bkg <- system.file("shape/MGP_TER.shp",
#'   package = "cartograflow",
#'   lib.loc = NULL, mustWork = TRUE
#' )
#'
#' ## 1a:Computes flowtypes: Matrix format
#' matflow <- flowtabmat(flows, matlist = "M")
#' m <- flowtype(matflow, format = "M", x = "flux")
#' m <- flowtype(matflow, format = "M", x = "transpose")
#' m <- flowtype(matflow, format = "M", x = "bivolum")
#' m <- flowtype(matflow, format = "M", x = "bibal")
#'
#' ## 1b:Computes flowtypes: Long format
#' types_all <- flowtype(flows,origin ="i",destination="j",fij="Fij", format = "L",
#' x = "alltypes")
#' bivol<- flowtype(flows,origin ="i",destination="j",fij="Fij",format = "L",
#' x = "bivolum",lowup="up")
#' bibal_net<- flowtype(flows,origin ="i",destination="j",fij="Fij",format = "L",
#' x = "bibal", net="negative")
#' \donttest{
#' # 2:flowmapping: example of bibal_net
#' flowmap(bibal_net,
#'   format = "L", bkg, code = "EPT_NUM",
#'   filter = TRUE, threshold = 20, taille = 5, a.head = 1,
#' )
#' }
#' @references
#' Bahoken Francoise, 2016, L'approche cartographique de la décomposition des matrices de flux,
#' Mappemonde, Revue sur l'image géographique et les formes du territoire,
#' number 116, URL : https://mappemonde-archive.mgm.fr/num44/articles/art14404.html
#' @export

flowtype <- function(tab,origin=NULL,destination=NULL,fij=NULL,format, lowup, net,x) {
  
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
          
              if (x == "bibal") {
                fji <- t(tab)
                sij <- tab - fji
                return(sij)
              }
              
              if(x== "bibal" || x== "biasym" || x== "bimin" || x == "bmax" || x == "birange" ){
                message("This flow type is only available with the L format")
              }
    
  }
  
  if (format == "L") {
    maxFij<-Fij<-Fji<-NULL 
    minFij<-Fij<-Fji<-NULL 
    
    f1 <- tab %>% select(origin,destination,fij)
    names(f1) <- c("i", "j", "Fij")
   
    f2<-tab %>% select(destination,origin,fij)
    names(f2) <- c("i", "j", "Fji")
    
    tabflow <- merge(f1, f2, by = c("i", "j"), all.X = TRUE, all.Y = TRUE)

    tabflow<- tabflow %>% 
              mutate(FSij = .data$Fij + .data$Fji)%>% 
              mutate(FBij = .data$Fji - .data$Fij) %>% 
              mutate(FAij = .data$FBij/ .data$FSij) %>% 
              mutate(minFij = ifelse(.data$Fij < .data$Fji, .data$Fij, .data$Fji)) %>%
              mutate(maxFij = ifelse(.data$Fij > .data$Fji, .data$Fij, .data$Fji)) %>%
              mutate(rangeFij = maxFij - minFij) %>%
              mutate(FDij = (.data$rangeFij)/.data$FSij)
    

            if (missing(x)) {
              message("You must specify a choice of flow computation : alltypes, flux, transpose, bivolum ...")
            }
            if (x == "alltypes") {
              return(tabflow)
            }
        
            if (x == "flux") {
              tabflow <- tabflow %>% select(.data$i,.data$j,.data$Fij)
              return(tabflow)
            }
        
            if (x == "transpose") {
              tabflow <- tabflow %>% select(.data$i,.data$j,.data$Fji)
              return(tabflow)
            }
        
            if (x == "bivolum") {
                    flow_gross <- tabflow %>% select(.data$i,.data$j,.data$FSij)
                    
                    tab_up<-flowtabmat(flow_gross,matlist = "M")
                    temp_up<-lower.tri(tab_up, diag = FALSE)
                    
                    tab_low<-flowtabmat(flow_gross,matlist = "M")
                    temp_low<-upper.tri(tab_low,diag=FALSE)
                    
                    nbi<-dim(tab_up)[1]
                    nbj<-dim(tab_up)[2]
                    
                    for (i in 1:nbi){
                      for (j in 1:nbj){
                        if (temp_up[i,j] == TRUE){tab_up[i,j]<-0 }
                        if (temp_low[i,j] == TRUE){tab_low[i,j]<-0 }
                      }}
                    
                    tabflow_low<-cartograflow::flowtabmat(tab_low,matlist = "L")
                    tabflow_up<-cartograflow::flowtabmat(tab_up,matlist = "L")
                    
                    if(missing(lowup)){return(flow_gross)}
                    
                    if (lowup == "up"){
                      return(tabflow_up)}
                    
                    if (lowup == "low"){
                      return(tabflow_low)}    
            }
                    
    
            if (x == "bibal") {
              tabflow_net <- tabflow %>% select(.data$i,.data$j,.data$FBij)
             
                  if (missing(net)){return(tabflow_net)}
      
                  if (net == "negative"){
                  tabflow_net <- tabflow_net %>% filter(.data$FBij<0)
                  return(tabflow_net)}
      
                  if (net == "positive") {
                  tabflow_net <- tabflow_net %>% filter(.data$FBij>=0)
                  return(tabflow_net)}
            }  
    
    
            if (x == "biasym") {
              tabflow <- tabflow %>% select(.data$i,.data$j,.data$FAij)
              return(tabflow)
            }
            
            if (x == "bimin") {
              tabflow <- tabflow %>% select(.data$i,.data$j,.data$minFij)
              return(tabflow)
            }
            
            if (x == "bimax") {
              tabflow <- tabflow %>% select(.data$i,.data$j,.data$maxFij)
              return(tabflow)
            }
            
            if (x == "birange") {
              tabflow <- tabflow %>% select(.data$i,.data$j,.data$rangeFij)
              return(tabflow)
            }
            
            if (x == "bidisym") {
                tabflow <- tabflow %>% select(.data$i,.data$j,.data$FDij)
                return(tabflow)
              }
    
          }
}
