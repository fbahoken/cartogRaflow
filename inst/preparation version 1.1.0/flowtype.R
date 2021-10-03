#' @title Compute flowdata types (volum, balance)
#' @description Compute gross (volumn) and net (balance) flows from initial asymetric flow values
#' @param tab is the input flow dataset
#' @param origin the place of origin code
#' @param destination the place of destination code
#' @param fij the flow value between origin and destination places
#' @param format specify the flow dataset format, "M " for square matrix [n*n] or L for long [i,j,data]
#' @param x enter the computation type : "alltypes", "flux", "transpose", "bivolum", "bibal","biasym","bimin", "bimax","birange" and "bidisym"
#' @details The matrice must be squared (if not, see \link{flowcarre}).
#' This function compute for all pairs or origin-destination places (i,j)
#' involved in an asymetric flow matrix (Fij<> Fji) several matrix :\cr
#' - x = "flux" for remaining initial flow (Fij)\cr
#' - x = "transpose" for reverse flow value (Fji)\cr
#' - x = "bivolum" for bilateral gross flow Vij=(Fij+Fji)\cr
#' - x = "bibal" for bilateral balance or net flow Sij=(Fij-Fji) \cr
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
#' ## 1a:Computes flowtypes: Matrice format
#' matflow <- flowtabmat(flows, matlist = "M")
#' m <- flowtype(matflow, format = "M", x = "flux")
#' m <- flowtype(matflow, format = "M", x = "transpose")
#' m <- flowtype(matflow, format = "M", x = "bivolum")
#' m <- flowtype(matflow, format = "M", x = "bibal")
#'
#' ## 1b:Computes flowtypes: Long format
#' list <- flowtabmat(matflow, matlist = "L")
#' colnames(list) <- c("i", "j", "Fij")
#' l_all <- flowtype(list,origin ="i",destination="j",fij="Fij", format = "L", x = "alltypes")
#' l_sold <- flowtype(list, origin ="i",destination="j",fij="Fij",format = "L", x = "bibal")
#' \donttest{
#' # 2:flowmapping: example of bibal
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

flowtype <- function(tab,origin=NULL,destination=NULL,fij=NULL,format, x) {
  
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
              
              if(x== "bibal" || x== "biasym" || x== "bimin" || x == "bmax" || x == "range" ){
                message("This flow type is only available the format L.")
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
              message("renseigner un choix de calcul : alltypes, flux, transpose, bivolum ...")
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
              tabflow <- tabflow %>% select(.data$i,.data$j,.data$FSij)
              return(tabflow)
            }
        
            if (x == "bibal") {
              tabflow <- tabflow %>% select(.data$i,.data$j,.data$FBij)
              return(tabflow)
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
