#' @title Computes flow indicators from the margins of the matrix
#' @description Compute indicators on the places of origin and/or destination (in and out degrees, gross and net flows, asymmetry) from initial (a)symetric flow values
#' @param tab is the input flow dataset
#' @param origin the place of origin code
#' @param destination the place of destination code
#' @param fij the flow value between origin and destination places
#' @param format specify the flow dataset format, "M " for square matrix [n*n] or "L" for long [i,j,data]
#' @param x enter the flowplaces indicator type : "allflowplaces", "ini", "outi", "degi", "Oi", "Dj", voli","bali","asyi". See Details.
#' @details 
#' This function compute for all pairs or origin-destination places (i,j)
#' a data table that describes the flows from the point of view of Origin / destination places
#' - x = "ini" for the number of incoming links (as in-degree) \cr
#' - x = "outi" for the number of outcoming links (as out-degree)\cr
#' - x = "degi" for the total number of links (as in and out degrees)\cr
#' - x = "Oi" for the total flows emitted by (i) place  \cr
#' - x = "bali" for the total flows received by (j) place \cr
#' - x = "voli" for the total flow volume by place \cr
#' - x = "bali" for the net balance by place \cr
#' - x = "asyi" for the asymetry of flow by place \cr
#' - x = "allflowplaces" for computing all the above indicators\cr
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
#' #1:Computes the total flow volume of places : Long format
#' voli <- flowplaces(tab, origin ="i",destination="j",fij="Fij",format = "L", x = "voli")
#' #2:Computes the total flows received by destination place : Long format
#' tab_bali <- flowplaces(tab, origin ="i",destination="j",fij="Fij",format = "L", x = "bali")
#' @export

flowplaces <- function(tab,origin=NULL,destination=NULL,fij=NULL,format, x) {
  
  
  
  if (format == "L") {
    
    tab <- tab %>% select(origin,destination,fij)
    names(tab) <- c("i", "j", "Fij")
    
    
    tabOi<-tab %>%
          group_by(i) %>%
          summarise(outi = n(),Oi = sum(.data$Fij))
                 
                
    # Dj : marginal sum of the place of destination
    tabDj<-tab %>%
          group_by(j)%>%
          summarise(ini = n(),Dj = sum(.data$Fij))
                          
    colnames(tabDj)<-c("i","ini","Dj")
                  
    as.data.frame(tabOi)
    as.data.frame(tabDj)
      
    # merge Oi Dj
     
    tabOiDj<-left_join(tabOi,tabDj,
                      by="i")
                  
    # Add Asy
    
    tabOiDj <- tabOiDj %>%
             mutate (voli=Oi+Dj, bali=Oi-Dj, asyi=bali/voli)
                  
    tabOiDj$i<-as.character(tabOiDj$i)
    tabOiDj$ini<-as.numeric(tabOiDj$ini)
    tabOiDj$outi<-as.numeric(tabOiDj$outi)
    tabOiDj$Oi<-as.numeric(tabOiDj$Oi)
    tabOiDj$Dj<-as.numeric(tabOiDj$Dj)
    tabOiDj$Vol<-as.numeric(tabOiDj$voli)
    tabOiDj$Bal<-as.numeric(tabOiDj$bali)
    tabOiDj$Asy<-as.numeric(tabOiDj$asyi)
   
      
    if (missing(x)) {
      message("You must specify a choice of flowplaces computation : alltypes, ini, degi, outi, Oi,Dj,...")
    }
    if (x == "allflowplaces") {
      return(tabOiDj)
    }
     
    if (x == "ini") {
      tabOiDj <- tabOiDj %>% select(.data$i,.data$ini)
      return(tabOiDj)
    }
      
          if (x == "outi") {
      tabOiDj <- tabOiDj %>% select(.data$i,.data$outi)
      return(tabOiDj)
    }
      
     if (x == "degi") {
       tabOiDj <- tabOiDj %>% select(.data$i,.data$degi)
        return(tabOiDj)
      } 
      
      
    if (x == "voli") {
      tabOiDj <- tabOiDj %>% select(.data$i,.data$voli)
      return(tabOiDj)
    }
    
    if (x == "bali") {
      tabOiDj <- tabOiDj %>% select(.data$i,.data$bali)
      return(tabOiDj)
    }
    
    if (x == "asyi") {
      tabOiDj <- tabOiDj %>% select(.data$i,.data$asyi)
      return(tabOiDj)
    }
   
  }
}