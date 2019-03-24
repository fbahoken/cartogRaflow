#' Compute flowdata types
#'
#' Compute gross and net flows from initial asymetric flow values
#' @param tab is the input flowdata table
#' @param format specify the flowdata format : M = square matrix [n*n] or L=list [i,j,data]
#' i= origin and y=destination and data=relationnal data
#' @param x enter the computation type : "flux", "transpose", "bivolum" and "bisold".
#' @details The matrix must be squared (if not, see flowcarre.R).
#' This function compute for all (i,j) index involved in an asymetric flow matrix (Fij) several matrix.
#' "flux" for initial flow (Fij) - "transpose" for reverse flow value (Fji) - "bivolum" for bilateral gross flow Vij=(Fij+Fji) -
#' "bisold" for bilateral net flow Sij=(Fij-Fji).
#' @examples
#' ## Example 1 : compute bivolum
#' #library(cartograflow)
#' #y<-flowtype(tab,format="M","bivolum")
#' #Then use flowmap.R for mapping gross flow with non valued links
#' #flowmap(y,format = "M",filtre= TRUE,seuil = 100, taille = 20,
#' #fdc = "isere_com.shp",code = "INSEE_COM",varf=FALSE)
#' @references
#' Bahoken Francoise, 2016, L'approche cartographique de la décomposition des matrices de flux », Mappemonde, Revue sur l'image géographique et les formes du territoire, n°116, URL : https://mappemonde-archive.mgm.fr/num44/articles/art14404.html
#' @export

flowtype<-function(tab,format,x){

if(format == "M"){
  if(nrow(tab)!=ncol(tab)){warning("your matrix is not square")}
  if (x== "flux"){return(tab)}
  if  (x == "transpose"){
    fji<-t(tab)
    return(fji)}
  if (x == "bivolum"){
    fji<-t(tab)
    vij<-tab+fji
    return(vij)}
  if (x == "bisold"){
    fji<-t(tab)
    sij<-tab-fji
    return(sij)}
}
if(format=="L"){

  f1<-data.frame(tab$i,tab$j,tab$Fij)
  names(f1)<-c("i","j","Fij")
  f2<-data.frame(tab$j,tab$i,tab$Fij)
  names(f2)<-c("i","j","Fji")
  tabflow<-merge(f1,f2,by=c("i","j"),all.X=TRUE,all.Y=TRUE)
  tabflow$FSij<-tabflow$Fij+tabflow$Fji
  tabflow$FDij<-tabflow$Fji-tabflow$Fij

  if (missing(x)){message("renseigner un choix de calcul : all, flux, transpose, solde, volume")}
  if(x == "all"){return(tabflow)}
  if(x == "flux"){
    result<-data.frame(tabflow$i,tabflow$j,tabflow$Fij)
    names(result)<-c("i","j","Fij")
    return(result)
                 }
  if(x =="transpose"){
    result<-data.frame(tabflow$i,tabflow$j,tabflow$Fji)
    names(result)<-c("i","j","Fji")
    return(result)
                    }
  if (x=="bivolum"){
    result<-data.frame(tabflow$i,tabflow$j,tabflow$FSij)
    names(result)<-c("i","j","FSij")
    return(result)
                  }
  if (x=="bisold"){
    result<-data.frame(tabflow$i,tabflow$j,tabflow$FDij)
    names(result)<-c("i","j","FDij")
    return(result)
                  }
 }
}

