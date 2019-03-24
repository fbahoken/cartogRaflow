#' Computes ordinal distance matrixes based on contiguity
#'
#' From a spatial basemap, compute an ordinal distance matrix based on a k-contiguity matrix.
#' The resulting neighbourhood graph can be map using \link{flowmap}
#' @param fdc is the map background file, ie. a shapefile of polygons
#' @param code identifiant in shape file
#' @param ordre ordre est à 1, 2 ou 4
#' @return a (k) contiguity matrice with the k contiguity measures
#' @details
#' Contiguity is in terms of the (k=1,2,4) numberof spatial boundaries to be crossed between
#' a place of origin and a place of destination
#' ordre=1 is when the flow have to cross only 1 boundary, that is to say the corresponding origin and destination places are
#' contiguous, adjacent
#' ordre=2 is when the origin-destinations places are distant from 2 borders
#' ordre=4 is when the origin-destinations places are distant from 4 borders
#' @export
#' @importFrom rgeos gIntersects


flowcontig<-function(fdc,code,ordre){
            ordre1<-function(fdc,code){
            carte<- readOGR(fdc,verbose = FALSE)
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
            ordre2<-function(c1){
              CKij_1<-c1
              CKij_1$CODE_i1<-0
              CKij_1$CODE_j1<-0
              CKij_1$CODE_j2<-0
              CKij_1$cij2<-0

              for (k in 1:nrow(CKij_1))
              {
                if (CKij_1$cij[k]==1)
                {
                  col_i<-CKij_1$CODE_i[k]
                  col_j<-CKij_1$CODE_j[k]
                  for (v in 1:nrow(CKij_1)){
                    if (CKij_1$CODE_i[v]==col_j && CKij_1$CODE_i[v]!=CKij_1$CODE_j[v]){
                      CKij_1$CODE_i1[k]<-col_i
                      CKij_1$CODE_j1[k]<-col_j
                      CKij_1$CODE_j2[k]<- CKij_1$CODE_j[v]
                      CKij_1$cij2[k]<-2}
                  }
                }
              }
              ordre_2<-data.frame(CKij_1$CODE_i,CKij_1$CODE_j2,CKij_1$cij2)
              names(ordre_2) = c("i", "j2","cij2")
              for (z in 1:nrow(ordre_2))
                if (ordre_2$i[z] == ordre_2$j2[z]) ordre_2$cij2[z]<-0
              ordre_2<-ordre_2[ordre_2[,"cij2"]!=0,]
            }

            ordre4<-function(c2){
            ordre_2<-c2
            ordre_2$CODE_i<-0
            ordre_2$CODE_j2<-0
            ordre_2$CODE_j4<-0
            ordre_2$cij4<-0
            for (k in 1:nrow(ordre_2)){
              if (ordre_2$cij2[k]==2)
              {
                col_i4<-ordre_2$i[k]
                col_j4<-ordre_2$j2[k]

                for (v in 1:nrow(ordre_2))
                {
                  if (ordre_2$i[v]==col_j4 && ordre_2$i[v]!=ordre_2$j2[v]){
                  ordre_2$CODE_i[k]<-col_i4
                  ordre_2$CODE_j2[k]<-col_j4
                  ordre_2$CODE_j4[k]<-ordre_2$j2[v]
                  ordre_2$cij4[k]<-4}
                }
              }
             }
            ordre_4<-data.frame(ordre_2$CODE_i,ordre_2$CODE_j4,ordre_2$cij4)
            names(ordre_4) = c("i", "j4","cij4")
            for (z in 1:nrow(ordre_4))
              if (ordre_4$i[z] == ordre_4$j4[z]) ordre_4$cij4[z]<-0
            ordre_4<-ordre_4[ordre_4[,"cij4"]!=0,]
            }

  if (ordre=="1"){
    tab1<-ordre1(fdc,code)
    colnames(tab1)<-c("i","j","ydata")
    return(tab1)}
  if (ordre=="2"){
    tab1<-ordre1(fdc,code)
    tab2<-ordre2(tab1)
    colnames(tab2)<-c("i","j","ydata")
    return(tab2)}
  if (ordre=="4"){
    tab1<-ordre1(fdc,code)
    tab2<-ordre2(tab1)
    tab4<-ordre4(tab2)
    colnames(tab4)<-c("i","j","ydata")
    return(tab4)}
}


