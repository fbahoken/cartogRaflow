#' Plot a layer of origin-destination flows values
#'
#' @param tab is the input flowdata table
#' @param format is the flowdata table format : M=matrix [n*n] or L=list [, i,j, Fij].
#' @param filter allows you to filter (or not) the flow dataset.
#' @param threshold is the value of the threshold criterion used to filter the values. The default threshold is set to 1
#' @param taille is a graphical parameter for modifying the width of the flow feature
#' @param fdc is the geometry or the geographical background file .shp composed of spatial units
#' @param code is the column with the spatial units ID code
#' @param a.head  integer code, determining the kind of arrows to be drawn.
#' @param a.length length of the edges of the arrow head (in inches).
#' @param a.angle angle from the shaft of the arrow to the edge of the arrow head.
#' @param a.col color of the arrows
#' @return a matrix or a list with the correct tabflow ID code
#' @return The resulting flowmap
#' @export
#' @details
#' The flow dataset must be converted to a dataframe for optimal performance (troubles remains with tibble format)
#'
#' filter allows to choose theoretical or empirical links to draw.
#' If filter = FALSE, all the matrice values are plot [(n*(n-1)] cells, i.e. all links out of the main diagonal.
#' If filter = TRUE only non-zero values are plotted, i.e. existing links with or without threshold.
#' The default threshold is set to 1.
#'
#' varf = "True" draw arrows, unless it draw links
#'
#' If varf="true", an arrows parameter is set.
#' code=0 : the link has no head.
#' code=1 : an arrow is draw at (x0[i], y0[i]).
#' code=2 : an arrow is draw at (x1[j], y1[j])
#' code=3 : an arrow is draw at both nodes.
#' @examples
#' ## Example 1 : flowmapping a network - draw non valued links
#' #library(cartograflow)
#' #flowmap(tabflow,format = "L",filter = FALSE, carte = "file.shp",code = "geoID",varf = FALSE)
#' ## Example 2 : flowmapping - draw valued and oriented links or arrows
#' #library(cartograflow)
#' #flowmap(tabflow,format = "L",fdc = "file.shp",filter = TRUE,
#' #threshold = (user value),taille = (user value),code = "geoID",
#' #a.head = 1 ,a.length = 0.2,a.angle=45,a.col="blue")
#' @importFrom graphics segments
#' @importFrom graphics arrows
#' @import rgdal
#' @seealso \code{arrows}

flowmap <- function(tab,format,fdc,code,filter,threshold,taille,
                    a.head,a.length,a.angle,a.col){

  if (format == "M" ){
    t.list<-flowtabmat(tab,matlist = "L")
    tabflow<-flowjointure(t.list,fdc,code)
    c.fdc <- readOGR(fdc,verbose = FALSE)
    plot(c.fdc,col="#cccccc",border="#f7f7f7",lwd=0.5)
    if(missing(filter)){filter<-FALSE}
    else filter
    if(filter == FALSE){
      trace<-segments(tabflow$X1, tabflow$Y1, tabflow$X2, tabflow$Y2,col="black")
      message("All theorical links are plotted")}
    if (filter == TRUE){
          if(missing(threshold)){
            threshold<-1
            message("you use the default threshold= 1")}
            else threshold
          tabflow<-tabflow[tabflow$ydata >=threshold,]
          if(missing(taille)){taille<-1}
            else taille
            maxsize<-taille
            tabflow$size<-(tabflow$ydata/max(tabflow$ydata))*maxsize
          if(missing(a.head)){a.head<-0}
            else a.head
          if(missing(a.length)){a.length<-0.1}
             else a.length
          if(missing(a.angle)){a.angle<-30}
             else a.angle
          if(missing(a.col)){a.col<-"#000311"}
            else a.col
          trace<-arrows(tabflow$X1, tabflow$Y1, tabflow$X2, tabflow$Y2,
                        length = a.length,angle = a.angle,code = a.head,col = a.col, lwd=tabflow$size)
        }
    }
if (format == "L"){
    if(ncol(tab)>3){message("votre liste doit avoir trois colonnes : origine, destination et le flux a cartographier")}
    c.fdc <- readOGR(fdc,verbose = FALSE)
    plot(c.fdc,col="#cccccc",border="#f7f7f7",lwd=0.5)
    tabflow<-flowjointure(tab,fdc,code)
    if(missing(filter)){filter<-FALSE}
      else filter
    if(filter == FALSE){
      trace<-segments(tabflow$X1, tabflow$Y1, tabflow$X2, tabflow$Y2,col="black")
      message("All theorical links are plotted")}
    if (filter == TRUE){
          if(missing(threshold)){
            threshold<-1
            message("warning : you use the default threshold= 1")}
          else threshold
          tabflow<-tabflow[tabflow$ydata >= threshold,]
          if(missing(taille)){taille<-1}
            else taille
          tabflow$size<-(tabflow$ydata/max(tabflow$ydata))*taille
          if(missing(a.head)){a.head<-0}
          else a.head
           if(missing(a.length)){a.length<-0.1}
           else a.length
          if(missing(a.angle)){a.angle<-30}
            else a.angle
          if(missing(a.col)){a.col<-"#000311"}
            else a.col
          trace<-arrows(tabflow$X1, tabflow$Y1, tabflow$X2, tabflow$Y2,
                        length = a.length,angle = a.angle,code = a.head,col = a.col, lwd=tabflow$size)
    }
  }
}
