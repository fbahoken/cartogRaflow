#' @title Mapping of an origin-destination flow matrix 
#' @param tab the input flow dataset in .csv format. See Details
#' @param fij the flow value between origin and destination places
#' @param origin.f the place of origin code
#' @param destination.f the place of destination code
#' @param crs the coordinate reference system (CRS) 
#' @param nodes the input points file in .csv format
#' @param code the spatial features code
#' @param nodes.X the X coordinate of the point or places
#' @param nodes.Y the Y coordinate of the point or places
#' @param filter is to filter or not the flow values. See details
#' @param threshold the value of the threshold criterion to filter flows. Default is 1.
#' @param taille the value of the width of the flow feature
#' @param bkg a spatial feature layer, as a map background, in .shp or .json or other format
#' @param a.head  for arrow's head is the arrow head parameter code. 
#' It allows to choose the kind of arrow. See Details
#' @param a.length for arrow's length is the length of the edges of the arrow head (in inches)
#' @param a.angle for arrow's angle is the angle from the shaft of the arrow to the edge of the arrow head
#' @param a.col for arrow's color
#' @param plota is to add spatial features as map background to the flows's plot
#' @param add is to allow to overlay flow features on external spatial features background
#' @param ... Adds the set of variables of the arrow function
#' @return a matrix or a list with the correct flow dataframe ID code
#' @return The resulting flowmap
#' @details
#' The input .csv flow dataset must be first converted to a dataframe
#' for optimal performance (troubles remains with tibble format)\cr \cr
#'
#' - filter is "FALSE" means that all the flow value will be plot as segments [(n*(n-1)], 
#' i.e. all the OD matrice's cells out of the main diagonal will be plot.\cr
#' - filter is "TRUE" means only non-zero values will be plot,
#' i.e. existing links with or without threshold.\cr
#' The default threshold is set to 1.\cr
#'
#' Flow features are plot as segments betwwen (x0,y0) and (x1,y1)\cr
#' - a.head is for applying an arrow or not to a segment:\cr
#' -- code="0" : the link has no head - no arrow\cr
#' -- code="1" : an arrow is draw at (x0[i], y0[i])\cr
#' -- code="2" : an arrow is draw at (x1[j], y1[j])\cr
#' -- code="3" : an arrow is draw at both nodes.\cr
#' @importFrom graphics segments
#' @importFrom graphics arrows
#' @import sf
#' @examples
#' library(cartograflow)
#' library(sf)
#' data(flowdata)
#' # example with the background
#' map <- st_read(system.file("shape/MGP_TER.shp", package = "cartograflow"))
#' par(bg = "NA")
#' plot(st_geometry(map), col = "blue")
#' flowmap(
#'   tab = flows, fij = "Fij", origin.f = "i", destination.f = "j",
#'   bkg = map,add=TRUE, code = "EPT_NUM", nodes.X = "X", nodes.Y = "Y",
#'   filter = FALSE
#' )
#' \donttest{
#' # example with nodes files
#' map <- st_read("MGP_territoires.json")
#' pt <- read.csv2("points.csv")# points files origin destination
#' flows<-red.cs2("flows.csv") # flows files 
#' par(bg = "NA")
#' plot(st_geometry(map), col = "blue")
#' flowmap(tab = flows, fij = "d", origin.f = "a", destination.f = "b",
#'         crs=4326,nodes = pt, code = "EPT_NUM", nodes.X = "X", nodes.Y = "Y",
#'         filter = TRUE,add=TRUE,threshold = 10, taille = 8,arr.length = 0.1)
#' }
#' @export

flowmap <- function(tab, fij, origin.f, destination.f, 
                    bkg = NULL,crs,
                    nodes = NULL, code, nodes.X, nodes.Y,
                    filter, plota, threshold, taille,
                    a.head, a.length, a.angle, a.col,add=NULL,...){
  
  tab <- tab %>% select(origin.f, destination.f, fij)
  

  if (!is.null(nodes)) {geo <- "pt"
  
                        nodes <- nodes %>% select(code,nodes.X,nodes.Y)
  
                        mgp_flow <- flowjointure(geom = geo, 
                                                 DF.flow = tab, origin = origin.f, destination = destination.f,
                                                 DF.point = nodes, id = code, x = nodes.X, y = nodes.Y)
                 
                        nodes$code <- as.character(nodes[, code])
                        nodes$nodes.X <- as.numeric(nodes[, nodes.X])
                        nodes$nodes.Y <- as.numeric(nodes[, nodes.Y])
                        
                        x = if (missing(crs)) NA_crs_ else st_crs(crs)
                        nodes <- sf::st_as_sf(x = nodes, coords = c(nodes.X, nodes.Y), crs = x)
                        
                        if(!is.null(add)){
                            plot(sf::st_geometry(nodes), col = "grey", lwd = 0.05,add = add)}
                              else if(missing(add) || add == FALSE){
                                      message("You can add spatial layer add = TRUE")
                                      plot(sf::st_geometry(nodes), col = "grey", lwd = 0.05)}
                        
                        }

  if (!is.null(bkg)){geo <- "area"
  
                     mgp_flow <- flowjointure(geom = geo, bkg, DF.flow = tab, 
                                              origin = origin.f, destination = destination.f,
                                              id = code, x = nodes.X, y = nodes.Y)

                     if(!is.null(add)){
                            plot(sf::st_geometry(mgp_flow$geometry.X), col = "grey", lwd = 0.05,add=add)}
                            else if(missing(add) || add == FALSE){
                                    message("You can add spatial layer with add = TRUE")
                                    plot(sf::st_geometry(mgp_flow$geometry.X), col = "grey", lwd = 0.05)}
                    
                     if (missing(plota)) {plota <- FALSE}
                        else {plot(sf::st_geometry(bkg), add = TRUE)}
                     }
  
  if (missing(filter)){filter <- FALSE}
  else {filter}

  if (filter == FALSE) {
                        arrows(mgp_flow$Xi, mgp_flow$Yi, mgp_flow$Xj, mgp_flow$Yj, code = 0, col = "black")
                        message("All theorical links are plotted")
                        }

  if (filter == TRUE) {
                      if (missing(threshold)) {
                         threshold <- 1
                         message("you use the default threshold= 1")}
                         else {threshold}

                      mgp_flow <- mgp_flow[mgp_flow$ydata >= threshold, ]

                      if (missing(taille)) {taille <- 1}
                         else {taille}
                         maxsize <- taille
                         mgp_flow$size <- (mgp_flow$ydata / max(mgp_flow$ydata)) * maxsize

                      if (missing(a.head)) {a.head <- 0}
                         else {a.head}

                      if (missing(a.length)) {a.length <- 0.1}
                         else {a.length}

                      if (missing(a.angle)) {a.angle <- 30}
                         else {a.angle}

                      if (missing(a.col)) {a.col <- "#000311"}
                         else {a.col}

                      arrows(mgp_flow$Xi, mgp_flow$Yi, mgp_flow$Xj, mgp_flow$Yj,
                             length = a.length, angle = a.angle, code = a.head, col = a.col,
                             lwd = mgp_flow$size)
                      }
}
