#' @title Builds a spatial join with a flow dataset
#' @description Builds a spatial join between a flow dataset and a
#' spatial features layer (as a map background)
#' @param DF.flow the input flow dataset as a dataframe
#' @param origin the place of origin code
#' @param destination the place of destination code
#' @param DF.point a dataframe of points or places
#' @param id dataframe of points or places file code
#' @param x the X coordinate of the point or places 
#' @param y the Y coordinate of the point or places 
#' @param geom the geometry of the spatial features layer: points or areas
#' @param bkg the spatial features layer
#' @return the corresponding joint table between the flow dataset and the
#' spatial feature layer
#' @export
#' @importFrom maptools readShapeSpatial
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @import sp
#' @examples
#' library(cartograflow)
#' library(sf)
#' data(flowdata)
#' map <- st_read(system.file("shape/MGP_TER.shp", package = "cartograflow"))
#' tabflow <- flowjointure(
#'   geom = "area", bkg = map, DF.flow = flows, origin = "i", destination = "j",
#'   id = "EPT_NUM", x = "X", y = "Y"
#' )
#' @export


flowjointure <- function(geom, bkg, DF.flow, origin, destination,
                         DF.point, id, x, y) {
  if (geom == "pt") {
    if (missing(bkg)) {
      bkg <- NULL
    }
    if (missing(id) && missing(x) && missing(y)) {
      message("It's necessary to put the variable id,x,y")
    }

    df.point <- DF.point %>% select(id, x, y)

    dff.jointure <- DF.flow %>%
                    left_join(df.point, by = setNames(id, nm = origin)) %>%
                    rename(Xi = x, Yi = y) %>%
                    left_join(df.point, by = setNames(id, nm = destination)) %>%
                    rename(Xj = x, Yj = y)
    colnames(dff.jointure) <- c("i", "j", "ydata", "Xi", "Yi", "Xj", "Yj")
    return(dff.jointure)
  }

  if (geom == "area") {
    if (missing(DF.point)) {
      DF.point <- NULL
    }
    if (missing(bkg) && missing(DF.flow)) {
      message("It's necessary to put the variable bkg and DF.flow")
    }
    if (missing(id) && missing(x) && missing(y)) {
      message("It's necessary to put the variable id,x,y")
    }

    pt_centroid <- bkg %>% sf::st_centroid()
    pts <- sf::st_coordinates(pt_centroid)

    p <- cbind(pts, pt_centroid)

    df.point <- p %>% select(id, x, y)

    dfp.jointure <- DF.flow %>%
                    left_join(df.point, by = setNames(id, nm = origin)) %>%
                    rename(Xi = x, Yi = y) %>%
                    left_join(df.point, by = setNames(id, nm = destination)) %>%
                    rename(Xj = x, Yj = y)
    colnames(dfp.jointure) <- c("i", "j", "ydata", "Xi", "Yi", "geometry.X", "Xj", "Yj", "geometry.Y")
    return(dfp.jointure)
  }
}
