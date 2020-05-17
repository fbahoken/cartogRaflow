#' @title Analysis of flow concentration (Gini coefficient)
#' @description
#' Calculates Gini coefficient, plot Lorenz curve and
#' threshold the matrice according to a global concentration criterion
#' for mapping flow intensity or flow density.\cr
#' To be use before \link{flowanalysis}
#' @param ODpts the input  dataset with : nodes code, flow values and XY coordinates
#' @param origin ID origin place, in long format
#' @param destination ID destination place, long format
#' @param valflow flow value between origin and destination places
#' @param lorenz.plot to plot or the Lorenz curve. See Details
#' @return plot Lorenz curve for the cumulated flow and links : flowgini(...,gini.plot = TRUE),warning : the function must be not assign a variable
#' @return value of the Gini's coefficent and the table : table<-flowgini(...,missing(gini.plot) or gini.plot = FALSE )
#' @details
#' flowgini(...,lorenz.plot = TRUE) for ploting Lorenz curve  associate to the gini coefficient, from cumulated flows and links.
#' @rawNamespace import(plotly, except = last_plot)
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_text
#' @references
#' Bahoken Françoise, 2016,« La cartographie d’une sélection globale de flux, entre ‘significativité’ et ‘densité’ »,
#' Netcom Online, 30-3/4 | 2016, Online since 23 March 2017, connection on 05 May 2019. URL : http://journals.openedition.org/netcom/2565 ;
#' DOI : 10.4000/netcom.2565. \cr
#' Grasland Claude, 2014, "Flows analysis carto", unpublished R functions.
#' @import dplyr
#' @import sp
#' @importFrom rlang .data
#' @importFrom utils tail
#' @examples
#' library(cartograflow)
#' data(flowdata)
#' # Computes Gini's coefficent
#' tabgini <- flowgini(ODpts = flows, origin = "i", destination = "j", 
#'                      valflow = "Fij", lorenz.plot = FALSE)
#' # Plot Lorenz curve
#' flowgini(ODpts = flows, origin = "i", dest = "j", valflow = "Fij", lorenz.plot = TRUE)
#' # See \link{flowanalysis} for viewing the tab_gini table
#' @export

flowgini <- function(ODpts, origin, destination, valflow, lorenz.plot) {
  gini <- function(vec1, vec2) {
    tot <- vec2[1] / 2 * vec1[1]
    i <- 2
    while (i <= length(vec1)) {
      tot <- tot + (vec1[i] - vec1[i - 1]) * (vec2[i] + vec2[i - 1]) / 2
      i <- i + 1
    }
    res <- 2 * (0.5 - tot)
    return(res)
  }

  ginigraph <- function(x, y) {
    p <- ggplot(x) +
      geom_line(aes(x = x$flowcum, y = x$linkcum)) +
      geom_line(aes(x = x$flowcum, y = x$flowcum)) +
      xlab("Cumulative links") + ylab("Cumulative flows") +
      ggtitle(paste("Gini's coefficent =", round(y * 100, 2), " %")) +
      theme(
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(colour = "#68382C", size = 9)
      )
    ggplotly(p) %>% layout(dragmode = "select")
  }

  gini.tab <- function(g.tab) {
    gini.tab <- g.tab
    gini.tab$link <- 1
    gini.tab <- gini.tab[gini.tab[, valflow] > 0, ]
    gini.tab <- gini.tab[order(gini.tab[, valflow], decreasing = TRUE), ]
    gini.tab$flowcum <- cumsum(gini.tab[, valflow]) / sum(gini.tab[, valflow])
    gini.tab$linkcum <- cumsum(gini.tab$link) / sum(gini.tab$link)
    return(gini.tab)
  }

  tabgini <- gini.tab(ODpts)

  indice <- gini(tabgini$flowcum, tabgini$linkcum)

  if (missing(lorenz.plot) || lorenz.plot == FALSE) {
    message("Gini's coefficent =", paste(round(indice * 100, 2), "%"), "\n")
    return(tabgini)
  }
  if (lorenz.plot == TRUE) {
    ginigraph(tabgini, indice)
  }
}
