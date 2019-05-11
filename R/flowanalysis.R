#' @title Computation of a global concentration criterion of flows values or features
#' @description
#' \code{Computation of a global selection criterion for thresholding flow information and/or features before mapping.}\cr
#' To be use after \link{flowgini} and before \link{flowmap}.
#' @param tab flow dataset from \link{flowgini}
#' @param critflow level of flow significativity. See Details.
#' @param critlink level of features density. See Details.
#' @param result resulting filtering criterion value. See Details.
#' @details
#' -critflow =  desired level of flow's information significativity (e.g. 80%)
#'  of the total information ;\cr
#' -critlink = desired level of flow's features density (e.g. 20%) of the flow
#'        features that represents "more significant information.
#'
#' -result="density" returns the desired level of features density as
#'        a % of total features ;\cr
#' -result = "significativity" returns the level of flow significativity as a
#'        % of total of flow information ;
#' @examples
#' library(cartograflow)
#' data(flowdata)
#' bkg<- system.file("shape/MGP_TER.shp", package="cartograflow",
#'                   lib.loc = NULL, mustWork = TRUE)
#'
#' #1/4: Computes Gini's coefficent
#' tab_gini<-flowgini(flows,format="L",origin="i",dest="j",valflow="Fij",
#'           bkg,code="EPT_NUM",lorenz.plot = FALSE)
#' ### [1] Gini's coefficent = 73.16 %
#' \donttest{
#' #2/4: Plot Lorenz curve
#' flowgini(tab_gini,format="L",origin="i",dest="j",valflow="ydata",
#'           bkg,code="EPT_NUM",lorenz.plot = TRUE)
#' }
#' #3/4: Compute critflow filtering parameter
#' #critflow = 0.8 #selected criterion
#' flowanalysis(tab_gini,critflow = 0.8,result = "signif")
#' ### [1] "threshold =  11238  ---  flows =  80 % ---  links =  22.94 %"
#'
#' #4/4: Plot the flowmap
#' \donttest{
#' flowmap(flows,format="L",bkg,code = "EPT_NUM",filter = TRUE,
#'          threshold = 11238,taille = 8,a.head = 1,a.length = 0.11,
#'          a.angle = 30,a.col="#3f4247")}
#'
#' @references
#' Bahoken Françoise, 2016,« La cartographie d’une sélection globale de flux, entre ‘significativité’ et ‘densité’ »,
#' Netcom Online, 30-3/4 | 2016, Online since 23 March 2017, connection on 05 May 2019. URL : http://journals.openedition.org/netcom/2565 ;
#' DOI : 10.4000/netcom.2565
#' @export

flowanalysis<-function(tab,critflow,critlink,result){

   if (result=="signif"){
        if (missing(critflow)){stop("you must enter a value for the critflow when you signif ",call. = FALSE)}
        flow.select<-tab[tab$flowcum<critflow,]
        x<-tail(flow.select,1)
        flow.seuil<-x[3]
        flow.critlink<-x[10]
        gini.signif <-paste("threshold = ",round(flow.seuil,0)," --- ",
                            "flows = ",round(critflow*100,2),"% --- ",
                            "links = ",round(flow.critlink*100,2),"%")
    return(gini.signif)
  }

    if (result=="density"){
        if (missing(critlink)){stop("you must enter a value for the critlink when you used desnity",call. = FALSE)}
        link.select<-tab[tab$linkcum<critlink,]
        y<-tail(link.select,1)
        link.seuil<-y[3]
        link.critflow<-y[9]
        gini.density <-paste("threshold = ",round(link.seuil,0)," --- ",
                             "flows = ",round(link.critflow*100,2),"% --- ",
                             "links = ",round(critlink*100,2),"%")
    return(gini.density)
  }
}
