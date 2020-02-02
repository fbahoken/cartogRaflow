#' @title Computation of a global concentration criterion of flows information or graphic density
#' @description
#' \code{Computation of a global selection criterion for filtering flow, based on flow significativity or features density.}\cr
#' To be use after \link{flowgini} and before \link{flowmap}.
#' @param tab is the input flow dataset from \link{flowgini}
#' @param fij in the name of the flow value variable
#' @param critflow desired level of flow information significativity. See Details.
#' @param critlink desired level of flow features density. See Details.
#' @param result resulting filtering criterion value. See Details.
#' @details
#' -critflow =  desired level of significativity of flow information - e.g. 80%
#'  of total information ;\cr
#' -critlink = desired level of density of flow features - e.g. 20% of the
#'  strongest links.
#'
#' -result="density" corresponds to the % density of figures - also provides
#'  the corresponding % significance of flow information and the value of the
#'  flow filter criterion to be used for the flowmapping ;\cr
#'  - result="significativity" corresponds to the % significance of the flow
#'  information - also provides the corresponding % density of features symbolizing
#'  the flows and the value of the flow filtering criterion to be used for the flowmapping
#' 
#' @examples
#' library(cartograflow)
#' data(flowdata)
#' 
#' #1/4: Computes Gini's coefficent with {flowgini}
#' tabgini<-flowgini(ODpts = flows,origin="i",destination = "j",valflow = "Fij",lorenz.plot = FALSE)
#' ### [1] Gini's coefficent = 73.16 %
#' \donttest{
#' #2/4: Plot Lorenz curve with {flowgini}
#' flowgini(tab_gini,format="L",origin="i",dest="j",valflow="ydata",
#'           bkg,code="EPT_NUM",lorenz.plot = TRUE)
#' }
#' #3/4: Compute critflow filtering parameter
#' #critflow = 0.8 #selected criterion
#' flowanalysis(tabgini,critflow = 0.8,result = "signif")
#' ### [1] "threshold =  11238  ---  flows =  80 % ---  links =  22.94 %"
#'
#' #4/4: Plot the flowmap
#' \donttest{
#'    flowmap(tab=tabflow,fij="Fij",origin.f = "i",destination.f = "j",
#'            bkg = map,code="EPT_NUM",nodes.X="X",nodes.Y = "Y",
#'            filter=TRUE,
#'            threshold=11238,    
#'            taille=8,           
#'            a.head = 1,
#'            a.length = 0.11,
#'            a.angle = 30,
#'            a.col="#3f4247")}     
#'  
#' @references
#' Bahoken Françoise, 2016,« La cartographie d’une sélection globale de flux, entre ‘significativité’ et ‘densité’ »,
#' Netcom Online, 30-3/4 | 2016, Online since 23 March 2017, connection on 05 May 2019. URL : http://journals.openedition.org/netcom/2565 ;
#' DOI : 10.4000/netcom.2565
#' @export

flowanalysis<-function(tab,fij=NULL,critflow,critlink,result){

   if (result=="signif"){
        if (missing(critflow)){stop("you must enter a value for the critflow when you signif ",call. = FALSE)}
        flow.select<-tab[tab$flowcum<critflow,]
        x<-tail(flow.select,1)
        if(!is.null(fij)){flow.seuil<-x[,fij]}
        else flow.seuil<-x[3]
        flow.critlink<-x$linkcum
        gini.signif <-paste("threshold = ",round(flow.seuil,0)," --- ",
                            "flows = ",round(critflow*100,2),"% --- ",
                            "links = ",round(flow.critlink*100,2),"%")
    return(gini.signif)
  }

    if (result=="density"){
        if (missing(critlink)){stop("you must enter a value for the critlink when you used density",call. = FALSE)}
        link.select<-tab[tab$linkcum<critlink,]
        y<-tail(link.select,1)
        if(!is.null(fij)){link.seuil<-y[,fij]}
        else link.seuil<-y[3]
        link.critflow<-y$flowcum
        gini.density <-paste("threshold = ",round(link.seuil,0)," --- ",
                             "flows = ",round(link.critflow*100,2),"% --- ",
                             "links = ",round(critlink*100,2),"%")
    return(gini.density)
  }
}
