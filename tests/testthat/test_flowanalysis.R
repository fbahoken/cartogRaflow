#' Computation of a global selection criterion for flowmapping
#'
#' Computation of a global selection criterion for thresholding flow  information or features before mapping
#' To be use after \link{flowgini}
#'
#' @param tab flow dataset from \link{flowgini}
#' @param critflow level of flow significativity. See Details.
#' @param critlink level of features density. See Details.
#' @param result resulting criterion value. See Details
#' @details
#' Computes a double criterion for selecting flows before mapping :
#' -- critflow =  desired level of flow's information significativity, as a % of total of flow information ;
#' -- critlink = desired level of flow's features density, as a % of total of flow features.
#'
#' Return resuts :
#' -- result="density" returns the desired level of features density as a % of total features ;
#' -- result = "significativity" returns the level of flow significativity as a % of total of flow information ;

#' @references
#' Bahoken Francoise (2016), « La cartographie d’une sélection globale de flux, entre ‘significativité’ et ‘densité’ »,
#' Netcom [On ligne], 30-3/4 | URL : http://journals.openedition.org/netcom/2565 ; DOI : 10.4000/netcom.2565
#' Bahoken Francoise (2016), « Chapitre 8 Propositions de solutions liées au choix d’un critère de
#' sélection global : la cartographie de flux significatifs (Fij>α)» in Bahoken, F. Contribution à la cartographie d'une matrice de flux, Thèse de doctorant, Université Paris 7, pp. 325-346.
#' Grasland Claude (2011, 2014), « Flows analysis carto », unpublished R functions.
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
