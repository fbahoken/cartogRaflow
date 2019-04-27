#' @title Computation of a global selection criterion for thresholding flow values and/or flow features
#' @description
#' To be use after \link{flowgini}.
#' Computation of a global selection criterion for thresholding flow information and/or features before mapping.
#' @param tab flow dataset from \link{flowgini}
#' @param critflow level of flow significativity. See Details.
#' @param critlink level of features density. See Details.
#' @param result resulting filtering criterion value. See Details
#' @details
#' -- critflow =  desired level of flow's information significativity (e.g. 80% of the total information) ;
#' -- critlink = desired level of flow's features density (e.g. 20% of the flow features that represents "more significant information).
#'
#' -- result="density" returns the desired level of features density as a % of total features ;
#' -- result = "significativity" returns the level of flow significativity as a % of total of flow information ;
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
