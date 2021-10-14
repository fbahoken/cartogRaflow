---
title: "Cartograflow"
subtitle: "Filtering Matrix with concentration criterion for Thematic Flow Mapping"
author: "Cartograflow, 2020"
date: "`r Sys.Date()`"
output: html_vignette
vignette: >
  \usepackage[ps2pdf,
              bookmarks=true,
              dvipsone=pdftex,                                
              backref,
              ]{hyperref}
  %\VignetteIndexEntry{Cartograflow}
  %\VignetteEncoding{UTF-8}
  %\SweaveUTF8
  %\VignetteEngine{knitr::rmarkdown}
---


This vignette of `Cartograflow` is dedicated to the concentration analysis of an origin-destination matrix for flow mapping purposes.


## Main functions

**flowgini** 

-- `flowgini()` performs a Gini's concentration analysis of the flow features, by computing _Gini coefficient_ and plotting interactive _Lorenz curve_.

To be use before `flowanalysis()`


**flowanalysis**
Flow filtering according to a concentration criterion

-- `flowanalysis()` computes filters criterions based on:

- argument _critflow_ is to filter the flows according to their significativity (% of total of flow information) ; \cr 
- argument _critlink_ is to filter the flows according to their density (% of total features)\cr

These arguments can be used as filter criterion in `flowmap()`.


## Useful additional function for a complete analysis

**Flowmap**

-- `flowmap()` is to plot flows as segments or arrows, by acting on the following arguments:\cr 

- `filter` is to filter or not flow's information or features \cr
- `threshold` is used to set the filtering level of the flows when filter="True" \cr
- `taille` is the value of the width of the flow feature \cr
- `a.head` is the arrow head parameter (in, out, in and out) \cr
- `a.length` is the length of the edges of the arrow head (in inches) \cr
- `a.angle` is the angle from the shaft of the arrow to the edge of the arrow head \cr
- `a.col` is the arrow's color \cr
- `plota` is to add spatial features as map background to the flows's plot \cr
- `add` is to allow to overlay flow features on external spatial features background \cr


## Example


```{r include=FALSE, message=FALSE}
rm(list=ls())
library(sf)
library(dplyr)
library(cartograflow)
library(cartography)
```

**1. Load datasets**
--------------------

**Flow dataset**

```{r flowdata_preprocess, warning=FALSE, echo=TRUE}
# Load Statistical information
data<-read.csv2("./data/MOBPRO_ETP.csv",
                header=TRUE,
                sep=";",
                stringsAsFactors=FALSE,
                encoding="UTF-8",
                dec=".",
                check.names=FALSE)
str(data)
# Variable typing
data$i<-as.character(data$i)
data$j<-as.character(data$j)
data$Fij<-as.numeric(data$Fij)
data$count<-as.numeric(data$count)
# Selecting useful variables
tabflow<-data%>%select(i,j,Fij)
```

**Geographical dataset**
```{r data_preprocess, warning=FALSE, echo=TRUE}
# Load a list of geo codes
ID_CODE<-read.csv2("./data/COD_GEO_EPT.csv",
                   header=TRUE,
                   sep=";",
                   stringsAsFactors=FALSE,
                   encoding="UTF-8",
                   dec=".",
                   check.names=FALSE)
#head(ID_CODE)
CODE<-ID_CODE%>% dplyr::select(COD_GEO_EPT)
colnames(CODE)<-c("CODGEO")
#head(CODE)
map<-st_read("./data/MGP_TER.shp")
```


**2. Compute concentration criterion**
---------------------------------------

**2.1. Compute the concentration of flow values**


```{r analysis, fig.show='hold',message = TRUE , warning=FALSE}
# Computes Gini's coefficent
# head(tabflow,3)
tabgini<-flowgini(ODpts = tabflow,
                  origin="i",destination = "j",valflow = "Fij",
                  lorenz.plot = FALSE)
# Interpretation ; The flows are quite concentrated on a few links, the Gini coefficent is equal to 73.16% 
# Plot an interactive Lorenz curve
head(tabgini)
flowgini(ODpts = tabflow,
         origin="i",destination = "j",valflow = "Fij",
         lorenz.plot = TRUE)
```

**2.2. Compute the "critflow" parameter (ex. significance)**

```{r echo=TRUE, fig.show='hold', message=TRUE, warning=FALSE}
# Compute critflow parameter
flowanalysis(tabgini,
             critflow = 0.8,
             result = "signif")
# Interpretation : Flow values up to 13442 are the 80% largest one corresponding to 23,14 % of the total links' features.
```


**2.3. Flowmap filtered according to flows values significance**
Using the 'critflow" to filter flows

```{r echo=TRUE, fig.show='hold', message=TRUE, warning=FALSE}
# Graphic parameters
knitr::opts_chunk$set(fig.width=6, fig.height=6)
par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-150
# Overlay a spatial background 
par(bg = "NA")
#plot(st_geometry(map), col = "#67786c")
# Add the corresponding background 
plot(st_geometry(map), col = NA, border=NA, bg="#dfe6e1")
plot(st_geometry(map), col = "light grey", add=TRUE)
# For mapping flow up to 11238
flowmap(tab=tabflow,
        fij="Fij",origin.f = "i",destination.f = "j",
        bkg = map,code="EPT_NUM",nodes.X="X",nodes.Y = "Y",
        add=TRUE,
        filter=TRUE,
        threshold=13442,    
        taille=15,           
        a.head = 1,
        a.length = 0.11,
        a.angle = 30,
        a.col="#138913")
# Map Legend
legendPropLines(pos="topleft",
                title.txt="Commuters up to 13442\n (80% of the largest flows)",
                title.cex=0.8,   
                cex=0.5,
                values.cex= 0.7,  
                var=c(13442,max(tabflow$Fij)), 
                lwd=15, 
                frame = FALSE,
                col="#138913",
                values.rnd = 0
                )
#Map cosmetic
layoutLayer(title = "Significant professional mobility in Greater Paris",
           coltitle ="black",
           author = "Cartograflow, 2020",
           sources = "Data : INSEE, 2017 ; Basemap : APUR, RIATE, 2018.",
           scale = 2,
           tabtitle = FALSE,
           frame = TRUE,
           col = "grey",
            )
# north arrow
north("topright")
```


-- **2.4. Flowmap filtered according to flow features' density**
Using the 'critlink" to filter flows

```{r echo=TRUE, fig.show='hold', warning=FALSE}
flowanalysis(tabgini,
             critlink = 0.1,
             result = "density")
# Interpretation : Flows up to 45772 are the 58.12% largest one corresponding to 10 % of the total links
# Graphic parameters
knitr::opts_chunk$set(fig.width=6, fig.height=6)
par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-150
# Overlay a spatial background 
par(bg = "NA")
# Add the corresponding background 
plot(st_geometry(map), col = NA, border=NA, bg="#dfe6e1")
plot(st_geometry(map), col = "light grey", add=TRUE)
# For mapping 10% of the total features as flows up to 45772 commuters
flowmap(tab=tabgini,
        fij="Fij",origin.f = "i",destination.f = "j",
        bkg = map,code="EPT_NUM",nodes.X="X",nodes.Y = "Y",
        add=TRUE,
        plota = TRUE,
        filter=TRUE,
        threshold=45772,    
        taille=15,           
        a.head = 1,
        a.length = 0.15,
        a.angle = 30,
        a.col="#138913")
# Map Legend
legendPropLines(pos="topleft",
                title.txt="Number of commuters up to 45772\n(10% of the links)",
                title.cex=0.8,   
                cex=0.5,
                values.cex= 0.7,  
                var=c(45772,max(tabgini$Fij)), 
                lwd=15, 
                frame = FALSE,
                col="#138913",
                values.rnd = 0
                )
#Map cosmetic
layoutLayer(title = "Low density professional mobility in Greater Paris",
           coltitle ="black",
           author = "Cartograflow, 2020",
           sources = "Data : INSEE, 2017 ; Basemap : APUR, RIATE, 2018.",
           scale = 2,
           tabtitle = FALSE,
           frame = TRUE,
           col = "grey"
            )
# north arrow
north("topright")
```


## Sample datasets

-- _Statistical dataset_ : 
- INSEE - Base flux de mobilité (2015)
-URL : https://www.insee.fr/fr/statistiques/fichier/3566008/rp2015_mobpro_txt.zip

-- _Geographical dataset_ :

- municipalities : IGN, GEOFLA 2015 v2.1 
- Greater Paris : APUR, UMS 2414 RIATE, 2018.

## See also

-- cartograflow_general.html <br/>
-- cartograflow_distance.html <br/>
-- cartograflow_ordinal_distance.hmtl <br/>
-- cartograflow_concentration.hmtl <br/>

## Reference (for early version)

-- Bahoken Francoise (2016), Programmes pour R/Rtudio annexés, in :  _Contribution à la cartographie d'une matrix de flux_, Thèse de doctorat, Université Paris 7, pp. 325-346. URL  : https://halshs.archives-ouvertes.fr/tel-01273776, pp. 480-520.

## Reproducibility

```{r lecho=TRUE, fig.show='hold'}
sessionInfo()
```
