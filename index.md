# General presentation of Cartograflow
`Cartograflow` is designed to filter origin-destination (OD) flow matrix for thematic mapping purposes.

## Description of functions

**1. Preparing flow data sets:** <br/> 

**1.1 General functions**<br/>
You can use long "L" or matrix "M" [n*n] flow dataset formats. 

-- `flowtabmat()` is to transform "L" to "M" formats, also to build an empty square matrix from spatial codes.

-- `flowcarre()` is to square a matrix.

-- `flowjointure()` is to performs a spatial join between a flow dataset and a spatial features layer or an external matrix.

-- `flowstructmat()` fixes an unpreviously codes shift in the flow dataset "M" format. If necessary this function is to be used with `flowjointure` and `flowtabmat`.

**1.2. Flow computation:**

-- `flowtype()` is to compute several types of flow from an asymmetric matrix: <br/> 
x= `flux` for remaining initial flow (Fij) <br/>
x= `transpose` for reverse flow value (Fji) <br/>
x= `bivolum` for bilateral volum, as gross flow (FSij)<br/>
x= `bibal` for bilateral balance, as net flow (FBij) <br/>
x= `biasym` for asymetry of bilateral flow (FAij) <br/>
x= `bimin` for minimum of bilateral flow (minFij) <br/>
x= `bimax` for maximum of bilateral flow (maxFij) <br/>
x= `birange` for bilateral flow range (rangeFij) <br/>
x= `bidisym` for bilateral disymetry as (FDij) <br/>

-- `flowplaces()` is to compute several types of flow places oriented from an asymmetric:<br/>
ie. as a dataframe that describes the flows from Origin / destination point of view <br/> 
x= `ini` for the number of incoming links (as in-degree) <br/>
x= `outi` for the number of outcoming links (as out-degree) <br/>
x= `degi` for the total number of links (as in and out degrees) <br/>
x= `intra` for total intra zonal interaction (if main diagonal is not empty <br/>
x= `Dj` for the total flows received by (j) place <br/>
x= `voli` for the total volume of flow per place <br/>
x= `bali` for the net balance of flow per place <br/>
x= `asyi` for the asymetry of flow per place <br/>
x= `allflowplaces` for computing all the above indicators <br/>


**1.3. Flow reduction:**

-- `flowlowup()` is to extracts the upper or the lower triangular part of a matrix - preferably for symmetrical matrixes.

x= `up` for the part above the main diagonal <br/>
x= `low` for the part below the main diagonal <br/>

-- `flowreduct()` is to reduce the flow dataset regarding another matrix, e.g. distances travelled. <br/> 

`metric` is the metric of the distance matrix :<br/>
- metric= `continuous` (e.g. for kilometers) <br/>
- metric= `ordinal` (e.g. for `k` contiguity) <br/>

If the metric is `continuous` (e.g for filtering flows by kilometric distances travelled), use:<br/>

`d.criteria` is for selecting the minimum or the maximum distance criteria <br/>
- d.criteria= `dmin` for keeping only flows up to a _dmin_ criterion in km <br/>
- d.criteria= `dmax` for selecting values less than a _dmax_ criterion in km <br/>

`d` is the value of the selected `dmin` or `dmax` criteria.

Notice that these arguments can be used as a filter criterion in `flowmap()`.

See Cartograflow_distance and Cartograflow_ordinal_distance Vignettes for examples.<br/>
URL: https://github.com/fbahoken/cartogRaflow/tree/master/vignettes

**2. Flows filtering:** <br/> 

**2.1. Filtering from flow concentration analysis** 

**Flow concentration analysis:** 

-- `flowgini()` performs a Gini's concentration analysis of the flow features, by computing _Gini coefficient_ and plotting interactive _Lorenz curve_.

To be use before `flowanalysis()`

See Cartograflow_concentration Vignette for example.<br/>
URL: https://github.com/fbahoken/cartogRaflow/tree/master/vignettes

**Flow filtering according to a concentration criterion:**

-- `flowanalysis()` computes filters criterions based on:<br/>

- argument `critflow` is to filter the flows according to their significativity (% of total of flow information) ; <br/>
- argument `critlink` is to filter the flows according to their density (% of total features) <br/>

These arguments can be used as `filter` criterion in `flowmap()`.

See Cartograflow_concentration Vignette for example.<br/>
URL: https://github.com/fbahoken/cartogRaflow/tree/master/vignettes

**2.2. Spatial / territorial filtering of flows** 

**Flow filtering based on a continuous distance criterion**

-- `flowdist()` computes a _continous distance_ matrix from spatial features (area or points). The result is a matrix of the distances travelled between ODs, with flows filtered or not.

See Cartograflow_distance Vignette for example.<br/>
URL: https://github.com/fbahoken/cartogRaflow/tree/master/vignettes

**Flow filtering based on an ordinal distance / neighbourhood criterion**:

-- `flowcontig()` compute an _ordinal distance_ matrix from spatial features (area). The result is a matrix of adjacency or k-contiguity of the ODs.

- `background` is the areal spatial features ;<br/>
- code` is the spatial features codes ; <br/>
- `k` is to enter the number (k:1,2,...,k) of the contiguity matrix to be constructed : if (k=1), ODs places are adjacent, then the flow have to cross only 1 boundary, else (k=k) ODs places are distant from n borders ;<br/>
- `algo` is the algorithm to use for ordinal distance calculation (also Default is "automatic" for "Dijkstra's") ; <br/>
Notice that the function automatically returns the maximum (k) number of the spatial layer.
See Cartograflow_distance_ordinal Vignette for example.
**3. Flow mapping**
-- `flowmap()` is to plot flows as segments or arrows, by acting on the following arguments:<br/> 
- `filter` is to filter or not flow's information or features <br/>
- `threshold` is used to set the filtering level of the flows when filter="True" <br/>
- `taille` is the value of the width of the flow feature <br/>
- `a.head` is the arrow head parameter (in, out, in and out) <br/>
- `a.length` is the length of the edges of the arrow head (in inches) <br/>
- `a.angle` is the angle from the shaft of the arrow to the edge of the arrow head <br/>
- `a.col` is the arrow's color <br/>
- `plota` is to add spatial features as map background to the flows's plot <br/>
- `add` is to allow to overlay flow features on external spatial features background <br/>
## Examples of applications
-- **Useful packages**
Best external R package to use: 
{dplyr} {sf} {igraph} {rlang} {cartography}
```{r include=FALSE, message=FALSE}
rm(list=ls())
library(sf)
library(dplyr)
library(cartograflow)
library(cartography)
knitr::opts_chunk$set(fig.width=6, fig.height=6)
```


**1. Load datasets**
--------------------

**Flow dataset**

```{r flowdata_preprocess, warning=FALSE, echo=TRUE}
# Load Statistical information
tabflow<-read.csv2("./data/MOBPRO_ETP.csv", header=TRUE, sep=";",stringsAsFactors=FALSE,
                   encoding="UTF-8", dec=".",check.names=FALSE)
```


```{r var_typing, echo=FALSE, warning=FALSE}
# Variable typing
tabflow$i<-as.character(tabflow$i)
tabflow$j<-as.character(tabflow$j)
tabflow$Fij<-as.numeric(tabflow$Fij)
tabflow$count<-as.numeric(tabflow$count)
str(tabflow)
```

**Select variable and change matrix format**
```{r flowdata_reverse, echo=TRUE, message=FALSE, warning=FALSE}
# Selecting useful variables for changing format
tabflow<-tabflow %>% select(i,j,Fij)
# From list (L) to matrix (M) format
matflow <-flowtabmat(tabflow,matlist="M")
head(matflow[1:4,1:4])
dim(matflow)
```

```{r flowdata_reverseM, message=FALSE, warning=FALSE, include=FALSE}
# From matrix (M) to list (L) format
tabflow<-flowtabmat(tab=matflow,
                    matlist="L")
colnames(tabflow)<-c("i","j","Fij")
head(tabflow)
```


**Geographical dataset**
```{r data_preprocess, message=FALSE, warning=FALSE, include=FALSE}
# Load a list of geo codes
ID_CODE<-read.csv2("./data/COD_GEO_EPT.csv",
                   header=TRUE,sep=";",stringsAsFactors=FALSE,encoding="UTF-8", dec=".",   check.names=FALSE)
#head(ID_CODE)
CODE<-ID_CODE%>% dplyr::select(COD_GEO_EPT)
colnames(CODE)<-c("CODGEO")
#head(CODE)
```


**2. Flow types computing**
--------------------


```{r vara_typing2, message=FALSE, warning=FALSE, include=FALSE}
# Variable typing
tabflow$i<-as.character(tabflow$i)
tabflow$j<-as.character(tabflow$j)
tabflow$Fij<-as.numeric(tabflow$Fij)
as.data.frame(tabflow)
```

*Compute bilateral flows types : eg. volum, balance, bilateral maximum and all types*

```{r data_computing, echo=TRUE, message=FALSE, warning=FALSE}
# Bilateral volum (gross) FSij:  
tabflow_vol<-flowtype(tabflow, format="L", origin="i", destination="j", fij="Fij",  "bivolum")
# Matrix format (M= : matflow_vol<-flowtype(matflow, format="M", "bivolum")
# Bilateral balance (net ) FBij:  
tabflow_net<-flowtype(tabflow, format="L", origin="i", destination="j", fij="Fij", "bibal")
# Bilateral maximum (maxFij): 
tabflow_max<-flowtype(tabflow, format="L", origin="i", destination="j", fij="Fij", "bimax")
# Compute all types of bilateral flows, in one 11 columns
tabflow_all<-flowtype(tabflow,format="L", origin="i", destination="j", fij="Fij", x="alltypes")
head(tabflow_all) 
```


**3. Direct flow mapping**
---------------------------

**3.1. Plot all origin-destination without any filtering criterion** 
The result will reveal a graphic complexity ("spaghetti-effect"")

Plot links
```{r maps_links, echo=TRUE, fig.show='hold', fig.width=6, message=FALSE, warning=FALSE, ECHO=FALSE}
library(sf)
map<-st_read("./data/MGP_TER.shp")
# Add and overlay spatial background 
par(bg = "NA")
# Graphic parameters
par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-150
plot(st_geometry(map), col = NA, border=NA, bg="#dfe6e1")
plot(st_geometry(map), col = "light grey", add=TRUE)
# Flowmapping of all links
flowmap(tab=tabflow,
        fij="Fij",
        origin.f = "i",
        destination.f = "j",
        bkg = map,
        code="EPT_NUM",
        nodes.X="X",
        nodes.Y = "Y",
        filter=FALSE,
        add=TRUE
        )
library(cartography)
# Map cosmetics
layoutLayer(title = "All origin-destination for commuting in Greater Paris, 2017",
           coltitle ="black",
           author = "Cartograflow, 2020",
           sources = "Data : INSEE, 2017 ; Basemap : APUR, RIATE, 2018.",
           scale = 2,
           tabtitle = FALSE,
           frame = TRUE,
           col = "grey"
            )
# North arrow
north("topright")
```


**3.2. Plot the above-average flows** 

```{r maps_flowmean, echo=TRUE, fig.show='hold', fig.width=6, message=FALSE, warning=FALSE, ECHO=FALSE}
library(sf)
map<-st_read("./data/MGP_TER.shp")
# Add and overlay spatial background 
par(bg = "NA")
# Graphic parameters
par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-150
plot(st_geometry(map), col = NA, border=NA, bg="#dfe6e1")
plot(st_geometry(map), col = "light grey", add=TRUE)
# Flow mapping above-average flows
flowmap(tab=tabflow,
        fij="Fij",
        origin.f = "i",
        destination.f = "j",
        bkg = map,
        code="EPT_NUM",
        nodes.X="X",
        nodes.Y = "Y",
        filter=TRUE,
        threshold =(mean(tabflow$Fij)),  #mean value is the level of threshold
        taille=20,           
        a.head = 1,
        a.length = 0.11,
        a.angle = 30,
        a.col="#138913",
        add=TRUE)
# Map Legend
legendPropLines(pos="topleft",
                title.txt="Commuters > 13220 ",
                title.cex=0.8,   
                cex=0.5,
                values.cex= 0.7,  
                var=c(mean(tabflow$Fij),max(tabflow$Fij)), 
                lwd=5, 
                frame = FALSE,
                col="#138913",
                values.rnd = 0
                )
#Map cosmetic
layoutLayer(title = "Commuters up to above-average in Greater Paris",
           coltitle ="black",
           author = "Cartograflow, 2020",
           sources = "Data : INSEE, 2017 ; Basemap : APUR, RIATE, 2018.",
           scale = 2,
           tabtitle = FALSE,
           frame = TRUE,
           col = "grey"
            )
# North arrow
north("topright")
```

**3.3. Plot the net flows of bilateral flows** 

```{r maps_flownet, echo=TRUE, fig.show='hold', fig.width=6, message=FALSE, warning=FALSE, ECHO=FALSE}
#library(sf)
map<-st_read("./data/MGP_TER.shp")
# Net matrix reduction
tabflow_net <- tabflow_net %>% filter(.data$FBij>=0)
# Net matrix thresholding
Q80<-quantile(tabflow_net$FBij,0.95)
# Add and overlay spatial background 
par(bg = "NA")
# Graphic parameters
par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-150
plot(st_geometry(map), col = NA, border=NA, bg="#dfe6e1")
plot(st_geometry(map), col = "light grey", add=TRUE)
# Flow mapping above-average flows
flowmap(tab=tabflow_net,
        fij="FBij",
        origin.f = "i",
        destination.f = "j",
        bkg = map,
        code="EPT_NUM",
        nodes.X="X",
        nodes.Y = "Y",
        filter=TRUE,
        threshold = Q80,
        taille=12,           
        a.head = 1, 
        a.length = 0.11,
        a.angle = 30,
        a.col="#4e8ef5",
        add=TRUE)
# Map Legend
legendPropLines(pos="topleft",
                title.txt="Commuters > 5722 ",
                title.cex=0.8,   
                cex=0.5,
                values.cex= 0.7,  
                var=c(Q80,max(tabflow_net$FBij)), 
                lwd=12, 
                frame = FALSE,
                col="#4e8ef5",
                values.rnd = 0
                )
#Map cosmetic
layoutLayer(title = "Net commuters in Greater Paris (20% strongest)",
           coltitle ="black",
           author = "Cartograflow, 2020",
           sources = "Data : INSEE, 2017 ; Basemap : APUR, RIATE, 2018.",
           scale = 2,
           tabtitle = FALSE,
           frame = TRUE,
           col = "grey"
            )
# North arrow
north("topright")
```

## Sample datasets

-- _Statistical dataset_ : 
- INSEE - Base flux de mobilité (2015)
- URL : https://www.insee.fr/fr/statistiques/fichier/3566008/rp2015_mobpro_txt.zip

-- _Geographical dataset_ :

- municipalities : IGN, GEOFLA 2015 v2.1 
- Greater Paris : APUR, UMS 2414 RIATE, 2018.

## See also

https://github.com/fbahoken/cartogRaflow/tree/master/vignettes

-- cartograflow_general.html <br/>
-- cartograflow_concentration.html <br/>
-- cartograflow_distance.html <br/>
-- cartograflow_ordinal_distance.hmtl <br/>

## Reference

-- Bahoken Francoise (2016), Programmes pour R/Rtudio annexés, in :  _Contribution à la cartographie d'une matrix de flux_, Thèse de doctorat, Université Paris 7, pp. 325-346. URL  : https://halshs.archives-ouvertes.fr/tel-01273776, pp. 480-520.
