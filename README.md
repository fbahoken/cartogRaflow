# Cartograflow <img src="doc/Logo_cartograflow.png" align="right" alt="" width="120" />
**Filtering origin-destination Matrices for Thematic Flowmapping**

[![](https://www.r-pkg.org/badges/version/cartograflow)](https://cran.r-project.org/package=cartograflow)
[![](https://cranlogs.r-pkg.org/badges/cartograflow?color=brightgreen)](https://cran.r-project.org/package=cartograflow)

This package is designed to filter origin-destination matrices for flow mapping purposes. It is based on different functions that are mainly used to prepare the flow dataset.

# Installation

Installing cartograflow package from CRAN as follows:<br/>
`install.packages("cartograflow")`

To upgrade to the development version :<br/>
`install.packages("devtools")`<br/>
`devtools::install_github("fbahoken/cartograflow")`

# Follow-up of updates

_**Work in progress**_ : <br/>
Coding of `flowplace()` and `flowlowup()` for "M" format.<br/>
Towards flow filtering for local point of view (major, dominant...). <br/>

[16 juillet] `flowlowup()`
`flowlowup()` (currently available for "L" format of matrix),<br/> is to extracts the upper or the lower triangular part of a matrix.

[05 juin] `flowplace()`
`flowplace()` (currently available for "L" format of matrix),<br/> is to compute flows margins indicators (on i,j) for places-based flow analysis. 

[03 juin] `cartograflow` CRAN Update to 1.0.3  
Suppression of the dependencies in the namespace files gdata and rgdal.
Updated `flowtype()` 

[1.0.0] `cartograflow` initial CRAN version

# Available vignettes <br/>
[\Vignette : rmd and html](https://github.com/fbahoken/cartogRaflow/blob/master/vignettes/) <br/>

[Vignette : cartograflow general - CRAN version](https://github.com/fbahoken/cartogRaflow/blob/master/vignettes/cartograflow_general.html) <br/>
[subVignette : cartograflow concentration](https://github.com/fbahoken/cartogRaflow/blob/master/vignettes/cartograflow_concentration.html) <br/>
[subVignette : cartograflow distance](https://github.com/fbahoken/cartogRaflow/blob/master/vignettes/cartograflow_distance.html)<br/>
[subVignette : cartograflow ordinal distance](https://github.com/fbahoken/cartogRaflow/tree/master/vignettes/cartograflow_ordinal_distance.hmtl) <br/>

# List of functions

## 1. Handling flow data

#### 1.1. Pre-processing

-`flowcarre()` is to transform an un-square to a square matrice from a list of spatial objets ID (code).<br/>
-`flowjointure()` is to performs a spatial join between a flow dataset and a map background.<br/>
-`flowtabmat()` is to transform a matrice format to a long format and vice versa.<br/>
-`flowstructmat()` fixes an unpreviously ID shift in the flow dataset "M" format. If necessary this function is to be used with `flowjointure()` and `flowtabmat`.

#### 1.2. Computing bilateral flows (on Fij)

Check if the matrix is close and square. See `flowcarre()` if not.<br/>

-`flowtype()` is to compute the main types of bilateral flows from an asymmetric flow dataset (matrice or long format).<br/>
x = "flux" for remaining initial flow _(Fij)_ <br/>
or x = "transpose" for reverse flow value _(Fji)_ <br/>
or x = "bivolum" for bilateral volum as gross flow _(FSij)_=(Fij+Fji) <br/>
or x = "bibal" for bilateral balance as net flow _(FBij)_=(Fij-Fji) <br/>
or x = "biasym" for asymetry of bilateral flow _(FSij)_=(FBij/FSij) <br/>
or x = "bimin" for _(minFij)_=(Fij, Fji) <br/> 
or x = "bimax" for _(maxFij(Fij, Fji))_ <br/>
or x = "birange" for bilateral _rangeFij_=(maxFij - minFij) <br/>
or x = "bidisym" for bilateral disymetry as _(FDij)_=(rangeFij/FSij).

#### 1.3. Computing flows margins indicators (on i or on j) 
`flowplace()`is to compute flow indicators from the margins of the matrix, e.g. on the flow's places of origin and/or destination.

x = "ini" for the number of incoming links (in-degree)<br/>
or x = "outi" for the number of outcoming links (out-degree)<br/>
or x = "degi" for the total number of links _ini_=(ini + outi)<br/>
or x = "Oi" for the total flows emitted by (i) place <br/>
or x = "bali" for the total flows received by (j) place <br/>
or x = "voli" for the total flow volume by place <br/>
or x = "bali" for the net balance by place <br/>
or x = "asyi" for the asymetry of flow by place <br/>
or x = "allflowplaces" for computing all the above indicators.

#### 1.4. Extracting the lower / upper part of a matrix
`flowlowup()` is to extracts the upper or the lower triangular part of a matrix - preferably for symmetrical matrixes.

x = "up"  for the part above the main diagonal <br/>
or x = "low" for the part below the main diagonal.

## 2. Flow analysis

#### 2.1. Concentration

`flowgini()` performs a concentration analysis of a flow dataset - To be use before `flowanalysis()`
Computes _Gini coefficient_ and plot _Lorenz curve_

`flowanalysis()` for computing a flow filter based on _a double criterion for selecting flows_ before mapping.

See : [subVignette : cartograflow concentration](https://github.com/fbahoken/cartogRaflow/blob/master/vignettes/cartograflow_concentration.html) <br/>

#### 2.2. Distance travelled<br/>
See : [subVignette : cartograflow distance](https://github.com/fbahoken/cartogRaflow/blob/master/vignettes/cartograflow_distance.html)<br/>

You have two ways to consider the distance travelled by flows :
-- if you have a matrice distance, go directly to `flowreduct()` at §2.2.3 ;<br/>
-- if not, you can continue here, and have to choose the type of metric (continous or ordinal)

- if you choose the continous metric, you must first join your flows' dataset to a spatial shape, using `flowjointure()`, then use `flowdist()` as described below

**2.2.1. Compute continuous distances matrices**<br/>
-`flowjointure()` performs an attribute spatial join - by origin (i) and by destination (j) - between a flow dataset and a spatial shape in order to transfert the origin-destination coordinates (Xi, Yi, Xj, Yj) of the base map to the flow matrice.<br/>
`flowdist()` Computes a _continous distance_ matrice choosing metric ("rectilinear", "euclidian", "manhattan") before using  `flowreduct()` to filter the flow dataset.

**2.2.2. Compute ordinal distances matrices** <br/>
See : [subVignette : cartograflow ordinal distance](https://github.com/fbahoken/cartogRaflow/tree/master/vignettes/cartograflow_ordinal_distance.hmtl) <br/>

`flowcontig()` is to compute an _ordinal distance_  matrice based on a k-contiguity matrice.

**2.2.3. Reducting a flow matrice by an external matrice** <br/>
`flowreduct()` is to perform the reduction of the flow dataset according to another matrice.<br/>
See : [subVignette : cartograflow distance](https://github.com/fbahoken/cartogRaflow/blob/master/vignettes/cartograflow_distance.html)<br/>

## 3. Flow mapping <br/>
`flowmap()` is to plot flows as segments or arrows. Arguments are:<br/>
 `filter` is to filter or not flow's information or features <br/>
 `threshold` is used to set the filtering level of the flows when filter="True" <br/>
 `taille` is the value of the width of the flow feature <br/>
 `a.head` is the arrow head parameter 0: no arrow, 1:i->j, 2:i<-j, 3:i<->j. <br/>
 `a.length` is the length of the edges of the arrow head (in inches) <br/>
 `a.angle` is the angle from the shaft of the arrow to the edge of the arrow head <br/>
 `a.col` is the arrow's color <br/>
 `plota` is to add spatial features as map background to the flows's plot <br/>
 `add` is to allow to overlay flow features on external spatial features background <br/>

---
References : this comes after Bahoken, Françoise (2016), _Contribution à la cartographie d'une matrice de flux_, Thèse en Géographie - Siences des territoires, URL : https://halshs.archives-ouvertes.fr/tel-01273776. 
               
