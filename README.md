# Cartograflow <img src="doc/Logo_cartograflow.png" align="right" alt="" width="120" />
**Filtering origin-destination Matrices for Thematic Flowmapping**

[![](https://www.r-pkg.org/badges/version/cartograflow)](https://cran.r-project.org/package=cartograflow)
[![](https://cranlogs.r-pkg.org/badges/cartograflow?color=brightgreen)](https://cran.r-project.org/package=cartograflow)

This package is designed to filter origin-destination matrices for flow mapping purposes. It is based on different functions that are mainly used to prepare the flow dataset (links) relatively to geography (spatial shapes of nodes or areas) in order to reduce the flowmap graphic complexity.

# Installation

Installing cartograflow package from CRAN as follows:<br/>
`install.packages("cartograflow")`

To upgrade to the development version :<br/>
`install.packages("devtools")`<br/>
`devtools::install_github("fbahoken/cartograflow")`

# Follow-up of updates

_**Work in progress**_ : 
Computing flows margins indicators (on i,j) - for places-based flow analysis<br/>
Computing major, dominant... flows analysis - for places-based flow analysis<br/>

[22 mai] Addition possibilities of  `flowtype()` computations possibilities for bilateral flows : "biasym","bimin", "bimax","birange", "bidisym". Change name of bisold to "bibal".

[17 mai] Package being updated on the CRAN. Compatibiliy with `sf` seems ok for all functions.
- `flowmap()` resulting plot allow overlays with other spatial features

[1.0.1] (under development) updating functions to `sf` and solves overlay problems with `cartography`. <br/>
Makes especially `flowmap()` more flexible.

[1.0.0] `cartograflow` initial CRAN version

# Available vignettes <br/>
See in : (/cartograflow/vignettes/) <br/>
- _cartograflow_general.html_ (CRAN version) <br/>
- _cartograflow_concentration.html_ <br/>
- _cartograflow_distance.html_ <br/>
- _cartograflow_ordinal_distance.hmtl_ <br/>

# List of functions

## 1. Handling flow data

#### 1.1. Pre-processing

-`flowcarre()` is to transform an un-square to a square matrice from a list of spatial objets ID (code).<br/>
-`flowjointure()` is to performs a spatial join between a flow dataset and a map background.<br/>
-`flowtabmat()` is to transform a matrice format to a long format and vice versa.<br/>
-`flowstructmat()` fixes an unpreviously ID shift in the flow dataset "M" format. If necessary this function is to be used with `flowjointure()` and `flowtabmat`.

#### 1.2. Computing bilateral flows (on Fij)

Check if the matrix is close and square. See `flowcarre()` if not.<br/>

-`flowtype()` is to compute the main types of bilateral flows from an asymmetric flow dataset (matrice or long format). The result is x = "flux" for remaining initial flow _(Fij)_ or "transpose" for reverse flow value _(Fji)_ or "bivolum" for bilateral volum as gross flow _(FSij)_=(Fij+Fji) or "bibal" for bilateral balance or net flow _(FBij)_=(Fij-Fji) or "biasym" for asymetry of bilateral flow _(FSij)_=(FBij/FSij) or "bimin" for _(minFij)_=(Fij, Fji) or "bimax" for _(maxFij(Fij, Fji))_ or "birange" for bilateral _rangeFij_=(maxFij - minFij) or "bidisym" for bilateral disymetry as _(FDij)_=(FSij/rangeFij). 

#### 1.3. Computing flows margins indicators (on i,j) 
_Work in progress_

## 2. Flow analysis

#### 2.1. Concentration

-`flowgini()` performs a concentration analysis of a flow dataset - To be use before `flowanalysis()`
Computes _Gini coefficient_ and plot _Lorenz curve_

-`flowanalysis()` for computing a flow filter based on _a double criterion for selecting flows_ before mapping.

See vignette _cartograflow_concentration.html_

#### 2.2. Distance travelled<br/>
See vignette _cartograflow_distance.html_<br/>

You have two ways to consider the distance travelled by flows :
-- if you have a matrice distance, go directly to `flowreduct()` at §2.2.3 ;<br/>
-- if not, you can continue here, and have to choose the type of metric (continous or ordinal)

- if you choose the continous metric, you must first join your flows' dataset to a spatial shape, using `flowjointure()`, then use `flowdist()` as described below

**2.2.1. Compute continuous distances matrices**<br/>
-`flowjointure()` performs an attribute spatial join - by origin (i) and by destination (j) - between a flow dataset and a spatial shape in order to transfert the origin-destination coordinates (Xi, Yi, Xj, Yj) of the base map to the flow matrice.<br/>
-`flowdist()` Computes a _continous distance_ matrice choosing metric ("rectilinear", "euclidian", "manhattan") before using  `flowreduct()` to filter the flow dataset.

**2.2.2. Compute ordinal distances matrices** <br/>
See vignette _cartograflow_ordinal_distance.html_

-`flowcontig()` is to compute an _ordinal distance_  matrice based on a k-contiguity matrice.

**2.2.3. Reducting a flow matrice by an external matrice** <br/>
-`flowreduct()` is to perform the reduction of the flow dataset according to another matrice.

## 3. Flow mapping <br/>
- `flowmap()` is to plot flows as segments or arrows. Arguments are:<br/>
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
               
