# Cartograflow
Filtering Matrices for Thematic Cartography in R - Dealing with "spaghetti-effect"

This package is designed to create the so-called **flowmaps**, thematic origin-destination (OD) maps by filtering matrices.
It is based on different functions that are mainly used to prepare the flow dataset . The spatial objects processing are those of `{sp}``{sf}` and the mapping elements are often those of `{Cartography}` except particular cases.

# List of functions

## 1. Preparing flow dataset

#### 1.1. Pre-processing

-`flowcarre()` is to transform an un-square to a square matrice from a list of spatial objets ID (code).

-`flowjointure()` is to performs a spatial join between a flow dataset and a map background.

-`flowtabmat()` is to transform a matrice format to a long format and vice versa.

-`flowstructmat()` fixes an unpreviously ID shift in the flow dataset "M" format. If necessary this function is to be used with `flowjointure()` and `flowtabmat`.

#### 1.2. Computing flows

It is to decide firstly to zero or not the diagonal, see `{base::diag}`.

-`flowtype()` is to compute the main types of flows from an asymmetric flow dataset (matrice or long format). The result is a bilateral gross or bilateral net flows matrice.
It is also possible to compute the matrice's margins in order to calculate probabilities of sending and receiving flows or all kinds of indicators. Use for that the R `{base}` or `{dplyr}`.

## 2. Flow analysis

#### 2.1. Concentration

-`flowgini()` performs a concentration analysis of a flow dataset - To be use before `flowanalysis()`
Computes _Gini coefficient_ and plot _Lorenz curve_

-`flowanalysis()` is to be used after `flowgini()` for computing a flow filter based on _a double criterion for selecting flows_ before mapping :

- level of flow thresholding, and the corresponding filter ;
- desired threshold level of flow's information significativity (% of total of flow information) ;
or
- desired threshold level of flow's features density (% of total features). 

#### 2.2. Distance travelled

You have two ways to consider the distance travelled by flows :
-- if you have a matrice distance, go directly to `flowreduct()` at §2.2.3 ;

-- if not, you can continue here, and have to choose the type of metric (continous or ordinal)

- if you choose the continous metric, you must first join your flows' dataset to a spatial shape, using `flowjointure()`, then use `flowdist()` as described below

**2.2.1. Compute continuous distances matrices**

-`flowjointure()` performs an attribute spatial join - by origin (i) and by destination (j) - between a flow dataset and a spatial shape in order to transfert the origin-destination coordinates (Xi, Yi, Xj, Yj) of the base map to the flow matrice.

-`flowdist()` Computes a _continous distance_ matrice choosing metric (rectilinear, euclidian, manhattan, ...) before using  `flowreduct()` to filter the flow dataset.

**2.2.2. Compute ordinal distances matrices** 

-`flowcontig()` compute an _ordinal distance_ distance matrice based on a k-contiguity matrice. (k) is the order parameter, the number of borders to be crossed between origins and destinations places. Use after `flowreduct()` and directly `flowmap()`without applying the filter parameter. It is possible to map firstly the *k-order neighbourhood spatial graph* using `flowmap()` without applying the filter parameter.

**2.2.3. Reducting a flow matrice by an external matrice**

-`flowreduct()` is to perform the reduction of the flow dataset according to another matrice (especially a matrice distance)

## 3. Flow mapping
-`flowmap()` is to create a layer of lines and plot them, using a flow dataset and a spatial shape.

---
References : this comes after Bahoken, Françoise (2016), _Contribution à la cartographie d'une matrice de flux_, Thèse en Géographie - Siences des territoires, URL : https://halshs.archives-ouvertes.fr/tel-01273776. 


                
