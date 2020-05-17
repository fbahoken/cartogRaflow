## ----message=FALSE, include=FALSE---------------------------------------------

rm(list=ls())

library(sf)
library(dplyr)
library(cartograflow)
library(cartography)


## ----flowdata_preprocess, echo=TRUE, message=FALSE, warning=FALSE-------------

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


## ----flowdata_reverse, warning=FALSE, echo=TRUE-------------------------------

# Selecting useful variables
tabflow<-data%>%select(i,j,Fij)

# Change matrix format (if necessary)
matflow <-flowtabmat(tabflow,matlist="M")
head(matflow[1:4,1:4])
#dim(matflow)

# reverse : matrix to list format
tabflow<-flowtabmat(tab=matflow,
                    matlist="L")
colnames(tabflow)<-c("i","j","Fij")
head(tabflow)


## ----data_preprocess, warning=FALSE, echo=TRUE--------------------------------

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


## ----data_computing, warning=FALSE, echo=TRUE---------------------------------

# Compute bilateral volum : FSij
matflow_vol<-flowtype(matflow,
                      format="M",
                      "bivolum")

tabflow_vol<-flowtype(tabflow,
                     format="L",
                     "bivolum")

# Compute bilateral balance : FSij
tabflow_net<-flowtype(tabflow,
                      format="L",
                      "bisold")

# Compute all types of bilateral flows, in one 6 columns "L"format matrix
tabflow_all<-flowtype(tabflow, 
                      format="L", 
                      x="all")
head(tabflow_all) 
 
# Compute flow asymetry
tabflow_all$FAsy<-(tabflow_all$FDij / tabflow_all$FDij)*100


## ----maps, echo=TRUE, fig.width=7, message=FALSE, warning=FALSE---------------

library(sf)
data(flowdata)
map <- st_read(system.file("shape/MGP_TER.shp", package = "cartograflow"))

knitr::opts_chunk$set(fig.width=6, fig.height=6)
par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-150


## ----maps_links, echo=TRUE, fig.show='hold', fig.width=7, message=TRUE, warning=FALSE, ECHO=FALSE----

# Add and overlay spatial background 
par(bg = "NA")

knitr::opts_chunk$set(fig.width=6, fig.height=6)
par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-150

# Add the corresponding background 
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


## ----maps_flowmean, echo=TRUE, fig.show='hold', fig.width=7, message=FALSE, warning=FALSE, ECHO=FALSE----

library(sf)
data(flowdata)
map <- st_read(system.file("shape/MGP_TER.shp", package = "cartograflow"))

# Add and overlay spatial background 
par(bg = "NA")

knitr::opts_chunk$set(fig.width=6, fig.height=6)
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
                title.txt="Flows up to 13220 commuters",
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


