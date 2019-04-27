## ----include=FALSE, message=FALSE----------------------------------------

rm(list=ls())

library(dplyr)
library(cartograflow)
library(cartography)


## ----data, warning=FALSE-------------------------------------------------

data<-read.csv2("./data/MOBPRO_ETP.csv",
                header=TRUE,
                sep=";",
                stringsAsFactors=FALSE,
                encoding="UTF-8",
                dec=".",
                check.names=FALSE)
head(data)

str(data)

# Variable typing
data$i<-as.character(data$i)
data$j<-as.character(data$j)
data$Fij<-as.numeric(data$Fij)
data$count<-as.numeric(data$count)


# Loading a list of geo codes
ID_CODE<-read.csv2("./data/COD_GEO_EPT.csv",
                   header=TRUE,
                   sep=";",
                   stringsAsFactors=FALSE,
                   encoding="UTF-8",
                   dec=".",
                   check.names=FALSE)
head(ID_CODE)

CODE<-ID_CODE%>% dplyr::select(COD_GEO_EPT)

colnames(CODE)<-c("CODGEO")

head(CODE)


## ----pre-processing, fig.show='hold'-------------------------------------

tabflow<-data%>%select(i,j,Fij)

# Change matrice format (if necessary)
matflow <-flowtabmat(tabflow,matlist="M")
head(matflow[1:4,1:4])
dim(matflow)


## ----diag, fig.show='hold', echo=TRUE------------------------------------

# Zero the diagonal of matrice format (if necessary)
diag(matflow) <- 0
head(matflow[1:4,1:4])

# Change matrice to list format
tabflow<-flowtabmat(tab=matflow,
                    matlist="L")
head(tabflow)
colnames(tabflow)<-c("i","j","Fij")


## ----types of flow-------------------------------------------------------

# Compute bilateral flow volume - from a "M" format
matflow_vol<-flowtype(matflow,
                      format="M",
                      "bivolum")

# Compute bilateral flow volume - from a "L" format

# FSij will be the gross Fij flow values
tabflow_vol<-flowtype(tabflow,
                     format="L",
                     "bivolum")
head(tabflow_vol)

# Compute bilateral flow balance - from a "L" format

# FDij will be the net Fij flow values
tabflow_net<-flowtype(tabflow,
                      format="L",
                      "bisold")
head(tabflow_net)
  
# Compute all types of bilateral flows, in one 6 columns "L"format matrice
tabflow_all<-flowtype(tabflow, 
                      format="L", 
                      x="all")
head(tabflow_all) 
 
# Compute flow asymetry
tabflow_all$FAsy<-(tabflow_all$FDij / tabflow_all$FDij)*100


## ---- ECHO=FALSE,fig.width=7, maps_all,fig.show='hold', message=TRUE, warning=FALSE----

knitr::opts_chunk$set(fig.width=6, fig.height=6)

par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-300

# Plot all theoretical OD links 
flowmap(tab = tabflow,
        format="L",
        fdc="./data/MGP_TER.shp",
        code="EPT_NUM",
        filter=FALSE) #no filter criterion

mtext("All theoretical relations - no filter",side = 3)


## ---- maps_links,fig.show='hold', message=TRUE, warning=FALSE------------

par(mar=c(0,0,1,0))

#Plot existing relations (up to 1000 commuters)
flowmap(tab = tabflow,
        format="L",
        fdc="./data/MGP_TER.shp",
        code="EPT_NUM",
        filter=TRUE,        #add filter
        a.col="#3f4247",
        threshold=1000,     
        taille=8,           
        a.head = 1,
        a.length = 0.11)

mtext("Flows up to 1000 commuters (~ 50%)",side = 3)


## ----lecho=TRUE, fig.show='hold'-----------------------------------------

# Plot flow value up to a global filter criterion 

par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-300

# Mapping filtered observed commuters
flowmap(tab = tabflow,
              format="L",
              fdc="./data/MGP_TER.shp",
              code="EPT_NUM",
              filter=TRUE,
              a.col="#3f4247",
              threshold=7406,  # Mean=7406 
              taille=8,        
              a.head = 1,      
              a.length = 0.11)

mtext("Flows up to mean value (7406 commuters)",side = 3)

# Bilateral flow volum of commuters
flowmap(tab = tabflow_vol,
              format="L",
              fdc="./data/MGP_TER.shp",
              code="EPT_NUM",
              filter=TRUE,
              a.col="#3f4247",
              threshold=14812.4,  # Mean=14812.4
              taille=14,        
              a.head = 0,      
              a.length = 0.11)

mtext("Bilateral flow volume of commuters up to mean (14812 commuters)",side = 3)

# Bilateral flow balance of commuters
flowmap(tab = tabflow_net,
              format="L",
              fdc="./data/MGP_TER.shp",
              code="EPT_NUM",
              filter=TRUE,
              a.col="#3f4247",
              threshold=8547,  # Mean=8547
              taille=8,        
              a.head = 1,      
              a.length = 0.11)

mtext("Bilateral flow balance of commuters up to mean (8547 commuters)",side = 3)



## ----analysis, fig.show='hold',message = TRUE , warning=FALSE------------

head(tabflow,3)

# 1- Computes Gini's coefficent
#--------------------
tab_gini<-flowgini(tabflow,
                   format="L",
                   origin="i",
                   dest="j",
                   valflow="Fij",
                   fdc = "./data/MGP_TER.shp",
                   code="EPT_NUM",
                   lorenz.plot = FALSE)

#Interpretation ; The flows are quite concentrated on a few links, the Gini coefficent is equal to 71% 

# 2- Plot Lorenz curve
#--------------------
#head(tab_gini)

flowgini(tab_gini, 
         format="L",
         origin="i",
         dest="j",
         valflow="ydata",
         fdc = "./data/MGP_TER.shp",
         code="EPT_NUM",
         lorenz.plot = TRUE)

# 3- Compute critflow parameter and flowmap
#-------------------------------
#critflow = 0.8
flowanalysis(tab_gini,
             critflow = 0.8,
             result = "signif")

# Interpretation : Flow values up to 11238 are the 80% largest one corresponding to 22,94% of the total links.

#threshold = 11238

par(mar=c(0,0,1,0))
flowmap(tabflow,
        format="L",
        fdc="./data/MGP_TER.shp",
        code="EPT_NUM",
        filter=TRUE,
        threshold=11238,
        taille=8,
        a.head = 1,
        a.length = 0.11,
        a.angle = 30,
        a.col="#3f4247")

mtext("Significative flowmap : values up to 11238 - 80% flow information - 22.9% total links",side = 3)

# 4- Compute critlink parameter and flowmap 
#-------------------------------

flowanalysis(tab_gini,
             critlink = 0.02,
             result = "density")

# Interpretation : Flows up to 73743 are the 14.5% largest one corresponding to 2 % of the total links

# Plot 2 % of the total features equals to select flow greater than 73743 commuters
par(mar=c(0,0,1,0))

flowmap(tab = tabflow,
        format="L",
        fdc="./data/MGP_TER.shp",
        code="EPT_NUM",
        filter=TRUE,
        a.col="#3f4247",
        threshold=7343,
        taille=8,
        a.head = 1,
        a.length = 0.11,
        a.angle = 30
        )

mtext("Low density flowmap : values up to 73743 - 14.5% flow information  - 2%  total links",side = 3)


## ----echo=TRUE, fig.show='hold'------------------------------------------

par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-300

# Final flowmap customized
flowmap(tabflow,
        format="L",
        fdc="./data/MGP_TER.shp",
        code="EPT_NUM",
        filter=TRUE,
        threshold=7343, 
        taille=5,   
        a.head = 1,
        a.length = 0.11,
        a.angle = 30,
        a.col="#138913"
        )

# Legend
#legendPropLines(pos="topleft",
#                title.txt="Number of commuters up to 11238 (the 80% largest flows)",
#                title.cex=1,   
#                cex=0.8,
#                values.cex= 0.7,  
#                var=c(11238,max(tabflow$Fij)), 
#                lwd=5, 
#                frame = FALSE,
#                col="#138913",
#                values.rnd = 0
#                )

#layoutLayer(title = "Professional mobility in Greater Paris",
#           coltitle ="black",
#           author = "Cartograflow, 2019",
#            sources = "Sources : data : INSEE, RP, MOBPRO, 2017 ; basemap : IGN, APUR, UMS 2414 RIATE, 2018.",
#            scale = 0.2,
#            tabtitle = TRUE,
#            frame = TRUE,
#            north(pos = "topright"),
#            col = "grey"
#                        )


## ----echo=TRUE, fig.show='hold'------------------------------------------

head(tabflow)

tab<-flowjointure(tabflow,
                  "./data/MGP_TER.shp",
                  "EPT_NUM")

tab.distance<-flowdist(tab,
                       dist.method = "euclidian",
                       result = "dist")
head(tab.distance)

#reduce the flow dataset from a selected distance travelled < 8.5 km
library(rlang)

tab.flow<-flowreduct(tab,
                     tab.distance,
                     metric = "continous",
                     select = "dmax", #max distance parameter 
                     d = 8567)        #max distance value - Q1 : 8567 km

#select for all i,j flow values up to 0
flow.d<-tab.flow%>%
       select(i,j,flowfilter)%>%
        filter(flowfilter !=0)

#Flowmap : flow travelled less than 8.5 km  (Q1)

par(mar=c(0,0,1,0))

extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-300

flowmap(flow.d,format="L",
       "./data/MGP_TER.shp",
       "EPT_NUM",
        filter = TRUE,
        taille = 5,
        a.col="#138913",
        a.length = 0.11,
        a.head =1)

#legendPropLines(pos="topleft",
#                title.txt="Number of commuters (distance travelled less than 8,5 km)",
#                title.cex=1,    
#                cex=0.8,
#                values.cex= 0.8,  
#                var=c(min(flow.d$flowfilter),8567), 
#                col="#138913",
#                lwd=5,
#                frame = FALSE,
#                values.rnd = 0
#                )
# Habillage

#layoutLayer(title = "Professional mobility in Greater Paris : short distance travelled",
#            author = "Cartograflow, 2019",
#            sources = "Sources : data : INSEE, RP, MOBPRO, 2017 ; basemap : IGN, APUR, UMS 2414 RIATE, 2018",
#            scale = 5,
#            tabtitle = TRUE,
#            frame = TRUE,
#            north(pos = "topright"),
#            col = "grey",
#            coltitle ="black")


## ----echo=TRUE, fig.show='hold'------------------------------------------

head(tabflow)

tab<-flowjointure(tabflow,
                  "./data/MGP_TER.shp",
                  "EPT_NUM")

tab.distance<-flowdist(tab,
                       dist.method = "euclidian",
                       result = "dist")

tab.flow<-flowreduct(tab,
                     tab.distance,
                     metric = "continous",
                     select = "dmin",  
                     d = 14518)        #Q2 : 14518 km - Q3:19234 km

#select for all i,j flow values up to 0
flow.d<-tab.flow%>%
       select(i,j,flowfilter)%>%
        filter(flowfilter !=0)


#Flowmap : flow travelled up to (Q3)

par(mar=c(0,0,1,0))

extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-300

flowmap(flow.d,format="L",
       "./data/MGP_TER.shp",
       "EPT_NUM",
        filter = TRUE,
        taille = 5,
        a.col="#138913",
        a.length = 0.11,
        a.head =1)

#legendPropLines(pos="topleft",
#                title.txt="Number of commuters (distance travelled more than 14.5 km)",
#                title.cex=1,    
#                cex=0.8,
##                values.cex= 0.8,  
#                var=c(14518, max(flow.d$flowfilter)), 
#                col="#138913",
#                lwd=5, 
#                frame = FALSE,
#                values.rnd = 0
#                )
# Habillage

#layoutLayer(title = "Professional mobility in Greater Paris : mean distance travelled",
#            author = "Cartograflow, 2019",
#            sources = "Sources : data : INSEE, RP, MOBPRO, 2017 ; basemap : IGN, APUR, UMS 2414 RIATE, 2018",
#            scale = 5,
#            tabtitle = TRUE,
#            frame = TRUE,
#            north(pos = "topright"),
#            col = "grey",
#            coltitle ="black")


## ----echo=TRUE, fig.show='hold'------------------------------------------


## Neighbouring graph (ordre 1)
graph_ckij_1<-flowcontig("./data/MGP_TER.shp",
                         "EPT_NUM",
                         ordre =1)

flowmap(graph_ckij_1,
        format="L",
        "./data/MGP_TER.shp",
        "EPT_NUM",
        filter = TRUE, 
        taille = 0.5)

mtext("Neighbouring graph (order 1)",
      side=3)

## Reducing flow matrice by the neighbouring graph (order= 1)
reduc<-flowreduct(tabflow,
                  graph_ckij_1,
                  metric = "ordinal")

flow.c<-reduc %>%
  select(i,j,flux)%>%
  filter(flux!=0)

#Plot adjacent flowmap 
par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-300

flowmap(flow.c,
        format="L",
        "./data/MGP_TER.shp",
        "EPT_NUM",
        filter = TRUE,
        taille = 5,
        a.col="#138913",
        a.length = 0.1,
        a.head =1)

# Legend
#legendPropLines(pos="topleft",
#                title.txt="Number of commuters (one border distance)",
#                title.cex=1,    
#                cex=0.8,
#                values.cex= 0.8,  
#                var=c(min(flow.c$flux),max(flow.c$flux)), 
#                col="#138913",
#                lwd=5, 
#                frame = FALSE,
#                values.rnd = 0
#                )
# Habillage

#layoutLayer(title = "Professional mobility in Greater Paris between neighbouring municipalities",
#            author = "Cartograflow, 2019",
#            sources = "Sources : data : INSEE, RP, MOBPRO, 2017 ; basemap : IGN, APUR, UMS 2414 RIATE, 2018",
#            scale = 5,
#            tabtitle = TRUE,
#            frame = TRUE,
#            north(pos = "topright"),
#            col = "grey",
 #           coltitle ="black")


## ----lecho=TRUE, fig.show='hold'-----------------------------------------

sessionInfo()


