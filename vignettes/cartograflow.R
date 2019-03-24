## ----load packages, message=FALSE----------------------------------------

rm(list=ls())

library(dplyr)
library(sf)
library(cartography)
library(cartograflow)


## ----data----------------------------------------------------------------

data<-read.csv2("./data/tab_MGP_mobpro.csv",header=TRUE,sep=";",stringsAsFactors=FALSE,
                encoding="UTF-8",dec=".",check.names=FALSE)
head(data)

tabflow<-data%>% 
            select(CODGEO,DCLT,NBFLUX_C14_ACTOCC15P)
colnames(tabflow)<-c("i","j","Fij")

head(tabflow)

str(tabflow)

#typage des variables
tabflow$i<-as.character(tabflow$i)
tabflow$j<-as.character(tabflow$j)
tabflow$Fij<-as.numeric(tabflow$Fij)

ID_CODE<-read.csv2("./data/COD_GEO_MGP.csv",header=TRUE,sep=";",stringsAsFactors=FALSE,encoding="UTF-8",dec=".",check.names=FALSE)
head(ID_CODE)

CODE<-ID_CODE%>%select(COD_GEO)
colnames(CODE)<-c("CODGEO")

#CODE = as.data.table(CODE)
head(CODE)
#str(CODE)

#typage des variables
#CODE$CODGEO<-as.character(CODE$CODGEO)


## ----pre-processing, fig.show='hold'-------------------------------------

#change matrice format (if necessary)
matflow <-flowtabmat(tabflow,matlist="M")
head(matflow[1:4,1:4])
dim(matflow)

#The Warning says that the matrice is note square.Dimension is 141x183 that is why we make it square, using cartograflow::flowcarre
matflow.sq<-flowcarre(CODE,tabflow,"i","j","Fij",format = "M",diagonale = TRUE,empty.sq = FALSE)
head(matflow.sq[1:4,1:4])
dim(matflow.sq)


## ----diag, fig.show='hold'-----------------------------------------------

# zero the diagonal of matrice format (if necessary)
# mat_nodiag<-matflow.sq

# diag(mat_nodiag) <- 0
# head(mat_nodiag[1:4,1:4])

# re change matrice format
# tabflow_nodiag <-flowtabmat(tab=mat_nodiag,
#                   matlist="L")
# colnames(tabflow_nodiag) <- c("i","j","Fij")
# head(tabflow_nodiag)


## ----types of flow-------------------------------------------------------

#matflow <-mat_nodiag

# Compute bilateral volume - from a "M" format
  matflow_vol<-flowtype(tab=matflow.sq,
                        format="M","bivolum")

# Compute bilateral volume - from a "L" format
  tabflow.sq<-flowtabmat(matflow.sq,matlist = "L")
  
# Compute only bilateral volume or net flows in one 3 column "L" format matrice
  # FSij will be the gross Fij flow values
  
  tabflow_vol<-flowtype(tabflow,
                        format="L","bivolum")
  head(tabflow_vol)

  # FDij will be the net Fij flow values
  tabflow_sold<-flowtype(tabflow,
                        format="L","bisold")
  head(tabflow_sold)
  
#compute all types in one 6 columns "L" format matrice
tabflow_all<-flowtype(tabflow, 
                      format="L", 
                      x="all")
head(tabflow_all) 
 
#compute and flow asymetry
tabflow_all$FAsy<-(tabflow_all$FDij / tabflow_all$FDij)*100

 

## ---- maps_all,fig.show='hold', message=TRUE, warning=FALSE--------------
knitr::opts_chunk$set(fig.width=6, fig.height=6)

# Plot all theoretical OD links (no filter)
flowmap(tab = tabflow.sq,
               format="L",
               fdc="./data/MGP_communes.shp",
               code="IDCOM",
               filtre=FALSE)

mtext("All theoretical relations",side = 3)


## ---- maps_links,fig.show='hold', message=TRUE, warning=FALSE------------

#Plot all existing relations
flowmap(tab = tabflow.sq,
                 format="L",
                 fdc="./data/MGP_communes.shp",
                 code="IDCOM",
                 filtre=TRUE,
                 a.col="#3f4247",
                 seuil=1, #flow values up to 1
                 taille=1,
                 a.fleche = 0)

mtext("All existing relations ",side = 3)


## ---- maps_up_100,fig.show='hold', message=TRUE, warning=FALSE-----------

#Plot only flow values up to 100 (the french INSEE's)
flowmap(tab = tabflow.sq,
                 format="L",
                 fdc="./data/MGP_communes.shp",
                 code="IDCOM",
                 filtre=TRUE,
                 a.col="#3f4247",
                 seuil=100,   #flow value > 100
                 taille=5,     #graphic parameteter
                 a.fleche = 1, #add arrow
                 a.length = 0.11)

mtext("Existing flows up to 100",side = 3)



## ----lecho=TRUE, fig.show='hold'-----------------------------------------

tabflow.sq<-tabflow.sq %>% filter(tabflow.sq$ydata!=0)

#extraction des quantiles
summary(tabflow.sq$ydata)

decile<-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
quantile(tabflow.sq$ydata,decile)


#Plot all flow up to Q3

par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-300

flowmap(tab = tabflow.sq,
                 format="L",
                 fdc="./data/MGP_communes.shp",
                 code="IDCOM",
                 filtre=TRUE,
                 a.col="#3f4247",
                 seuil=748,   #flow value > 748 (90% information)
                 taille=5,     #graphic parameteter
                 a.fleche = 1, #add arrow
                 a.length = 0.11)

mtext("Existing flows up to 748 - 90% information",side = 3)


## ----analysis, fig.show='hold',message = FALSE , warning=FALSE-----------
# LIST FORMAT
#--------------------

tabflow.sq<-flowtabmat(matflow.sq,matlist = "L")

# 1- Computes Gini coefficient
#--------------------
tab_gini<-flowgini(tabflow.sq,
                   format="L",
                   origin="i",
                   dest="j",
                   valflow="ydata",
                   fdc = "./data/MGP_communes.shp",
                   code="IDCOM",
                   lorenz.plot = FALSE)

head(tab_gini)

# 2- Plot Lorenz curve
#--------------------

flowgini(tab_gini, 
         format="L",
         origin="i",
         dest="j",
         valflow="ydata",
         fdc = "./data/MGP_communes.shp",
         code="IDCOM",
         lorenz.plot = TRUE)


# 3- Compute critflow parameter and flowmap
#-------------------------------
#critflow = 0.02 (2% of the largest flows)
flowanalysis(tab_gini,
             critflow = 0.02,
             result = "signif")

#seuil = 3070 - 2% flows - 0,1% links
par(mar=c(0,0,1,0))
flowmap(tabflow.sq,
        format="L",
        fdc="./data/MGP_communes.shp",
        code="IDCOM",
        filtre=TRUE,
        seuil=1176,
        taille=5,
        a.fleche = 1,
        a.length = 0.11,
        a.angle = 30,
        a.col="#3f4247")

mtext("significative flowmap : values up to 3070 - 2% flow information - 0,1% links",side = 3)

# 4- Compute critlink parameter and flowmap 
#-------------------------------

flowanalysis(tab_gini,
             critlink = 0.01,
             result = "density")

# Plot 4 % of the total features, flow greater than 2015
par(mar=c(0,0,1,0))
flowmap(tab = tabflow.sq,
        format="L",
        fdc="./data/MGP_communes.shp",
        code="IDCOM",
        filtre=TRUE,
        a.col="#3f4247",
        seuil=2045,
        taille=5,
        a.fleche = 1,
        a.length = 0.11,
        a.angle = 30
        )

mtext("Small density flowmap : values up to 2045 - 7,21 flow information  - 1%  links",side = 3)


## ----echo=TRUE, fig.show='hold'------------------------------------------

library(cartography)

par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-300

# Flow map
flowmap(tabflow.sq,
        format="L",
        fdc="./data/MGP_communes.shp",
        code="IDCOM",
        filtre=TRUE,
        seuil=2045, # Min  value
        taille=4, #taille max du plus gros flux  
        a.fleche = 1,
        a.length = 0.11,
        a.angle = 30,
        a.col="#3f4247")

# Legend
##---------------
legendPropLines(pos="topleft",
                title.txt="Number of commuters",
                title.cex=1,    # taille du titre de la legende
                cex=0.8,
                values.cex= 0.7,  # size of the values in the legend.
                var=c(2045,max(tabflow.sq$ydata)), # # Min  - Max flow value
                lwd=4, #taille max du plus gros flux 
                frame = FALSE,
                col="#3f4247",
                values.rnd = 0
                )

layoutLayer(title = "Professional mobility in Greater Paris : the strongest 1%",
            author = "Cartograflow, 2019",
            sources = "Sources : data : INSEE, RP, MOBPRO, 2017 ; basemap : IGN - GEOFLA 2015, UMS 2414 RIATE, 2018",
            scale = 5,
            tabtitle = TRUE,
            frame = TRUE,
            north(pos = "topright"),
            col = "grey",
            coltitle ="black")

# Add territorial boundaries
fdc_ter <- st_read("./data/MGP_territoires.shp", stringsAsFactors = F)

# Harmonizing projections
prj <- 2154
fdc_ter <- st_transform(fdc_ter, prj)

plot(fdc_ter$geometry, col=NA, border="#332d2e",lwd=1.9, add=T)



## ----echo=TRUE, fig.show='hold'------------------------------------------

tab<-flowjointure(tabflow.sq,"./data/MGP_communes.shp","IDCOM")
tab.distance<-flowdist(tab,dist.method = "euclidian",result = "dist")

#summarise (for example) to find mean, dmin or dmax 
summary(tab.distance$distance)

#reduce the flow dataset from a selected distance travelled < 4506 m (corresponding to Q1)
tab.flow<-flowreduct(tabflow.sq,
                     tab.distance,
                     metric = "continous",
                     select = "dmax", #max distance parameter 
                     d = 4056)  #max distance value

#select for all i,j flow values up to 0
  flow.d<-tab.flow%>%
        select(i,j,flowfilter)%>%
        filter(flowfilter !=0)

flowmap(flow.d,format="L","./data/MGP_communes.shp","IDCOM",
        filtre = TRUE,
        a.col="#3f4247",
        a.length = 0.11,
        a.fleche =1)

mtext("Professional mobility less than 4506 meters (Q1)")

## Final flowmap less than 4,5 km 

#dev.off() #nettoyage fenetre graphique

par(mar=c(0,0,1,0))

extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-300

flowmap(flow.d,format="L","./data/MGP_communes.shp","IDCOM",
        filtre = TRUE,
        taille = 4,
        a.col="#3f4247",
        a.length = 0.11,
        a.fleche =1)

plot(fdc_ter$geometry, col=NA, border="black",lwd=1.9, add=T)

legendPropLines(pos="topleft",
                title.txt="Number of commuters",
                title.cex=1,    # taille du titre de la legende
                cex=0.8,
                values.cex= 0.7,  # size of the values in the legend.
                var=c(min(flow.d$flowfilter),max(flow.d$flowfilter)), # Min - Max de la serie
                col="#3f4247",
                lwd=4, #taille max du plus gros flux défini ds flowmap
                frame = FALSE,
                values.rnd = 0
                )
# Habillage

layoutLayer(title = "Professional mobility in Greater Paris : distance travelled less than 4.5 km",
            author = "Cartograflow, 2019",
            sources = "Sources : data : INSEE, RP, MOBPRO, 2017 ; basemap : IGN - GEOFLA 2015, UMS 2414 RIATE, 2018",
            scale = 5,
            tabtitle = TRUE,
            frame = TRUE,
            north(pos = "topright"),
            col = "grey",
            coltitle ="black")




## ----echo=TRUE, fig.show='hold'------------------------------------------

## Neighbouring graph (ordre 1)
graph_ckij_1<-flowcontig("./data/MGP_communes.shp","IDCOM",ordre =1)

flowmap(graph_ckij_1,
        format="L",
        "./data/MGP_communes.shp","IDCOM",
        filtre = TRUE, 
        taille = 0.5)

mtext("Neighbouring graph (ordre 1)",
      side=3)

## Reducing flow matrice by the neighbouring graph (ordre 1)
reduc<-flowreduct(tabflow.sq,graph_ckij_1,metric = "ordinal")
flow.c<-reduc %>%
  select(i,j,flux)%>%
  filter(flux!=0)

## Plot adjacent flows 
flowmap(flow.c,format="L","./data/MGP_communes.shp","IDCOM",
        filtre = TRUE,
        taille = 2,
        a.col="#3f4247",
        a.length = 0.11,
        a.fleche =1)

mtext("Adjacent flows",
      side=3)

## Final adjacent flowmap 

#dev.off() #nettoyage fenetre graphique

par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-300

flowmap(flow.c,format="L","./data/MGP_communes.shp","IDCOM",
        filtre = TRUE,
        taille = 4,
        a.col="#3f4247",
        a.length = 0.1,
        a.fleche =1)

# Add territorial boundaries
fdc_ter <- st_read("./data/MGP_territoires.shp", stringsAsFactors = F)

prj <- 2154
fdc_ter <- st_transform(fdc_ter, prj)

plot(fdc_ter$geometry, 
     col=NA, 
     border="#332d2e",
     lwd=1.9, 
     add=T)

# Add Legend
legendPropLines(pos="topleft",
                title.txt="Number of commuters",
                title.cex=1,    # taille du titre de la legende
                cex=0.8,
                values.cex= 0.7,  # size of the values in the legend.
                var=c(min(flow.c$flux),max(flow.c$flux)), # Min - Max de la serie
                col="#3f4247",
                lwd=4, #taille max du plus gros flux défini ds flowmap
                frame = FALSE,
                values.rnd = 0
                )
# Habillage

layoutLayer(title = "Professional mobility in Greater Paris between neighbouring municipalities",
            author = "Cartograflow, 2019",
            sources = "Sources : data : INSEE, RP, MOBPRO, 2017 ; basemap : IGN - GEOFLA 2015, UMS 2414 RIATE, 2018",
            scale = 5,
            tabtitle = TRUE,
            frame = TRUE,
            north(pos = "topright"),
            col = "grey",
            coltitle ="black")

#mtext("Note : Only flows between adjacent municipalities are plot",
 #     side=2)


## ---- fig.show='hold'----------------------------------------------------

## Neighbouring graph (ordre 2)
graph_ckij_2<-flowcontig("./data/MGP_communes.shp","IDCOM",ordre =2)
head(graph_ckij_2)

flowmap(graph_ckij_2,
        format="L",
        "./data/MGP_communes.shp","IDCOM",
        filtre = TRUE, 
        taille = 0.5)

mtext("Neighbouring graph (order 2)",
      side=3)

## Reducing flow matrice by the neighbouring graph (ordre 2)
reduc2<-flowreduct(tabflow.sq,graph_ckij_2,metric = "ordinal")

flow.c2<-reduc2 %>%
         select(i,j,flux)%>%
         filter(flux!=0)

## Plot adjacent flows 
flowmap(flow.c2,format="L","./data/MGP_communes.shp","IDCOM",
        filtre = TRUE,
        taille = 4,
        a.length = 0.11,
        a.fleche =1)

mtext("Flows between municipalities distant from 2 boundaries",
      side=3)


## ---- fig.show='hold'----------------------------------------------------

library(sf)

## Final adjacent flowmap 
#dev.off() #nettoyage fenetre graphique

par(mar=c(0,0,1,0))

extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-300

flowmap(flow.c2,format="L","./data/MGP_communes.shp","IDCOM",
        filtre = TRUE,
        taille = 4,
        a.length = 0.11,
        a.fleche =1,
        a.col = "#3f4247")

legendPropLines(pos="topleft",
                title.txt="Number of commuters",
                title.cex=1,    # taille du titre de la legende
                cex=0.8,
                values.cex=0.7,  # size of the values in the legend.
                var=c(min(flow.c2$flux),max(flow.c2$flux)), # Min - Max de la serie
                col="#3f4247",
                lwd=4, #taille max du plus gros flux défini ds flowmap
                frame = FALSE,
                values.rnd = 0
                )
#Habillage
layoutLayer(title = "Professional mobility in Greater Paris between municipalities from two boundaries",
            author = "Cartograflow, 2019",
            sources = "Sources : data : INSEE, RP, MOBPRO, 2017 ; basemap : IGN - GEOFLA 2015, UMS 2414 RIATE, 2018",
            scale = 5,
            tabtitle = TRUE,
            frame = TRUE,
            north(pos = "topright"),
            col = "grey",
            coltitle ="black")

#mtext("Note : Only flows between municipalities distant by 2 boundaries are plot",
#      side=2)

# Import de fonds
fdc_ter <- st_read("./data/MGP_territoires.shp", stringsAsFactors = F)
prj <- 2154

fdc_ter <- st_transform(fdc_ter, prj)

# Add territorial limits

#plot(fdc_com$geometry, col=NA, border="blue",lwd=0.2)
plot(fdc_ter$geometry, col=NA, border="#332d2e",lwd=1.9, add=T)
#plot(fdc_lim$geometry,col=NA,border="red",lwd=1.5,add=T)



## ----lecho=TRUE, fig.show='hold'-----------------------------------------

sessionInfo()


