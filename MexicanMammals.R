#Mexican mammals
rm(list=ls())
#install.packages("rgdal")
install.packages("sp")
install.packages("rasterVis")
install.packages("raster")
install.packages("picante")
install.packages("vegan")
install.packages("betapart")
install.packages("stats")
install.packages("NbClust")

require(raster)
require(rgdal)
require(sp)
require(rasterVis)
require(picante)
require("vegan")
require(betapart)
require(stats)
require(NbClust)

detach("package:stats", unload=T)
detach("package:betapart", unload=T)
detach("package:vegan", unload=T)
detach("package:picante", unload=T)
detach("package:rasterVis", unload=T)
detach("package:sp", unload=T)
detach("package:rgdal", unload=T)
detach("package:raster", unload=T)


#--- LOADING OF SAVED FILES TO USE ----
mexicomammalstack_10km <- brick(stack(list.files('mexicomammalstack_10km', full.names=T, pattern="*.grd")))
mexicomammalstack_10km<- brick('mexicomammalstack10km.grd') # load file to use

mexico_aea = readOGR(dsn="mexico_aea", layer="mexico_aea") # <-- LOAD THIS!
crs(mexico_aea) <- '+proj=aea +lat_1=14.5 +lat_2=32.5 +lat_0=24 +lon_0=-105 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

ALLPA <- readOGR(dsn="UNION_ALL_AP/ALL_AP_MX.shp", encoding=NULL, use_iconv=TRUE)

PA_R <- raster("PA_MX_Raster.tif") 

load("BdiversityVEC.RData") 

#--- Attempt to calculate SR ----
#ICUN mammal data
#mammal<-readOGR(dsn="/Users/alejandratomasini/Escritorio/R/SP_MX_488", layer="mml_mx", encoding="NULL", use_iconv="FALSE")
#mammal


# #Subsample only species partly overlapping with Mexico border
# mex_mammal<-mammal[mexico,]
# 
# 
# #Species list
# levels(droplevels(mex_mammal$BINOMIAL)) 
# length(levels(droplevels(mex_mammal$BINOMIAL))) #488 (?) species
# write.csv(mex_mammal@data,'MexicanMammals.csv')
# 
# 
# #Count species in order
# #as.data.frame(tapply(mex_mammal$BINOMIAL,droplevels(mex_mammal$order_name),length))#Higher value as some species listed more than once by seperate Islands
# 
# 
# #One row per species
# mex_mammal_rowpersp<-aggregate.data.frame(mex_mammal,list(mex_mammal$BINOMIAL), FUN=head, 1)
# #as.data.frame(tapply(mex_mammal_rowpersp$BINOMIAL,droplevels(mex_mammal_rowpersp$order_name),length))
# 
# 
# #Need to merge multiple polygons for some species (presence on islands etc)
# with(droplevels(mex_mammal@data),tapply(BINOMIAL,BINOMIAL,length))
# 
# 
# #Rasterize
# #Mexico elevation
# mexelev<-getData('alt',country='MEX')
# 
# #Aggregate to 1km cells
# #Need to first reproject to an equal area grid. Check best for Mexico...
# crsmex<-CRS('+proj=aea +lat_1=14.5 +lat_2=32.5 +lat_0=24 +lon_0=-105 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') #Mexico Albers Equal Area Conic
# mexelevEA<-projectRaster(mexelev,crs=crsmex)
# r1km<-raster(ext=extent(mexelevEA),res=1000,crs=crsmex)
# 
# #Individual species
# speciesraststack<-stack(r1km,nlayers=length(levels(mexmamdropped$BINOMIAL)))
# 
# #Make a dataframe to droplevels
# mexmamdropped<-droplevels(mex_mammal@data)
# mex_mammalEA<-spTransform(mex_mammal,crsmex)
#  for(i in 1:length(levels(mexmamdropped$BINOMIAL))){
#      print(i)  
#      speciesraststack[[i]]<-rasterize(mex_mammalEA[mex_mammalEA$BINOMIAL==levels(mexmamdropped$BINOMIAL)[i],],r1km,field=1)}#Need to do by levels since some species have multiple polygons
#  speciesraststack
# 
# #Mask to elevation
# names(speciesraststack)<-levels(mexmamdropped$BINOMIAL)[1:i]
# mexelevEA1km<-resample(mexelevEA,r1km)
# speciesrasterstack<-mask(speciesraststack,mexelevEA1km)
# mexicoEA<-spTransform(mexico,crsmex)
# levelplot(speciesrasterstack[[1:4]])+
# layer(sp.polygons(mexicoEA,lwd=0.5))
# 
# for(i in 1:length(levels(mexmamdropped$BINOMIAL))){
# writeRaster(speciesraststack[[i]],filename=paste0('MexicoMammalsRaster/',names(speciesraststack)[i],'.tif'),format='GTiff')  }
# 

#mexmam_sr<-rasterize(mex_mammalEA,r1km,field='binomial',fun=function(x, ...) {length(unique(na.omit(x)))})#Need to do by levels since some species have multiple polygons
#mexmam_sr_m<-mask(mexmam_sr,mexelev)

#--- RASTERIZING AND SAVING RANGES OF MISSING SPECIES----
e_Ferox <- readOGR(dsn="EUMOPS_FEROX/E_ferox.shp", encoding=NULL, use_iconv=TRUE)
e_ferox<-spTransform(e_Ferox, proj4string(mexicomammalstack)) #Tener todo en el mismo crs (lcc)
e_FeroxR <- rasterize(e_ferox, mexicomammalstack, field="BINOMIAL", fun="count") #rasterizing
plot(e_FeroxR) #Checking it
#writeRaster(e_FeroxR,file.path('MexicoMammalsRaster/Eumops.ferox.tif'),format='GTiff',overwrite=T) #save it

p_merriami <- readOGR(dsn="Peromyscus merriami/P_merriami.shp", encoding=NULL, use_iconv=TRUE)
p_merriami<-spTransform(p_merriami, proj4string(mexicomammalstack))
p_merriamiR <- rasterize(p_merriami, mexicomammalstack, field="BINOMIAL", fun="count") 
plot(p_merriamiR) 
#writeRaster(p_merriamiR,file.path('MexicoMammalsRaster/Peromyscus.merriami.tif'),format='GTiff',overwrite=T) #save it

S_parvidens <- readOGR(dsn="STURNIDA PARVIDENS/Sturnida_parvidens.shp", encoding=NULL, use_iconv=TRUE)
S_parvidens<-spTransform(S_parvidens, proj4string(mexicomammalstack))
S_parvidensR <- rasterize(S_parvidens, mexicomammalstack, field="BINOMIAL", fun="count") 
plot(S_parvidensR) 
#writeRaster(S_parvidensR,file.path('MexicoMammalsRaster/Sturnira.parvidens.tif'),format='GTiff',overwrite=T) #save it

O_atricapillus <- readOGR(dsn="Otospermophilus.atricapillus/Otospermophilus.atricapillus.shp", encoding=NULL, use_iconv=TRUE)
O_atricapillus<-spTransform(O_atricapillus, proj4string(mexicomammalstack))
O_atricapillusR <- rasterize(O_atricapillus, mexicomammalstack, field="BINOMIAL", fun="count") 
plot(O_atricapillusR) 
#writeRaster(O_atricapillusR,file.path('MexicoMammalsRaster/Otospermophilus.atricapillus.tif'),format='GTiff',overwrite=T) #save it

#ADDING RASTER FILES FROM TANIA (BIOGEOGRAPHIC ATLAS PROJECT)
bison <- raster("bisobis_f/w001001.adf") # -> class raster
crs(bison)<- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
#writeRaster(bison, file.path('MexicoMammalsRaster/Bison.bison.tif'))
bison<- projectRaster(bison,mexicomammalstack)#resample 

Clupus <- raster("canlup_f/w001001.adf") # -> class raster
crs(Clupus)<- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
#writeRaster(Clupus, file.path('MexicoMammalsRaster/Canis.lupus.tif'))
Clupus<- projectRaster(Clupus,mexicomammalstack)#resample


#--- Calculating SR ----
#Mexico outline
mexico<-getData('GADM', country='MEX', level=0)
mexico
plot(mexico)

#Read in raster files
filelist<-list.files('MexicoMammalsRaster', full.names = T)
filelist

#Stack up
mexicomammalstack<-stack(filelist)
mexicomammalstack
plot(mexicomammalstack)

plot(mexicomammalstack[[1]])
plot(mexicomammalstack$Canis.lupus)

mexico_aea<-spTransform(mexico, crs(mexicomammalstack))
plot(mexico_aea, add=T)

#writeOGR(mexico_aea, "mexico_aea", layer="mexico_aea", driver="ESRI Shapefile")
mexico_aea = readOGR(dsn="mexico_aea", layer="mexico_aea") # <-- LOAD THIS!
crs(mexico_aea) <- '+proj=aea +lat_1=14.5 +lat_2=32.5 +lat_0=24 +lon_0=-105 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'


#Species richness
# takes SUPER!!!! long time to run
mexicomammalstack_10km<-aggregate(mexicomammalstack, fact=10, fun="modal") 

#writeRaster(mexicomammalstack_10km, 'mexicomammalstack10km.grd',format='raster',overwrite=T)#save file 2 use
mexicomammalstack_10km<- brick('mexicomammalstack10km.grd') # load file to use
#writeRaster(mexicomammalstack_10km, filename='mexicomammalstack_10km/mexicomammalstack_10km.grd',format='raster',bylayer=TRUE) # save files to github
mexicomammalstack_10km <- brick(stack(list.files('mexicomammalstack_10km', full.names=T, pattern="*.grd"))) # <-- LOAD THIS!

mexmam_sr10km<-sum(mexicomammalstack_10km, na.rm = T)
mexmam_sr10km_m<-mask(mexmam_sr10km, mexico_aea)

summary(mexmam_sr10km_m)#1-121 species

levelplot(mexmam_sr10km_m, margin=F,main='Mammal Species Richness')+
  layer(sp.polygons(mexico_aea,lwd=0.5))

#--- Loading shape files --------------------------------------------------------------
aea <- CRS('+proj=aea +lat_1=14.5 +lat_2=32.5 +lat_0=24 +lon_0=-105 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
#PA
protected_areas <- readOGR(dsn="AP_FED/AP_FED.shp", encoding=NULL, use_iconv=TRUE)
plot(protected_areas) #Only federal

PA_E_P <- readOGR(dsn="ANP_EST_PRIV/AP_EST.shp", encoding=NULL, use_iconv=TRUE)
plot(PA_E_P) #Estatales y privadas  

ALLPA <- readOGR(dsn="UNION_ALL_AP/ALL_AP_MX.shp", encoding=NULL, use_iconv=TRUE) # <-- LOAD THIS!
plot(ALLPA) #This is all federal, private and statal Protected Areas
ALLPA_aea <- spTransform(ALLPA, aea) # protected areas with crs of mexicomammalstack

PAs <- readOGR(dsn="PA_BN_FINAL.shp", encoding=NULL, use_iconv=TRUE) #PA complete (only federal), with PAs that include marine area... (islands and PA in the coastline)
PAs_aea <- spTransform(PAs, aea) 

#FVT
FVT <- readOGR(dsn="FVT/FVT_new.shp", encoding=NULL, use_iconv=TRUE)
plot(FVT)

#División Política ###NO CARGA
DivPol <- readOGR(dsn="division_política/destdv250k_2gw.shp", encoding=NULL, use_iconv=TRUE)
plot(DivPol)

#ZTM
ZTM<- readOGR(dsn="Provincias2002/ZTM/ztm0.shp", encoding=NULL, use_iconv=TRUE)
crs(ZTM)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(ZTM)
ZTM_aea<-spTransform(ZTM, aea)
NEART<- readOGR(dsn="Provincias2002/neartica.shp", encoding=NULL, use_iconv=TRUE)
crs(NEART)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
NEART_aea<-spTransform(NEART, aea)
plot(NEART_aea,add=T)
NEO<- readOGR(dsn="Provincias2002/neo0.shp", encoding=NULL, use_iconv=TRUE)
plot(NEO)
crs(NEO)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
NEO_aea<-spTransform(NEO, aea)
plot(NEO_aea)

"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
"+proj=longlat +ellps=GRS80 +no_defs "
#ZTM0.shp, neartica.shp´, neo0.shp

#--- Reprojecting to Datum=ITRF 92. All AP, FVT and SR. proj=Lambert conformal conic ----
lcc <- CRS("+proj=lcc +lat_1=17.5 +lat_2=19.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
PA_lcc <- spTransform(protected_areas, lcc) # protected areas with Lambert conformal conic (LCC)
PA_EP_lcc <- spTransform(PA_E_P, lcc)
ALLPA_lcc <- spTransform(ALLPA, lcc)
FVT_lcc <- spTransform(FVT, lcc)
PAs_lcc <- spTransform(PAs, lcc)

mexico_lcc <- spTransform(mexico_aea, lcc)
ZTM_lcc <- project(ZTM, crs(lcc))
DivPol_lcc <- spTransform(DivPol, lcc)

#--- Phylogeny ----

#Read in phylogeny
phylogeny<-read.tree('TREE_R_BN.nwk') # <-- LOAD THIS
plot(phylogeny)
phylogeny$tip.label

#Change format of tip.labels to match species data
phylogeny$tip.label<-gsub('_','.',phylogeny$tip.label)

#Do I have same list of sp. in spatial data and phylogeny?
names(mexicomammalstack_10km)[!names(mexicomammalstack_10km) %in% phylogeny$tip.label]
phylogeny$tip.label[!phylogeny$tip.label %in% names(mexicomammalstack_10km)]

#Match synonym names
phylogeny$tip.label[phylogeny$tip.label=="Peromyscus.bullatus34"] <- "Peromyscus.bullatus"
phylogeny$tip.label[phylogeny$tip.label=="Peromyscus.melanurus11"]<-  "Peromyscus.melanurus"
phylogeny$tip.label[phylogeny$tip.label=="Tamias.durangae"]<- "Neotamias.durangae"         
phylogeny$tip.label[phylogeny$tip.label=="Tamias.bulleri"]<-"Neotamias.bulleri"
names(mexicomammalstack_10km) [names(mexicomammalstack_10km) =="Cryptotis.mexicana"]<-"Cryptotis.mexicanus"
names(mexicomammalstack_10km) [names(mexicomammalstack_10km) =="Cryptotis.obscura"]<-"Cryptotis.obscurus"
names(mexicomammalstack_10km) [names(mexicomammalstack_10km) =="Baeodon.gracilis"]<-"Rhogeessa.gracilis"
names(mexicomammalstack_10km) [names(mexicomammalstack_10km) =="Baeodon.alleni"]<-"Rhogeessa.alleni"
names(mexicomammalstack_10km) [names(mexicomammalstack_10km) =="Pteronotus.mesoamericanus"]<-"Pteronotus.parnellii"
names(mexicomammalstack_10km) [names(mexicomammalstack_10km) =="Diaemus.youngi"]<-"Diaemus.youngii"
names(mexicomammalstack_10km) [names(mexicomammalstack_10km) =="Gardnerycteris.crenulatum"]<-"Mimon.crenulatum"
names(mexicomammalstack_10km) [names(mexicomammalstack_10km) =="Heterogeomys.lanius"]<-"Orthogeomys.lanius"
names(mexicomammalstack_10km) [names(mexicomammalstack_10km) =="Handleyomys.alfaroi"]<-"Oryzomys.alfaroi"
names(mexicomammalstack_10km) [names(mexicomammalstack_10km) =="Handleyomys.melanotis"]<-"Oryzomys.melanotis"
names(mexicomammalstack_10km) [names(mexicomammalstack_10km) =="Handleyomys.rostratus"]<-"Oryzomys.rostratus"
names(mexicomammalstack_10km) [names(mexicomammalstack_10km) =="Handleyomys.chapmani"]<-"Oryzomys.chapmani"
names(mexicomammalstack_10km) [names(mexicomammalstack_10km) =="Handleyomys.rhabdops"]<-"Oryzomys.rhabdops"
names(mexicomammalstack_10km) [names(mexicomammalstack_10km) =="Handleyomys.saturatior"]<-"Oryzomys.saturatior"

#write.tree(phylogeny, file="phylogeny.nwk")

#Convert raster stack to community dataframe
communitydata<- getValues(mexicomammalstack_10km)

#Replace NA with 0
communitydata[is.na(communitydata)]<-0

#Use picante to trim community and phylogenetic data
phydata <- match.phylo.comm(phylogeny, communitydata)

###
##Check through the dropped species carefully and fix any that are errors.###
###Checked, No errors, extra species in phylogeny should be dropped.
##Locate spatial data for those that are missing
###

#Calculate phylogenetic diversity
phydiv<-pd(phydata$comm, phydata$phy, include.root=T)

#Rasterize this
phydivraster<-raster(mexmam_sr10km_m)
phydivraster<-setValues(phydivraster, phydiv$PD)
phydivraster<-mask(phydivraster, mexmam_sr10km_m, maskvalue=NA)

#writeRaster(phydivraster,'MEX_MML_PD.tif',format='GTiff',overwrite=T)

levelplot(phydivraster, par.settings=YlOrRdTheme, margin=F, main='Mammal Phylogenetic Diversity',scales=list(draw=F))+
  layer(sp.polygons(mexico_aea,lwd=0.5))

levelplot(phydivraster, margin=F,main='Mammal Phylogenetic Diversity', xlab=if(isLonLat(phydivraster)) 'Longitude' else NULL,
          ylab=if(isLonLat(phydivraster)) 'Latitude' else NULL,)+
  layer(sp.polygons(mexico_aea,lwd=0.5))
#  layer(sp.polygons(bPolslaea))+
#layer(sp.polygons(mexico,lty=2))


#--- Stack together SR and PD ----
# Stack together - each as a proportion of the total species richness or phylogeney branch length

diversitystack<-stack(mexmam_sr10km_m/nlayers(mexicomammalstack_10km), phydivraster/sum(phylogeny$edge.length))
names(diversitystack)<-c("Species Richness","Phylogenetic Diversity")

##REPROJECTING TO LCC EVERYTHING: SR & PD
SR_lcc<-projectRaster(mexmam_sr10km_m, crs = lcc, method="ngb")
PD_lcc<-projectRaster(phydivraster, crs = lcc, method="ngb")
diversitystack_lcc<-projectRaster(diversitystack, crs = lcc, method="ngb")

levelplot(diversitystack_lcc, par.settings=YlOrRdTheme, margin=F, main=list("Diversity Measurements", cex=2), scales=list(draw=F))+
  layer(sp.polygons(mexico_lcc,lwd=0.5))#+ #Mapa de México
#layer(sp.polygons(ALLPA_lcc, lwd=0.5))+ #Todas las AP
#layer(sp.polygons(DivPol_lcc, lwd=0.5)) #+ #División Política de México
# layer(sp.polygons(ZTM, lwd=0.5)) #ZTM


#--- Violin box plots of SR and PD -------------------------------------------

#SR in PA
SRiPA<-mask(SR_lcc, PAs_lcc) 

levelplot(SRiPA, par.settings=YlOrRdTheme, margin=F, main='Species Richness in Protected Areas', scales=list(draw=F))+
  layer(sp.polygons(mexico_lcc, lwd=0.5))+
  layer(sp.polygons(PAs_lcc, lwd=0.5))
#BOXPLOT OF SRiPA 
boxplot(SRiPA, y=NULL, maxpixels=100000, col='blue', xlab='InsidePA', ylab='SR')

#SR out PA
SRnotPA<-mask(SR_lcc, PAs_lcc, inverse='T')

SR_i_not <- stack(SRiPA, SRnotPA)
names(SR_i_not)<-c('Protected areas','Unprotected areas')

levelplot(SRnotPA, par.settings=YlOrRdTheme, margin=F, main='Species Richness outside Protected Areas',scales=list(draw=F))+
  layer(sp.polygons(mexico_lcc,lwd=0.5))+
  layer(sp.polygons(PAs_lcc,lwd=0.5))
#BOXPLOT OF SRnotPA 
boxplot(SR_i_not, y=NULL, maxpixels=100000, col='blue', xlab='OutsidePA', ylab='SR')

#violin boxplot
myTheme <- bwTheme(
  box.rectangle = list(col = 'black', fill = 'orange'),
  plot.polygon = list(col = 'red'),
  plot.symbol = list(col = 'gray', cex = 0.8, alpha = 0.1)
)

bwplot(SR_i_not, violin = TRUE, box.ratio = 0.5, par.settings=myTheme, ylab='Species Richness', main=list('Species Richness inside/outside PA', cex=2))


#PD in PA
PDiPA<-mask(PD_lcc, PAs_lcc)

levelplot(PDiPA, par.settings=YlOrRdTheme, margin=F,main='Phylogenetic Diversity in Protected Areas')+
  layer(sp.polygons(PAs_lcc,lwd=0.5))+
  layer(sp.polygons(mexico_lcc,lwd=0.5))
#BOXPLOT OF PDiPA 
boxplot(PDiPA, y=NULL, maxpixels=100000, col='RED', xlab='InsidePA', ylab='PD')

#PD out PA
PDnotPA<-mask(PD_lcc, PAs_lcc, inverse='T')
PD_i_not <- stack(PDiPA, PDnotPA)
names(PD_i_not)<-c('Protected areas','Unprotected areas')

levelplot(PDnotPA, par.settings=YlOrRdTheme, margin=F, main='Phylogenetic Diversity outside Protected Areas',scales=list(draw=F))+
  layer(sp.polygons(mexico_lcc,lwd=0.5))+
  layer(sp.polygons(PAs_lcc,lwd=0.5))
#BOXPLOT OF PDoutPA 
boxplot(PD_i_not, y=NULL, maxpixels=100000, col='blue', xlab='OutsidePA', ylab='PD')

#violin boxplot
bwplot(PD_i_not, violin = TRUE, box.ratio = 0.5, par.settings = myTheme, ylab='Pylogenetic diversity', main=list('Phylogenetic Diversity inside/outside PA', cex=2))


#--- Basic Raster Descriptive stats ----


#SR
cellStats(mexmam_sr10km_m, stat='mean', na.rm=TRUE, asSample=TRUE)
cellStats(mexmam_sr10km_m, stat='max', na.rm=TRUE, asSample=TRUE)
cellStats(mexmam_sr10km_m, stat='min', na.rm=TRUE, asSample=TRUE)
#SRiPA
cellStats(SRiPA, stat='mean', na.rm=TRUE, asSample=TRUE)
cellStats(SRiPA, stat='max', na.rm=TRUE, asSample=TRUE)
cellStats(SRiPA, stat='min', na.rm=TRUE, asSample=TRUE)


#PD
PD_mean<-cellStats(phydivraster, stat='mean', na.rm=TRUE, asSample=TRUE)
PD_max<-cellStats(phydivraster, stat='max', na.rm=TRUE, asSample=TRUE)
PD_min<-cellStats(phydivraster, stat='min', na.rm=TRUE, asSample=TRUE)
#PDiPA
cellStats(PDiPA, stat='mean', na.rm=TRUE, asSample=TRUE)
cellStats(PDiPA, stat='max', na.rm=TRUE, asSample=TRUE)
cellStats(PDiPA, stat='min', na.rm=TRUE, asSample=TRUE)




#### to obtain number of cells, subtract NA cell count from total sum of count

srfrequency <- data.frame(freq(SR_lcc))


#--- BETA DIVERSITY ANALYSIS ----
#WHICH SP. ARE INSIDE AND WHICH OUTSIDE PA?
#PA RASTER
#mexmam_50km<- aggregate(mexmam_10km_m, fact=5, fun="modal")

mexmam_10km_m<-mask(mexicomammalstack_10km, mexico_aea)

# making raster of protected areas, giving the cells within the areas the value 1 and cells outside 0
PAs_agg<-aggregate(PAs_aea, dissolve=T)
PAs_R <- mask(sum(mexmam_10km_m, na.rm = T), PAs_agg, updatevalue=0)
PAs_R[PAs_R > 0] <- 1
names(PAs_R)<-'PA_MX_Raster'
plot(PAs_aea)

levelplot(PAs_R, par.settings=YlOrRdTheme, margin=F)+
  latticeExtra::layer(sp.polygons(mexico_aea, lwd=0.5))+
  latticeExtra::layer(sp.polygons(PAs_aea, lwd=0.5))

# stacking

PAnMmlStack <- stack(mexmam_10km_m, PAs_R) 
plot(PAnMmlStack[[8]])

levelplot(mexman_50km, par.settings=YlOrRdTheme, margin=F)+
  #levelplot(PAnMmlStack$Lepus.flavigularis,par.settings=YlOrRdTheme, margin=F)+
  layer(sp.polygons(ALLPA_aea, lwd=0.5))+
  layer(sp.polygons(mexico_aea, lwd=0.5))

#PAnMmlStack[is.na(PAnMmlStack)]<-0 # swap NA with 0 :)
#colSums(PAnMmlStack[getValues(PAnMmlStack)[,'PA_MX_Raster']>0])

# make a vector of protected and non-protected species
PAnMmlStackcolSum <- colSums(PAnMmlStack[PAnMmlStack$PA_MX_Raster > 0], na.rm="TRUE")
non_protected_species <- PAnMmlStackcolSum[PAnMmlStackcolSum==0] 
View(non_protected_species)

names(non_protected_species) #Species that are not inside any PA cell. 48

# plot the unprotected species
plot(mexico_aea)
for (name in names(non_protected_species)){
  image(mexmam_10km_m[[name]], add=TRUE)
}

levelplot(mexicomammalstack_10km$Peromyscus.melanurus, par.settings=YlOrRdTheme, margin=F, scales=list(draw=F))+
  layer(sp.polygons(mexico_aea, lwd=0.5))+
  layer(sp.polygons(ALLPA_aea, lwd=0.5))


#--- Preparing data for beta-diversity ----
communitydataPA <- data.frame(rbind(PAnMmlStackcolSum)) # make copy
communitydataPA$PA_MX_Raster <- NULL # remove PA_MX_Raster column
communitydataPA[communitydataPA > 0] <-1

# check that the vector only contains 1 and 0s
sum(communitydataPA) + length(communitydataPA[communitydataPA ==0 ]) - length(communitydataPA)

non_communitydataPA <- data.frame(PAnMmlStack[PAnMmlStack$PA_MX_Raster != 1])
non_communitydataPA$PA_MX_Raster <- NULL  # remove PA_MX_Raster column
#non_communitydataPA<-non_communitydataPA[rowSums(non_communitydataPA[, -1] > 0) != 0, ] # removing rows with only zeros

combined_communitydataPA <- rbind(non_communitydataPA, communitydataPA)


#---- B-DIVERSITY ---- 

# takes SUPER!!!! long time to run
Bdiversity <- vegdist(combined_communitydataPA, method = "bray", upper=TRUE, na.rm=TRUE)
BdiversityMTX <- as.matrix(Bdiversity)
BdiversityVEC <- BdiversityMTX[1:(ncol(BdiversityMTX)-1), ncol(BdiversityMTX)]
#save(BdiversityMTX, file="BdiversityMTX.RData")
#load("BdiversityMTX.RData")
#save(BdiversityVEC, file="BdiversityVEC.RData")
load("BdiversityVEC.RData") # <-- LOAD THIS!


BD_pair <- list()
#print(paste(nrow(phydataB$comm)))
combined_communitydataPA[is.na(combined_communitydataPA)]<-0
for (i in 1:(nrow(combined_communitydataPA)-1)){ 
  print(paste("i=",i))
  combi_comdata_cell <- combined_communitydataPA[c(i, nrow(combined_communitydataPA)),] #
  BD_pair[[i]]<-beta.pair(combi_comdata_cell, index.family="sorensen")$beta.sor
} 

BD_pair <- unlist(BD_pair)
save(BD_pair, file="BD_pair.RData")
load("BD_pair.RData") # <-- LOAD THIS!

#Rasterize this
Bdivraster<-PAs_R
Bdivraster[Bdivraster$PA_MX_Raster != 1] <- BD_pair# # replace cell values outside protected area with the beta diversity values for the corresponding cell.  
Bdivraster[Bdivraster$PA_MX_Raster == 1] <- NA # set all cell values of the protected are cells to 0 since that is the beta diversity result for those cells

levelplot(Bdivraster$PA_MX_Raster, par.settings=YlOrRdTheme, margin=F, main=list(label='Mammal Beta-Diversity',side=1,line=0.5, cex=2), scales=list(draw=F))+
  layer(sp.polygons(mexico_aea,lwd=0.5))+
  layer(sp.polygons(PAs_aea,lwa=0.5))




#--- Betadiversity PA with cluster ----


PABD_pair <- list()
#print(paste(nrow(phydataB$comm)))
PA_comm_BD <- PA_comm_all
PA_comm_BD[is.na(PA_comm_BD)]<-0

PABD_pair<-beta.pair(PA_comm_BD, index.family="sorensen")$beta.sor

PABDMTX<-as.matrix(PABD_pair)
save(PABDMTX, file="PABDMTX.RData")
load("PABDMTX.RData")
PABD_df<-as.data.frame(PABDMTX)
PABD_df<-PABD_df[is.na(rowSums(PABD_df))==F,is.na(colSums(PABD_df))==F]
heatmap(as.matrix(PABD_df),Colv = NA, Rowv = NA, scale="column")

# hierarchical clustering
df<-as.dist(as.matrix(PABD_df))
hc<-hclust(df, method = "ward.D")

# optimal number of clusters
optclust<-NbClust(df,method='ward.D', index='cindex',min.nc=2,max.nc=10)#3
optclust

PA_cluster<-cutree(hc,k=10)
mycol<-c("#1d7eff", "#ff3434", "#3daa31", "#0492c2", "#99b038", "#0fa721", "#c70000", "#32cd32", "#384800", "#0a1172")
#mycol<-c("#429afd", "#d4271e","#3daa31", "#5e9a78", "#115011", "#6b8e23", "#0a1172", "#3daa31", "#53aeb6", "#0327d6", "#b21c1c") #, "#2b976c","#73a4a7", "#e1577a", "#b9c189")#"#f6726d", "#b7a204", "#1fbe37","#37cbcd","#7ca9fd","#ee71e1")  #c(brewer.pal(8,'Dark2'),1,2)
plot(as.phylo(hc),tip.color=mycol[PA_cluster], cex=0.5, no.margin=T)

PA_cluster<-as.matrix(PA_cluster)

i=1
for (PA_name in rownames(PA_cluster)){
  PA_single_aea <- PAs_aea[PAs_aea@data$NOMBRE == PA_name,]
  PA_single_mms <- mask(PAs_R, PA_single_aea)
  PA_single_mms[PA_single_mms==1]<-PA_cluster[PA_name,][[1]]
  names(PA_single_mms)<-PA_name
  if(i ==1){P_sim_PA<-PA_single_mms}else{P_sim_PA <- merge(P_sim_PA, PA_single_mms)}
  i = i+1
}

P_sim_PA[P_sim_PA==0] <- NA # set non-ground cells to NA

myTheme <- modifyList(custom.theme(region=mycol),
                      list(
                        strip.background=list(col='white'),
                        panel.background=list(col='white')))

#YlOrRdTheme

levelplot(P_sim_PA, par.settings=myTheme, colorkey=F, margin=F, main=list('Compositional Similarity between PA', cex=2), scales=list(draw=F))+
  latticeExtra::layer(sp.polygons(mexico_aea,lwd=0.5))+
  latticeExtra::layer(sp.polygons(PAs_aea,lwd=0.5))#+

#--- PHYLOGENETIC BETA DIVERSITY ----
phylogeny <- read.tree("phylogeny.nwk")

communitydataB<-combined_communitydataPA
communitydataB[is.na(communitydataB)]<-0
phydataB<-match.phylo.comm(phylogeny, communitydataB)

# the following for-loop calculates only the phylogenetic beta-diversity between the cells outside the protected area with the protected area community cell.
PBD_pair <- list()
for (i in 1:(nrow(phydataB$comm)-1)){ 
  print(paste("i=",i))
  combi_comdata_cell <- phydataB$comm[c(i, nrow(phydataB$comm)),] #
  PBD_pair[[i]]<-phylo.beta.pair(combi_comdata_cell, phydataB$phy, index.family="sorensen")$phylo.beta.sor
}  

PBD_pair <- unlist(PBD_pair)

#Rasterize this
PBDraster<-PAs_R
PBDraster[PBDraster$PA_MX_Raster != 1] <- PBD_pair # replace cell values outside protected area with beta diversity values for the corresponding cell. 
PBDraster[PBDraster$PA_MX_Raster == 1] <- NA # set all cell values of the protected are cells to 0 since that is the beta diversity result for those cells

save(PBDraster, file="PBDrasterV2.RData")
load("PBDrasterV2.RData") # <-- LOAD THIS!

levelplot(PBDraster$PA_MX_Raster, par.settings=YlOrRdTheme, margin=F, main=list('Mammal Phylogenetic Beta-Diversity', cex=2), scales=list(draw=F))+
  layer(sp.polygons(mexico_aea,lwd=0.5))+
  layer(sp.polygons(PAs_aea,lwd=0.5))

#--- PBD cell check ----

cell<-2055
PBDMTX <- as.matrix(PBD)
PBDVEC <- PBDMTX[1:(ncol(PBDMTX)-1), cell] #ncol(PBDMTX)]
PBDVEC[cell]<-NA
save(PBDMTX, file="PBDMTXv3.RData")
load("PBDMTXv4.RData")
save(PBDVEC, file="PBDVECv2.RData")
load("BdiversityVEC.RData") # <-- LOAD THIS!

PBDVEC <- PBDMTX[1:(ncol(PBDMTX)-1), ncol(PBDMTX)]
PBDVEC[cell]<-NA

PBDraster<-PAs_R
PBDraster[PBDraster$PA_MX_Raster != 1] <- PBDVEC # replace cell values outside protected area with beta diversity values for the corresponding cell. 
PBDraster[PBDraster$PA_MX_Raster == 1] <- NA
names(PBDraster)<-"PBD"

levelplot(PBDraster$PA_MX_Raster, par.settings=YlOrRdTheme, margin=F, main='Mammal Phylogenetic Beta-Diversity',scales=list(draw=F))+
  layer(sp.polygons(mexico_aea,lwd=0.5))

#--- pairplot of PD and PyloBetaD ----
PDraster<-phydivraster
names(PDraster)<-"PD"
names(PBDraster)<-"PBD"
PD_PBD_stack <- stack(PDraster, PBDraster)
pairs(PD_PBD_stack, hist=TRUE, cor=TRUE, use="pairwise.complete.obs") #"Pair-plot of PD vs PBD"
# it is linear. the correlation is 1.0....

#--- pairplot of PD and SR ----
SRraster<-raster(mexmam_10km_m)
SRraster<-setValues(SRraster, PD$SR)
SRraster<-mask(SRraster, sum(mexmam_10km_m, na.rm=T), maskvalue=NA)

SRraster<-mexmam_sr10km_m
names(SRraster)<-"SR"
PD_SR_stack <- stack(SRraster, PDraster)
pairs(PD_SR_stack, hist=T, cor=T, use="pairwise.complete.obs") #"Pair-plot of SR vs PD"

#--- pairplot of PyloBetaD and BetaD ----
names(Bdivraster)<-"BD"
PBD_BD_stack <- stack(Bdivraster, PBDraster)
pairs(PBD_BD_stack, hist=T, cor=T, use="pairwise.complete.obs") # Pair-plot of PBD vs BD


#--- PBD PA ----
PA_names <- as.character(PAs_aea@data$NOMBRE)

i <- 1
for(PA_name in PA_names) {
  print(i)
  PA_single <- PAs_aea[PAs_aea@data$NOMBRE == PA_name,]
  PA_single_aea <- spTransform(PA_single, crs(mexmam_sr10km_m))
  PA_single_mms <- mask(PAnMmlStack, PA_single_aea)
  PA_single_mms_col_sum <- colSums(getValues(PA_single_mms), na.rm="TRUE")
  PA_comm_single <- data.frame(rbind(PA_single_mms_col_sum ))
  PA_comm_single$PA_MX_Raster <- NULL # remove PA_MX_Raster column
  PA_comm_single[PA_comm_single > 0] <-1
  rownames(PA_comm_single) <- PA_name
  
  if (i == 1){
    PA_comm_all <- PA_comm_single
  } 
  else{
    PA_comm_all<-rbind(PA_comm_all, PA_comm_single) 
  }
  i <- i + 1
}


save(PA_comm_all, file="PA_comm_all.RData")
load("PA_comm_all.RData") # <-- LOAD THIS!

PA_comm_all<-PA_comm_all[,colSums(PA_comm_all!=0)> 0] # removes columns of unprotected species

phylogeny <- read.tree("phylogeny.nwk")

#Use picante to trim community and phylogenetic data
phydataPAB <- match.phylo.comm(phylogeny, PA_comm_all)

PBD_2<-phylo.beta.pair(phydataPAB$comm, phydataPAB$phy, index.family = "sorensen")

PBDMTX_1<-as.matrix(PBD_2$phylo.beta.sor)
save(PBDMTX_1, file="PBDMTXv4.RData")
load("PBDMTXv4.RData")
PBD_df<-as.data.frame(PBDMTX_1)
PBD_df<-PBD_df[is.na(rowSums(PBD_df))==F,is.na(colSums(PBD_df))==F]
heatmap(as.matrix(PBD_df),Colv = NA, Rowv = NA, scale="column")

#name_lable <- cbind(rownames(PBD_df), 1:nrow(PBD_df))
#write.csv(name_lable, file = "RESULTS/name lables of PA for cluster plot.csv")

# hierarchical clustering
df<-as.dist(as.matrix(PBD_df))
hc<-hclust(df, method = "ward.D")

# optimal number of clusters
optclust<-NbClust(df,method='ward.D', index='cindex',min.nc=2,max.nc=10)#3
optclust

PA_cluster<-cutree(hc,k=10)
mycol<-c("#1d7eff", "#ff3434", "#3daa31", "#0492c2", "#1338be", "#0fa721", "#c70000", "#32cd32", "#384800", "#0a1172")
#mycol<-c("#429afd", "#d4271e","#3daa31", "#5e9a78", "#115011", "#6b8e23", "#0a1172", "#3daa31", "#53aeb6", "#0327d6", "#b21c1c") #, "#2b976c","#73a4a7", "#e1577a", "#b9c189")#"#f6726d", "#b7a204", "#1fbe37","#37cbcd","#7ca9fd","#ee71e1")  #c(brewer.pal(8,'Dark2'),1,2)
plot(as.phylo(hc),tip.color=mycol[PA_cluster], cex=0.5, no.margin=T)

PA_cluster<-as.matrix(PA_cluster)

i=1
for (PA_name in rownames(PA_cluster)){
  PA_single_aea <- PAs_aea[PAs_aea@data$NOMBRE == PA_name,]
  PA_single_mms <- mask(PAs_R, PA_single_aea)
  PA_single_mms[PA_single_mms==1]<-PA_cluster[PA_name,][[1]]
  names(PA_single_mms)<-PA_name
  if(i ==1){P_sim_PA<-PA_single_mms}else{P_sim_PA <- merge(P_sim_PA, PA_single_mms)}
  i = i+1
}

P_sim_PA[P_sim_PA==0] <- NA # set non-ground cells to NA

myTheme <- modifyList(custom.theme(region=mycol),
                      list(
                        strip.background=list(col='white'),
                        panel.background=list(col='white')))

#YlOrRdTheme

levelplot(P_sim_PA, par.settings=myTheme, colorkey=F, margin=F, main=list('Phylogenetic Similarity between PA', cex=2), scales=list(draw=F))+
  latticeExtra::layer(sp.polygons(mexico_aea,lwd=0.5))+
  latticeExtra::layer(sp.polygons(PAs_aea,lwd=0.5))#+
  #layer(sp.polygons(NEART_aea, lwd=0.5))+
  #layer(sp.polygons(ZTM_aea, lwd=0.5))

# PA and their cluster #
PA_cluster_vec <- as.data.frame(cbind(rownames(PA_cluster), PA_cluster))
col <- c( 'Federal Protected Areas','Cluster number')
colnames(PA_cluster_vec) <- col
PA_cluster_ranked <- PA_cluster_vec[order(PA_cluster_vec$`Cluster number`),]
write.csv(PA_cluster_ranked, file = "RESULTS/cluster of federal protected areas.csv")


#--- Ranking of PA based on PD ----
##RANK PA BASED ON PD - must important PA for protecting mammal PD
##for this we will only use federal protected areas

PA_names <- as.character(PAs_aea@data$NOMBRE)
plot(PAs_aea[PAs_aea@data$NOMBRE == PA_names[47],])

PA_name <- PA_names[47]
PA_single <- PAs_aea[PAs_aea@data$NOMBRE == PA_name,]
PA_single_aea <- spTransform(PA_single, crs(mexmam_sr10km_m))
PA_single_sr <- mask(mexmam_sr10km_m, PA_single_aea)
PA_single_sr
plot(mexico_aea)+
  plot(PA_single_aea, add=T)

# make a table with federal protected areas and their maximum PD value
PA_PD_max_all <- list()
i <- 1
for(PA_name in PA_names) {
  print(i)
  PA_single <- PAs_aea[PAs_aea@data$NOMBRE == PA_name,]
  PA_single_aea <- spTransform(PA_single, crs(mexmam_sr10km_m))
  PA_single_sr <- mask(mexmam_sr10km_m, PA_single_aea)
  
  # from pholygeny
  phydivraster_PA<-raster(mexmam_sr10km_m)
  phydivraster_PA<-setValues(phydivraster_PA, phydiv$PD)
  phydivraster_PA<-mask(phydivraster_PA, PA_single_aea)
  
  PA_PD_max_all[[i]] <- (maxValue(phydivraster_PA)/PD_mean-1)*100 #14.19698
  i <- i + 1
}

col <- c('Federal Protected Areas', 'Maximum Value of the Phylogenetic Diversity normalized to the mean')
PA_PD_table <- data.frame(PA_names, unlist(PA_PD_max_all))
colnames(PA_PD_table) <- col
PA_PD_table_ranked <- PA_PD_table[order(PA_PD_table$`Maximum Value of the Phylogenetic Diversity`, decreasing = T),]
write.csv(PA_PD_table_ranked, file = "RESULTS/Ranking of federal protected areas norm.csv")
sum(is.na(PA_PD_table)) # number of federal PA that are too small to get PD from. 59

levelplot(phydivraster_PA, par.settings=YlOrRdTheme, margin=F, main='Mammal Phylogenetic Diversity',scales=list(draw=F))+
  layer(sp.polygons(PA_single_aea, lwd=0.5))+
  layer(sp.polygons(mexico_aea, lwd=0.5))


###TO DO
#HOW MUCH OF THE UNPROTECTED AREA WILL BE AHOTSPOT-Most valuable areas to add to current PA scheme
