#Mexican mammals
rm(list=ls())
#install.packages("rgdal")
#install.packages("sp")
#install.packages("rasterVis")

require(raster)
require(rasterVis)
require(rgdal)
require(sp)


#Mexico outline
mexico<-getData('GADM',country='MEX',level=0)
mexico
plot(mexico)

mexalt<-getData('alt',country='MEX')

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
# 
# #Mask to elevation
# names(speciesraststack)<-levels(mexmamdropped$BINOMIAL)[1:i]
# mexelevEA1km<-resample(mexelevEA,r1km)
# speciesrasterstack<-mask(speciesraststack,mexelevEA1km)
# mexicoEA<-spTransform(mexico,crsmex)
# levelplot(speciesrasterstack[[1:4]])+
# layer(sp.polygons(mexicoEA,lwd=0.5))
# 
# 
# for(i in 1:length(levels(mexmamdropped$BINOMIAL))){
# writeRaster(speciesraststack[[i]],filename=paste0('MexicoMammalsRaster/',names(speciesraststack)[i],'.tif'),format='GTiff')  }
# 

#Read in raster files
filelist<-list.files('MexicoMammalsRaster',full.names = T)
filelist
#Stack up
mexicomammalstack<-stack(filelist)
mexicomammalstack

plot(mexicomammalstack[[1]])
plot(mexicomammalstack$Zygogeomys.trichopus)

mexicoEA<-spTransform(mexico,crs(mexicomammalstack))
plot(mexicoEA,add=T)


plot(mexicomammalstack[[1]])
plot(mexicomammalstack$Romerolagus.diazi)


#Species richness
#mexmam_sr<-rasterize(mex_mammalEA,r1km,field='binomial',fun=function(x, ...) {length(unique(na.omit(x)))})#Need to do by levels since some species have multiple polygons
#mexmam_sr_m<-mask(mexmam_sr,mexelev)

mexicomammalstack_10km<-aggregate(mexicomammalstack,fact=10,fun="modal")
mexmam_sr10km<-sum(mexicomammalstack_10km,na.rm = T)
mexmam_sr10km_m<-mask(mexmam_sr10km,mexicoEA)

levelplot(mexmam_sr10km_m,margin=F,main='Mammal Species Richness')+
  layer(sp.polygons(mexico,lwd=0.5))

writeRaster(mexmam_sr10km_m,'MexicoMammalSpeciesRichness_10km.tif',format='GTiff',overwrite=T)
summary(mexmam_sr_m)#1-121 species

mexmam_sr10km_m<-mask(mexmam_sr10km,mexicoEA)
 levelplot(mexmam_sr10km_m,margin=F,main='Mammal Species Richness')+
   layer(sp.polygons(mexico,lwd=0.5))
 #mexicomammalstack_10km<-aggregate(mexicomammalstack,fact=10,fun="modal")
 
 


#PA Mexico
protected_areas <- readOGR(dsn="~/Mexican-mammal-phylogenetic-diversity/AP_FED.shp", encoding=NULL, use_iconv=TRUE)
plot(protected_areas) #Only federal
PA_E_P <- readOGR(dsn="~/Mexican-mammal-phylogenetic-diversity/ANP_EST_PRIV/AP_EST.shp", encoding=NULL, use_iconv=TRUE)
plot(PA_E_P) #Estatales y privadas  
ALLPA <- readOGR(dsn="~/Mexican-mammal-phylogenetic-diversity/UNION_ALL_AP/ALL_AP_MX.shp", encoding=NULL, use_iconv=TRUE)
plot(ALLPA) #This is all federal, private and statal Protected Areas


#FVT
FVT <- readOGR(dsn="~/Mexican-mammal-phylogenetic-diversity/FVT/FVT_new.shp", encoding=NULL, use_iconv=TRUE)
plot(FVT)


#Reprojecting to Datum=ITRF 92. All AP, FVT and SR. proj=Lambert conformal conic
#Not sure if lcc has correct Datum. 
lcc <- CRS("+proj=lcc +lat_1=17.5 +lat_2=19.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
PA_lcc <- spTransform(protected_areas, lcc) # protected areas with Lambert conformal conic (LCC)
PA_EP_lcc <- spTransform(PA_E_P, lcc)
ALLPA_lcc <- spTransform(ALLPA, lcc)
FVT_lcc <- spTransform(FVT, lcc)



#we were not sure how to reproject the raster 
new2 <- spTransform(protected_areas, proj4string(mexicomammalstack_10km)) # protected areas with raster CRS
FVT2 <- spTransform(FVT, proj4string(mexicomammalstack_10km))
ALLPA2 <- spTransform(ALLPA, proj4string(mexicomammalstack_10km))


# Plot
image(mexmam_sr10km_m) # plot raster
plot(new2, add=TRUE) # add vector on top
plot(FVT2, add=TRUE)
plot(ALLPA2, add=TRUE)


#For saving and loading rasters

# saving
#writeRaster(mexmam_sr10km_m,'MexicoMammalSpeciesRichness_10km.tif',format='GTiff',overwrite=T)
#Loading
tiff_SR<-raster('MexicoMammalSpeciesRichness_10km.tif')
plot(tiff_SR)

#reprokecting SR raster
#SR_lcc is the name of the SR raster file with the correct projection
SR_lcc<-projectRaster(tiff_SR,crs = lcc)




# #Phylogeny --------------------------------------------------------------

#Read in phylogeny
install.packages("picante")
require(picante)
phylogeny<-read.tree('TREE_R_BN.nwk')
plot(phylogeny)
phylogeny$tip.label

#Change format of tip.labels to match species data
phylogeny$tip.label<-gsub('_','.',phylogeny$tip.label)

#Match synonym names

names(mexicomammalstack)[!names(mexicomammalstack) %in% phylogeny$tip.label]
#
phylogeny$tip.label[!phylogeny$tip.label %in% names(mexicomammalstack)]

phylogeny$tip.label[phylogeny$tip.label=="Peromyscus.bullatus34"] <- "SPeromyscus.bullatus"
phylogeny$tip.label[phylogeny$tip.label=="Peromyscus.melanurus11"]<-  "Peromyscus.melanurus"
phylogeny$tip.label[phylogeny$tip.label=="Tamias.durangae"]<- "Neotamias.durangae"         
phylogeny$tip.label[phylogeny$tip.label=="Tamias.bulleri"]<-"Neotamias.bulleri"
mexicomammalstack$names[mexicomammalstack$names=="Tamias.bulleri"]<-"Neotamias.bulleri"

#Convert raster stack to community dataframe
communitydata<- getValues(herbivore_dataset3)
#Replace NA with 0
communitydata[is.na(communitydata)]<-0

#Use picante to trim community and phylogenetic data
phydata<-match.phylo.comm(phylogeny,communitydata)

###
##Check through the dropped species carefully and fix any that are errors.###
###Checked, No errors, extra species in phylogeny should be dropped.
#
##Locate spatial data for those that are missing
###

#Calculate phylogenetic diversity
phydiv<-pd(phydata$comm,phydata$phy,include.root=T)

#Rasterize this
phydivraster<-raster(speciesrichness)
phydivraster<-setValues(phydivraster,phydiv$PD)
phydivraster<-mask(phydivraster,speciesrichness,maskvalue=NA)

levelplot(phydivraster,par.settings=YlOrRdTheme,margin=F)+
   layer(sp.polygons(bPolslaea))+
   layer(sp.polygons(arczones_laea,lty=2))



