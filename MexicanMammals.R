#Mexican mammals
rm(list=ls())
require(raster)
require(rgdal)

#Mexico outline
mexico<-getData('GADM',country='MEX',level=0)
mexico
plot(mexico)

mexalt<-getData('alt',country='MEX')

#ICUN mammal data
#mammal<-readOGR(dsn="/Users/alejandratomasini/Escritorio/R/SP_MX_488", layer="mml_mx", encoding="NULL", use_iconv="FALSE")
mammal<-readOGR('mammal shapefile','mml_mx')
mammal


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




