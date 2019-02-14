#Mexican mammals
rm(list=ls())
require(raster)
require(rgdal)


#Mexico outline
mexico<-getData('GADM',country='MEX',level=0)
mexico
plot(mexico)

#ICUN mammal data
# mammal<-readOGR('TERRESTRIAL_MAMMALS_2016','TERRESTRIAL_MAMMALS')
# mammal
# 
# #Subsample only species partly overlapping with Mexico border
# mex_mammal<-mammal[mexico,]
# #Species list
# levels(droplevels(mex_mammal$binomial)) 
# length(levels(droplevels(mex_mammal$binomial))) #376 species
# write.csv(mex_mammal@data,'MexicanMammals.csv')
# 
# #Count species in order
# as.data.frame(tapply(mex_mammal$binomial,droplevels(mex_mammal$order_name),length))#Higher value as some species listed more than once by seperate Islands
# 
# #One row per species
# mex_mammal_rowpersp<-aggregate.data.frame(mex_mammal,list(mex_mammal$binomial), FUN=head, 1)
# as.data.frame(tapply(mex_mammal_rowpersp$binomial,droplevels(mex_mammal_rowpersp$order_name),length))
# 
# 
# #Need to merge multiple polygons for some species (presence on islands etc)
# with(droplevels(mex_mammal@data),tapply(binomial,binomial,length))
# 
# #Rasterize
# #Mexico elevation
# mexelev<-getData('alt',country='MEX')
# #Aggregate to 1km cells
# #Need to first reproject to an equal area grid. Check best for Mexico...
# crsmex<-CRS('+proj=aea +lat_1=14.5 +lat_2=32.5 +lat_0=24 +lon_0=-105 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs') #Mexico Albers Equal Area Conic
# mexelevEA<-projectRaster(mexelev,crs=crsmex)
# r1km<-raster(ext=extent(mexelevEA),res=1000,crs=crsmex)
# 
# #Individual species
# speciesraststack<-stack(r1km)
# #Make a dataframe to droplevels
# mexmamdropped<-droplevels(mex_mammal@data)
# mex_mammalEA<-spTransform(mex_mammal,crsmex)
# for(i in 1:length(levels(mexmamdropped$binomial))){
#     print(i)  
#     speciesraststack[[i]]<-rasterize(mex_mammalEA[mex_mammalEA$binomial==levels(mexmamdropped$binomial)[i],],r1km,field=1)}#Need to do by levels since some species have multiple polygons
# speciesraststack
# #Mask to elevation
# names(speciesraststack)<-levels(mexmamdropped$binomial)[1:i]
# mexelevEA1km<-resample(mexelevEA,r1km)
# speciesrasterstack<-mask(speciesraststack,mexelevEA1km)
# mexicoEA<-spTransform(mexico,crsmex)
# levelplot(speciesrasterstack[[1:4]])+
#   layer(sp.polygons(mexicoEA,lwd=0.5))
# 
# for(i in 1:length(levels(mexmamdropped$binomial))){
#  writeRaster(speciesrasterstack[[i]],filename=paste0('MexicoMammals/',names(speciesrasterstack)[i],'.tif'),format='GTiff')  }


#Species richness
mexmam_sr<-rasterize(mex_mammalEA,r1km,field='binomial',fun=function(x, ...) {length(unique(na.omit(x)))})#Need to do by levels since some species have multiple polygons
mexmam_sr_m<-mask(mexmam_sr,mexelev)
levelplot(mexmam_sr_m,margin=F,main='Mammal Species Richness')+
  layer(sp.polygons(mexico,lwd=0.5))
writeRaster(mexmam_sr_m,'MexicoMammalSpeciesRichness_1km.tif',format='GTiff',overwrite=T)
summary(mexmam_sr_m)#1-121 species


#Checking some species names

require(rgbif)
#File from mike
missingsp<-read.csv('result.csv',header=T)
#First 45 species
missingspnames<-missingsp$taxon[1:45]
#Checking rgbif occ_search
a1<-occ_search(scientificName='Neotoma palatina',basisOfRecord='PRESERVED_SPECIMEN',country='MX',publishingCountry = 'MX')
a1
a1$data$datasetName
a1$data$institutionID

mexspeccounts<-data.frame(taxon=missingspnames,count_pubMX=NA,count_occTot=NA)
#Replace underscore with space
mexspeccounts$taxon<-sub("_"," ",mexspeccounts$taxon)

for(i in 1:45){
  a1<-occ_search(scientificName=mexspeccounts$taxon[i],basisOfRecord='PRESERVED_SPECIMEN',country='MX',publishingCountry = 'MX')
  a2<-occ_search(scientificName=mexspeccounts$taxon[i],basisOfRecord='PRESERVED_SPECIMEN')
  mexspeccounts$count_pubMX[i]<-a1$meta$count
  mexspeccounts$count_occTot[i]<-a2$meta$count
}
mexspeccounts
write.csv(mexspeccounts,'CountGBIFRecordsforMissingSp.csv')
