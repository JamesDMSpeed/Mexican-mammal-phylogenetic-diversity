#Mexican mammals
rm(list=ls())
#install.packages("rgdal")
install.packages("sp")
install.packages("rasterVis")
install.packages("raster")


require(raster)
require(rgdal)
require(sp)
require(rasterVis)

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

#RASTERIZING AND SAVING RANGES OF MISSING SPECIES
e_Ferox <- readOGR(dsn="~/Mexican-mammal-phylogenetic-diversity/EUMOPS_FEROX/E_ferox.shp", encoding=NULL, use_iconv=TRUE)
e_ferox<-spTransform(e_Ferox, proj4string(mexicomammalstack)) #Tener todo en el mismo crs (lcc)
e_FeroxR <- rasterize(e_ferox, mexicomammalstack, field="BINOMIAL", fun="count") #rasterizing
plot(e_FeroxR) #Checking it
writeRaster(e_FeroxR,file.path('~/Mexican-mammal-phylogenetic-diversity/MexicoMammalsRaster/Eumops.ferox.tif'),format='GTiff',overwrite=T) #save it

p_merriami <- readOGR(dsn="~/Mexican-mammal-phylogenetic-diversity/Peromyscus merriami/P_merriami.shp", encoding=NULL, use_iconv=TRUE)
p_merriami<-spTransform(p_merriami, proj4string(mexicomammalstack))
p_merriamiR <- rasterize(p_merriami, mexicomammalstack, field="BINOMIAL", fun="count") 
plot(p_merriamiR) 
writeRaster(p_merriamiR,file.path('~/Mexican-mammal-phylogenetic-diversity/MexicoMammalsRaster/Peromyscus.merriami.tif'),format='GTiff',overwrite=T) #save it

S_parvidens <- readOGR(dsn="~/Mexican-mammal-phylogenetic-diversity/STURNIDA PARVIDENS/Sturnida_parvidens.shp", encoding=NULL, use_iconv=TRUE)
S_parvidens<-spTransform(S_parvidens, proj4string(mexicomammalstack))
S_parvidensR <- rasterize(S_parvidens, mexicomammalstack, field="BINOMIAL", fun="count") 
plot(S_parvidensR) 
writeRaster(S_parvidensR,file.path('~/Mexican-mammal-phylogenetic-diversity/MexicoMammalsRaster/Sturnira.parvidens.tif'),format='GTiff',overwrite=T) #save it

O_atricapillus <- readOGR(dsn="~/Mexican-mammal-phylogenetic-diversity/Otospermophilus.atricapillus/Otospermophilus.atricapillus.shp", encoding=NULL, use_iconv=TRUE)
O_atricapillus<-spTransform(O_atricapillus, proj4string(mexicomammalstack))
O_atricapillusR <- rasterize(O_atricapillus, mexicomammalstack, field="BINOMIAL", fun="count") 
plot(O_atricapillusR) 
writeRaster(O_atricapillusR,file.path('~/Mexican-mammal-phylogenetic-diversity/MexicoMammalsRaster/Otospermophilus.atricapillus.tif'),format='GTiff',overwrite=T) #save it


#ADDING RASTER FILES FROM TANIA (BIOGEOGRAPHIC ATLAS PROJECT)
bison <- raster("~/Mexican-mammal-phylogenetic-diversity/bisobis_f/w001001.adf") # -> class raster
crs(bison)<- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
writeRaster(bison, file.path('~/Mexican-mammal-phylogenetic-diversity/MexicoMammalsRaster/Bison.bison.tif'))
bison<- projectRaster(bison,mexicomammalstack)#resample 

Clupus <- raster("~/Mexican-mammal-phylogenetic-diversity/canlup_f/w001001.adf") # -> class raster
crs(Clupus)<- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
writeRaster(Clupus, file.path('~/Mexican-mammal-phylogenetic-diversity/MexicoMammalsRaster/Canis.lupus.tif'))
Clupus<- projectRaster(Clupus,mexicomammalstack)#resample



#Mexico outline
mexico<-getData('GADM',country='MEX',level=0)
mexico
plot(mexico)

mexalt<-getData('alt',country='MEX')

#Read in raster files
filelist<-list.files('MexicoMammalsRaster',full.names = T)
filelist

#Stack up
mexicomammalstack<-stack(filelist)
mexicomammalstack
plot(mexicomammalstack)

plot(mexicomammalstack[[1]])
plot(mexicomammalstack$Canis.lupus)

mexicoEA<-spTransform(mexico,crs(mexicomammalstack))
plot(mexicoEA,add=T)

mexicomammalstackcrop<-crop(mexicomammalstack,mexicoEA)
mexicomammalstackcrop[is.na(mexicomammalstackcrop)]<-0
########## KRISTIANS TEST AREA###############



########## END OF KRISTIANS TEST AREA###############

#Species richness
mexicomammalstack_10km<-aggregate(mexicomammalstack,fact=10,fun="modal")
writeRaster(mexicomammalstack_10km, 'mexicomammalstack10km.grd',format='raster',overwrite=T)#save file 2 use
mexicomammalstack_10km<- brick('mexicomammalstack10km.grd') # load file to use
writeRaster(mexicomammalstack_10km, filename='mexicomammalstack_10km/mexicomammalstack_10km.grd',format='raster',bylayer=TRUE) # save files to github
#filelist_test<-list.files('mexicomammalstack_10km',full.names = T,pattern="*.grd")
#test<-brick(stack(filelist_test))

mexmam_sr10km<-sum(mexicomammalstack_10km,na.rm = T)
mexmam_sr10km_m<-mask(mexmam_sr10km,mexicoEA)

levelplot(mexmam_sr10km_m,margin=F,main='Mammal Species Richness')+
  layer(sp.polygons(mexico,lwd=0.5))

writeRaster(mexmam_sr10km_m,'MexicoMammalSpeciesRichness_10km.tif',format='GTiff',overwrite=T)
summary(mexmam_sr10km_m)#1-121 species



#Loading files --------------------------------------------------------------

#PA
protected_areas <- readOGR(dsn="~/Mexican-mammal-phylogenetic-diversity/AP_FED.shp", encoding=NULL, use_iconv=TRUE)
plot(protected_areas) #Only federal
PA_E_P <- readOGR(dsn="~/Mexican-mammal-phylogenetic-diversity/ANP_EST_PRIV/AP_EST.shp", encoding=NULL, use_iconv=TRUE)
plot(PA_E_P) #Estatales y privadas  
ALLPA <- readOGR(dsn="~/NEW/Mexican-mammal-phylogenetic-diversity/UNION_ALL_AP/ALL_AP_MX.shp", encoding=NULL, use_iconv=TRUE)
plot(ALLPA) #This is all federal, private and statal Protected Areas

ALLPA3<-spTransform(ALLPA,crs(mexicomammalstack)) # protected areas with crs of mexicomammalstack

#FVT
FVT <- readOGR(dsn="~/Mexican-mammal-phylogenetic-diversity/FVT/FVT_new.shp", encoding=NULL, use_iconv=TRUE)
plot(FVT)

#División Política ###NO CARGA
DivPol <- readOGR(dsn="~/Mexican-mammal-phylogenetic-diversity/division_política/destdv250k_2gw.shp", encoding=NULL, use_iconv=TRUE)
plot(DivPol)
DivPol_lcc <- spTransform(DivPol, lcc)

#ZTM
ZTM <- readOGR(dsn="~/Mexican-mammal-phylogenetic-diversity/ZTM/ztm0.shp", encoding=NULL, use_iconv=TRUE)
plot(ZTM)



#Reprojecting to Datum=ITRF 92. All AP, FVT and SR. proj=Lambert conformal conic
#Not sure if lcc has correct Datum. 
lcc <- CRS("+proj=lcc +lat_1=17.5 +lat_2=19.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
PA_lcc <- spTransform(protected_areas, lcc) # protected areas with Lambert conformal conic (LCC)
PA_EP_lcc <- spTransform(PA_E_P, lcc)
ALLPA_lcc <- spTransform(ALLPA, lcc)
FVT_lcc <- spTransform(FVT, lcc)



#we were not sure how to reproject the raster --> with project raster
new2 <- spTransform(protected_areas, proj4string(mexicomammalstack_10km)) # protected areas with raster CRS
FVT2 <- spTransform(FVT_lcc, proj4string(mexicomammalstack_10km))
ALLPA2 <- spTransform(ALLPA, proj4string(mexicomammalstack_10km))


# Plot
image(mexmam_sr10km_m) # plot raster
plot(new2, add=TRUE) # add vector on top
plot(FVT2, add=TRUE)
plot(ALLPA2, add=TRUE)





#### rasterize(polygon,stack_of_other_species_ranges,field=”Species”,…)
#Assuming you have many species in one polygon dataset. Separate lines for each species if they are separate. You can set the file name as you do this.


#reprojecting SR raster
#SR_lcc is the name of the SR raster file with the correct projection
SR_lcc<-projectRaster(mexicomammalstack,crs = lcc)



# #Phylogeny --------------------------------------------------------------

#Read in phylogeny
install.packages("picante")
require(picante)
phylogeny<-read.tree('TREE_R_BN.nwk')
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

#Convert raster stack to community dataframe
communitydata<- getValues(mexicomammalstack_10km)

#Replace NA with 0
communitydata[is.na(communitydata)]<-0

#Use picante to trim community and phylogenetic data
phydata<-match.phylo.comm(phylogeny,communitydata)

###
##Check through the dropped species carefully and fix any that are errors.###
###Checked, No errors, extra species in phylogeny should be dropped.
##Locate spatial data for those that are missing
###

#Calculate phylogenetic diversity
phydiv<-pd(phydata$comm,phydata$phy,include.root=T)

#Rasterize this
phydivraster<-raster(mexmam_sr10km_m)
phydivraster<-setValues(phydivraster,phydiv$PD)
phydivraster<-mask(phydivraster,mexmam_sr10km_m,maskvalue=NA)

levelplot(phydivraster,par.settings=YlOrRdTheme,margin=F, main='Mammal Phylogenetic Diversity',scales=list(draw=F))+
   layer(sp.polygons(mexicoEA,lwd=0.5))
#  layer(sp.polygons(bPolslaea))+
#layer(sp.polygons(mexico,lty=2))

   
writeRaster(phydivraster,'MEX_MML_PD.tif',format='GTiff',overwrite=T)


#Stack together - each as a proportion of the total species richness or phylogeney branch length
diversitystack<-stack(mexmam_sr10km_m/nlayers(mexicomammalstack_10km),phydivraster/sum(phylogeny$edge.length))
names(diversitystack)<-c('Species Richness','Phylogenetic Diversity')

levelplot(diversitystack_lcc,par.settings=YlOrRdTheme,margin=F,main= "Diversity Measurements", scales=list(draw=F))+
  layer(sp.polygons(mexicoEA1,lwd=0.5))#+ #Mapa de México
  #layer(sp.polygons(ALLPA_ToUse, lwd=0.5))+ #Todas las AP
  layer(sp.polygons(DivPol_lcc, lwd=0.5)) #+ #División Política de México
  # layer(sp.polygons(ZTM, lwd=0.5)) #ZTM

ZTM_lcc <- project(ZTM, "+proj=lcc +lat_1=17.5 +lat_2=19.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

ALLPA_ToUse<-spTransform(ALLPA,crs(lcc))
plot(ALLPA_ToUse,add=T)



##REPROJECTING TO LCC EVERYTHING: SR & PD
SR_lcc<-projectRaster(mexmam_sr10km_m,crs = lcc)
PD_lcc<-projectRaster(phydivraster,crs = lcc)
diversitystack_lcc<-projectRaster(diversitystack,crs = lcc)




 ##########DIVERSITY INISDE AND OUTSIDE PA-------------------------------------------

#SR in PA
SRiPA<-mask(SR_lcc,ALLPA_ToUse) 
levelplot(SRiPA)

levelplot(SRiPA,par.settings=YlOrRdTheme,margin=F, main='Species Richness in Protected Areas',scales=list(draw=F))+
  layer(sp.polygons(mexicoEA1,lwd=0.5))+
  layer(sp.polygons(ALLPA_ToUse,lwd=0.5))
#BOXPLOT OF SRiPA 
boxplot(SRiPA, y=NULL, maxpixels=100000, col='blue', xlab='InsidePA', ylab='SR')

#SR out PA
SRnotPA<-mask(SR_lcc,ALLPA_ToUse, inverse='T')
levelplot(SRnotPA)
levelplot(SRnotPA,par.settings=YlOrRdTheme,margin=F, main='Species Richness outside Protected Areas',scales=list(draw=F))+
  layer(sp.polygons(mexicoEA1,lwd=0.5))+
  layer(sp.polygons(ALLPA_ToUse,lwd=0.5))
#BOXPLOT OF SRnotPA 
boxplot(SR_i_not, y=NULL, maxpixels=100000, col='blue', xlab='OutsidePA', ylab='SR')

#violin boxplot
SR_i_not <- stack(SRiPA, SRnotPA)
names(SR_i_not)<-c('Protected areas','Unprotected areas')
bwplot(SR_i_not, violin = TRUE, box.ratio = 0.5, par.settings = myTheme, ylab='Species Richness', main= 'Species Richness inside/outside PA')


#PD in PA
PDiPA<-mask(PD_lcc,ALLPA_ToUse)
levelplot(PDiPA)
levelplot(PDiPA,par.settings=YlOrRdTheme, margin=F,main='Phylogenetic Diversity in Protected Areas')+
  layer(sp.polygons(ALLPA_ToUse,lwd=0.5))+
  layer(sp.polygons(mexicoEA1,lwd=0.5))

#BOXPLOT OF PDiPA 
boxplot(PDiPA, y=NULL, maxpixels=100000, col='RED', xlab='InsidePA', ylab='PD')

#PD out PA
PDnotPA<-mask(PD_lcc,ALLPA_ToUse, inverse='T')
levelplot(PDnotPA)
levelplot(PDnotPA,par.settings=YlOrRdTheme,margin=F, main='Phylogenetic Diversity outside Protected Areas',scales=list(draw=F))+
  layer(sp.polygons(mexicoEA1,lwd=0.5))+
  layer(sp.polygons(ALLPA_ToUse,lwd=0.5))
#BOXPLOT OF PDoutPA 
boxplot(PD_i_not, y=NULL, maxpixels=100000, col='blue', xlab='OutsidePA', ylab='SR')

#violin boxplot
PD_i_not <- stack(PDiPA, PDnotPA)
names(PD_i_not)<-c('Protected areas','Unprotected areas')
bwplot(PD_i_not, violin = TRUE, box.ratio = 0.5, par.settings = myTheme, ylab='Pylogenetic diversity', main= 'Phylogenetic Diversity inside/outside PA')







####-----------------------------Basic Raster Descriptive stats-------------####
###QUESESTO????

#SR
cellStats(SRiPA, stat='mean', na.rm=TRUE, asSample=TRUE)
cellStats(SRiPA, stat='max', na.rm=TRUE, asSample=TRUE)
cellStats(SRiPA, stat='min', na.rm=TRUE, asSample=TRUE)

#PD
cellStats(PDiPA, stat='mean', na.rm=TRUE, asSample=TRUE)
cellStats(PDiPA, stat='max', na.rm=TRUE, asSample=TRUE)
cellStats(PDiPA, stat='min', na.rm=TRUE, asSample=TRUE)


#### to obtain number of cells, subtract NA cell count from total sum of count

srfrequency <- data.frame(freq(SR_lcc))

###TO DO
######To find out which non-protected cells have the greatest phylogenetic distance from-
######all protected areas within Mexico, you could make a new dataset where all of Mexico’s-
######protected areas are merged into a single community, and look at the phylosor similarity-
######between that community and every other cell (assuming that there are several  species-
######which are not present in any protected areas).

#HOW MANY SP. DO I HAVE INSIDE AND HOW MANY OUTSIDE PA?
# SP. REDUNDANCY.... how many times a sp./branch tip is protected in the total set of PA
#B-DIVERSITY (PHYLOSOR). turnover between PA-nonPA
#COMAPRE DIFFERENT TYPES OF DIVERSITY BETWEEN DIFFERENT TYPES OF PA (FEDERA, ESTATAL, PRIVADA)
#HOW MUCH OF THE UNPROTECTED AREA WILL BE AHOTSPOT-Most valuable areas to add to current PA scheme
#RANK PA BASED ON PD - must important PA for protecting mammal PD

#PA RASTER
PA_R <- raster("~/NEW/Mexican-mammal-phylogenetic-diversity/PA_MX_Raster.tif")
plot(PA_R)
PA_R<- projectRaster(PA_R,mexicomammalstack_10km)

PA_R[getValues(PA_R)>0]<-1 # set "buffer" values to 1
sum(getValues(PA_R))
PAnMmlStack<- stack(mexicomammalstack_10km, PA_R) # I cant find the PA column.... just the names of the sp and at then sth clle "PA_MX_raster"

PAnMmlStack[is.na(PAnMmlStack)]<-0 # swap NA with 0 :)



dataPAnMmlStack<-getValues(PAnMmlStack)



