
#
#### R code for phd chapter for Mutali Winnie Netshanzhe =========
### Paper authors
## Mutali Winnie Netshanzhe
## Corrie Maria Swanepoel
## Alan J Gardiner
## Lourens Hendrik Swanepoel

# Using presence-only modelling to quantify the 
# suitable habitat of Gonimbrasia belina, 
# a communally exploited edible insect, 
# in southern African mopane (Colophospermum mopane) Savannah


### Help from
### https://damariszurell.github.io/EEC-MGC/b7_SDM_ensembles.html


### Load packages
library(dplyr)
library(rJava)

library(rgdal)
library(maps)
library(mapdata)
library(dismo)  
  
library(maptools)
library(jsonlite)
library(usdm)
library(ggplot2)

# data access
library(raster)
library(sp)
library(geodata)
library(ragg)

## Get the climate data - do this the first time
library(geodata)
country_codes(query=NULL)
getOption("max.print")

currentEnv=raster::getData("worldclim", var="bio", res=2.5)
plot(currentEnv)

### Get climatic variables based on Literature and biological hypothesis
### Six bioclimatic variables were used: 
### mean annual temperature BIO1
### max temperature of the warmest month BIO5
### minimum temperature of the coldest month BIO6
### annual precipitation BIO12
### precipitation of the driest month BIO14
### precipitation of the wettest month BIO13


### Drop Climate variables not used

currentEnv=dropLayer(currentEnv, c("bio2",
                                   "bio3",
                                   "bio4",
                                   "bio7",
                                   "bio8",
                                   "bio9",
                                   "bio10",
                                   "bio11",
                                   "bio15",
                                   "bio16",
                                   "bio17",
                                   "bio18",
                                   "bio19"))

## get soil data and add to worlclim data = foord stat here
## Soil data comes from https://www.isric.org/instruction-wms
## follow the help file and then download it in qgis
## Then extract for the Mopane worm Africa layer shp
## Then load the tif file from your local folder

#library(tiff)

# set name of the tif file
#clay<-'clay2.tif' 
# load with raster package
clay=raster(clay)
raster::plot(clay)
## Do this when you have downloaded the climatic data already
## Set path to the climate data on your computer, use the wc5 dataset

#use this when opening project file
fnames.l <- list.files(path=paste(getwd(),"/wc5",sep =""), 
                      pattern='bil', full.names=TRUE)
## Make them into a stack

currentEnv <- stack(fnames.l)


### Crop the variable to the same extent
### Get county polygons
library(maptools)
data(wrld_simpl)
plot(wrld_simpl, axes=TRUE,
     col="light yellow") #

### Select only for African countries with mopane worms 
### got these names if you plot the GBIF data

countrydf <- data.frame(country = c('South Africa', 
                                        'Zimbabwe', 
                                        'Botswana', 
                                        'Namibia', 
                                        'Zambia', 
                                        'Mozambique', 
                                        'Malawi',
                                        'Kenya',
                                        'United Republic of Tanzania',
                                        'Lesotho',
                                        'Swaziland'))

### drop the rest and keep the list above

sampl <- subset(wrld_simpl, NAME %in% countrydf$country)
plot(sampl)

## Export this shp to qgis to crop the soil layer as well

writeOGR(sampl, dsn = '.', layer = 'sampl4', driver = "ESRI Shapefile")
plot(currentEnv)
### Get the files into similar region
### First crop, then mask
currentEnv <- raster::crop(currentEnv,sampl)
currentEnv <- mask(currentEnv,sampl)

plot(currentEnv)
class(currentEnv)
print(currentEnv)
# List the names of the layers if applicable
if (class(currentEnv) %in% c("RasterStack", "RasterBrick")) {
  print(names(currentEnv))
}
## Since the extent will give issues, resample the clay to the 
## climate data - use terra package as it is faster

names(clay)<- "clay"

currentEnv <- terra::resample(currentEnv, clay, method="bilinear")
## add the clay layer to the env data
currentEnv <- addLayer(currentEnv,clay)
raster::plot(currentEnv)

## Export the raster brick to file if possible to speed up processing in future
library(terra)
terra::writeRaster(currentEnv,"currentEnv2.tif", overwrite=TRUE)

currentEnv.3 <-raster::stack("currentEnv2.tif")

plot(currentEnv.3)
### check for colinarity in variables
library(usdm)

ex <- raster::extract(currentEnv,dat1)
v  <-vifstep(ex)

# Convert the extracted data to a data frame
ex_df <- as.data.frame(ex)

# Calculate and display the VIF values for all variables before any removal
vif_values_before <- vif(ex_df)
print(vif_values_before)

# Now check for multicollinearity and remove variables with high VIF
v <- vifstep(ex_df)

# Get the VIF values for all variables
vif_values <- vif(ex)
print(vif_values)

## remove the correlated variables

currentEnv.1 <- exclude(currentEnv, v)
raster::plot(currentEnv.1)


## Get mopane worm data from GBIF
library(rgbif)
library(dplyr)

# Download GBIF data for a species
gbif_data <- occ_search(scientificName = "Gonimbrasia belina", 
                        limit = 2000, 
                        hasCoordinate = TRUE, 
                        hasGeospatialIssue = FALSE)

# Convert data to a data frame
gbif_df <- gbif_data$data

# Filter data for research-grade quality
# Example criteria:
# 1. No geospatial issues
# 2. Coordinates present
# 3. Identification confidence (if available)
 
filtered_data <-gbif_df %>%
  filter(
    !is.na(decimalLatitude),                          # Ensure latitude is present
    !is.na(decimalLongitude),                         # Ensure longitude is present
    !basisOfRecord %in% c("FOSSIL_SPECIMEN"),          # Exclude fossils
    taxonRank == "SPECIES",                           # Only include species-level records
    protocol %in% c("DWC_ARCHIVE", "EML", "BIOCASE"),   # Only include records with DWC_ARCHIVE protocol
    as.numeric(format(as.Date(eventDate), "%Y")) < 2024  # Include records before 2023 based on eventDate
  )

## Plot the worm data

ggplot(filtered_data, aes(x =decimalLongitude , y =decimalLatitude )) +
  coord_quickmap() +
  geom_point()


## Human observation
table(mw$basisOfRecord[mw$datasetKey ==
                         "50c9509d-22c7-4a22-a47d-8c48425ef4a7"]) 



# plot worm data on Africa map
windows()
plot(sampl, axes = T)

points(filtered_data$decimalLongitude,
       filtered_data$decimalLatitude, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
points(filtered_data$decimalLongitude,
       filtered_data$decimalLatitude, col='red', cex=0.75)

### Rename filtered data to dat
#dat1 <- filtered_data

## Get data ready form model
# Get the worm data and drop all the NA data 
#dat1 <- dplyr::filter(dat,!is.na(long))
#dat1 <- dplyr::filter(dat,!is.na(Latitude))
#dat1 <- dplyr::select(dat1,long,Latitude)

# make the spatial points 
coordinates(dat1) <- ~long+Latitude
# set the coordination system from worm data to the world map
crs(dat1) <- crs(wrld_simpl)
plot(dat1)

## Fitting ensembe models
## first off try to reduce resolution by a factor of five - 
## hopefully it speeds up the analysis
predictors <- aggregate(currentEnv, fact=5) 
plot(predictors)

## rename the caterpillar data
occ <- dat1
library(tidyr)
occ = occ %>%
  drop_na(long, Latitude) %>% 
  select(long, Latitude)

## Ensemble model=====
countrydf.enm <- data.frame(country = c('South Africa', 
                                    'Zimbabwe', 
                                    'Botswana', 
                                    'Namibia', 
                                    'Zambia', 
                                    'Mozambique', 
                                    'Malawi',
                                    'Kenya',
                                    'United Republic of Tanzania',
                                    'Lesotho',
                                    'Swaziland'))
#drop the rest and keep the list above

sampl.enm <- subset(wrld_simpl , NAME %in% countrydf.enm$country)
plot(sampl.enm)
pred.1 <- crop(currentEnv,sampl)
pred.2 <- mask(pred.1,sampl)

plot(currentEnv)

### create pseudo-absences
bg <- randomPoints(currentEnv, 1000)
colnames(bg) <- c("lon","lat")
bg_shape <- as.data.frame(bg)
coordinates(bg_shape)<- ~lon+lat
species_abs <- cbind.data.frame(bg,0)
colnames(species_abs)[3] <- "occurrence"
species_pres <- cbind.data.frame(dat1,1)
#species_pres <- cbind.data.frame(pts4@coords,1) 

colnames(species_pres)[3] <- "occurrence"
colnames(species_pres)[1] <- "lon"
colnames(species_pres)[2] <- "lat"
test <- rbind.data.frame(species_pres,species_abs)
coordinates(test) <- ~lon+lat

plot(test)
## Fit ths sdm models
library(maptools)
library(sdm)
# prepare the sdm data
d <- sdmData(occurrence~., train = test,predictors = currentEnv)

## SDMs were made using three algorithms: Bioclimate Envelopes
##(BIOCLIM), Generalised Linear Models (GLM),
## and Random Forests (RF).
## The habitat suitability probability in each cell across all three
## algorithms was averaged and weighted by each algorithm’s
## calculated area under the received operator curve
## statistic (AUC) obtained from 3-fold cross validation. The
## AUC represents how well the output of each algorithm
## matches the original occurrence data, with higher scores
## representing better model performance.


### This is to check from 5 modelling algorithms
m1 <- sdm(occurrence~.,
          data=d,
          methods=c('gam','glm','maxent','rf', 'bioclim'),
          replication='cv',
          cv.folds=5,
          n=5) 

### This is code to generate the 3 best algorithms
m3 <- sdm(occurrence~.,
          data=d,
          methods=c('maxent','gam','rf'),
          replication='cv',
          cv.folds=5,
          n=5) 

getModelInfo(m3)

roc(m3)

p1 <- predict(m2,newdata=predictors, overwrite=TRUE)

### Generate the ensemble model

e1 <- ensemble(m3,newdata=currentEnv,
 
                             setting=list(method='weighted',stat='AUC'))
plot(e1)
terra::writeRaster(e1,"e1.tif", overwrite=TRUE)

# set name of the tif file
e1<-'e1.tif' 
e1=raster(e1)
raster::plot(e1)

median(area(e1)[])

## Methods to get threshold for suitable and not suitable
## evaluate ensemble model
## As well as to evaluate the model
## https://www.biogeoinformatics.org/
## Get variable importance

getModelId(m3)
getModelInfo(m3)

vi.em3 <- getVarImp(e1,id="ensemble",setting=list(method='weighted',stat='auc'))

rf.var <- getVarImp(m3,method='rf')
gam.var <- getVarImp(m3,method='gam')
max.var <- getVarImp(m3,method='maxent')
mean.var <- getVarImp(m3)

str(rf.var)

rf.var@varImportanceMean$corTest


df.rf <- as.data.frame(rf.var@varImportanceMean$corTest)
df.rf$model <- "RF"
df.gam <- as.data.frame(gam.var@varImportanceMean$corTest)
df.gam$model <- "GAM"
df.max <- as.data.frame(max.var@varImportanceMean$corTest)
df.max$model <- "MAXENT"
df.mean <- as.data.frame(mean.var@varImportanceMean$corTest)
df.mean$model <- "Mean response"
df.rf.auc <- as.data.frame(rf.var@varImportanceMean$AUCtest)
df.rf.auc$model <- "RF"
df.gam.auc <- as.data.frame(gam.var@varImportanceMean$AUCtest)
df.gam.auc$model <- "GAM"
df.max.auc <- as.data.frame(max.var@varImportanceMean$AUCtest)
df.max.auc$model <- "MAXENT"
df.mean.auc <- as.data.frame(mean.var@varImportanceMean$AUCtest)
df.mean.auc$model <- "Mean response"

library(ggplot2)
all.df <- rbind(df.rf,df.gam,df.max, df.mean)

# Load the RColorBrewer package
library(RColorBrewer)

# Define a colorblind-friendly palette using RColorBrewer
cb_palette <- brewer.pal(n = 8, name = "Set1")

# Create a new variable with the ranking of 'variables' from large to small
all.df <- all.df %>%
  mutate(variables_ranked = reorder(variables, -corTest))


# Modify your ggplot code to use the colorblind-friendly palette
Fig2A <- ggplot(all.df, aes(variables_ranked, corTest, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_classic() +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.4, position = position_dodge(.9)) +
  scale_fill_manual(values = cb_palette) +  # Use the colorblind-friendly palette
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"),
        panel.background = element_rect(fill = "White", colour = "White", size = 0),
        legend.position = "top",
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("Relative variable importance")

### Fig 2B

all.df.auc <- rbind(df.rf.auc,df.gam.auc,df.max.auc, df.mean.auc)

# Create a new variable with the ranking of 'variables' from large to small
all.df.auc <- all.df.auc %>%
  mutate(variables_ranked = reorder(variables, -AUCtest))


Fig2B <- ggplot(all.df.auc, aes(variables_ranked, AUCtest, fill=model))+
  geom_bar(stat="identity", position = "dodge")+ 
  theme_classic() +
  geom_errorbar(aes(ymin = lower, ymax =upper), 
                width=0.4, position = position_dodge(.9)) +
  scale_fill_manual(values = cb_palette) + 
  theme (axis.text=element_text(size=15),
         axis.title=element_text(size=15,face="bold"),
         panel.background = element_rect(fill = "White", colour = "White", size = 0))+
  theme(legend.position = "top")+
  xlab("")+
  ylab("Relative variable importance")+
  theme(axis.text.x=element_text(angle=90,hjust=1))

# Combine to one

library(patchwork)


Fig2 <-(Fig2A | Fig2B) +
  plot_annotation(tag_levels = "A") &
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10), 
    legend.key.size = unit(0.5, "lines"),  # Reduces legend box size
    plot.tag = element_text(face = "bold", size = 10),
    strip.text = element_text(size = 12))
  

ggsave("Fig2_1800px.png", 
       plot = Fig2, 
       device = "png",  
       width = 7,     # 1800 pixels at 300 DPI
       height = 4,    # Adjust height as per aspect ratio
       dpi = 600)     # High resolution



## Plot response curves from the different models
windows()

rc.max <- rcurve(m3, id=1:25)
rc.max.df <- rc.max$data
rc.max.df$model <- "MAXENT"
rc.rf <- rcurve(m3, id=51:75)
rc.rf.df <- rc.rf$data
rc.rf.df$model <- "RF"
rc.gam <- rcurve(m3, id=26:50)
rc.gam.df <- rc.gam$data
rc.gam.df$model <- "GAM"

rc.all <-as.data.frame(rbind(rc.max.df, rc.rf.df, rc.gam.df))


Fig3 <- ggplot(rc.all, aes(x = Value, y = Response, group = model)) +
  geom_line(aes(color = model), linetype = "solid", size = 0.5, color = "black") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5,
              fill = "grey70",
              color = "black",
              linetype = "solid") +
  facet_grid(model ~ variable, scales = "free") +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15, face = "bold")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("") +  # Optional: Remove x-axis label if not needed
  ylab("Response")  # Optional: Set y-axis label


Fig3 <- (Fig3 )+
  plot_annotation(tag_levels="A") &
  theme(axis.title = element_text(size = 12)) &
  theme(legend.text = element_text(size = 8))&
  theme(plot.tag = element_text(face = 'bold', size = 10))&
  theme(axis.text.x = element_text(size = 10)) &
  theme(axis.text.y = element_text(size = 10)) &
  theme(axis.text = element_text(size = 12)) &
  theme (strip.text = element_text(size = 12))

ggsave("Fig3_1800px.png", 
       plot = Fig3, 
       device = "png",  
       width = 7,     # 1800 pixels at 300 DPI
       height = 4,    # Adjust height as per aspect ratio
       dpi = 600)     # High resolution

## Get threshold based evaluation of ensemble model
df <- as.data.frame(d)
df <- data.frame(species=df$occurrence,coordinates(d))
xy <- as.matrix(df[,c('lon','lat')])
p <- raster::extract(e1,xy)

ev <- evaluates(df$species,p)
ev@statistics
th <- ev@threshold_based$threshold[2]

ev@threshold_based

getEvaluation(m3,opt=2)

## Plot raster 
pa1 <- raster(e1)
## Plot 1 and 0; so all above threshold = 1 and others is 0; binary map
pa1[] <- ifelse(e1[] >= th, 1, 0)
plot(pa1)

#other ways to plot the ensemble model
e2_df <- as.data.frame(e1, xy = TRUE)

str(e2_df)
library(tidyr)
library(dplyr)
## Drop NA data
e2_df = drop_na(e2_df) 

## Bin the suitablity values, just for interest
e2_df <- e2_df %>%
  mutate(fct_prev = cut(e2_df$layer, breaks = 3))

ggplot() +
  geom_bar(data = e2_df, aes(fct_prev))

## Use custom bins to show suitability scores. Lowest value will be the threshold based on the ensemble
## model
custom_bins <- c(0.32, 0.41, 0.61, 1)

## Use ggplot to plot the ensemble model, based on the bins
library(forcats)
e3_df <- e2_df %>%
  mutate(fct_prev_2 = cut(e2_df$layer, breaks = custom_bins)) %>% 
  mutate(across(where(is.numeric),~replace_na(.,0)),
         across(where(is.factor),~fct_explicit_na(.,'0')))

 ggplot() +
  geom_raster(data = e3_df , aes(x = x, y = y, fill = fct_prev_2)) + 
  coord_quickmap()

## Use different color schema
terrain.colors(4)

ggplot() +
  geom_raster(data = e3_df , aes(x = x, y = y,
                                       fill = fct_prev_2)) + 
  scale_fill_manual(values = terrain.colors(4)) + 
  coord_quickmap() + xlab("Longitude") + ylab("latitude")

#### ESTIMATE SUITABLE HABITAT ========

## Estimate suitable habitat per country for the full ensemble model first
## Add this as a supplementary file
## African countries
## e1 is the ensemble model
## extract the hsi score for each pixel and add to the polygon
sampl$hsi <- raster::extract(e1, sampl)
## Convert to the extracted data to a data frame
afr.table <- as.data.frame(sampl@data)
## Since each country has multiple hsi scores, and they are stored as a list
## in the data frame, you need to un-nest them first
library(tidyr)
library(purrr)
afr.table <-unnest(afr.table, hsi)
## Then drop the NA values
## Use the custom bins ## Suitable and not suitable bins
bins.1<-c(0,0.32, 1)

#data.frame(cut(afr.table$hsi, breaks = c(0, 0.35, 1)) %>% table)

## How much suitable is predicted overall
afr.table %>%
  group_by(group = cut(hsi, breaks = c(0, 0.35, 1))) %>%
  na.omit %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n)) %>% 
  mutate (area = n*median(area(e1)[]))

Supplementtable1 = afr.table %>% 
  drop_na(hsi) %>% 
  mutate(km2=median(area(e1)[])) %>% 
  mutate(hsi_binned = cut(hsi, bins.1, labels = c(0,1))) %>% 
  group_by(NAME,hsi_binned) %>%
  filter(hsi_binned == 1) %>% 
  group_by(NAME, AREA) %>% 
  summarise(C.AREA = sum(km2)) %>% 
  mutate(Prop.country = (C.AREA/(AREA*10))*100) %>% 
  mutate(AREA = AREA*10) %>% 
  cbind(
    afr.table %>%
      mutate(hsi_binned = cut(hsi, bins.1, labels = c(0,1))) %>%
      group_by(NAME,hsi_binned) %>%
      filter(hsi_binned == 1) %>% 
      summarise(hsi_binned = n(), .groups = 'drop') %>%
      mutate(prop.all = (hsi_binned / sum(hsi_binned))*100) %>% 
      dplyr::select(prop.all))

### RESTRICTED MODEL to MOPANE TREES =============
## Take the ensemble model and extract the areas were they intersect
## We are only interested in areas where mopane worms and trees occur
## since these areas are most likely to have outbreak areas
## mopane tree model is taken from 
## Ngarega et al  "Forecasting the effects of bioclimatic characteristics and climate 
## change on the potential distribution of Colophospermum mopane in southern Africa 
## using Maximum Entropy (Maxent)." Ecological Informatics 65 (2021): 101419

### Get the Mopane tree Maxent model
### 
library(tiff)
library(raster)
# set name of the tif file
trees<-'Colophospermum_mopane_avg_binary.tif' # Ask for tif from authors
# load with raster package

trees=raster(trees)
plot(trees)
## mask the tree model to the study area
x <- mask(trees, sampl)
## resample the tree model to the ensemble model; to get into similar extent etc
trees.2 <- terra::resample(x, e1, method="bilinear")

plot(trees.2)
pol <- rasterToPolygons(trees.2, fun=function(x){x>0.2},dissolve = TRUE )

### Restrict ensemble model to mopane tree maxent model
e1.1 <- mask(e1, pol)

plot(e1.1)
terra::writeRaster(e1.1,"e1.1.tif", overwrite=TRUE)
#terra::writeRaster(e3.1,"e3.1.tif", overwrite=TRUE)


cell_size<-raster::area(e1.1, na.rm=TRUE, weights=FALSE)

cell_size@data

median(cell_size@data@values)

r.e.df.1 <- as.data.frame(e1.1, xy = TRUE, na.rm=TRUE)


## Remove the NA data
r.e.df.1 = r.e.df.1 %>%
  tidyr::drop_na(e1) 

r.e.df.1 <- r.e.df.1 %>%
  mutate(fct_prev = cut(e1, breaks = 3))

ggplot() +
  geom_bar(data = r.e.df.1, aes(fct_prev))

custom_bins <- c(0.20, 0.41, 0.61, 1)

## This is now to plot the ensemble model once you cropped it to the tree layer
library(tidyverse)
library(ggplot2)
r.e.df.2 <- r.e.df.1 %>%
  mutate(fct_prev = cut(e1, breaks = custom_bins)) %>% 
  mutate(across(where(is.numeric),~replace_na(.,0)),
         across(where(is.factor),~fct_explicit_na(.,'0')))

ggplot() +
  geom_raster(data = r.e.df.2 , aes(x = x, y = y, fill = fct_prev)) + 
  coord_quickmap()

terrain.colors(4)
ggplot() +
  geom_raster(data = r.e.df.2 , aes(x = x, y = y,
                                 fill = fct_prev)) + 
  scale_fill_manual(values = terrain.colors(4)) + 
  coord_quickmap() + xlab("Longitude") + ylab("Latitude")


### Following code is to estimate suitable habitat in each country
### Conservation areas
### Tribal or communal land

### African countries

## e1.1 is the ensemble model, cropped to mopane tree layer

## extract the hsi score for each pixel and add to the polygon
sampl$hsi <- raster::extract(e1.1, sampl)
## Convert to the extracted data to a data frame
afr.table.re <- as.data.frame(sampl@data)
## Since each country has multiple hsi scores, and they are stored as a list
## in the data frame, you need to un-nest them first
library(tidyr)
library(purrr)
afr.table.re <-unnest(afr.table.re, hsi)
## Then drop the NA values
table1.re = afr.table.re %>% 
            drop_na(hsi) %>% 
            mutate(km2=median(area(e1.1)[])) %>% 
            mutate(hsi_binned = cut(hsi, bins.1, labels = c(0,1))) %>% 
            group_by(NAME,hsi_binned) %>%
            filter(hsi_binned == 1) %>% 
            group_by(NAME, AREA) %>% 
            summarise(C.AREA = sum(km2)) %>% 
            mutate(Prop.country = (C.AREA/(AREA*10))*100) %>% 
            mutate(AREA = AREA*10) %>% 
            cbind(
  afr.table.re %>%
  mutate(hsi_binned = cut(hsi, bins.1, labels = c(0,1))) %>%
  group_by(NAME,hsi_binned) %>%
  filter(hsi_binned == 1) %>% 
  summarise(hsi_binned = n(), .groups = 'drop') %>%
  mutate(prop.all = (hsi_binned / sum(hsi_binned))*100) %>% 
    dplyr::select(prop.all))

## How much suitable is predicted overall
afr.table.re %>%
  group_by(group = cut(hsi, breaks = c(0, 0.35, 1))) %>%
  na.omit %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n)) %>% 
  mutate (area = n*median(area(e1)[]))


# Combine for plotting

table1.re$model <- "Restricted"
Supplementtable1$model <- "Full Ensemble"

Tot.suit.hab <- rbind(table1.re,Supplementtable1)

write.csv(Tot.suit.hab, "Tot.suit.hab.csv")

ggplot(Tot.suit.hab, aes(x=reorder(NAME,-prop.all) , prop.all, fill=model))+
  geom_bar(stat="identity", position = "dodge")+ 
  theme_classic() +
  theme (axis.text=element_text(size=15),
         axis.title=element_text(size=15,face="bold"),
         panel.background = element_rect(fill = "White", colour = "White", size = 0))+
  theme(legend.position = "top")+
  xlab("")+
  ylab("% suitable mopane caterpillar habitat")+
  theme(axis.text.x=element_text(angle=90,hjust=1))



## 0.35 (the threshold) will be 0, others 1 
## in essence unsuitable vs suitable
## Then we group by country (NAME) and hsi_binned
## Then we filter out all the unsuitable habitat
## then count the number of suitable pix per country/total for all countries


afr.plot.en = afr.table.en %>%
  mutate(hsi_binned = cut(hsi, bins.1, labels = c(0,1))) %>%
  group_by(NAME,hsi_binned) %>%
  filter(hsi_binned == 1) %>% 
  summarise(hsi_binned = n(), .groups = 'drop') %>%
  mutate(share = hsi_binned / sum(hsi_binned)) %>% 
  ggplot(., aes(x = reorder(NAME, -share),  y =share)) +
  geom_bar(stat="identity") +
  theme_classic()+
  theme (axis.text=element_text(size=15),
         axis.title=element_text(size=15,face="bold"),
         panel.background = element_rect(fill = "White", colour = "White", size = 0))+
  theme(legend.position = "none")+
  xlab("")+
  ylab("% suitable mopane caterpillar habitat")+
  theme(axis.text.x=element_text(angle=90,hjust=1))

## Get for each protected area in SA
cons        <- readShapeSpatial("qq",delete_null_obj=TRUE)

cons$hsi    <- raster::extract(e1.1, cons)
cons.table  <- as.data.frame(cons@data)
library(tidyr)
library(purrr)
cons.table <-unnest(cons.table, hsi)
cons.table = cons.table %>% 
  drop_na(hsi)


cons.table %>%
  mutate(hsi_binned = cut(hsi, bins.1, labels = c(0,1))) %>%
  group_by(CONS,hsi_binned) %>%
  filter(hsi_binned == 1) %>% 
  summarise(hsi_binned = n(), .groups = 'drop') %>%
  mutate(share = hsi_binned / sum(hsi_binned)) %>% 
  ggplot(., aes(x = reorder(CONS, -share),  y =share)) +
  geom_bar(stat="identity") +
  theme_classic()+
  theme (axis.text=element_text(size=20),
         axis.title=element_text(size=20,face="bold"),
         panel.background = element_rect(fill = "White", colour = "White", size = 0))+
  theme(legend.position = "none")+
  xlab("")+
  ylab("% suitable mopane caterpillar habitat")+
  theme(axis.text.x=element_text(angle=90,hjust=1))


#### Some plots for presentation
m <- rbind(c(1, 2), c(3, 4), c(5,6))
layout(m)
# plot worm data on Africa map
plot(sampl, axes = T)

points(mw$lon, mw$lat, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
points(mw$lon, mw$lat, col='red', cex=0.75)

## Plot ensemble model over study area
library(terra)
e3.plot <- rast(e3)
r1 = terra::plot(e3.plot, fun=function()lines(sampl))


library("ggpubr")
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)
as.data.frame(dat1)
library(sf)
sampl_sf <- st_as_sf(sampl)
distr <- dplyr::select(dat1,lon,lat)
worm.plot =ggplot(sampl_sf) + geom_sf() +
 geom_point(aes(x = lon, y = lat),dat = as.data.frame(dat1)) +
  coord_sf()

ens.plpt = ggplot(sampl_sf) + geom_sf() +
  geom_raster(data = e2_df , aes(x = x, y = y,
                                 fill = fct_prev, alpha = 0.1
  )) +
  scale_fill_manual(values = terrain.colors(4)) + 
  coord_sf() + xlab("") + ylab("")

final.plot = ggplot(sampl_sf) + geom_sf() +
  geom_raster(data = r.e.df.1 , aes(x = x, y = y,
                                   fill = fct_prev, alpha = 0.1
                                   )) + 
  scale_fill_manual(values = terrain.colors(4)) + 
  coord_sf() + xlab("") + ylab("")

figure <- ggarrange(worm.plot, final.plot, ens.plpt,
                    labels = c("Points", "Final map", "Ensemble"),
                    ncol = 2, nrow = 2)


### Cons for all SADC
## Get for each protected area in SA
cons.sadc <- readShapeSpatial("sadc_cons.shp",delete_null_obj=TRUE)
plot(cons.sadc)
cons.sadc$hsi <- raster::extract(e1.1, cons.sadc)
cons.sadc.table <- as.data.frame(cons.sadc@data)
library(tidyr)
library(purrr)
cons.sadc.table <-unnest(cons.sadc.table, hsi)
cons.sadc.table = cons.sadc.table %>% 
  drop_na(hsi)

## Suitable and not suitable bins
bins.1<-c(0,0.20, 1)   

unique(cons.sadc.table$ISO3)

write.csv(cons.sadc.table, file = "cons.sadc.table.csv")

cons.sadc.table %>%
  dplyr::select(NAME, IUCN_CAT, ISO3, VERIF, hsi) %>% 
  mutate(country =
           case_when(ISO3 == "MOZ" ~ "Mozambique",
                     ISO3 == "ZAF" ~ "South Africa",
                     ISO3 == "ZWE" ~ "Zimbabwe",
                     ISO3 == "BWA" ~ "Botswana",
                     ISO3 == "NAM" ~ "Namibia",
                     ISO3 == "AGO" ~ "Angola")) %>% 
  filter(ISO3 %in% c("MOZ",
                     "ZAF",
                     "ZWE",
                     "BWA",
                     "NAM",
                     "AGO")) %>% 
  mutate(hsi_binned = cut(hsi,bins.1, labels = c(0,1))) %>%
  filter(hsi_binned == 1) %>% 
  group_by(ISO3,IUCN_CAT,hsi_binned) %>%
  summarise(hsi_binned = n(),.groups = 'drop')   %>%               
  full_join(afr.table %>%
                mutate(ISO3 =
                         case_when(NAME == "United Republic of Tanzania" ~ "TZA", 
                                   NAME == "Mozambique" ~ "MOZ",
                                   NAME == "South Africa" ~ "ZAF" ,
                                   NAME == "Zambia" ~"ZMB",
                                   NAME == "Zimbabwe" ~ "ZWE",
                                   NAME == "Malawi" ~ "MWI",
                                   NAME == "Botswana"~ "BWA",
                                   NAME == "Namibia"~ "NAM",
                                   NAME == "Angola" ~"AGO")) %>% 
                mutate(hsi_binned = cut(hsi, bins.1, labels = c(0,1))) %>%
                group_by(ISO3,hsi_binned) %>%
                filter(hsi_binned == 1) %>% 
                summarise(hsi_binned = n(), .groups = 'drop'), by = "ISO3") %>% 
  drop_na(hsi_binned.y) %>% 
  mutate(share = hsi_binned.x / hsi_binned.y) %>% 
  drop_na(share) %>% 
ggplot(., aes(x = reorder(IUCN_CAT, -share),  y =share)) +
  geom_bar(stat="identity") +
  facet_grid(~ISO3)+
  theme_classic()+
  theme (axis.text=element_text(size=20),
         axis.title=element_text(size=20,face="bold"),
         panel.background = element_rect(fill = "White", colour = "White", size = 0))+
  theme(legend.position = "none")+
  xlab("")+
  ylab("% suitable mopane caterpillar habitat")+
  theme(axis.text.x=element_text(angle=90,hjust=1))
 

### Plot per conservation status
### Get Excel file from Winnie - 30 June 2023

library(readxl)

library(RColorBrewer)

iucn = Conservation_table_June_2023

iucn = iucn %>% 
  mutate(Per = Prop_area*100) %>% 
  filter(Type !="Total")

# The palette with black:
# The palette with black:
cbp1 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
library(ggplot2)
reorder(CONS, -share)


ggplot(iucn, aes(fill=Type, x= reorder(Country, -Per), y=Per)) + 
  geom_bar(position="stack", stat="identity")+
  theme(legend.position = "top")+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic()+
  theme (axis.text=element_text(size=20),
         axis.title=element_text(size=20,face="bold"),
         panel.background = element_rect(fill = "White", colour = "White", size = 0))+
  theme(legend.position = "top")+
  xlab("")+
  ylab("% suitable mopane caterpillar habitat")+
  theme(axis.text.x=element_text(angle=90,hjust=1))

