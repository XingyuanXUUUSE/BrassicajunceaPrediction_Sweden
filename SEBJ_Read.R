df <- read.xlsx("Brassica.xlsx")

# In the default setting of the coding, you load your BIOdata here
BIO <- getData('worldclim', var='bio', res=2.5)

# Here you get the mask for your research area
data <- getData('GADM', country = 'SWE', level = 0)
plot(data, col = 'cyan3', main = "Sweden")

extent <- extent(9, 26, 54, 71)
# Consult for precise extent otherwise You will get bad map interpretation
mapraster <- raster(ext = extent, res = 2.5, crs = projection(BIO))

# If you have your customized raster data, please put that to the raster file in the workfile.

#
# And remember to crop your customized data here
BIO <- mask(crop(BIO, mapraster),data)

plot(BIO)

### HERE YOU GET THE SPECIES SPECIFIC DATA ###
### PLEASE CHECK THE XLSX FILE ###

juncea <- subset(df, df$speciesKey == 3042751)

### And then, you covert the dataframe to points

folds = 5

juncea.points <- SpatialPoints(juncea[, c("decimalLongitude", "decimalLatitude")])
proj4string(juncea.points) <- CRS("+proj=longlat +datum=WGS84")
junceaBIO <- extract(BIO, juncea.points)

cljuncea <- juncea[,c(35,3,4)]
names(cljuncea)[2:3] <- c('lat','lon')
cljuncea <- subset(cljuncea,(!is.na(lon) | !is.na(lat)) & (lon!= 0 & lat != 0))

clcjuncea <- cljuncea
cljuncea <- cljuncea[,2:3]
coordinates(cljuncea) <- c('lon','lat')
proj4string(cljuncea) <- CRS("+proj=longlat +datum=WGS84 +no_defs +
                            ellps=WGS84 +towgs84=0,0,0")


juncea_data <- extract(BIO,cljuncea)
juncea_data <- cbind.data.frame(cljuncea,as.data.frame(juncea_data))

back.xy <- randomPoints(BIO, n = 1000, p = cljuncea, ext=extent(cljuncea))
back <- SpatialPoints(back.xy, crs(cljuncea))

eA <- extract(BIO,back)
eP <- extract(BIO,cljuncea)
Back.cov <- data.frame(eA,Pres = 0)
Pres.cov <- data.frame(eP,Pres = 1)
all.cov <- rbind(Pres.cov,Back.cov)

all.cov <- na.omit(all.cov)
Pres.cov <- na.omit(Pres.cov)
Back.cov <- na.omit(Back.cov)

kfold_pres <- kfold(Pres.cov, folds)
kfold_back <- kfold(Back.cov, folds)

## Now the data is ready and you should be able to run the models.
