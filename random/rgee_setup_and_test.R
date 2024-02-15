#remotes::install_github("r-spatial/rgee")
library(rgee)

## It is necessary just once
#ee_install()

# Initialize Earth Engine!
ee_Initialize()

# full check of status of rgee dependencies
# ee_check() # Check non-R dependencies
# ee_clean_credentials() # Remove credentials of a specific user
# ee_clean_pyenv() # Remove reticulate system variables

#............................................................

## FOLLOWING THE GETTING STARTED CODE FROM HERE: https://csaybar.github.io/rgee-examples/#Introduction

#
## HELLO WORLD -------
#

# traditional R character
print("Hello world!")

# Earth Engine Python Style
ee$String("Hello World from Earth Engine!")$getInfo()
ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")$getInfo()

# Earth Engine Pipes Style
ee$String("Hello World from Earth Engine!") %>%
  ee$String$getInfo()

ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318") %>%
  ee$Image$getInfo()

# 
## ADD DATA TO MAP --------
#

# Load an image.
image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")

# Display the image.
Map$centerObject(image)
Map$addLayer(image, name = "Landsat 8 original image")

# Define visualization parameters in an object literal.
vizParams <- list(
  bands = c("B5", "B4", "B3"),
  min = 5000, max = 15000, gamma = 1.3
)

Map$addLayer(image, vizParams, "Landsat 8 False color")

# Use Map to add features and feature collections to the map. For example,
counties <- ee$FeatureCollection("TIGER/2016/Counties")

Map$addLayer(
  eeObject = counties,
  visParams = vizParams,
  name = "counties"
)

#
## FINDING IMAGES -----------
#

collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1")

point <- ee$Geometry$Point(-122.262, 37.8719)
start <- ee$Date("2014-06-01")
finish <- ee$Date("2014-10-01")

filteredCollection <- ee$ImageCollection("LANDSAT/LC08/C01/T1")$
  filterBounds(point)$
  filterDate(start, finish)$
  sort("CLOUD_COVER", TRUE)

first <- filteredCollection$first()

# Define visualization parameters in an object literal.
vizParams <- list(
  bands = c("B5", "B4", "B3"),
  min = 5000,
  max = 15000,
  gamma = 1.3
)

Map$addLayer(first, vizParams, "Landsat 8 image")

# Load a feature collection.
featureCollection <- ee$FeatureCollection("TIGER/2016/States")

# Filter the collection.
filteredFC <- featureCollection$filter(ee$Filter$eq("NAME", "California"))

# Display the collection.
Map$addLayer(
  eeObject = filteredFC,
  visParams = list(palette = "red"),
  name = "California"
)

#
## BAND MATH ------------
#

# This function gets NDVI from Landsat 5 imagery.
getNDVI <- function(image) {
  return(image$normalizedDifference(c("B4", "B3")))
}

# Load two Landsat 5 images, 20 years apart.
image1 <- ee$Image("LANDSAT/LT05/C01/T1_TOA/LT05_044034_19900604")
image2 <- ee$Image("LANDSAT/LT05/C01/T1_TOA/LT05_044034_20100611")

# Compute NDVI from the scenes.
ndvi1 <- getNDVI(image1)
ndvi2 <- getNDVI(image2)

# Compute the difference in NDVI.
ndviDifference <- ndvi2$subtract(ndvi1)

ndviParams <- list(palette = c(
  "#d73027", "#f46d43", "#fdae61", "#fee08b",
  "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850"
))
ndwiParams <- list(min = -0.5, max = 0.5, palette = c("FF0000", "FFFFFF", "0000FF"))

Map$centerObject(ndvi1)
Map$addLayer(ndvi1, ndviParams, "NDVI 1") +
  Map$addLayer(ndvi2, ndviParams, "NDVI 2") +
  Map$addLayer(ndviDifference, ndwiParams, "NDVI difference")

#
## MAPPING ---------
#

# This function gets NDVI from Landsat 8 imagery.
addNDVI <- function(image) {
  return(image$addBands(image$normalizedDifference(c("B5", "B4"))))
}

# Load the Landsat 8 raw data, filter by location and date.
collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1")$
  filterBounds(ee$Geometry$Point(-122.262, 37.8719))$
  filterDate("2014-06-01", "2014-10-01")

# Map the function over the collection.
ndviCollection <- collection$map(addNDVI)

first <- ndviCollection$first()
print(first$getInfo())

bandNames <- first$bandNames()
print(bandNames$getInfo())

#
## REDUCING -------
#

# Load a Landsat 8 collection.
collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1")$
  filterBounds(ee$Geometry$Point(-122.262, 37.8719))$
  filterDate("2014-01-01", "2014-12-31")$
  sort("CLOUD_COVER")

# Compute the median of each pixel for each band of the 5 least cloudy scenes.
median <- collection$limit(5)$reduce(ee$Reducer$median())

# Define visualization parameters in an object literal.
vizParams <- list(
  bands = c("B5_median", "B4_median", "B3_median"),
  min = 5000, max = 15000, gamma = 1.3
)

Map$addLayer(
  eeObject = median,
  visParams = vizParams,
  name = "Median image"
)

#
## IMAGE STATISTICS --------
#

# Load and display a Landsat TOA image.
image <- ee$Image("LANDSAT/LC08/C01/T1_TOA/LC08_044034_20140318")
Map$addLayer(
  eeObject = image,
  visParams = list(bands = c("B4", "B3", "B2"), max = 30000),
  name = "Landsat 8"
)

# Create an arbitrary rectangle as a region and display it.
region <- ee$Geometry$Rectangle(-122.2806, 37.1209, -122.0554, 37.2413)
Map$addLayer(
  eeObject = region,
  name = "Region"
)

# Get a dictionary of means in the region.  Keys are bandnames.
mean <- image$reduceRegion(
  reducer = ee$Reducer$mean(),
  geometry = region,
  scale = 30
)

print(mean$getInfo())

#
## MASKING ----------
#

# This function gets NDVI from Landsat 5 imagery.
getNDVI <- function(image) {
  return(image$normalizedDifference(c("B4", "B3")))
}

# Load two Landsat 5 images, 20 years apart.
image1 <- ee$Image("LANDSAT/LT05/C01/T1_TOA/LT05_044034_19900604")
image2 <- ee$Image("LANDSAT/LT05/C01/T1_TOA/LT05_044034_20100611")

# Compute NDVI from the scenes.
ndvi1 <- getNDVI(image1)
ndvi2 <- getNDVI(image2)

# Compute the difference in NDVI.
ndviDifference <- ndvi2$subtract(ndvi1)
# Load the land mask from the SRTM DEM.
landMask <- ee$Image("CGIAR/SRTM90_V4")$mask()

# Update the NDVI difference mask with the land mask.
maskedDifference <- ndviDifference$updateMask(landMask)

# Display the masked result.
vizParams <- list(
  min = -0.5,
  max = 0.5,
  palette = c("FF0000", "FFFFFF", "0000FF")
)

Map$addLayer(
  eeObject = maskedDifference,
  visParams = vizParams,
  name = "NDVI difference"
)

#
## A COMPLETE EXAMPLE ---------
#

# This function gets NDVI from Landsat 8 imagery.
addNDVI <- function(image) {
  return(image$addBands(image$normalizedDifference(c("B5", "B4"))))
}

# This function masks cloudy pixels.
cloudMask <- function(image) {
  clouds <- ee$Algorithms$Landsat$simpleCloudScore(image)$select("cloud")
  return(image$updateMask(clouds$lt(10)))
}

# Load a Landsat collection, map the NDVI and cloud masking functions over it.
collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(c(-122.262, 37.8719)))$
  filterDate("2014-03-01", "2014-05-31")$
  map(addNDVI)$
  map(cloudMask)

# Reduce the collection to the mean of each pixel and display.
meanImage <- collection$reduce(ee$Reducer$mean())
vizParams <- list(
  bands = c("B5_mean", "B4_mean", "B3_mean"),
  min = 0,
  max = 0.5
)

Map$addLayer(
  eeObject = meanImage,
  visParams = vizParams,
  name = "mean"
)

# Load a region in which to compute the mean and display it.
counties <- ee$FeatureCollection("TIGER/2016/Counties")
santaClara <- ee$Feature(counties$filter(
  ee$Filter$eq("NAME", "Santa Clara")
)$first())

Map$addLayer(
  eeObject = santaClara,
  visParams = list(palette = "yellow"),
  name = "Santa Clara"
)

# Get the mean of NDVI in the region.
mean <- meanImage$select("nd_mean")$reduceRegion(
  reducer = ee$Reducer$mean(),
  geometry = santaClara$geometry(),
  scale = 30
)

# Print mean NDVI for the region.
cat("Santa Clara spring mean NDVI:", mean$get("nd_mean")$getInfo())
