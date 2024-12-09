## -----------------------------------------------------------------------------
# Libraries
library(terra)
library(dismo)
library(tmap)
library(mapview)
library(mapedit)


## ----prep-data----------------------------------------------------------------
# get list of predictor variables in dismo
fnames <- list.files(
  path = paste(system.file(package = "dismo"), "/ex", sep = ""),
  pattern = "grd", full.names = TRUE
)

# Get Bio1 and Bio12
bio1 <- rast(fnames[1])
bio12 <- rast(fnames[2])

# Transform Bio12 to simulate a different extent, origin and resolution
bio12_trans <- aggregate(bio12, 4)


## ----dataset, fig.height=6, fig.width=13--------------------------------------
# Temperature and precipiation maps
tm1 <- tm_shape(bio1) +
  tm_raster(
    title = "Temperature (°C)", palette = "-RdBu",
    midpoint = mean(values(bio1), na.rm = TRUE)
  ) +
  tm_layout(title = "Mean annual temperature")

tm2 <- tm_shape(bio12_trans) +
  tm_raster(title = "Precipitation", palette = "Greens") +
  tm_layout(title = "Total annual precipitation (mm)")

# Arrange maps
tmap_arrange(tm1, tm2)


## ----resample, fig.height=6, fig.width=13-------------------------------------
# Aggregate and resample chlorophyll raster
bio1_r <- resample(bio1, bio12_trans)

# Stack rasters for analysis
bio_1_12 <- c(bio1_r, bio12_trans)
names(bio_1_12) <- c("temperature", "precipitation")

# Temperature and precipiation maps
tm1 <- tm_shape(bio1_r) +
  tm_raster(
    title = "Temperature (°C)", palette = "-RdBu",
    midpoint = mean(values(bio1_r), na.rm = TRUE)
  ) +
  tm_layout(title = "Mean annual temperature")

tm2 <- tm_shape(bio12_trans) +
  tm_raster(title = "Precipitation", palette = "Greens") +
  tm_layout(title = "Total annual precipitation (mm)")

# Arrange maps
tmap_arrange(tm1, tm2)

## ----echo=FALSE---------------------------------------------------------------
writeRaster(bio_1_12,
  file.path(extraWD, "bio_1_12.grd"),
  overwrite = TRUE
)


## -----------------------------------------------------------------------------
# Correlation between layers
cor(
  values(bio_1_12)[, 1],
  values(bio_1_12)[, 2],
  use = "na.or.complete"
)


## -----------------------------------------------------------------------------
lm1 <- lm(values(bio_1_12)[, 2] ~ values(bio_1_12)[, 1])
summary(lm1)

## ----lm, fig.width=7, fig.height=5--------------------------------------------
# Create and plot residual raster
resid_r <- rast(bio_1_12, 1)
values(resid_r) <- NA
values(resid_r)[
  !is.na(values(bio_1_12)[, 1]) &
    !is.na(values(bio_1_12)[, 2])
] <- lm1$residuals

tm_shape(resid_r) +
  tm_raster(title = "Residuals", palette = "-RdBu", midpoint = 0) +
  tm_layout(title = "Model Residuals")


## -----------------------------------------------------------------------------
bio_1_12_nb <- rast(bio_1_12, 1)
values(bio_1_12_nb) <- 1:ncell(bio_1_12)

matrix_bio_1_12 <- values(bio_1_12) # stack as raster [MW]

focal_cor <- focal(
  x = bio_1_12_nb,
  w = matrix(1, 5, 5),
  fun = function(x, y = matrix_bio_1_12) {
    cor(y[x, 1], y[x, 2],
      use = "na.or.complete"
    )
  },
  filename = file.path(extraWD, "focal_cor.tif"),
  overwrite = TRUE
)


## ----focal, fig.width=8, fig.height=6-----------------------------------------
focal_cor <- raster(file.path(extraWD, "focal_cor.tif"))

# Plot local correlation
tm_shape(focal_cor) +
  tm_raster(title = "Local Correlation", palette = "-RdYlGn", midpoint = 0) +
  tm_layout(title = "Local Correlation Map")

