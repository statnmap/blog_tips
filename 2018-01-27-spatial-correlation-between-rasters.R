## ------------------------------------------------------------------------
# Libraries
library(raster)
library(dplyr)
library(mapview)
library(mapedit)
library(sf)
library(readr)
library(ggplot2)

## ----dataset, fig.height=6, fig.width=13, eval=!load---------------------
temp_r <- raster(file.path(extraWD, "PREVIMER_F1-MARS3D-MANGAE2500_2010-2014_spring_TEMP_mean.tif"))

chl_r <- raster(file.path(extraWD, "chlorophyll_a_2003-2010_spring_mean.tif"))


# Geographic Western Europe
Europe <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
  "Czech Rep.", "Denmark", "Estonia", "Finland", "France",
  "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia",
  "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland",
  "Portugal", "Romania", "Slovakia", "Slovenia", "Spain",
  "Sweden", "UK",
  "Switzerland", "Norway", "Monaco", "Jersey", "Guernsey",
  "Azores")

Europe_border <- map_data("world") %>%
  filter(region %in% Europe)

# This extract part of the data for lighter plots (like rasterVis)
temp_dat <- SDMSelect::gplot_data(temp_r, maxpixels = 50000) %>%
  mutate(variable = "Temperature") %>%
  filter(!is.na(value))
chl_r_dat <- SDMSelect::gplot_data(chl_r, maxpixels = 50000) %>%
  mutate(variable = "Chlorophyll-a") %>%
  filter(!is.na(value))

# Plot
g1 <- ggplot(temp_dat) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_gradient2("T°C",
    low = scales::muted("blue"),
    high = scales::muted("red"),
    midpoint = mean(temp_dat$value)) +
  geom_polygon(data = Europe_border,
               aes(long, lat, group = group),
               fill = "white",
               colour = "grey20", size = 0.1) +
  coord_quickmap(
    xlim = range(temp_dat$x),
    ylim = range(temp_dat$y)
  ) + xlab("") + ylab("")

g2 <- ggplot(chl_r_dat) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_gradient("Chl-a", low = "white",
                      high = "forestgreen",
                      trans = "log") +
    geom_polygon(data = Europe_border,
               aes(long, lat, group = group),
               fill = "white",
               colour = "grey20", size = 0.1) +
  coord_quickmap(
    xlim = range(chl_r_dat$x),
    ylim = range(chl_r_dat$y)
  ) + xlab("") + ylab("")

gridExtra::grid.arrange(g1, g2, ncol = 2)


## ---- echo=TRUE, eval=FALSE----------------------------------------------
## ext_pol <- mapview(temp_r) %>%
##   editMap()
## 
## extent_pol <- ext_pol$finished

## ---- echo=TRUE, eval=FALSE----------------------------------------------
## write_rds(extent_pol, file.path(extraWD, "extent_pol.rds"))

## ---- echo=FALSE---------------------------------------------------------
extent_pol <- read_rds(file.path(extraWD, "extent_pol.rds"))

## ---- echo=FALSE---------------------------------------------------------
mapview(extent_pol)

## ----crop, eval=!load, fig.height=6, fig.width=13------------------------
temp_crop_r <- raster::crop(temp_r, as_Spatial(st_geometry(extent_pol))) %>%
  aggregate(2)

chl_res_r <- aggregate(chl_r, 4) %>% 
  resample(temp_crop_r)

# Stack covariates
temp_chl_s <- stack(temp_crop_r, chl_res_r)
names(temp_chl_s) <- c("temperature", "chlorophyll-a")

# Old way to plot...
plot(temp_chl_s)


## ---- echo=FALSE, eval=!load---------------------------------------------
writeRaster(temp_chl_s, 
            file.path(extraWD, "temp_chl_s.grd"),
            overwrite = TRUE)

## ---- echo=FALSE---------------------------------------------------------
temp_chl_s <- stack(file.path(extraWD, "temp_chl_s.grd"))

## ------------------------------------------------------------------------
# Correlation between layers
cor(values(temp_chl_s)[,1],
    values(temp_chl_s)[,2],
    use = "na.or.complete")


## ------------------------------------------------------------------------
lm1 <- lm(values(temp_chl_s)[,2] ~ values(temp_chl_s)[,1])
summary(lm1)


## ----lm, eval=!load, fig.width=7, fig.height=5---------------------------
# Retrieve residuals considering missing values
resid_lm <- raster(temp_chl_s, 1) * NA
values(resid_lm)[-lm1$na.action] <- lm1$residuals

# Figure
resid_lm_dat <- SDMSelect::gplot_data(
  resid_lm, maxpixels = 50000) %>%
  mutate(variable = "Residuals") %>%
  filter(!is.na(value))

ggplot(resid_lm_dat) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_gradient2("Residuals",
    low = scales::muted("red"),
    high = scales::muted("blue"),
    midpoint = 0) +
    geom_polygon(data = Europe_border,
               aes(long, lat, group = group),
               fill = "white",
               colour = "grey20", size = 0.1) +
  coord_quickmap(
    xlim = range(resid_lm_dat$x),
    ylim = range(resid_lm_dat$y)
  ) + xlab("") + ylab("")

## ---- eval=FALSE---------------------------------------------------------
## temp_chl_s_nb <- raster(temp_chl_s, 1)
## values(temp_chl_s_nb) <- 1:ncell(temp_chl_s)
## 
## focal_cor <- focal(
##   x = temp_chl_s_nb,
##   w = matrix(1, 5, 5),
##   fun = function(x, y = temp_chl_s){
##     cor(values(y)[x, 1], values(y)[x, 2],
##         use = "na.or.complete")
##   },
##   filename = file.path(extraWD, "focal_cor.tif"),
##   overwrite = TRUE
## )
## 

## ----focal, eval=!load, fig.width=8, fig.height=6------------------------
focal_cor <- raster(file.path(extraWD, "focal_cor.tif"))
# Get data for ggplot
focal_cor_dat <- SDMSelect::gplot_data(focal_cor, maxpixels = 50000) %>%
  mutate(variable = "Correlation") %>%
  filter(!is.na(value))

# Plot
ggplot(focal_cor_dat) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_gradient2("Corr",
    low = "#d7191c",
    mid = "#ffffbf",
    high = "#1a9641",
    midpoint = 0) +
  geom_polygon(data = Europe_border,
               aes(long, lat, group = group),
               fill = "white",
               colour = "grey20", size = 0.1) +
  coord_quickmap(
    xlim = range(focal_cor_dat$x),
    ylim = range(focal_cor_dat$y)
  ) + xlab("") + ylab("")


