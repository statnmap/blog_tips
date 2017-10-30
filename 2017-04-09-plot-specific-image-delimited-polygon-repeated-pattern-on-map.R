## ----Libraries-----------------------------------------------------------
library(maptools)
library(rgdal)
library(sp)
library(emojifont)
library(raster)
library(dplyr)
library(tidyr)
library(rgeos)
library(ggplot2)
library(png)

# load.emojifont(font = "EmojiOne.ttf")
load.emojifont(font = "OpenSansEmoji.ttf")

## ----LoadData------------------------------------------------------------
# Load global map
fra.sp.orig <- getData('GADM', country = 'FRA', level = 1)
fra.sp <- gSimplify(fra.sp.orig, tol = 0.005) %>%
  SpatialPolygonsDataFrame(data = data.frame(NAME = fra.sp.orig$NAME_1))

# Select polygon in which adding symbols
pol <- fra.sp[which(fra.sp$NAME == "Pays de la Loire"),]

# Define extent for figures
e <- extend(extent(pol), c(2.75, 1.6, 1.3, 1.5))
ext.pol <- SpatialPolygons(list(Polygons(list(Polygon(
  rbind(c(e@xmin, e@ymin), c(e@xmin, e@ymax), 
        c(e@xmax, e@ymax), c(e@xmax, e@ymin),
        c(e@xmin, e@ymin)))), ID = 1)))
proj4string(ext.pol) <- proj4string(fra.sp)

# Define colors fixed for some regions
col.n <- c(9,1,4,2,3,6,4,5,5,6,7,6,7,10,7,3,9,8,7,9,7,1)
col.region <- yarrr::piratepal("basel")[col.n]
dev.off()

## ----PointGrid-----------------------------------------------------------
# Create a point grid in a specific polygon -----------------------------------
grid.pt <- sp::spsample(pol, n = 25, type = "regular")

jpeg(filename = "Symbols_in_polygon.jpg", quality = 100)
# png(filename = "Symbols_in_polygon.png", type = "cairo")
par(mai = c(0, 0, 0, 0))
# plot the entire map with restricted limits
plot(ext.pol, xaxs = "i", yaxs = "i", border = NA)
plot(fra.sp, col = scales::alpha(col.region, alpha = 0.7), add = TRUE)
# Highlight the selected polygon
plot(pol, border = "red", lwd = 3, add = TRUE)
# Normal points
points(grid.pt, pch = 20, col = "blue", cex = 0.1)
# Add emojifont instead of points
text(coordinates(grid.pt)[, 1], coordinates(grid.pt)[, 2],
     labels = emoji('evergreen_tree'), cex = 2, lwd = 3,
     col = 'forestgreen', family = 'OpenSansEmoji')
box()
dev.off()


## ----SymbolsWithBuffer---------------------------------------------------
# Realize a grid point assuring icons not crossing lines -----------------------
# icons half size
px_size <- 0.15
# Create a Buffer ith negative width to reduce polygon size
polBuf <- gBuffer(pol, width = -px_size)
# Sample in the buffer
grid.pt <- sp::spsample(polBuf, n = 25, type = "regular")

jpeg(filename = "Symbols_in_polygon_with_buffer.jpg", quality = 100)
par(mai = c(0, 0, 0, 0))
# plot the entire map with restricted limits
plot(ext.pol, xaxs = "i", yaxs = "i", border = NA)
plot(fra.sp, col = scales::alpha(col.region, alpha = 0.7), add = TRUE)
# Highlight the selected polygon
plot(pol, border = "red", lwd = 3, add = TRUE)
# Show buffer area where no points
plot(polBuf, col = scales::alpha("red", 0.5) , border = NA, add = TRUE)
# Add emojifont instead of points
text(coordinates(grid.pt)[, 1], coordinates(grid.pt)[, 2],
     labels = emoji('evergreen_tree'), cex = 2, lwd = 3,
     col = 'forestgreen', family = 'OpenSansEmoji')
box()
dev.off()


## ----OwnImageRepeated----------------------------------------------------
# Add your own image as a repeated pattern -------------------------------------
iconfile1 <- download.file('https://www.r-project.org/logo/Rlogo.png',
                           destfile = 'icon1.png', mode = 'wb')
icon1 <- png::readPNG('icon1.png')

# Define the desired size of the image
offset <- 0.1

jpeg(filename = "Personal_image_in_polygon.jpg", quality = 100)
par(mai = c(0, 0, 0, 0))
# plot the entire map with restricted limits
plot(ext.pol, xaxs = "i", yaxs = "i", border = NA)
plot(fra.sp, col = scales::alpha(col.region, alpha = 0.7), add = TRUE)
# Show buffer area where no points
plot(pol, border = "red", lwd = 3, add = TRUE)
# Plot all images at points locations
for (i in 1:length(grid.pt)) {
  graphics::rasterImage(
    icon1, 
    coordinates(grid.pt)[i, 1] - offset,
    coordinates(grid.pt)[i, 2] - offset,
    coordinates(grid.pt)[i, 1] + offset,
    coordinates(grid.pt)[i, 2] + offset
  )
}
box()
dev.off()

## ----FunctionCrop--------------------------------------------------------
# Function to crop image within the limits of a polygon ----------------------

#' Plot your own image at a specific position cropped in a delimited polygon.
#' 
#' @param path path to PNG image to be added to the plot
#' @param offset vector of size 1 or 2. Size of the image in the scale of coordinates system of pol
#' @param offset.pc vector of size 1 or 2. Percent of the extent of the polygon.
#' @param pt coordinates where to plot the raster (x, y)
#' @param pol polygon to limit extent of the raster
#' @param type "normal" or "ggplot", matrix or tbl to be plotted
#' @param plot logical wether to plot (TRUE) or to return matrix/tbl for future plot (FALSE)

rasterImageCropPt <- function(path, offset, offset.pc, pt, pol,
                              type = "normal", plot = TRUE) {
  # Transform as raster (not Raster) to be plot by rasterImage
  r.png <- as.raster(png::readPNG(path))
  # Read as Raster to be cropped by polygon
  r <- raster(path, 1)
  # Calculate extent of the raster
  if (!missing(offset)) {
    # offset is image size
    offset <- 0.5 * offset
    if (length(offset) == 1) {offset <- rep(offset, 2)}
    xmin <- pt[1] - offset[1]
    xmax <- pt[1] + offset[1]
    ymin <- pt[2] - offset[2]
    ymax <- pt[2] + offset[2]
  }
  if (!missing(offset.pc)) {
    # offset is image size
    offset.pc <- 0.5 * offset.pc
    if (length(offset.pc) == 1) {offset.pc <- rep(offset.pc, 2)}
    xmin <- pt[1] - offset.pc[1]/100 * (xmax(pol) - xmin(pol))
    xmax <- pt[1] + offset.pc[1]/100 * (xmax(pol) - xmin(pol))
    ymin <- pt[2] - offset.pc[2]/100 * (ymax(pol) - ymin(pol))
    ymax <- pt[2] + offset.pc[2]/100 * (ymax(pol) - ymin(pol))
  }
  extent(r) <- c(xmin, xmax, ymin, ymax)
  projection(r) <- projection(pol)
  r.res <- mask(r, pol)
  
  # Add transparency to areas out of the polygon
  r.mat <- as.matrix(r.res)
  r.png[is.na(r.mat)] <- "#FFFFFF00"
  
  # Transform for ggplot
  if (type == "ggplot") {
    r.png <- data.frame(coordinates(r), value = c(t(as.matrix(r.png)))) %>% as.tbl()
    # r.png <- tbl_df(as.data.frame(as.matrix(r.png))) %>%
    #   mutate(y = rev(seq(ymin, ymax, length = dim(r.png)[1]))) %>%
    #   gather(x, value, -y) %>%
    #   mutate(x = rep(seq(xmin, xmax, length = dim(r.png)[2]), each = dim(r.png)[1]))
  }
  
  if (plot) {
    if (type == "normal") {
      graphics::rasterImage(r.png, xmin, ymin, xmax, ymax)
    } else {
      return(geom_tile(data = r.png, aes(x, y), fill = r.png$value))
    } 
  } else {
    return(r.png)
  }
}

## ----OwnImageCrop--------------------------------------------------------
# Add your own image as repeated pattern in a polygon, cropped when needed -----
# Realise a point grid in a specific polygon, no buffer needed
grid.pt <- sp::spsample(pol, n = 15, type = "regular")

# Image size
offset <- 0.4

jpeg(filename = "Personal_image_in_polygon_cropped.jpg", quality = 100)
# plot the entire map with restricted limits
par(mai = c(0, 0, 0, 0))
# plot the entire map with restricted limits
plot(ext.pol, xaxs = "i", yaxs = "i", border = NA)
plot(fra.sp, col = scales::alpha(col.region, alpha = 0.7), add = TRUE)
# Plot images
for (i in 1:length(grid.pt)) {
  rasterImageCropPt(path = 'icon1.png', offset,
                    pt = coordinates(grid.pt)[i, ], 
                    pol = pol, type = "normal", plot = TRUE)  
}
box()
dev.off()

## ----wallpaper-----------------------------------------------------------
# Create a wallpaper image with repeated pattern -------------------------------
pol_square <- Polygon(coords = matrix(c(0,1,1,0,0,
                                        0,0,1,1,0), ncol = 2))
grid.wp <- sp::spsample(pol_square, n = 100, type = "regular")

offset <- 0.03

png(filename = "wallpaper.png", 
    width = 600, height = 600, unit = "px") 
par(mai = c(0.1,0.1,0.1,0.1), bg = "transparent", xpd = TRUE)
plot(grid.wp, col = "transparent")
for (i in 1:length(grid.wp)) {
  graphics::rasterImage(
    icon1, 
    coordinates(grid.wp)[i, 1] - offset,
    coordinates(grid.wp)[i, 2] - offset,
    coordinates(grid.wp)[i, 1] + offset,
    coordinates(grid.wp)[i, 2] + offset
  )
}
dev.off()

## ----WallpaperCrop-------------------------------------------------------
# Add your own wallpaper with repeated images in a polygon ---------------------
path.wp <- "wallpaper.png"
ratio.pol <- (xmax(pol) - xmin(pol)) / (ymax(pol) - ymin(pol))

jpeg(filename = "Wallpaper_in_polygon_cropped.jpg", quality = 100)
# plot the entire map with restricted limits
par(mai = c(0, 0, 0, 0))
# plot the entire map with restricted limits
plot(ext.pol, xaxs = "i", yaxs = "i", border = NA)
plot(fra.sp, col = scales::alpha(col.region, alpha = 0.7), add = TRUE)

# Add in the middle of the polygon
rasterImageCropPt(path = path.wp, offset.pc = c(100 * ratio.pol, 100),
                  pt = c(xmin(pol) + (xmax(pol) - xmin(pol))/2,
                         ymin(pol) + (ymax(pol) - ymin(pol))/2),
                  pol = pol, type = "normal", plot = TRUE)  
box()
dev.off()

## ----WallpaperGGplot-----------------------------------------------------
# Add your own wallpaper with repeated images in a polygon with ggplot ---------
# Direct ----
# Transform spatial data as `sf` objectas it is easier with ggplot
fra.sf <- sf::st_as_sf(fra.sp)

g <- ggplot() +
  geom_sf(data = fra.sf,  aes(fill = NAME), col = "black") +
  scale_fill_manual(values = scales::alpha(col.region, alpha = 0.7),
                    breaks = fra.sp$NAME) +
  coord_sf(xlim = c(xmin(ext.pol), xmax(ext.pol)), 
           ylim = c(ymin(ext.pol), ymax(ext.pol)),
           expand = FALSE) +
  rasterImageCropPt(path = path.wp, offset.pc = c(100 * ratio.pol, 100),
                    pt = c(xmin(pol) + (xmax(pol) - xmin(pol))/2,
                           ymin(pol) + (ymax(pol) - ymin(pol))/2),
                    pol = pol, type = "ggplot", plot = TRUE) +
  guides(colour = FALSE, fill = FALSE)

ggsave(plot = g, filename = "Wallpaper_in_polygon_cropped_ggplot_direct.jpg",
       quality = 100, width = 6, height = 6, units = "in", dpi = 80)


## ----WallpaperGGplot2----------------------------------------------------
# Indirect ----
dataPNG <- rasterImageCropPt(
  path = path.wp,
  offset.pc = c(100 * ratio.pol, 100),
  pt = c(xmin(pol) + (xmax(pol) - xmin(pol))/2,
         ymin(pol) + (ymax(pol) - ymin(pol))/2),
  pol = pol, type = "ggplot", plot = FALSE)
# You can filter transparent tiles (out of the area)
dataPNG.filter <- filter(dataPNG, value != "#FFFFFF00")


g <- ggplot() +
  geom_sf(data = fra.sf,  aes(fill = NAME), col = "black") +
    scale_fill_manual(values = scales::alpha(col.region, alpha = 0.7),
                      breaks = fra.sp$NAME) +
    coord_sf(xlim = c(xmin(ext.pol), xmax(ext.pol)), 
                  ylim = c(ymin(ext.pol), ymax(ext.pol)),
             expand = FALSE) +
  geom_tile(data = dataPNG.filter, aes(x, y), fill = dataPNG.filter$value) +
  guides(colour = FALSE, fill = FALSE)

ggsave(plot = g, filename = "Wallpaper_in_polygon_cropped_ggplot_indirect.jpg",
       quality = 100, width = 6, height = 6, units = "in", dpi = 80)



## ----WallpaperEmoji------------------------------------------------------
# Create a wallpaper image with repeated emoji and polygon dimensions ----
path.wp <- "wallpaperEmoji.png"
ratio.pol <- (xmax(pol) - xmin(pol)) / (ymax(pol) - ymin(pol))

pol_square <- Polygon(coords = 
                        matrix(c(0,ratio.pol,ratio.pol,0,0,
                                 0,0,1,1,0),
                               ncol = 2))
grid.wp <- sp::spsample(pol_square, n = 100, type = "hexagonal")

png(filename = path.wp, 
    width = 300 * ratio.pol, height = 300, unit = "px") 
par(mai = c(0.1,0.1,0.1,0.1), bg = "transparent", xpd = TRUE)
plot(grid.wp, col = "transparent")

text(coordinates(grid.wp)[, 1], coordinates(grid.wp)[, 2],
     labels = emoji('evergreen_tree'), cex = 3, lwd = 4,
     col = 'forestgreen', family = 'OpenSansEmoji')
dev.off()

## ----GGplotEmoji---------------------------------------------------------
dataPNG <- rasterImageCropPt(
  path = path.wp,
  offset.pc = c(100, 100),
  pt = c(xmin(pol) + (xmax(pol) - xmin(pol))/2,
         ymin(pol) + (ymax(pol) - ymin(pol))/2),
  pol = pol, type = "ggplot", plot = FALSE)
# You can filter transparent tiles (out of the area)
dataPNG.filter <- filter(dataPNG, value != "#FFFFFF00")


g <- ggplot() +
  geom_sf(data = fra.sf,  aes(fill = NAME), col = "black") +
    scale_fill_manual(values = scales::alpha(col.region, alpha = 0.7),
                      breaks = fra.sp$NAME) +
    coord_sf(xlim = c(xmin(ext.pol), xmax(ext.pol)), 
                  ylim = c(ymin(ext.pol), ymax(ext.pol)),
             expand = FALSE) +
  geom_tile(data = dataPNG.filter, aes(x, y), fill = dataPNG.filter$value) +
  guides(colour = FALSE, fill = FALSE)

ggsave(plot = g, 
       filename = "Wallpaper_in_polygon_cropped_ggplot_indirect_Emoji.jpg",
       quality = 100, width = 6, height = 6, units = "in", dpi = 80)


