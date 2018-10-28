## ---- message=FALSE, warning=FALSE---------------------------------------
library(mgx2r)
library(rgl)
library(dplyr)
library(sf)
library(ggplot2)
library(raster)
library(rayshader)
library(magick)
# extraWD <- tempdir()

## ----readmgx-------------------------------------------------------------
filePly <- system.file("extdata", "full/mesh/mesh_meristem_full_T0.ply", package = "mgx2r")

myMesh <- read_mgxPly(
  file = filePly, ShowSpecimen = FALSE, addNormals = TRUE,
  MatCol = 1, header_max = 30)

try(rgl.close(), silent = TRUE)
plot3d(myMesh) 
# Change view
view3d(theta = 30, phi = -40, zoom = 0.5)
rglwidget()


## ----test, echo=FALSE, eval=FALSE----------------------------------------
## # Try to change orientation of 3d mesh and save matrix
## # saveview <- par3d()
## # trans_matrix <- dput(saveview$userMatrix)
## # Transform mesh in other direction
## # myMeshProj <- transform3d(myMesh,
## #                           matrix = trans_matrix)
## 
## myMeshProj <- transform3d(myMesh,
##                          matrix = identityMatrix())
## myMeshProj <- myMesh
## 
## try(rgl.close(), silent = TRUE)
## plot3d(myMeshProj)
## rglwidget()

## ----meshtosf, eval=!load------------------------------------------------
mesh_sf <- t(myMesh$vb) %>% 
  as_tibble() %>% 
  st_as_sf(coords = c("x", "y"), crs = 2154)

ggplot(mesh_sf) +
  geom_sf(aes(colour = z)) +
  coord_sf(crs = 2154, datum = 2154) +
  scale_colour_gradient2(low = "#d7191c", high = "#1a9641", mid = "#ffffbf",
                         midpoint = mean(mesh_sf$z))

## ----rasterinterp, eval=!load--------------------------------------------
# Create an empty raster with dimensions of the mesh ----
r <- raster(
  as(mesh_sf, "Spatial"),
  nrows = 100, ncols = 100,
  crs = st_crs(mesh_sf)$proj4string
)

# Transform raster as spatial points with sf
r_sf <- st_as_sf(as.data.frame(coordinates(r)),
                 coords = c("x", "y"),
                   crs = 2154)

# Distance between points and raster ----
obs.r.dists <- st_distance(mesh_sf, r_sf)
obs.r.dists <- unclass(obs.r.dists)

# Inverse distance interpolation ----
## pred = 1/dist^idp
idp <- 2
inv.w <- (1/(obs.r.dists^idp))
z <- (t(inv.w) %*% matrix(mesh_sf$z)) / apply(inv.w, 2, sum)

# Remove values far from data
dist.min <- apply(obs.r.dists, 2, function(x) min(x, na.rm = TRUE))
z[dist.min > 2] <- min(z, na.rm = TRUE)

# Fill raster with predictions
r.pred <- r
values(r.pred) <- z

# Plot
rasterVis::gplot(r.pred) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2(low = "#d7191c", high = "#1a9641", mid = "#ffffbf",
                         midpoint = mean(mesh_sf$z))

## ----meshtoray-----------------------------------------------------------
# Transform raster as matrix for rayshader
r.matrix <- matrix(
  raster::extract(r.pred, raster::extent(r.pred)),
  nrow = ncol(r.pred),
  ncol = nrow(r.pred))

zscale <- 1

# Transform points in the scale of matrix in rayshader
mesh_sf_scale <- data.frame(st_coordinates(mesh_sf), mesh_sf$z) %>% 
  as_tibble() %>% 
  rename(z = mesh_sf.z) %>% 
  mutate(X_ray = (X - xmin(r.pred)) / xres(r.pred),
         Y_ray = -1*(Y - ymin(r.pred)) / yres(r.pred),
         Z_ray = z*zscale + 1.5)

# Create shades
try(rgl.close(), silent = TRUE)
r.matrix %>%
  sphere_shade(zscale = 0.1, texture = "unicorn", progbar = FALSE) %>%
  add_shadow(ray_shade(r.matrix, zscale = 500, progbar = FALSE), 0.5) %>%
  plot_3d(r.matrix, zscale = zscale, theta = -45, zoom = 0.5, phi = 30)

# Add mesh
spheres3d(mesh_sf_scale[,c("X_ray", "Z_ray", "Y_ray")],
       col = "grey40", radius = 0.2, add = TRUE)
# Change view
view3d(theta = 30, phi = 45, zoom = 0.5)

# rgl.snapshot(filename = file.path(extraWD))
# Output html widget for Rmd
rglwidget()

## ----fieldview, eval=!load-----------------------------------------------
render_depth(
  focus = 0.5, focallength = 10,
  bokehshape = "hex", bokehintensity = 5,
  progbar = FALSE
)

## ----giffieldview, eval=!load--------------------------------------------
seq_focus <- seq(0.4, 1, length.out = 35)
for (i in 1:length(seq_focus)) {
  view3d(theta = 30, phi = 35 + i/2, zoom = 0.5)
  
  render_depth(
    focus = seq_focus[i], focallength = 25,
    bokehshape = "hex", bokehintensity = 5,
    progbar = FALSE,
    filename = file.path(extraWD, "gif",
                         paste0(formatC(i, flag = "0", width = 3)))
  )
  
  if (i == 1) {
  all_img <- image_read(file.path(extraWD, "gif",
                         paste0(formatC(i, flag = "0", width = 3), ".png")))
  } else {
    new_img <- image_read(file.path(extraWD, "gif",
                              paste0(formatC(i, flag = "0", width = 3), ".png")))
    all_img <- c(all_img, new_img)
  }
}

# Reduce width of images
gif <- image_animate(all_img, fps = 5, loop = 0) %>% 
  image_apply(FUN = function(x) 
    image_resize(x, geometry = geometry_size_pixels(width = 500)))

# Save as gif in external directory
image_write_gif(gif, path = glue("{extraWD}/focus-1.gif"),
                delay = 0.25)

# Reduce size on disk
system(glue("convert {extraWD}/focus-1.gif -fuzz 10% -layers Optimize ../../static{StaticImgWD}/focus-low-1.gif"))

