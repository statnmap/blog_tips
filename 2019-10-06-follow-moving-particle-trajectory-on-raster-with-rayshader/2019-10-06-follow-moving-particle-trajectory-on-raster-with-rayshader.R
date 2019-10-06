## ---- eval=FALSE---------------------------------------------------------
## # For header
## i <- 20
## position <- points_ray_z$coords$coords[[i]]
## position_next <- points_ray_z$coords$coords[[i + 1]]
## render_cropped_scene(ray_image, datamat, position,
##                      position_next, windowsize = c(1024, 512),
##                      zoom = 0.3)
## 
## rgl::snapshot3d(file.path(extraWD, paste0(slug, "_header.png")))

