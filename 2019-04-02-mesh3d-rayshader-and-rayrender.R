## ---- message=FALSE, warning=FALSE---------------------------------------
# Packages
library(raster)
library(dplyr)
library(Rvcg)
library(rgl)
library(rayshader)
library(rayrender)
# remotes::install_github("statnmap/mesh2ray")
library(mesh2ray)
library(magick)
# Directories
# extraWD <- tmpImgWD <- ""


























## ---- echo=FALSE, eval=load----------------------------------------------
human_materials <- readr::read_rds(file.path(extraWD, "human_materials.rds"))

## ------------------------------------------------------------------------
human_materials


## ------------------------------------------------------------------------
# 3. Draw with rayrender
if (!file.exists(file.path(extraWD, "rayrender-human-scene.png"))) {
  # _scene
  scene <- generate_cornell(lightintensity = 20)
  # _add cubes on scene with material data.frame /!\ A little long /!\
  scene <- mesh2ray::add_cubes_to_scene(scene, cubes = human_cubes, 
                                        material = human_materials)
  
  # _draw scene /!\ long calculation /!\
  options(cores = 4)
  render_scene(scene, width = 600, height = 600,
               lookfrom = c(278, 278, -800) ,
               lookat = c(278, 278, 0), fov = 40, ambient_light = FALSE,
               samples = 500, parallel = TRUE, clamp_value = 5,
               filename =  file.path(tmpImgWD, "figure-html/rayrender-human-scene.png"))
}




## ---- warning=FALSE, message=FALSE---------------------------------------
attachment::att_from_rmd("2019-04-02-mesh3d-rayshader-and-rayrender.Rmd")

