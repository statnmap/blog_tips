## -----------------------------------------------------------------------------
library(dplyr)
library(sf)
library(rnaturalearth)
library(tmap)
# install.packages("echarts4r")
# remotes::install_github('JohnCoene/echarts4r.assets')
# remotes::install_github('JohnCoene/echarts4r.maps')
library(echarts4r)
library(echarts4r.assets)
library(sp)
# remotes::install_github("JohnCoene/globe4r")
library(globe4r)
library(maps)
library(ggplot2)
library(readr)
library(raster)
library(rgl)
library(geometry)

## Customize
font <- extrafont::choose_font(c("Nanum Pen", "Lato", "sans"))

my_blue <- "#1e73be"
my_theme <- function(font = "Nanum Pen", ...) {
  theme_dark() %+replace%
    theme(
      plot.title = element_text(family = font, 
                                colour = my_blue, size = 18),
      plot.subtitle = element_text(family = font, size = 15),
      # text = element_text(size = 13), #family = font, 
      plot.margin = margin(4, 4, 4, 4),
      plot.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.text = element_text(family = font, size = 10),
      legend.title = element_text(family = font),
      # legend.text = element_text(size = 8),
      legend.background = element_rect(fill = "#f5f5f2"),
      ...
    )
}


## ----france, eval=FALSE, echo=FALSE-------------------------------------------
## # Data preparation
## # extraWD <- "." # Uncomment for the script to work
## # Read communes
## Communes_L93 <- st_read(file.path(extraWD, "COMMUNE", "COMMUNE.shp"), quiet = TRUE) %>%
##   st_drop_geometry() %>%
##   select(INSEE_COM, NOM_COM, X_CHF_LIEU, Y_CHF_LIEU, SUPERFICIE, POPULATION) %>%
##   st_as_sf(coords = c("X_CHF_LIEU", "Y_CHF_LIEU"),
##            crs = 2154)
## 
## # mapview::mapview(Communes_L93)
## 
## st_write(Communes_L93, file.path(extraWD, "commune_centers.shp"), delete_layer = TRUE)
## 








## ---- echo=FALSE, eval=FALSE--------------------------------------------------
## # Prepare Loire river
## French_rivers <- st_read(file.path(extraWD, "BDTOPO_3-0_HYDROGRAPHIE_SHP_LAMB93_FXX_2019-09-19.7z/BDTOPO_3-0_HYDROGRAPHIE_SHP_LAMB93_FXX_2019-09-19/BDTOPO/1_DONNEES_LIVRAISON_2019-09-00371/BDT_3-0_SHP_LAMB93_FXX_ED2019-09-19/HYDROGRAPHIE/COURS_D_EAU.shp"))
## Loire_river <- French_rivers %>%
##   filter(CODE_HYDRO == "04C0000002000808543")
## 
## st_write(Loire_river, file.path(extraWD, "Loire.shp"))
## 
## mapview::mapview(Loire_river)




















## ---- results="asis"----------------------------------------------------------
# Biblio with {chameleon}
tmp_biblio <- tempdir()
attachment::att_from_rmd("2019-11-22-30daymapchallenge-building-maps-3-earth-is-sphere.Rmd") %>% 
  chameleon::create_biblio_file(
    out.dir = tmp_biblio, to = "html", edit = FALSE,
    output = "packages")
htmltools::includeMarkdown(file.path(tmp_biblio, "bibliography.html"))

