## ------------------------------------------------------------------------
library(dplyr)
library(sf)
library(raster)
library(ggplot2)
library(readr)
library(tmap)
# library(hexbin)

## Customize
# Test for font family
# extrafont::font_import(prompt = FALSE)
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




























## ---- results="asis"-----------------------------------------------------
# Biblio with {chameleon}
tmp_biblio <- tempdir()
attachment::att_from_rmd("2019-11-08-30daymapchallenge-building-maps-1.Rmd") %>% 
  chameleon::create_biblio_file(
    out.dir = tmp_biblio, to = "html", edit = FALSE)
htmltools::includeMarkdown(file.path(tmp_biblio, "bibliography.html"))

