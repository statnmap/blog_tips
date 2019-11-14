## ------------------------------------------------------------------------
library(dplyr)
library(sf)
library(raster)
library(readr)
library(tmap)
library(xml2)
library(purrr)
# remotes::install_github("tylermorganwall/rayshader")
library(rayshader)

## Customize
my_blue <- "#1e73be"
# Test for font family
# extrafont::font_import(prompt = FALSE)
font <- extrafont::choose_font(c("Nanum Pen", "Lato", "sans"))





## ------------------------------------------------------------------------
#' @param amenity Character. An amenity.
#' @param file_amenity path to where to save downloaded data
#' @param hex Hexagonal polygon grid
get_and_sf <- function(amenity, file_amenity, hex) {
  if (!file.exists(file_amenity)) {
    # devtools::install_github("tutuchan/fodr")
    # portal <- fodr::fodr_portal("ods")
    poi <- fodr::fodr_dataset("ods", "points-dinterets-openstreetmap-en-france")
    
    # Get amenity
    amenity <- poi$get_records(refine = list(amenity = amenity))
    
    amenity_chr <- amenity %>% 
      dplyr::select(-geo_shape, -other_tags)
    
    write_csv(amenity_chr, file_amenity)
    
  } else {
    amenity_chr <- read_csv(file_amenity)
  }
  
  amenity_sf_l93 <- amenity_chr %>% 
    st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
    st_transform(crs = 2154)
  
  # Join amenity wth hex and count
  amenity_hex_l93 <- st_join(amenity_sf_l93, hex)
  amenity_hex_count <- amenity_hex_l93 %>% 
    st_drop_geometry() %>% 
    count(id_hex)
  
  # Join hex with bars count
  France_hex_amenity <- hex %>% 
    left_join(amenity_hex_count)
}




















## ----tracks--------------------------------------------------------------
# count tracks
my_tracks_count <- my_move_sf_lines %>% 
  mutate(
    unique_id = map2_chr(
      city, city.1, 
      ~paste(range(.x, .y), collapse = "-"))
  ) %>% 
  count(unique_id, sort = TRUE)

# count cities
my_cities_count <- my_move_sf %>% 
  count(city, sort = TRUE)

# Plot
tm <- tm_shape(europe_ne) +
  tm_fill() +
  tm_borders(col = "grey10") +
tm_shape(my_cities_count, is.master = TRUE) +
  tm_symbols(col = "n", size = 2,
          style = "pretty", n = 5,
          title.col = "Stop count") +
tm_shape(my_tracks_count) +
  tm_lines(lwd = "n",
           scale = 8,
           title.lwd = "Trip Count") +
tm_credits("ign.fr - {raster} + {tmap} - @statnmap",
           position = c(0.48, -0.01),
           size = 1.3, fontfamily = "Nanum Pen",
           col = "grey40") +
tm_layout(
    main.title = paste("The", nrow(my_tracks_count), "different tracks I used in 2019"),
    main.title.color = my_blue,
    main.title.fontfamily = "Nanum Pen",
    main.title.size = 1.75,
    title.fontfamily = "Nanum Pen",
    # Margin to allow legend
    inner.margins = c(0.15, 0.15, 0.15, 0.15),
    # Colours
    outer.bg.color = c("#E6EFFA"),
    bg.color = my_blue, #"grey60",
    # Legend
    legend.title.color = "grey80",
    legend.text.color = "grey80",
    legend.position = c("left", "top")
  )

tmap_save(tm, filename = file.path(extraWD, "tracks.jpg"),
          width = 1024, height = 512, units = "px",
          dpi = 100)

tm






## ---- results="asis"-----------------------------------------------------
# Biblio with {chameleon}
tmp_biblio <- tempdir()
attachment::att_from_rmd("2019-11-15-30daymapchallenge-building-maps-2-tmap.Rmd") %>% 
  chameleon::create_biblio_file(
    out.dir = tmp_biblio, to = "html", edit = FALSE)
htmltools::includeMarkdown(file.path(tmp_biblio, "bibliography.html"))

