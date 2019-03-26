## ------------------------------------------------------------------------
# Path to data
# extraWD <- file.path("data/")
path <- extraWD


## ----packages, message=FALSE---------------------------------------------
library(readr)
library(tidyr)
library(maps)
library(mapdata)
library(maptools) # required to transform map object as sf
library(sf)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(mapview)
library(glue)
library(tricolore)
library(forcats)
library(ggtern)
library(rnaturalearth)


## ----custompal-----------------------------------------------------------
bluepal <- colorRampPalette(
  c("#FFFFFF", "#D9F0F5", "#B2E2EC", "#8CD3E3", "#288EA5", "#1A5F6E"))
orangebluepal <- colorRampPalette(
  c(rev(c("#F8DFD8", "#F1C0B0", "#EBA18A", "#B4421E", "#782C14")),
    c("#FFFFFF", "#D9F0F5", "#B2E2EC", "#8CD3E3", "#288EA5", "#1A5F6E")))
catpal <- c("#DE633C", "#41B7D1", "#A6AAAB", "#FF9919", "#C0504D", "#5B5960", "#EEECE1")

par(mfrow = c(1, 3))
barplot(rep(1, 6), col = bluepal(6), space = 0)
barplot(rep(1, 6), col = orangebluepal(6), space = 0)
barplot(rep(1, 7), col = catpal, space = 0)


## ----readdata------------------------------------------------------------
mcr <- read_csv(file.path(path, 'multipleChoiceResponses.csv'), skip = 1) 


## ----cleanquestions------------------------------------------------------
## Programming languages
questions <- tibble(value = names(mcr)) %>% 
  mutate(col_name = case_when(
    grepl("In which country", value) ~ "country",
    grepl("What is your age (# years)?", value) ~ "age",
    grepl("What programming languages do you use", value) ~
      paste0("lang_use_all_", str_extract(value, '(?<=Choice - )(\\w*\\s*[:graph:]*)*$')),
    grepl("What specific programming language do you use most often?", value) ~ "lang_use_often",
    grepl("What programming language would you recommend", value) ~ "lang_recommend",
    grepl("primary tool", value) ~ paste0("tool_", str_extract(value, '(?<= - )(.*)(?= - Text$)')),
    grepl("IDE", value) ~ paste0("IDE_", str_extract(value, '(?<=Choice - )(\\w*\\s*[:graph:]*)*$')),
    grepl("your work easy to reproduce", value) ~ paste0("repro_how_", str_extract(value, '(?<= Choice - )(\\w*\\s*[:graph:]*)*$')),
    grepl("reuse and reproduce", value) ~ paste0("repro_barrier_", str_extract(value, '(?<= Choice - )(\\w*\\s*[:graph:]*)*$')),
    grepl("which specific data visualization library", value) ~ "dataviz_often",
    grepl("Which types of data", value) ~ paste0("datatype_all_", str_extract(value, '(?<= Choice - )(\\w*\\s*[:graph:]*)*$')),
    grepl("What is the type of data", value) ~ "datatype_often",
    # grepl("cloud computing services"),
    TRUE ~ value
  ))

# Duplicated colnames are open answers
questions$col_name[duplicated(questions$col_name)] <- 
  paste0(questions$col_name[duplicated(questions$col_name)], "_open")

names(mcr) <- questions$col_name

# Clean names
questions


## ----worldmap------------------------------------------------------------
# Get world map data
worldmap <- maps::map("world", fill = TRUE, plot = FALSE)

# Rewrite st_as_sf for map object to get subregions
st_as_sf.map <- function(x, ...) {
  # browser()
    ID0 = vapply(strsplit(x$names, ":"), function(y) y[1], "")
    ID1 = vapply(strsplit(x$names, ":"), function(y) y[2], "")
    ID_unique <- 1:length(ID0)
    m.sp = maptools::map2SpatialPolygons(x, IDs = ID_unique, proj4string = sp::CRS("+init=epsg:4326"))
    m = st_as_sf(m.sp)
    m$ID = as.numeric(vapply(m.sp@polygons, function(x) slot(x, "ID"), ""))
    m$region = ID0[m$ID]
    m$subregion = ID1[m$ID]
m    
}

# Transform to Winkel tripel projection for World representation
worldmap_sf <- worldmap %>% 
  st_as_sf.map()

plot(worldmap_sf)





## ----spdatamap-----------------------------------------------------------
ne_world <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")

world_eqe <- ne_world %>% 
  st_transform("+proj=eqearth +wktext") 

world_eqe %>%
  ggplot() +
  geom_sf()


## ------------------------------------------------------------------------
# List countries not written identically in both datasets
mcr %>% 
  mutate(is_in_map = country %in% world_eqe$name_long) %>% 
  filter(!is_in_map) %>% 
  pull(country) %>% 
  unique()

# Rename countries on the map for correspondance
world_eqe_country <- world_eqe %>% 
    mutate(country = case_when(
      name_long == "Republic of Korea" ~ "South Korea",
      subunit == "Hong Kong S.A.R." ~ "Hong Kong (S.A.R.)", 
      name_long == "Vietnam" ~ "Viet Nam",
      # sovereignt == "Czechia" ~ "Czech Republic",
      name_long == "Russian Federation" ~ "Russia",
      TRUE ~ name_long
    ))

# Rename countries in the data for correspondance
mcr_country <- mcr %>% 
  mutate(country = case_when(
    country == "Republic of Korea" ~ "South Korea",
    country == "United States of America" ~ "United States",
    country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
    country == "Iran, Islamic Republic of..." ~ "Iran",
    TRUE ~ country
  ))

# Test again # List countries not written identically in both datasets
# mcr_country %>%
#   mutate(is_in_map = country %in% world_eqe_country$country) %>%
#   filter(!is_in_map) %>%
#   pull(country) %>%
#   unique()





## ----function------------------------------------------------------------
#' Create ternary map
#' @param most data.frame with columns named value and n
#' @param q_country data.frame with country and value columns
#' @param p_legend Vector of 3 names for triangle legend
#' @param join_map The sf map to join dataset with
#' @param title title of the graph
#' @param top indices of the three values to keep
map_triangle <- function(most, q_country, p_legend, join_map, title, top = 1:3) {
 # browser()
  # clean_names <- most$value[top] %>% 
  #   make.names()
  # 
  # Keep only three indices
  top <- top[1:3]
  # Number of respondant by country of three most used
  x_country <- q_country %>% 
    filter(value %in% most$value[top]) %>% 
    count(country, value) 
  
  x_country_spread <- x_country %>% 
    spread(value, n) %>% 
    mutate_at(vars(most$value[top]), list(~ifelse(is.na(.), 0, .))) # dplyr >= 0.8
    # mutate_at(vars(most$value[top]), funs(ifelse(is.na(.), 0, .))) # dplyr < 0.8
  
  # Whole data mean
  center <- apply(x_country_spread %>% select(most$value[top]), 2, mean)
  center <- center / sum(center)

  # As spatial
  x_sf <- x_country_spread %>% 
    left_join(join_map, ., by = "country")
  
  # Scaling factor
  sum_prop <- t(apply(x_country_spread %>% select(most$value[top]), 1, function(x) x/sum(x)))
  mins <- apply(sum_prop, 2, min)
  zoomed_side <- (1 - (mins[2] + mins[3])) - mins[1]
  true_spread <- 1 / zoomed_side
  
  # Triangle colors
  triangle <- Tricolore(x_sf, p1 = most$value[top][1], p2 = most$value[top][2], p3 = most$value[top][3],
                        label_as = "pct", center = center,
                        spread = true_spread)
  # Triangle legend
  triangle_legend <- triangle$key + 
    labs(L = p_legend[1], T = p_legend[2], R = p_legend[3]) +
    theme(
      tern.axis.arrow.show = TRUE,
      plot.background = element_rect(fill = NA, color = NA),
      axis.title = element_text(size = 10))
  
  # Map
  x_sf %>% 
    mutate(rgb = triangle$rgb) %>% 
    ggplot() +
    geom_sf(aes(fill = rgb), size = 0.1) +
    coord_sf(crs = st_crs(x_sf)) +
    scale_fill_identity() +
    # triangle scale annotation
    annotation_custom(
      ggtern::ggplotGrob(triangle_legend),
      xmin = st_bbox(x_sf)[1] + 0.01*(st_bbox(x_sf)[3] - st_bbox(x_sf)[1]),
      xmax = st_bbox(x_sf)[1] + 0.35*(st_bbox(x_sf)[3] - st_bbox(x_sf)[1]),
      ymin = st_bbox(x_sf)[2] + 0.01*(st_bbox(x_sf)[4] - st_bbox(x_sf)[2]),
      ymax = st_bbox(x_sf)[2] + 0.50*(st_bbox(x_sf)[4] - st_bbox(x_sf)[2])
    ) +
    ggtitle(c(title, ""))
}


## ------------------------------------------------------------------------
lang_country <- mcr_country %>% 
  select(country, starts_with("lang_use_all")) %>% 
  gather(key, value, -country) %>% 
  filter(value != -1)

# most_used
most_used <- lang_country %>% 
  count(value) %>% 
  filter(!is.na(value)) %>% 
  arrange(desc(n))

most_used





## ------------------------------------------------------------------------
lang_often_country <- mcr_country %>% 
  select(country, starts_with("lang_use_often")) %>% 
  gather(key, value, -country) %>% 
  filter(!is.na(value))

# most_often
most_often <- lang_often_country %>% 
  count(value) %>% 
  arrange(desc(n))

most_often











## ------------------------------------------------------------------------
repro_how_country <- mcr_country %>% 
  select(country, starts_with("repro_how_")) %>% 
  gather(key, value, -country) %>% 
  filter(!is.na(value))

# most_repro_how
most_repro_how <- repro_how_country %>% 
  count(value) %>% 
  arrange(desc(n))

most_repro_how





## ------------------------------------------------------------------------
repro_barriers_country <- mcr_country %>% 
  select(country, starts_with("repro_barrier_")) %>% 
  gather(key, value, -country) %>% 
  filter(!is.na(value))

# most_repro_how
most_barriers_how <- repro_barriers_country %>% 
  count(value) %>% 
  arrange(desc(n))

most_barriers_how

