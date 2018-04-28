## ----Libraries-----------------------------------------------------------
# devtools::install_github("tidyverse/ggplot2")
library(ggplot2)
# devtools::install_github("gadenbuie/ggpomological")
library(ggpomological)
library(scales)
library(readr)
library(sf)
library(dplyr)
library(grid)
# devtools::install_github("statnmap/ggsn", ref="font-family")
library(ggsn)
library(units)
library(extrafont)
# extrafont::font_import()

## ---- eval=!load---------------------------------------------------------
# extraWD <- "."
if (!file.exists(file.path(extraWD, "Region_L93.rds"))) {
  githubURL <- "https://github.com/statnmap/blog_tips/raw/master/2018-04-18-draw-maps-like-paintings/Region_L93.rds"
  download.file(githubURL, file.path(extraWD, "Region_L93.rds"))
}
Region_L93 <- read_rds(file.path(extraWD, "Region_L93.rds"))

## ----youngs, fig.height=7, fig.width=9, eval=!load-----------------------
ggplot() +
  geom_sf(data = Region_L93,
          aes(fill = Age_0.19.ans), alpha = 0.75, colour = NA
  ) +
  geom_sf(data = Region_L93,
          aes(colour = Age_0.19.ans), alpha = 1, fill = NA
  ) +
  scale_fill_gradient(
    "Youngs (< 20y)",
    low = muted(ggpomological:::pomological_palette[2], l = 90, c = 70),
    high = ggpomological:::pomological_palette[2]
  ) +
  scale_colour_gradient(
    low = muted(ggpomological:::pomological_palette[2], l = 90, c = 70),
    high = ggpomological:::pomological_palette[2],
    guide = FALSE
  ) +
  theme_pomological_fancy(base_family = "Homemade Apple")

## ----youngskm2, fig.height=7, fig.width=9, eval=!load--------------------
Region_Prop_L93 <- Region_L93 %>%
  mutate(area_km2 = st_area(.) %>% set_units(km^2) %>% drop_units()) %>%
  mutate(Prop_20_km2 = `Age_0.19.ans` / area_km2)

ggplot() +
  geom_sf(data = Region_Prop_L93,
          aes(fill = Prop_20_km2), alpha = 0.9, colour = NA
  ) +
  geom_sf(data = Region_Prop_L93,
          aes(colour = Prop_20_km2), alpha = 1, fill = NA
  ) +
  scale_fill_gradient(
    "Youngs / km²",
    low = muted(ggpomological:::pomological_palette[2], l = 100, c = 70),
    high = muted(ggpomological:::pomological_palette[2], l = 50, c = 70),
    trans = "log"
  ) +
  scale_colour_gradient(
    low = muted(ggpomological:::pomological_palette[2], l = 80, c = 70),
    high = muted(ggpomological:::pomological_palette[2], l = 50, c = 70),
    guide = FALSE
  ) +
  theme_pomological_fancy(base_family = "Nanum Pen") +
  labs(
    title = "Number of people aged below 20 by km² in 2015",
    caption = "source: http://www.ecosante.fr/"
  )

## ----youngskm2cities, fig.height=7, fig.width=9, eval=!load--------------
# Add cities and project in Lambert93
cities_L93 <- tibble(
  city = c("Paris", "Rennes", "Lille", "Strasbourg", "Brest", "Bordeaux",
    "Montpellier", "Nice", "lyon"),
  lat = c(48.857256, 48.110867, 50.625291, 48.576816, 48.384679, 44.843019,
          43.609519, 43.694233, 45.749206),
  long = c(2.344655, -1.678327, 3.057288, 7.754883, -4.498229, -0.581680,
           3.877594, 7.245262, 4.847652)
) %>%
  st_as_sf(coords = c("long", "lat")) %>%
  st_set_crs(4326) %>%
  st_transform(2154) %>% 
  bind_cols(st_coordinates(.) %>% as.data.frame())

# Plot
ggplot(Region_Prop_L93) +
  geom_sf(data = Region_Prop_L93,
          aes(fill = Prop_20_km2), alpha = 0.9, colour = NA
  ) +
  geom_sf(data = Region_Prop_L93,
          aes(colour = Prop_20_km2), alpha = 1, fill = NA
  ) +
  geom_sf(data = cities_L93, colour = "grey30") +
  geom_text(
    data = cities_L93, aes(X, Y, label = city), nudge_y = 20000,
    family = "Nanum Pen", colour = "grey20", size = 6
  ) +
  scale_fill_gradient("Youngs / km²",
    low = muted(ggpomological:::pomological_palette[2], l = 100, c = 70),
    high = muted(ggpomological:::pomological_palette[2],l = 50, c = 70),
    trans = "log"
  ) +
  scale_colour_gradient(
    low = muted(ggpomological:::pomological_palette[2],l = 80, c = 70),
    high = muted(ggpomological:::pomological_palette[2],l = 50, c = 70),
    guide = FALSE
  ) +
  theme_pomological_fancy(base_family = "Nanum Pen") +
  labs(
    title = "Number of people aged below 20 by km² in 2015",
    caption = "source: http://www.ecosante.fr/\nproj: Lambert93, epsg: 2154 aut.: S. Rochette, ThinkR."
  ) +
  xlab("") + ylab("") +
  north(Region_Prop_L93, symbol = 4, scale = 0.1) +
  scalebar(Region_Prop_L93, location = "bottomleft", dist = 200, dd2km = FALSE,
           model = "WGS84", box.fill = c("grey30", "white"), 
           box.color = "grey30", st.color = "grey30", family = "Nanum Pen"
  ) +
  theme(text = element_text(family = "Nanum Pen")) +
  coord_sf(crs = 2154)


## ----regions, fig.height=7, fig.width=9, eval=!load----------------------
# palette
cols <- rep(ggpomological:::pomological_palette,
  length.out = length(Region_Prop_L93$NOM_REG))

ggplot(Region_Prop_L93) +
  geom_sf(
    data = Region_Prop_L93,
    aes(fill = NOM_REG),
    alpha = 0.9, colour = NA
  ) +
  geom_sf(
    data = Region_Prop_L93,
    aes(colour = NOM_REG),
    alpha = 1, fill = NA
  ) +
  geom_sf(data = cities_L93, colour = "grey30") +
  geom_text(
    data = cities_L93, aes(X, Y, label = city),
    nudge_y = 20000, family = "Nanum Pen",
    colour = "grey20", size = 6
  ) +
  scale_fill_manual(
    "Regions",
    values = muted(cols, l = 60, c = 70)
  ) +
  scale_colour_manual(
    values = muted(cols, l = 40, c = 70),
    guide = FALSE
  ) +
  theme_pomological_fancy(base_family = "Nanum Pen") +
  labs(
    title = "French regions",
    caption = "source: https://www.ign.fr/\nproj: Lambert93, epsg: 2154 aut.: S. Rochette, ThinkR."
  ) +
  xlab("") + ylab("") +
  north(Region_Prop_L93, symbol = 4, scale = 0.1) +
  scalebar(
    Region_Prop_L93,
    location = "bottomleft", dist = 200,
    dd2km = FALSE, model = "WGS84",
    box.fill = c("grey30", "white"), box.color = "grey30",
    st.color = "grey30", family = "Nanum Pen"
  ) +
  theme(text = element_text(family = "Nanum Pen")) +
  coord_sf(crs = 2154)


