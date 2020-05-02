## -----------------------------------------------------------------------------
library(dplyr)
library(sf)
library(ggplot2)
library(tmap)
library(leaflet)


## -----------------------------------------------------------------------------
extraWD <- "."
if (!file.exists(file.path(extraWD, "departement.zip"))) {
  githubURL <- "https://github.com/statnmap/blog_tips/raw/master/2018-07-14-introduction-to-mapping-with-sf-and-co/data/departement.zip"
  download.file(githubURL, file.path(extraWD, "departement.zip"))
  unzip(file.path(extraWD, "departement.zip"), exdir = extraWD)
}
departements_L93 <- st_read(dsn = extraWD, layer = "DEPARTEMENT",
                            quiet = TRUE) %>% 
  st_transform(2154)




## ---- echo=TRUE---------------------------------------------------------------
departements_L93[,2:3]


## ---- echo=TRUE---------------------------------------------------------------
str(as(departements_L93[,2:3], "Spatial"), max.level = 2)
# str(as(departements_L93[,2:3], "Spatial"), max.level = 3)


## ---- echo=TRUE---------------------------------------------------------------
Bret_L93 <- 
  departements_L93 %>% 
  mutate_at(
    vars(NOM_DEPT, NOM_REG),
    tolower) %>% 
  select(CODE_DEPT, NOM_DEPT, NOM_REG) %>% 
  filter(NOM_REG == "bretagne")
Bret_L93








## ---- echo = TRUE-------------------------------------------------------------
region_L93 <- departements_L93 %>% 
  group_by(CODE_REG, NOM_REG) %>% 
  summarize()





## -----------------------------------------------------------------------------
# extraWD <- "."
# -- Communes --
if (!file.exists(file.path(extraWD, "communes.zip"))) {
  githubURL <- "https://github.com/statnmap/blog_tips/raw/master/2018-07-14-introduction-to-mapping-with-sf-and-co/data/communes.zip"
  download.file(githubURL, file.path(extraWD, "communes.zip"))
}
unzip(file.path(extraWD, "communes.zip"), exdir = extraWD)

# -- Maternites --
if (!file.exists(file.path(extraWD, "Maternite_2004-2016.csv"))) {
  githubURL <- "https://github.com/statnmap/blog_tips/raw/master/2018-07-14-introduction-to-mapping-with-sf-and-co/data-mater/Maternite_2004-2016.csv"
  download.file(githubURL, file.path(extraWD, "Maternite_2004-2016.csv"))
}

# Read shapefile of French communes
communes <- st_read(dsn = extraWD, layer = 'COMMUNE', quiet = TRUE) %>% 
  select(NOM_COM, INSEE_COM)
# Read file of maternities for 2016
data.maternite <- readr::read_csv(file.path(extraWD, "Maternite_2004-2016.csv")) %>% 
  filter(an == 2016)


## ---- warning=FALSE, message=FALSE--------------------------------------------
# Join database with shapefile by attributes
maternites_L93 <- communes %>%
  right_join(data.maternite, 
             by = "INSEE_COM") %>% 
  st_transform(2154) 





## ---- echo=TRUE, warning=FALSE------------------------------------------------
maternites_Bret_L93 <- maternites_L93 %>%
  st_intersection(Bret_L93)

## ---- echo=FALSE--------------------------------------------------------------
glue::glue("Attributs du fichier 'Maternités' avant intersection")
names(maternites_L93)
glue::glue("Attributs après Intersection avec région")
names(maternites_Bret_L93)





## ---- echo=TRUE, warning=FALSE------------------------------------------------
# Buffer area for Brittany
Bret_buffer10_L93 <- 
  Bret_L93 %>% 
  st_buffer(
    dist = 
      units::set_units(10, km)
  ) %>% 
  st_cast() # useful sometimes !

# Buffer Bretagne for larger study area bbox only
Bret_buffer30_L93 <- 
  Bret_L93 %>% 
  st_buffer(
    dist = 
      units::set_units(30, km)
  ) 





## -----------------------------------------------------------------------------
# Define couples distances - years to buffer
#' @param x sf object
#' @param dists  c(10000, 25000, 50000, 75000)
#' @param ans  unique(maternites_pt$an)

dist_circles <- function(x,
                         dists = units::set_units(c(10, 25, 50), km),
                         ans,
                         x.limits) {
  # browser()
  if (!missing(ans)) {
    dists_ans <- data.frame(
      dists = rep(dists, length(ans)),
      ans = rep(ans, each = length(dists))
    )
    # Create buffer areas for each distances / year
    pts_buf <- purrr::map2(
      dists_ans$dists, dists_ans$ans,
      ~st_buffer(
        filter(x, an == .y),
        .x) %>%
        mutate(
          dist = .x,
        )
    ) %>%
      do.call("rbind", .) %>% 
      st_cast() %>%
      mutate(dist.leg = glue::glue("<{dist/1000} km"))
    
    
    # Define triplet big/small-distance/year
    big_small <- data.frame(
      big_dist = dists[rep(2:length(dists), length(ans))],
      small_dist = dists[rep(1:(length(dists) - 1), length(ans))],
      an = ans[rep(1:length(ans), each = length(dists) - 1)]
    )
    # Remove part of polygons overlapping smaller buffer
    pts_holes <- big_small %>%
      split(1:nrow(big_small)) %>%
      purrr::map( 
        ~st_difference(
          filter(pts_buf, dist == .$big_dist, an == .$an),
          filter(pts_buf, dist == .$small_dist, an == .$an)
        )
      ) %>%
      do.call("rbind", .) %>% 
      select(-contains(".1")) %>%
      st_cast()
    
    # Add smallest polygons and re-order distance names for legend
    pts_holes_tot <- pts_holes %>% 
      rbind(
        filter(pts_buf, dist == min(dists))
      ) %>%
      arrange(an, dist) %>%
      mutate(dist = forcats::fct_reorder(dist.leg, dist))
    
  } else {
    pts_buf <- purrr::map(
      dists,
      ~st_buffer(x, .x) %>%
        mutate(dist = .x)
    ) %>%
      do.call("rbind", .) %>% 
      st_cast() %>%
      mutate(dist.leg = as.character(dist))
    
    
    # Define triplet big/small-distance/year
    dists_order_char <- sort(dists) %>% as.character()
    big_small <- data.frame(
      big_dist = tail(dists_order_char, -1),
      small_dist = head(dists_order_char, -1)
    )
    
    
    # Remove part of polygons overlapping smaller buffer
    pts_holes <- big_small %>%
      split(1:nrow(big_small)) %>%
      purrr::map( 
        ~st_difference(
          filter(pts_buf, dist.leg == .$big_dist),
          filter(pts_buf, dist.leg == .$small_dist) %>% 
            select(geometry) %>% st_union()
        )
      ) %>%
      do.call("rbind", .) %>% 
      select(-contains(".1")) %>%
      st_cast()
    
    # Add smallest polygons and re-order distance names for legend
    pts_holes_tot <- pts_holes %>% 
      rbind(
        filter(pts_buf, dist.leg == dists_order_char[1])
      ) %>%
      arrange(dist) %>%
      mutate(dist = forcats::fct_reorder(dist.leg, dist))
    
  }
  
  pts_holes_fr <- st_intersection(pts_holes_tot, 
                                  dplyr::select(x.limits, geometry))
  
  return(pts_holes_fr)
  
}


## ---- warning=FALSE-----------------------------------------------------------
# Centroides des communes des maternités
maternites_centroid_Bret_L93 <- 
  maternites_Bret_L93 %>% 
  st_centroid() 

# Circles around centroids
dists <- c(5, 15, 25, 50)
maternites_circles_L93 <- dist_circles(
  maternites_centroid_Bret_L93,
  dists = units::set_units(dists, km),
  x.limits = Bret_L93)


x11()
plot(maternites_circles_L93)





## ---- echo=TRUE---------------------------------------------------------------
# Load background map
# data(Europe) # deprecated in tmap 2.0
data(World)
Europe <- World %>% 
  filter(continent == "Europe" & name != "France") 

# Map
tm <- tm_shape(Europe) +
  tm_polygons() +
tm_shape(departements_L93, is.master = TRUE) +
  tm_fill(col = "NOM_DEPT", legend.show = FALSE, palette = "Set1") +
  tm_borders("grey30") +
tm_shape(maternites_circles_L93) +
  tm_fill(col = "dist", palette = "-Blues") +
tm_scale_bar(position = c("right", "bottom"), size = 1) +
tm_compass(position = c("right", "top"), size = 1.8) + 
# tm_style_natural() #deprecated in tmap 2.0
tm_style("natural")


x11()
tm

## ---- echo=TRUE---------------------------------------------------------------
departements_wgs84 <- 
  st_transform(departements_L93, crs = 4326)

factpal <- colorFactor(topo.colors(5), departements_wgs84$NOM_REG)

m <- leaflet() %>%
  addTiles() %>% 
  addPolygons(data = departements_wgs84,
              color = ~factpal(NOM_REG),
              fillOpacity = 0.8, stroke = FALSE)
m


## ---- echo=TRUE, eval=TRUE----------------------------------------------------
tmap_leaflet(tm)


## ---- eval=TRUE, echo=TRUE----------------------------------------------------
library(mapview)
mapView(departements_L93)


## ---- eval=FALSE--------------------------------------------------------------
## logo_pol <- viewRGB(logo) %>% editMap()
## logo_overlap <- logo_pol$finished


