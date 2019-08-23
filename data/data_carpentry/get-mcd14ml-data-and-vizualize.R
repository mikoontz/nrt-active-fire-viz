# Download latest MCD14ML data and add rectangular buffers around them.

library(tidyverse)
library(sf)

st_square_buffer <- function(obj, radius = NULL) {
  pts <- st_coordinates(obj)
  
  if(!is.null(radius)) {  
    xmin <- pts[, "X"] - radius
    xmax <- pts[, "X"] + radius
    ymin <- pts[, "Y"] - radius
    ymax <- pts[, "Y"] + radius
  } else {
    xmin <- pts[, "X"] - (pull(obj, scan) / 2 / (10000 / 90))
    xmax <- pts[, "X"] + (pull(obj, scan) / 2 / (10000 / 90))
    ymin <- pts[, "Y"] - (pull(obj, track) / 2 / (10000 / 90))
    ymax <- pts[, "Y"] + (pull(obj, track) / 2 / (10000 / 90))
  }
  
  corners <- tibble(xmin, xmax, ymin, ymax)
  
  square_polys <- 
    corners %>% 
    pmap(.f = function(xmin, xmax, ymin, ymax) {
      square_poly <- st_polygon(list(matrix(c(xmin, ymax, 
                                              xmax, ymax, 
                                              xmax, ymin, 
                                              xmin, ymin, 
                                              xmin, ymax), 
                                            byrow = TRUE, 
                                            ncol = 2)))
      return(square_poly)
    })
  
  new_obj <-
    obj %>%
    st_drop_geometry() %>% 
    dplyr::mutate(geometry = st_sfc(square_polys, crs = st_crs(obj))) %>% 
    st_as_sf()
  
  return(new_obj) 
}

afd <- read_csv("https://firms.modaps.eosdis.nasa.gov/active_fire/c6/text/MODIS_C6_USA_contiguous_and_Hawaii_24h.csv")

afd <-
  st_as_sf(afd, 
           coords = c("longitude", "latitude"), 
           crs = 4326) %>% 
  st_square_buffer()

plot(afd$geometry)

small <- afd[2:4, ]
plot(small$geometry)

plot(st_union(small))

small
st_convex_hull(small)

plot(st_convex_hull(st_union(small)))
plot(st_convex_hull(small))

test <-
  small %>% 
  dplyr::summarize(mean_frp = mean(frp))

plot(test$geometry)
