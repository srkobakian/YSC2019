# Young statisticians conference presentation plots

library(absmapsdata)
library(tidyverse)
library(cartogram)
library(ggthemes)
library(cowplot)
library(sf)
library(sp)

## States of Australia

states <- absmapsdata::state2011 %>% 
  filter(state_name_2011 != "Other Territories")

# Act pop out plot
act_map <- absmapsdata::state2011 %>% 
  filter(state_name_2011 == "Australian Capital Territory") %>% st_transform(., "+init=epsg:3112") %>% 
  ggplot() + 
  theme_void() +
  geom_sf(fill = "white") + 
  geom_rect(aes(xmin =1323014, xmax = 1404210, ymin=-4132815, ymax=-4026022), fill = NA, colour = "black")


# Australian map
aus_map <- ggplot(states) + 
  geom_sf(aes(fill = albers_sqkm)) +
  scale_fill_distiller(type = "seq", palette = "BuGn",  direction = 1, na.value = "light grey") + 
  theme_void() +
  coord_sf(crs = CRS("+init=epsg:3112")) +
   theme(legend.position ="left", legend.title = element_text("Area (sqkm)"))

ggsave(filename = "figures/aus.png", plot = aus_map,
  device = "png", dpi = 300,  width = 10, height = 8)

# Australian map with pop out
aus_states <- ggdraw() + 
  draw_plot(aus_map, 0, 0, 1, 1) +
  draw_plot(act_map, 0.82, 0.01, 0.17, 0.3)

ggsave(filename = "figures/aus_states.png", plot = aus_states,
  device = "png", dpi = 300,  width = 10, height = 8)


###############################################################################
###########################     ABS AREAS

sa4 <- absmapsdata::sa42011 %>% 
  filter(state_name_2011 != "Other Territories")

sa4_map <- ggplot(sa4) + 
  geom_sf(aes(fill = albers_sqkm)) +
  scale_fill_distiller(type = "seq", palette = "BuGn",  direction = 1, na.value = "light grey") + 
  theme_void() +
  coord_sf(crs = CRS("+init=epsg:3112")) +
  theme(legend.position ="left", legend.title = element_text("Area (sqkm)"))

ggsave(filename = "figures/aus_sa4.png", plot = sa4_map,
  device = "png", dpi = 300,  width = 10, height = 8)

## sa3

sa3 <- absmapsdata::sa32011 %>% 
  filter(state_name_2011 != "Other Territories")

sa3_map <- ggplot(sa3) + 
  geom_sf(aes(fill = albers_sqkm)) +
  scale_fill_distiller(type = "seq", palette = "BuGn",  direction = 1, na.value = "light grey") + 
  theme_void() +
  coord_sf(crs = CRS("+init=epsg:3112")) +
   theme(legend.position ="left", legend.title = element_text("Area (sqkm)"))

ggsave(filename = "figures/aus_sa3.png", plot = sa3_map,
  device = "png", dpi = 300,  width = 10, height = 8)


## sa2
sa2 <- absmapsdata::sa22011 %>% 
  filter(state_name_2011 != "Other Territories")

sa2_map <- ggplot(sa2) + 
  geom_sf(aes(fill = albers_sqkm)) +
  scale_fill_distiller(type = "seq", palette = "BuGn",  direction = 1, na.value = "light grey") + 
  theme_void() +
  coord_sf(crs = CRS("+init=epsg:3112")) +
   theme(legend.position ="left", legend.title = element_text("Area (sqkm)"))

ggsave(filename = "figures/aus_sa2.png", plot = sa2_map,
  device = "png", dpi = 300,  width = 10, height = 8)



###############################################################################
###########################     CARTOGRAMS

ERP <- read_csv("data/ERP.csv")

ERP_sa4 <- ERP %>% filter(`Geography Level` == "Statistical Area Level 4", Time == 2011) %>% arrange(`Geography Level`)
  
sa4$ERP
  left_join(sa4, by = c("Geography Level" = "sa4_name_2011"))  %>% 
  st_as_sf() %>% 
  filter(!st_is_empty(geometry)) %>% 
  cartogram_cont(.,
    weight = "Value", itermax = 15)
  

aus_ggcont <- ggplot(sa4_cont) + 
  geom_sf(aes(fill = `Age-standardised rate (per 100,000)`)) + 
  scale_fill_distiller(type = "seq", palette = "Purples",  direction = 1) + 
  coord_sf(crs = CRS("+init=epsg:3112"), xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_void()+guides(fill=FALSE)





sa2 <- sa2 %>% 
  filter((sa2_name_2011 %in% SIR$SA2_name)) 

###############################################################################
###########################     CANCER DATA

# Cancer data for SA2 areas Australia
SIR <- read_csv("data/SIR Downloadable Data.csv")

SIR_persons <- SIR %>% 
  filter(Year=="2010-2014") %>% 
  filter(Sex_name =="Persons") %>% 
  select(Cancer_name, SA2_name, p50) %>% 
  spread(Cancer_name, p50) %>% 
  left_join(sa2, ., by = c("sa2_name_2011"="SA2_name"))



SIR_females <- SIR %>% 
  filter(Year=="2005-2014") %>% 
  filter(Sex_name =="Females") %>% 
  select(Cancer_name, SA2_name, p50) %>% 
  spread(Cancer_name, p50) %>% 
  left_join(sa2, ., by = c("sa2_name_2011"="SA2_name"))


SIR_males <- SIR %>% 
  filter(Year=="2005-2014") %>% 
  filter(Sex_name =="Males") %>% 
  select(Cancer_name, SA2_name, p50) %>% 
  spread(Cancer_name, p50) %>% 
  left_join(sa2, ., by = c("sa2_name_2011"="SA2_name"))

# function to allocate colours to regions
aus_colours <- function(sir_p50){
  
    ifelse(sir_p50 < 0.5, return("#33809d"),
    ifelse(sir_p50 >= 0.5 & sir_p50 < 1, return("#aec6c7"),
    ifelse(sir_p50 >= 1 & sir_p50 < 1.5, return("#fff4bc"),
    ifelse(sir_p50 >= 1.5 & sir_p50 < 2, return("#ff9a64"),
    ifelse(sir_p50 >= 2, return("#ff3500"))))))
}

aus_liver_f <- SIR_females %>% 
  mutate(colour_SIR = map(Liver, aus_colours)) %>% 
ggplot() + 
  geom_sf(aes(fill = colour_SIR)) + theme_map()

ggsave(filename = "figures/aus_liver_f.png", plot = aus_liver_f,
  device = "png", dpi = 300,  width = 10, height = 10)



aus_lung_p <- SIR_persons %>% 
  mutate(colour_SIR = map(Lung, aus_colours)) %>% 
  ggplot() + 
  geom_sf(aes(fill = colour_SIR)) + theme_map()

ggsave(filename = "figures/aus_lung_p.png", plot = aus_lung_p,
  device = "png", dpi = 300,  width = 10, height = 10)



aus_melanoma_p <- SIR_persons %>% 
  mutate(colour_SIR = map(Melanoma, aus_colours)) %>% 
  ggplot() + 
  geom_sf(aes(fill = colour_SIR)) + theme_map()

ggsave(filename = "figures/aus_melanoma_p.png", plot = aus_melanoma_p,
  device = "png", dpi = 300,  width = 10, height = 10)



###############################################################################
###########################     SUGARBAG
library(sugarbag)
sf_id <- "sa2_name_2011"


# Create centroids set
centroids <- create_centroids(sa2, "sa2_name_2011")
# Create hexagon location grid
grid <- create_grid(centroids = centroids, hex_size = 0.2, buffer_dist = 12)
# Allocate polygon centroids to hexagon grid points
hex_allocated <- allocate(centroids = centroids,
  hex_grid = grid,
  sf_id = "sa2_name_2011",
  hex_size = 0.18, # same size used in create_grid
  hex_filter = 10,
  focal_points = capital_cities,
  width = 30, verbose = TRUE) # same column used in create_centroids

save(hex_allocated, file = "hex_allocated.rda")



###############################################################################
###########################     SUGARBAG Tasmania

tas_sa2 <- absmapsdata::sa22011 %>% filter(state_name_2011 == "Tasmania") 

fort_sa2 <- tas_sa2 %>% fortify_sfc()

hex_size <- .25
buffer_dist <- 2

centroids <- sugarbag::create_centroids(shp_sf = tas_sa2, sf_id = sf_id)

bbox <- tibble::tibble(min = c(min(centroids$longitude), min(centroids$latitude)),
  max = c(max(centroids$longitude), max(centroids$latitude)))

########################### Tas square grid

grid <- tibble::as_tibble(expand.grid(hex_long = seq(bbox$min[1] - buffer_dist,
  bbox$max[1] + buffer_dist,
  hex_size),
  hex_lat = seq(bbox$min[2] - buffer_dist,
    bbox$max[2] + buffer_dist,
    hex_size)))

t1 <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_sa2, fill = "grey", colour = "white") + 
  geom_point(aes(x=longitude, y = latitude), data= centroids, colour = "#2d4713") + theme_void() + 
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#1f78b4", data = grid, size = 0.75) +
  coord_equal()

ggsave(filename = "figures/tas_1grid.png", plot = t1,
  device = "png", dpi = 300,  width = 10, height = 10)


########################### Tas hexagon grid
# Find every second latitude
shift_lat <- grid %>% dplyr::select(hex_lat) %>%
  dplyr::distinct() %>%
  dplyr::filter(dplyr::row_number() %% 2 == 1) %>% unlist()

# Shift the longitude of every second latitude to the right to make hex structure
grid <- grid %>%
  dplyr::mutate(hex_long = ifelse(hex_lat %in% shift_lat, hex_long,
    hex_long + (hex_size / 2))) %>%
  dplyr::mutate(id=1:NROW(.))  %>%
  dplyr::mutate(assigned=FALSE)


t2 <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_sa2, fill = "grey", colour = "white") + 
  geom_point(aes(x=longitude, y = latitude), data= centroids, colour = "#2d4713") + theme_void() + 
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#1f78b4", data = grid, size = 0.75) +
  coord_equal()

ggsave(filename = "figures/tas_2hexgrid.png", plot = t2,
  device = "png", dpi = 300,  width = 10, height = 10)



########################### Tas hexagon grid, Rolling windows
full_grid <- grid <- grid %>%
  mutate(hex_long_int = dense_rank(hex_long)-1,
    hex_lat_int = dense_rank(hex_lat)-1)


nlong <- length(unique(grid$hex_long))
nlat <- length(unique(grid$hex_lat))

centroids <- centroids %>%
  mutate(long_int = round((longitude-min(grid$hex_long))/(max(grid$hex_long)-min(grid$hex_long))*nlong, 0),
    lat_int = round((latitude-min(grid$hex_lat))/(max(grid$hex_lat)-min(grid$hex_lat))*nlat, 0))

# Amount of lats and longs in each group
lat_size = round(nlat/10,0)
long_size = round(nlong/10,0)


# make a list of groups, manual sliding windows
nlat_list <- purrr::map2(seq(1:nlat), lat_size + seq(1:nlat), c)
nlong_list <- purrr::map2(seq(1:nlong), long_size + seq(1:nlong), c)


lat_window <- function(x, cents = centroids, maximum = nlat){
  max_int = min(x[2],maximum)
  
  cents_in <- filter(cents, between(lat_int, x[1], max_int))
  return(cents_in)
}

long_window <- function(x, cents = centroids, maximum = nlong){
  max_int = x[2]
  while (max_int > maximum){
    max_int = max_int - 1
  }
  
  cents_in <- filter(cents, between(long_int, x[1], max_int))
  return(cents_in)
}


# LATITUDE ROWS FILTER
# amount of latitude in sliding window
lat_windows <- purrr::map(.x = nlat_list, .f = lat_window)

# find the min and max longitude for each latitude
range_rows <- purrr::map_dfr(.x = lat_windows,
  .f = function(x) {x %>%
      dplyr::summarise(
        long_min = ifelse(purrr::is_empty(long_int), NA, min(x$long_int)),
        long_max = ifelse(purrr::is_empty(long_int), NA, max(x$long_int))
      )}
)


## ----echo = FALSE--------------------------------------------------------
# smooth the minimums
av_range_rows <- purrr::map_dfr(.x = nlat_list, .f = function(x, rows = range_rows) {
  rows[x[1]:min(x[2], NROW(rows)),] %>%
    dplyr::summarise(mean_long_min = mean(long_min, na.rm=T), mean_long_max = mean(long_max, na.rm=T))
}) %>%
  bind_cols(lat_id = c(seq(1:nlat) +lat_size), .)

# LONGITUDE COLS FILTER
long_windows <- purrr::map(.x = nlong_list, .f = long_window, centroids, nlong)

# find the min and max longitude for each latitude
range_cols <- purrr::map_dfr(.x = long_windows, .f = function(x) { x %>%
    dplyr::summarise(
      lat_min = ifelse(purrr::is_empty(lat_int), NA, min(x$lat_int)),
      lat_max = ifelse(purrr::is_empty(lat_int), NA, max(x$lat_int))
    )}
)

# smooth the minimums
av_range_cols <- purrr::map_dfr(.x = nlong_list, .f = function(x, cols = range_cols) {
  cols[x[1]:min(x[2], NROW(cols)),] %>%
    dplyr::summarise(mean_lat_min = mean(lat_min, na.rm=T), mean_lat_max = mean(lat_max, na.rm=T))
}) %>%
  bind_cols(long_id = c(seq(1:nlong) + round(long_size/2)), .)

# function to replace Nan values
 checkNaN <- function(x){ifelse(x == "NaN", NA, x)}
# APPLY A BUFFER
# change buffer to amount of hexagons (ints) either side
hex_buffer <- floor(buffer_dist/hex_size)

grid <- grid %>%
  left_join(., av_range_rows, by = c("hex_lat_int" = "lat_id")) %>%
  left_join(., av_range_cols, by = c("hex_long_int" = "long_id")) %>%
  rowwise() %>% 
  mutate_at(vars(contains('mean')), .funs = checkNaN) %>% 
  mutate(long_buffer = ifelse(between(hex_long_int,mean_long_min - hex_buffer,
    mean_long_max + hex_buffer), "in", "out")) %>%
  mutate(lat_buffer = ifelse(between(hex_lat_int,mean_lat_min - hex_buffer,
    mean_lat_max + hex_buffer), "in", "out")) %>%
  filter(lat_buffer =="in" | long_buffer == "in")


t3 <- ggplot() + 
   geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_sa2, fill = "grey", colour = "white") + 
  geom_point(aes(x=longitude, y = latitude),data= centroids, colour = "#2d4713") + 
  theme_void() + coord_equal() +
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#a6cee3", data = full_grid, size = 0.25) +
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#1f78b4", data = grid, size = 0.75)

ggsave(filename = "figures/tas_3buffer.png", plot = t3,
  device = "png", dpi = 300,  width = 10, height = 10)


## Fucntions:

unnest_tbl <- function(.data, tbl_col, .sep = NULL){
  row_indices <- rep.int(seq_len(NROW(.data)), purrr::map_int(.data[[tbl_col[[1]]]], NROW))
  
  nested_cols <- purrr::map(tbl_col, function(x){
    lst_col <- .data[[x]]
    if(is.data.frame(lst_col[[1]])){
      dplyr::bind_rows(!!!set_names(lst_col, rep(x, length(lst_col))))
    }
    else{
      rlang::list2(!!x := unlist(lst_col))
    }
  })
  
  if(!is.null(.sep)){
    nested_cols <- purrr::map2(
      nested_cols, tbl_col,
      function(x, nm) set_names(x, paste(nm, colnames(x), sep = .sep))
    )
  }
  
  dplyr::bind_cols(
    .data[row_indices, setdiff(names(.data), tbl_col), drop = FALSE], # Parent cols
    !!!nested_cols # Nested cols
  )
}

closest_focal_point <- function(centroid, focal_points) {
  
  if ("long" %in% colnames(focal_points)) {
    colnames(focal_points)[which(colnames(focal_points) == "long")] <- "longitude"
  }
  if ("lat" %in% colnames(focal_points)) {
    colnames(focal_points)[which(colnames(focal_points) == "lat")] <- "latitude"
  }
  
  if(!all(c("longitude", "latitude") %in% colnames(focal_points))){
    abort('The `focal_points` must contain the columns "longitude", and "latitude"')
  }
  
  # When applying to hexagon grid
  if ("hex_long" %in% colnames(focal_points)) {
    colnames(focal_points)[which(colnames(focal_points) == "hex_long")] <- "longitude"
    
    colnames(focal_points)[which(colnames(focal_points) == "hex_lat")] <- "latitude"
  }
  
  
  # create a martix for distance calculations
  fp_matrix <- as.matrix(focal_points[c("longitude", "latitude")])
  
  focal_distance <- geosphere::distVincentyEllipsoid(
    c(centroid$longitude, centroid$latitude), fp_matrix,
    a = 6378110, b = 6356774.719, f = 1 / 298.257222101
  )
  
  # closest point
  focal_distance_df <- focal_points %>% 
    dplyr::bind_cols(., focal_distance = focal_distance) %>% 
    top_n(-1, wt = focal_distance)
  
  # angle from city to centroid
  focal_distance_df$angle <- geosphere::finalBearing(
    focal_distance_df[, c("longitude", "latitude")],
    c(centroid$longitude, centroid$latitude),
    a = 6378110, f = 0)
  
  # ensure no name clashes
  focal_distance_df <- focal_distance_df %>% 
    rename(focal_longitude = longitude, focal_latitude = latitude)
  
  return(focal_distance_df)
}


## ----split_centroids-----------------------------------------------------
# Split the centroid data set

centroids <- centroids %>% tidyr::nest(data = c(longitude, latitude)) %>% 
  mutate(closest = purrr::map(data, 
  closest_focal_point, focal_points = tibble(points= "Hobart", longitude = 147.32941, latitude = -42.87936))) %>% 
  unnest(c("data", "closest")) %>% 
  arrange(focal_distance) %>% 
  dplyr::ungroup() %>% 
  mutate(rownumber = row_number())



## ----f_dist--------------------------------------------------------------
## Not necessary when using `create_hexmap`:
## Consider hexagon points up to a certain distance from centroid

hex_filter <- (hex_size)*10


## ------------------------------------------------------------------------
width = 35

## ----arrange-------------------------------------------------------------
s_centroids <- centroids %>% arrange(focal_distance)

s_centroids <- split(s_centroids, s_centroids[["rownumber"]])

# Set up allocation data frame
centroid_allocation <- NULL


## ----orig_dist-----------------------------------------------------------
# keep value to reset expanded distances
expand_dist <- hex_filter


## ----available_points----------------------------------------------------
# filter for only the available hex grid points
hex_grid <- grid %>% filter(!assigned)


## ----close_points--------------------------------------------------------
# filter grid for avaiable points
centroid1 <- centroids %>% head(1)

flong <- centroid1$longitude
flat <- centroid1$latitude

hex_grid <- hex_grid %>% ungroup() %>%
  filter(flat - hex_filter < hex_lat & hex_lat < flat + hex_filter) %>%
  filter(flong - hex_filter < hex_long & hex_long < flong + hex_filter)



t4 <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_sa2, fill = "grey", colour = "white") + 
  geom_point(aes(x = focal_longitude, y = focal_latitude), data= centroid1, colour = "red", size = 3) +  geom_point(aes(x=longitude, y = latitude), data= centroid1, colour = "#2d4713", size = 3) + theme_void() + 
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#1f78b4", data = grid, size = 0.75) +
geom_point(aes(x = hex_long, y = hex_lat), colour = "#1f78b4", data = hex_grid, size = 0.75)+
  coord_equal()

ggsave(filename = "figures/tas_4centroid1.png", plot = t4,
  device = "png", dpi = 300,  width = 10, height = 10)


## ------------------------------------------------------------------------
hex_grid <- hex_grid %>%
  rowwise %>%
  mutate(
    hex_lat_c = hex_lat - flat,
    hex_long_c = hex_long - flong) %>%
  mutate(hyp = ((hex_lat_c^2) + (hex_long_c^2))^(1/2))


f_angle <- centroid1 %>%
  mutate(atan = atan2(latitude-focal_latitude,longitude-focal_longitude),
    angle = (atan*180/pi),
    pangle = ifelse(angle<0, angle +360, angle)) %>% pull()


hex_grid <- hex_grid %>%
  # create circle of radius: hex_filter
  filter(hyp < hex_filter) %>%
  mutate(
    # geosphere takes a long time
    angle = f_angle,
    angle_plus = (angle + width)%%360,
    angle_minus = (angle - width)%%360,
    atan = atan2(hex_lat_c, hex_long_c),
    hex_angle = (atan*180/pi),
    hex_angle = ifelse(hex_angle<0, hex_angle +360, hex_angle))


t5 <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_sa2, fill = "grey", colour = "white") +
  theme_void() + coord_equal() +
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#a6cee3", data = grid, size = 0.25) +
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#1f78b4", data = hex_grid, size = 0.75) + 
  geom_point(aes(x=longitude, y = latitude), data= centroid1, colour = "#2d4713", size = 3) +
  geom_point(aes(x = focal_longitude, y = focal_latitude), data= centroid1, colour = "red", size = 3)

ggsave(filename = "figures/tas_5centroid1.png", plot = t5,
  device = "png", dpi = 300,  width = 10, height = 10)



## ------------------------------------------------------------------------
# Filter for angle within circle

if (hex_grid$angle_minus[1] < hex_grid$angle_plus[1]) {
  hex_grid <- hex_grid %>%
    # create slice of 60 degrees from centroid
    filter(angle_minus < hex_angle & hex_angle < angle_plus)
} else {
  hex_grid <- hex_grid %>%
    # create slice of 60 degrees from centroid
    filter(hex_angle < angle_plus | angle_minus > hex_angle)
}



## ------------------------------------------------------------------------
t6 <- ggplot() + geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_sa2, fill = "grey", colour = "white") +
  theme_void() + coord_equal() +
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#a6cee3", data = grid, size = 0.25) +
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#1f78b4", data = hex_grid, size = 0.75) + 
  geom_point(aes(x = focal_longitude, y = focal_latitude), data= centroid1, colour = "red", size = 3) +    geom_point(aes(x=longitude, y = latitude), data= centroid1, colour = "#2d4713", size = 3) 

ggsave(filename = "figures/tas_6centroid1.png", plot = t6,
  device = "png", dpi = 300,  width = 10, height = 10)

t7 <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_sa2 %>% filter(sa2_name_2011 == centroid1$sa2_name_2011), fill = "grey", colour = "white") +
  theme_void() + coord_equal() +
  geom_point(aes(x = hex_long, y = hex_lat, label = hex_angle), colour = "#1f78b4", data = hex_grid, size = 0.75) + 
  geom_point(aes(x = focal_longitude, y = focal_latitude), data= centroid1, colour = "red", size = 3) + 
  geom_point(aes(x=longitude, y = latitude), data= centroid1, colour = "#2d4713", size = 3) 

ggsave(filename = "figures/tas_7centroid1.png", plot = t7,
  device = "png", dpi = 300,  width = 10, height = 10)
## ------------------------------------------------------------------------

# Choose first available point
cent <- centroid1 %>% dplyr::rename(focal_point = points, focal_dist = focal_distance, focal_angle = angle)

# Filter should give one hex point
hex <- hex_grid %>% 
  ungroup %>% 
  filter(hyp == min(hyp)) %>%
  select(hex_long, hex_lat, hex_id = id)

#update grid to show this centroid as assigned
hex_grid[which(hex_grid$id == hex$hex_id),]$assigned <- TRUE

centroid_allocation <- bind_rows(centroid_allocation, dplyr::bind_cols(cent, hex)) %>% as_tibble()



t8 <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_sa2 %>% filter(sa2_name_2011 == centroid1$sa2_name_2011), fill = "grey", colour = "white") +
  theme_void() + coord_equal() +
  geom_point(aes(x = hex_long, y = hex_lat, label = hex_angle), colour = "#1f78b4", data = hex_grid, size = 0.75) + 
  geom_point(aes(x = focal_longitude, y = focal_latitude), data= centroid1, colour = "red", size = 3) + 
  geom_point(aes(x=longitude, y = latitude), data= centroid1, colour = "#2d4713", size = 3) + 
  geom_point(aes(x=hex_long, y = hex_lat), data= hex, colour = "#e2850b", size = 5) 

ggsave(filename = "figures/tas_8centroid1.png", plot = t8,
  device = "png", dpi = 300,  width = 10, height = 10)


## ----fullhexmap----------------------------------------------------------
# Create centroids set
centroids <- create_centroids(tas_sa2, "sa2_name_2011")
# Create hexagon location grid
grid <- create_grid(centroids = centroids, hex_size = 0.2, buffer_dist = 1.2)
# Allocate polygon centroids to hexagon grid points
hex_allocated <- allocate(centroids = centroids,
  hex_grid = grid,
  sf_id = "sa2_name_2011",
  hex_size = 0.2, # same size used in create_grid
  hex_filter = 10,
  focal_points = tibble(points= "Hobart", longitude = 147.32941, latitude = -42.87936),
  width = 30, verbose = FALSE) # same column used in create_centroids


## ------------------------------------------------------------------------
h1 <- hex_allocated %>%
  fortify_hexagon(hex_size = 0.2, sf_id = sf_id) %>%
  left_join(., tas_sa2)

p1 <- fortify_sfc(tas_sa2)

end_hex <- ggplot() +
  geom_polygon(data = p1, aes(x=long, lat, group = interaction(sa2_name_2011, polygon)), alpha = 0.1) +
  geom_polygon(data = h1, aes(x=long, lat, group = sa2_name_2011), fill = "#2d4713") + theme_void() + coord_equal()

end_cents <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_sa2, fill = "grey", colour = "white") + 
  geom_point(aes(x=longitude, y = latitude), data= centroids, colour = "#2d4713", size = 2) + theme_void() + 
  coord_equal()



