# ===========================================
# Step 0: Install and load necessary packages
# ===========================================
library(rvest)
library(tidyverse)
library(sf)
library(dplyr)
library(osmdata)
library(future)
library(furrr)
library(ggspatial)



# ===============================================
# Step 1: Get all CSV file links from the webpage
# ===============================================
setwd("~/Documents/Consulting/Code/")
base_url <- "https://archive.sensor.community/2025-01-01/"
save_dir <- "sensor_data_test"
page <- read_html(base_url)

# Define the target sensor types
target_sensors <- c("bme280", "bmp180", "dht22", "sds011")
sensor_pattern <- paste(target_sensors, collapse = "|")

# Extract hrefs and filter IMMEDIATELY
file_list <- page %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  keep(~ str_detect(., "\\.csv$")) %>%
  keep(~ str_detect(., sensor_pattern))

cat(paste0("Found ", length(file_list), " matching CSV files (bme280, bmp180, dht22, sds011).\n"))


# ==========================================
# Step 2: Parallel Download & Processing
# ==========================================
n_cores <- parallel::detectCores() - 1
plan(multisession, workers = n_cores) 

# Define a function to handle a single file
process_single_file <- function(file_name) {
  file_url <- paste0(base_url, file_name)
  dest_path <- file.path(save_dir, file_name)
  
  # Download if missing
  if (!file.exists(dest_path)) {
    tryCatch({
      download.file(file_url, destfile = dest_path, mode = "wb", quiet = TRUE)
    }, error = function(e) {
      return(NULL)
    })
  }
  
  # Read first row for location
  if (file.exists(dest_path)) {
    tryCatch({
      # Only read header (n_max = 1)
      header_data <- read_delim(dest_path, delim = ";", n_max = 1, 
                                col_select = c("lat", "lon"), 
                                show_col_types = FALSE)
      
      # Check if data exists and return a tibble
      if (nrow(header_data) > 0) {
        return(tibble(
          filename = file_name,
          lat = as.numeric(header_data$lat),
          lon = as.numeric(header_data$lon)
        ))
      }
    }, error = function(e) {
      return(NULL)
    })
  }
  return(NULL)
}

# Execute in parallel
sensor_locations <- future_map_dfr(file_list, process_single_file, .progress = TRUE)


# ==========================================================
# Step 3: Get Munich city boundary (Admin Level 9)
# ==========================================================
q <- opq(bbox = "Munich, Germany", timeout = 600) %>%
  add_osm_feature(key = "admin_level", value = "9") %>% 
  osmdata_sf()

districts <- q$osm_multipolygons
districts <- st_transform(districts, 4326) %>% st_make_valid()
city_boundary <- st_union(districts)
  
# Clean map: Remove exclaves and smooth
all_polys <- st_cast(city_boundary, "POLYGON")
city_boundary <- all_polys[which.max(st_area(all_polys))]
city_boundary <- st_simplify(city_boundary, preserveTopology = TRUE, dTolerance = 0.001)
districts <- st_intersection(districts, city_boundary)


# ============================================
# Step 4: Filter sensors located within Munich
# ============================================
sensors_sf <- st_as_sf(sensor_locations, coords = c("lon", "lat"), crs = 4326)
sensors <- st_filter(sensors_sf, city_boundary)


# ==========================================================
# Step 5: Data Aggregation (Handle Overlaps)
# ==========================================================
sensors_agg <- sensors %>%
  mutate(raw_type = str_extract(filename, "bme280|bmp180|dht22|sds011")) %>%
  filter(!is.na(raw_type)) %>%
  group_by(geometry) %>%
  summarise(
    sensor_type = paste(sort(unique(raw_type)), collapse = "+"),
    count = n(),
    .groups = "drop"
  )
  cat(paste("Unique Locations:", nrow(sensors_agg), "\n"))
  print(table(sensors_agg$sensor_type)) # Check this to see which combinations exist

sds011_sensor_names <- sensors %>%
  st_drop_geometry() %>%
  filter(str_detect(filename, "sds011")) %>%
  mutate(sensor_name = str_extract(filename, "sds011_sensor_\\d+")) %>%
  distinct(sensor_name) %>%
  pull(sensor_name)
print(sds011_sensor_names)

# ==========================================================
# Step 6: Get Roads (Mittlerer Ring & Specific Streets)
# ==========================================================
# 1. Fetch Mittlerer Ring (B 2 R)
q_ring <- opq(bbox = "Munich, Germany", timeout = 600) %>%
  add_osm_feature(key = "ref", value = c("B 2 R", "B 2R", "B2R")) %>% 
  osmdata_sf()

ring <- q_ring$osm_lines

#ring <- q_ring$osm_lines
#ring <- q_ring$osm_multilines # Check multilines

# 2. Fetch Specific Streets (Moosacher Str. & Landshuter Allee)
target_streets <- c("Moosacher Straße", "Landshuter Allee")
q_streets <- opq(bbox = "Munich, Germany", timeout = 600) %>%
  add_osm_feature(key = "name", value = target_streets) %>% 
  osmdata_sf()

highlight_streets <- q_streets$osm_lines

# Transform to correct CRS
ring <- st_transform(ring, 4326)
highlight_streets <- st_transform(highlight_streets, 4326)

# =========================================================
# Step 7: Visualization (SDS011)
# =========================================================
# Filter for SDS011 Sensors ---
sensors_sds011 <- sensors_agg %>% filter(str_detect(sensor_type, "sds011"))

pattern <- paste0(
  "^[0-9]{4}-[0-9]{2}-[0-9]{2}_(",
  paste(sds011_sensor_names, collapse = "|"),
  ")\\.csv$"
)
sds011_sf <- sensors_sf %>%
  filter(str_detect(filename, pattern))
  
# Split Streets (Keep distinct colors for context) ---
moosacher <- NULL
landshuter <- NULL
moosacher <- highlight_streets %>% filter(str_detect(name, "Moosacher"))
landshuter <- highlight_streets %>% filter(str_detect(name, "Landshuter"))
  
# Create Plot ---
p <- ggplot() +
  # 1. Background
  geom_sf(data = districts, fill = "#F5F5F5", color = "white", linewidth = 0.5) +
      
  # Mittlerer Ring (Blue Line)
  geom_sf(data = ring, color = "#B0C4DE", linewidth = 1.2, alpha = 0.6) +
      
  # Highlighted Street 1: Moosacher Str. (Orange)
  geom_sf(data = moosacher, color = "#FF8C00", linewidth = 1.5, alpha = 1) + #FF8C00
      
  # 4. Highlighted Street 2: Landshuter Allee (Deep Pink)
  geom_sf(data = landshuter, color = "#C71585", linewidth = 1.5, alpha = 1) +
      
  # 5. Sensors (SDS011)
  geom_sf(data = sds011_sf, 
          color = "#66CDAA",
          shape = 19, size = 3, alpha = 0.85) +
      
  # 6. Labels for Streets
  geom_sf_text(data = moosacher[1,], aes(label = "Moosacher Str."), 
               nudge_x = 0.012, nudge_y = 0.007, color = "#FF8C00", size = 4, fontface = "bold") +
      
  geom_sf_text(data = landshuter[max(1, floor(nrow(landshuter)/2)),], aes(label = "Landshuter Allee"), 
               nudge_x = 0.0235, nudge_y = 0.017, color = "#C71585", size = 4, fontface = "bold") +
  
  geom_sf_text(data = landshuter[max(1, floor(nrow(landshuter) / 2)),], aes(label = "Mittlerer Ring"), 
               nudge_x = 0.016, nudge_y = -0.036, color = "#B0C4DE", size = 4, fontface = "bold") +
      
  # Scale & Arrow
  annotation_scale(location = "bl", width_hint = 0.3, bar_cols = c("black", "white")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                             style = north_arrow_fancy_orienteering,
                             height = unit(1, "cm"), width = unit(1, "cm")) +
      
  # Theme
  theme_void() + 
  labs(title = "Sensor Locations in Munich",
           subtitle = paste("Active outdoors Sensors:", nrow(sds011_sf)),
           caption = "Blue: Mittlerer Ring | Orange: Moosacher Str. | Pink: Landshuter Allee") +
  
  theme(
    plot.title = element_text(size = 25, face = "bold"),
    plot.subtitle = element_text(size = 18),
    plot.caption = element_text(size = 13)
  )
    
print(p)



# =========================================================
# Step 8: Extract SDS011 sensors and re-group
# =========================================================
sds011_sensor_107_names <- sds011_sf %>%
  st_drop_geometry() %>%
  mutate(sensor_name = str_extract(filename, "sds011_sensor_\\d+")) %>%
  pull(sensor_name)
print(sds011_sensor_107_names)

crs_m <- 25832
sensors_m   <- st_transform(sds011_sf, crs_m)
ring_m      <- st_transform(ring, crs_m)
moosacher_m <- st_transform(moosacher, crs_m)
landshuter_m<- st_transform(landshuter, crs_m)

near_ring <- st_is_within_distance(
  sensors_m, ring_m, dist = 400   #670
) %>% lengths() > 0

streets_m <- rbind(moosacher_m, landshuter_m)

near_streets <- st_is_within_distance(
  sensors_m, streets_m, dist = 400
) %>% lengths() > 0

ring_polygon <- st_polygonize(st_union(ring_m))
inside_ring <- st_within(sensors_m, ring_polygon, sparse = FALSE)[,1]

sensors_classified <- sensors_m %>%
  mutate(
    group = case_when(
      near_streets ~ "Moosacher / Landshuter",
      near_ring | inside_ring ~ "Mittlerer Ring",
      TRUE ~ "City Background"
    )
  )

p2 <- ggplot() +
  geom_sf(data = districts, fill = "#F5F5F5", color = "white", linewidth = 0.5) +
  geom_sf(data = ring, color = "#B0C4DE", linewidth = 1.2, alpha = 0.6) +
  geom_sf(data = moosacher, color = "#FF8C00", linewidth = 1.5) +
  geom_sf(data = landshuter, color = "#C71585", linewidth = 1.5) +
  geom_sf(
    data = sensors_classified,
    aes(color = group),
    shape = 19, size = 3, alpha = 0.85
  ) +
  scale_color_manual(
    values = c(
      "Mittlerer Ring" = "#1F78B4",
      "Moosacher / Landshuter" = "#E31A1C",
      "City Background" = "#33A02C"
    ),
    name = "Sensor location"
  ) +
  geom_sf_text(data = moosacher[1,], aes(label = "Moosacher Str."), 
               nudge_x = 0.012, nudge_y = 0.007,
               color = "#FF8C00", size = 4, fontface = "bold") +
  geom_sf_text(data = landshuter[max(1, floor(nrow(landshuter)/2)),],
               aes(label = "Landshuter Allee"),
               nudge_x = 0.0235, nudge_y = 0.017,
               color = "#C71585", size = 4, fontface = "bold") +
  geom_sf_text(data = landshuter[max(1, floor(nrow(landshuter) / 2)),],
               aes(label = "Mittlerer Ring"),
               nudge_x = 0.016, nudge_y = -0.036,
               color = "#B0C4DE", size = 4, fontface = "bold") +
  annotation_scale(location = "bl", width_hint = 0.3,
                   bar_cols = c("black", "white")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"),
                         width = unit(1, "cm")) +
  theme_void() + 
  labs(
    title = "Sensor Locations in Munich",
    subtitle = paste("Active outdoor sensors:", nrow(sensors_classified)),
    caption = "Grouped by proximity (≤300 m)"
  ) +
  theme(
    plot.title = element_text(size = 25, face = "bold"),
    plot.subtitle = element_text(size = 18),
    plot.caption = element_text(size = 13),
    legend.position = c(1.1, 1.02),
    legend.justification = c(1, 1),
    legend.direction = "horizontal",
    legend.title = element_text(size = 18),
    legend.text  = element_text(size = 14),
    legend.background = element_rect(fill = "transparent", color = NA)
  )
print(p2)



sensors_classified$distance <- as.numeric(
  st_distance(
    sensors_classified$geometry,
    st_transform(
      st_sfc(st_point(c(11.53653, 48.14955)), crs = 4326),
      st_crs(sensors_classified)
    )
  )
)


official_point <- data.frame(
  lon = 11.53653,
  lat = 48.14955,
  filename = "official",
  group = "Official Measurement Station",
  distance = 0
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(sensors_classified))


official_point <- data.frame(
  lon = c(11.53653, 11.46444, 11.64804, 11.55466, 11.56481),
  lat = c(48.14955, 48.18165, 48.17319, 48.15455, 48.13732),
  filename = c("Landshuter Allee Station", "Allach Station", "Johanneskirchen Station", "Lothstraße Station", "Stachus Station"),
  group = c("Official Measurement Station",
            "Official Measurement Station",
            "Official Measurement Station",
            "Official Measurement Station",
            "Official Measurement Station"),
  distance = c(0, 0, 0, 0, 0)
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(sensors_classified))

sensors_classified_official <- dplyr::bind_rows(sensors_classified, official_point)

p3 <- ggplot() +
  geom_sf(data = districts, fill = "#F5F5F5", color = "white", linewidth = 0.5) +
  geom_sf(data = ring, color = "#B0C4DE", linewidth = 1.2, alpha = 0.6) +
  geom_sf(data = moosacher, color = "#C71585", linewidth = 1.5) +
  geom_sf(data = landshuter, color = "#C71585", linewidth = 1.5) +
  geom_sf(
    data = sensors_classified_official,
    aes(color = group, shape = group, size = group, alpha = group)
  ) +
  scale_color_manual(
    values = c(
      "Mittlerer Ring" = "#1F78B4",
      "Moosacher / Landshuter" = "#E31A1C",
      "City Background" = "#33A02C",
      "Official Measurement Station" = "#FFD100"
    ),
    name = "Sensor location"
  ) +
  scale_shape_manual(
    values = c(
      "Mittlerer Ring" = 19,
      "Moosacher / Landshuter" = 19,
      "City Background" = 19,
      "Official Measurement Station" = 17
    ),
    name = "Sensor location"
  ) +
  scale_size_manual(
    values = c(
      "Mittlerer Ring" = 3,
      "Moosacher / Landshuter" = 3,
      "City Background" = 3,
      "Official Measurement Station" = 5
    ),
    name = "Sensor location"
  ) +
  scale_alpha_manual(
    values = c(
      "Mittlerer Ring" = 0.85,
      "Moosacher / Landshuter" = 0.85,
      "City Background" = 0.85,
      "Official Measurement Station" = 0.95
    ),
    name = "Sensor location"
  ) +
  geom_sf_text(data = moosacher[1,], aes(label = "Moosacher Str."), 
               nudge_x = 0.012, nudge_y = 0.007,
               color = "#C71585", size = 4, fontface = "bold") + 
  geom_sf_text(data = landshuter[max(1, floor(nrow(landshuter)/2)),],
               aes(label = "Landshuter Allee"),
               nudge_x = 0.0235, nudge_y = 0.017,
               color = "#C71585", size = 4, fontface = "bold") +
  geom_sf_text(data = landshuter[max(1, floor(nrow(landshuter) / 2)),],
               aes(label = "Mittlerer Ring"),
               nudge_x = 0.016, nudge_y = -0.036,
               color = "#B0C4DE", size = 4, fontface = "bold") +
  annotation_scale(location = "bl", width_hint = 0.3,
                   bar_cols = c("black", "white")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"),
                         width = unit(1, "cm")) +
  theme_void() + 
  labs(
    title = "Sensor Locations in Munich",
    subtitle = paste("Outdoor sensors: 107"),
    caption = "Grouped by proximity (≤300 m)"
  ) +
  theme(
    plot.title = element_text(size = 25, face = "bold"),
    plot.subtitle = element_text(size = 18),
    plot.caption = element_text(size = 13),
    legend.position = c(1.17, 1.025),
    legend.justification = c(1, 1),
    legend.direction = "horizontal",
    legend.title = element_text(size = 18),
    legend.text  = element_text(size = 14),
    legend.background = element_rect(fill = "transparent", color = NA)
  )
print(p3)



official_point_2 <- data.frame(
  lon = c(11.53653, 11.46444, 11.64804, 11.55466, 11.56481, 11.5429),
  lat = c(48.14955, 48.18165, 48.17319, 48.15455, 48.13732, 48.1648),  #48.1632
  filename = c("Landshuter Allee Station", "Allach Station", "Johanneskirchen Station", "Lothstraße Station", "Stachus Station", "Munich City"),
  group = c("LfU Air Quality Station",
            "LfU Air Quality Station",
            "LfU Air Quality Station",
            "LfU Air Quality Station",
            "LfU Air Quality Station",
            "DWD Weather Station Munich"),
  distance = c(0, 0, 0, 0, 0, 0)
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(sensors_classified))

sensors_classified_official_2 <- dplyr::bind_rows(sensors_classified, official_point_2)


p4 <- ggplot() +
  geom_sf(data = districts, fill = "#F5F5F5", color = "white", linewidth = 0.5) +
  geom_sf(data = ring, color = "#B0C4DE", linewidth = 1.2, alpha = 0.6) +
  geom_sf(data = moosacher, color = "#C71585", linewidth = 1.5) +
  geom_sf(data = landshuter, color = "#C71585", linewidth = 1.5) +
  geom_sf(
    data = sensors_classified_official_2,
    aes(color = group, shape = group, size = group, alpha = group)
  ) +
  scale_color_manual(
    values = c(
      "Mittlerer Ring" = "#1F78B4",
      "Moosacher / Landshuter" = "#E31A1C",
      "City Background" = "#33A02C",
      "LfU Air Quality Station" = "#FFD100",
      "DWD Weather Station Munich" = "#FFD100"
    ),
    name = "Sensor location"
  ) +
  scale_shape_manual(
    values = c(
      "Mittlerer Ring" = 19,
      "Moosacher / Landshuter" = 19,
      "City Background" = 19,
      "LfU Air Quality Station" = 17,
      "DWD Weather Station Munich" = 15
    ),
    name = "Sensor location"
  ) +
  scale_size_manual(
    values = c(
      "Mittlerer Ring" = 3,
      "Moosacher / Landshuter" = 3,
      "City Background" = 3,
      "LfU Air Quality Station" = 5,
      "DWD Weather Station Munich" = 5
    ),
    name = "Sensor location"
  ) +
  scale_alpha_manual(
    values = c(
      "Mittlerer Ring" = 0.8,
      "Moosacher / Landshuter" = 0.8,
      "City Background" = 0.8,
      "LfU Air Quality Station" = 0.95,
      "DWD Weather Station Munich" = 0.95
    ),
    name = "Sensor location"
  ) +
  geom_sf_text(data = moosacher[1,], aes(label = "Moosacher Str."), 
               nudge_x = 0.012, nudge_y = 0.007,
               color = "#C71585", size = 4, fontface = "bold") + 
  geom_sf_text(data = landshuter[max(1, floor(nrow(landshuter)/2)),],
               aes(label = "Landshuter Allee"),
               nudge_x = 0.0235, nudge_y = 0.017,
               color = "#C71585", size = 4, fontface = "bold") +
  geom_sf_text(data = landshuter[max(1, floor(nrow(landshuter) / 2)),],
               aes(label = "Mittlerer Ring"),
               nudge_x = 0.016, nudge_y = -0.036,
               color = "#B0C4DE", size = 4, fontface = "bold") +
  annotation_scale(location = "bl", width_hint = 0.3,
                   bar_cols = c("black", "white")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"),
                         width = unit(1, "cm")) +
  theme_void() + 
  labs(
    title = "Sensor Locations in Munich",
    subtitle = paste("Outdoor sensors: 107"),
    caption = "Grouped by proximity (≤300 m)"
  ) +
  theme(
    plot.title = element_text(size = 25, face = "bold"),
    plot.subtitle = element_text(size = 18),
    plot.caption = element_text(size = 13),
    legend.position = c(1.25, 1.026),
    legend.justification = c(1, 1),
    legend.direction = "horizontal",
    legend.title = element_text(size = 18),
    legend.text  = element_text(size = 14),
    legend.background = element_rect(fill = "transparent", color = NA)
  )
print(p4)






sensor_stage_all_75 <- intersect(sensor_stage_1_75, sensor_stage_2_75)
sensors_classified$sensor_name <- sub("^.*_(sds011_sensor_[0-9]+)\\.csv$", "\\1", sensors_classified$filename)
sensors_classified_all_stage <- sensors_classified[sensors_classified$sensor_name %in% sensor_stage_all_75, ]

sensors_classified_all_stage$filename <- sensors_classified_all_stage$sensor_name
sensors_classified_all_stage <- sensors_classified_all_stage[, -ncol(sensors_classified_all_stage)]

sensors_classified_all_stage_2 <- dplyr::bind_rows(sensors_classified_all_stage, official_point_2)


sensors_classified_all_stage_2$group <- factor(
  sensors_classified_all_stage_2$group,
  levels = c(
    "Mittlerer Ring",
    "Moosacher / Landshuter",
    "City Background",
    "LfU Air Quality Station",
    "DWD Weather Station Munich"
  )
)



p5 <- ggplot() +
  geom_sf(data = districts, fill = "#F5F5F5", color = "white", linewidth = 0.5) +
  geom_sf(data = ring, color = "#B0C4DE", linewidth = 1.2, alpha = 0.6) +
  geom_sf(data = moosacher, color = "#C71585", linewidth = 1.5) +
  geom_sf(data = landshuter, color = "#C71585", linewidth = 1.5) +
  geom_sf(
    data = sensors_classified_all_stage_2,
    aes(color = group, shape = group, size = group, alpha = group)
  ) +
  scale_color_manual(
    values = c(
      "Mittlerer Ring" = "#1F78B4",
      "Moosacher / Landshuter" = "#E31A1C",
      "City Background" = "#33A02C",
      "LfU Air Quality Station" = "#FFD100",
      "DWD Weather Station Munich" = "#FFD100"
    ),
    name = "Sensor location"
  ) +
  scale_shape_manual(
    values = c(
      "Mittlerer Ring" = 19,
      "Moosacher / Landshuter" = 19,
      "City Background" = 19,
      "LfU Air Quality Station" = 17,
      "DWD Weather Station Munich" = 15
    ),
    name = "Sensor location"
  ) +
  scale_size_manual(
    values = c(
      "Mittlerer Ring" = 3,
      "Moosacher / Landshuter" = 3,
      "City Background" = 3,
      "LfU Air Quality Station" = 5,
      "DWD Weather Station Munich" = 5
    ),
    name = "Sensor location"
  ) +
  scale_alpha_manual(
    values = c(
      "Mittlerer Ring" = 0.8,
      "Moosacher / Landshuter" = 0.8,
      "City Background" = 0.8,
      "LfU Air Quality Station" = 0.95,
      "DWD Weather Station Munich" = 0.95
    ),
    name = "Sensor location"
  ) +
  geom_sf_text(data = moosacher[1,], aes(label = "Moosacher Str."), 
               nudge_x = 0.012, nudge_y = 0.007,
               color = "#C71585", size = 4, fontface = "bold") + 
  geom_sf_text(data = landshuter[max(1, floor(nrow(landshuter)/2)),],
               aes(label = "Landshuter Allee"),
               nudge_x = 0.0235, nudge_y = 0.017,
               color = "#C71585", size = 4, fontface = "bold") +
  geom_sf_text(data = landshuter[max(1, floor(nrow(landshuter) / 2)),],
               aes(label = "Mittlerer Ring"),
               nudge_x = 0.016, nudge_y = -0.036,
               color = "#B0C4DE", size = 4, fontface = "bold") +
  annotation_scale(location = "bl", width_hint = 0.3,
                   bar_cols = c("black", "white")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"),
                         width = unit(1, "cm")) +
  theme_void() + 
  labs(
    title = "Sensor Locations in Munich",
    subtitle = paste("Outdoor sensors: 78"),
    caption = "Grouped by proximity (≤300 m)"
  ) +
  theme(
    plot.title = element_text(size = 25, face = "bold"),
    plot.subtitle = element_text(size = 18),
    plot.caption = element_text(size = 13),
    legend.position = c(1.25, 1.026),
    legend.justification = c(1, 1),
    legend.direction = "horizontal",
    legend.title = element_text(size = 18),
    legend.text  = element_text(size = 14),
    legend.background = element_rect(fill = "transparent", color = NA)
  )
print(p5)






