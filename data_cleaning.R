# ===========================================
# Step 0: Install and load necessary packages
# ===========================================
library(dplyr)
library(lubridate)
library(slider)
library(ggplot2)
library(tidyr)
library(readxl)
library(forecast)
library(ggbreak)
library(stats)
library(zoo)
library(missForest)
library(mgcv)
library(patchwork)


# ===============================================
# Step 1: Data Cleaning
# ===============================================
process_sensor_data <- function(filename) {
  
  base_name <- tools::file_path_sans_ext(filename)
  
  df1 <- read.csv(file.path("stage_1", filename))
  df2 <- read.csv(file.path("stage_2", filename))
  df <- bind_rows(df1, df2)
  
  df <- df %>%
    mutate(
      timestamp = ymd_hms(timestamp, tz = "UTC"),  
      date = as.Date(timestamp),
      hour_bin = floor_date(timestamp, "hour")
    ) %>%
    distinct(timestamp, .keep_all = TRUE) %>%
    select(-sensor_id, -P2, -lat, -lon)  
  
  hampel_filter <- function(x) {
    roll_med <- slide_dbl(x, median, na.rm = TRUE, .before = 3, .after = 3)
    roll_mad <- slide_dbl(x, mad, na.rm = TRUE, .before = 3, .after = 3)
    ifelse(abs(x - roll_med) > 3 * roll_mad, roll_med, x)
  }
  
  df_clean <- df %>%
    group_by(date) %>%
    arrange(timestamp) %>%
    mutate(
      P1 = hampel_filter(P1)
    ) %>%
    ungroup()
  
  THRESHOLD <- 1 
  
  df_hourly <- df_clean %>%
    group_by(hour_bin) %>%
    summarise(
      valid_count = n(),
      `PM2.5` = ifelse(valid_count >= THRESHOLD, mean(P1, na.rm = TRUE), NA),
      .groups = "drop"
    ) %>%
    select(-valid_count) %>%
    rename(timestamp = hour_bin)
  
  df_hourly <- df_hourly %>%
    complete(
      timestamp = seq(
        ymd_hms("2022-02-01 00:00:00", tz = "UTC"), 
        ymd_hms("2025-05-31 23:00:00", tz = "UTC"), 
        by = "hour"
      )
    )
  
  saveRDS(df_hourly, file.path("stage_process", paste0(base_name, ".rds")))
  
  return(df_hourly)
}




sds011_sensor_489 <- process_sensor_data("sds011_sensor_489.csv")
sds011_sensor_12275_process_1 <- process_sensor_data("sds011_sensor_12275.csv")

any(is.na(sds011_sensor_489_process_1$PM2.5))
any(is.na(sds011_sensor_489_process_1$PM10))

#sds011_sensor_489_process_1_1 <- read.csv("stage_process_1/sds011_sensor_489.csv", header = TRUE)
#str(sds011_sensor_489_process_1_1$timestamp)


sds011_sensor_489_process_1$PM2.5[sds011_sensor_489_process_1$PM2.5 > 125] <- NA
sds011_sensor_489_process_1$PM10[sds011_sensor_489_process_1$PM10 > 125] <- NA

sum(is.na(sds011_sensor_489_process_1$PM2.5))
which(is.na(sds011_sensor_489_process_1$PM2.5))

ggplot(sds011_sensor_489_process_1, aes(x = timestamp)) +
  geom_line(aes(y = PM2.5, color = "PM2.5"), na.rm = TRUE) +
  #geom_line(aes(y = PM10, color = "PM10"), na.rm = TRUE) +
  labs(title = "Change",
       x = "Time",
       y = "Value",
       color = "Type") +
  theme_minimal()


ggplot(sds011_sensor_12275_process_1, aes(x = timestamp)) +
  geom_line(aes(y = PM2.5, color = "PM2.5"), na.rm = TRUE) +
  #geom_line(aes(y = PM10, color = "PM10"), na.rm = TRUE) +
  labs(title = "Change",
       x = "Time",
       y = "Value",
       color = "Type") +
  theme_minimal()




sensor_data_list <- lapply(sensor_stage_all_75, function(sensor_name) {
  process_sensor_data(paste0(sensor_name, ".csv"))
})
names(sensor_data_list) <- sensor_stage_all_75


plot_all_sensors <- function(sensor_data_list) {
  
  big_df <- bind_rows(sensor_data_list, .id = "sensor_name")
  
  ggplot(big_df, aes(x = timestamp)) +
    geom_line(aes(y = `PM2.5`), na.rm = TRUE) +
    facet_wrap(~ sensor_name, ncol = 6) +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 6),
      axis.text.x = element_text(size = 5),
      axis.text.y = element_text(size = 5)
    )
}

plot_all_1 <- plot_all_sensors(sensor_data_list)
plot_all_1







process_sensor_data_2 <- function(filename) {
  
  base_name <- tools::file_path_sans_ext(filename)
  
  df1 <- read.csv(file.path("stage_1", filename))
  df2 <- read.csv(file.path("stage_2", filename))
  df <- bind_rows(df1, df2)
  
  df <- df %>%
    mutate(
      timestamp = ymd_hms(timestamp, tz = "UTC"),
      date = as.Date(timestamp),
      hour_bin = floor_date(timestamp, "hour")
    ) %>%
    distinct(timestamp, .keep_all = TRUE) %>%
    select(-sensor_id, -lat, -lon)  
  
  hampel_filter <- function(x) {
    roll_med <- slide_dbl(x, median, na.rm = TRUE, .before = 3, .after = 3)
    roll_mad <- slide_dbl(x, mad, na.rm = TRUE, .before = 3, .after = 3)
    ifelse(abs(x - roll_med) > 3 * roll_mad, roll_med, x)
  }
  
  df_clean <- df %>%
    group_by(date) %>%
    arrange(timestamp) %>%
    mutate(
      P1 = hampel_filter(P1),
      P2 = hampel_filter(P2)
    ) %>%
    ungroup()
  
  THRESHOLD <- 1 
  
  df_hourly <- df_clean %>%
    group_by(hour_bin) %>%
    summarise(
      valid_count = n(),
      `PM2.5` = ifelse(valid_count >= THRESHOLD, mean(P1, na.rm = TRUE), NA),
      `PM10`  = ifelse(valid_count >= THRESHOLD, mean(P2, na.rm = TRUE), NA),
      .groups = "drop"
    ) %>%
    select(-valid_count) %>%
    rename(timestamp = hour_bin)
  
  df_hourly <- df_hourly %>%
    complete(
      timestamp = seq(
        ymd_hms("2022-02-01 00:00:00", tz = "UTC"), 
        ymd_hms("2025-05-31 23:00:00", tz = "UTC"), 
        by = "hour"
      )
    )
  
  saveRDS(df_hourly, file.path("stage_process_pm25_10", paste0(base_name, ".rds")))
  
  return(df_hourly)
}


sensor_data_list_2 <- lapply(sensor_stage_all_75, function(sensor_name) {
  process_sensor_data_2(paste0(sensor_name, ".csv"))
})
names(sensor_data_list_2) <- sensor_stage_all_75






plot_all_sensors_2 <- function(sensor_data_list) {
  
  big_df <- bind_rows(sensor_data_list, .id = "sensor_name")
  
  ggplot(big_df, aes(x = timestamp)) +
    geom_line(aes(y = `PM10`), na.rm = TRUE) +
    facet_wrap(~ sensor_name, ncol = 6) +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 6),
      axis.text.x = element_text(size = 5),
      axis.text.y = element_text(size = 5)
    )
}



plot_all_pm10_1 <- plot_all_sensors_2(sensor_data_list_2)
plot_all_pm10_1





moisture_1 <- read.table(
  "weather/moisture_1.txt",
  header = TRUE,
  sep = ";",
  stringsAsFactors = FALSE,
  fill = TRUE
)

moisture_1 <- moisture_1 %>%
  mutate(
    timestamp = as.POSIXct(
      as.character(MESS_DATUM),
      format = "%Y%m%d%H",
      tz = "UTC"
    )
  ) %>%
  filter(
    timestamp >= as.POSIXct("2022-02-01 00:00:00", tz="UTC") &
      timestamp <= as.POSIXct("2024-12-31 23:00:00", tz="UTC")
  ) %>%
  select(timestamp, RF_STD) %>%
  rename(RH = RF_STD)


moisture_2 <- read.table(
  "weather/moisture_2.txt",
  header = TRUE,
  sep = ";",
  stringsAsFactors = FALSE,
  fill = TRUE
)

moisture_2 <- moisture_2 %>%
  mutate(
    timestamp = as.POSIXct(
      as.character(MESS_DATUM),
      format = "%Y%m%d%H",
      tz = "UTC"
    )
  ) %>%
  filter(
    timestamp >= as.POSIXct("2025-01-01 00:00:00", tz="UTC") &
      timestamp <= as.POSIXct("2025-05-31 23:00:00", tz="UTC")
  ) %>%
  select(timestamp, RF_STD) %>%
  rename(RH = RF_STD)


moisture <- bind_rows(moisture_1, moisture_2)


moisture <- data.frame(
  timestamp = seq(
    from = as.POSIXct("2022-02-01 00:00:00", tz = tz_used),
    to   = as.POSIXct("2025-05-31 23:00:00", tz = tz_used),
    by   = "hour"
  )
) %>%
  left_join(moisture, by = "timestamp") %>%
  arrange(timestamp)

sum(is.na(moisture$RH))
moisture$RH <- na.interp(moisture$RH)
sum(is.na(moisture$RH))
moisture$RH <- as.numeric(moisture$RH)


ggplot(moisture, aes(x = timestamp)) +
  geom_line(aes(y = RH,
                color = "RH"),
            na.rm = TRUE) +
  labs(title = "Change",
       x = "Time",
       y = "Value",
       color = "Type") +
  theme_minimal()


df_hourly$timestamp <- format(df_hourly$timestamp, "%Y-%m-%d %H:%M:%S")
write.csv(moisture, "moisture.csv", row.names = FALSE)
df_hourly$timestamp <- as.POSIXct(df_hourly$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")

moisture_all <- moisture %>% rename(moisture = RH)

saveRDS(moisture, "moisture.rds")
moisture_test <- readRDS("moisture.rds")
saveRDS(moisture_all, "moisture_all.rds")


#----------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------
temperature_1 <- read.table(
  "weather/temperature_1.txt",
  header = TRUE,
  sep = ";",
  stringsAsFactors = FALSE,
  fill = TRUE
)

temperature_1 <- temperature_1 %>%
  mutate(
    timestamp = as.POSIXct(
      as.character(MESS_DATUM),
      format = "%Y%m%d%H",
      tz = "UTC"
    )
  ) %>%
  filter(
    timestamp >= as.POSIXct("2022-02-01 00:00:00", tz="UTC") &
      timestamp <= as.POSIXct("2024-12-31 23:00:00", tz="UTC")
  ) %>%
  select(timestamp, TT_TU) %>%
  rename(temperature = TT_TU)


temperature_2 <- read.table(
  "weather/temperature_2.txt",
  header = TRUE,
  sep = ";",
  stringsAsFactors = FALSE,
  fill = TRUE
)

temperature_2 <- temperature_2 %>%
  mutate(
    timestamp = as.POSIXct(
      as.character(MESS_DATUM),
      format = "%Y%m%d%H",
      tz = "UTC"
    )
  ) %>%
  filter(
    timestamp >= as.POSIXct("2025-01-01 00:00:00", tz="UTC") &
      timestamp <= as.POSIXct("2025-05-31 23:00:00", tz="UTC")
  ) %>%
  select(timestamp, TT_TU) %>%
  rename(temperature = TT_TU)


temperature_all <- bind_rows(temperature_1, temperature_2)
saveRDS(temperature_all, "temperature_all.rds")


#----------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------
precipitation_1 <- read.table(
  "weather/precipitation_1.txt",
  header = TRUE,
  sep = ";",
  stringsAsFactors = FALSE,
  fill = TRUE
)

precipitation_1 <- precipitation_1 %>%
  mutate(
    timestamp = as.POSIXct(
      as.character(MESS_DATUM),
      format = "%Y%m%d%H",
      tz = "UTC"
    )
  ) %>%
  filter(
    timestamp >= as.POSIXct("2022-02-01 00:00:00", tz="UTC") &
      timestamp <= as.POSIXct("2024-12-31 23:00:00", tz="UTC")
  ) %>%
  select(timestamp, R1, RS_IND) %>%
  rename(precipitation = R1, preci_indicator = RS_IND)


precipitation_2 <- read.table(
  "weather/precipitation_2.txt",
  header = TRUE,
  sep = ";",
  stringsAsFactors = FALSE,
  fill = TRUE
)

precipitation_2 <- precipitation_2 %>%
  mutate(
    timestamp = as.POSIXct(
      as.character(MESS_DATUM),
      format = "%Y%m%d%H",
      tz = "UTC"
    )
  ) %>%
  filter(
    timestamp >= as.POSIXct("2025-01-01 00:00:00", tz="UTC") &
      timestamp <= as.POSIXct("2025-05-31 23:00:00", tz="UTC")
  ) %>%
  select(timestamp, R1, RS_IND) %>%
  rename(precipitation = R1, preci_indicator = RS_IND)


precipitation_all <- bind_rows(precipitation_1, precipitation_2)
saveRDS(precipitation_all, "precipitation_all.rds")

#----------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------
wind_1 <- read.table(
  "weather/wind_1.txt",
  header = TRUE,
  sep = ";",
  stringsAsFactors = FALSE,
  fill = TRUE
)

wind_1 <- wind_1 %>%
  mutate(
    timestamp = as.POSIXct(
      as.character(MESS_DATUM),
      format = "%Y%m%d%H",
      tz = "UTC"
    )
  ) %>%
  filter(
    timestamp >= as.POSIXct("2022-02-01 00:00:00", tz="UTC") &
      timestamp <= as.POSIXct("2024-12-31 23:00:00", tz="UTC")
  ) %>%
  select(timestamp, FF, DD) %>%
  rename(wind = FF, direction = DD)


wind_2 <- read.table(
  "weather/wind_2.txt",
  header = TRUE,
  sep = ";",
  stringsAsFactors = FALSE,
  fill = TRUE
)

wind_2 <- wind_2 %>%
  mutate(
    timestamp = as.POSIXct(
      as.character(MESS_DATUM),
      format = "%Y%m%d%H",
      tz = "UTC"
    )
  ) %>%
  filter(
    timestamp >= as.POSIXct("2025-01-01 00:00:00", tz="UTC") &
      timestamp <= as.POSIXct("2025-05-31 23:00:00", tz="UTC")
  ) %>%
  select(timestamp, FF, DD) %>%
  rename(wind = FF, direction = DD)


wind_all <- bind_rows(wind_1, wind_2)
saveRDS(wind_all, "wind_all.rds")


#----------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------
weather_all <- temperature_all %>%
  left_join(moisture_all,   by = "timestamp") %>%
  left_join(precipitation_all, by = "timestamp") %>%
  left_join(wind_all, by = "timestamp")

saveRDS(weather_all, "weather_all.rds")

weather_stage_1 <- weather_all %>%
  filter(
    timestamp <= as.POSIXct("2024-05-31 23:00:00", tz="UTC")
  )


weather_stage_2 <- weather_all %>%
  filter(
    timestamp >= as.POSIXct("2023-02-01 00:00:00", tz="UTC")
  )

saveRDS(weather_stage_1, "weather_stage_1.rds")
saveRDS(weather_stage_2, "weather_stage_2.rds")



#----------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------

impute_and_calibrate <- function(filename, PM2x5_all, moisture, sensor_locations, all_sensors_wide) {
  
  df <- readRDS(file.path("stage_process", filename))
  base_name <- tools::file_path_sans_ext(filename)
  
  current_lon <- df$longitude[1]
  current_lat <- df$latitude[1]
  
  neighbors <- sensor_locations %>% 
    filter(sensor != base_name) %>%
    mutate(dist = sqrt((longitude - current_lon)^2 + (latitude - current_lat)^2)) %>%
    arrange(dist)
  
  nearest_2_sensors <- neighbors$sensor[1:2]
  
  nearest_data <- all_sensors_wide %>%
    select(timestamp, all_of(nearest_2_sensors)) %>%
    rename(nearest_1 = nearest_2_sensors[1], nearest_2 = nearest_2_sensors[2])
  
  merged <- df %>%
    rename(PM_sensor = PM2.5) %>%
    left_join(PM2x5_all %>% rename(PM_official = PM2.5), by = "timestamp") %>%
    left_join(moisture %>% rename(RH = RH), by = "timestamp") %>%
    left_join(nearest_data, by = "timestamp") %>% 
    mutate(
      month_factor = as.factor(month(timestamp)),
      is_new_year = ifelse(month(timestamp) == 1 & day(timestamp) == 1 & hour(timestamp) <= 6, 1, 0)
    )
  
  sensor_99th_percentile <- quantile(merged$PM_sensor, 0.99, na.rm = TRUE)
  
  merged <- merged %>%
    mutate(
      ratio = PM_sensor / (PM_official + 1), 
      
      is_glitch = !is.na(ratio) & (PM_sensor > sensor_99th_percentile) & (ratio > 3),
      
      is_glitch = case_when(
        base_name %in% c("sds011_sensor_1785", "sds011_sensor_47519") & !is.na(ratio) & (ratio > 2 & PM_sensor > 30) ~ TRUE,
        
        base_name %in% c("sds011_sensor_23496", "sds011_sensor_62102", "sds011_sensor_7661", "sds011_sensor_8254") & !is.na(ratio) & (ratio > 1.5 | ratio < 0.3) & PM_sensor > 15 ~ TRUE,
        
        TRUE ~ is_glitch
      ),
      
      PM_clean = ifelse(is_glitch, NA, PM_sensor)
    )
  
  merged <- merged %>%
    arrange(timestamp) %>%
    mutate(PM_clean = zoo::na.approx(PM_clean, maxgap = 3, na.rm = FALSE))
  
  if (any(is.na(merged$PM_clean))) {
    set.seed(123)
    mf_df <- merged %>% 
      select(PM_clean, PM_official, RH, nearest_1, nearest_2, month_factor, is_new_year) %>%
      as.data.frame()
    
    mf_result <- missForest::missForest(mf_df, maxiter = 10, ntree = 100)
    merged$PM_imputed <- mf_result$ximp$PM_clean
  } else {
    merged$PM_imputed <- merged$PM_clean
  }
  ）
  model <- lm(PM_clean ~ PM_official + RH + PM_official:RH + is_new_year + month_factor, 
              data = merged)
  
  merged <- merged %>%
    mutate(
      PM_calibrated = PM_imputed - (coef(model)["RH"] * RH),
      
      PM_final = ifelse(PM_calibrated < 0, 0, PM_calibrated)
    ) %>%
    select(timestamp, PM2.5 = PM_final)
  
  saveRDS(
    merged,
    file.path("stage_cleaned", paste0(base_name, ".rds"))
  )
  
  return(merged)
}


sensor_data_cleaned_list <- setNames(
  lapply(sensor_stage_all_75, function(sensor_name) {
    impute_and_calibrate(
      paste0(sensor_name, ".rds"),
      PM2x5_all,
      moisture
    )
  }),
  sensor_stage_all_75
)



plot_all_sensors <- function(sensor_data_list) {
  
  big_df <- bind_rows(sensor_data_list, .id = "sensor_name")
  
  ggplot(big_df, aes(x = timestamp)) +
    geom_line(aes(y = `PM2.5`), na.rm = TRUE) +
    facet_wrap(~ sensor_name, ncol = 6) +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 6),
      axis.text.x = element_text(size = 5),
      axis.text.y = element_text(size = 5)
    )
}

plot_all_2 <- plot_all_sensors(sensor_data_cleaned_list)
plot_all_2

saveRDS(sensor_data_cleaned_list, "sensor_data_cleaned_list.rds")





plot_all_3 <- plot_all_sensors(sensor_data_cleaned_list_modified)
plot_all_3









impute_and_calibrate_2 <- function(filename, PM10_all, moisture, sensor_locations, all_sensors_wide) {
  
  df <- readRDS(file.path("stage_process_pm25_10", filename))
  base_name <- tools::file_path_sans_ext(filename)
  
  current_lon <- df$longitude[1]
  current_lat <- df$latitude[1]
  
  neighbors <- sensor_locations %>% 
    filter(sensor != base_name) %>%
    mutate(dist = sqrt((longitude - current_lon)^2 + (latitude - current_lat)^2)) %>%
    arrange(dist)
  
  nearest_2_sensors <- neighbors$sensor[1:2]
  
  nearest_data <- all_sensors_wide %>%
    select(timestamp, all_of(nearest_2_sensors)) %>%
    rename(nearest_1 = nearest_2_sensors[1], nearest_2 = nearest_2_sensors[2])
  
  merged <- df %>%
    rename(PM_sensor = PM10) %>%
    left_join(PM10_all %>% rename(PM_official = PM10), by = "timestamp") %>%
    left_join(moisture %>% rename(RH = RH), by = "timestamp") %>%
    left_join(nearest_data, by = "timestamp") %>% 
    mutate(
      month_factor = as.factor(month(timestamp)),
      is_new_year = ifelse(month(timestamp) == 1 & day(timestamp) == 1 & hour(timestamp) <= 6, 1, 0)
    )
  
  sensor_99th_percentile <- quantile(merged$PM_sensor, 0.99, na.rm = TRUE)
  
  merged <- merged %>%
    mutate(
      ratio = PM_sensor / (PM_official + 1), 
      
      is_glitch = !is.na(ratio) & (PM_sensor > sensor_99th_percentile) & (ratio > 3),
      
      is_glitch = case_when(
        base_name %in% c("sds011_sensor_1785", "sds011_sensor_47519") & !is.na(ratio) & (ratio > 2 & PM_sensor > 30) ~ TRUE,
        
        base_name %in% c("sds011_sensor_23496", "sds011_sensor_62102", "sds011_sensor_7661", "sds011_sensor_8254") & !is.na(ratio) & (ratio > 1.3 | ratio < 0.5) & PM_sensor > 15 ~ TRUE,
        
        TRUE ~ is_glitch
      ),
      
      PM_clean = ifelse(is_glitch, NA, PM_sensor)
    )
  
  merged <- merged %>%
    arrange(timestamp) %>%
    mutate(PM_clean = zoo::na.approx(PM_clean, maxgap = 3, na.rm = FALSE))
  
  if (any(is.na(merged$PM_clean))) {
    set.seed(123) 
    mf_df <- merged %>% 
      select(PM_clean, PM_official, RH, nearest_1, nearest_2, month_factor, is_new_year) %>%
      as.data.frame()
    
    mf_result <- missForest::missForest(mf_df, maxiter = 10, ntree = 100)
    merged$PM_imputed <- mf_result$ximp$PM_clean
  } else {
    merged$PM_imputed <- merged$PM_clean
  }
  
  model <- lm(PM_clean ~ PM_official + RH + PM_official:RH + is_new_year + month_factor, 
              data = merged)
  
  merged <- merged %>%
    mutate(
      PM_calibrated = PM_imputed - (coef(model)["RH"] * RH),
      
      PM_final = ifelse(PM_calibrated < 0, 0, PM_calibrated)
    ) %>%
    select(timestamp, PM10 = PM_final) 
  
  saveRDS(
    merged,
    file.path("stage_cleaned_pm25_10", paste0(base_name, ".rds"))
  )
  
  return(merged)
}





sensor_data_cleaned_list_2 <- setNames(
  lapply(sensor_stage_all_75, function(sensor_name) {
    impute_and_calibrate_2(
      paste0(sensor_name, ".rds"),
      PM10_all,
      moisture
    )
  }),
  sensor_stage_all_75
)



plot_all_sensors_2 <- function(sensor_data_list) {
  
  big_df <- bind_rows(sensor_data_list, .id = "sensor_name")
  
  ggplot(big_df, aes(x = timestamp)) +
    geom_line(aes(y = `PM10`), na.rm = TRUE) +
    facet_wrap(~ sensor_name, ncol = 6) +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 6),
      axis.text.x = element_text(size = 5),
      axis.text.y = element_text(size = 5)
    )
}



plot_all_pm10_2 <- plot_all_sensors_2(sensor_data_cleaned_list_2)
plot_all_pm10_2

saveRDS(sensor_data_cleaned_list_2, "sensor_data_cleaned_list_2.rds")


plot_all_pm10_3 <- plot_all_sensors_2(sensor_data_cleaned_list_modified_2)
plot_all_pm10_3





NO2_all <- bind_rows(NO2_2022, NO2_2023, NO2_2024, NO2_2025)
sum(is.na(NO2_all$`München/Landshuter Allee`))

NO2_all$`München/Landshuter Allee` <- na.interp(NO2_all$`München/Landshuter Allee`)
sum(is.na(NO2_all$`München/Landshuter Allee`))
NO2_all$`München/Landshuter Allee` <- as.numeric(NO2_all$`München/Landshuter Allee`)
sum(is.na(NO2_all$Zeitpunkt))
NO2_all <- na.omit(NO2_all)

NO2_all <- data.frame(
  Zeitpunkt = seq(
    from = as.POSIXct("2022-02-01 00:00:00", tz = tz_used),
    to   = as.POSIXct("2025-05-31 23:00:00", tz = tz_used),
    by   = "hour"
  )
) %>%
  left_join(NO2_all, by = "Zeitpunkt") %>%
  arrange(Zeitpunkt)

sum(is.na(NO2_all$`München/Landshuter Allee`))
NO2_all$`München/Landshuter Allee` <- na.interp(NO2_all$`München/Landshuter Allee`)
sum(is.na(NO2_all$`München/Landshuter Allee`))
NO2_all$`München/Landshuter Allee` <- as.numeric(NO2_all$`München/Landshuter Allee`)

NO2_all<- NO2_all %>% rename(timestamp = Zeitpunkt)
NO2_all<- NO2_all %>% rename(NO2 = `München/Landshuter Allee`)
sum(is.na(NO2_all$NO2))

saveRDS(NO2_all, "NO2_all.rds")


ggplot(NO2_all, aes(x = timestamp)) +
  geom_line(aes(y = NO2,
                color = "NO2"),
            na.rm = TRUE) +
  labs(title = "Change",
       x = "Time",
       y = "Value",
       color = "Type") +
  theme_minimal()


my_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )



NO2_all$NO2_ma <- rollmean(NO2_all$NO2,
                           k = 360,
                           fill = NA,
                           align = "center")

NO2_all_plot_1 <- ggplot(NO2_all, aes(x = timestamp)) +
  geom_line(aes(y = NO2),
            alpha = 0.2,
            size = 0.2,
            color = "grey40") +
  geom_line(aes(y = NO2_ma),
            color = "#E63946",
            size = 1.2) +
  coord_cartesian(ylim = c(0, 200)) +
  scale_x_datetime(
    limits = as.POSIXct(c("2022-02-01 00:00:00",
                          "2025-05-31 23:00:00"),
                        tz = "UTC"),
    breaks = as.POSIXct(
      c("2022-02-01 00:00:00",
        "2023-02-01 00:00:00",
        "2024-01-01 00:00:00",
        "2024-05-01 00:00:00",
        "2025-01-01 00:00:00",
        "2025-06-01 00:00:00"),
      tz = attr(NO2_all$timestamp, "tzone")
    ),
    date_labels = "%Y-%m"
  ) +
  labs(title = expression("Official " * NO[2] * " Change (Moving Average)"),
       x = NULL,
       y = expression(NO[2]~(mu*g/m^3))) +
  my_theme


NO2_all_plot_2 <- ggplot(NO2_all, aes(x = timestamp)) +
  geom_line(aes(y = NO2),
            alpha = 0.2,
            size = 0.2,
            color = "grey40") +
  geom_smooth(aes(y = NO2),
              method = "gam",
              formula = y ~ s(x, bs = "cs"),
              se = FALSE,
              color = "#E63946",
              size = 1.2) +
  coord_cartesian(ylim = c(0, 200)) +
  scale_x_datetime(
    limits = as.POSIXct(c("2022-02-01 00:00:00",
                          "2025-05-31 23:00:00"),
                        tz = "UTC"),
    breaks = as.POSIXct(
      c("2022-02-01 00:00:00",
        "2023-02-01 00:00:00",
        "2024-01-01 00:00:00",
        "2024-05-01 00:00:00",
        "2025-01-01 00:00:00",
        "2025-06-01 00:00:00"),
      tz = attr(NO2_all$timestamp, "tzone")
    ),
    date_labels = "%Y-%m"
  ) +
  labs(title = expression("Official " * NO[2] * " Change (GAM)"),
       x = NULL,
       y = expression(NO[2]~(mu*g/m^3))) +
  my_theme


NO2_all_plot_1 / plot_spacer() / NO2_all_plot_2 +
  plot_layout(heights = c(1, 0.2, 1))









ggplot(PM2x5_all, aes(x = timestamp)) +
  geom_line(aes(y = PM2.5,
                color = "PM2.5"),
            na.rm = TRUE) +
  labs(title = "Change",
       x = "Time",
       y = "Value",
       color = "Type") +
  theme_minimal()



PM2x5_all_plot_0 <- ggplot(PM2x5_all, aes(x = timestamp)) +
  geom_line(aes(y = PM2.5),
            alpha = 0.8,
            size = 0.8,
            color = "grey40") +
  coord_cartesian(ylim = c(0, 500)) +
  labs(title = expression("Official " * PM[2.5] * " Change (Original Data)"),
       x = NULL,
       y = expression(PM[2.5]~(mu*g/m^3))) +
  scale_x_datetime(
    limits = as.POSIXct(c("2022-02-01 00:00:00",
                          "2025-05-31 23:00:00"),
                        tz = "UTC"),
    breaks = as.POSIXct(
      c("2022-02-01 00:00:00",
        "2023-02-01 00:00:00",
        "2024-01-01 00:00:00",
        "2024-05-01 00:00:00",
        "2025-01-01 00:00:00",
        "2025-06-01 00:00:00"),
      tz = attr(PM2x5_all$timestamp, "tzone")
    ),
    date_labels = "%Y-%m"
  ) +
  my_theme




PM2x5_all$PM2.5_ma <- rollmean(PM2x5_all$PM2.5,
                               k = 240,
                               fill = NA,
                               align = "center")

PM2x5_all_plot_1 <- ggplot(PM2x5_all, aes(x = timestamp)) +
  geom_line(aes(y = PM2.5),
            alpha = 0.2,
            size = 0.2,
            color = "grey40") +
  geom_line(aes(y = PM2.5_ma),
            color = "#E63946",
            size = 1.2) +
  coord_cartesian(ylim = c(0, 200)) +   
  scale_x_datetime(
    limits = as.POSIXct(c("2022-02-01 00:00:00",
                          "2025-05-31 23:00:00"),
                        tz = "UTC"),
    breaks = as.POSIXct(
      c("2022-02-01 00:00:00",
        "2023-02-01 00:00:00",
        "2024-01-01 00:00:00",
        "2024-05-01 00:00:00",
        "2025-01-01 00:00:00",
        "2025-06-01 00:00:00"),
      tz = attr(PM2x5_all$timestamp, "tzone")
    ),
    date_labels = "%Y-%m"
  ) +
  labs(title = expression("Official " * PM[2.5] * " Change (Moving Average)"),
       x = NULL,
       y = expression(PM[2.5]~(mu*g/m^3))) +
  my_theme


PM2x5_all_plot_2 <- ggplot(PM2x5_all, aes(x = timestamp)) +
  geom_line(aes(y = PM2.5,
                color = "PM2.5"),
            alpha = 0.2,      
            size = 0.2,
            color = "grey40") +
  geom_smooth(aes(y = PM2.5),
              method = "gam",
              formula = y ~ s(x, bs = "cs"),
              se = FALSE,
              color = "#E63946",
              size = 1.2) +
  coord_cartesian(ylim = c(0, 200)) +
  scale_x_datetime(
    limits = as.POSIXct(c("2022-02-01 00:00:00",
                          "2025-05-31 23:00:00"),
                        tz = "UTC"),
    breaks = as.POSIXct(
      c("2022-02-01 00:00:00",
        "2023-02-01 00:00:00",
        "2024-01-01 00:00:00",
        "2024-05-01 00:00:00",
        "2025-01-01 00:00:00",
        "2025-06-01 00:00:00"),
      tz = attr(PM2x5_all$timestamp, "tzone")
    ),
    date_labels = "%Y-%m"
  ) +
  labs(title = expression("Official " * PM[2.5] * " Change (GAM)"),
       x = NULL,
       y = expression(PM[2.5]~(mu*g/m^3))) +
  my_theme


PM2x5_all_plot_1 / plot_spacer() / PM2x5_all_plot_2 +
  plot_layout(heights = c(1, 0.2, 1))



PM2x5_all_plot_0 / plot_spacer() /PM2x5_all_plot_1 / plot_spacer() / PM2x5_all_plot_2 +
  plot_layout(heights = c(1, 0.05, 1, 0.05, 1))






ggplot(PM10_all, aes(x = timestamp)) +
  geom_line(aes(y = PM10,
                color = "PM10"),
            na.rm = TRUE) +
  labs(title = "Change",
       x = "Time",
       y = "Value",
       color = "Type") +
  theme_minimal()



PM10_all_plot_0 <- ggplot(PM10_all, aes(x = timestamp)) +
  geom_line(aes(y = PM10),
            alpha = 0.8,
            size = 0.8,
            color = "grey40") +
  coord_cartesian(ylim = c(0, 500)) +
  labs(title = expression("Official " * PM[10] * " Change (Original Data)"),
       x = NULL,
       y = expression(PM[10]~(mu*g/m^3))) +
  scale_x_datetime(
    limits = as.POSIXct(c("2022-02-01 00:00:00",
                          "2025-05-31 23:00:00"),
                        tz = "UTC"),
    breaks = as.POSIXct(
      c("2022-02-01 00:00:00",
        "2023-02-01 00:00:00",
        "2024-01-01 00:00:00",
        "2024-05-01 00:00:00",
        "2025-01-01 00:00:00",
        "2025-06-01 00:00:00"),
      tz = attr(PM10_all$timestamp, "tzone")
    ),
    date_labels = "%Y-%m"
  ) +
  my_theme



PM10_all$PM10_ma <- rollmean(PM10_all$PM10,
                               k = 240,
                               fill = NA,
                               align = "center")

PM10_all_plot_1 <- ggplot(PM10_all, aes(x = timestamp)) +
  geom_line(aes(y = PM10),
            alpha = 0.2,
            size = 0.2,
            color = "grey40") +
  geom_line(aes(y = PM10_ma),
            color = "#E63946",
            size = 1.2) +
  coord_cartesian(ylim = c(0, 200)) +
  scale_x_datetime(
    limits = as.POSIXct(c("2022-02-01 00:00:00",
                          "2025-05-31 23:00:00"),
                        tz = "UTC"),
    breaks = as.POSIXct(
      c("2022-02-01 00:00:00",
        "2023-02-01 00:00:00",
        "2024-01-01 00:00:00",
        "2024-05-01 00:00:00",
        "2025-01-01 00:00:00",
        "2025-06-01 00:00:00"),
      tz = attr(PM2x5_all$timestamp, "tzone")
    ),
    date_labels = "%Y-%m"
  ) +
  labs(title = expression("Official " * PM[10] * " Change (Moving Average)"),
       x = NULL,
       y = expression(PM[10]~(mu*g/m^3))) +
  my_theme


PM10_all_plot_2 <- ggplot(PM10_all, aes(x = timestamp)) +
  geom_line(aes(y = PM10,
                color = "PM10"),
            alpha = 0.2,    
            size = 0.2,
            color = "grey40") +
  geom_smooth(aes(y = PM10),
              method = "gam",
              formula = y ~ s(x, bs = "cs"),
              se = FALSE,
              color = "#E63946",
              size = 1.3) +
  coord_cartesian(ylim = c(0, 200)) +
  scale_x_datetime(
    limits = as.POSIXct(c("2022-02-01 00:00:00",
                          "2025-05-31 23:00:00"),
                        tz = "UTC"),
    breaks = as.POSIXct(
      c("2022-02-01 00:00:00",
        "2023-02-01 00:00:00",
        "2024-01-01 00:00:00",
        "2024-05-01 00:00:00",
        "2025-01-01 00:00:00",
        "2025-06-01 00:00:00"),
      tz = attr(PM2x5_all$timestamp, "tzone")
    ),
    date_labels = "%Y-%m"
  ) +
  labs(title = expression("Official " * PM[10] * " Change (GAM)"),
       x = NULL,
       y = expression(PM[10]~(mu*g/m^3))) +
  my_theme


PM10_all_plot_1 / plot_spacer() / PM10_all_plot_2 +
  plot_layout(heights = c(1, 0.2, 1))

PM10_all_plot_0 / plot_spacer() / PM10_all_plot_1 / plot_spacer() / PM10_all_plot_2 +
  plot_layout(heights = c(1, 0.05, 1, 0.05, 1))


