# ===========================================
# Step 0: Install and load necessary packages
# ===========================================
library(tidyverse)
library(furrr)      
library(data.table)
library(lubridate)
library(R.utils)
library(httr)
library(stringr)
library(ggplot2)
library(dplyr)


# =====================
# Step 1: Configuration
# =====================
start_date <- as.Date("2022-02-01")
end_date   <- as.Date("2024-05-31")
date_seq   <- seq(start_date, end_date, by = "day")

sensors_input <- "sds011_sensor_489,sds011_sensor_826,sds011_sensor_1785,sds011_sensor_1831,sds011_sensor_2620,sds011_sensor_3815,sds011_sensor_3817,sds011_sensor_3867,sds011_sensor_4418,sds011_sensor_4430,sds011_sensor_4452,sds011_sensor_4630,sds011_sensor_4658,sds011_sensor_4676,sds011_sensor_6117,sds011_sensor_6217,sds011_sensor_6280,sds011_sensor_6334,sds011_sensor_7444,sds011_sensor_7503,sds011_sensor_7661,sds011_sensor_8254,sds011_sensor_8353,sds011_sensor_8466,sds011_sensor_9657,sds011_sensor_9838,sds011_sensor_12165,sds011_sensor_12275,sds011_sensor_14021,sds011_sensor_14096,sds011_sensor_15404,sds011_sensor_15679,sds011_sensor_15966,sds011_sensor_16600,sds011_sensor_20859,sds011_sensor_21122,sds011_sensor_21513,sds011_sensor_22388,sds011_sensor_22955,sds011_sensor_23496,sds011_sensor_24173,sds011_sensor_25419,sds011_sensor_25776,sds011_sensor_25780,sds011_sensor_25915,sds011_sensor_26824,sds011_sensor_26977,sds011_sensor_27199,sds011_sensor_27367,sds011_sensor_27792,sds011_sensor_27802,sds011_sensor_27907,sds011_sensor_30472,sds011_sensor_33674,sds011_sensor_34381,sds011_sensor_35364,sds011_sensor_37873,sds011_sensor_37937,sds011_sensor_38009,sds011_sensor_38224,sds011_sensor_41823,sds011_sensor_41923,sds011_sensor_42420,sds011_sensor_43162,sds011_sensor_43664,sds011_sensor_45011,sds011_sensor_45768,sds011_sensor_47043,sds011_sensor_47519,sds011_sensor_49814,sds011_sensor_50161,sds011_sensor_50165,sds011_sensor_51300,sds011_sensor_53002,sds011_sensor_55148,sds011_sensor_56599,sds011_sensor_58091,sds011_sensor_61531,sds011_sensor_62102,sds011_sensor_63201,sds011_sensor_64110,sds011_sensor_64313,sds011_sensor_65094,sds011_sensor_65799,sds011_sensor_68377,sds011_sensor_69730,sds011_sensor_70683,sds011_sensor_72092,sds011_sensor_72394,sds011_sensor_73382,sds011_sensor_73670,sds011_sensor_74172,sds011_sensor_75618,sds011_sensor_75982,sds011_sensor_76398,sds011_sensor_79348,sds011_sensor_79574,sds011_sensor_79611,sds011_sensor_81607,sds011_sensor_82554,sds011_sensor_83103,sds011_sensor_84123,sds011_sensor_84997,sds011_sensor_86205,sds011_sensor_86619,sds011_sensor_87748,sds011_sensor_90738"
target_sensors <- str_split(sensors_input, ",")[[1]] %>% str_trim()


# ===============================================
# Step 2: Web Crawling
# ===============================================
check_day_inventory <- function(date, sensors) {
  
  library(httr)
  library(stringr)
  
  date_str <- as.character(date)
  year_str <- year(date)

  index_url <- paste0("https://archive.sensor.community/", year_str, "/", date_str, "/")
  
  max_retries <- 3
  page_content <- NULL
  fetch_status <- "UNKNOWN"
  
  for (attempt in 1:max_retries) {
    Sys.sleep(runif(1, 0.2, 0.6))
    
    res <- tryCatch({
      GET(index_url, timeout(15))
    }, error = function(e) return(NULL))
    
    if (!is.null(res)) {
      if (status_code(res) == 200) {
        page_content <- content(res, "text", encoding = "UTF-8")
        fetch_status <- "OK"
        break
      } else if (status_code(res) == 404) {
        fetch_status <- "MISSING_DAY"
        break
      } else {
        Sys.sleep(1 * attempt)
      }
    } else {
      Sys.sleep(1 * attempt)
    }
  }
  
  result_list <- data.frame(
    date = rep(date, length(sensors)),
    sensor_id = sensors,
    status = rep("UNKNOWN", length(sensors)),
    stringsAsFactors = FALSE
  )
  
  if (fetch_status == "OK" && !is.null(page_content)) {
    exists_check <- map_lgl(sensors, function(s) {
      pattern <- paste0(date_str, "_", s) 
      return(str_detect(page_content, fixed(pattern)))
    })
    
    result_list$status <- ifelse(exists_check, "EXISTS", "NOT_FOUND")
    
  } else if (fetch_status == "MISSING_DAY") {
    result_list$status <- "DAY_EMPTY" 
  } else {
    result_list$status <- "CHECK_FAILED" 
  }
  
  return(result_list)
}


# ==========================================
# Step 3: Parallel Execution
# ==========================================
plan(multisession, workers = 8)

inventory_df <- future_map_dfr(date_seq, function(d) {
  check_day_inventory(d, target_sensors)
}, .progress = TRUE)


# ==========================================
# Step 4: Statistical Output
# ==========================================
table(inventory_df$status)

failed_checks <- inventory_df %>% filter(status == "CHECK_FAILED")
if (nrow(failed_checks) > 0) {
  cat("There are", n_distinct(failed_checks$date), "days failed to download; the data status for these days is unknown.")
} else {
  cat("The directory pages for all dates have been successfully retrieved.")
}

sensor_stats <- inventory_df %>%
  group_by(sensor_id) %>%
  summarise(
    total_days_checked = n(),
    days_exist = sum(status == "EXISTS"),
    days_missing = sum(status %in% c("NOT_FOUND", "DAY_EMPTY")),
    coverage_percent = round(days_exist / total_days_checked * 100, 2)
  ) %>%
  arrange(desc(coverage_percent))

print(head(sensor_stats, 10))

fwrite(inventory_df, "data_inventory_daily.csv")
fwrite(sensor_stats, "data_inventory_summary.csv")







# ==========================================
# Step 1: Configuration
# ==========================================
start_date <- as.Date("2023-02-01")
end_date   <- as.Date("2025-05-31")
date_seq   <- seq(start_date, end_date, by = "day")

sensors_input <- "sds011_sensor_489,sds011_sensor_826,sds011_sensor_1785,sds011_sensor_1831,sds011_sensor_2620,sds011_sensor_3815,sds011_sensor_3817,sds011_sensor_3867,sds011_sensor_4418,sds011_sensor_4430,sds011_sensor_4452,sds011_sensor_4630,sds011_sensor_4658,sds011_sensor_4676,sds011_sensor_6117,sds011_sensor_6217,sds011_sensor_6280,sds011_sensor_6334,sds011_sensor_7444,sds011_sensor_7503,sds011_sensor_7661,sds011_sensor_8254,sds011_sensor_8353,sds011_sensor_8466,sds011_sensor_9657,sds011_sensor_9838,sds011_sensor_12165,sds011_sensor_12275,sds011_sensor_14021,sds011_sensor_14096,sds011_sensor_15404,sds011_sensor_15679,sds011_sensor_15966,sds011_sensor_16600,sds011_sensor_20859,sds011_sensor_21122,sds011_sensor_21513,sds011_sensor_22388,sds011_sensor_22955,sds011_sensor_23496,sds011_sensor_24173,sds011_sensor_25419,sds011_sensor_25776,sds011_sensor_25780,sds011_sensor_25915,sds011_sensor_26824,sds011_sensor_26977,sds011_sensor_27199,sds011_sensor_27367,sds011_sensor_27792,sds011_sensor_27802,sds011_sensor_27907,sds011_sensor_30472,sds011_sensor_33674,sds011_sensor_34381,sds011_sensor_35364,sds011_sensor_37873,sds011_sensor_37937,sds011_sensor_38009,sds011_sensor_38224,sds011_sensor_41823,sds011_sensor_41923,sds011_sensor_42420,sds011_sensor_43162,sds011_sensor_43664,sds011_sensor_45011,sds011_sensor_45768,sds011_sensor_47043,sds011_sensor_47519,sds011_sensor_49814,sds011_sensor_50161,sds011_sensor_50165,sds011_sensor_51300,sds011_sensor_53002,sds011_sensor_55148,sds011_sensor_56599,sds011_sensor_58091,sds011_sensor_61531,sds011_sensor_62102,sds011_sensor_63201,sds011_sensor_64110,sds011_sensor_64313,sds011_sensor_65094,sds011_sensor_65799,sds011_sensor_68377,sds011_sensor_69730,sds011_sensor_70683,sds011_sensor_72092,sds011_sensor_72394,sds011_sensor_73382,sds011_sensor_73670,sds011_sensor_74172,sds011_sensor_75618,sds011_sensor_75982,sds011_sensor_76398,sds011_sensor_79348,sds011_sensor_79574,sds011_sensor_79611,sds011_sensor_81607,sds011_sensor_82554,sds011_sensor_83103,sds011_sensor_84123,sds011_sensor_84997,sds011_sensor_86205,sds011_sensor_86619,sds011_sensor_87748,sds011_sensor_90738"
target_sensors <- str_split(sensors_input, ",")[[1]] %>% str_trim()


check_day_inventory_v2 <- function(date, sensors) {
  
  library(httr)
  library(stringr)
  
  date_str <- as.character(date)
  year_str <- year(date)
  
  if (date < as.Date("2025-01-01")) {
    index_url <- paste0("https://archive.sensor.community/", year_str, "/", date_str, "/")
  } else {
    index_url <- paste0("https://archive.sensor.community/", date_str, "/")
  }
  
  # ---------------------
  
  max_retries <- 5
  page_content <- NULL
  fetch_status <- "UNKNOWN"
  
  my_ua <- user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
  
  for (attempt in 1:max_retries) {
    Sys.sleep(runif(1, 0.5, 1.2)) 
    
    res <- tryCatch({
      GET(index_url, my_ua, timeout(20))
    }, error = function(e) return(NULL))
    
    if (!is.null(res)) {
      status <- status_code(res)
      if (status == 200) {
        page_content <- content(res, "text", encoding = "UTF-8")
        fetch_status <- "OK"
        break
      } else if (status == 404) {
        fetch_status <- "MISSING_DAY" 
        break
      } else {
        Sys.sleep(3 * attempt)
      }
    } else {
      Sys.sleep(2 * attempt)
    }
  }
  
  result_list <- data.frame(
    date = rep(date, length(sensors)),
    sensor_id = sensors,
    status = rep("UNKNOWN", length(sensors)),
    stringsAsFactors = FALSE
  )
  
  if (fetch_status == "OK" && !is.null(page_content)) {
    exists_check <- map_lgl(sensors, function(s) {
      pattern <- paste0(date_str, "_", s) 
      return(str_detect(page_content, fixed(pattern)))
    })
    result_list$status <- ifelse(exists_check, "EXISTS", "NOT_FOUND")
    
  } else if (fetch_status == "MISSING_DAY") {
    result_list$status <- "DAY_EMPTY" 
  } else {
    result_list$status <- "CHECK_FAILED" 
  }
  
  return(result_list)
}

# ==========================================
# Step 2: Processing
# ==========================================
plan(multisession, workers = 4)

inventory_df <- future_map_dfr(date_seq, function(d) {
  check_day_inventory_v2(d, target_sensors)
}, .progress = TRUE)


# ==========================================
# Step 3: Summary
# ==========================================
failed_days <- inventory_df %>% filter(status == "CHECK_FAILED") %>% distinct(date)

final_stats <- inventory_df %>%
  group_by(sensor_id) %>%
  summarise(
    real_days_count = sum(status == "EXISTS"),
    missing_days = sum(status %in% c("NOT_FOUND", "DAY_EMPTY")),
    coverage = round(real_days_count / n() * 100, 2)
  ) %>%
  arrange(desc(real_days_count))

print(head(final_stats, 10))

fwrite(inventory_df, "inventory_daily_2023_2025.csv")
fwrite(final_stats, "inventory_summary_2023_2025.csv")













# ==========================================
# Step 4: Data Preparation
# ==========================================
df_stage1 <- fread("data_inventory_daily.csv")
df_stage2 <- fread("inventory_daily_2023_2025.csv")
df_all <- bind_rows(fread("data_inventory_daily.csv"), fread("inventory_daily_2023_2025.csv"))


start_date <- as.Date("2022-02-01")
end_date   <- as.Date("2025-05-31")

clean_df <- df_all %>%
  mutate(sensor_label = str_extract(sensor_id, "\\d+$")) %>%
  distinct(sensor_label, date, .keep_all = TRUE) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= start_date & date <= end_date) %>%
  mutate(is_valid = status == "EXISTS")


# ==========================================
# Step 5: Data Calculation
# ==========================================
grid_df <- clean_df %>%
  mutate(
    year = year(date),
    month = month(date),
    day = mday(date),
    days_in_m = days_in_month(date),
    
    month_diff = (year - year(start_date)) * 12 + (month - month(start_date)),

    third_month_offset = case_when(
      day <= 10 ~ 0,
      day <= 20 ~ 1,
      TRUE ~ 2
    ),
    
    x_idx = month_diff * 3 + third_month_offset + 1,
    
    theoretical_days = case_when(
      third_month_offset == 0 ~ 10,
      third_month_offset == 1 ~ 10,
      TRUE ~ days_in_m - 20
    )
  ) %>%
  group_by(sensor_label, x_idx) %>%
  summarise(
    actual_data_days = sum(is_valid),
    bin_days = first(theoretical_days),
    .groups = 'drop'
  ) %>%
  mutate(
    missing_rate = (bin_days - actual_data_days) / bin_days,
    missing_rate = pmax(0, pmin(1, missing_rate))
  )


total_months <- (year(end_date) - year(start_date)) * 12 + (month(end_date) - month(start_date)) + 1
max_idx <- total_months * 3 

df_plot <- grid_df %>%
  complete(sensor_label, x_idx = 1:max_idx, fill = list(missing_rate = 1.0))


sensor_rank <- df_plot %>%
  group_by(sensor_label) %>%
  summarise(perfect_bins = sum(missing_rate == 0)) %>%
  arrange(perfect_bins) 

df_plot$sensor_label <- factor(df_plot$sensor_label, levels = sensor_rank$sensor_label)

date_seq <- seq(start_date, end_date, by = "1 month")
label_df <- data.frame(date = date_seq) %>%
  mutate(
    year = year(date),
    month = month(date),
    show_label = (row_number() - 1) %% 3 == 0, 
    month_diff = (year - year(start_date)) * 12 + (month - month(start_date)),
    x_idx = month_diff * 3 + 1,
    label_text = format(date, "%Y-%m")
  ) %>%
  filter(show_label == TRUE)


p_grid_all <- ggplot(df_plot, aes(x = x_idx, y = sensor_label, fill = missing_rate)) +
  
  geom_tile(color = "white", linewidth = 0.15) + 
  
  scale_fill_gradientn(
    colors = c("#2A9D8F", "#D3D3D3", "#E63946"),
    values = c(0, 0.001, 1),
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%\n(Complete)", "25%", "50%", "75%", "100%\n(Missing)"),
    name = "Missing Rate"
  ) +
  
  scale_x_continuous(
    breaks = label_df$x_idx,
    labels = label_df$label_text,
    expand = c(0, 0),        
    position = "bottom"      
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, color = "#333333"),
    axis.text.y = element_text(size = 7, color = "#555555"), 
    panel.grid = element_blank(), 
    
    legend.position = c(0.98, 1.063),
    legend.justification = c(1, 1), 
    legend.direction = "horizontal",
    
    legend.background = element_rect(fill = alpha("white", 0.8), color = NA, size = 0),
    
    legend.title = element_text(size = 12, vjust = 0.8),
    legend.text = element_text(size = 10),
    
    legend.key.width = unit(1.2, "cm"),
    legend.key.height = unit(0.25, "cm"),
    legend.margin = margin(2, 2, 2, 2),
    
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(color = "gray40", size = 13, margin = margin(b = 15))
  ) +
  
  labs(
    title = "Temporal Distribution of Missing Sensor Readings in Munich",
    subtitle = paste0("Range: ", start_date, " to ", end_date, " (Each cell \u2248 10 days)"),
    x = NULL,
    y = "Sensor ID"
  )

print(p_grid_all)




sensor_stage_1_75 <- stats_s1$sensor_id[stats_s1$coverage > 75]
sensor_stage_2_75 <- stats_s2$sensor_id[stats_s2$coverage > 75]




start_date_1 <- as.Date("2022-02-01")
end_date_1   <- as.Date("2024-05-31")

clean_df_1 <- df_all %>%
  filter(sensor_id %in% sensor_stage_1_75) %>%
  mutate(sensor_label = str_extract(sensor_id, "\\d+$")) %>%
  distinct(sensor_label, date, .keep_all = TRUE) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= start_date_1 & date <= end_date_1) %>%
  mutate(is_valid = status == "EXISTS")

grid_df_1 <- clean_df_1 %>%
  mutate(
    year = year(date),
    month = month(date),
    day = mday(date),
    days_in_m = days_in_month(date),
    
    month_diff = (year - year(start_date_1)) * 12 + (month - month(start_date_1)),
    
    third_month_offset = case_when(
      day <= 10 ~ 0,
      day <= 20 ~ 1,
      TRUE ~ 2
    ),
    
    x_idx = month_diff * 3 + third_month_offset + 1,
    
    theoretical_days = case_when(
      third_month_offset == 0 ~ 10,
      third_month_offset == 1 ~ 10,
      TRUE ~ days_in_m - 20
    )
  ) %>%
  group_by(sensor_label, x_idx) %>%
  summarise(
    actual_data_days = sum(is_valid),
    bin_days = first(theoretical_days),
    .groups = 'drop'
  ) %>%
  mutate(
    missing_rate = (bin_days - actual_data_days) / bin_days,
    missing_rate = pmax(0, pmin(1, missing_rate))
  )

total_months_1 <- (year(end_date_1) - year(start_date_1)) * 12 + (month(end_date_1) - month(start_date_1)) + 1
max_idx_1 <- total_months_1 * 3 

df_plot_1 <- grid_df_1 %>%
  complete(sensor_label, x_idx = 1:max_idx_1, fill = list(missing_rate = 1.0))

sensor_rank_1 <- df_plot_1 %>%
  group_by(sensor_label) %>%
  summarise(perfect_bins = sum(missing_rate == 0)) %>%
  arrange(perfect_bins) 

df_plot_1$sensor_label <- factor(df_plot_1$sensor_label, levels = sensor_rank_1$sensor_label)

date_seq_1 <- seq(start_date_1, end_date_1, by = "1 month")
label_df_1 <- data.frame(date = date_seq_1) %>%
  mutate(
    year = year(date),
    month = month(date),
    show_label = (row_number() - 1) %% 3 == 0, 
    month_diff = (year - year(start_date_1)) * 12 + (month - month(start_date_1)),
    x_idx = month_diff * 3 + 1,
    label_text = format(date, "%Y-%m")
  ) %>%
  filter(show_label == TRUE)

p_grid_1 <- ggplot(df_plot_1, aes(x = x_idx, y = sensor_label, fill = missing_rate)) +
  
  geom_tile(color = "white", linewidth = 0.15) + 
  
  scale_fill_gradientn(
    colors = c("#2A9D8F", "#D3D3D3", "#E63946"),
    values = c(0, 0.001, 1),
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%\n(Complete)", "25%", "50%", "75%", "100%\n(Missing)"),
    name = "Missing Rate"
  ) +
  
  scale_x_continuous(
    breaks = label_df_1$x_idx,
    labels = label_df_1$label_text,
    expand = c(0, 0),        
    position = "bottom"      
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, color = "#333333"),
    axis.text.y = element_text(size = 7, color = "#555555"), 
    panel.grid = element_blank(), 
    
    legend.position = c(0.98, 1.063),
    legend.justification = c(1, 1), 
    legend.direction = "horizontal",
    
    legend.background = element_rect(fill = alpha("white", 0.8), color = NA, size = 0),
    
    legend.title = element_text(size = 12, vjust = 0.8),
    legend.text = element_text(size = 10),
    
    legend.key.width = unit(1.2, "cm"),
    legend.key.height = unit(0.25, "cm"),
    legend.margin = margin(2, 2, 2, 2),
    
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(color = "gray40", size = 13, margin = margin(b = 15))
  ) +
  
  labs(
    title = "Temporal Distribution of Missing Sensor Readings in Munich (Stage 1)",
    subtitle = paste0("Number of sensors: 78       Range: ", start_date_1, " to ", end_date_1, " (Each cell \u2248 10 days)"),
    x = NULL,
    y = "Sensor ID"
  )

print(p_grid_1)




start_date_2 <- as.Date("2023-02-01")
end_date_2   <- as.Date("2025-05-31")

clean_df_2 <- df_all %>%
  filter(sensor_id %in% sensor_stage_2_75) %>%
  mutate(sensor_label = str_extract(sensor_id, "\\d+$")) %>%
  distinct(sensor_label, date, .keep_all = TRUE) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= start_date_2 & date <= end_date_2) %>%
  mutate(is_valid = status == "EXISTS")

grid_df_2 <- clean_df_2 %>%
  mutate(
    year = year(date),
    month = month(date),
    day = mday(date),
    days_in_m = days_in_month(date),
    
    month_diff = (year - year(start_date_2)) * 12 + (month - month(start_date_2)),
    
    third_month_offset = case_when(
      day <= 10 ~ 0,
      day <= 20 ~ 1,
      TRUE ~ 2
    ),
    
    x_idx = month_diff * 3 + third_month_offset + 1,
    
    theoretical_days = case_when(
      third_month_offset == 0 ~ 10,
      third_month_offset == 1 ~ 10,
      TRUE ~ days_in_m - 20
    )
  ) %>%
  group_by(sensor_label, x_idx) %>%
  summarise(
    actual_data_days = sum(is_valid),
    bin_days = first(theoretical_days),
    .groups = 'drop'
  ) %>%
  mutate(
    missing_rate = (bin_days - actual_data_days) / bin_days,
    missing_rate = pmax(0, pmin(1, missing_rate))
  )

total_months_2 <- (year(end_date_2) - year(start_date_2)) * 12 + (month(end_date_2) - month(start_date_2)) + 1
max_idx_2 <- total_months_2 * 3 

df_plot_2 <- grid_df_2 %>%
  complete(sensor_label, x_idx = 1:max_idx_2, fill = list(missing_rate = 1.0))

sensor_rank_2 <- df_plot_2 %>%
  group_by(sensor_label) %>%
  summarise(perfect_bins = sum(missing_rate == 0)) %>%
  arrange(perfect_bins) 

df_plot_2$sensor_label <- factor(df_plot_2$sensor_label, levels = sensor_rank_2$sensor_label)

date_seq_2 <- seq(start_date_2, end_date_2, by = "1 month")
label_df_2 <- data.frame(date = date_seq_2) %>%
  mutate(
    year = year(date),
    month = month(date),
    show_label = (row_number() - 1) %% 3 == 0, 
    month_diff = (year - year(start_date_2)) * 12 + (month - month(start_date_2)),
    x_idx = month_diff * 3 + 1,
    label_text = format(date, "%Y-%m")
  ) %>%
  filter(show_label == TRUE)

p_grid_2 <- ggplot(df_plot_2, aes(x = x_idx, y = sensor_label, fill = missing_rate)) +
  
  geom_tile(color = "white", linewidth = 0.15) + 
  
  scale_fill_gradientn(
    colors = c("#2A9D8F", "#D3D3D3", "#E63946"),
    values = c(0, 0.001, 1),
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%\n(Complete)", "25%", "50%", "75%", "100%\n(Missing)"),
    name = "Missing Rate"
  ) +
  
  scale_x_continuous(
    breaks = label_df_2$x_idx,
    labels = label_df_2$label_text,
    expand = c(0, 0),        
    position = "bottom"      
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, color = "#333333"),
    axis.text.y = element_text(size = 7, color = "#555555"), 
    panel.grid = element_blank(), 
    
    legend.position = c(0.98, 1.063),
    legend.justification = c(1, 1), 
    legend.direction = "horizontal",
    
    legend.background = element_rect(fill = alpha("white", 0.8), color = NA, size = 0),
    
    legend.title = element_text(size = 12, vjust = 0.8),
    legend.text = element_text(size = 10),
    
    legend.key.width = unit(1.2, "cm"),
    legend.key.height = unit(0.25, "cm"),
    legend.margin = margin(2, 2, 2, 2),
    
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(color = "gray40", size = 13, margin = margin(b = 15))
  ) +
  
  labs(
    title = "Temporal Distribution of Missing Sensor Readings in Munich (Stage 2)",
    subtitle = paste0("Number of sensors: 89       Range: ", start_date_2, " to ", end_date_2, " (Each cell \u2248 10 days)"),
    x = NULL,
    y = "Sensor ID"
  )

print(p_grid_2)






sensor_stage_all_75 <- intersect(sensor_stage_1_75, sensor_stage_2_75)
print(sensor_stage_all_75)

start_date_all <- as.Date("2022-02-01")
end_date_all   <- as.Date("2025-05-31")

clean_df_all <- df_all %>%
  filter(sensor_id %in% sensor_stage_all_75) %>%
  mutate(sensor_label = str_extract(sensor_id, "\\d+$")) %>%
  distinct(sensor_label, date, .keep_all = TRUE) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= start_date_all & date <= end_date_all) %>%
  mutate(is_valid = status == "EXISTS")

grid_df_all <- clean_df_all %>%
  mutate(
    year = year(date),
    month = month(date),
    day = mday(date),
    days_in_m = days_in_month(date),
    
    month_diff = (year - year(start_date_all)) * 12 + (month - month(start_date_all)),
    
    third_month_offset = case_when(
      day <= 10 ~ 0,
      day <= 20 ~ 1,
      TRUE ~ 2
    ),
    
    x_idx = month_diff * 3 + third_month_offset + 1,
    
    theoretical_days = case_when(
      third_month_offset == 0 ~ 10,
      third_month_offset == 1 ~ 10,
      TRUE ~ days_in_m - 20
    )
  ) %>%
  group_by(sensor_label, x_idx) %>%
  summarise(
    actual_data_days = sum(is_valid),
    bin_days = first(theoretical_days),
    .groups = 'drop'
  ) %>%
  mutate(
    missing_rate = (bin_days - actual_data_days) / bin_days,
    missing_rate = pmax(0, pmin(1, missing_rate))
  )

total_months_2 <- (year(end_date_all) - year(start_date_all)) * 12 + (month(end_date_all) - month(start_date_all)) + 1
max_idx_2 <- total_months_2 * 3 

df_plot_all <- grid_df_all %>%
  complete(sensor_label, x_idx = 1:max_idx_2, fill = list(missing_rate = 1.0))

sensor_rank_all <- df_plot_all %>%
  group_by(sensor_label) %>%
  summarise(perfect_bins = sum(missing_rate == 0)) %>%
  arrange(perfect_bins) 

df_plot_all$sensor_label <- factor(df_plot_all$sensor_label, levels = sensor_rank_all$sensor_label)

date_seq_all <- seq(start_date_all, end_date_all, by = "1 month")
label_df_all <- data.frame(date = date_seq_all) %>%
  mutate(
    year = year(date),
    month = month(date),
    show_label = (row_number() - 1) %% 3 == 0, 
    month_diff = (year - year(start_date_all)) * 12 + (month - month(start_date_all)),
    x_idx = month_diff * 3 + 1,
    label_text = format(date, "%Y-%m")
  ) %>%
  filter(show_label == TRUE)

p_grid_all <- ggplot(df_plot_all, aes(x = x_idx, y = sensor_label, fill = missing_rate)) +
  
  geom_tile(color = "white", linewidth = 0.15) + 
  
  scale_fill_gradientn(
    colors = c("#2A9D8F", "#D3D3D3", "#E63946"),
    values = c(0, 0.001, 1),
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%\n(Complete)", "25%", "50%", "75%", "100%\n(Missing)"),
    name = "Missing Rate"
  ) +
  
  scale_x_continuous(
    breaks = label_df_all$x_idx,
    labels = label_df_all$label_text,
    expand = c(0, 0),        
    position = "bottom"      
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, color = "#333333"),
    axis.text.y = element_text(size = 7, color = "#555555"), 
    panel.grid = element_blank(), 
    
    legend.position = c(0.98, 1.063),
    legend.justification = c(1, 1), 
    legend.direction = "horizontal",
    
    legend.background = element_rect(fill = alpha("white", 0.8), color = NA, size = 0),
    
    legend.title = element_text(size = 12, vjust = 0.8),
    legend.text = element_text(size = 10),
    
    legend.key.width = unit(1.2, "cm"),
    legend.key.height = unit(0.25, "cm"),
    legend.margin = margin(2, 2, 2, 2),
    
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(color = "gray40", size = 13, margin = margin(b = 15))
  ) +
  
  labs(
    title = "Temporal Distribution of Missing Sensor Readings in Munich",
    subtitle = paste0("Number of sensors: 78       Range: ", start_date_2, " to ", end_date_2, " (Each cell \u2248 10 days)"),
    x = NULL,
    y = "Sensor ID"
  )

print(p_grid_all)





days_all_missing <- df_all %>%
  group_by(date) %>%
  summarise(all_not_found = all(status == "NOT_FOUND")) %>%
  filter(all_not_found) %>%
  pull(date)

days_all_missing







