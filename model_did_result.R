# ===========================================
# Step 0: Install and load necessary packages
# ===========================================
library(dplyr)
library(lubridate)
library(fixest)
library(circular)



# ===========================================
# Step 1: Baseline DiD Model
# ===========================================
did_results_2x5 <- function(sensor_list, weather_all, sensor_groups) {

  panel_data <- bind_rows(sensor_list, .id = "filename") 
  
  df_did <- panel_data %>%
    left_join(weather_all, by = "timestamp") %>%
    left_join(sensor_groups, by = "filename")
  
  df_did <- df_did %>%
    mutate(
      log_pm25 = log(PM2.5 + 1),
      wind_dir_factor = cut(direction, breaks = seq(0, 360, by = 45), include.lowest = TRUE),
      
      post_ban = ifelse(timestamp >= ymd_hms("2023-02-01 00:00:00"), 1, 0),
      post_speed = ifelse(timestamp >= ymd_hms("2024-06-01 00:00:00"), 1, 0),
      
      treat_ban = ifelse(grepl("Ring|Moosacher|Landshuter", group, ignore.case = TRUE), 1, 0),
      treat_speed = ifelse(grepl("Moosacher|Landshuter", group, ignore.case = TRUE), 1, 0),
      
      DiD_stage_1 = post_ban * treat_ban,
      DiD_stage_2 = post_speed * treat_speed,
      
      year_month = format(timestamp, "%Y-%m"), 
      hour = as.factor(hour(timestamp)) 
    )
  
  did_model <- feols(
    log_pm25 ~ DiD_stage_1 + DiD_stage_2 + temperature + moisture + wind + preci_indicator + precipitation + wind_dir_factor |
      filename + year_month + hour,
    data = df_did, cluster = ~filename
  )
  
  print(summary(did_model))
  
  return(list(data = df_did, model = did_model))
}



did_results_10 <- function(sensor_list, weather_all, sensor_groups) {
  
  panel_data <- bind_rows(sensor_list, .id = "filename") 
  
  df_did <- panel_data %>%
    left_join(weather_all, by = "timestamp") %>%
    left_join(sensor_groups, by = "filename")
  
  df_did <- df_did %>%
    mutate(
      log_pm10 = log(PM10 + 1),
      wind_dir_factor = cut(direction, breaks = seq(0, 360, by = 45), include.lowest = TRUE),
      
      post_ban = ifelse(timestamp >= ymd_hms("2023-02-01 00:00:00"), 1, 0),
      post_speed = ifelse(timestamp >= ymd_hms("2024-06-01 00:00:00"), 1, 0),
      
      treat_ban = ifelse(grepl("Ring|Moosacher|Landshuter", group, ignore.case = TRUE), 1, 0),
      treat_speed = ifelse(grepl("Moosacher|Landshuter", group, ignore.case = TRUE), 1, 0),
      
      DiD_stage_1 = post_ban * treat_ban,
      DiD_stage_2 = post_speed * treat_speed,
      
      year_month = format(timestamp, "%Y-%m"), 
      hour = as.factor(hour(timestamp))       
    )
  
  did_model <- feols(
    log_pm10 ~ DiD_stage_1 + DiD_stage_2 + temperature + moisture + wind + preci_indicator + precipitation + wind_dir_factor |
      filename + year_month + hour,
    data = df_did, cluster = ~filename
  )
  
  print(summary(did_model))
  
  return(list(data = df_did, model = did_model))
}



did_results_2x5_all <- did_results_2x5(sensor_data_cleaned_list_modified, weather_all_imp, sensors_classified_grouped)
did_results_2x5_all$model


#> did_results_2x5_all$model
#OLS estimation, Dep. Var.: log_pm25
#Observations: 2,276,352
#Fixed-effects: filename: 78,  year_month: 40,  hour: 24
#Standard-errors: Clustered (filename) 
#                            Estimate Std. Error   t value   Pr(>|t|)    
#  DiD_stage_1              -0.157958   0.038500  -4.10279 1.0053e-04 ***
#  DiD_stage_2              -0.076614   0.034014  -2.25238 2.7147e-02 *  
#  temperature               0.005179   0.000974   5.32006 9.8772e-07 ***
#  moisture                  0.009446   0.000485  19.49498  < 2.2e-16 ***
#  wind                     -0.101728   0.005259 -19.34275  < 2.2e-16 ***
#  preci_indicator          -0.195184   0.009452 -20.64992  < 2.2e-16 ***
#  precipitation            -0.055934   0.003026 -18.48644  < 2.2e-16 ***
#  wind_dir_factor(45,90]    0.044010   0.004067  10.82081  < 2.2e-16 ***
#  wind_dir_factor(90,135]  -0.158127   0.010458 -15.12089  < 2.2e-16 ***
#  wind_dir_factor(135,180] -0.423589   0.023637 -17.92060  < 2.2e-16 ***
#  wind_dir_factor(180,225] -0.510000   0.027012 -18.88083  < 2.2e-16 ***
#  wind_dir_factor(225,270] -0.402679   0.021218 -18.97856  < 2.2e-16 ***
#  wind_dir_factor(270,315] -0.192218   0.011306 -17.00209  < 2.2e-16 ***
#  wind_dir_factor(315,360] -0.052452   0.004609 -11.37970  < 2.2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#RMSE: 0.774101     Adj. R2: 0.472713
#Within R2: 0.131443




did_results_10_all <- did_results_10(sensor_data_cleaned_list_modified_2, weather_all_imp, sensors_classified_grouped)
did_results_10_all$model


#> did_results_10_all$model
#OLS estimation, Dep. Var.: log_pm10
#Observations: 2,276,352
#Fixed-effects: filename: 78,  year_month: 40,  hour: 24
#Standard-errors: Clustered (filename) 
#                            Estimate Std. Error   t value   Pr(>|t|)    
#  DiD_stage_1              -0.053061   0.024182  -2.19420 3.1237e-02 *  
#  DiD_stage_2              -0.128103   0.038419  -3.33440 1.3168e-03 ** 
#  temperature               0.003841   0.000718   5.35074 8.7309e-07 ***
#  moisture                  0.014022   0.000487  28.76620  < 2.2e-16 ***
#  wind                     -0.078611   0.003622 -21.70688  < 2.2e-16 ***
#  preci_indicator          -0.145122   0.005590 -25.96209  < 2.2e-16 ***
#  precipitation            -0.044943   0.002262 -19.86756  < 2.2e-16 ***
#  wind_dir_factor(45,90]    0.022983   0.002503   9.18098 5.3737e-14 ***
#  wind_dir_factor(90,135]  -0.099973   0.005745 -17.40054  < 2.2e-16 ***
#  wind_dir_factor(135,180] -0.280431   0.015021 -18.66960  < 2.2e-16 ***
#  wind_dir_factor(180,225] -0.329001   0.017234 -19.09048  < 2.2e-16 ***
#  wind_dir_factor(225,270] -0.268759   0.014239 -18.87525  < 2.2e-16 ***
#  wind_dir_factor(270,315] -0.135145   0.007678 -17.60106  < 2.2e-16 ***
#  wind_dir_factor(315,360] -0.042302   0.002750 -15.38003  < 2.2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#RMSE: 0.472096     Adj. R2: 0.566884
#Within R2: 0.23481






# ===========================================
# Step 2: DiD Model with Interactions
# ===========================================
did_results_2x5_interactions <- function(sensor_list, weather_all, sensor_groups) {
  
  panel_data <- bind_rows(sensor_list, .id = "filename") 
  
  df_did <- panel_data %>%
    left_join(weather_all, by = "timestamp") %>%
    left_join(sensor_groups, by = "filename")
  
  df_did <- df_did %>%
    mutate(
      log_pm25 = log(PM2.5 + 1),
      wind_dir_factor = cut(direction, breaks = seq(0, 360, by = 45), include.lowest = TRUE),
      
      post_ban = ifelse(timestamp >= ymd_hms("2023-02-01 00:00:00"), 1, 0),
      post_speed = ifelse(timestamp >= ymd_hms("2024-06-01 00:00:00"), 1, 0),
      
      treat_ban = ifelse(grepl("Ring|Moosacher|Landshuter", group, ignore.case = TRUE), 1, 0),
      treat_speed = ifelse(grepl("Moosacher|Landshuter", group, ignore.case = TRUE), 1, 0),
      
      DiD_stage_1 = post_ban * treat_ban,
      DiD_stage_2 = post_speed * treat_speed,
      
      is_weekend = ifelse(format(timestamp, "%u") %in% c("6", "7"), 1, 0),
      is_peak = ifelse(hour(timestamp) %in% c(7, 8, 9, 16, 17, 18, 19), 1, 0),
      
      is_workday_peak = ifelse(is_weekend == 0 & is_peak == 1, 1, 0),
      
      year_month = format(timestamp, "%Y-%m"),
      hour = as.factor(hour(timestamp))        
    )
  
  did_model <- feols(
    log_pm25 ~ DiD_stage_1 * is_weekend + DiD_stage_2 * is_workday_peak + temperature + moisture + wind + preci_indicator + precipitation + wind_dir_factor |
      filename + year_month + hour,
    data = df_did, cluster = ~filename
  )
  
  print(summary(did_model))
  
  return(list(data = df_did, model = did_model))
}




did_results_10_interactions <- function(sensor_list, weather_all, sensor_groups) {
  
  panel_data <- bind_rows(sensor_list, .id = "filename") 
  
  df_did <- panel_data %>%
    left_join(weather_all, by = "timestamp") %>%
    left_join(sensor_groups, by = "filename")
  
  df_did <- df_did %>%
    mutate(
      log_pm10 = log(PM10 + 1),
      wind_dir_factor = cut(direction, breaks = seq(0, 360, by = 45), include.lowest = TRUE),
      
      post_ban = ifelse(timestamp >= ymd_hms("2023-02-01 00:00:00"), 1, 0),
      post_speed = ifelse(timestamp >= ymd_hms("2024-06-01 00:00:00"), 1, 0),
      
      treat_ban = ifelse(grepl("Ring|Moosacher|Landshuter", group, ignore.case = TRUE), 1, 0),
      treat_speed = ifelse(grepl("Moosacher|Landshuter", group, ignore.case = TRUE), 1, 0),
      
      DiD_stage_1 = post_ban * treat_ban,
      DiD_stage_2 = post_speed * treat_speed,
      is_peak = ifelse(hour(timestamp) %in% c(7, 8, 9, 16, 17, 18, 19), 1, 0),
      is_weekend = ifelse(format(timestamp, "%u") %in% c("6", "7"), 1, 0),
      is_workday_peak = ifelse(is_weekend == 0 & is_peak == 1, 1, 0),
      
      year_month = format(timestamp, "%Y-%m"), 
      hour = as.factor(hour(timestamp)) 
    )
  
  did_model <- feols(
    log_pm10 ~ DiD_stage_1 * is_weekend + DiD_stage_2 * is_workday_peak + temperature + moisture + wind + preci_indicator + precipitation + wind_dir_factor |
      filename + year_month + hour,
    data = df_did, cluster = ~filename
  )
  
  print(summary(did_model))
  
  return(list(data = df_did, model = did_model))
}



did_results_2x5_all_interactions <- did_results_2x5_interactions(sensor_data_cleaned_list_modified, weather_all_imp, sensors_classified_grouped)
did_results_2x5_all_interactions$model


#> did_results_2x5_all_interactions$model
#OLS estimation, Dep. Var.: log_pm25
#Observations: 2,276,352
#Fixed-effects: filename: 78,  year_month: 40,  hour: 24
#Standard-errors: Clustered (filename) 
#                               Estimate Std. Error   t value   Pr(>|t|)    
#  DiD_stage_1                 -0.150139   0.038493  -3.90045 2.0391e-04 ***
#  is_weekend                   0.031151   0.003737   8.33605 2.2890e-12 ***
#  DiD_stage_2                 -0.066917   0.011851  -1.85866 4.2897e-04 *** 
#  is_workday_peak              0.043516   0.002620  16.61233  < 2.2e-16 ***
#  temperature                  0.005248   0.000975   5.38299 7.6666e-07 ***
#  moisture                     0.009454   0.000485  19.50849  < 2.2e-16 ***
#  wind                        -0.101774   0.005261 -19.34456  < 2.2e-16 ***
#  preci_indicator             -0.194933   0.009460 -20.60648  < 2.2e-16 ***
#  precipitation               -0.055917   0.003022 -18.50051  < 2.2e-16 ***
#  wind_dir_factor(45,90]       0.044341   0.004065  10.90878  < 2.2e-16 ***
#  wind_dir_factor(90,135]     -0.158199   0.010467 -15.11429  < 2.2e-16 ***
#  wind_dir_factor(135,180]    -0.423395   0.023637 -17.91212  < 2.2e-16 ***
#  wind_dir_factor(180,225]    -0.509190   0.026965 -18.88353  < 2.2e-16 ***
#  wind_dir_factor(225,270]    -0.402230   0.021193 -18.97950  < 2.2e-16 ***
#  wind_dir_factor(270,315]    -0.192271   0.011317 -16.98959  < 2.2e-16 ***
#  wind_dir_factor(315,360]    -0.052552   0.004607 -11.40617  < 2.2e-16 ***
#  DiD_stage_1:is_weekend      -0.027397   0.004639  -5.90527 8.9762e-08 ***
#  DiD_stage_2:is_workday_peak -0.065648   0.007401  -8.86984 2.1371e-13 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#RMSE: 0.773909     Adj. R2: 0.512838
#Within R2: 0.14165



did_results_10_all_interactions <- did_results_10_interactions(sensor_data_cleaned_list_modified_2, weather_all_imp, sensors_classified_grouped)
did_results_10_all_interactions$model


#> did_results_10_all_interactions$model
#OLS estimation, Dep. Var.: log_pm10
#Observations: 2,276,352
#Fixed-effects: filename: 78,  year_month: 40,  hour: 24
#Standard-errors: Clustered (filename) 
#                               Estimate Std. Error   t value   Pr(>|t|)    
#  DiD_stage_1                 -0.046900   0.008951  -1.95816 4.0834e-04 ***  
#  is_weekend                   0.018470   0.002224   8.30434 2.6352e-12 ***
#  DiD_stage_2                 -0.122116   0.028525  -2.76977 2.1896e-05 *** 
#  is_workday_peak              0.021469   0.001421  15.10709  < 2.2e-16 ***
#  temperature                  0.003887   0.000719   5.41007 6.8724e-07 ***
#  moisture                     0.014028   0.000488  28.76937  < 2.2e-16 ***
#  wind                        -0.078638   0.003622 -21.71139  < 2.2e-16 ***
#  preci_indicator             -0.144999   0.005582 -25.97720  < 2.2e-16 ***
#  precipitation               -0.044929   0.002264 -19.84142  < 2.2e-16 ***
#  wind_dir_factor(45,90]       0.023181   0.002509   9.23866 4.1617e-14 ***
#  wind_dir_factor(90,135]     -0.099979   0.005752 -17.38201  < 2.2e-16 ***
#  wind_dir_factor(135,180]    -0.280298   0.015024 -18.65710  < 2.2e-16 ***
#  wind_dir_factor(180,225]    -0.328500   0.017202 -19.09609  < 2.2e-16 ***
#  wind_dir_factor(225,270]    -0.268471   0.014223 -18.87618  < 2.2e-16 ***
#  wind_dir_factor(270,315]    -0.135119   0.007688 -17.57571  < 2.2e-16 ***
#  wind_dir_factor(315,360]    -0.042378   0.002748 -15.41908  < 2.2e-16 ***
#  DiD_stage_1:is_weekend      -0.021586   0.003454  -6.24991 2.1034e-08 ***
#  DiD_stage_2:is_workday_peak -0.028600   0.005268  -5.42910 6.3629e-07 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#RMSE: 0.471846     Adj. R2: 0.596976
#Within R2: 0.244973





