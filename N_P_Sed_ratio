# Load packages
library(dplyr)
library(readr)
library(tidyverse)
library(data.table)


# Specify the file path
sq_path <- "S:/Mikolaj/TxtInOut-CS4/" # Change the directory


# Read table function
read_tbl <- function(file, run_path, n_skip) {
  file_path <- paste0(run_path, '/', file)
  
  col_names <- read_lines(file = file_path, skip = 1, n_max = 1, lazy = FALSE) %>%
    str_trim(.) %>%
    str_split(., '[:space:]+') %>%
    unlist()
  
  name_duplicate <- table(col_names) %>%
    .[. > 1]
  if(length(name_duplicate) > 0) {
    for (i in 1:length(name_duplicate)) {
      col_names[col_names == names(name_duplicate[i])] <-
        paste0(names(name_duplicate[i]), 1:name_duplicate[i])
    }
  }
  
  fread(file_path, skip = n_skip, header = FALSE) %>%
    set_names(., col_names) %>%
    tibble(.)
}


# Read file
channel_sd_day <- read_tbl('channel_sd_day.txt', sq_path, 3)

# Specify the columns you want to keep
columns_to_keep <- c("jday", "mon", "day", "yr", "unit", "gis_id", "name", 
                     "flo_out", "sed_out", "orgn_out", "sedp_out", "no3_out", 
                     "solp_out", "chla_out", "nh3_out", "no2_out")


# Create a new data frame with only the selected columns
df_selected <- channel_sd_day[, columns_to_keep]

# Calculate the sum of no3, orgn, nh3, and no2
df_selected <- df_selected %>%
  mutate(sum_nutrients_N = no3_out + orgn_out + nh3_out + no2_out)

# Calculate the ratio in mg/L per day and store it in a new column "N_ratio_mgl"
df_selected <- df_selected %>%
  mutate(N_ratio_mgl = ifelse(flo_out == 0, 0, (sum_nutrients_N * 1000) / (flo_out * 86400)))

# Calculate the sum of sedp and solp
df_selected <- df_selected %>%
  mutate(sum_nutrients_P = solp_out + sedp_out)

# Calculate the ratio in mg/L per day and store it in a new column "P_ratio_mgl"
df_selected <- df_selected %>%
  mutate(P_ratio_mgl = ifelse(flo_out == 0, 0, (sum_nutrients_P * 1000) / (flo_out * 86400)))

# Calculate the ratio of sed_out (in tons) to flo_out and store it in a new column "sed_ratio_mgl"
df_selected <- df_selected %>%
  mutate(sed_ratio_mgl = ifelse(flo_out == 0, 0, (sed_out * 1e6) / (flo_out * 86400)))


# Specify the thresholds for each parameter
threshold_N <- 1  # Change this to the threshold for N
threshold_P <- 1  # Change this to the threshold for P
threshold_Sed <- 1  # Change this to the threshold for Sediment

# Calculate how many days each parameter's ratio is above or below the threshold 
# For all channels 
days_above_threshold_N <- sum(df_selected$N_ratio_mgl > threshold_N)
days_below_threshold_N <- sum(df_selected$N_ratio_mgl < threshold_N)

days_above_threshold_P <- sum(df_selected$P_ratio_mgl > threshold_P)
days_below_threshold_P <- sum(df_selected$P_ratio_mgl < threshold_P)

days_above_threshold_Sed <- sum(df_selected$sed_ratio_mgl > threshold_Sed)
days_below_threshold_Sed <- sum(df_selected$sed_ratio_mgl < threshold_Sed)


# Print the number of days for each parameter (sum of all channels)
cat("Days with N ratio above the threshold:", days_above_threshold_N, "\n")
cat("Days with N ratio below the threshold:", days_below_threshold_N, "\n")

cat("Days with P ratio above the threshold:", days_above_threshold_P, "\n")
cat("Days with P ratio below the threshold:", days_below_threshold_P, "\n")

cat("Days with Sediment ratio above the threshold:", days_above_threshold_Sed, "\n")
cat("Days with Sediment ratio below the threshold:", days_below_threshold_Sed, "\n")


# Calculate the frequency of exceeding the thresholds for each "unit" (channel)
frequency_summary_mean <- df_selected %>%
  group_by(unit) %>%
  summarize(
    freq_above_threshold_N = mean(N_ratio_mgl > threshold_N, na.rm = TRUE),
    freq_above_threshold_P = mean(P_ratio_mgl > threshold_P, na.rm = TRUE),
    freq_above_threshold_Sed = mean(sed_ratio_mgl > threshold_Sed, na.rm = TRUE)
  )

# Print the frequency summary
print(frequency_summary_mean)



# Ratio for all channels (average of all)
# Calculate the total number of rows
total_rows <- nrow(df_selected)

# Calculate the average number of days for each parameter and threshold
avg_days_above_threshold_N <- days_above_threshold_N / total_rows
avg_days_below_threshold_N <- days_below_threshold_N / total_rows

avg_days_above_threshold_P <- days_above_threshold_P / total_rows
avg_days_below_threshold_P <- days_below_threshold_P / total_rows

avg_days_above_threshold_Sed <- days_above_threshold_Sed / total_rows
avg_days_below_threshold_Sed <- days_below_threshold_Sed / total_rows

# Print the average results for each parameter and threshold
cat("Average Days with N ratio above the threshold:", avg_days_above_threshold_N, "\n")
cat("Average Days with N ratio below the threshold:", avg_days_below_threshold_N, "\n")

cat("Average Days with P ratio above the threshold:", avg_days_above_threshold_P, "\n")
cat("Average Days with P ratio below the threshold:", avg_days_below_threshold_P, "\n")

cat("Average Days with Sediment ratio above the threshold:", avg_days_above_threshold_Sed, "\n")
cat("Average Days with Sediment ratio below the threshold:", avg_days_below_threshold_Sed, "\n")




