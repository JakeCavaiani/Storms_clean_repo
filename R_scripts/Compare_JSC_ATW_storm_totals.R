### Script to compare ATW storm total precip and other metrics to my original moisture calcs 
## Created by JSC 9/12/25

## 
## 1) Pull in ATW antecedent moisture stats for each storm
## 2) Pull in JSC original antecedent moisture stats for each storm
## 3) Plot to compare

# ============================== Libraries ========================================
library(here)
library(tidyverse)
library(lubridate)
library(slider)
library(ggpubr) 

# ================================= User inputs UPDATED ==========================
# Define the base directory
data_dir <- here("Output_from_analysis")

# Read in ATW storm totals 
# Function to process a single file
process_file <- function(file_path) {
  # Extract site ID from the file name
  site_id <- str_extract(file_path, "(VAUL|CARI|STRT|FRCH|MOOS|POKE)") 
  
  # Read the file
  data <- read_csv(file_path)
  
  # Add the 'site.ID' column
  data <- data %>%
    mutate(site.ID = site_id)
  
  return(data)
}

# Set the directory containing the files
input_directory <- "~/GitHub/Storms_clean_repo/Output_from_analysis" # Replace with the actual path to your directory

# Get list of files to process
files <- list.files(input_directory, full.names = TRUE, pattern = "*.csv") # Adjust the pattern if needed

# Process all files and combine them into one data frame
ATW_data <- files %>%
  map_dfr(process_file) # This applies `process_file` to each file and combines them

# export file
write_csv(ATW_data, here("Output_from_analysis", "compare_JSC_ATW_storm_totals", "updated_AMC_metrics_2509.csv"))


# Write the combined data to a new CSV file (optional)
# write_csv(combined_data, "combined_output.csv")

# Read in JSC storm totals 
JSC_data <- read_csv(here("Output_from_analysis",
"07_Combine_HI_BETA_FI", "antecedent_HI_FI_AllYears.csv")) %>% 
  select(site.ID, storm.ID, year, precip, precip.week, precip.month, ThreeMonth, Intensity) %>% 
  rename(site.ID = site.ID,
         storm.num = storm.ID,
         total_storm_ppt = precip,
         intensity = Intensity,
         prev7 = precip.week, 
         prev30 = precip.month)


# Make columns similar
ATW_data <- ATW_data %>% 
  mutate(
    year = format(storm_start, "%Y"), 
    unique_storm_id = str_c(site.ID, year, storm.num, sep = "-")
  )

# Create the unique identifier column in JSC_storm_files
JSC_data <- JSC_data %>% 
  mutate(
    unique_storm_id = str_c(site.ID, year, storm.num, sep = "-")
  ) %>% 
  distinct(unique_storm_id, .keep_all = TRUE)

# Summary stats
# Count storms for each site and year
storm_counts <- JSC_data %>%
  group_by(site.ID, year) %>%
  summarize(num_storms = n_distinct(storm.num), .groups = "drop")

print(storm_counts)

storm_counts_ATW <- ATW_data %>%
  group_by(site.ID, year) %>%
  summarize(num_storms = n_distinct(storm.num), .groups = "drop")

# Compare via plot 
# Merge the two datasets by unique_storm_id
merged_data <- ATW_data %>%
  inner_join(JSC_data, by = c("unique_storm_id", "site.ID"), suffix = c("_ATW", "_JSC"))

# Function to generate scatter plots and export as PDF
generate_comparison_plots <- function(data, variables, output_dir) {
  for (variable in variables) {
    # Create the plot
    plot <- data %>%
      ggplot(aes_string(
        x = paste0(variable, "_ATW"),
        y = paste0(variable, "_JSC"),
        color = "site.ID"
      )) +
      geom_point(alpha = 0.7) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      facet_wrap(~ site.ID, scales = "free") +
      labs(
        x = paste(variable, "(ATW_data)"),
        y = paste(variable, "(JSC_data)"),
        title = paste("Comparison of", variable, "by Site"),
        color = "Site ID"
      ) +
      theme_minimal()
    
    # Export the plot as PDF
    ggsave(
      filename = paste0(output_dir, "/", variable, "_comparison.pdf"),
      plot = plot,
      width = 8,
      height = 6
    )
  }
}

# Define the variables of interest and output directory
variables_to_compare <- c("total_storm_ppt", "intensity", "prev7", "prev30")
output_directory <- "~/GitHub/Storms_clean_repo/plots/ATW_JSC_amc_comp"  # Make sure this folder exists or create it using dir.create("plots")

# Run the function to generate and export plots
generate_comparison_plots(merged_data, variables_to_compare, output_directory)



# Add R^2 #
# Merge the two datasets by unique_storm_id
merged_data <- ATW_data %>%
  inner_join(JSC_data, by = c("unique_storm_id", "site.ID"), suffix = c("_ATW", "_JSC"))

# Function to calculate R² and update site labels
update_site_labels_with_r2 <- function(data, x_var, y_var) {
  # Calculate R² for each site and update `site.ID`
  data %>%
    group_by(site.ID) %>%
    mutate(
      r_squared = cor(!!sym(x_var), !!sym(y_var), use = "complete.obs")^2,
      site_with_r2 = paste0(site.ID, " (R² = ", round(r_squared, 2), ")")
    ) %>%
    ungroup()
}

# Function to generate scatter plots and export to PDF
generate_comparison_plots_with_r2 <- function(data, variables, output_dir) {
  for (variable in variables) {
    # Update the site labels for the variable with R²
    data_with_r2 <- update_site_labels_with_r2(data, 
                                               paste0(variable, "_ATW"), 
                                               paste0(variable, "_JSC"))
    
    # Create the plot
    plot <- data_with_r2 %>%
      ggplot(aes_string(
        x = paste0(variable, "_ATW"),
        y = paste0(variable, "_JSC"),
        color = "site.ID"
      )) +
      geom_point(alpha = 0.7) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      facet_wrap(~ site_with_r2, scales = "free") +  # Updated faceting with site + R²
      labs(
        x = paste(variable, "(ATW_data)"),
        y = paste(variable, "(JSC_data)"),
        title = paste("Comparison of", variable),
        color = "Site ID"
      ) +
      theme_minimal() +
      theme(strip.text = element_text(size = 10, face = "bold"))  # Improve facet label readability
    
    # Export the plot as PDF
    ggsave(
      filename = paste0(output_dir, "/", variable, "_comparison_with_r2.pdf"),
      plot = plot,
      width = 10,
      height = 6
    )
  }
}

# Define the variables of interest and output directory
variables_to_compare <- c("total_storm_ppt", "intensity", "prev7", "prev30")
output_directory <- "~/GitHub/Storms_clean_repo/plots/ATW_JSC_amc_comp"  # Ensure this folder exists

# Run the function to generate and export plots with R² annotations in facet labels
generate_comparison_plots_with_r2(merged_data, variables_to_compare, output_directory)



#####  Merge Updated AMC metrics with Storm metrics  ####
# Changing the name of the storm column to match my original dataframe
ATW_filtered <- ATW_data %>% 
  rename(storm.ID = "storm.num", 
         precip = "total_storm_ppt",
         precip.week = "prev7",
         Intensity = "intensity",
         precip.month = "prev30") %>%
  mutate(
    year = format(storm_start, "%Y"),
    unique_storm_id = str_c(site.ID, year, storm.ID, sep = "-")) %>%
  select(site.ID, year, storm.ID, unique_storm_id, precip, precip.week, precip.month, Intensity) %>% 
  mutate(year = as.double(year))

antecedent_HI_FI_AllYears <- read_csv(here("Output_from_analysis", "07_Combine_HI_BETA_FI", "antecedent_HI_FI_AllYears.csv"))

AMC <- antecedent_HI_FI_AllYears[c("Hyst_index", "HI_ymin", "HI_ymax", 
                                                         "site.ID", "storm.ID", "month.x", 
                                                         "day.x", "response_var", "Flush_index", 
                                                         "FI_ymin", "FI_ymax", "year", "Parameter",  
                                                         "Beta_index", "SE", "CI", "Beta_ymin", "Beta_ymax", 
                                                         "t", "df", "p", "precip", "temp", "precip.week", 
                                                         "precip.month", "ThreeMonth", "temp.week", 
                                                         "TOTAL.TIME", "Intensity", "doy", "burn", "pf", 
                                                        "date", "TimeSinceChena")] # selecting the columns that I want

AMC_updated <- full_join(AMC, ATW_filtered, by = c("storm.ID", "site.ID", "year"))

AMC_updated <- AMC_updated[c("Hyst_index", "HI_ymin", "HI_ymax", 
                             "site.ID", "storm.ID", "month.x", 
                             "day.x", "response_var", "Flush_index", 
                             "FI_ymin", "FI_ymax", "year", "Parameter",  
                             "Beta_index", "SE", "CI", "Beta_ymin", "Beta_ymax", 
                             "t", "df", "p", "precip.y", "temp", "precip.week.y", 
                             "precip.month.y", "ThreeMonth", "temp.week", 
                             "TOTAL.TIME", "Intensity.y", "doy", "burn", "pf", 
                             "date", "TimeSinceChena")] # selecting the columns that I want
AMC_updated <- AMC_updated %>% 
  rename(precip = precip.y, 
         precip.week = precip.week.y,
         precip.month = precip.month.y,
         Intensity = Intensity.y)

# export file
write_csv(AMC_updated, here("Output_from_analysis", "compare_JSC_ATW_storm_totals", "updated_AMC_HI_BETA_file_2509.csv"))



