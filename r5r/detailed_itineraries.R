################################################################################
# R5R Public Transit Analysis Script
# Purpose: Calculate and analyze public transit routes between origin-destination pairs
# Author: [Your Name]
# Last Updated: April 28, 2025
################################################################################

#------------------------------------------------------------------------------
# 1. SETUP AND CONFIGURATION
#------------------------------------------------------------------------------

# Set working directory
# library(here)
# setwd(here())
setwd("D:/0.mine/15분생활권_부산/r5r") # Session > Set Working Directory > To Source File Location
getwd()

# Configure Java for R5R
# Note: R5R requires Java to run properly

# Check version of Java currently installed (if any) 
# install.packages('rJavaEnv')  # Uncomment if not installed
rJavaEnv::java_check_version_rjava()

# Install Java 21 if needed
rJavaEnv::java_quick_install(version = 21)

# Allocate 32GB of memory to Java for processing large networks
options(java.parameters = "-Xmx32G")

#------------------------------------------------------------------------------
# 2. LOAD REQUIRED LIBRARIES
#------------------------------------------------------------------------------

library(r5r)        # Core package for routing
library(ggplot2)    # For visualization
library(sf)         # For spatial data handling
library(tictoc)     # For timing operations
library(progress)   # For progress tracking
library(dplyr)      # For data manipulation

# Disable scientific notation for easier reading of results
options(scipen = 999)

#------------------------------------------------------------------------------
# 3. INITIALIZE R5R ROUTING ENGINE
#------------------------------------------------------------------------------

# Set path to data directory containing OSM and GTFS files
data_path = "busan"

# Create R5R core object for routing calculations
# Parameters:
#   - data_path: location of transportation network data
#   - verbose: show detailed information during setup
#   - overwrite: whether to recreate the network if it exists
#   - elevation: elevation data type (NONE, RASTER, etc.)
r5r_core <- setup_r5(data_path = data_path,
                     verbose = TRUE,
                     overwrite = FALSE, 
                     elevation = "NONE")

# Display r5r_core object information
r5r_core

#------------------------------------------------------------------------------
# 4. PREPARE INPUT DATA
#------------------------------------------------------------------------------

# Load origin-destination travel data
TCD <- read.csv("data/distance_df_part_1.csv", fileEncoding="CP949")
View(TCD)

# Prepare origins dataframe (starting points)
origins <- data.frame(
  id = as.character(TCD$start_gid),
  lon = TCD$start_longitude,
  lat = TCD$start_latitude
)

# Prepare destinations dataframe (ending points)
destinations <- data.frame(
  id = as.character(TCD$end_gid),
  lon = TCD$end_longitude,
  lat = TCD$end_latitude
)

# Set default departure time for analysis
# Format: day-month-year hour:minute:second
departure_datetime <- as.POSIXct("14-04-2025 14:13:00",
                                 format = "%d-%m-%Y %H:%M:%S")

#------------------------------------------------------------------------------
# 5. CONFIGURE ANALYSIS PARAMETERS
#------------------------------------------------------------------------------

# Initialize progress bar for tracking
pb <- progress_bar$new(
  format = "진행률: :percent 완료 [:bar] 예상 남은 시간: :eta",
  total = nrow(TCD),
  clear = FALSE,
  width = 60
)

# Initialize results dataframes
results <- data.frame()

# Create columns in TCD to store analysis results
TCD$best_total_duration <- NA
TCD$best_option <- NA
TCD$best_total_distance <- NA

# Set how often to save intermediate results (every 1000 rows)
save_frequency <- 1000

# Set total number of iterations
end <- nrow(TCD)

#------------------------------------------------------------------------------
# 6. MAIN ANALYSIS LOOP
#------------------------------------------------------------------------------

for (i in 1:end) {
  # Update progress bar
  pb$tick()
  
  # Get departure time from data (if available) or use default
  # Note: This line assumes TCD has a departure_datetime column
  # If using the default time for all rows, comment this and use the default departure_datetime
  # dep_datetime <- TCD$departure_datetime[i]
  
  # Calculate detailed itineraries between origin and destination
  # Parameters:
  #   - r5r_core: routing engine
  #   - origins/destinations: start and end points
  #   - mode: transportation modes to consider
  #   - mode_egress: mode for last mile
  #   - departure_datetime: when the trip starts
  #   - max_walk_time: maximum walking time in minutes
  #   - shortest_path: whether to find only shortest path
  det <- detailed_itineraries(
    r5r_core = r5r_core,
    origins = origins[i,],
    destinations = destinations[i,],
    mode = c("WALK", "TRANSIT"),
    mode_egress = "WALK",
    departure_datetime = departure_datetime,
    max_walk_time = 30,
    shortest_path = FALSE
  )
  
  # Process results only if routes were found
  if (nrow(det) > 0) {
    # Add reference to original row ID
    det$i <- i
    
    # Filter to keep only the top 3 route options
    det_filtered <- subset(det, option <= 3)
    
    # Append to master results dataframe
    results <- rbind(results, det_filtered)
    
    # Extract current results for processing
    current_results <- det_filtered
    
    # Count segments in each route option
    # (segment count indicates number of transfers + 1)
    segment_counts <- aggregate(segment ~ option, data = current_results, FUN = max)
    
    # Filter to keep only multi-segment routes (with at least one transfer)
    valid_options <- segment_counts[segment_counts$segment >= 2, ]
    
    # Process only if valid options exist
    if (nrow(valid_options) > 0) {
      # Filter results to include only valid options
      valid_results <- current_results[current_results$option %in% valid_options$option, ]
      
      # Get summary info for each option (first row contains trip totals)
      option_summary <- valid_results %>%
        group_by(option) %>%
        slice(1) %>%
        ungroup()
      
      # Find the option with shortest total duration
      best_option <- option_summary[which.min(option_summary$total_duration), ]
      
      # Save best option details back to TCD dataset
      TCD$best_total_duration[i] <- best_option$total_duration
      TCD$best_option[i] <- best_option$option
      TCD$best_total_distance[i] <- best_option$total_distance
    }
  }
  
  # Save intermediate results periodically
  if (i %% save_frequency == 0) {
    write.csv(TCD, "TCD_processed.csv", row.names = FALSE)
    write.csv(results, "results_processed.csv", row.names = FALSE)
    cat("Progress saved up to row", i, "of", nrow(TCD), "\n")
  }
  
  # Print status update every 100 iterations
  if (i %% 100 == 0) {
    cat("Processing iteration:", i, "of", end, "\n")
  }
}

#------------------------------------------------------------------------------
# 7. SAVE FINAL RESULTS
#------------------------------------------------------------------------------

# Save complete results
write.csv(TCD, "TCD_final.csv", row.names = FALSE)
write.csv(results, "results_final.csv", row.names = FALSE)

# Display final results
View(results)
View(TCD)

# #------------------------------------------------------------------------------
# # 8. EXPORT SPATIAL DATA
# #------------------------------------------------------------------------------
# 
# # Export as shapefile
# # Note: This requires results to be an sf object
# # If results is not an sf object, you need to convert it first
# # Example: results_sf <- st_as_sf(results, coords = c("lon", "lat"), crs = 4326)
# output_path <- file.path(getwd(), "TRANSIT_ROUTES.SHP")
# # Uncomment the line below if results is an sf object
# # st_write(results, output_path)
# 
# #------------------------------------------------------------------------------
# # 9. VISUALIZATION
# #------------------------------------------------------------------------------
# 
# # Visualize route options
# # Note: This plots the first 3 route options with color by transportation mode
# ggplot() +
#   # Uncomment to add transit network as background
#   # geom_sf(data = transit_net$routes, color='gray85') +
#   geom_sf(data = det[1:3,], aes(color=mode)) +
#   facet_wrap(.~option) +
#   theme_void()
# 
# #------------------------------------------------------------------------------
# # 10. AUXILIARY FUNCTIONS (UNCOMMENT IF NEEDED)
# #------------------------------------------------------------------------------
# 
# # Extract street network
# # street_net <- street_network_to_sf(r5r_core)
# 
# # Extract public transport network
# # transit_net <- r5r::transit_network_to_sf(r5r_core)