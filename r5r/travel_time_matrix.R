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
TCD <- read.csv("data/distance_df.csv", fileEncoding="CP949")
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

# routing inputs
mode <- c('walk', 'transit')
max_trip_duration <- 60 # minutes

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

# Set how often to save intermediate results (every 1000 rows)
save_frequency <- 10000

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
  
  # estimate travel time matrix
  # expanded_travel_time_matrix
  # travel_time_matrix
  ttm <- travel_time_matrix(r5r_core,   
                            origins = origins[i,],
                            destinations = destinations[i,],    
                            mode = mode,
                            mode_egress = "WALK",
                            max_trip_duration = max_trip_duration,
                            max_walk_time = 20,
                            departure_datetime = departure_datetime )
  
  
  
  results <- rbind(results, ttm)
  
  # Save intermediate results periodically
  if (i %% save_frequency == 0) {
    write.csv(results, "results_processed.csv", row.names = FALSE)
    cat("Progress saved up to row", i, "of", nrow(TCD), "\n")
  }

  # Print status update every 100 iterations
  if (i %% 10000 == 0) {
    cat("Processing iteration:", i, "of", end, "\n")
  }
}

# Save complete results
write.csv(results, "results/results_final.csv", row.names = FALSE, fileEncoding="utfg-8" )
# write.csv(TCD, "TCD_final.csv", row.names = FALSE)
View(results)

#-------------------------------------#

r5r::stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)

