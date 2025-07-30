###Decoding the spread of non-indigenous fishes in the Mediterranean Sea

###For inquiries contact to @Vagenas G. 2024 (g.vagenas@hcmr.gr) | (georgvagenas@gmail.com)

#Required_Libraries


#NEW SET 2025 analysis

# Required Libraries
library(terra)        # For spatial operations
library(tidyverse)    # For data manipulation
library(doBy)         # For ordering and summaries
library(powerjoin)    # For advanced joins
library(ggplot2)      # For visualization
#install.packages("ggridges")
library(ggridges)     # For density plots

# Set Working Directory
setwd("C:/Users/geo_v/Desktop/rSDMs/MHW_NIS/")

str(DATA)

# Read and Preprocess Data
DATA <- read.csv("Dataset_Zenetos_2025_Triple_CSVF.csv", header = TRUE, sep = ",", dec = ".") %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    year = as.integer(year)
  ) %>%
  filter(!is.na(Latitude), !is.na(Longitude))  # Remove invalid coordinates

# Country Classification System
country_classification <- tribble(
  ~CounTurkeyy,            ~country_id, ~msfd_id,
  "Greece",             "GR",       "EMED",
  "Turkey",             "TR",       "EMED",
  "Syria",              "SY",       "EMED",
  "Cyprus",             "CY",       "EMED",
  "Lebanon",            "LB",       "EMED",
  "Israel",             "IL",       "EMED",
  "Palestine Authority","PS",       "EMED",
  "Egypt",              "EG",       "EMED",
  "Libya",              "LY",       "EMED",  # Primary assignment
  "Albania",            "AL",       "ADRIA",
  "Montenegro",         "ME",       "ADRIA",
  "Croatia",            "HR",       "ADRIA",
  "Tunisia",            "TN",       "CMED",
  "Italy",              "IT",       "CMED",  # Primary assignment
  "Malta",              "MT",       "CMED",
  "Spain",              "ES",       "WMED",
  "Algeria",            "DZ",       "WMED",
  "France",             "FR",       "WMED",
  "Morocco",            "MA",       "WMED"
)

# Add country_id and msfd_id
DATA <- DATA %>%
  left_join(country_classification, by = "CounTurkeyy") %>%
  filter(!is.na(country_id))  # Remove unclassified countries

# Coordinate Validation using Mediterranean Bounding Box
med_bbox <- c(xmin = -6, xmax = 36, ymin = 30, ymax = 46)  # Mediterranean approx
valid_coords <- DATA %>%
  filter(
    between(Longitude, med_bbox["xmin"], med_bbox["xmax"]),
    between(Latitude, med_bbox["ymin"], med_bbox["ymax"])
  )

# Spatial Object Creation
spat_data <- vect(
  valid_coords,
  geom = c("Longitude", "Latitude"),
  crs = "EPSG:4326"
)

# Optional: Visualize points
plot(spat_data, col = "red", pch = 16)

extent<-ext(spat_data)

plot(extent)
# writeVector(spat_data, "invasive_species.shp", overwrite = TRUE)

# Data Aggregation (First Sighting per Species-Country) with proper ordering
agg_data <- valid_coords %>%
  group_by(species, country_id,Latitude,Longitude) %>%
  summarise(
    first_area_sighting = min(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(first_area_sighting) %>%  # Arrange by year first
  mutate(ID = as.numeric(factor(species))) %>%  # Create species ID
  arrange(species, first_area_sighting)  # Then arrange by species and year

# Verify the results
head(agg_data,20)

#works

# Momentum Calculation Function
calculate_momentum <- function(data) {
  data <- data %>%
    arrange(ID, first_area_sighting,Latitude,Longitude) %>%
    group_by(ID) %>%
    mutate(
      t_step = row_number() - 1,  # t0, t1, t2...
      t_flag = ifelse(t_step < 10, 1, 0)  # Limit to t0-t50
    ) %>%
    filter(t_flag == 1) %>%
    ungroup()
  return(data)
}


# Apply Momentum Calculation
momentum_data <- calculate_momentum(agg_data)


# Define east-to-west ordering for countries
country_order <- c("EG", "PS", "IL", "LB", "SY", "TR", "CY", "GR", 
                   "AL", "ME", "HR", "LY", "IT", "MT", "TN", "DZ", "ES")

# Define east-to-west ordering for MSFD regions
msfd_order <- c("EMED", "ADRIA", "CMED", "WMED")


generate_heatmap <- function(data, group_var) {
  group_sym <- sym(group_var)
  
  heat_data <- data %>%
    count(!!group_sym, t_step) %>%
    complete(!!group_sym, t_step = 0:10, fill = list(n = 0)) %>%
    filter(!is.na(!!group_sym))
  
  # Apply ordering based on group_var
  if (group_var == "country_id") {
    heat_data <- heat_data %>%
      mutate(!!group_sym := factor(!!group_sym, levels = country_order))
  } else if (group_var == "msfd_id") {
    heat_data <- heat_data %>%
      mutate(!!group_sym := factor(!!group_sym, levels = msfd_order))
  }
  
  ggplot(heat_data, aes(x = !!group_sym, y = t_step, fill = n)) +
    geom_tile(color = "white", lwd = 0.5) +
    scale_fill_viridis_c(option = "inferno") +
    scale_y_continuous(
      breaks = 0:10,
      labels = c("t₀", paste0("t", 1:10))
    ) +
    labs(
      x = ifelse(group_var == "country_id", "Countries", "MSFD Regions"),
      y = "Introduction Momentum",
      fill = "Count"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# Generate and save heatmaps with east-to-west ordering
ggsave("country_heatmap.png", 
       generate_heatmap(momentum_data, "country_id"),
       width = 10, height = 6)

ggsave("msfd_heatmap.png", 
       generate_heatmap(momentum_data %>% left_join(country_classification, by = "country_id"), 
                        "msfd_id"),
       width = 8, height = 6)


# Temporal Analysis (Parallel Processing)
library(future.apply)
plan(multisession)  # Enable parallel processing


# Create the temporal_analysis data frame from your momentum_data_geo
temporal_analysis <- as.data.frame(momentum_data) %>%
  group_by(ID) %>%  # Group by species ID
  arrange(t_step) %>%  # Order by time step
  mutate(
    delta_t = first_area_sighting - lag(first_area_sighting)  # Calculate time differences
  ) %>%
  filter(!is.na(delta_t)) %>%  # Remove NA values from first time step
  ungroup()


# ECDF Plot
ecdf_plot <- ggplot(temporal_analysis, aes(x = delta_t, y = factor(t_step), fill = after_stat(ecdf))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "ECDF", option = "plasma") +
  scale_y_discrete(
    labels = c(expression(Delta*"t"["0-1"]),
               lapply(1:9, function(i) bquote(Delta*"t"[.(i)])))
  ) +
  labs(x = "Time Difference (Years)", y = "Introduction Step") +
  theme_ridges()+scale_x_continuous(limit=c(0,20))

ggsave("ecdf_analysis.png", ecdf_plot, width = 9, height = 7)

# Cumulative Spread Analysis
cumulative_spread_median <- temporal_analysis %>%
  group_by(t_step) %>%
  summarise(median_delta = median(delta_t, na.rm = TRUE)) %>%
  mutate(cumulative_time = cumsum(coalesce(median_delta, 0))) 

ggplot(cumulative_spread_median, aes(x = t_step, y = cumulative_time)) +
  geom_line(color = "steelblue", size = 1.5) +
  geom_point(size = 3) +
  labs(title = "Cumulative Spread Timeline (Median)",
       x = "Introduction Step", 
       y = "Cumulative Years") +
  theme_bw()

cumulative_spread_mean <- temporal_analysis %>%
  group_by(t_step) %>%
  summarise(mean_delta = mean(delta_t, na.rm = TRUE)) %>%
  mutate(cumulative_time = cumsum(coalesce(mean_delta, 0))) 

ggplot(cumulative_spread_mean, aes(x = t_step, y = cumulative_time)) +
  geom_line(color = "steelblue", size = 1.5) +
  geom_point(size = 3) +
  labs(title = "Cumulative Spread Timeline (Average)",
       x = "Introduction Step", 
       y = "Cumulative Years") +
  theme_bw()



# 1. Define the custom labels using Unicode subscripts
custom_labels <- c(
  "t₁₋₀", "t₂₋₁", "t₃₋₂", "t₄₋₃", "t₅₋₄",
  "t₆₋₅", "t₇₋₆", "t₈₋₇", "t₉₋₈", "t₁₀₋₉"
)


# 2. Create the plot
ggplot(cumulative_spread, aes(x = t_step, y = cumulative_time)) +
  geom_line(color = "steelblue", size = 1.5) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = 1:10,  # Positions
  ) +
  labs(
    title = "Cumulative Spread Timeline",
    x = "Introduction Step", 
    y = "Cumulative Years"
  ) +
  theme_bw()
    
# Save final dataset
#write.csv(momentum_data, "processed_invasive_species_data.csv", row.names = FALSE)




#ADD THE SPATIAL ASPECT


library(terra)
MED<-vect("C:/Users/geo_v/Desktop/rSDMs/MHW_NIS/MED/iho/iho.shp")
MED

# 1. First ensure all polygons are valid
MED_valid <- makeValid(MED)

# 2. Create a common ID for all features
MED_valid$merge_id <- 1

# 3. Aggregate with dissolve (this is the key step)
MED_merged <- aggregate(MED_valid, by = "merge_id", dissolve = TRUE)

# 4. Force merge with minimal buffer if lines remain
#    (using 0.00001 instead of 0 for better stability)
MED_final <- buffer(MED_merged, width = 0.00001)

# 5. Verify the result
plot(MED_final, col = "lightblue", main = "Fully Merged Mediterranean")

library(ncdf4)
library(terra)

# Open the NetCDF file #einai ena aplo raster giati mas dini meses times
nc_file_temp <- rast("Project/layers/thetao_baseline_2000_2019_depthsurf_4e3e_1426_a71d_U1751549382872.nc")  # Replace with your file path
plot(nc_file_temp,ext=extent)

# 4. Add points to the plot
plot(spat_data, add = TRUE, col = "red", pch = 16, cex = 1.2)

nc_file_temp<-mask(nc_file_temp,MED_final)

# Momentum Calculation Function
calculate_momentum <- function(data) {
  data <- data %>%
    arrange(ID, first_area_sighting) %>%
    group_by(ID) %>%
    mutate(
      t_step = row_number() - 1,  # t0, t1, t2...
      t_flag = ifelse(t_step < 10, 1, 0)  # Limit to t0-t50
    ) %>%
    filter(t_flag == 1) %>%
    ungroup()
  return(data)
}


# Apply Momentum Calculation
momentum_data_geo<-  vect(momentum_data,
geom = c("Longitude", "Latitude"),
crs = "EPSG:4326")

momentum_data_geo

#Vamos

library(terra)

# 1. Prepare empty list to store velocity rasters for each species
velocity_rasters <- list()

# 2. Get unique species IDs
unique_ids <- unique(momentum_data_geo$ID)

# Create a lookup table of ID to species name
id_to_name <- data.frame(
  ID = momentum_data_geo$ID,
  species = momentum_data_geo$species
) %>% distinct()

# 3. Process each species separately
for(species_id in unique_ids) {
  # Subset data for current species
  species_data <- momentum_data_geo[momentum_data_geo$ID == species_id, ]
  
  # Order by t_step
  species_data <- species_data[order(species_data$t_step), ]
  
  # Skip if only one observation
  if(nrow(species_data) < 2) next
  
  # Get coordinates
  coords <- crds(species_data)
  
  # Calculate distances between consecutive points (in km)
  # Using Haversine distance for better accuracy
  dists <- sqrt((diff(coords[,1]) * 111.32 * cos(coords[,2] * pi/180))^2 + 
                  (diff(coords[,2]) * 111.32)^2)
  
  # Improved calculation with proper handling of duplicate years
  time_diffs <- diff(species_data$first_area_sighting)
  
  # First identify which steps to keep (where time_diff != 0)
  keep <- which(time_diffs != 0)
  
  # Then calculate only for valid steps
  velocities <- numeric(length(time_diffs)) # Initialize empty vector
  velocities[] <- NA # Default to NA
  
  # Only calculate where time_diff != 0
  if(length(keep) > 0) {
    velocities[keep] <- dists[keep]/time_diffs[keep]
  }
  
  # Alternative approach using case-wise calculation:
  # velocities <- mapply(function(d, t) ifelse(t == 0, NA, d/t), 
  #                     dists, time_diffs)
  
  # Create output SpatVector with velocity values
  result_vect <- species_data[-1, ] # Remove first point (no velocity for it)
  result_vect$velocity <- velocities
  
  #plot(species_data,add=TRUE)
  
  # Rasterize velocities for this species
  vel_raster <- rasterize(result_vect, nc_file_temp, field = "velocity")
  
  # Get species name for this ID
  species_name <- id_to_name$species[id_to_name$ID == species_id]
  
  # Create a clean filename (replace spaces with underscores)
  clean_name <- gsub(" ", "_", species_name)
  
  # Store with combined ID-name identifier
  velocity_rasters[[paste0("ID_", species_id, "_", clean_name)]] <- vel_raster}

library(mapview)
#mapview(velocity_rasters[[18]])

# For one raster layer (e.g., element 25 in your list)
non_na_count <- global(!is.na(velocity_rasters[[18]]), "sum", na.rm = TRUE)
print(paste("Number of cells with values:", non_na_count))

# 4. Create a raster stack of all species velocities
velocity_stack <- rast(velocity_rasters)

#SUM VELOCITIES
composite_sum <- sum(velocity_stack, na.rm = TRUE)
names(composite_sum) <- "total_velocity_sum"
composite_sum
#mapview(composite_sum)

#MEAN VELOCITIES
composite_mean <- mean(velocity_stack, na.rm = TRUE)
names(composite_mean) <- "mean_velocity"
composite_mean
#mapview(composite_mean)

#MAX VELOCITIES
composite_max <- max(velocity_stack, na.rm = TRUE)
names(composite_max) <- "max_velocity"
composite_max

# For one raster layer (e.g., element 25 in your list)
non_na_count <- global(!is.na(composite_mean), "sum", na.rm = TRUE)
print(paste("Number of cells with values:", non_na_count))


# Tabulate min/max/mean for each layer
stats<-data.frame(
  Layer = names(velocity_stack),
  Min = global(velocity_stack, "min", na.rm=TRUE)[,1],
  Mean = global(velocity_stack, "mean", na.rm=TRUE)[,1],
  Max = global(velocity_stack, "max", na.rm=TRUE)[,1]
)

summary(stats$Max)
summary(stats$Min)
summary(stats$Mean)

##########

plot(composite_mean)

#vamos aver max -- Inverse Distance Weighting (IDW)

library(terra)
library(gstat)

# 1. Prepare your data (example with known points)
known_pts <- as.points(composite_mean, na.rm = TRUE)
known_df <- as.data.frame(known_pts, geom = "XY")

# 2. Fit a gstat model (IDW example)
gmodel <- gstat(
  formula = mean_velocity~1,  # Using mean value
  locations = ~x+y, 
  data = known_df, 
  nmax = 10  # Use 10 nearest neighbors
)

# 3. Create prediction grid (only over marine areas)
marine_mask <- composite_mean  # Copy of original
values(marine_mask) <- !is.na(values(marine_mask))  # Convert to 1/NA mask

# 4. Perform interpolation WITH masking
filled <- interpolate(
  object = marine_mask,  # Use mask as template
  model = gmodel,
  xyNames = c("x", "y"),
  fun = predict
) %>% 
  mask(nc_file_temp)  # Restrict to marine areas

plot(filled)

# 5. Combine with original (marine areas only)
final_result <- cover(composite_mean, filled)

# Verify results
print(paste("Original NAs:", global(is.na(composite_mean), "sum")[1,1]))
print(paste("Remaining NAs:", global(is.na(final_result), "sum")[1,1]))

# Visual comparison

plot(c(composite_max, filled), main = c("Original", "Interpolated"))


#mapview(filled)

#### UNCERTAINTY #####

# Calculate uncertainty based on point density (inverse distance to nearest points)
pts <- as.points(composite_mean, na.rm = TRUE)  # Your original points

# Create distance-to-nearest-point raster
dist_raster <- distance(marine_mask, pts) %>% 
  mask(marine_mask)

# 2. Create normalized uncertainty (0-1 range)
uncertainty <- 1/(dist_raster + 1)  # +1 avoids division by zero
uncertainty <- uncertainty / global(uncertainty, max, na.rm = TRUE)[1,1]

plot(uncertainty)
plot(dist_raster)
#


#Scale

# 1. First ensure no NA values in distance raster
dist_raster <- mask(dist_raster, marine_mask)

dist_raster_mask<-mask(dist_raster,filled)

plot(dist_raster_mask)

# 2. Scale distances to 0-1 range (1 = farthest/most uncertain)
scaled_uncertainty <- dist_raster_mask / global(dist_raster_mask, max, na.rm = TRUE)[1,1]

scaled_uncertainty

plot(scaled_uncertainty)


plot(c(composite_max, filled,scaled_uncertainty), main = c("Original", "Interpolated","Uncertainty"))



mapview(filled)

mapview(scaled_uncertainty)


plot(filled$var1.pred,main=c("Interpolated"))
plot(momentum_data_geo,add=TRUE)


# 5. Calculate summary statistics across all species
mean_velocity <- mean(velocity_stack, na.rm = TRUE)
max_velocity <- max(velocity_stack, na.rm = TRUE)
sd_velocity <- stdev(velocity_stack, na.rm = TRUE)

# 6. Visualize results
plot(mean_velocity, main = "Mean Spread Velocity (km/year)")
plot(max_velocity, main = "Maximum Spread Velocity (km/year)")
plot(sd_velocity, main = "Standard Deviation of Spread Velocities")


#PLOT EM
library(terra)
library(ggplot2)

# 1. Prepare the data
# Convert velocity raster to data frame
vel_df <- as.data.frame(filled$var1.pred, xy = TRUE, na.rm = TRUE)
names(vel_df) <- c("longitude", "latitude", "velocity")

# Convert points to data frame
pts_df <- as.data.frame(geom(momentum_data_geo)[, c("x", "y")])
names(pts_df) <- c("longitude", "latitude")

# 2. Create the plot
ggplot() +
  # Velocity raster
  geom_raster(data = vel_df, 
              aes(x = longitude, y = latitude, fill = velocity)) +
  
  # Observation points
  geom_point(data = pts_df, 
             aes(x = longitude, y = latitude),
             color = "red",fill="red", size = 0.5, shape = 21) +  # Red X markers
  
  # Color scale
  scale_fill_viridis_c(option = "plasma", 
                       name = "Velocity\n(km/year)",
                       na.value = NA) +
  
  # Titles and theme
  ggtitle("Interpolated Spread of NIS based on momentum theory 
    \nbased on IDW (Observation Points, N=14487)") +
  coord_equal() +  # Maintain aspect ratio
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center title


#PLOT UNCERTAINTY

#PLOT EM
library(terra)
library(ggplot2)

# 1. Prepare the data
# Convert velocity raster to data frame
vel_df <- as.data.frame(scaled_uncertainty$lyr1, xy = TRUE, na.rm = TRUE)
names(vel_df) <- c("longitude", "latitude", "Uncertainty")

# Convert points to data frame
pts_df <- as.data.frame(geom(momentum_data_geo)[, c("x", "y")])
names(pts_df) <- c("longitude", "latitude")

# 2. Create the plot
ggplot() +
  # Velocity raster
  geom_raster(data = vel_df, 
              aes(x = longitude, y = latitude, fill = Uncertainty)) +
  
  # Observation points
  geom_point(data = pts_df, 
             aes(x = longitude, y = latitude),
             color = "red",fill="red", size = 0.5, shape = 21,stroke = 0.5) +  # Red X markers
  
  # Color scale
  scale_fill_viridis_c(option = "plasma", 
                       name = "Uncertainty",
                       na.value = NA) +
  
  # Titles and theme
  ggtitle("Uncertainty based on Inverted Distance (Observation Points, N=14487)") +
  coord_equal() +  # Maintain aspect ratio
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center title


plot(composite_max)

#resample at 10arcmin

library(terra)

# 1. Create target raster at 20 arc-minute resolution (0.166667 degrees)
target_res <- 1/3  # 20 arc-minutes in degrees
target_ext <- ext(-6.05, 36.2, 30.05, 45.8)  # Keep original extent
target_crs <- "EPSG:4326"  # WGS84

target_raster <- rast(
  resolution = target_res,
  extent = target_ext,
  crs = target_crs
)

# 2. Resample using nearest neighbor (for categorical/preserving values)
filled_20min <- resample(
  filled,
  target_raster,
  method = "near"  # Nearest neighbor
)

# 3. Verify results
print(filled_20min)  # Check new resolution
plot(filled_20min[[1]], main = "20 arc-min resolution")