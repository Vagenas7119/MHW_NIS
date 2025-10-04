#NEW SET 2025 analysis - MHW combined

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

annual_c_int_dt <- readRDS("C:/Users/geo_v/Desktop/annual_c_int_dt.rds")

tail(annual_c_int_dt)

#works

# Momentum Calculation Function
calculate_momentum <- function(data) {
  data <- data %>%
    arrange(ID, first_area_sighting,Latitude,Longitude) %>%
    group_by(ID) %>%
    mutate(
      t_step = row_number() - 1,  # t0, t1, t2...
      t_flag = ifelse(t_step < 11, 1, 0)  # Limit to t0-t50
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
msfd_order <- c("EMED","CMED","ADRIA","WMED")


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

# First, join the MSFD information to your momentum data
momentum_data <- momentum_data %>%
  left_join(country_classification %>% select(country_id, msfd_id), 
            by = "country_id") %>%
  filter(!is.na(msfd_id))  # Remove any rows without MSFD classification

# Now generate the MSFD heatmap
msfd_heatmap <- generate_heatmap(momentum_data, "msfd_id") +
  geom_tile(
    color = "white",
    lwd = 1.5,
    linetype = 1
  ) +
  coord_fixed() +
  scale_fill_gradient2(
    low = "white",
    mid = "snow1",
    high = "black"
  ) +
  scale_y_continuous(
    breaks = 0:10,
    labels = c(expression(t[0]),
               expression(t[1]),
               expression(t[2]),
               expression(t[3]),
               expression(t[4]),
               expression(t[5]),
               expression(t[6]),
               expression(t[7]),
               expression(t[8]),
               expression(t[9]),
               expression(t[10]))
  ) +
  geom_text(aes(label = n), color = "white", size = 6) +
  guides(
    fill = guide_colourbar(
      title = "NIS",
      barwidth = 1,
      barheight = 20
    )
  ) +
  labs(
    x = "MSFD Regions",
    y = "Introductions (Momentum)"
  ) +
  theme_light() +
  theme(
    axis.text = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Display the plot
msfd_heatmap

#Country
country_heatmap <- generate_heatmap(momentum_data, "country_id") +
  geom_tile(
    color = "white",
    lwd = 1.5,
    linetype = 1
  ) +
  coord_fixed() +
  scale_fill_gradient2(
    low = "white",
    mid = "snow1",
    high = "black"
  ) +
  scale_y_continuous(
    breaks = 0:10,
    labels = c(expression(t[0]),
               expression(t[1]),
               expression(t[2]),
               expression(t[3]),
               expression(t[4]),
               expression(t[5]),
               expression(t[6]),
               expression(t[7]),
               expression(t[8]),
               expression(t[9]),
               expression(t[10]))
  ) +
  geom_text(aes(label = n), color = "white", size = 6) +
  guides(
    fill = guide_colourbar(
      title = "NIS",
      barwidth = 1,
      barheight = 20
    )
  ) +
  labs(
    x = "MSFD Regions",
    y = "Introductions (Momentum)"
  ) +
  theme_light() +
  theme(
    axis.text = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#display
country_heatmap

# Generate and save heatmaps with east-to-west ordering
# ggsave("country_heatmap.jpg", 
#        country_heatmap,
#        width = 10, height = 6)
# 
# ggsave("msfd_heatmap.jpg", 
#        msfd_heatmap,
#        width = 8, height = 6)


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
  theme_ridges()+scale_x_continuous(limit=c(0,40))


#modified as in Vagenas 2024

# ECDF Plot
ecdf_plot <- ggplot(temporal_analysis, aes(x = delta_t, y = factor(t_step), fill = after_stat(ecdf))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "ECDF", option = "plasma") +
  scale_y_discrete(
    labels = c(expression(Delta*"t"["0-1"]),
               lapply(1:9, function(i) bquote(Delta*"t"[.(i)])))
  ) +
  labs(x = "Time Difference (Years)", y = "Introduction Step") +
  theme_ridges()+scale_x_continuous(limit=c(0,50))

ecdf_plot

# ECDF Plot with matching technical specs
ecdf_plot <- ggplot(temporal_analysis, aes(x = delta_t, y = factor(t_step), 
                                           fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantile_lines = TRUE,
    quantiles = 2,
    color = "red",
    linewidth = 0.7
  ) +
  scale_fill_viridis_c(
    name = "Probability",
    direction = -1,
    option = "F"
  ) +
  scale_x_continuous(
    breaks = seq(0, 50, 5),
    limits = c(0, 50)
  ) +
  scale_y_discrete(
    labels = c(expression(Delta*tau[1]-tau[0]),
               expression(Delta*tau[2]-tau[1]),
               expression(Delta*tau[3]-tau[2]),
               expression(Delta*tau[4]-tau[3]),
               expression(Delta*tau[5]-tau[4]),
               expression(Delta*tau[6]-tau[5]),
               expression(Delta*tau[7]-tau[6]),
               expression(Delta*tau[8]-tau[7]),
               expression(Delta*tau[9]-tau[8]),
               expression(Delta*tau[10]-tau[9]))
  ) +
  labs(
    x = "Years",
    y = "Delta (Δ) of successive momenta"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(vjust = 0.5) # Ensures proper vertical alignment
  )

# Display the plot
ecdf_plot

#ggsave("ecdf_analysis.jpg", ecdf_plot, width = 9, height = 7)

# Cumulative Spread Analysis
cumulative_spread_median <- temporal_analysis %>%
  group_by(t_step) %>%
  summarise(median_delta = median(delta_t, na.rm = TRUE)) %>%
  mutate(cumulative_time = cumsum(coalesce(median_delta, 0)))

zero<-c(0,0,0)

cumulative_spread_median<-rbind(zero,cumulative_spread_median)

ggplot(cumulative_spread_median, aes(x = t_step, y = cumulative_time)) +
  geom_line(color = "steelblue", size = 1.5) +
  geom_point(size = 3) +
  labs(title = "Cumulative Spread Timeline (Median)",
       x = "Introduction Step", 
       y = "Cumulative Years") +
  scale_x_continuous(limits = c(0, 10), breaks = 0:10) +
  scale_y_continuous(limits = c(0, 6), breaks = 0:round(max(cumulative_spread_median$cumulative_time))) +
  theme_bw()

cumulative_spread_mean <- temporal_analysis %>%
  group_by(t_step) %>%
  summarise(mean_delta = mean(delta_t, na.rm = TRUE)) %>%
  mutate(cumulative_time = cumsum(coalesce(mean_delta, 0)))
zero<-c(0,0,0)

cumulative_spread_mean<-rbind(zero,cumulative_spread_mean)

ggplot(cumulative_spread_mean, aes(x = t_step, y = cumulative_time)) +
  geom_line(color = "steelblue", size = 1.5) +
  geom_point(size = 3) +
  labs(title = "Cumulative Spread Timeline (Average)",
       x = "Introduction Step", 
       y = "Cumulative Years") +
  scale_x_continuous(limits = c(0, 10), breaks = 1:10) +
  scale_y_continuous(limits = c(0, NA), breaks = 0:round(max(cumulative_spread_mean$cumulative_time)))+
  theme_bw()


#ADD THE SPATIAL ASPECT

#Shahar's file

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

annual_c_int_dt <- readRDS("C:/Users/geo_v/Desktop/annual_c_int_dt.rds")

str(annual_c_int_dt)

#NEED TO BE SOLVED

# First, make sure your column names are consistent
# Your data shows "Longitude" but your code uses "long" - let's fix that

# Option 1: Rename columns to match your code
agg_data_clean <- agg_data %>%
  rename(lat = Latitude, long = Longitude)

str(agg_data_clean)

# Option 2: Or modify the code to use your actual column names
# Associate records with H3 hexagons (resolution 7 is commonly used, adjust as needed)
# Convert species data to H3 resolution 3 to match MHW data
# Convert species data to H3 resolution 3 to match MHW data
agg_data_with_hex <- agg_data %>%
  mutate(
    h3_id = h3jsr::point_to_cell(
      sf::st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326),
      res = 3  # Match the MHW data resolution
    ),
    # Create year column that matches year_of_records in MHW data
    year_for_join = first_area_sighting
  )

str(agg_data_with_hex)

# Join species data with MHW data
combined_data <- agg_data_with_hex %>%
  left_join(annual_c_int_dt, by = c("h3_id" = "h3_id", "year_for_join" = "year_of_mhws"))

str(combined_data)

# Calculate total records per hexagon per year with MHW data
records_mhw <- combined_data %>%
  group_by(h3_id, year_for_join, annual_c_int,ID,first_area_sighting) %>%
  summarise(
    total_records = n(),
    .groups = 'drop'
  )

str(records_mhw)

# Remove rows with NA MHW data
records_mhw_clean <- records_mhw %>% filter(!is.na(annual_c_int))

# Optional: Visualize the relationship

# Optional: Visualize the relationship
ggplot(records_mhw_clean, aes(x = annual_c_int, y = total_records)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "MHW Intensity (C-days)", y = "Number of Records", 
       title = "Relationship between MHW Intensity and Record Count") +
  theme_minimal()


ggplot(records_mhw_clean, aes(x = log(annual_c_int), y = log(total_records))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "MHW Intensity (C-days)", y = "Number of Records", 
       title = "Relationship between MHW Intensity and Record Count") +
  theme_minimal()


#are there are new hexagons added because of MHWs or is it random?

# 1. Calculate annual expansion metrics with MHW context
annual_expansion <- records_mhw_clean %>%
  arrange(year_for_join) %>%
  group_by(year_for_join) %>%
  summarise(
    # Spatial expansion metrics
    total_hexagons = n_distinct(h3_id),
    total_records = sum(total_records),  # Sampling effort proxy
    mean_mhw_intensity = mean(annual_c_int, na.rm = TRUE),
    max_mhw_intensity = max(annual_c_int, na.rm = TRUE),
    prop_hexagons_with_mhw = sum(annual_c_int > 0) / n(),
    mhw_occurred = any(annual_c_int > 0),
    .groups = 'drop'
  ) %>%
  # Calculate year-over-year changes
  mutate(
    hexagon_growth = total_hexagons - lag(total_hexagons),
    record_growth = total_records - lag(total_records),
    percent_hexagon_growth = (total_hexagons / lag(total_hexagons) - 1) * 100,
    expansion_rate = hexagon_growth / lag(total_hexagons) * 100
  )

# 2. Identify which years had significant MHW events
mhw_threshold <- quantile(annual_expansion$mean_mhw_intensity, 0.75, na.rm = TRUE)  # Top 25% as "significant" MHW
mhw_threshold_low<-quantile(annual_expansion$mean_mhw_intensity, 0.25, na.rm = TRUE) 
annual_expansion <- annual_expansion %>%
  mutate(
    significant_mhw = mean_mhw_intensity > mhw_threshold,
    mhw_category = case_when(
      mean_mhw_intensity == 0 ~ "No MHW",
      mean_mhw_intensity > mhw_threshold_low & mean_mhw_intensity <= mhw_threshold ~ "Medium MHW",
      mean_mhw_intensity > 0 & mean_mhw_intensity <= mhw_threshold_low ~ "Low MHW",
      mean_mhw_intensity > mhw_threshold ~ "High MHW"
    )
  )

print("Annual expansion with MHW context:")
print(annual_expansion)


#Check whether the ratio between hexagons every year and records is changing across time


# Calculate hexagon-to-records ratio
annual_expansion <- annual_expansion %>%
  mutate(
    hexagon_to_record_ratio = total_hexagons / total_records
  )

print("Annual expansion with hexagon-to-record ratio:")
print(annual_expansion %>% select(year_for_join, total_hexagons, total_records, hexagon_to_record_ratio, mean_mhw_intensity))

# Plot the ratio over time
ggplot(annual_expansion, aes(x = year_for_join, y = hexagon_to_record_ratio)) +
  geom_line(color = "blue", size = 1) +
  geom_point(aes(color = mhw_category, size = mean_mhw_intensity), alpha = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "red", alpha = 0.3) +
  scale_color_manual(values = c("No MHW" = "blue", "Low MHW" = "orange","Medium MHW"="purple", "High MHW" = "red")) +
  scale_size_continuous(name = "MHW Intensity") +
  labs(
    x = "Year",
    y = "Hexagon-to-Record Ratio",
    title = "Spatial Efficiency of Sampling Over Time",
    subtitle = "Higher ratio = more hexagons covered per record (better spatial coverage)",
    color = "MHW Category"
  ) +
  theme_minimal()

# Plot with MHW intensity overlay
ggplot(annual_expansion, aes(x = year_for_join)) +
  geom_col(aes(y = hexagon_to_record_ratio, fill = mhw_category), alpha = 0.7) +
  geom_line(aes(y = mean_mhw_intensity / max(mean_mhw_intensity, na.rm = TRUE) * max(hexagon_to_record_ratio, na.rm = TRUE)), 
            color = "red", size = 1) +
  geom_point(aes(y = mean_mhw_intensity / max(mean_mhw_intensity, na.rm = TRUE) * max(hexagon_to_record_ratio, na.rm = TRUE)), 
             color = "darkred", size = 1) +
  scale_y_continuous(
    name = "Hexagon-to-Record Ratio",
    sec.axis = sec_axis(~ . / max(annual_expansion$hexagon_to_record_ratio, na.rm = TRUE) * max(annual_expansion$mean_mhw_intensity, na.rm = TRUE), 
                        name = "MHW Intensity (°C-days)")
  ) +
  scale_fill_manual(values = c("No MHW" = "blue", "Low MHW" = "orange","Medium MHW"="purple", "High MHW" = "red")) +
  labs(
    x = "Year",
    title = "Spatial Coverage Efficiency vs Marine Heatwaves",
    subtitle = "Bars: spatial efficiency | Red line: MHW intensity",
    fill = "MHW Category"
  ) +
  theme_minimal()




# 3. Compare expansion in MHW vs non-MHW years
expansion_comparison <- annual_expansion %>%
  filter(!is.na(hexagon_growth)) %>%  # Remove first year with no growth data
  group_by(mhw_category) %>%
  summarise(
    n_years = n(),
    mean_hexagon_growth = mean(hexagon_growth, na.rm = TRUE),
    mean_record_growth = mean(record_growth, na.rm = TRUE),
    mean_expansion_rate = mean(expansion_rate, na.rm = TRUE),
    median_hexagon_growth = median(hexagon_growth, na.rm = TRUE),
    .groups = 'drop'
  )

print("Expansion comparison by MHW category:")
print(expansion_comparison)


# Check if the High MHW vs Others difference is statistically significant
high_mhw_growth <- annual_expansion %>% filter(mhw_category == "High MHW" & !is.na(hexagon_growth)) %>% pull(hexagon_growth)
other_growth <- annual_expansion %>% filter(mhw_category != "High MHW" & !is.na(hexagon_growth)) %>% pull(hexagon_growth)

t_test_high_vs_other <- t.test(high_mhw_growth, other_growth)
cat("High MHW vs Other years - P-value:", t_test_high_vs_other$p.value, "\n")


# 4. Statistical tests
# Test if hexagon growth differs between MHW and non-MHW years
mhw_years <- annual_expansion %>% filter(significant_mhw == TRUE & !is.na(hexagon_growth))
non_mhw_years <- annual_expansion %>% filter(significant_mhw == FALSE & !is.na(hexagon_growth))

t_test_result <- t.test(mhw_years$hexagon_growth, non_mhw_years$hexagon_growth)
cat("\nT-test for hexagon growth (MHW vs non-MHW years):\n")
cat("P-value:", t_test_result$p.value, "\n")
cat("Mean growth in MHW years:", mean(mhw_years$hexagon_growth, na.rm = TRUE), "\n")
cat("Mean growth in non-MHW years:", mean(non_mhw_years$hexagon_growth, na.rm = TRUE), "\n")

# 5. Correlation analysis
correlation_analysis <- annual_expansion %>%
  filter(!is.na(hexagon_growth))

cor_hexagon_mhw <- cor(correlation_analysis$hexagon_growth, 
                       correlation_analysis$mean_mhw_intensity, 
                       use = "complete.obs")

cor_records_mhw <- cor(correlation_analysis$record_growth, 
                       correlation_analysis$mean_mhw_intensity, 
                       use = "complete.obs")

cat("\nCorrelation analysis:\n")
cat("Hexagon growth vs MHW intensity:", cor_hexagon_mhw, "\n")
cat("Record growth vs MHW intensity:", cor_records_mhw, "\n")

# 6. Visualize the relationship
# Plot 1: Hexagon growth vs MHW intensity
p1 <- ggplot(correlation_analysis, aes(x = mean_mhw_intensity, y = hexagon_growth)) +
  geom_point(aes(size = total_records, color = mhw_category), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_color_manual(values = c("No MHW" = "blue", "Low MHW" = "orange","Medium MHW"="purple", "High MHW" = "red")) +
  labs(
    x = "Mean MHW Intensity (°C-days)",
    y = "New Hexagons Added",
    title = "Spatial Expansion vs Marine Heatwave Intensity",
    subtitle = paste("Correlation:", round(cor_hexagon_mhw, 3)),
    color = "MHW Category",
    size = "Total Records"
  ) +
  theme_minimal()

print(p1)

# Plot 2: Time series of expansion and MHW
p2 <- ggplot(annual_expansion, aes(x = year_for_join)) +
  geom_col(aes(y = hexagon_growth, fill = mhw_category), alpha = 0.7) +
  geom_line(aes(y = mean_mhw_intensity * 2), color = "red", size = 1) +  # Scaled for visibility
  geom_point(aes(y = mean_mhw_intensity * 2), color = "darkred", size = 1) +
  scale_y_continuous(
    name = "New Hexagons Added",
    sec.axis = sec_axis(~ . / 2, name = "MHW Intensity (°C-days)")
  ) +
  scale_fill_manual(values = c("No MHW" = "blue", "Low MHW" = "orange","Medium MHW"="purple", "High MHW" = "red")) +
  labs(
    x = "Year",
    title = "Temporal Pattern: Spatial Expansion and Marine Heatwaves",
    subtitle = "Bars: new hexagons | Red line: MHW intensity",
    fill = "MHW Category"
  ) +
  theme_minimal()

print(p2)

# 7. Multiple regression to separate sampling effort from MHW effect
model <- lm(hexagon_growth ~ record_growth + mean_mhw_intensity, 
            data = correlation_analysis)

cat("\nMultiple regression: Hexagon growth ~ Record growth + MHW intensity\n")
print(summary(model))

# 8. Analyze if MHW drives expansion beyond sampling effort
# Calculate "excess expansion" - growth beyond what sampling would predict
sampling_model <- lm(hexagon_growth ~ record_growth, data = correlation_analysis)
correlation_analysis$predicted_from_sampling <- predict(sampling_model)
correlation_analysis$excess_expansion <- correlation_analysis$hexagon_growth - correlation_analysis$predicted_from_sampling

# Test if excess expansion correlates with MHW
cor_excess_mhw <- cor(correlation_analysis$excess_expansion, 
                      correlation_analysis$mean_mhw_intensity, 
                      use = "complete.obs")

cat("\nExcess expansion analysis:\n")
cat("Correlation between excess expansion and MHW:", cor_excess_mhw, "\n")

# Plot excess expansion vs MHW
p3 <- ggplot(correlation_analysis, aes(x = mean_mhw_intensity, y = excess_expansion)) +
  geom_point(aes(color = mhw_category), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  scale_color_manual(values = c("No MHW" = "blue", "Low MHW" = "orange", "High MHW" = "red")) +
  labs(
    x = "MHW Intensity (°C-days)",
    y = "Excess Expansion (beyond sampling prediction)",
    title = "MHW Effect Beyond Sampling Effort",
    subtitle = paste("Positive values = more expansion than expected from sampling alone\nCorrelation:", round(cor_excess_mhw, 3)),
    color = "MHW Category"
  ) +
  theme_minimal()

print(p3)


### end ###

#remove the 10th percentile of extremes

records_mhw_filtered <- records_mhw_clean #%>%
  # filter(
  #   annual_c_int <= quantile(annual_c_int, 0.9, na.rm = TRUE),
  #   total_records <= quantile(total_records, 0.9, na.rm = TRUE)
  # )

ggplot(records_mhw_filtered, aes(x = log(annual_c_int), y = log(total_records))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "MHW Intensity (C-days)", y = "Number of Records", 
       title = "Relationship between MHW Intensity and Record Count (90th percentile)") +
  theme_minimal()


#Visualization
# Step 1: Aggregate data (same as before)
hexagon_summary <- records_mhw_clean %>%
  group_by(h3_id) %>%
  summarise(
    mean_records = mean(total_records, na.rm = TRUE),
    mean_mhw = mean(annual_c_int, na.rm = TRUE),
    n_years = n_distinct(year_for_join),
    .groups = 'drop'
  )

# Step 2: Create polygons as a separate dataframe with h3_id
hex_polygons_df <- data.frame(
  h3_id = hexagon_summary$h3_id,
  geometry = h3jsr::cell_to_polygon(hexagon_summary$h3_id, simple = FALSE)
)

# Step 3: Join and convert to SF
hexagons_sf <- hex_polygons_df %>%
  left_join(hexagon_summary, by = "h3_id") %>%
  st_as_sf()

# Plot by record density
ggplot(hexagons_sf) +
  geom_sf(aes(fill = mean_records), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    name = "Mean Records",
    trans = "log10",
    labels = scales::comma
  ) +
  labs(
    title = "Spatial Distribution of Biodiversity Records",
    subtitle = "Mean records per hexagon across all years"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank())

# Plot by MHW intensity
ggplot(hexagons_sf) +
  geom_sf(aes(fill = mean_mhw), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    name = "Mean MHW Intensity\n(°C-days)",
    option = "plasma"
  ) +
  labs(
    title = "Spatial Distribution of Marine Heatwave Intensity",
    subtitle = "Mean annual cumulative intensity per hexagon"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank())



#

# Install packages if needed
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")

library(rnaturalearth)
library(rnaturalearthdata)

# Get Natural Earth background map (land and ocean)
world <- ne_countries(scale = "medium", returnclass = "sf")
ocean <- ne_download(scale = "medium", type = 'ocean', category = 'physical', returnclass = "sf")

# Get the bounding box of your hexagons
hex_bbox <- st_bbox(hexagons_sf)

# Plot with background cropped to hexagon extent
ggplot() +
  # Ocean background (light grey) - crop to hexagon extent
  geom_sf(data = ocean, fill = "grey90", color = NA) +
  # Land (darker grey) - crop to hexagon extent
  geom_sf(data = world, fill = "grey80",size = 0.2) +
  # Your hexagons with transparency
  geom_sf(data = hexagons_sf, aes(fill = mean_records), 
          color = "white", size = 0.1, alpha = 0.4) +
  scale_fill_viridis_c(
    name = "Mean Records",
    trans = "log10",
    labels = scales::comma
  ) +
  labs(
    title = "Spatial Distribution of Biodiversity Records",
    subtitle = "Mean records per hexagon across all years"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "lightblue", color = NA)
  ) +
  # Set the plot limits to match hexagon bounding box
  coord_sf(
    xlim = c(hex_bbox$xmin, hex_bbox$xmax),
    ylim = c(hex_bbox$ymin, hex_bbox$ymax)
  )



# Plot with background cropped to hexagon extent
ggplot() +
  # Ocean background (light grey) - crop to hexagon extent
  geom_sf(data = ocean, fill = "grey99", color = NA) +
  # Land (darker grey) - crop to hexagon extent
  geom_sf(data = world, fill = "grey90", size = 0.2) +
  # Your hexagons with orange-red palette
  geom_sf(data = hexagons_sf, aes(fill = mean_records), 
          color = "white", size = 0.1, alpha = 0.6) +
  scale_fill_gradientn(
    name = "Mean Records",
    colors = c("#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B", "#FD8D3C", "#F16913", "#D94801", "#A63603", "#7F2704"),
    trans = "log10",
    labels = scales::comma
  ) +
  labs(
    title = "Spatial Distribution of Biodiversity Records",
    subtitle = "Mean NIS records per hexagon across all years"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "lightblue", color = NA)
  ) +
  # Set the plot limits to match hexagon bounding box
  coord_sf(
    xlim = c(hex_bbox$xmin, hex_bbox$xmax),
    ylim = c(hex_bbox$ymin, hex_bbox$ymax)
  )


#Lets work on the momentum a bit






# Data Aggregation (First Sighting per Species-Hexagon) with proper ordering
agg_data_hex <- combined_data %>%
  group_by(species, h3_id, Latitude, Longitude) %>%
  summarise(
    first_area_sighting = min(first_area_sighting, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(first_area_sighting) %>%  # Arrange by year first
  mutate(ID = as.numeric(factor(species))) %>%  # Create species ID
  arrange(species, first_area_sighting)  # Then arrange by species and year

# Verify the results
head(agg_data_hex, 20)

# Momentum Calculation Function (same as before)
calculate_momentum <- function(data) {
  data <- data %>%
    arrange(ID, first_area_sighting, Latitude, Longitude) %>%
    group_by(ID) %>%
    mutate(
      t_step = row_number() - 1,  # t0, t1, t2...
      t_flag = ifelse(t_step < 11, 1, 0)  # Limit to t0-t10
    ) %>%
    filter(t_flag == 1) %>%
    ungroup()
  return(data)
}

# Apply Momentum Calculation
momentum_data_hex <- calculate_momentum(agg_data_hex)

# Create hexagon ordering (east to west based on longitude)
hexagon_order <- momentum_data_hex %>%
  group_by(h3_id) %>%
  summarise(mean_lon = mean(Longitude, na.rm = TRUE)) %>%
  arrange(mean_lon) %>%
  pull(h3_id)

# Generate hexagon heatmap
generate_hexagon_heatmap <- function(data) {
  heat_data <- data %>%
    count(h3_id, t_step) %>%
    complete(h3_id, t_step = 0:10, fill = list(n = 0)) %>%
    filter(!is.na(h3_id)) %>%
    mutate(h3_id = factor(h3_id, levels = hexagon_order))
  
  ggplot(heat_data, aes(x = h3_id, y = t_step, fill = n)) +
    geom_tile(color = "white", lwd = 0.5) +
    scale_fill_viridis_c(option = "inferno") +
    scale_y_continuous(
      breaks = 0:10,
      labels = c("t₀", paste0("t", 1:10))
    ) +
    labs(
      x = "Hexagons (East to West)",
      y = "Introduction Momentum",
      fill = "Count"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
      axis.text.y = element_text(size = 10)
    )
}

# Generate hexagon heatmap
hexagon_heatmap <- generate_hexagon_heatmap(momentum_data_hex)

# Display the plot
hexagon_heatmap

# If you have too many hexagons, you might want to filter for the most active ones:
# Filter for hexagons with at least X records
active_hexagons <- momentum_data_hex %>%
  count(h3_id) %>%
  filter(n >= 0) %>%  # Adjust threshold as needed
  pull(h3_id)

momentum_data_hex_filtered <- momentum_data_hex %>%
  filter(h3_id %in% active_hexagons)

# Regenerate heatmap with filtered data
hexagon_heatmap_filtered <- generate_hexagon_heatmap(momentum_data_hex_filtered)
hexagon_heatmap_filtered

active_hexagons


#provide the spatial representation

# 1. Get hexagons with t_step = 0 (first introductions)
hexagons_t0 <- momentum_data_hex_filtered %>%
  filter(t_step == 0) %>%
  group_by(h3_id) %>%
  summarise(
    n_first_introductions = n(),
    species_list = paste(unique(species), collapse = ", "),
    mean_lat = mean(Latitude, na.rm = TRUE),
    mean_lon = mean(Longitude, na.rm = TRUE),
    .groups = 'drop'
  )

# 2. Create density-based probability using ECDF
# Count how many t_step = 0 records each hexagon has
t0_counts <- hexagons_t0$n_first_introductions

# Create ECDF of t_step = 0 counts
ecdf_t0 <- ecdf(t0_counts)

# Calculate probability for each hexagon based on its count density
hexagons_t0$probability <- ecdf_t0(hexagons_t0$n_first_introductions)

# 3. Convert to spatial polygons
hexagons_t0_sf <- hexagons_t0 %>%
  mutate(geometry = h3jsr::cell_to_polygon(h3_id)) %>%
  st_as_sf()

# Calculate centroid for label placement
hexagons_t0_sf$centroid <- st_centroid(hexagons_t0_sf$geometry)

# 4. Display hexagons with t_step = 0 and their probabilities
print("Hexagons with t_step = 0 and their probabilities:")
print(hexagons_t0_sf %>% st_drop_geometry() %>% arrange(desc(probability)))

# 5. Visualize only hexagons with t_step = 0
ggplot() +
  geom_sf(data = ocean, fill = "grey95", color = NA) +
  geom_sf(data = world, fill = "grey85", color = "grey70", size = 0.1) +
  geom_sf(data = hexagons_t0_sf, aes(fill = "First Introductions"), 
          color = "white", size = 0.2, alpha = 0.8) +
  scale_fill_manual(values = c("First Introductions" = "red"), 
                    name = "Introduction Type") +
  labs(
    title = "Hexagons with First Species Introductions (t_step = 0)",
    subtitle = "Locations where species were first recorded"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  coord_sf(
    xlim = c(hex_bbox$xmin, hex_bbox$xmax),
    ylim = c(hex_bbox$ymin, hex_bbox$ymax)
  )

# 6. Visualize with ECDF probability coloring and number of introductions
ggplot() +
  geom_sf(data = ocean, fill = "grey95", color = NA) +
  geom_sf(data = world, fill = "grey85", color = "grey70", size = 0.1) +
  geom_sf(data = hexagons_t0_sf, aes(fill = probability), 
          color = "white", size = 0.2, alpha = 0.8) +
  # Add text labels with number of introductions
  geom_sf_text(data = hexagons_t0_sf, 
               aes(geometry = centroid, label = n_first_introductions),
               color = "white", 
               size = 3,
               fontface = "bold",alpha=0.5) +
  scale_fill_viridis_c(
    name = "ECDF\nProbability",
    option = "plasma",
    limits = c(0, 1),
    labels = scales::percent
  ) +
  labs(
    title = "ECDF-Based Probability of First Introduction Density",
    subtitle = "Higher probability = more first introductions in hexagon | Numbers show count of introductions"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  coord_sf(
    xlim = c(hex_bbox$xmin, hex_bbox$xmax),
    ylim = c(hex_bbox$ymin, hex_bbox$ymax)
  )



### run a loop for all ###








# Create figures for all t_steps from 0 to 10
for (t_step_value in 0:10) {
  
  # 1. Get hexagons for current t_step
  hexagons_t <- momentum_data_hex_filtered %>%
    filter(t_step == t_step_value) %>%
    group_by(h3_id) %>%
    summarise(
      n_introductions = n(),
      species_list = paste(unique(species), collapse = ", "),
      mean_lat = mean(Latitude, na.rm = TRUE),
      mean_lon = mean(Longitude, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Skip if no data for this t_step
  if (nrow(hexagons_t) == 0) {
    cat("No data for t_step =", t_step_value, "\n")
    next
  }
  
  # 2. Create density-based probability using ECDF
  t_counts <- hexagons_t$n_introductions
  ecdf_t <- ecdf(t_counts)
  hexagons_t$probability <- ecdf_t(hexagons_t$n_introductions)
  
  # 3. Convert to spatial polygons
  hexagons_t_sf <- hexagons_t %>%
    mutate(geometry = h3jsr::cell_to_polygon(h3_id)) %>%
    st_as_sf()
  
  # Calculate centroid for label placement
  hexagons_t_sf$centroid <- st_centroid(hexagons_t_sf$geometry)
  
  # 4. Display summary for current t_step
  cat("\n=== t_step =", t_step_value, "===\n")
  cat("Number of hexagons:", nrow(hexagons_t), "\n")
  cat("Total introductions:", sum(hexagons_t$n_introductions), "\n")
  cat("Mean introductions per hexagon:", mean(hexagons_t$n_introductions), "\n")
  
  # 5. Create and save the plot
  p <- ggplot() +
    geom_sf(data = ocean, fill = "grey95", color = NA) +
    geom_sf(data = world, fill = "grey85", color = "grey70", size = 0.1) +
    geom_sf(data = hexagons_t_sf, aes(fill = probability), 
            color = "white", size = 0.2, alpha = 0.8) +
    # Add text labels with number of introductions
    geom_sf_text(data = hexagons_t_sf, 
                 aes(geometry = centroid, label = n_introductions),
                 color = "white", 
                 size = 3,
                 fontface = "bold", alpha = 0.5) +
    scale_fill_viridis_c(
      name = "ECDF\nProbability",
      option = "plasma",
      limits = c(0, 1),
      labels = scales::percent
    ) +
    labs(
      title = paste("ECDF-Based Probability of t_step =", t_step_value, "Introduction Density"),
      subtitle = paste("Higher probability = more introductions in hexagon | Numbers show count of introductions")
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10)
    ) +
    coord_sf(
      xlim = c(hex_bbox$xmin, hex_bbox$xmax),
      ylim = c(hex_bbox$ymin, hex_bbox$ymax)
    )
  
  # Display the plot
  print(p)
  
  # Save the plot
  ggsave(filename = paste0("t_step_", t_step_value, "_probability_map.png"),
         plot = p,
         width = 10,
         height = 8,
         dpi = 300)
  
  # Print top hexagons for this t_step
  cat("Top hexagons for t_step", t_step_value, ":\n")
  print(hexagons_t_sf %>% 
          st_drop_geometry() %>% 
          arrange(desc(probability)) %>%
          head(5) %>%
          select(h3_id, n_introductions, probability))
}

# Additional: Create a summary table of all t_steps
t_step_summary <- momentum_data_hex_filtered %>%
  group_by(t_step) %>%
  summarise(
    n_hexagons = n_distinct(h3_id),
    n_introductions = n(),
    n_species = n_distinct(species),
    .groups = 'drop'
  )

print("Summary across all t_steps:")
print(t_step_summary)

# Create a summary plot showing trends across t_steps
summary_plot <- ggplot(t_step_summary, aes(x = t_step, y = n_introductions)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_text(aes(label = n_introductions), vjust = -0.5, size = 3) +
  labs(
    title = "Number of Introductions Across Time Steps",
    x = "Time Step (t)",
    y = "Number of Introductions"
  ) +
  theme_minimal()

print(summary_plot)

# Save summary plot
#ggsave("t_step_summary_trend.png", summary_plot, width = 10, height = 6, dpi = 300)








###############


#find sequences in the polygons#









# Find sequential hexagon movements for each species







# Load required libraries
library(tidyverse)
library(sf)
library(h3jsr)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)

# Get background maps
world <- ne_countries(scale = "medium", returnclass = "sf")
ocean <- ne_download(scale = "medium", type = 'ocean', category = 'physical', returnclass = "sf")

# 1. Find sequential hexagon movements for each species
species_movements <- momentum_data_hex_filtered %>%
  arrange(species, t_step) %>%
  group_by(species, ID) %>%
  mutate(
    # Create pairs of consecutive hexagons
    from_hex = lag(h3_id),
    to_hex = h3_id,
    from_t_step = lag(t_step),
    to_t_step = t_step,
    from_year = lag(first_area_sighting),
    to_year = first_area_sighting
  ) %>%
  filter(!is.na(from_hex)) %>%  # Remove first observation (no previous hex)
  ungroup() %>%
  select(species, ID, from_hex, to_hex, from_t_step, to_t_step, 
         from_year, to_year)

# 2. Analyze movement patterns across all species
movement_patterns <- species_movements %>%
  group_by(from_hex, to_hex) %>%
  summarise(
    n_species = n_distinct(species),  # How many species use this route
    species_list = paste(unique(species), collapse = ", "),
    from_t_step = first(from_t_step),
    to_t_step = first(to_t_step),
    .groups = 'drop'
  ) %>%
  arrange(desc(n_species))

# 3. Identify hub hexagons (most outgoing connections)
hub_hexagons <- movement_patterns %>%
  group_by(from_hex) %>%
  summarise(
    out_degree = n(),                    # Number of unique outgoing connections
    total_species_out = sum(n_species),  # Total species using these routes
    avg_t_step = mean(from_t_step),      # Average time step of departures
    .groups = 'drop'
  ) %>%
  arrange(desc(out_degree))

print("Top hub hexagons (most outgoing connections):")
print(hub_hexagons)

# 4. Convert to spatial polygons for mapping
hub_sf <- hub_hexagons %>%
  mutate(geometry = h3jsr::cell_to_polygon(from_hex)) %>%
  st_as_sf()

# Get bounding box for focused mapping
hex_bbox <- st_bbox(hub_sf)

# 5. Visualize hub hexagons
hub_plot <- ggplot() +
  # Background maps
  geom_sf(data = ocean, fill = "grey95", color = NA) +
  geom_sf(data = world, fill = "grey85", color = "grey70", size = 0.1) +
  # Hub hexagons colored by outgoing connections
  geom_sf(data = hub_sf, aes(fill = out_degree), 
          color = "white", size = 0.2, alpha = 0.8) +
  # Labels for top hubs (top 20%)
  geom_sf_text(data = hub_sf %>% filter(out_degree >= quantile(out_degree, 0.8)),
               aes(label = out_degree),
               color = "white", size = 3, fontface = "bold") +
  scale_fill_viridis_c(
    name = "Outgoing\nConnections",
    option = "plasma"
  ) +
  labs(
    title = "Hub Hexagons in Species Movement Network",
    subtitle = "Higher values indicate more outgoing connections to other hexagons",
    caption = "Numbers show count of unique outgoing connections"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  coord_sf(
    xlim = c(hex_bbox$xmin, hex_bbox$xmax),
    ylim = c(hex_bbox$ymin, hex_bbox$ymax)
  )

print(hub_plot)

# 6. Detailed analysis of top hubs
top_hubs <- hub_hexagons %>%
  head(10)  # Top 10 hubs

print("Detailed analysis of top 10 hub hexagons:")
for(i in 1:nrow(top_hubs)) {
  hub <- top_hubs[i, ]
  cat("\n--- Hub", i, ":", hub$from_hex, "---\n")
  cat("Outgoing connections:", hub$out_degree, "\n")
  cat("Total species using routes:", hub$total_species_out, "\n")
  
  # Get destinations from this hub
  destinations <- movement_patterns %>%
    filter(from_hex == hub$from_hex) %>%
    arrange(desc(n_species))
  
  cat("Top destinations:\n")
  print(destinations %>% head(3) %>% select(to_hex, n_species))
}

# 7. Save the results
#write_csv(hub_hexagons, "hub_hexagons_analysis.csv")
#ggsave("hub_hexagons_map.png", hub_plot, width = 10, height = 8, dpi = 300)



######### Check if the outgoing connection is related to a marine heatwave


# 1. Join MHW data with hub hexagons
hub_mhw_analysis <- hub_hexagons %>%
  # Join with annual MHW data to get MHW intensity for each hub
  left_join(annual_c_int_dt, by = c("from_hex" = "h3_id")) %>%
  # For each hub, calculate MHW statistics
  group_by(from_hex, out_degree, total_species_out) %>%
  summarise(
    # MHW intensity metrics
    mean_mhw_intensity = mean(annual_c_int, na.rm = TRUE),
    max_mhw_intensity = max(annual_c_int, na.rm = TRUE),
    mhw_frequency = sum(annual_c_int > 0, na.rm = TRUE) / n(),  # Proportion of years with MHW
    total_mhw_days = sum(annual_c_int, na.rm = TRUE),  # Cumulative MHW intensity
    n_years_mhw = n_distinct(year_of_mhws),
    .groups = 'drop'
  ) %>%
  arrange(desc(out_degree))

print("Hub hexagons with MHW analysis:")
print(hub_mhw_analysis)

# 2. Statistical correlation between outgoing connections and MHW metrics
correlation_analysis <- hub_mhw_analysis %>%
  select(out_degree, total_species_out, mean_mhw_intensity, max_mhw_intensity, mhw_frequency, total_mhw_days)

correlation_matrix <- cor(correlation_analysis, use = "complete.obs")
print("Correlation matrix between hub connectivity and MHW metrics:")
print(correlation_matrix)

# 3. Visualize relationship between outgoing connections and MHW intensity
ggplot(hub_mhw_analysis, aes(x = mean_mhw_intensity, y = out_degree)) +
  geom_point(aes(size = total_species_out, color = mhw_frequency), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_color_viridis_c(name = "MHW Frequency") +
  scale_size_continuous(name = "Total Species") +
  labs(
    x = "Mean MHW Intensity (°C-days)",
    y = "Outgoing Connections",
    title = "Relationship Between Hub Connectivity and Marine Heatwave Intensity",
    subtitle = "Do hexagons with higher MHW intensity have more outgoing connections?"
  ) +
  theme_minimal()

# 4. Compare MHW conditions before and during species movements
movement_mhw_analysis <- species_movements %>%
  # Get MHW data for source hexagon in the year before movement
  left_join(annual_c_int_dt, by = c("from_hex" = "h3_id", "from_year" = "year_of_records")) %>%
  rename(mhw_before_movement = annual_c_int) %>%
  # Get MHW data for destination hexagon in the movement year
  left_join(annual_c_int_dt, by = c("to_hex" = "h3_id", "to_year" = "year_of_records")) %>%
  rename(mhw_at_destination = annual_c_int) %>%
  # Calculate MHW difference
  mutate(mhw_difference = mhw_at_destination - mhw_before_movement)

print("MHW conditions during species movements:")
print(summary(movement_mhw_analysis %>% select(mhw_before_movement, mhw_at_destination, mhw_difference)))

# 5. Analyze if movements are towards higher/lower MHW areas
mhw_movement_direction <- movement_mhw_analysis %>%
  mutate(
    movement_to_higher_mhw = mhw_difference > 0,
    movement_to_lower_mhw = mhw_difference < 0,
    movement_to_similar_mhw = mhw_difference == 0
  ) %>%
  summarise(
    n_movements = n(),
    prop_to_higher_mhw = sum(movement_to_higher_mhw) / n(),
    prop_to_lower_mhw = sum(movement_to_lower_mhw) / n(),
    prop_to_similar_mhw = sum(movement_to_similar_mhw) / n()
  )

print("Movement direction relative to MHW intensity:")
print(mhw_movement_direction)

# 6. Hub-specific MHW analysis
hub_movement_mhw <- movement_mhw_analysis %>%
  group_by(from_hex) %>%
  summarise(
    n_movements = n(),
    avg_mhw_before = mean(mhw_before_movement, na.rm = TRUE),
    avg_mhw_at_dest = mean(mhw_at_destination, na.rm = TRUE),
    avg_mhw_difference = mean(mhw_difference, na.rm = TRUE),
    prop_to_higher_mhw = sum(mhw_difference > 0, na.rm = TRUE) / n(),
    .groups = 'drop'
  ) %>%
  left_join(hub_hexagons, by = "from_hex") %>%
  arrange(desc(out_degree))

print("Hub-specific MHW movement patterns:")
print(hub_movement_mhw)

# 7. Visualize hubs with MHW context
hub_sf_mhw <- hub_sf %>%
  left_join(hub_mhw_analysis, by = "from_hex")

ggplot() +
  geom_sf(data = ocean, fill = "grey95", color = NA) +
  geom_sf(data = world, fill = "grey85", color = "grey70", size = 0.1) +
  geom_sf(data = hub_sf_mhw, aes(fill = mean_mhw_intensity), 
          color = "white", size = 0.2, alpha = 0.8) +
  geom_sf_text(data = hub_sf_mhw %>% filter(out_degree >= quantile(out_degree, 0.8)),
               aes(label = paste(out_degree, "\n", round(mean_mhw_intensity, 1))),
               color = "white", size = 2.5, fontface = "bold") +
  scale_fill_viridis_c(
    name = "Mean MHW\nIntensity\n(°C-days)",
    option = "plasma"
  ) +
  labs(
    title = "Hub Hexagons: Connectivity and Marine Heatwave Intensity",
    subtitle = "Numbers show: outgoing connections | mean MHW intensity",
    caption = "Higher MHW intensity may influence species dispersal patterns"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  coord_sf(
    xlim = c(hex_bbox$xmin, hex_bbox$xmax),
    ylim = c(hex_bbox$ymin, hex_bbox$ymax)
  )

# 8. Statistical tests
# Test if hubs with more connections have different MHW characteristics
high_connectivity_hubs <- hub_mhw_analysis %>%
  filter(out_degree >= median(out_degree))

low_connectivity_hubs <- hub_mhw_analysis %>%
  filter(out_degree < median(out_degree))

# T-test for MHW intensity difference
t_test_result <- t.test(high_connectivity_hubs$mean_mhw_intensity, 
                        low_connectivity_hubs$mean_mhw_intensity)

cat("\nStatistical test for MHW intensity difference:\n")
cat("High connectivity hubs (n =", nrow(high_connectivity_hubs), "):", 
    mean(high_connectivity_hubs$mean_mhw_intensity, na.rm = TRUE), "°C-days\n")
cat("Low connectivity hubs (n =", nrow(low_connectivity_hubs), "):", 
    mean(low_connectivity_hubs$mean_mhw_intensity, na.rm = TRUE), "°C-days\n")
cat("T-test p-value:", t_test_result$p.value, "\n")

# 9. Temporal analysis: MHW trends in hub hexagons
hub_mhw_temporal <- annual_c_int_dt %>%
  filter(h3_id %in% hub_hexagons$from_hex) %>%
  group_by(year_of_mhws) %>%
  summarise(
    mean_mhw_hubs = mean(annual_c_int, na.rm = TRUE),
    n_hubs_with_mhw = sum(annual_c_int > 0),
    .groups = 'drop'
  )

ggplot(hub_mhw_temporal, aes(x = year_of_mhws, y = mean_mhw_hubs)) +
  geom_line(color = "red") +
  geom_point(aes(size = n_hubs_with_mhw), color = "darkred", alpha = 0.6) +
  labs(
    x = "Year",
    y = "Mean MHW Intensity in Hub Hexagons (°C-days)",
    title = "Temporal Trends of Marine Heatwaves in Hub Locations",
    subtitle = "Size of points indicates number of hubs experiencing MHW each year"
  ) +
  theme_minimal()

# 10. Save MHW-hub analysis results
#write_csv(hub_mhw_analysis, "hub_mhw_correlation_analysis.csv")
#write_csv(movement_mhw_analysis, "species_movements_mhw_analysis.csv")



###


#break down the hubs annually



# 1. Create annual movement patterns
annual_movements <- species_movements %>%
  group_by(from_hex, to_hex, from_year) %>%
  summarise(
    n_species = n_distinct(species),
    species_list = paste(unique(species), collapse = ", "),
    from_t_step = first(from_t_step),
    .groups = 'drop'
  )

# 2. Calculate annual hub connectivity
annual_hub_hexagons <- annual_movements %>%
  group_by(from_hex, from_year) %>%
  summarise(
    annual_out_degree = n(),                    # Outgoing connections that year
    annual_species_out = sum(n_species),        # Total species moving that year
    .groups = 'drop'
  ) %>%
  arrange(from_year, desc(annual_out_degree))

print("Annual hub connectivity (first 20 rows):")
print(annual_hub_hexagons %>% head(20))

# 3. Join with annual MHW data
annual_hub_mhw <- annual_hub_hexagons %>%
  left_join(annual_c_int_dt, by = c("from_hex" = "h3_id", "from_year" = "year_of_records")) %>%
  rename(annual_mhw_intensity = annual_c_int) %>%
  # Add whether MHW occurred that year
  mutate(mhw_occurred = annual_mhw_intensity > 0)

print("Annual hub data with MHW information:")
print(annual_hub_mhw %>% head(20))


#filter after 1987
annual_hub_mhw<-filter(annual_hub_mhw,annual_hub_mhw$from_year>1989)


# 4. Analyze top hubs year by year
top_annual_hubs <- annual_hub_mhw %>%
  group_by(from_year) %>%
  slice_max(order_by = annual_out_degree, n = 5) %>%  # Top 5 hubs each year
  ungroup()

print("Top 5 hubs each year:")
print(top_annual_hubs)

# 5. Year-by-year correlation analysis
annual_correlations <- annual_hub_mhw %>%
  group_by(from_year) %>%
  summarise(
    n_hubs = n(),
    correlation_outdegree_mhw = cor(annual_out_degree, annual_mhw_intensity, use = "complete.obs"),
    correlation_species_mhw = cor(annual_species_out, annual_mhw_intensity, use = "complete.obs"),
    mean_out_degree = mean(annual_out_degree, na.rm = TRUE),
    mean_mhw_intensity = mean(annual_mhw_intensity, na.rm = TRUE),
    prop_years_with_mhw = sum(mhw_occurred) / n(),
    .groups = 'drop'
  )

print("Annual correlations between connectivity and MHW:")
print(annual_correlations)

# 6. Visualize annual patterns for top overall hubs
top_overall_hubs <- hub_hexagons$from_hex[1:6]  # Top 6 overall hubs

annual_top_hubs <- annual_hub_mhw %>%
  filter(from_hex %in% top_overall_hubs)

ggplot(annual_top_hubs, aes(x = from_year)) +
  geom_line(aes(y = annual_out_degree, color = "Outgoing Connections"), size = 1) +
  geom_line(aes(y = annual_mhw_intensity / max(annual_mhw_intensity, na.rm = TRUE) * max(annual_out_degree, na.rm = TRUE), 
                color = "MHW Intensity (scaled)"), size = 1) +
  geom_point(aes(y = annual_out_degree, size = annual_species_out), alpha = 0.6) +
  facet_wrap(~from_hex, scales = "free_y") +
  scale_y_continuous(
    name = "Outgoing Connections",
    sec.axis = sec_axis(~ . / max(annual_top_hubs$annual_out_degree, na.rm = TRUE) * max(annual_top_hubs$annual_mhw_intensity, na.rm = TRUE), 
                        name = "MHW Intensity (°C-days)")
  ) +
  scale_color_manual(values = c("Outgoing Connections" = "blue", "MHW Intensity (scaled)" = "red")) +
  scale_size_continuous(name = "Number of Species") +
  labs(
    title = "Annual Connectivity and MHW Intensity for Top Hub Hexagons",
    subtitle = "Blue: outgoing connections | Red: MHW intensity | Point size: number of species",
    x = "Year"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 7. Yearly hub network visualization
# Create annual hub maps for key years
key_years <- seq(min(annual_hub_mhw$from_year, na.rm = TRUE), 
                 max(annual_hub_mhw$from_year, na.rm = TRUE), by = 5)  # Every 5 years

for(year in key_years) {
  annual_data <- annual_hub_mhw %>%
    filter(from_year == year) %>%
    mutate(geometry = h3jsr::cell_to_polygon(from_hex)) %>%
    st_as_sf()
  
  if(nrow(annual_data) > 0) {
    p <- ggplot() +
      geom_sf(data = ocean, fill = "grey95", color = NA) +
      geom_sf(data = world, fill = "grey85", color = "grey70", size = 0.1) +
      geom_sf(data = annual_data, aes(fill = annual_out_degree), 
              color = "white", size = 0.2, alpha = 0.8) +
      geom_sf_text(data = annual_data %>% filter(annual_out_degree >= quantile(annual_out_degree, 0.8, na.rm = TRUE)),
                   aes(label = annual_out_degree),
                   color = "white", size = 3, fontface = "bold") +
      scale_fill_viridis_c(
        name = "Annual\nOutgoing\nConnections",
        option = "plasma"
      ) +
      labs(
        title = paste("Hub Connectivity Network in", year),
        subtitle = paste("MHW intensity range:", round(min(annual_data$annual_mhw_intensity, na.rm = TRUE), 1), 
                         "to", round(max(annual_data$annual_mhw_intensity, na.rm = TRUE), 1), "°C-days")
      ) +
      theme_minimal() +
      theme(axis.text = element_blank()) +
      coord_sf(
        xlim = c(hex_bbox$xmin, hex_bbox$xmax),
        ylim = c(hex_bbox$ymin, hex_bbox$ymax)
      )
    
    print(p)
    ggsave(paste0("hub_network_", year, ".png"), p, width = 10, height = 8, dpi = 300)
  }
}

# 8. MHW events and connectivity spikes
mhw_events_analysis <- annual_hub_mhw %>%
  group_by(from_hex) %>%
  mutate(
    connectivity_change = annual_out_degree - lag(annual_out_degree),
    mhw_change = annual_mhw_intensity - lag(annual_mhw_intensity),
    connectivity_spike = connectivity_change > quantile(connectivity_change, 0.75, na.rm = TRUE),
    mhw_spike = mhw_change > quantile(mhw_change, 0.75, na.rm = TRUE)
  ) %>%
  filter(!is.na(connectivity_change) & !is.na(mhw_change))

# Analyze if MHW spikes precede connectivity spikes
mhw_connectivity_lag <- mhw_events_analysis %>%
  mutate(
    mhw_precedes_connectivity = lag(mhw_spike) & connectivity_spike,
    same_year_spike = mhw_spike & connectivity_spike
  ) %>%
  summarise(
    n_years = n(),
    prop_mhw_precedes_connectivity = sum(mhw_precedes_connectivity, na.rm = TRUE) / n(),
    prop_same_year_spike = sum(same_year_spike, na.rm = TRUE) / n(),
    .groups = 'drop'
  )

print("Do MHW spikes precede connectivity spikes?")
print(mhw_connectivity_lag)

# 9. Statistical model: annual connectivity ~ MHW intensity + time
library(lme4)

# Mixed effects model accounting for hub-specific effects
connectivity_model <- lmer(annual_out_degree ~ annual_mhw_intensity + from_year + (1 | from_hex), 
                           data = annual_hub_mhw)

print("Mixed effects model results:")
print(summary(connectivity_model))

# 10. Create annual summary table
annual_summary <- annual_hub_mhw %>%
  group_by(from_year) %>%
  summarise(
    total_hubs = n_distinct(from_hex),
    total_connections = sum(annual_out_degree, na.rm = TRUE),
    total_species_movements = sum(annual_species_out, na.rm = TRUE),
    mean_mhw_intensity = mean(annual_mhw_intensity, na.rm = TRUE),
    hubs_with_mhw = sum(mhw_occurred, na.rm = TRUE),
    prop_hubs_with_mhw = hubs_with_mhw / total_hubs,
    .groups = 'drop'
  )

print("Annual summary of hub network and MHW conditions:")
print(annual_summary)

# 11. Save annual analysis results
#write_csv(annual_hub_mhw, "annual_hub_connectivity_mhw.csv")
#write_csv(annual_summary, "annual_network_summary.csv")
#write_csv(annual_correlations, "annual_correlations_analysis.csv")

