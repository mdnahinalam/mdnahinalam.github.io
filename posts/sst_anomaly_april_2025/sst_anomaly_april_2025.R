##### Load libraries #####
library(terra)
library(tidyverse)
library(ggthemes)
library(sf)
library(giscoR)
library(RColorBrewer)
library(gifski)
library(gganimate)
library(sf)
library(rerddap)
library(patchwork)

# read all the shapefiles of required countries
bd = st_read("K:/shapefiles/country/BGD_adm/BGD_adm0.shp")
bd_sf = st_as_sf(bd)

ind = st_read("K:/shapefiles/country/IND_adm/IND_adm0.shp")
ind_sf = st_as_sf(ind)

myn = st_read("K:/shapefiles/country/MMR_adm/MMR_adm0.shp")
myn_sf = st_as_sf(myn)

sri = st_read("K:/shapefiles/country/srilanka/LKA_adm/LKA_adm0.shp")
sri_sf = st_as_sf(sri)  


bd_sf <- st_transform(bd_sf, crs = 4326)
ind_sf <- st_transform(ind_sf, crs = 4326)
myn_sf <- st_transform(myn_sf, crs = 4326)
sri_sf <- st_transform(sri_sf, crs = 4326)

# read the sst data from the errddap

windowsFonts(JP1 = windowsFont("MS Mincho"),
             JP2 = windowsFont("MS Gothic"),
             JP3 = windowsFont("Arial Unicode MS"),
             gill = windowsFont('Gill Sans'),
             gill_mt = windowsFont('Gill Sans MT'),
             open_sans = windowsFont('Open Sans'),
             open_sans_bold = windowsFont('Open Sans Bold'),
             open_sans_sb = windowsFont('Open Sans SemiBold'),
             barlow = windowsFont("Barlow"))

library(showtext)

# Add Google font (bold will be auto-matched)
font_add_google("Roboto", "roboto")

# Turn on showtext
showtext_auto()

world = ne_countries(returnclass = "sf")

# all datasets
dataset_errddap = ed_datasets('table')


# search dataset
sst_all_data = ed_search("SST")
sst_all_data$info




# get info of velocity dataset
sst_info = info("ncdcOisst21Agg")
sst_info$variables$variable_name

# now i will define the time, latitude, longitude
sst_name = "ncdcOisst21Agg"
time = c('2015-01-01','2025-04-29')
latitude = c(5, 23.5)
longitude = c(79, 95)


# Download the data
sst = griddap(
  sst_name,
  time = time,
  latitude = latitude,
  longitude = longitude,
  fields = c('sst')
)


# extract the data
sst = sst$data


# convert the date to actual date format
sst = sst |>
  mutate(time = lubridate::as_date(time)) |>
  select(-zlev)


# qucick visualization of day one of 2015

sst_2015_01_01 = sst |> filter(month(time) == 1)


ggplot() +
  geom_raster(data = sst_2015_01_01, aes(x = longitude, y = latitude, fill = sst)) +
  geom_sf(data = world, fill = "gray80", color = "black") +
  coord_sf(xlim = c(min(sst_2015_01_01$longitude), max(sst_2015_01_01$longitude)), 
           ylim = c(min(sst_2015_01_01$latitude), max(sst_2015_01_01$latitude)),
           expand = F)



##

# extract the days
sst_day = sst |>
  mutate(day = day(time),
         month = month(time))

# filter baseline period 

baseline_period = sst_day |>
  filter(year(time) < 2025)


# Filter only April data for climatology calculation
baseline_april <- baseline_period %>% filter(month == 4)

# Calculate daily climatology for each location (longitude, latitude) and each day in April
daily_climatology <- baseline_april %>%
  group_by(longitude, latitude, day) %>%
  summarise(climatology_sst = mean(sst, na.rm = TRUE), .groups = 'drop')



# filter 2025 frpm main data
sst_2025_april = sst_day |>
  filter(year(time) == 2025, month(time) == 4)


anomaly_april_2025 <- sst_2025_april %>%
  left_join(daily_climatology, by = c("longitude", "latitude", "day"), 
            relationship = "many-to-many") %>%
  mutate(anomaly = sst - climatology_sst)


##############

# make animation


# Make sure anomaly_april_2025 has 'day' as a factor for animation frame
anomaly_april_2025 <- anomaly_april_2025 %>%
  mutate(day = as.factor(day))

# Define map limits based on your data range (adjust if needed)
xlim_range <- range(anomaly_april_2025$longitude)
ylim_range <- range(anomaly_april_2025$latitude)

# Basic plot
p <- ggplot() +
  # Plot the world map as background
  geom_sf(data = world, fill = "gray80", color = "black") +
  
  # Plot anomaly as tiles or points
  geom_tile(data = anomaly_april_2025, 
            aes(x = longitude, y = latitude, fill = anomaly)) +
  
  # Set color scale for anomaly (blue for negative, red for positive)
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0,
    limits = c(min(anomaly_april_2025$anomaly, na.rm = TRUE), 
               max(anomaly_april_2025$anomaly, na.rm = TRUE)),
    name = "SST Anomaly (°C)"
  ) +
  
  coord_sf(xlim = xlim_range, ylim = ylim_range, expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Daily Sea Surface Temperature Anomaly",
    subtitle = "April 2025 - Day: {closest_state}",
    x = "Longitude", y = "Latitude"
  ) +
  
  # Animate over 'day' variable
  transition_states(day, transition_length = 2, state_length = 1) +
  ease_aes('linear')

# Render the animation (will save as GIF)
gganimate::animate(p, nframes = length(unique(anomaly_april_2025$day)) * 5, fps = 10, width = 700, height = 500, renderer = gifski_renderer("sst_anomaly_april2025.gif"))


########################################################################

# step 1
anomaly_data <- anomaly_april_2025 %>%
  mutate(
    date = as.Date(time),
    day = as.integer(day),
    anomaly = as.numeric(anomaly)
  ) %>%
  arrange(date, longitude, latitude)


# Create a function that generates the grid lines as a separate layer
create_grid_layer <- function(x_breaks, y_breaks, color = "gray50", size = 0.2, alpha = 0.5) {
  list(
    # Horizontal grid lines
    annotation_custom(
      grob = segmentsGrob(
        x0 = unit(0, "npc"), x1 = unit(1, "npc"),
        y0 = unit(y_breaks, "native"), y1 = unit(y_breaks, "native"),
        gp = gpar(col = color, lwd = size, alpha = alpha)
      ),
      xmin = min(x_breaks), xmax = max(x_breaks)
    ),
    # Vertical grid lines
    annotation_custom(
      grob = segmentsGrob(
        y0 = unit(0, "npc"), y1 = unit(1, "npc"),
        x0 = unit(x_breaks, "native"), x1 = unit(x_breaks, "native"),
        gp = gpar(col = color, lwd = size, alpha = alpha)
      ),
      ymin = min(y_breaks), ymax = max(y_breaks)
    )
  )
}

# Define your grid spacing
x_breaks <- seq(floor(min(anomaly_data$longitude)), ceiling(max(anomaly_data$longitude)), by = 1)
y_breaks <- seq(floor(min(anomaly_data$latitude)), ceiling(max(anomaly_data$latitude)), by = 1)


# step 2
library(rnaturalearth)
coastline <- ne_coastline(scale = "large", returnclass = "sf") %>%
  st_crop(xmin = min(anomaly_data$longitude), xmax = max(anomaly_data$longitude),
          ymin = min(anomaly_data$latitude), ymax = max(anomaly_data$latitude))

# Step 3: Create Static Map Template

base_map <- ggplot() +
  geom_sf(data = bd_sf, color = "gray40", fill = "grey95",
          size = 0.15, lineend = "round") +
  geom_sf(data = ind_sf, color = "gray40", fill = "grey95",
          size = 0.15, lineend = "round") +
  geom_sf(data = myn_sf, color = "gray40", fill = "grey95",
          size = 0.15, lineend = "round") +
  geom_sf(data = sri_sf, color = "gray40", fill = "grey95",
          size = 0.15, lineend = "round") +
  coord_sf(
    xlim = c(79, 95),
    ylim = c(5, 25)
  )
  
  
# Step 4: Create Animated Plot
library(viridis)  # For better color scales

animated_plot <- ggplot() +

  geom_raster(data = anomaly_data, 
            aes(x = longitude, y = latitude, fill = anomaly)) +

  
  geom_contour(data = anomaly_data,
               aes(x = longitude, y = latitude, z = anomaly),
               color = "black", alpha = 0.5, bins = 10) +
  geom_sf(data = bd_sf, fill = "grey90", color = "gray40", size = 0.2) +
  geom_sf(data = ind_sf, fill = "grey90", color = "gray40", size = 0.2) +
  geom_sf(data = myn_sf, fill = "grey90", color = "gray40", size = 0.2) +
  geom_sf(data = sri_sf, fill = "grey90", color = "gray40", size = 0.2) +
  
  coord_sf(xlim = c(79, 95),
           ylim = c(5, 25),
           expand = FALSE ) +
  
  scale_fill_gradient2(
    low = "#313695", mid = "white", high = "#A50026",
    midpoint = 0, limits = c(-1.5, 1.5),  # Adjust to match desired range
    breaks = c(-1.5, 0, 1.5),
    labels = c("-1.5", "0", "+1.5"),
    name = "Anomaly (°C)",
    guide = guide_colorbar(
      barwidth = unit(6, "cm"),
      barheight = unit(0.5, "cm"),
      frame.colour = "black",
      ticks = FALSE,
      title.position = "top"
    )
  ) +
  
  scale_x_continuous(
    breaks = seq(79, 95, by = 5),
    labels = function(x) paste0(x, "°E")
  ) +
  
  scale_y_continuous(
    breaks = seq(5, 25, by = 5),
    labels = function(y) paste0(y, "°N")
  ) +
      
  labs(
    title = "Daily Sea Surface Temperature Anomalies",    
    subtitle = "{frame_time}",
    caption = "Baseline Period: 2015–2024                                                                              Data: NOAA"
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "grey60",
                                linewidth = 0.5),
    axis.text = element_text(color = "black", 
                             family = "barlow"),
    legend.position = "bottom",
    legend.title = element_text(size = 10, hjust = .5, 
                                color = "black"),
    axis.ticks = element_line(color = "black",
                                linewidth = 0.7),
    legend.text = element_text(size = 8, color = "black",
                               family = "barlow"),
    plot.caption = element_text(hjust = 0, size = 12, 
                                color = "black", 
                                family = "barlow"),
    plot.margin = margin(t = 10, r = 0, b = 40, l = 0),
    plot.title = element_text(face = "bold", 
                              family = "roboto",
                              size = 24,
                              margin = margin(t = 0.4, r = 0,
                                              b = 0.2, l = 0,
                                              unit = "cm"),
                              hjust = 0.5),
    plot.subtitle = element_text(face = "bold", 
                                 family = "roboto",
                                 size = 14,
                                 margin = margin(t = 0.2, r = 0,
                                                 b = 0.6, l = 0,
                                                 unit = "cm"),
                                 hjust = 0.5)
  ) +


  transition_time(date) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()

# step 4.2 
# Define your lat/lon lines
lon_lines <- seq(79, 95, by = 5)   # Adjust for your domain
lat_lines <- seq(5, 25, by = 5)

# Add to your `animated_plot` after everything else
animated_plot <- animated_plot +
  geom_vline(xintercept = lon_lines, color = "gray60", size = 0.7, linetype = "dotted") +
  geom_hline(yintercept = lat_lines, color = "gray60", size = 0.7, linetype = "dotted")


# Step 5: Render the Animation


final_animation <- animate(
  animated_plot,
  nframes = length(unique(anomaly_data$date)),
  fps = 4,  # 2 frames per second
  width = 900,
  height = 1200,
  res = 150,
  renderer = gifski_renderer()
)


# Step 6: Save the Animation

anim_save("april_2025_sst_anomalies.gif", final_animation)



################################################

####### data preparing for line chart

##################################################

line_data <- anomaly_data %>%
  group_by(date) %>%
  summarise(mean_anomaly = mean(anomaly, na.rm = TRUE))

library(ggrepel)


# Create a more robust version of your line plot
line_plot <- ggplot(line_data, aes(x = date, y = mean_anomaly, group = 1)) +  # Added group aesthetic
  # Background line (shows full path)
  geom_line(color = "gray80", size = 1.5) +
  
  # Animated line (will grow with animation)
  geom_line(color = "#d7191c", size = 1.5) +
  
  # Current point highlight
  geom_point(color = "black", size = 4) +
  
  # Label for current value
  geom_text(
    aes(label = sprintf("%.2f°C", mean_anomaly)),
    vjust = -1,  # Adjust this to position labels
    color = "black",
    size = 5
  ) +
  
  # Reference line at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  # Scales and labels
  scale_x_date(date_labels = "%b %d", date_breaks = "5 days") +
  scale_y_continuous(limits = c(min(line_data$mean_anomaly) - 0.1, 
                                max(line_data$mean_anomaly) + 0.1)) +
  labs(
    title = "Daily Mean Sea Surface Temperature Anomaly",
    x = "Date",
    y = "Temperature Anomaly (°C)"
  ) +
  
  # Theme
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14,
                              family = "roboto", 
                              hjust = 0.5),
    axis.text = element_text(size = 12, 
                             family = "barlow"),
    axis.title = element_text(size = 12, 
                              family = "barlow")
  )

# Create animation with more conservative settings
animated_line <- line_plot +
  transition_reveal(date) +
  ease_aes('linear')

# Render animation with safer parameters
final_line_animation <- animate(
  animated_line,
  nframes = length(unique(line_data$date)),  # Reduced from 5 to 3 frames per day
  fps = 4,                       # Reduced from 15
  width = 800,
  height = 500,
  res = 150,
  renderer = gifski_renderer(),
  loop = TRUE
)

# Save with error handling
tryCatch({
  anim_save("april2025_sst_anomaly_trend.gif", final_line_animation)
}, error = function(e) {
  message("Error saving animation: ", e$message)
  # Try saving as mp4 if gif fails
  anim_save("april2025_sst_anomaly_trend.mp4", final_line_animation, renderer = ffmpeg_renderer())
})




library(magick)

# Read your GIFs
gif1 <- image_read("april_2025_sst_anomalies.gif")
gif2 <- image_read("april2025_sst_anomaly_trend.gif")

combined <- image_append(c(gif1, gif2), stack = T)

# Save the result
image_write(combined_side, "combined_side.gif")

