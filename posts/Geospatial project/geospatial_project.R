require(rerddap)
require(sf)
require(tidyverse)
require(lubridate)
require(oce)
require(gganimate)
require(RColorBrewer)
require(rnaturalearth)
require(rnaturalearthdata)

windowsFonts(JP1 = windowsFont("MS Mincho"),
             JP2 = windowsFont("MS Gothic"),
             JP3 = windowsFont("Arial Unicode MS"),
             gill = windowsFont('Gill Sans'),
             gill_mt = windowsFont('Gill Sans MT'),
             open_sans = windowsFont('Open Sans'),
             open_sans_bold = windowsFont('Open Sans Bold'),
             open_sans_sb = windowsFont('Open Sans SemiBold'),
             barlow = windowsFont("Barlow"))

world = ne_countries(returnclass = "sf")

# all datasets
dataset_errddap = ed_datasets('table')


# search dataset
velocity_all_data = ed_search("velocity")


# get info of velocity dataset
velocity_info = info("miamicurrents")
velocity_info$variables$variable_name

# now i will define the time, latitude, longitude
velocity_name = "miamicurrents"
time = c('2025-01-01','2025-05-06')
latitude = c(5, 27)
longitude = c(50, 96)


# Download the data
current = griddap(
  "miamicurrents",
  time = time,
  latitude = latitude,
  longitude = longitude,
  fields = c('u_current','v_current')
)

class(current)

# Extract the current data from the

current = current$data


# lets create a function to calcualte the velocity using the east-ward (u) and west-ward (v) data

calc_velocity = function(data, uc, vc) {
  calculation = sqrt(uc^2 + vc^2)
  return(calculation)
}

# create a new column to calculate the velocity
current_with_velocity = current |>
  mutate(velocity = calc_velocity(uc = u_current, vc = v_current))


# working with time
current_with_velocity$time = as.Date(current_with_velocity$time)

# remove the hours
current_with_velocity = current_with_velocity |>
  mutate(time = ymd(time))

# extract the month
current_with_velocity = current_with_velocity |>
  mutate(month = month(time)|> as.integer()) 

# now calcualte the monthly average (climatology)

current_with_velocity_monthly = current_with_velocity |> 
  select(longitude, latitude, month, velocity) |>
  group_by(longitude, latitude, month) |>
  summarise(mean_vel = mean(velocity), .groups = "drop")

# create a new column with month name
current_with_velocity_monthly = current_with_velocity_monthly|>
  mutate(month_name = case_when(
    month == 1 ~"January",
    month == 2 ~"February",
    month == 3 ~"March",
    month == 4 ~"April",
    month == 5 ~"May",
  ))

# Ensure month_name is ordered correctly
current_with_velocity_monthly$month_name <- factor(
  current_with_velocity_monthly$month_name,
  levels = month.name[1:5]
)


# Create a lookup for month names
month_labels <- setNames(
  month.name[1:5],  # Using abbreviated month names (Jan-Dec)
  1:5
)

# test using single month
current_with_velocity_monthly_january = current_with_velocity_monthly |> 
  filter(month == "January")

# ploting january current

ggplot() +
  geom_raster(data = current_with_velocity_monthly, 
              aes(x = longitude, y = latitude, fill = mean_vel), 
              interpolate = F) + 
  geom_sf(data = world, col = "grey", fill = "grey100") +
  coord_sf(xlim = c(50, 96), ylim = c(5, 27), expand = FALSE) +
  
  theme_void() + 
  
  labs(x = "Longitude", y = "Latitude") +
  
  
  scale_fill_gradientn(
    colors = oce.colorsVelocity(128),
    name = "Velocity [m/s]",
    breaks = seq(0, 2, by = 0.5),
    labels = function(x) sprintf("%.2f", x),
    guide = guide_colorbar(
      barheight = unit(0.5, "cm"),  
      barwidth = unit(12, "cm"),
      frame.colour = "black",
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 0.5,
      label.position = "bottom",
      label.vjust = 0.5,
      label.theme = element_text(
        family = "open_sans",
        size = 14,
        face = "bold",
        margin = margin(r = 10)
      )           
    )
  ) +
  labs(
    title = "Surface Current in Arabian sea and Bay of Bengal",
    subtitle = "January 01 2025 to May 06 2025",
    caption = "Data: NOAA"
  ) +
  
  theme(
    axis.text = element_blank(),
    panel.grid = element_line(colour = NA), 
    plot.title = element_text(family = "open_sans_bold", size = 16, hjust = 0.5, face = "bold"),
    legend.title = element_text(family = "open_sans", size = 12, face = "bold",
                                angle = 0),
    legend.text = element_text(family = "open_sans", size = 12, face = "bold", angle = 0),
    legend.ticks = element_blank(),
    plot.subtitle = element_text(family = "open_sans", size = 12, hjust = 0.5, 
                                 margin = margin(b = 30, t = 10)),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    legend.position = "bottom",
    panel.border = element_blank()
  ) +
   annotate("text", 
            x = 65,
            y = 18,
            color = "white",
            label = "Arabian Sea",
            fontface = "italic",
            family = "open_sans") +
  annotate("text", 
           x = 89,
           y = 18,
           color = "white",
           label = "Bay of Bengal",
           fontface = "italic",
           family = "open_sans") +
  annotate("text", 
           x = 90.5,
           y = 24.5,
           color = "black",
           label = "Bangladesh",
           family = "open_sans",
           size = 2.6) +
  annotate("text", 
           x = 79,
           y = 24,
           color = "black",
           label = "India",
           family = "open_sans") +
  annotate("text", 
           x = 66,
           y = 26.5,
           color = "black",
           label = "Pakistan",
           family = "open_sans") +
  annotate("text", 
           x = 57.5,
           y = 22,
           color = "black",
           label = "Oman",
           family = "open_sans") +
  annotate("text", 
           x = 54,
           y = 23.5,
           color = "black",
           label = "UAE",
           family = "open_sans") +
  annotate("text", 
           x = 52.5,
           y = 21,
           color = "black",
           label = "Saudi Arabia",
           family = "open_sans",
           size = 3) +
  annotate("text", 
           x = 80.5,
           y = 8,
           color = "black",
           label = "Sri Lanka",
           family = "open_sans",
           size = 2.5, 
           angle = 105) +
  annotate("text", 
           x = 94.3,
           y = 21,
           color = "black",
           label = "Myanmar",
           family = "open_sans",
           size = 2.8) +
  annotate("text", 
           x = 60,
           y = 26.5,
           color = "black",
           label = "Afghanistan",
           family = "open_sans",
           size = 2)



  

  
  
  # Create a base plot with oce colors
bay_plot <- ggplot() +
  geom_raster(data = current_with_velocity_monthly, 
              aes(x = longitude, y = latitude, fill = mean_vel), 
              interpolate = F) + 
  geom_sf(data = spData::world, col = "grey50", fill = "grey95") +
  coord_sf(xlim = c(50, 96), ylim = c(5, 27), expand = FALSE) +
  theme_bw() + 
  
  labs(x = "Longitude", y = "Latitude") +
  
  # Custom x-axis scale
  scale_x_continuous(
    breaks = seq(50, 96, by = 5),
    labels = function(x) paste0(x, "°E"),
    expand = c(0, 0)
  ) +
  
  # Custom y-axis scale
  scale_y_continuous(
    breaks = seq(5, 27, by = 2),
    labels = function(y) paste0(y, "°N"),
    expand = c(0, 0)
  ) +
  
  scale_fill_gradientn(
    colors = oce.colorsVelocity(128),
    name = "Velocity [m/s]",
    breaks = seq(0, 2, by = 0.5),
    labels = function(x) sprintf("%.2f", x),
    guide = guide_colorbar(
      barheight = unit(0.5, "cm"),  
      barwidth = unit(12, "cm"),
      frame.colour = "black",
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 0.5,
      label.position = "bottom",
      label.vjust = 0.5,
      label.theme = element_text(
        family = "open_sans",
        size = 14,
        face = "bold",
        margin = margin(r = 10)
      )           
    )
  ) +
  
  
  labs(
    title = "Surface Current in Arabian sea and Bay of Bengal, 2025",
    subtitle = 'Month: {month_labels[round(frame_time)]}',
    caption = "Data: NOAA"
  ) +
  
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12, family = "open_sans"),
    panel.grid = element_line(colour = NA), 
    plot.title = element_text(family = "Gill Sans MT", size = 35, hjust = 0.5, face = "bold"),
    legend.title = element_text(family = "open_sans", size = 14, face = "bold",
                                angle = 0),
    legend.text = element_text(family = "open_sans", size = 12, face = "bold", angle = 0),
    legend.ticks = element_blank(),
    plot.subtitle = element_text(family = "Gill Sans MT", size = 24, hjust = 0.5, , face = "bold",
                                 margin = margin(b = 30, t = 10)),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    legend.position = "bottom",
    panel.border = element_blank()
  ) +
  annotate("text", 
           x = 65,
           y = 18,
           color = "white",
           label = "Arabian Sea",
           fontface = "italic",
           family = "open_sans",
           size = 10) +
  annotate("text", 
           x = 89,
           y = 18,
           color = "white",
           label = "Bay of Bengal",
           fontface = "italic",
           family = "open_sans",
           size = 10) +
  annotate("text", 
           x = 90.5,
           y = 24.5,
           color = "black",
           label = "Bangladesh",
           family = "open_sans",
           size = 6) +
  annotate("text", 
           x = 79,
           y = 24,
           color = "black",
           label = "India",
           family = "open_sans",
           size = 6) +
  annotate("text", 
           x = 66,
           y = 26.5,
           color = "black",
           label = "Pakistan",
           family = "open_sans",
           size = 6) +
  annotate("text", 
           x = 57.5,
           y = 22,
           color = "black",
           label = "Oman",
           family = "open_sans",
           size = 6) +
  annotate("text", 
           x = 54,
           y = 23.5,
           color = "black",
           label = "UAE",
           family = "open_sans") +
  annotate("text", 
           x = 52.5,
           y = 21,
           color = "black",
           label = "Saudi Arabia",
           family = "open_sans",
           size = 7) +
  annotate("text", 
           x = 80.5,
           y = 8,
           color = "black",
           label = "Sri Lanka",
           family = "open_sans",
           angle = 105,
           size = 5) +
  annotate("text", 
           x = 94.3,
           y = 21,
           color = "black",
           label = "Myanmar",
           family = "open_sans",
           size = 6) +
  annotate("text", 
           x = 60,
           y = 26.5,
           color = "black",
           label = "Afghanistan",
           family = "open_sans",
           size = 5) +
  
  transition_time(month) +
  ease_aes('linear') +
  enter_fade() +
  exit_fade()

# Render the animation directly
final_animation <- animate(
  bay_plot,  # Use the plot object, not last_animation()
  nframes = length(unique(current_with_velocity_monthly$month)) * 10,
  fps = 8,
  width = 1500,
  height = 900,
  renderer = gifski_renderer(loop = TRUE)
)

# Save the animation
anim_save("BOB_Arabian_sea_CURRENT_animation_2025_2.gif")










