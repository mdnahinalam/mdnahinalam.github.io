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
             open_sans_sb = windowsFont('Open Sans SemiBold'))

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
latitude = c(5, 24)
longitude = c(79, 95)


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


# quick visualize the data
current_with_velocity |> filter(month(time) == 1) |>
  ggplot() + 
  #geom_raster(aes(longitude,  latitude, fill = velocity)) +
  geom_contour_filled(aes(longitude,  latitude, z = velocity))

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



# Create a base plot with oce colors
bay_plot <- ggplot() +
  geom_raster(data = current_with_velocity_monthly, 
              aes(x = longitude, y = latitude, fill = mean_vel), 
              interpolate = F) + 
  geom_sf(data = spData::world, col = "grey10", fill = "grey70") +
  coord_sf(xlim = c(79, 95), ylim = c(5, 24), expand = FALSE) +
  theme_bw() + 
  
  labs(x = "Longitude", y = "Latitude") +
  
  # Custom x-axis scale
  scale_x_continuous(
    breaks = seq(80, 95, by = 2),
    labels = function(x) paste0(x, "°E"),
    expand = c(0, 0)
  ) +
  
  # Custom y-axis scale
  scale_y_continuous(
    breaks = seq(5, 25, by = 2),
    labels = function(y) paste0(y, "°N"),
    expand = c(0, 0)
  ) +
  
  scale_fill_gradientn(
    colors = oce.colorsVelocity(128),
    name = "Velocity [m/s]",
    breaks = seq(0, 2, length.out = 8),
    labels = function(x) sprintf("%.2f", x),
    guide = guide_colorbar(
      barheight = unit(28, "cm"),  
      barwidth = unit(1.5, "cm"),
      frame.colour = "black",
      title.position = "right",
      title.hjust = 0.5,
      title.vjust = 0.5,
      label.position = "right",
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
    title = "Surface Current in BOB, 2025",
    subtitle = 'Month: {month_labels[round(frame_time)]}'
  ) +
  
  theme(
    axis.text = element_text(family = "open_sans", size = 12, colour = "black"),
    panel.grid = element_line(colour = NA), 
    plot.title = element_text(family = "open_sans_bold", size = 24),
    legend.title = element_text(family = "open_sans", size = 12, face = "bold",angle = 90),
    legend.text = element_text(family = "open_sans", size = 12, face = "bold", angle = 90),
    legend.ticks = element_blank(),
    plot.subtitle = element_text(family = "open_sans", size = 12)
  ) +
  
  transition_time(month) +
  ease_aes('linear') +
  enter_fade() +
  exit_fade()

# Render the animation directly
final_animation <- animate(
  bay_plot,  # Use the plot object, not last_animation()
  nframes = length(unique(current_with_velocity_monthly$month)) * 10,
  fps = 8,
  width = 1200,
  height = 900,
  renderer = gifski_renderer(loop = TRUE)
)

# Save the animation
anim_save("BOB_CURRENT_animation_2025_2.gif")
anim_save("BOB_CURRENT_animation_2025_2.mp4")



















