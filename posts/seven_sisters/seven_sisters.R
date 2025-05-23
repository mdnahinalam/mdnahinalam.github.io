# Install necessary packages
library(sf)
library(rnaturalearth)
library(ggplot2)
library(ggforce)
library(showtext)

# Optional: Use Google font
font_add_google("Poppins", "poppins")
showtext_auto()

# Load India admin boundaries
india <- ne_states(country = "India", returnclass = "sf")

# List of Seven Sisters
seven_sisters <- c("Arunachal Pradesh", "Assam", "Meghalaya", "Manipur", 
                   "Mizoram", "Nagaland", "Tripura")

# Subset Seven Sister states
seven_states <- india[india$name %in% seven_sisters, ]

# Capital coordinates
capitals <- data.frame(
  state = seven_sisters,
  capital = c("Itanagar", "Dispur", "Shillong", "Imphal", "Aizawl", "Kohima", "Agartala"),
  lon = c(93.6157, 91.7898, 91.8807, 93.9368, 92.7176, 94.1194, 91.2794),
  lat = c(27.1107, 26.1433, 25.5788, 24.8170, 23.7271, 25.6747, 23.8315)
)

# Optional: offset capital labels
label_offsets <- data.frame(
  dx = c(1, 0.8, 1, 1, 1, 1, 1),
  dy = c(1, 1, -1.5, -1, -1, 1.3, -1.5)
)

capitals <- cbind(capitals, label_offsets)

# Create plot
ggplot() +
  # Plot states
  geom_sf(data = seven_states, fill = NA, color = "darkgreen", linewidth = 1.2) +
  
  # Capital points
  geom_point(data = capitals, aes(x = lon, y = lat), color = "black", size = 3) +
  
  # Arrows from labels to capitals
  geom_curve(data = capitals, aes(x = lon + dx, y = lat + dy, xend = lon, yend = lat),
             curvature = 0.3, arrow = arrow(length = unit(0.15, "cm")), linewidth = 0.3) +
  
  # State names
  geom_text(data = capitals, aes(x = lon + dx, y = lat + dy + 0.4, label = state),
            fontface = "bold", family = "poppins", size = 4, hjust = 0) +
  
  # Capital names
  geom_text(data = capitals, aes(x = lon + dx, y = lat + dy - 0.4, label = capital),
            fontface = "italic", family = "poppins", size = 3.5, hjust = 0) +
  
  # Title and caption
  labs(title = "INDIA", subtitle = "Seven Sisters and their Capitals", caption = "*Map not to be scale") +
  
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.05, family = "poppins"),
    plot.subtitle = element_text(size = 10, hjust = 0.07, family = "poppins"),
    plot.caption = element_text(size = 8, hjust = 1, family = "poppins")
  ) +
  coord_sf(xlim = c(89.5, 97), ylim =c(21,29.5))
