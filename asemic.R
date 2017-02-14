# Creates visualisations of asemic writing

# Load packages ----
library(dplyr)
library(ggplot2)

# Setup ----
set.seed(101) # make reproducible

# Parameters ----
n_cpts <- 8 # number of control points
min_edges <- 2 # minimum number of edges in a letter
max_edges <- n_cpts - 1 # maximum number of edges in a letter
n_letters <- 30^2 # number of letters in alphabet
bg_col <- "#F9F8EF" # rgb(255 / 255, 255 / 255, 255 / 255)

# Plotting theme
theme_blankcanvas <- theme(
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  legend.position = "none",
  panel.background = element_rect(fill = "transparent"),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  plot.margin = unit(c(0 + 5, 0 + 5, -1 + 5, -1 + 5), "mm"), # top, right, bottom, left
  strip.background = element_blank(),
  strip.text = element_blank())

# Create alphabet ----

# Create control points
control_pts <- data.frame(x = c(0, 1, 1, 0, runif(n_cpts - 4)), y = c(0, 0, 1, 1, runif(n_cpts - 4)))

# Calculate complete graph on control points
complete_graph <- data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0))

for(i in seq(1, n_cpts - 1)) {
  for(j in seq(i + 1, n_cpts)) {
    complete_graph <- complete_graph %>%
      rbind(data.frame(x = control_pts$x[i], y = control_pts$y[i],
                       xend = control_pts$x[j], yend = control_pts$y[j]))
  }
}

# Create letters
alphabet <- data.frame(letter_id = integer(0), x = numeric(0), y = numeric(0),
                       xend = numeric(0), yend = numeric(0))

for(i in 1:n_letters) {
  letter = sample_n(complete_graph, floor(runif(1, min_edges, max_edges + 1))) %>%
    mutate(letter_id = i)
  alphabet <- alphabet %>% rbind(letter)
}

# Make plot ----
p <- ggplot() +
  geom_segment(aes(x, y, xend = xend, yend = yend), alphabet) +
  # geom_point(aes(x, y), control_pts) +
  coord_equal() +
  facet_wrap(~letter_id) +
  theme_blankcanvas

# Save plot ----
ggsave("asemic.png", p, width = 297, height = 297, units = "mm")
