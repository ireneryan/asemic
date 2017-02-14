# Creates visualisations of asemic writing

# Load packages ----
library(animation)
library(cowplot)
library(dplyr)
library(gganimate)
library(ggplot2)

# Setup ----
set.seed(101) # make reproducible

# Parameters ----
n_cpts <- 5 # number of control points
min_edges <- 2 # minimum number of edges in a letter
max_edges <- n_cpts - 1 # maximum number of edges in a letter
n_letters <- 26 # number of letters in alphabet
bg_col <- "#F9F8EF" # rgb(255 / 255, 255 / 255, 255 / 255)
canvas_width <- 793.700787402 # 210mm in pixels
canvas_height <- canvas_width # 297mm in pixels
margin_left <- 75.590551181 # 20mm in pixels
margin_right <- 75.590551181 # 20mm in pixels
margin_top <- 75.590551181 # 20mm in pixels
margin_bottom <- 75.590551181 # 20mm in pixels
letter_height <- 22 # 5mm in pixels
letter_width <- letter_height / 1.5
letter_spacing <- 75.590551181 / 20 # 1mm in pixels
line_spacing <- 37.795275591 / 10 # 2mm in pixels
paragraph_indent <- 75.590551181 # 20mm in pixels
p_space <- 0.3
p_newline <- 0.0075
nrow_newline <- 4
space_width <- letter_width * 0.45 # 5mm in pixels
paragraph_spacing <- 1.5 * letter_height

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
control_pts <- data.frame(x = c(0, 1, 1, 0, runif(n_cpts - 4)), y = c(0, 0, 1, 1, runif(n_cpts - 4))) %>%
  mutate(x = x * letter_width, y = y * letter_height)

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
  alphabet <- alphabet %>%
    rbind(letter)
}

# Write text ----
x_pos <- margin_left + paragraph_indent
y_pos <- canvas_height - margin_top
stop_writing <- FALSE
text <- data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0), letter_id = integer(0), frame_id = integer(0))
last_space <- TRUE
line_id <- 1
frame_id <- 1

while(stop_writing == FALSE) {
  # sample letter from alphabet
  if(runif(1) <= p_space & last_space == FALSE) {
    last_space <- TRUE
  } else {
    letter <- sample(1:n_letters, 1)
    # assign the letter its location on the canvas
    new_letter <- alphabet %>% filter(letter_id == letter) %>%
      mutate(x = x + x_pos, y = y + y_pos, xend = xend + x_pos, yend = yend + y_pos, frame = floor(frame_id / 2))
    text <- text %>% rbind(new_letter)
    last_space <- FALSE
  }
  # update x_pos and y_pos
  if(runif(1) < p_newline & line_id >= nrow_newline) {
    x_pos <- margin_left + paragraph_indent
    y_pos <- y_pos - paragraph_spacing - letter_height
    line_id <- 1
  } else if(x_pos + letter_spacing + letter_width > canvas_width - margin_right) { # go to next line
    x_pos <- margin_left
    y_pos <- y_pos - line_spacing - letter_height
    line_id <- line_id + 1
  } else {
    x_pos <- x_pos + letter_spacing + letter_width + (space_width - letter_spacing) * (last_space == TRUE)
  }
  if(y_pos - line_spacing - letter_height < margin_bottom) {
    stop_writing <- TRUE
  }
  frame_id <- frame_id + 1
}

# Make plot ----
p <- ggplot() +
  #geom_segment(aes(x, y, xend = xend, yend = yend), alphabet) +
  geom_segment(aes(x, y, xend = xend, yend = yend, frame = frame, cumulative = TRUE), text %>% filter(frame <= 150)) +
  # geom_point(aes(x, y), control_pts) +
  #coord_equal() +
  scale_x_continuous(limits = c(0, canvas_width), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, canvas_height), expand = c(0, 0)) +
  #facet_wrap(~letter_id) +
  theme_blankcanvas

# Save plot ----
#ggsave("asemic.png", p, width = 210, height = 210, units = "mm")

# Save gif ----
animation::ani.options(interval = 1/25)
gganimate(p, filename = "asemic.gif", title_frame = FALSE)
