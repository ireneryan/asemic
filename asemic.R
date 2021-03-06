# Creates visualisations of asemic writing

# Load packages ----
library(animation)
library(cowplot)
library(dplyr)
library(gganimate)
library(ggart)
library(ggplot2)
library(steiner)
library(stringr)
library(tweenr)
library(viridis)

# Setup ----
set.seed(101) # make reproducible

# Parameters ----
n_cpts <- 7 # number of control points
min_edges <- 2 # minimum number of edges in a letter
max_edges <- n_cpts - 1 # maximum number of edges in a letter
n_letters <- 26 # number of letters in alphabet
bg_col <- "transparent" #rgb(248 / 255, 236 / 255, 194 / 255) #"lightGray" #"white" #"#F0EEE1" # rgb(255 / 255, 255 / 255, 255 / 255)
canvas_width <- 610 # 210mm in pixels
canvas_height <- 610 * 35 / 24 #* 297 / 210 # 297mm in pixels
margin_left <- 1 * 75.590551181 # 20mm in pixels
margin_right <- 1 * 75.590551181 # 20mm in pixels
margin_top <- 1 * 75.590551181 # 20mm in pixels
margin_bottom <- 1 * 75.590551181 # 20mm in pixels
letter_height <- 10
letter_width <- letter_height / ((1 + sqrt(5)) / 2)
letter_spacing <- letter_width / 2
line_spacing <- letter_spacing * 1 # 2mm in pixels
paragraph_indent <- 0 * margin_left # 20mm in pixels
p_space <- 0.05 # probability of a space
p_newline <- 0.05 # probability of a new line
nrow_newline <- 3 # minimum number of rows before starting a new line
space_width <- letter_width # 5mm in pixels
paragraph_spacing <- 1 * letter_height
font_colour <- "black" #"#07158A" # "darkgreen" #rgb(35 / 255, 38 / 255, 109 / 255)
cursive <- FALSE
corner_points <- TRUE
steiner <- FALSE
space_by_width <- FALSE
s <- 0.5
ruled_lines <- FALSE
highlight_text <- FALSE
write_script <- FALSE
script <- "this is a test"
script_vector <- str_split(script, "", simplify = TRUE)[1, ]
centre_vertically <- FALSE
noise <- TRUE
nudge <- letter_width * 1 / 2 # amount of noise to add to segments
nframes <- 1000

# Pre-processing
if(steiner) {
  n_cpts <- 3 * n_letters
  min_edges <- 3
  max_edges <- 3
  corner_points <- FALSE
}

# Create alphabet ----
# Create control points
if(corner_points) {
  control_pts <- data.frame(x = c(0, 1, 1, 0, runif(n_cpts - 4)), y = c(0, 0, 1, 1, runif(n_cpts - 4))) %>%
    #filter(d <= 1) %>%
    mutate(x = x * letter_width, y = y * letter_height)
  
  # control_pts <- data.frame(x = rep(seq(0, 1, s), times = 1/s+1),
  #                  y = rep(seq(0, 1, s), each = 1/s+1)) %>%
  #   mutate(x = x * letter_width, y = y * letter_height)
  # n_cpts <- nrow(control_pts)
} else {
  control_pts <- data.frame(x = c(runif(n_cpts)), y = runif(n_cpts)) %>%
    mutate(x = x * letter_width, y = y * letter_height)
}

# Calculate complete graph on control points
complete_graph <- complete_graph(control_pts)

# Create letters
alphabet <- data.frame(letter_id = integer(0), x = numeric(0), y = numeric(0),
                       xend = numeric(0), yend = numeric(0))

for(i in 1:n_letters) {
  if(steiner) {
    letter <- compute_smt3(c(control_pts$x[3 * (i - 1) + 1], control_pts$y[3 * (i - 1) + 1]),
                            c(control_pts$x[3 * (i - 1) + 2], control_pts$y[3 * (i - 1) + 2]),
                            c(control_pts$x[3 * (i - 1) + 3], control_pts$y[3 * (i - 1) + 3])) %>%
      mutate(letter_id = i) %>%
      select(letter_id, x, y, xend, yend)
  } else {
    letter <- sample_n(complete_graph, floor(runif(1, min_edges, max_edges + 1))) %>%
      mutate(letter_id = i)
  }
  alphabet <- alphabet %>%
    rbind(letter)
}

# Manual lookup
alphabet <- alphabet %>%
  rbind(data.frame(x = 0, y = 0, xend = 0.001, yend = 0.001, letter_id = nrow(.) + 1))
alphabet_lookup <- data.frame(letter = c(letters, " "),
                              index = c(346, 146, 29, 35, 186, 154, 203, 220, 330, 38, 24, 123,
                                        174, 100, 328, 384, 282, 258, 91, 120, 157, 19, 319, 41, 1, 162, nrow(alphabet)))

# Write text ----
lines <- data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0))
x_pos <- margin_left + paragraph_indent
y_pos <- canvas_height - margin_top - letter_height
lines <- lines %>% rbind(data.frame(x = margin_left, y = y_pos, xend = canvas_width - margin_right, yend = y_pos))
stop_writing <- FALSE
text <- data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0), letter_id = integer(0),
                   paragraph_id = integer(0), frame_id = integer(0))
points <- data.frame(x = numeric(0), y = numeric(0))
last_space <- TRUE
line_id <- 1
frame_id <- 1
paragraph_id <- 1
script_index <- 1

while(stop_writing == FALSE) {
  # sample letter from alphabet
  if(runif(1) < p_space & last_space == FALSE & write_script == FALSE) {
    last_space <- TRUE
  } else {
    if(write_script == TRUE) {
      letter <- alphabet_lookup$index[alphabet_lookup$letter == script_vector[script_index]]
      # if(letter == " ") {
      #   last_space <- TRUE
      # }
      script_index <- script_index + 1
    } else {
      letter <- sample(1:n_letters, 1)
    }
    # assign the letter its location on the canvas
    new_letter <- alphabet %>% filter(letter_id == letter) %>%
      mutate(x = x + x_pos, y = y + y_pos, xend = xend + x_pos, yend = yend + y_pos, paragraph_id = paragraph_id, frame = floor(frame_id / 2))
    text <- text %>% rbind(new_letter)
    points <- points %>% rbind(control_pts %>% mutate(x = x + x_pos, y = y + y_pos))
    last_space <- FALSE
  }
  if(write_script && script_index > length(script)) {
    stop_writing == TRUE
  }
  # update x_pos and y_pos
  if((runif(1) < p_newline & line_id >= nrow_newline)) {
    x_pos <- margin_left + paragraph_indent
    y_pos <- y_pos - paragraph_spacing - letter_height
    lines <- lines %>% rbind(data.frame(x = margin_left, y = y_pos, xend = canvas_width - margin_right, yend = y_pos))
    line_id <- 1
    paragraph_id <- paragraph_id + 1
  } else if(x_pos + letter_spacing + 2 * letter_width > canvas_width - margin_right) { # go to next line
    x_pos <- margin_left
    y_pos <- y_pos - line_spacing - letter_height
    lines <- lines %>% rbind(data.frame(x = margin_left, y = y_pos, xend = canvas_width - margin_right, yend = y_pos))
    line_id <- line_id + 1
    paragraph_id <- paragraph_id + 1
  } else {
    x_pos <- x_pos + letter_spacing + ifelse(space_by_width,
                                             max(new_letter[, c("x", "xend")]) - min(new_letter[, c("x", "xend")]), letter_width) +
      (space_width - letter_spacing) * (last_space == TRUE)
  }
  if(y_pos < margin_bottom) {
    stop_writing <- TRUE
  }
  frame_id <- frame_id + 1
}

# Centre vertically
if(centre_vertically == TRUE) {
  offset = (max(text$y) + 0 - min(text$y)) / 2 - canvas_height / 2
  text <- text %>%
    mutate(y = y + offset, yend = yend + offset)
}

# Create highlights ----
highlights <- lines %>%
  mutate(xmin = runif(nrow(.), margin_left, canvas_width - margin_right),
         xmax = runif(nrow(.), margin_left, canvas_width - margin_right),
         ymin = y, ymax = y + letter_height)

# Create command arrows
command_arrows0 <- data.frame(y = lines$y + 0.25 * letter_height,
                              yend = lines$yend + letter_height / 2,
                              x = paragraph_indent / 2 - 0.5 * letter_height) %>% mutate(xend = x + letter_height * 0.5)
command_arrows1 <- data.frame(y = lines$y + 0.75 * letter_height, yend = lines$yend + letter_height / 2, x = paragraph_indent / 2 - 0.5 * letter_height) %>% mutate(xend = x + 0.5 * letter_height)
command_arrows <- rbind(command_arrows0, command_arrows1)

# Make plot ----

if(noise) {
  text2 <- text %>% mutate(delta1 = runif(nrow(.), -nudge, nudge), x = x + delta1,
                           delta2 = runif(nrow(.), -nudge, nudge), y = y + delta2,
                           delta3 = runif(nrow(.), -nudge, nudge), xend = xend + delta3,
                           delta4 = runif(nrow(.), -nudge, nudge), yend = yend + delta4) %>%
    select(-delta1, -delta2, -delta3, -delta4)
  
  df <- list(text, text2, text)
  
  tf <- tween_states(df, tweenlength = 3, statelength = 0,
                     ease = "exponential-in",
                     nframes = nframes)
}

# Plot alphabet
# p2 <- ggplot(alphabet) +
#   geom_segment(aes(x = x, y = y, xend = xend, yend = yend), alphabet) +
#   coord_equal() +
#   facet_wrap(~letter_id) +
#   theme_blankcanvas +
#   theme(strip.text = element_text(size = 6))

# Main plot
p <- ggplot() +
  scale_x_continuous(limits = c(0, canvas_width), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, canvas_height), expand = c(0, 0)) +
  theme_blankcanvas(bg_col = bg_col)

if(ruled_lines) {
  p <- p + geom_segment(aes(x, y, xend = xend, yend = yend), lines, colour = "black", alpha = 1,
                        size = 0.1)
}

if(cursive) {
  p <- p +
    geom_path(aes(x, y, group = paragraph_id, frame = frame, cumulative = TRUE), tf, size = 0.5, colour = font_colour)
} else if (noise) {
  p <- p +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend, frame = .frame, cumulative = TRUE,
                     colour = letter_id),
                 tf %>% filter(x != xend & y != yend), # %>% filter(letter_id != nrow(alphabet)),
                 lineend = "round", alpha = 0.03, size = 1) +
    #scale_color_viridis(option = "B")
    scale_colour_gradient(low = "black", high = "red")
    #facet_wrap(~letter_id, scales = "free")
} else {
  p <- p +
    #geom_tile(aes(x = x, y = y, width = width, height = height), text %>% mutate(width = letter_height / 10, height = width), fill = font_colour)
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
                 text, # %>% filter(letter_id != nrow(alphabet)),
                 lineend = "round", alpha = 1, size = 0.3, colour = font_colour)
    #scale_size_continuous(range = c(0.1, 0.4)) + theme(legend.position = "none")
    #geom_point(aes(x, y), text, size = 0.5, colour = font_colour) +
    #geom_point(aes(xend, yend), text, size = 0.5, colour = font_colour)
    #geom_segment(aes(x, y, xend = xend, yend = yend), command_arrows, size = 0.35, colour = font_colour)
}

#p <- p + coord_polar()
  #geom_segment(aes(x, y, xend = xend, yend = yend), alphabet) +
  #
  #coord_equal() +
  #facet_wrap(~letter_id)

if(highlight_text) {
  p <- p +
    geom_rect(aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), highlights,
              fill = "yellow" , alpha = 0.5)
}
#p
#p <- p + coord_polar()

# Save plot ----
ggsave("test-14.png", p, width = 24, height = 35, units = "in")


# Save gif ----
# animation::ani.options(interval = 1/50)
# gganimate(p, "test.gif", title_frame = FALSE)
# 
