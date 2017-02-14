# Creates visualisations of asemic writing

# Load packages ----
library(animation)
library(cowplot)
library(dplyr)
library(gganimate)
library(ggplot2)
library(stringr)
library(tweenr)

# Setup ----
set.seed(101) # make reproducible

# Parameters ----
n_cpts <- 7 # number of control points
min_edges <- 2 # minimum number of edges in a letter
max_edges <- n_cpts - 1 # maximum number of edges in a letter
n_letters <- 400 # number of letters in alphabet
bg_col <- "gray95" #rgb(248 / 255, 236 / 255, 194 / 255) #"lightGray" #"white" #"#F0EEE1" # rgb(255 / 255, 255 / 255, 255 / 255)
canvas_width <- 793.700787402 # 210mm in pixels
canvas_height <- canvas_width #* 297 / 210 # 297mm in pixels
margin_left <- 75.590551181 # 20mm in pixels
margin_right <- 75.590551181 # 20mm in pixels
margin_top <- 0 * 75.590551181 # 20mm in pixels
margin_bottom <- 0 * 75.590551181 # 20mm in pixels
letter_height <- 65 # 5mm in pixels
letter_width <- letter_height / 1.6
letter_spacing <- letter_width / 10 # 1mm in pixels
line_spacing <- letter_height / 2 # 2mm in pixels
paragraph_indent <- 0 * margin_left # 20mm in pixels
p_space <- 0 # probability of a space
p_newline <- 0 # probability of a new line
nrow_newline <- 0 # minimum number of rows before starting a new line
space_width <- letter_width # 5mm in pixels
paragraph_spacing <- letter_height
font_colour <- "black" #"#07158A" # "darkgreen" #rgb(35 / 255, 38 / 255, 109 / 255)
cursive <- FALSE
corner_points <- TRUE
steiner <- FALSE
space_by_width <- FALSE
s <- 0.5
ruled_lines <- FALSE
highlight_text <- FALSE
write_script <- TRUE
script <- "the five boxing wizards jump quickly"
script_vector <- str_split(script, "", simplify = TRUE)[1, ]
centre_vertically <- TRUE

# Pre-processing
if(steiner) {
  n_cpts <- 3 * n_letters
  min_edges <- 3
  max_edges <- 3
  corner_points <- FALSE
}

# General functions ----
# Function for computing equilateral point for ab (c is point of opposite side of equilateral point)
compute_e <- function(a, b, c) { # a, b and c are vectors of length two (x and y components)
  # Calculate the midpoint of ab
  m <- (a + b) / 2
  # Calculate the gradient of the line through m that is perpendicular to ab
  grad_ab <- ifelse(a[1] == b[1], Inf, (b[2] - a[2]) / (b[1] - a[1])) # m = (y2 - y1) / (x2 - x1)
  grad_em <- ifelse(grad_ab == 0, Inf, -1 / grad_ab) #m1m2 = -1 for perpendicular lines
  # Calculate the intercept of the line through m that is perpendicular to ab
  int_em <- ifelse(grad_em %in% c(-Inf, Inf), NA, m[2] - grad_em * m[1]) # y = mx + c --> c = y - mx
  # calculate the distance from the midpoint of ab to the equilateral point
  dist <- sqrt(3) / 2 * sqrt((b[1] - a[1])^2 + (b[2] - a[2])^2)
  # calculate the co-ordinates of equilateral point 1
  ex_1 <- m[1] + sqrt(dist^2 / (1 + grad_em^2)) # x = x1 +/- sqrt(d^2 / (1 + m^2))
  ey_1 <- ifelse(is.na(int_em), m[2] + dist, grad_em * ex_1 + int_em)
  # calculate the co-ordinates of equilateral point 2
  ex_2 <- m[1] - sqrt(dist^2 / (1 + grad_em^2)) # x = x1 +/- sqrt(d^2 / (1 + m^2))
  ey_2 <- ifelse(is.na(int_em), m[2] - dist, grad_em * ex_2 + int_em)
  # calculate the distances from e1 and e2 to c
  d_e1c <- sqrt((ex_1 - c[1])^2 + (ey_1 - c[2])^2)
  d_e2c <- sqrt((ex_2 - c[1])^2 + (ey_2 - c[2])^2)
  # e point is the one that is furthest from c
  if (d_e1c >= d_e2c) {
    e <- c(ex_1, ey_1)
  } else {
    e <- c(ex_2, ey_2)
  }
}

# Function for calculating the intersection of two lines
compute_intersect <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
  c(((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) / ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)),
    ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) / ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)))
}

# Function for calculating the Euclidean distance between two points
d <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

# Function for creating Steiner tree on three points
make_steiner3 <- function(a, b, c) {
  ab <- sqrt((a[1] - b[1])^2 + (a[2] - b[2])^2)
  ac <- sqrt((a[1] - c[1])^2 + (a[2] - c[2])^2)
  bc <- sqrt((c[1] - b[1])^2 + (c[2] - b[2])^2)
  angle_bac <- acos((ab^2 + ac^2 - bc^2) / (2 * ab * ac)) * 180 / pi
  angle_abc <- acos((ab^2 + bc^2 - ac^2) / (2 * ab * bc)) * 180 / pi
  angle_acb <- acos((ac^2 + bc^2 - ab^2) / (2 * ac * bc)) * 180 / pi
  # find Steiner point
  if(angle_bac >= 120) {
    s <- a
    valid <- 0
  } else if (angle_abc >= 120) {
    s <- b
    valid <- 0
  } else if (angle_acb >= 120) {
    s <- c
    valid <- 0
  } else {
    e1 <- compute_e(a, b, c)
    e2 <- compute_e(c, a, b)
    s <- compute_intersect(e1[1], e1[2], c[1], c[2], e2[1], e2[2], b[1], b[2])
    valid <- 1
  }
  edges <- data.frame(x =    c(a[1], b[1], c[1]),
                      y =    c(a[2], b[2], c[2]),
                      xend = c(s[1], s[1], s[1]),
                      yend = c(s[2], s[2], s[2]))
  return(edges)
}

# Plotting theme
theme_blankcanvas <- theme(
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  legend.position = "none",
  panel.background = element_rect(fill = bg_col, colour = bg_col),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  plot.margin = unit(c(0, 0, -2, -1), "mm"), # top, right, bottom, left
  strip.background = element_blank(),
  strip.text = element_blank()
)

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
  if(steiner) {
    letter <- make_steiner3(c(control_pts$x[3 * (i - 1) + 1], control_pts$y[3 * (i - 1) + 1]),
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
y_pos <- canvas_height - margin_top
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
  if(script_index > length(script)) {
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
  if(y_pos - line_spacing - letter_height < margin_bottom) {
    stop_writing <- TRUE
  }
  frame_id <- frame_id + 1
}

# Centre vertically
if(centre_vertically == TRUE) {
  offset = (max(text$y) - min(text$y)) / 2 - canvas_height / 2
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

# temp <- text %>% group_by(frame) %>% summarise(maxx = max(x)) %>% mutate(size = runif(nrow(.)))
# text <- text %>% left_join(temp)
# text2 <- text %>% mutate(prop = runif(nrow(.)), x = x + prop * letter_width / 3,
#                          xend = xend + prop * letter_width / 3) %>% select(-prop)
# 
# text <- text %>% mutate(frame2 = 1)
# text2 <- text2 %>% mutate(frame2 = 2)
# 
# df <- list(text, text2)
# 
# tf <- tween_states(df, tweenlength = 1.5, statelength = 0,
#                    ease = "linear",
#                    nframes = 25)

# Make plot ----
nudge <- 250
text2 <- text %>% mutate(delta1 = runif(nrow(.), -nudge, nudge), x = x + delta1,
                         delta2 = runif(nrow(.), -nudge, nudge), y = y + delta2,
                         delta3 = runif(nrow(.), -nudge, nudge), xend = xend + delta3,
                         delta4 = runif(nrow(.), -nudge, nudge), yend = yend + delta4) %>%
  select(-delta1, -delta2, -delta3, -delta4)
#text3 <- text %>% mutate(delta = runif(nrow(.), -nudge, nudge), xend = xend + delta) %>% select(-delta)
#text4 <- text %>% mutate(delta = runif(nrow(.), -nudge, nudge), y = y + delta) %>% select(-delta)
#text5 <- text %>% mutate(delta = runif(nrow(.), -nudge, nudge), yend = yend + delta) %>% select(-delta)

df <- list(text, text2)

tf <- tween_states(df, tweenlength = 1.5, statelength = 0,
                   ease = "exponential-in",
                   nframes = 1000)

# Plot alphabet
p2 <- ggplot(alphabet) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), alphabet) +
  coord_equal() +
  facet_wrap(~letter_id) +
  theme_blankcanvas +
  theme(strip.text = element_text(size = 6))

p <- ggplot() +
  scale_x_continuous(limits = c(0, canvas_width), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, canvas_height), expand = c(0, 0)) +
  theme_blankcanvas

if(ruled_lines) {
  p <- p + geom_segment(aes(x, y, xend = xend, yend = yend), lines, colour = "black", alpha = 1,
                        size = 0.1)
}

if(cursive) {
  p <- p +
    geom_path(aes(x, y, group = paragraph_id, frame = frame, cumulative = TRUE), text, size = 0.5, colour = font_colour)
} else {
  p <- p +
    #geom_tile(aes(x = x, y = y, width = width, height = height), text %>% mutate(width = letter_height / 10, height = width), fill = font_colour)
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend, frame = frame, cumulative = FALSE),
                 tf %>% filter(letter_id != nrow(alphabet)),
                 colour = font_colour, lineend = "round", size = 0.2, alpha = 0.03) #+
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

# p <- p + coord_polar()

# Save plot ----
ggsave("asemic-40.png", p, width = 210, height = 210, units = "mm")
# ggsave("alphabet-01.png", p2, width = 210, height = 297, units = "mm")

# Save gif ----
#animation::ani.options(interval = 1/25)
#gganimate(p, "matrix-3.gif", title_frame = FALSE)

#save(alphabet, file = "alphabet.Rda")
#load("alphabet.Rda")
