library(ggplot2)

mirror_wing <- function(points, side) {
  data.frame(
    x = points$x * side,
    y = points$y
  )
}

# Wing outlines: forewing and hindwing
forewing <- data.frame(
  x = c(0.08, 0.25, 0.55, 0.92, 1.32, 1.62,
        1.53, 1.35, 1.05, 0.70, 0.35, 0.12),
  y = c(0.12, 0.42, 0.78, 1.12, 1.42, 1.52,
        1.13, 0.78, 0.48, 0.25, 0.10, -0.02)
)

hindwing <- data.frame(
  x = c(0.08, 0.32, 0.63, 0.96, 1.25, 1.42,
        1.38, 1.24, 1.08, 0.88, 0.63, 0.38,
        0.18, 0.10),
  y = c(0.03, 0.12, 0.25, 0.36, 0.30, 0.12,
        -0.18, -0.48, -0.70, -0.83, -0.86,
        -0.72, -0.43, -0.16)
)

# Create mirrored wing data
make_wings <- function(shape, name) {
  do.call(rbind, lapply(c(-1, 1), function(s) {
    d <- mirror_wing(shape, s)
    d$side <- s
    d$wing <- name
    d
  }))
}

fw <- make_wings(forewing, "fore")
hw <- make_wings(hindwing, "hind")

# Wing veins radiate from near the body
vein_ends <- data.frame(
  x = c(0.32, 0.58, 0.86, 1.15, 1.43,
        0.34, 0.63, 0.94, 1.22, 1.35),
  y = c(0.35, 0.63, 0.88, 1.15, 1.42,
        -0.20, -0.40, -0.58, -0.42, 0.02)
)

veins <- do.call(rbind, lapply(c(-1, 1), function(s) {
  data.frame(
    x = 0.08 * s,
    y = 0.02,
    xend = vein_ends$x * s,
    yend = vein_ends$y
  )
}))

# Small pale spots near the wing margins
spots <- data.frame(
  x = c(1.48, 1.40, 1.30, 1.18, 1.05,
        0.92, 0.80, 0.68, 0.56,
        1.30, 1.20, 1.08, 0.95, 0.82),
  y = c(1.31, 1.15, 0.99, 0.84, 0.71,
        0.60, 0.49, 0.39, 0.29,
        0.08, -0.16, -0.39, -0.58, -0.70)
)

spots <- do.call(rbind, lapply(c(-1, 1), function(s) {
  d <- spots
  d$x <- d$x * s
  d
}))

# Delicate antennae
antennae <- data.frame(
  x = c(0.035, -0.035),
  y = c(0.42, 0.42),
  xend = c(0.17, -0.17),
  yend = c(0.70, 0.70)
)

# Improved butterfly body and wing attachment

# Make the wing roots extend slightly beneath the body
fw_attached <- fw
fw_attached$x <- fw_attached$x * 1.04

hw_attached <- hw
hw_attached$x <- hw_attached$x * 1.04

# Body: head, thorax, and tapered abdomen
body <- data.frame(
  x = c(-0.055, -0.075, -0.065, -0.045, -0.035,
         0.035, 0.045, 0.065, 0.075, 0.055),
  y = c(0.34, 0.18, -0.08, -0.34, -0.55,
        -0.55, -0.34, -0.08, 0.18, 0.34)
)

# A wider thorax helps visually join all four wings
thorax <- data.frame(
  x = c(-0.09, -0.12, -0.10, 0.10, 0.12, 0.09),
  y = c(0.16, 0.02, -0.12, -0.12, 0.02, 0.16)
)

p <- ggplot() +
  # Draw wings first so the body sits naturally on top
  geom_polygon(
    data = hw_attached,
    aes(x = x, y = y, group = interaction(side, wing)),
    fill = "#174D91", color = "#102C4C", linewidth = 0.8
  ) +
  geom_polygon(
    data = fw_attached,
    aes(x = x, y = y, group = interaction(side, wing)),
    fill = "#2478C8", color = "#102C4C", linewidth = 0.8
  ) +
  # Soft light-blue wing centers
  geom_polygon(
    data = transform(fw_attached, x = x * 0.86, y = y * 0.86),
    aes(x = x, y = y, group = interaction(side, wing)),
    fill = "#8BD5F5", color = NA, alpha = 0.94
  ) +
  geom_polygon(
    data = transform(hw_attached, x = x * 0.84, y = y * 0.82),
    aes(x = x, y = y, group = interaction(side, wing)),
    fill = "#66BCEB", color = NA, alpha = 0.94
  ) +
  # Veins
  geom_segment(
    data = veins,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "#17416B", linewidth = 0.4, alpha = 0.8
  ) +
  # Wing-edge spots
  geom_point(
    data = spots,
    aes(x = x, y = y),
    shape = 21, size = 1.6,
    fill = "#E1F6FF", color = "#A4DDF8", stroke = 0.35
  ) +
  # Thorax and abdomen overlap the wing roots
  geom_polygon(
    data = thorax,
    aes(x = x, y = y),
    fill = "#172638", color = "#0B1928", linewidth = 0.5
  ) +
  geom_polygon(
    data = body,
    aes(x = x, y = y),
    fill = "#172638", color = "#0B1928", linewidth = 0.5
  ) +
  # Head
  geom_point(aes(x = 0, y = 0.39),
             size = 3.5, color = "#172638") +
  # Antennae
  geom_segment(
    data = antennae,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "#172638", linewidth = 0.9, lineend = "round"
  ) +
  coord_fixed(
    xlim = c(-1.85, 1.85),
    ylim = c(-1.1, 1.8),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F5FAFF", color = NA),
    panel.background = element_rect(fill = "#F5FAFF", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

print(p)

ggsave(
  "light_blue_morpho_v2.png",
  plot = p, width = 10, height = 8,
  dpi = 300, bg = "#F5FAFF"
)

cat("Saved as light_blue_morpho_v2.png\n")