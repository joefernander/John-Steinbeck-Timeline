# John Steinbeck Timeline
# High-quality composite timeline with inspiration image

# Load packages
library(ggplot2)
library(ggtext)
library(magick)
library(grid)
library(dplyr)

# Steinbeck life data
steinbeck_life <- tibble::tribble(
  ~event,                       ~date,        ~year, ~type,
  "Born",                       "1902-02-27", 1902,  "birth",
  "Married Carol Henning",      "1930-06-01", 1930,  "personal",
  "Wrote 'Of Mice and Men'",      "1937-06-01", 1937,  "major_work",
  "Wrote 'The Grapes of Wrath'",  "1939-01-01", 1939,  "major_work",
  "Published 'The Moon Is Down'", "1942-01-01", 1942,  "major_work",
  "Published 'Cannery Row'",     "1945-01-01", 1945,  "major_work",
  "Won Pulitzer Prize",         "1946-01-01", 1946,  "award",
  "Published 'East of Eden'",      "1952-01-01", 1952,  "major_work",
  "Died",                       "1980-12-26", 1980,  "death"
) |>
  mutate(
    date = as.Date(date),
    y_pos = reorder(event, desc(year)),
    tooltip_text = paste0(
      "Event: ", event, "\n",
      "Year: ", year, "\n",
      "Type: ", tools::toTitleCase(type)
    )
  )

# Colors extracted from inspiration image
img_bg         <- "#FDFDFD"
img_sage       <- "#7C918F"
img_warm_brown <- "#A88E72"
img_dark_brown <- "#685F50"
img_charcoal   <- "#342315"
img_tan        <- "#C8BDAA"

# Event palette
steinbeck_palette_image <- c(
  "birth"      = img_warm_brown,
  "major_work" = img_sage,
  "award"      = img_dark_brown,
  "personal"   = img_tan,
  "death"      = img_charcoal
)

# Subtitle text
steinbeck_subtitle_longer <- paste(
  "Born in Salinas, California, in 1902, John Steinbeck grew up among the ranch hands,",
  "migrant workers, and coastal towns that would shape his imagination. He gave voice",
  "to the dispossessed during the Great Depression, blending intimate human stories",
  "with a sharp social conscience. From *The Grapes of Wrath* to *Of Mice and Men*, his",
  "work captured the struggles of working people with deep empathy, moral clarity,",
  "and an enduring literary power. A relentless observer of American life, he traveled",
  "with displaced families, chronicled labor camps, and bore witness to both cruelty",
  "and grace. His fiction and nonfiction earned him the Nobel Prize in Literature in 1962,",
  "securing his place as one of the defining writers of the twentieth century.",
  sep = "\n"
)

# Theme
steinbeck_theme_double_space <- function() {
  theme_void() +
    theme(
      plot.title = element_text(
        family = "serif", face = "bold", size = 40, color = img_charcoal,
        hjust = 0.5, vjust = 0, margin = margin(b = 56)
      ),
      plot.subtitle = element_textbox_simple(
        family = "serif", size = 18, color = img_dark_brown,
        halign = 0, lineheight = 1.5,
        margin = margin(b = 96)
      ),
      plot.caption = element_text(
        family = "sans", size = 13, color = img_dark_brown,
        hjust = 0, margin = margin(t = 40)
      ),
      axis.title = element_text(
        family = "sans", face = "bold", size = 17, color = img_charcoal,
        margin = margin(t = 24)
      ),
      axis.text.y = element_text(
        family = "sans", face = "bold", size = 16, color = img_charcoal,
        hjust = 1, margin = margin(r = 8)
      ),
      axis.text.x = element_text(
        family = "sans", face = "bold", size = 16, color = img_charcoal,
        margin = margin(t = 8)
      ),
      legend.position = "none",
      plot.background = element_rect(fill = img_bg, color = NA),
      panel.background = element_rect(fill = img_bg, color = NA),
      plot.title.position = "plot",
      plot.margin = margin(80, 35, 40, 21) # Reduced right margin by ~30%
    )
}

# Timeline plot
timeline_plot_double_space <- ggplot(
  steinbeck_life,
  aes(x = year, y = y_pos)
) +
  geom_segment(
    aes(x = 1900, xend = year, y = y_pos, yend = y_pos),
    color = img_tan,
    linewidth = 0.8,
    alpha = 0.8
  ) +
  geom_point(
    aes(fill = type),
    size = 9,
    stroke = 1.3,
    color = img_charcoal,
    shape = 21
  ) +
  geom_text(
    aes(label = year, color = type),
    hjust = -0.5, size = 5.5, family = "sans", fontface = "bold"
  ) +
  scale_fill_manual(values = steinbeck_palette_image) +
  scale_color_manual(values = steinbeck_palette_image) +
  scale_x_continuous(limits = c(1900, 1995), breaks = seq(1900, 1990, by = 10), expand = c(0, 0)) +
  labs(
    title = "John Steinbeck",
    subtitle = steinbeck_subtitle_longer,
    x = "Year",
    y = NULL,
    caption = "Timeline of birth, major works, awards, and personal milestones"
  ) +
  steinbeck_theme_double_space()

# Save high-resolution timeline
timeline_double_space_file <- here::here("steinbeck_timeline_hq.png")
ggsave(
  timeline_double_space_file,
  plot = timeline_plot_double_space,
  width = 12,
  height = 15,
  dpi = 300,
  bg = img_bg
)

# Paths for composite
inspiration_image <- "https://i.pinimg.com/736x/fc/94/62/fc9462d491978d46a1bb85f650946074.jpg"
final_file <- here::here("steinbeck_final_hq.png")

# Download inspiration image if not already present
inspiration_path <- here::here("steinbeck_inspiration.jpg")
if (!file.exists(inspiration_path)) {
  download.file(inspiration_image, inspiration_path, mode = "wb")
}

# Composite image + timeline
img_orig      <- image_read(inspiration_path)
timeline_orig <- image_read(timeline_double_space_file)

final_width  <- 3600
final_height <- 1900

img_resized      <- image_resize(img_orig, paste0("x", final_height))
img_w            <- image_info(img_resized)$width
timeline_resized <- image_resize(timeline_orig, paste0("x", final_height))
timeline_w       <- image_info(timeline_resized)$width

if (img_w + timeline_w + 60 > final_width) {
  available_timeline_w <- final_width - img_w - 60
  scale_factor         <- available_timeline_w / timeline_w
  timeline_resized     <- image_resize(
    timeline_resized,
    paste0(round(image_info(timeline_resized)$width * scale_factor), "x", final_height)
  )
  timeline_w <- image_info(timeline_resized)$width
}

canvas <- image_blank(width = final_width, height = final_height, color = img_bg)
canvas <- image_composite(canvas, img_resized, gravity = "northwest", offset = "+0+0")
canvas <- image_composite(
  canvas,
  timeline_resized,
  gravity = "northwest",
  offset = paste0("+", img_w + 21, "+0") # Reduced spacing by ~30%
)

image_write(canvas, path = final_file, format = "png")

message("Saved: ", final_file)

