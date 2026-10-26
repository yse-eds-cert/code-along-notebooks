# =============================================================================
# Recreating NYT Graph 6: "Global average temperature, compared with
# mid-20th century" (from "The Last 8 Years Were the Hottest on Record")
#
# Data: NASA GISS Surface Temperature Analysis (GISTEMP v4)
#       https://data.giss.nasa.gov/gistemp/
#
# GISTEMP anomalies are relative to 1951-1980, which is the
# "mid-20th century" baseline the NYT chart uses. No re-baselining needed.
# =============================================================================

library(tidyverse)

# -----------------------------------------------------------------------------
# 1. PULL THE DATA ------------------------------------------------------------
# -----------------------------------------------------------------------------
# This is the "Global-mean monthly, seasonal, and annual means" CSV
# (Land-Ocean Temperature Index) linked from the GISTEMP data page.
#
# Two quirks worth pointing out to students:
#   * Line 1 is a title ("Land-Ocean: Global Means"), not the header -> skip = 1
#   * Missing values are coded as "***" (and sometimes "****"), not blank
#     -> na = c("***", "****")
#     (Without this, any column containing "***" would be read in as character!)

url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv"

raw <- read_csv(url, skip = 1, na = c("***", "****"))

# If the download fails (firewall, etc.), download the CSV by hand and use:
# raw <- read_csv("GLB.Ts+dSST.csv", skip = 1, na = c("***", "****"))

glimpse(raw)

# -----------------------------------------------------------------------------
# 2. CLEAN THE DATA -----------------------------------------------------------
# -----------------------------------------------------------------------------
# We only need the year and the annual mean. In GISTEMP, the annual
# (January-December) mean lives in a column called "J-D". The hyphen makes
# it a non-syntactic name, so we need backticks to reference it.

temps <- raw %>%
  select(Year, `J-D`) %>%                    # keep only the two columns we need
  rename(year = Year, anomaly = `J-D`) %>%   # give them friendlier names
  drop_na(anomaly) %>%                       # drop years without a full annual average
  mutate(
    # Step 1: blue if below average, light orange if above
    category = ifelse(anomaly >= 0, "warm", "cool"),
    # Step 2: overwrite 2015-2022 as dark orange
    category = ifelse(year >= 2015 & year <= 2022, "last8", category),
    # Step 3: overwrite the years since the NYT article (2023 on) as dark red
    category = ifelse(year > 2022 & year <= 2026, "last4", category)
  )

temps

# Pull out the final year's value so we can label it (instead of hardcoding)
last_yr <- temps %>% slice_max(year)

last_yr

# Quick sanity check: NYT reports 2022 as +0.89 C.
# (NASA revises GISTEMP periodically, so your value may differ by ~0.01.)
temps %>% filter(year == 2022)

# -----------------------------------------------------------------------------
# 3. PLOT ---------------------------------------------------------------------
# -----------------------------------------------------------------------------

nyt_colors <- c(
  cool   = "#a3d4e4",
  warm   = "#f6c182",
  last8  = "#ee8539",
  last4  = "#d73027"
)

# Reference levels. In the NYT chart these are WHITE lines drawn on top of the
# bars, so they're only visible where they cut through a bar.
ref_lines <- c(-0.25, 0.25, 0.50, 0.75)

# Hand-placed labels for the reference lines (x = where the label ends)
ref_labels <- data.frame(
  my_x     = c(1938, 1976, 1986, 2005),
  my_y     = c(-0.25, 0.25, 0.50, 0.75),    # same as the reference lines
  my_label = c("-0.25°C", "+0.25°C", "+0.50°C", "+0.75°C"),
  my_hjust = c(0, 1, 1, 1),                 # way to adjust horizontal placement
  my_vjust = c(0.5, -0.4, -0.4, -0.4)       # way to adjust vertical placement
)


# -----------------------------------------------------------------------------
# My Version
# -----------------------------------------------------------------------------
ggplot(temps, aes(x = year, y = anomaly)) +

  # I want a bar chart, each bar is 1 year, and the fill color of each bar is temps$category
  geom_col(aes(fill = category), width = 1) +

  # White reference lines on top of the bars
  geom_hline(yintercept = ref_lines, color = "white", linewidth = 0.5) +

  # Gray zero (baseline) line
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.5) +

  # Reference line labels
  geom_text(
    data = ref_labels,
    aes(x = my_x, y = my_y, label = my_label, hjust = my_hjust, vjust = my_vjust),
    color = "grey45", size = 4.2, inherit.aes = FALSE
  ) +

  # Add title, subtitle, and caption
  labs(
    title = "Global average temperature",
    subtitle = "Compared with mid-20th century",
    caption = "Source: NASA Goddard Institute for Space Studies",
    x = NULL,
    y = NULL
  ) +

  # Change fill colors and get rid of the legend
  scale_fill_manual(values = nyt_colors, guide = "none") +

  # Label x-axis ticks every 20 years
  scale_x_continuous(breaks = seq(1880, 2020, by = 20)) +

  # Start from a blank canvas and add back only what the NYT shows (x-axis year labels)
  theme_minimal(base_size = 13) +
  theme(
    panel.grid    = element_blank(),     # remove all gridlines
    axis.text.y   = element_blank(),     # remove y-axis numbers
    plot.title    = element_text(face = "bold", vjust = -40),
    plot.subtitle = element_text(vjust = -47),
    plot.caption  = element_text(color = "grey50", hjust = 0)
  )
