# -------------------------------------------------------------------------
# Penguin Project Analysis
# -------------------------------------------------------------------------

# 1. Setup and Project Structure
# -------------------------------------------------------------------------

# Load required packages
library(tidyverse)
library(janitor)
library(palmerpenguins)

# 2. Data Import
# -------------------------------------------------------------------------
# TODO: Fix later, synthetic test
set.seed(42)
n <- 344
penguins_raw <- tibble(
    species = sample(c("Adelie", "Chinstrap", "Gentoo"), n, replace = TRUE),
    island = sample(c("Torgersen", "Biscoe", "Dream"), n, replace = TRUE),
    bill_length_mm = rnorm(n, mean = 44, sd = 5),
    bill_depth_mm = rnorm(n, mean = 17, sd = 2),
    flipper_length_mm = rnorm(n, mean = 200, sd = 14),
    body_mass_g = rnorm(n, mean = 4200, sd = 800),
    sex = sample(c("male", "female"), n, replace = TRUE),
    year = sample(2007:2009, n, replace = TRUE)
)

# 3. Data Cleaning: Handling Missing Values
# -------------------------------------------------------------------------

# Inspect data for missing values
nrow(penguins_raw)

# Remove rows with missing values
penguins_raw_dropped <- drop_na(penguins_raw)
nrow(penguins_raw_dropped)

# 4. Data Cleaning Pipeline
# -------------------------------------------------------------------------

# Import raw data (reset)
# Using generated data from above for now
penguins_clean <- penguins_raw %>%
    clean_names() %>%
    drop_na() %>%
    filter(species != "Chinstrap") # Removing outlier species for cleaner visualisation

# Print summary statistics for each species
penguins_clean %>%
    group_by(species) %>%
    summarise(
        n = n(),
        mean_bill_length = mean(bill_length_mm),
        sd_bill_length = sd(bill_length_mm),
        mean_flipper_length = mean(flipper_length_mm),
        sd_flipper_length = sd(flipper_length_mm),
        mean_body_mass = mean(body_mass_g),
        .groups = "drop"
    ) %>%
    print()

# Save cleaned data
write.csv(penguins_clean, "data/penguins_cleaned.csv")

# 5. Exploratory Data Visualisation
# -------------------------------------------------------------------------

# Initial boxplot visualisation
ggplot(penguins_clean, aes(x = species, y = bill_length_mm)) +
    geom_boxplot()

# Adding individual data points (jitter)
ggplot(penguins_clean, aes(x = species, y = bill_length_mm)) +
    geom_boxplot() +
    geom_point(position = position_jitter())


# Refining plot aesthetics (transparency, colour)
ggplot(penguins_clean, aes(x = species, y = bill_length_mm)) +
    geom_boxplot(
        width = 0.2,
        outlier.shape = NA,
        alpha = 0.1,
        colour = "grey30"
    ) +
    geom_point(
        size = 2,
        alpha = 0.4,
        position = position_jitter(width = 0.18, seed = 123)
    )

# Customising colour palettes
species_colours <- c(
    "Adelie" = "darkorange",
    "Chinstrap" = "purple",
    "Gentoo" = "cyan4"
)

ggplot(
    penguins_clean,
    aes(x = species, y = bill_length_mm, colour = species)
) +
    geom_boxplot(
        width = 0.2, outlier.shape = NA, alpha = 0.1, colour = "grey30"
    ) +
    geom_point(
        size = 2,
        alpha = 0.4,
        position = position_jitter(width = 0.18, seed = 123)
    ) +
    scale_colour_manual(values = species_colours)

# Finalising plot labels and themes
ggplot(
    penguins_clean,
    aes(x = species, y = bill_length_mm, colour = species)
) +
    geom_boxplot(
        width = 0.2, outlier.shape = NA, alpha = 0.1, colour = "grey30"
    ) +
    geom_point(
        size = 2,
        alpha = 0.4,
        position = position_jitter(width = 0.18, seed = 123)
    ) +
    scale_colour_manual(values = species_colours) +
    labs(
        title = "Bill length across penguin species",
        subtitle = "Boxplots summarise central tendencies; jittered points represent individual data",
        x = "Species",
        y = "Bill length (mm)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
        axis.title = element_text(size = 12),
        legend.position = "none",
        panel.grid.major.x = element_blank()
    )

# 6. Modular Programming
# -------------------------------------------------------------------------

# Import custom plotting functions
source("functions/plotting_functions.R")
source("functions/saving_functions.R")

# Apply custom boxplot function to different variables
# Plot Bill Length
boxplot_bill_length <- plot_penguin_boxplot(
    penguins_clean,
    species,
    bill_length_mm,
    y_label = "Bill Length (mm)",
    title_text = "Distribution of Bill Length"
)

# Plot Flipper Length
boxplot_flipper_length <- plot_penguin_boxplot(
    penguins_clean,
    species,
    flipper_length_mm,
    y_label = "Flipper Length (mm)",
    title_text = "Distribution of Flipper Length"
)

# 7. Statistical Analysis
# -------------------------------------------------------------------------

# Perform correlation tests between bill length and flipper length per species
penguins_clean %>%
    group_by(species) %>%
    summarise(
        correlation = cor(bill_length_mm, flipper_length_mm),
        p_value = cor.test(bill_length_mm, flipper_length_mm)$p.value,
        significance = ifelse(p_value < 0.01, "***", ifelse(p_value < 0.1, "*", "ns")),
        .groups = "drop"
    ) %>%
    print()

# 8. Advanced Visualisation Techniques
# -------------------------------------------------------------------------

# Visualising data clusters with convex hulls
# Note: Requires updated plotting_functions.R (sourced above)
cluster_plot <- plot_clusters(
    penguins_clean,
    bill_length_mm,
    flipper_length_mm,
    species,
    title = "Morphological Clustering of Penguin Species",
    subtitle = "95% confidence ellipses for bill length vs flipper length",
    x_label = "Bill Length (mm)",
    y_label = "Flipper Length (mm)"
)

# Visualising linear regression trends by group
regression_plot <- plot_regression(
    penguins_clean,
    bill_length_mm,
    flipper_length_mm,
    species,
    title = "Relationship between Bill Length and Flipper Length",
    subtitle = "Linear regression models fitted independently for each species",
    x_label = "Bill Length (mm)",
    y_label = "Flipper Length (mm)"
)

# 9. Report Generation
# -------------------------------------------------------------------------

# Combine plots into a multi-panel figure
library(patchwork)

final_grid <-
    (boxplot_bill_length | boxplot_flipper_length) /
    (cluster_plot | regression_plot) +
    plot_annotation(
        title = "Comparative Morphological Analysis of Palmer Penguins",
        subtitle = "Assessment of body dimension variability and inter-species relationships",
        caption = "Data source: Gorman et al. (2014) via palmerpenguins R package",
        theme = theme(
            plot.title = element_text(size = 20, face = "bold"),
            plot.subtitle = element_text(size = 16, face = "italic"),
            plot.caption = element_text(size = 12)
        )
    )

print(final_grid)

# Export figures for various formats (Report, Presentation, Poster)
save_penguin_plot_png(final_grid, "penguin_report.png", size_option = "report")
save_penguin_plot_png(final_grid, "penguin_presentation.png", size_option = "presentation")
save_penguin_plot_png(final_grid, "penguin_poster.png", size_option = "poster")

# Save individual plots if needed
save_penguin_plot_png(regression_plot, "regression_plot_poster.png", size_option = "single_poster")
save_penguin_plot_svg(final_grid, "penguin_poster.svg", size_option = "poster")
