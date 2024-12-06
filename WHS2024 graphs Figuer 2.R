install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("lubricate")
install.packages("shiny")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)


# replicate Figure 1.2 in WHS 2024 chapter 1.

# Read the CSV file
data <- read.csv("LE_HALE.csv")

# Aggregate data to calculate global trends, keep male and female and certain years
global_trends <- data %>%
  filter(region !="World" & year %in% c(2000,2010, 2019, 2020, 2021) & sex %in% c(1,2))

# Separate male and female data for plotting
global_trends$sex <- ifelse(global_trends$sex == 1, "Male", "Female")

# Prepare the dataset for stacked bars
plot_data <- global_trends %>%
  mutate(
    Healthy_LE = HALE,
    Unhealthy_LE = LE - HALE
        ) %>%
  select(region, year, sex,Healthy_LE, Unhealthy_LE) %>%
  pivot_longer(cols = c("Healthy_LE", "Unhealthy_LE"), names_to = "Category", values_to = "Value") %>%
  # Reorder the 'Category' factor to make 'Unhealthy_LE' come first, and male com first
  mutate(Category = factor(Category, levels = c("Unhealthy_LE", "Healthy_LE"))) %>%
  mutate(sex = factor(sex, levels = c("Male", "Female")))

# Define color palette
colors <- c("Healthy_LE.Male" = "green", 
            "Unhealthy_LE.Male" = "lightgreen", 
            "Healthy_LE.Female" = "orange", 
            "Unhealthy_LE.Female" = "peachpuff")

# Create the plot with separate Male and Female bars, and stack Healthy_LE and Unhealthy_LE
ggplot(data = plot_data, aes(x = factor(sex), y = Value, fill = interaction(Category,sex))) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +  # Stack Healthy_LE and Unhealthy_LE
  facet_grid(region~year, scales = "free_y") + 
  scale_fill_manual(values = colors) +
  labs(
    x = " ",
    y = "Life Expectancy At Birth (Years)",
    fill = "",
    title = "Figure 1.1 Global trends in life expectancy and HALE at birth, by sex, 2000–2021"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.05),
    plot.title = element_text(hjust = 0.5),
  )
