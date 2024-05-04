# Falls mapping
library("dplyr")
library("ggplot2")

# Load census data
census_data <- readxl::read_excel(
  "../data/birmingham-all-ages.xlsx"
) %>%
  mutate(
    Age = `Age (101 categories) Code`,
    N = Observation
  ) %>%
  select(Age, N)

ane_data <- readxl::read_excel(
  "../data/falls-ane-data.xlsx",
  sheet = "age breakdown"
) %>%
  mutate(
    Age = AgeOnAdmission,
    n = N
  ) %>%
  select(Age, n)

fall_rate <- ane_data %>%
  left_join(census_data, by = "Age") %>%
  mutate(fall_rate = n/N*1000) %>%
  filter(Age<=100)

plt <- ggplot(fall_rate, aes(x = Age, y = fall_rate)) +
  geom_bar(stat = "identity", fill = "#105ca5") +
  theme_bw() +
  ylab(
    "Emergency hospital admissions for falls\ninjuries per 1000 residents (2022/23)"
    ) +
  scale_y_continuous(
      # Make y-axis percentages
      expand = c(0, 0), limits = c(0, 260)
    ) +
  scale_x_continuous(
    # Make y-axis percentages
    expand = c(0, 0), limits = c(-1, 101)
  )

plt

ggsave("../output/rate_by_age.png", plt, 
       width = 5, height = 3.5)