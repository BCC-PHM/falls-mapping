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

ane_data_2223 <- readxl::read_excel(
  "../data/falls-ane-data-22-23.xlsx",
  sheet = "age breakdown"
) %>%
  mutate(
    Age = AgeOnAdmission,
    n = N,
    year = "2022/23"
  ) %>%
  select(year, Age, n)

ane_data_2324 <- readxl::read_excel(
  "../data/falls-ane-data-23-24.xlsx",
  sheet = "age breakdown"
) %>%
  mutate(
    Age = AgeOnAdmission,
    n = N,
    year = "2023/24"
  ) %>%
  select(year, Age, n)



fall_rate <- rbind(
  ane_data_2223,
  ane_data_2324
) %>%
  left_join(
    census_data, 
    by = "Age",
    relationship = "many-to-one"
    ) %>%
  filter(Age< 100) %>%
  mutate(
    Age_group = case_when(
      Age < 10 ~ "1-9",
      Age < 20 ~ "10-19",
      Age < 30 ~ "20-29",
      Age < 40 ~ "30-39",
      Age < 50 ~ "40-49",
      Age < 60 ~ "50-59",
      Age < 70 ~ "60-69",
      Age < 80 ~ "70-79",
      Age < 90 ~ "80-89",
      Age < 100 ~ "90-99"
    )
  ) %>%
  group_by(year, Age_group) %>%
  summarize(
    n = sum(n),
    N = sum(N)
  ) %>%
  mutate(
    p_hat = n / N,
    magnitude = 1000,
    fall_rate = magnitude * p_hat,
    # for use in Byar's method
    a_prime = n + 1,
    # Calculate errors
    Z = qnorm(0.975),
    LowerCI95 = magnitude * n * (1 - 1/(9*n) - Z/3 * sqrt(1/a_prime))**3/N,
    UpperCI95 = magnitude * a_prime * (1 - 1/(9*a_prime) + Z/3 * sqrt(1/a_prime))**3/N
    )

plt <- ggplot(fall_rate, aes(x = Age_group, y = fall_rate, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(
    aes(x = Age_group, ymin = LowerCI95, ymax = UpperCI95),
    position=position_dodge(.9),
    width = 0.4
  ) + 
  theme_bw() +
  ylab(
    "Emergency hospital admissions for falls\ninjuries per 1000 residents"
    ) +
  scale_y_continuous(
      # Make y-axis percentages
      expand = c(0, 0), limits = c(0, 200)
    ) +
  labs(
    fill = "",
    x = "Age Group"
  )

plt

ggsave("../output/rate_by_age.png", plt, 
       width = 6, height = 3)