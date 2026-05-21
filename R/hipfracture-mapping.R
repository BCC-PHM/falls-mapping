# Hip fractures mapping
library(dplyr)
library(BSol.mapR)
library(stringr)
library(writexl)
library(ggplot2)
source("R/config.R")

# Define labels for age groups of interest
age_groups = c("Under 65", "65-84", "85+")
FilterYear = "2025/26"

palette <- ggpubr::get_palette(c("#FFFFFF", "#105ca5"), 20)

# Load census data
census_data <- readxl::read_excel(
  "data/birmingham_ages_census.xlsx"
  ) %>%
  mutate(
    Ward = gsub(" \\(Birmingham\\)", "", x = `Electoral wards and divisions`),
    `Ward Code` = `Electoral wards and divisions Code`,
    age_group = case_when(
      `Age (D) (3 categories)` == "Aged 64 years and under" ~ "Under 65",
      `Age (D) (3 categories)` == "Aged 65 to 84 years" ~ "65-84",
      `Age (D) (3 categories)` == "Aged 85 years and over" ~ "85+",
      TRUE ~ "Error: Impossible age."
    )
    ) %>%
  filter(age_group %in% c("65-84", "85+")) %>%
  group_by(
    `Ward Code`, Ward
    ) %>%
  summarise(
     Pop65Plus = sum(Observation),
     .groups = "drop"
  )

output_data <- list()

# Load A&E data from ICB warehouse
inpatient_data <- readxl::read_excel(
  file.path(
    data_path,
    "BSol-hipfractures-2122to2526.xlsx"
  ),
  sheet = "LSOA21"
) %>% 
  filter(
    FinancialYear == FilterYear,
    AgeGroup %in% c("65-84", "85+")
  )  %>%
  left_join(
    read.csv(
      "data/West Midlands postcodes.csv",
      check.names=FALSE
    ) %>%
      # mutate() %>%
      group_by(`LSOA Code`) %>%
      summarize (
        `Ward Code` = names(which.max(table(`Ward Code`)))
      ),
    by = join_by("LSOA21" == "LSOA Code")
  ) %>%
  group_by(
    `Ward Code`
  ) %>%
  summarise(
    number_of_fractures = sum(N)
  )

ward_fractures <- census_data %>%
  left_join(inpatient_data,
            by = join_by("Ward Code")) %>%
  replace(is.na(.), 0) %>%
  mutate(
    magnitude = 1000,
    Z = qnorm(0.975),
    p_hat = number_of_fractures/Pop65Plus,
    fractures_per_1000_residents = p_hat*magnitude,
    number_of_fractures = number_of_fractures,
    LowerCI95 = magnitude * (p_hat + Z^2/(2*Pop65Plus) - Z * sqrt((p_hat*(1-p_hat)/Pop65Plus) + Z^2/(4*Pop65Plus^2))) / (1 + Z^2/Pop65Plus),
    UpperCI95 = magnitude * (p_hat + Z^2/(2*Pop65Plus) + Z * sqrt((p_hat*(1-p_hat)/Pop65Plus) + Z^2/(4*Pop65Plus^2))) / (1 + Z^2/Pop65Plus)
  ) %>%
  select(
    c(`Ward Code`, Ward, Pop65Plus,
      number_of_fractures, fractures_per_1000_residents, 
      LowerCI95, UpperCI95)
    ) %>%
  arrange(fractures_per_1000_residents)

brum_average <- ward_fractures %>%
  ungroup() %>%
  summarise(
    number_of_fractures = sum(number_of_fractures),
    Pop65Plus = sum(Pop65Plus)
  ) %>% 
  mutate(
    magnitude = 1000,
    Z = qnorm(0.975),
    p_hat = number_of_fractures/Pop65Plus,
    fractures_per_1000_residents = p_hat*magnitude,
    number_of_fractures = number_of_fractures,
    LowerCI95 = magnitude * (p_hat + Z^2/(2*Pop65Plus) - Z * sqrt((p_hat*(1-p_hat)/Pop65Plus) + Z^2/(4*Pop65Plus^2))) / (1 + Z^2/Pop65Plus),
    UpperCI95 = magnitude * (p_hat + Z^2/(2*Pop65Plus) + Z * sqrt((p_hat*(1-p_hat)/Pop65Plus) + Z^2/(4*Pop65Plus^2))) / (1 + Z^2/Pop65Plus),
    Ward = "Birmingham",
    `Ward Code` = NA
  )%>%
  select(
    c(`Ward Code`, Ward, Pop65Plus,
      number_of_fractures, fractures_per_1000_residents, 
      LowerCI95, UpperCI95)
  )

# Save data
write_xlsx(
  rbind(brum_average, ward_fractures), 
  "output/2025-26/fractures/fractures-inpatient-data-25-26.xlsx"
  )

## Weighted fractures ##

title1 <- paste0(
  "Emergency hospital admissions for hip fractures in persons aged 65+ ",
  " per 1000 residents",
  " (", FilterYear,")"
  )

# Plot Birmingham map
map <- plot_map(
  ward_fractures,
  value_header = "fractures_per_1000_residents",
  map_type = "Ward",
  area_name = "Birmingham",
  map_title  = title1,
  style = "cont",
  palette = palette,
  textNA = NA
)

save_map(map, save_name = "output/2025-26/fractures/brum-fractures-map-25-26.png",
         width = 4.5, height = 6)


# Bar plot
ward_fractures %>%
  mutate(
    Ward = factor(Ward, levels=Ward),
    significance = case_when(
      LowerCI95 > brum_average$UpperCI95 ~ "Above average",
      UpperCI95 < brum_average$LowerCI95 ~ "Below average",
      TRUE ~ "No significant difference"
    ),
    significance = factor(
      significance, 
      levels = c("Below average",
                 "No significant difference",
                 "Above average"))
  ) %>%
  ggplot(aes(y = Ward, x = fractures_per_1000_residents, fill = significance)) +
  geom_col() +
  geom_errorbar(aes(xmin = LowerCI95, xmax = UpperCI95)) +
  theme_bw() +
  theme(
    legend.position = "top"
  ) +
  scale_fill_manual(
    breaks = c("Below average", "No significant difference", "Above average"),
    values = c("#105ca5", "darkgray", "lightblue")
  ) +
  scale_x_continuous(
    limits = c(0, 35),
    expand = c(0, 0)
  ) +
  geom_rect(
    inherit.aes = FALSE,
    data = brum_average[1, ],
    aes(xmin = LowerCI95,
        xmax = UpperCI95),
    ymin = -Inf,
    ymax = Inf,
    fill = "#7b439a",
    alpha = 0.3,
    lwd = 0.7,
    #color = "black",
    linetype = "dotted"
  )+
  geom_vline(
    aes(color = "Birmingham average",
        xintercept = brum_average$fractures_per_1000_residents),
    lwd = 1
  ) +
  labs(
    fill = "",
    x = stringr::str_wrap(title1, 60),
    y = "",
    color = ""
  ) +
  scale_color_manual(values = c("#7b439a"))
ggsave("output/2025-26/fractures/brum-fractures-25-26.png",
       width = 8, height = 10)