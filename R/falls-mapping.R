# Falls mapping
library("dplyr")
library("BSol.mapR")
library("stringr")
library("writexl")
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
    `Ward Code`, Ward, age_group
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
    "BSol-falls-2122to2526.xlsx"
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
    number_of_falls = sum(N)
  )

ward_falls <- census_data %>%
  left_join(inpatient_data,
            by = join_by("Ward Code")) %>%
  replace(is.na(.), 0) %>%
  mutate(
    `Falls per 1000 residents` = number_of_falls/Pop65Plus*1000,
    `Number of falls` = number_of_falls,
    `Residents in age range` = Pop65Plus
  ) %>%
  select(
    c(`Ward Code`, Ward, `Residents in age range`,
      `Number of falls`, `Falls per 1000 residents`)
    )

## Weighted falls ##

title1 <- paste0(
  "Emergency hospital admissions for falls injuries in persons aged 65 + ",
  " per 1000 residents",
  " (", FilterYear,")"
  )

# Plot Birmingham map
map <- plot_map(
  ward_falls,
  value_header = "Falls per 1000 residents",
  map_type = "Ward",
  area_name = "Birmingham",
  map_title  = title1,
  style = "cont",
  palette = palette,
  textNA = NA
)

save_map(map, save_name = "output/2025-26/brum-falls-25-26.png",
         width = 4.5, height = 6)

write_xlsx(ward_falls, "output/2025-26/falls-inpatient-data-25-26.xlsx")
