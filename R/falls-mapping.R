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
  select(
    `Ward Code`, Ward, age_group, Observation
  )

services <- readxl::read_excel("data/service-data.xlsx",
                               sheet = "processed")

output_data <- list()

for (age_i in age_groups) {
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
      AgeGroup == age_i
    ) %>%
    left_join(
      read.csv(
        "data/West Midlands postcodes.csv",
        check.names=FALSE
      ) %>%
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
      N = sum(N)
    )
  
  # Filter census data
  ward_counts <- census_data %>%
    filter(
      age_group == age_i
    ) %>%
    select(
      `Ward Code`, Ward, Observation
    )

  ward_falls <- ward_counts %>%
    left_join(inpatient_data,
              by = join_by("Ward Code")) %>%
    replace(is.na(.), 0) %>%
    mutate(
      `Falls per 1000 residents` = N/Observation*1000,
      `Number of falls` = N,
      `Residents in age range` = Observation
    ) %>%
    select(
      c(`Ward Code`, Ward, `Residents in age range`,
        `Number of falls`, `Falls per 1000 residents`)
      )


  # Store age group rate to save later
  output_data[[age_i]] <- ward_falls

  ## Weighted falls ##

  title1 <- paste0(
    "Emergency hospital admissions for falls injuries in persons ",
    age_i,
    " per 1000 residents aged ",
    age_i,
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

  map1 <- add_points(
    map, services,
    color = "Service Type",
    size = 0.2
  )


  save_name1 <- paste(
    "output/",
    str_replace(FilterYear, "/", "-"), "/",
    str_replace_all(age_i, " ", "-"),
    "/Brum-falls-",
    str_replace(FilterYear, "/", "-"),
    "-age-",
    str_replace_all(age_i, " ", "-"),
    sep = ""
  )
  save_map(map1, save_name = paste(save_name1, ".png", sep = ""),
           width = 4.5, height = 6)

}

write_xlsx(output_data,
           paste0("output/brum-ward-falls-",
                  str_replace(FilterYear, "/", "-"),
                  ".xlsx")
           )
