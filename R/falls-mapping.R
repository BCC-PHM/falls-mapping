# Falls mapping
library("dplyr")
library("BSol.mapR")
library("stringr")

# Define labels for age groups of interest
age_groups = c("under 65", "65 to 84", "85 and over")
year = "22/23"

palette <- ggpubr::get_palette(c("#FFFFFF", "#105ca5"), 20)

# Load census data
census_data <- readxl::read_excel(
  "../data/birmingham_ages_census.xlsx"
  ) %>% 
  mutate(
    Ward = gsub(" \\(Birmingham\\)", "", x = `Electoral wards and divisions`),
    `Ward Code` = `Electoral wards and divisions Code`,
    age_group = case_when(
      `Age (D) (3 categories)` == "Aged 64 years and under" ~ "under 65",
      `Age (D) (3 categories)` == "Aged 65 to 84 years" ~ "65 to 84",
      `Age (D) (3 categories)` == "Aged 85 years and over" ~ "85 and over",
      TRUE ~ "Error: Impossible age."
    )
    ) %>%
  select(
    `Ward Code`, Ward, age_group, Observation
  )

services <- readxl::read_excel("../data/service-data.xlsx", 
                               sheet = "processed")

for (age_i in age_groups) {
  # Load A&E data from ICB warehouse
  ane_data <- readxl::read_excel(
    paste0(
      "../data/falls-ane-data-", 
      str_replace(year, "/", "-"),
      ".xlsx" 
    ),
    sheet = age_i
  ) %>%
    left_join(
      read.csv(
        "../data/West Midlands postcodes.csv",
        check.names=FALSE
      ) %>% 
      # mutate() %>%
      group_by(`LSOA Code`) %>% 
      summarize (
        `Ward Code` = names(which.max(table(`Ward Code`)))
      ) 
    ) %>%
    group_by(`Ward Code`) %>%
    summarise(
      number_of_falls = sum(number_of_falls)
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
    left_join(ane_data,
              by = join_by("Ward Code")) %>%
    replace(is.na(.), -1) %>%
    mutate(
      `Falls per 1000 residents` = `number_of_falls`/Observation*1000,
      `Number of falls` = number_of_falls
    )
  
  ## Weighted falls ##
  
  title1 <- paste0(
    "Emergency hospital admissions for falls injuries in persons ",
    age_i,
    " per 1000 residents aged ",
    age_i,
    " (",year,")"
    )

  # Plot Birmingham map
  map <- plot_map(
    ward_falls,
    value_header = "Falls per 1000 residents",
    map_type = "Ward",
    area_name = "Birmingham",
    map_title  = title1,
    style = "cont",
    palette = palette
  )
  
  map1 <- add_points(
    map, services, 
    color = "Service Type",
    size = 0.2
  )
  

  save_name1 <- paste(
    "../output/", 
    str_replace(year, "/", "-"), "/",
    str_replace_all(age_i, " ", "-"),
    "/Brum-falls-23-24-age-", 
    str_replace_all(age_i, " ", "-"),
    sep = ""
  )
  save_map(map1, save_name = paste(save_name1, ".png", sep = ""),
           width = 4.5, height = 6)

  
  ## html version ##
  
  # Plot raw falls map
  map2 <- plot_map(
    ward_falls,
    value_header = "Falls per 1000 residents",
    map_type = "Ward",
    area_name = "Birmingham",
    fill_title  = stringr::str_replace_all(
      stringr::str_wrap(title1, 40), "\n", "</br>"
      ),
    style = "cont",
    palette = palette
  )
  map2 <- add_points(
    map2, services, 
    color = "Service Type",
    shape = "Provision",
    size = 0.2
  )
  
  save_map(map2, save_name = paste(save_name1, ".html", sep = ""),
           width = 4.5, height = 6)
  
}
