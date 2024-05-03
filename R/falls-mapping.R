# Falls mapping
library("dplyr")
library("BSol.mapR")

# Load A&E data from ICB warehouse
ane_data <- readxl::read_excel(
  "../data/falls-ane-data.xlsx"
  ) %>%
  left_join(
    read.csv(
      "../data/West Midlands postcodes.csv",
      check.names=FALSE
    ) %>% 
      mutate() %>%
      group_by(`LSOA Code`) %>% 
      summarize (
        `Ward Code` = names(which.max(table(`Ward Code`)))
      ) 
  ) %>%
  group_by(`Ward Code`) %>%
  summarise(
    number_of_falls = sum(number_of_falls)
  )

# Load Birmingham Census data
ward_65plus_counts <- readxl::read_excel(
  "../data/Birmingham_65plus_census.xlsx"
  ) %>%
  filter(
    `Age (3 categories)` >= "Aged 65 years and over"
  ) %>%
  select(
    `Ward Code`, Ward, Observation
  )
  
ward_falls <- ward_65plus_counts %>%
  left_join(ane_data,
             by = join_by("Ward Code")) %>%
  replace(is.na(.), -1) %>%
  mutate(
    `Falls per 1000 patients aged 65+` = `number_of_falls`/Observation*1000,
    `Number of falls` = number_of_falls
  )

palette <- ggpubr::get_palette((c("#FFFFFF", "#105ca5")), 20)
# Plot BSol map
map <- plot_map(
  ward_falls,
  value_header = "Falls per 1000 patients aged 65+",
  map_type = "Ward",
  area_name = "Birmingham",
  map_title = "Emergency hospital admissions for falls injuries in persons aged 65 and over per 1000 residents (2022/23)",
  style = "cont",
  palette = palette,
  breaks = c(0, 10, 20, 30, 40)
)
map

save_map(map, save_name = "../output/BSol-falls-22-23-v2.png",
         width = 4.5, height = 6)
save_map(map, save_name = "../output/BSol-falls-22-23-v2.html",
         width = 4.5, height = 6)


# Plot BSol map
raw_falls_map <- plot_map(
  ward_falls,
  value_header = "Number of falls",
  map_type = "Ward",
  area_name = "Birmingham",
  map_title = "Emergency hospital admissions for falls injuries in persons aged 65 and over (2022/23)",
  style = "cont",
  palette = palette,
  breaks = c(0, 50, 100, 150)
)
raw_falls_map

save_map(raw_falls_map, save_name = "../output/BSol-falls-raw-22-23-v2.png",
         width = 4.5, height = 6)
save_map(raw_falls_map, save_name = "../output/BSol-falls-raw-22-23-v2.html",
         width = 4.5, height = 6)