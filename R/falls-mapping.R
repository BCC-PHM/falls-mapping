# Falls mapping
library("dplyr")
library("BSol.mapR")

# Load A&E data from ICB warehouse
ane_data <- readxl::read_excel(
  "../data/falls-ane-data.xlsx"
  )

# Load GP patient data and calculate patients 65+ for each practice
path <- "//SVWCCG111/PublicHealth$/2.0 KNOWLEDGE EVIDENCE & GOVERNANCE - KEG/2.12 PHM AND RESEARCH/Data/Primary Care/"
gp_65plus_counts <- readxl::read_excel(
  paste( path, "BSOL GP Population List - Sept 2023.xlsx", sep =""),
  sheet = "Dataset"
  ) %>%
  filter(
    ProxyAgeAtEOM >= 65
  ) %>%
  group_by(GP_Code) %>%
  summarize(
    Patients65Plus = sum(Count)
  )
  
GP_falls <- gp_65plus_counts %>%
  left_join(ane_data,
             by = c("GP_Code" = "GMPOrganisationCode")) %>%
  replace(is.na(.), 0) 

# Convert GP data to ward-level
ward_falls <- convert_GP_data(
  data = GP_falls,
  GP_code_header = "GP_Code",
  value_header = "number_of_falls",
  norm_header = "Patients65Plus",
  norm_output_per = 1000
) %>%
  mutate(
    `Falls per 1000 patients aged 65+` = `number_of_falls per 1000 Patients65Plus`,
    `Number of falls` = number_of_falls
  )

palette <- ggpubr::get_palette((c("#FFFFFF", "#105ca5")), 20)
# Plot BSol map
map <- plot_map(
  ward_falls,
  value_header = "Falls per 1000 patients aged 65+",
  map_type = "Ward",
  area_name = "Birmingham",
  map_title = "Emergency hospital admissions for falls injuries in persons aged 65 and over per 1000 patients registered to a BSol GP (2022/23)",
  style = "cont",
  breaks = c(0, 10, 20, 30),
  palette = palette
)
map

save_map(map, save_name = "../output/BSol-falls-22-23.png",
         width = 4.5, height = 6)
save_map(map, save_name = "../output/BSol-falls-22-23.html",
         width = 4.5, height = 6)


# Plot BSol map
raw_falls_map <- plot_map(
  ward_falls,
  value_header = "Number of falls",
  map_type = "Ward",
  area_name = "Birmingham",
  map_title = "Emergency hospital admissions for falls injuries in persons aged 65 and over (2022/23)",
  style = "cont",
  #breaks = c(0, 10, 20, 30),
  palette = palette
)
raw_falls_map

save_map(raw_falls_map, save_name = "../output/BSol-falls-raw-22-23.png",
         width = 4.5, height = 6)
save_map(raw_falls_map, save_name = "../output/BSol-falls-raw-22-23.html",
         width = 4.5, height = 6)