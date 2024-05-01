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
)

# Plot BSol map
map <- plot_map(
  ward_falls,
  value_header = "number_of_falls per 1000 Patients65Plus",
  map_type = "Ward",
  map_title = "Emergency hospital admissions for falls injuries in persons aged 65 and over per 1000 patients registered to a BSol GP (2022/23)"
)
map

save_map(map, save_name = "../output/BSol-falls-22-23.svg",
         width = 6, height = 6)