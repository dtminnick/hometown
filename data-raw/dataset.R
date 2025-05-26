
packages <- c("curl", "dplyr", "ggplot2", "tidycensus", "tidyr")

installed_packages <- packages %in% rownames(installed.packages())

if(any(installed_packages == FALSE)) {
  
  install.packages(packages[!installed_packages])
  
}

invisible(lapply(packages, library, character.only = TRUE))

census_api_key("2b35978c7be3d9e149207a8e2ee9386b1a82b37d", 
               install = TRUE, 
               overwrite = TRUE)

years <- 2013:2022

place_fips <- "42079"

get_survey_data <- function(year) {
  
  vars <- c(total_pop = "B01003_001",
            median_age = "B01002_001",
            education = "B15003")
  
  total <- get_acs(geography = "county", 
                   variables = vars[1:2], 
                   year = year, 
                   state = "PA", 
                   county = "Luzerne", 
                   survey = "acs5") %>%
    select(variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate)
  
  edu_raw <- get_acs(geography = "county", 
                     table = vars[3], 
                     year = year, 
                     state = "PA", 
                     county = "Luzerne", 
                     survey = "acs5")
  
  bachelors_plus <- edu_raw %>%
    filter(variable %in% c("B15003_022", 
                           "B15003_023", 
                           "B15003_024", 
                           "B15003_025")) %>%
    summarise(bach_or_higher = sum(estimate))
  
  total_25plus <- edu_raw %>%
    filter(variable == "B15003_001") %>%
    pull(estimate)
  
  edu_pct <- (bachelors_plus$bach_or_higher / total_25plus) * 100
  
  total$education_pct <- edu_pct
  
  total$year <- year
  
  return(total)
  
}

survey_data <- lapply(years, get_survey_data) %>% bind_rows()

survey_data <- survey_data %>%
  select(year, median_age, total_pop, education_pct)

saveRDS(survey_data, file = "./data/survey_data.rds")

