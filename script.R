library(tidyverse)

complaints <- read_csv(
  URLencode("https://data.cityofnewyork.us/resource/ygpa-z7cr.csv?$limit=150000&$where=received_date >= '2026-01-20T00:00:00'"
))

heat_hotwater <- complaints %>% 
  filter(major_category == "HEAT/HOT WATER")


new_status <- heat_hotwater %>% 
  mutate(new_status = case_when(
    str_detect(status_description, "The Department of Housing Preservation and Development was not able to gain access") ~ "No access",
    str_detect(status_description, "HPD attempted to conduct an inspection in response to this complaint, but was unable to complete the inspection") ~ "No inspection (no reason)",
    str_detect(status_description, "The conditions observed by the inspector did not violate the housing laws enforced by HPD")~ "No violation",
    str_detect(status_description, "HPD called the telephone number on file for this complaint.  Someone at that number indicated that the condition was corrected") ~ "Tenant no issue (phone call)",
    str_detect(status_description, "An occupant of the building confirmed heat and hot water had been restored when the Inspector attempted to conduct the inspection")~"Tenant no issue (in person)",
    str_detect(status_description, "The Department of Housing Preservation and Development contacted a tenant in the building and verified that the following conditions were corrected")~"Tenant no issue (ambiguous)",
    str_detect(status_description, "Violations were issued") ~ "Violation issued",
    str_detect(status_description, "Violations were previously issued for these conditions.") ~ "Violation already issued",
    str_detect(status_description, "The Department of Housing Preservation and Development inspected the following conditions. No violations were issued.") ~ "No violation issued",
    str_detect(status_description, "Heat was not required at the time of the inspection") ~ "Heat not required",
    str_detect(status_description, "The following complaint conditions are still open.") ~ "Still open",
    str_detect(status_description, "More than one complaint was received for this building-wide condition. This complaint status is for the initial complaint") ~ "Other complaint",
  )) %>%
  arrange(building_id, desc(received_date))

new_status %>% count(new_status, complaint_status) %>% print(n=Inf)

unsorted <- new_status %>% filter(is.na(new_status))

