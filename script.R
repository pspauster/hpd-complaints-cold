library(tidyverse)

complaints <- read_csv(
  URLencode(
    "https://data.cityofnewyork.us/resource/ygpa-z7cr.csv?$limit=350000&$where=received_date >= '2026-01-25T00:00:00' AND received_date <= '2026-02-02T00:00:00'"
  )
)

heat_hotwater <- complaints %>% 
  filter(major_category == "HEAT/HOT WATER")


##CATEGORIES##
#contacted tenant
#contacted via phone call
#contacted  in person
#Violation issued + violation already issued
#No access
#Inspection but no violation
#Still open
#Other


new_status <- heat_hotwater %>% 
  mutate(new_status = case_when(
    str_detect(status_description, "The Department of Housing Preservation and Development was not able to gain access") ~ "No access",
    str_detect(status_description, "HPD attempted to conduct an inspection in response to this complaint, but was unable to complete the inspection") ~ "Other",
    str_detect(status_description, "The conditions observed by the inspector did not violate the housing laws enforced by HPD")~ "No violation",
    str_detect(status_description, "HPD called the telephone number on file for this complaint.  Someone at that number indicated that the condition was corrected") ~ "Contacted via phone call",
    str_detect(status_description, "An occupant of the building confirmed heat and hot water had been restored when the Inspector attempted to conduct the inspection")~"Contacted in person",
    str_detect(status_description, "The Department of Housing Preservation and Development contacted a tenant in the building and verified that the following conditions were corrected")~"Contacted tenant",
    str_detect(status_description, "Violations were issued") ~ "Violation issued",
    str_detect(status_description, "Violations were previously issued for these conditions.") ~ "Violation already issued",
    str_detect(status_description, "The Department of Housing Preservation and Development inspected the following conditions. No violations were issued.") ~ "No violation",
    str_detect(status_description, "Heat was not required at the time of the inspection") ~ "No violation",
    str_detect(status_description, "The following complaint conditions are still open.") ~ "Still open",
    #str_detect(status_description, "More than one complaint was received for this building-wide condition. This complaint status is for the initial complaint") ~ "Other complaint",
    #should all get a status that is not their duplicate status and is inherited from the main status
    ),
  duplicate = str_detect(status_description, "More than one complaint was received for this building-wide condition."),
  apt = if_else(unit_type=="BUILDING-WIDE", FALSE, TRUE)) %>%
  arrange(building_id, desc(received_date))

new_status %>% count(new_status, complaint_status) %>% print(n=Inf)
new_status %>% count(new_status, problem_duplicate_flag) %>% print(n=Inf)

new_status %>% count(duplicate, problem_duplicate_flag)

new_status %>% filter(problem_duplicate_flag=="Y") %>% count(new_status)

new_status %>% count(duplicate, apt)

unsorted <- new_status %>% filter(is.na(new_status))

bldg_count <- new_status %>% group_by(building_id) %>% summarize(count = n())

summary(bldg_count)

count(bldg_count, count)

resolution_sum <- new_status %>% 
  mutate(source = if_else(problem_duplicate_flag=="Y", "Tagged as duplicate", "Primary complaint"),
         from = 1,
         to = 2,
         dest = new_status) %>% 
  group_by(source, dest, from, to) %>% 
  summarize(value = n())

dup_sum <- new_status %>% 
  mutate(source = "Complaint",
         from = 0,
         to = 1,
         dest = if_else(problem_duplicate_flag=="Y", "Tagged as duplicate", "Primary complaint")) %>% 
  group_by(source, dest, from, to) %>% 
  summarize(value = n())

bind_rows(dup_sum, resolution_sum) %>% write_csv("flowchart.csv")
