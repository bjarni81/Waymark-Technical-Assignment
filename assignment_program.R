# import libraries
library(tidyverse)
#list files in directory to stay tidy
waymark_files = list.files("/Users/bnbh_imac/Desktop/Bjarni/Waymark Technical Assignment/Input/",
                           pattern = ".csv",
                           include.dirs = TRUE,
                           full.names = TRUE)
#read-in data, get min/max dates by patient_id
patient_enrollment_span = read_csv(waymark_files[2]) %>%
  mutate(month_year = mdy(month_year)) %>%
  group_by(patient_id) %>%
  summarise(enrollment_start_date = min(month_year),
            enrollment_end_date = max(month_year))
#output .csv
write_csv(patient_enrollment_span,
          "/Users/bnbh_imac/Desktop/Bjarni/Waymark Technical Assignment/patient_enrollment_span.csv")
#count rows
nrow(patient_enrollment_span)
#Answer 1: there are 1,000 rows in this table
#----------
outpat_viz = patient_enrollment_span %>%
  left_join(., read_csv(waymark_files[1])) %>%
  mutate(date = mdy(date),
         #make a variable for identifying encounters outside enrollment
         uh_oh = if_else(date < enrollment_start_date | date > enrollment_end_date, 1, 0)) %>%
    #remove encounters outside enrollment, make sure to keep ids with no encounters
  filter(uh_oh == 0 | is.na(uh_oh)) %>%
  group_by(patient_id) %>%
  #get sum of visits, and count of distinct days by patient_id
  summarise(ct_outpatient_visits = sum(outpatient_visit_count),
            ct_days_with_outpatient_visits = n()) %>%
  #replace NA with 0 in sum, and replace 1 with 0 b/c n() is counting distinct patient_ids
  mutate(ct_outpatient_visits = replace_na(ct_outpatient_visits, 0),
         ct_days_with_outpatient_visits = if_else(ct_outpatient_visits == 0, 
                                                  0, 
                                                  ct_days_with_outpatient_visits))
#join with table from step 1
result = outpat_viz %>%
  left_join(., patient_enrollment_span,
            by = "patient_id") %>%
  select(patient_id, enrollment_start_date, enrollment_end_date, ct_outpatient_visits, ct_days_with_outpatient_visits)
#output
write_csv(result,
          "/Users/bnbh_imac/Desktop/Bjarni/Waymark Technical Assignment/result.csv")
#count distinct
n_distinct(result$ct_days_with_outpatient_visits)
#Answer 2: there are 29 distinct values of ct_days_with_outpatient_visits