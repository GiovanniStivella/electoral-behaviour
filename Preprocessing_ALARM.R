library(tidyr)
library(dplyr)

# Load the Pennsylvania 2020 VTD data
pennsylvania <- read.table("https://raw.githubusercontent.com/alarm-redist/census-2020/main/census-vest-2020/pa_2020_vtd.csv",
                    header = TRUE,    # first row of file contains variable names
                    sep = ",",        # need to specify that the file is comma-separator
                    dec = ".")        # useless here (as it is the default), can be useful with Italian settings (dec = ",")

#Rename the columns that contain the election data (columns from 23 to 36)
pennsylvania <- pennsylvania %>%
  rename_with(~ paste0("election_", names(pennsylvania)[23:36]), 23:36)

#Pivot the data to long format, so that each row indicates a different election
pennsylvania_long <- pennsylvania %>%
  pivot_longer(cols = starts_with("election"), 
               names_to = "election", 
               values_to = "votes") %>%
  mutate(election = gsub("election_", "", election)) %>% 
  separate(election, into = c("election_type", "election_year", "candidate"), sep = "_", extra = "merge")

pennsylvania_long <- pennsylvania_long %>%
  group_by(GEOID20, election_type, election_year) %>% 
  mutate(candidate_num = row_number()) %>% 
  pivot_wider(names_from = candidate_num, values_from = c(candidate, votes), names_sep = "_") %>%
  ungroup()

pennsylvania_long <- pennsylvania_long %>%
  select(1:4, 23:26, 5:22, 27:ncol(pennsylvania_long))


pennsylvania_long <- pennsylvania_long %>%
  mutate(candidate_name_1 = case_when(
    candidate_1 == "dem_cli" ~ "clinton",
    candidate_1 == "dem_mcg" ~ "mcginty",
    candidate_1 == "dem_sha" ~ "shapiro",
    candidate_1 == "dem_cas" ~ "casey",
    candidate_1 == "dem_wol" ~ "wolf",
    candidate_1 == "dem_bid" ~ "biden"
  ),
  candidate_name_2 = case_when(
    candidate_2 == "rep_tru" ~ "trump",
    candidate_2 == "rep_too" ~ "toomey",
    candidate_2 == "rep_raf" ~ "rafferty",
    candidate_2 == "rep_bar" ~ "barletta",
    candidate_2 == "rep_wag" ~ "wagner",
    candidate_2 == "rep_hei" ~ "heidelbaugh"
  )) %>%
  relocate(candidate_1, candidate_name_1, candidate_2, candidate_name_2, .after = election_year)

#Save this
write.csv(pennsylvania_long, "pennsylvania_long.csv", row.names = FALSE)
