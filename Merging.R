library(tidyr)
library(dplyr)
library(broom)

#Open DIME data and read it as a dataframe
dime_pa_candidates <- read.csv("dime_pa_candidates.csv")

#Drop columns "bonica.rid", "bonica.cid", "name", "ffname", "fname", "mname", "title", "suffix", "party", "state", "district", "distcyc", "fec.cand.status", "recipient.type"
dime_pa <- dime_pa_candidates %>%
  select(-c(bonica.rid, bonica.cid, name, ffname, fname, mname, title, suffix, party, state, district, distcyc, fec.cand.status, recipient.type))

#I want to eliminate some duplicates: apparently, same candidates appear more than once with different values for some columns
#Drop all columns from "num.givers" to the last column
dime_pa <- dime_pa %>%
  select(-c(num.givers:last_col()))

#Drop columns from "election" to "ico.status" excluding "lname"
dime_pa <- dime_pa %>%
  select(-c(election:fecyear),-c(seat:ico.status))

#Get average values for each candidate for columns from recipient.cfscore to composite.score
#For other columns, keep the first non-missing value
dime_pa_a <- dime_pa %>%
  group_by(lname) %>%
  reframe(across(-c(recipient.cfscore:composite.score)), 
    across(recipient.cfscore:composite.score, mean, na.rm = TRUE))

#Eliminate duplicates
dime_pa_a <- dime_pa_a %>%
  distinct()

#Open ALARM data and read it as a dataframe
alarm <- read.csv("pennsylvania_long.csv")

merged_data <- alarm %>%
    left_join(dime_pa_a, by = c("candidate_name_1" = "lname")) %>%
    rename_with(
        ~paste0(.x, "_candidate_1"),
        c("cand.gender","recipient.cfscore","recipient.cfscore.dyn","contributor.cfscore","dwdime", "composite.score", "dwnom1", "dwnom2", "ps.dwnom1", "ps.dwnom2", "irt.cfscore")
    )

merged_data <- merged_data %>%
  left_join(dime_pa_a, by = c("candidate_name_2" = "lname")) %>%
  rename_with(
    ~paste0(.x, "_candidate_2"),
    c("cand.gender","recipient.cfscore","recipient.cfscore.dyn","contributor.cfscore","dwdime", "composite.score", "dwnom1", "dwnom2", "ps.dwnom1", "ps.dwnom2", "irt.cfscore")
  )

#Drop the columns whose name start with: "cand.gender", "recipient.cfscore", "recipient.cfscore.dyn", "contributor.cfscore", "dwnom1", "dwnom2", "ps.dwnom1", "ps.dwnom2", "irt.cfscore"
simplified_data <- merged_data %>%
  select(-starts_with("cand.gender"), -starts_with("recipient.cfscore"), -starts_with("recipient.cfscore.dyn"), -starts_with("contributor.cfscore"), -starts_with("dwnom1"), -starts_with("dwnom2"), -starts_with("ps.dwnom1"), -starts_with("ps.dwnom2"), -starts_with("irt.cfscore")) 

#Drop the columns that start with "pop"
simplified_data <- simplified_data %>%
  select(-starts_with("pop"))

#Divide the values in the columns that start with "vap" by the value in the column "vap"
simplified_data <- simplified_data %>%
  mutate(across(starts_with("vap"), ~ .x / vap, .names = "per_{.col}"))

#Create a column "diff_dime" whose value is "dwdime_candidate_1"-"dwdime_candidate_2"
simplified_data <- simplified_data %>%
  mutate(diff_dime = dwdime_candidate_1 - dwdime_candidate_2)

#Create a column "diff_comp" whose value is "composite.score_candidate_1"-"composite.score_candidate_2"
simplified_data <- simplified_data %>%
  mutate(diff_comp = composite.score_candidate_1 - composite.score_candidate_2)

#Sum the values in columns "votes_1" and "votes_2" and then divide the values in columns "votes_1" and "votes_2" by this sum; new columns will be called "share_1" and "share_2"

simplified_data <- simplified_data %>%
  mutate(votes_sum = votes_1 + votes_2,
         share_1 = votes_1 / votes_sum,
         share_2 = votes_2 / votes_sum) %>%
  select(-votes_sum)

#Drop columns from "vap" to "vap_two"
simplified_data <- simplified_data %>%
  select(-c(vap:vap_two))

#Drop columns from "votes_1" to "composite.score_candidate_2"
simplified_data <- simplified_data %>%
  select(-c(votes_1:dwdime_candidate_1, dwdime_candidate_2))

write.csv(simplified_data, "simplified_data.csv", row.names = FALSE)

#We can now add other variables
educ <-read.table("/Users/giovannistivella/Documents/Università/SSSUP/PSU/Data/Pennsylvania (going forward)/final_wide.csv",
                  header = TRUE,    # first row of file contains variable names
                  sep = ",",        # need to specify that the file is comma-separator
                  dec = ".")        # useless here (as it is the default), can be useful with Italian settings (dec = ",")

educ_data <- simplified_data %>%
             left_join(educ, by = c("GEOID20" = "VTDST20GEOID"))

#Drop column X
educ_data <- educ_data %>%
  select(-X)

#Divide values in columns that start by B15003 by the value in column "B15003_001"
educ_data <- educ_data %>%
  mutate(across(starts_with("B15003"), ~ .x / B15003_001, .names = "per_{.col}"))%>%
  select(-starts_with("B15003"))

write.csv(educ_data, "educ_data.csv", row.names = FALSE)