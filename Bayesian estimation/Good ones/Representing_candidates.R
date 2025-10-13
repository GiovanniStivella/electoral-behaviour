library(lme4)
library(tidyr)
library(dplyr)
library(broom)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(tidyverse)
color_scheme_set("brightblue")

summary <- readRDS("~/Electoral Behaviour/RDS/MCMC_summary_seed456.rds")

summary_wide <- summary %>%
  select(1,2)%>%
  slice(3:nrow(summary))%>% #select the right number
  separate(variable,
           into = c("unit", "dimension"),
           sep = ",",
           extra = "merge",   # if there are >1 commas, everything after 1st goes into after_comma
           fill = "right") %>%
  pivot_wider(names_from = dimension,
              values_from = mean,
              names_prefix = "value_")

summary_cand <- summary_wide%>%slice(9168:9179)%>%select(c(1:3))

plot(summary_cand$`value_1]`, summary_cand$`value_2]`)


#ADDING NAMES AND COLOURS
lets <- read.csv('educ_data.csv')
dat <- lets%>%
  mutate(election = paste(lets[[13]], lets[[14]]))%>%
  select(1, 16, 18, 19, 20, last_col())

cand <- dat[1:7,] %>% select(2:5)

cand_long <- cand %>%
  pivot_longer(cols = everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.*)_(.*)") %>%
  select(-set)%>%
  unique()%>%
  mutate(candidate_number = 1:12)

colors <- ifelse(cand_long$candidate_number %% 2 == 1, "blue", "red")

text(
  summary_cand$`value_1]`,
  summary_cand$`value_2]`,
  labels = cand_long$candidate_name,
  pos = 4,
  cex = 0.8,
  col = colors
)