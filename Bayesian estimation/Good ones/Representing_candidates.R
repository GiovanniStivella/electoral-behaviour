library(lme4)
library(tidyr)
library(dplyr)
library(broom)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(tidyverse)
color_scheme_set("brightblue")

#summary <-readRDS()

summary_wide <- summary %>%
  select(1,2)%>%
  slice(3:nrow(summary))%>%
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

colors <- ifelse(cand_long$candidate_number %% 2 == 1, "blue", "red")

text(
  summary_cand$`value_1]`,
  summary_cand$`value_2]`,
  labels = cand_long$candidate_name,
  pos = 4,
  cex = 0.8,
  col = colors
)