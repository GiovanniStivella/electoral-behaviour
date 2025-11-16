library(lme4)
library(tidyr)
library(dplyr)
library(broom)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(tidyverse)
color_scheme_set("brightblue")

summary <- readRDS("~/Electoral Behaviour/RDS/MCMC_summary_seed123.rds")

summary_wide <- summary %>%
  select(1,2)%>%
  slice(5:nrow(summary))%>% #select the right number
  separate(variable,
           into = c("unit", "dimension"),
           sep = ",",
           extra = "merge",   # if there are >1 commas, everything after 1st goes into after_comma
           fill = "right") %>%
  pivot_wider(names_from = dimension,
              values_from = mean,
              names_prefix = "value_")

summary_cand <- summary_wide%>%slice(9168:9179)%>%select(c(1:3))

library(ggplot2)
library(ggrepel)

# costruisco il data.frame per il grafico (adatta se le dimensioni non corrispondono)
plot_df <- summary_cand %>%
  mutate(label = cand_long$candidate_name,
         candidate_number = cand_long$candidate_number,
         color = ifelse(candidate_number %% 2 == 1, "blue", "red"))

ggplot(plot_df, aes(x = `value_1]`, y = `value_2]`)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = label, colour = color),
                  size = 5,          # dimensione testo
                  box.padding = 0.3, # spazio attorno alle etichette
                  point.padding = 0.4,
                  max.overlaps = Inf) +
  scale_colour_identity() +
  labs(x = "Value 1", y = "Value 2") +
  theme_minimal(base_size = 14)