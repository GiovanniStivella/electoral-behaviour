library(lme4)
library(tidyr)
library(dplyr)
library(broom)
library(broom.mixed)
library(tigris)
library(ggplot2)

pa_voting_districts <- voting_districts("PA")

rmapshaper::ms_simplify(pa_voting_districts, keep=0.05, keep_shapes=TRUE)

summary <- readRDS("~/Electoral Behaviour/RDS/MCMC_summary_seed123.rds")

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

summary_prec <- summary_wide%>%slice(1:9167)%>%select(c(1:3))

plot(summary_prec$`value_1]`, summary_prec$`value_2]`)


#ADDING PRECINCT CODES
lets <- read.csv('educ_data.csv')

na_rows_share <- lets %>% filter(is.na(share_1))
dem <- lets%>%
  mutate (bach = rowSums(select(lets,55:58)))%>%
  filter(!GEOID20 %in% na_rows_share$GEOID20)%>%
  select(1, 22:29, last_col())%>%
  unique()%>%
  filter(complete.cases(.))

#COMPLETE TABLE
table <- cbind(dem, summary_prec)

super_table <- pa_voting_districts %>%
  left_join(table, by = c("GEOID20" = "GEOID20"))

ggplot(super_table) + 
  geom_sf(aes(fill = `value_1]`), linewidth = 0) + 
  theme_void() +
  scale_fill_gradient2(
    name = "first dimension",
    high = "#00008B",   # red
    low = "#8B0000",  #blue
    midpoint = 0,
    na.value = "grey"
  )

ggplot(super_table) + 
  geom_sf(aes(fill = `value_2]`), linewidth = 0) + 
  theme_void() +
  scale_fill_gradient2(
    name = "second dimension",
    high = "#8B8B00",
    low = "#68228B",
    midpoint = 0,
    na.value = "grey"
  )