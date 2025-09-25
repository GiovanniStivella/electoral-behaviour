library(lme4)
library(tidyr)
library(dplyr)
library(broom)
library(broom.mixed)
library(tigris)
library(ggplot2)

pa_voting_districts <- voting_districts("PA")

rmapshaper::ms_simplify(pa_voting_districts, keep=0.05, keep_shapes=TRUE)

prep <- read.csv('/Users/giovannistivella/Documents/Università/SSSUP/PSU/Data/Pennsylvania (going forward)/simplified_data.csv')

educ <- read.csv('/Users/giovannistivella/Documents/Università/SSSUP/PSU/Data/Pennsylvania (going forward)/educ_data.csv')

super_educ <- pa_voting_districts %>%
  left_join(educ, by = c("GEOID20" = "GEOID20"))

mean(super_educ$per_B15003_022+super_educ$per_B15003_023+super_educ$per_B15003_024+super_educ$per_B15003_025, na.rm = TRUE)
median(super_educ$per_B15003_022+super_educ$per_B15003_023+super_educ$per_B15003_024+super_educ$per_B15003_025, na.rm = TRUE)

ggplot(super_educ) + 
  geom_sf(aes(fill = per_B15003_022 + per_B15003_023 + per_B15003_024 + per_B15003_025), linewidth = 0) + 
  theme_void() +
  scale_fill_gradient2(
    name = "people with bachelor's degree",
    high = "#00008B",   # red
    low = "#8B0000",  #blue
    midpoint = 0.27,
    na.value = "grey"
  )

prep <- prep %>% 
  mutate(y = (share_1-1/2)/diff_comp)

simple <- lmer(y ~ per_vap_hisp+per_vap_white+per_vap_black+per_vap_aian+per_vap_asian+per_vap_nhpi+per_vap_other+per_vap_two+(1|GEOID20),
                    data = prep)

# Show random effects for each GEOID20
ranef_simple <- ranef(simple)$GEOID20 %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("GEOID20") %>% 
  rename(ranef_simple = `(Intercept)`)

prep_simple <- prep%>%left_join(ranef_simple, by = c("GEOID20"="GEOID20"))

pa_simple_ranef <- pa_voting_districts %>%
  left_join(prep_simple, by = c("GEOID20" = "GEOID20"))

ggplot(pa_simple_ranef) + 
  geom_sf(aes(fill = ranef_simple), linewidth = 0) + 
  theme_void() +
  scale_fill_gradient2(
    name = "ideological_estimate",
    high = "#8B0000",   # red
    low = "#00008B",  #blue
    na.value = "grey"
  )

ggplot(pa_simple_ranef) + 
  geom_sf(aes(fill = share_1), linewidth = 0) + 
  theme_void() +
  scale_fill_gradient2(
    name = "dem share",
    high = "#00008B",   # red
    low = "#8B0000",  #blue
    midpoint = 0.5,
    na.value = "grey"
  )

pa_huntingdon <- pa_voting_districts %>%
  left_join(prep, by = c("GEOID20" = "GEOID20")) %>%
  filter(COUNTYFP20 == "061")

ggplot(pa_huntingdon) + 
  geom_sf(aes(fill = ranef_simple), linewidth = 0) + 
  theme_void() +
  scale_fill_gradient2(
    name = "ideological_estimate",
    high = "#8B0000",   # red
    low = "#00008B",  #blue
    na.value = "grey"
  )

#Let's add year random effects: there have been years where one party had more favorable environment

year <- lmer(y ~ (1|election_year)+(1|GEOID20)+per_vap_hisp+per_vap_white+per_vap_black+per_vap_aian+per_vap_asian+per_vap_nhpi+per_vap_other+per_vap_two, 
                  data = prep)

ranef_year_y <- ranef(year)$election_year %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("Election_year") %>% 
  rename(ranef_year = `(Intercept)`)

ggplot(ranef_year_y, aes(x = Election_year, y = ranef_year)) +
  geom_col() +
  labs(x = "Election Year", y = "Random Effect", title = "Random Effects by Year") +
  theme_minimal()

ranef_year_GEO <- ranef(year)$GEOID20 %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("GEOID20") %>% 
  rename(ranef = `(Intercept)`)

hist(ranef_year_GEO$`ranef`, main = "Distribution of GEOID20 Random Effects", xlab = "Random Effect")

prep_year <- prep%>%left_join(ranef_year_GEO, by = c("GEOID20"="GEOID20"))

pa_year_ranef <- pa_voting_districts %>%
  left_join(prep_year, by = c("GEOID20" = "GEOID20"))

ggplot(pa_year_ranef) + 
  geom_sf(aes(fill = ranef_simple), linewidth = 0) + 
  theme_void() +
  scale_fill_gradient2(
    name = "ideological_estimate",
    high = "#8B0000",   # red
    low = "#00008B",  #blue
    na.value = "grey"
  )

#Let's add election random effects: there have been elections that have favored one party over the other
#Election random effects may be more interesting: they may account for characteristics of candidates that are not described by their ideological score

prep_election <- prep%>%mutate(election = paste(election_year, election_type, sep="_"))

election <- lmer(y ~ (1|election)+(1|GEOID20)+per_vap_hisp+per_vap_white+per_vap_black+per_vap_aian+per_vap_asian+per_vap_nhpi+per_vap_other+per_vap_two, 
                 data = prep_election)

ranef_election_y <- ranef(election)$election %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("Election") %>% 
  rename(ranef_election = `(Intercept)`)

ggplot(ranef_election_y, aes(x = Election, y = ranef_election)) +
  geom_col() +
  labs(x = "Election", y = "Random Effect", title = "Random Effects by Election") +
  theme_minimal()