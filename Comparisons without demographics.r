library(tidyr)
library(dplyr)
library(broom)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(lme4)
color_scheme_set("brightblue")

#CONSTRUCTION OF THE NEEDED MATRICES

lets <- read.csv('educ_data.csv')

#MATRIX OF ELECTORAL RESULTS
g <- lets%>%
  mutate(election = paste(lets[[13]], lets[[14]]))%>%
  select(1, last_col(), 32)

#pivot wider
h <- g %>%
  pivot_wider(names_from = election, values_from = share_1)%>%
  select(-c(1))

h_clean <- h %>% drop_na()

#h_clean transposed
h_clean_t <- as.data.frame(t(h_clean))


#VECTOR OF CANDIDATES
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

cand_a <- cand %>%
  left_join(cand_long, by = c("candidate_name_1" = "candidate_name")) %>%
  select(last_col())

cand_b <- cand %>%
  left_join(cand_long, by = c("candidate_name_2" = "candidate_name")) %>%
  select(last_col())

dime <- cand_long %>% select(2)

m <- as.matrix(unname(h_clean)) - 0.5

n <-m[1:100,]

#Bayesian estimation with fixed ideology of candidates

fixed <- file.path("Fixed candidate ideology.stan")
prec <- cmdstan_model(fixed)

listfix <- list(
  T = ncol(h_clean),
  C = 12,
  G = nrow(h_clean),
  share = as.matrix(unname(m)),
  cand_a = as.numeric(unlist(cand_a)),
  cand_b = as.numeric(unlist(cand_b)),
  dime = as.numeric(unlist(dime))
)

fix <- prec$sample(
  data = listfix,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)


summary_fix <- as.data.frame(fix$summary())

write.csv(summary_fix, "summary_fixed_ideology.csv")


#Taking a linear model without demographic covariates
na_rows_prec <- lets %>% filter(is.na(share_1))
lets_clean <- lets %>% filter(!GEOID20 %in% na_rows_prec$GEOID20)

line <- lets_clean %>% 
  mutate(y = (share_1-1/2)/diff_comp)

comp <- lmer(y ~ (1|GEOID20),
               data = line)

ranef_comp <- ranef(comp)$GEOID20 %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("GEOID20") %>% 
  rename(ranef_simple = `(Intercept)`)

write.csv(ranef_comp, "ranef_comp.csv")