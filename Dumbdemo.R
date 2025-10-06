library(lme4)
library(tidyr)
library(dplyr)
library(broom)
library(cmdstanr)
library(posterior)
library(bayesplot)
color_scheme_set("brightblue")

#CONSTRUCTION OF THE NEEDED MATRICES

lets <- read.csv('educ_data.csv')

na_rows_share <- lets %>% filter(is.na(share_1))
dem <- lets%>%
  mutate (bach = rowSums(select(lets,55:58)))%>%
  filter(!GEOID20 %in% na_rows_share$GEOID20)%>%
  select(1, 22:29, last_col())%>%
  unique()%>%
  filter(complete.cases(.))

#MATRIX OF ELECTORAL RESULTS
g <- lets%>%
  mutate(election = paste(lets[[13]], lets[[14]]))%>%
  select(1, last_col(), 32)

#pivot wider
h <- g %>%
  pivot_wider(names_from = election, values_from = share_1, id_cols = GEOID20)

h_clean <- h %>% drop_na() %>% filter(GEOID20 %in% dem$GEOID20) %>% select(-c(1))

#h_clean transposed
h_clean_t <- as.data.frame(t(h_clean))

#VECTOR OF CANDIDATES
dat <- lets%>%
  mutate(election = paste(lets[[13]], lets[[14]]))%>%
  select(1, 16, 18, 19, 20, last_col())%>%
  mutate(diff = composite.score_candidate_1-composite.score_candidate_2)

dime_diff <- dat[1:7,] %>% select(7)

#DEMOGRAPHIC DATA
dem1 <- dem%>%select(-c(1, 4, 5, 6, 7, 8, 9))

m <- as.matrix(unname(h_clean)) - 0.5

n <-m[1:100,]

r <-dem1[1:100,]

#ESTIMATION
dumb <- file.path("~/Electoral Behaviour/Bayesian estimation/Dumbdemo.stan")
numb <- cmdstan_model(dumb)

listerine <- list(
  T = ncol(h_clean),
  G = 100,
  M = 3,
  share = as.matrix(unname(n)),
  dime_diff = as.numeric(unlist(dime_diff)),
  demo = as.matrix(unname(r)),
  dime_prior = 0.05
)

jersey <- numb$sample(
  data = listerine,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)


summary_jersey <- as.data.frame(jersey$summary())