library(tidyr)
library(dplyr)
library(broom)
library(cmdstanr)
library(posterior)
library(bayesplot)
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

#DEMOGRAPHIC DATA
na_rows_prec <- lets %>% filter(is.na(share_1))
dem <- lets%>%
  mutate (bach = rowSums(select(lets,55:58)))%>%
  filter(!GEOID20 %in% na_rows_prec$GEOID20)%>%
  select(1, 22:29, last_col())%>%
  unique()%>%
  select(-c(1))


m <- as.matrix(unname(h_clean)) - 0.5

n <-m[1:100,]

#One dimension

one <- file.path("Demographics.stan")
dimension <- cmdstan_model(one)

listmed <- list(
  T = ncol(h_clean),
  C = 12,
  G = 100,
  M = 9,
  share = as.matrix(unname(n)),
  cand_a = as.numeric(unlist(cand_a)),
  cand_b = as.numeric(unlist(cand_b)),
  dime = as.numeric(unlist(dime)),
  demo = as.matrix(unname(dem))
)

george <- mediate$sample(
  data = listmed,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)


summary_george <- as.data.frame(george$summary())

write.csv(summary_george, "summary_0.05.csv")


#Multidimensional STAN model

comp <- file.path("More complex.stan")
work <- cmdstan_model(comp)

listfour <- list(
  T = ncol(h_clean),
  C = 12,
  G = 100,
  K = 2,
  share = as.matrix(unname(n)),
  cand_a = as.numeric(unlist(cand_a)),
  cand_b = as.numeric(unlist(cand_b)),
  dime = as.numeric(unlist(dime))
)

ringo <- work$sample(
  data = listfour,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)

summary_ringo <- as.data.frame(ringo$summary())