library(ggplot2)
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

m <- as.matrix(unname(h_clean)) - 0.5

n <-m[1:100,]


#STAN model with one dimension

inter <- file.path('/Users/giovannistivella/Electoral Behaviour/Bayesian estimation/Intermediate.stan')
mediate <- cmdstan_model(inter)

listmed <- list(
  T = ncol(h_clean),
  C = 12,
  G = 100,
  share = as.matrix(unname(n)),
  cand_a = as.numeric(unlist(cand_a)),
  cand_b = as.numeric(unlist(cand_b)),
  dime = as.numeric(unlist(dime))
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

comp <- file.path('/Users/giovannistivella/Electoral Behaviour/Bayesian estimation/More complex.stan')
work <- cmdstan_model(comp)

listfour <- list(
  T = ncol(h_clean),
  C = 12,
  G = 100,
  K = 2,
  share = as.matrix(unname(n)),
  cand_a = as.numeric(unlist(cand_a)),
  cand_b = as.numeric(unlist(cand_b)),
  dime = as.numeric(unlist(dime)),
  dime_prior = 0.5
)

ringo <- work$sample(
  data = listfour,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)

summary_ringo <- as.data.frame(ringo$summary())

summary_bi <- summary_ringo[3:226, 1:2]

summary_bi_prec <- data.frame(
  V1_1 = summary_bi[3:102, 1],
  V1_2 = summary_bi[103:202, 1],
  V2_1 = summary_bi[3:102, 2],
  V2_2 = summary_bi[103:202, 2]
)

ggplot(summary_bi_prec, aes(x = V2_1, y = V2_2)) +
  geom_point() +
  labs(x = "Dimension 1", y = "Dimension 2", title = "Bidimensional Plot of Estimates") +
  theme_minimal()

summary_bi_cand <- data.frame(
  V1_1 = summary_bi[201:212, 1],
  V1_2 = summary_bi[213:224, 1],
  V2_1 = summary_bi[201:212, 2],
  V2_2 = summary_bi[213:224, 2]
)

ggplot(summary_bi_cand, aes(x = V2_1, y = V2_2)) +
  geom_point() +
  labs(x = "Dimension 1", y = "Dimension 2", title = "Bidimensional Plot of Estimates") +
  theme_minimal()

