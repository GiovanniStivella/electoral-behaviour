library(lme4)
library(tidyr)
library(dplyr)
library(broom)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(tidyverse)
library(plot3D)
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
demplus <- dem%>%select(-c(1, 5, 6, 7, 8, 9))

m <- as.matrix(unname(h_clean)) - 0.5

n <-m[1:100,]

r <-demplus[1:100,]


#ESTIMATION
est <- file.path("~/Electoral Behaviour/Bayesian estimation/NoDime.stan")
matio <- cmdstan_model(est)

listile <- list(
  T = ncol(h_clean),
  C = 12,
  G = 100,
  M = 4,
  K = 3,
  share = as.matrix(unname(n)),
  cand_a = as.numeric(unlist(cand_a)),
  cand_b = as.numeric(unlist(cand_b)),
  dime = as.numeric(unlist(dime)),
  demo = as.matrix(unname(r)),
  w_sigma = 0.25
)

three <- matio$sample(
  data = listile,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)


summary_three <- as.data.frame(three$summary())


three_wide <- summary_three %>%
  select(1,2)%>%
  slice(3:nrow(summary_three))%>%
  separate(variable,
           into = c("unit", "dimension"),
           sep = ",",
           extra = "merge",   # if there are >1 commas, everything after 1st goes into after_comma
           fill = "right") %>%
  pivot_wider(names_from = dimension,
              values_from = mean,
              names_prefix = "value_")

three_prec <- three_wide%>%slice(1:100)

plot(three_prec$`value_1]`, three_prec$`value_2]`)

three_cand <- three_wide%>%slice(101:112)

colors <- ifelse(cand_long$candidate_number %% 2 == 1, "blue", "red")

plot(three_cand$`value_1]`, three_cand$`value_2]`)

text(
  three_cand$`value_1]`,
  three_cand$`value_2]`,
  labels = cand_long$candidate_name,
  pos = 4,
  cex = 0.8,
  col = colors
)


points3D(three_cand$`value_1]`, three_cand$`value_2]`,  three_cand$`value_3]`)