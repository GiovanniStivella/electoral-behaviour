library(lme4)
library(tidyr)
library(dplyr)
library(broom)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(tidyverse)
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
dem1 <- dem%>%select(-c(1, 3, 4, 5, 6, 7, 8, 9))

m <- as.matrix(unname(h_clean)) - 0.5

n <-m[1:100,]

r <-dem1[1:100,]

#Two dimensions

two <- file.path("~/Electoral Behaviour/Bayesian estimation/K2Demo.stan")
multi <- cmdstan_model(two)

listanera <- list(
  T = ncol(h_clean),
  C = 12,
  G = 100,
  M = 2,
  K = 2,
  share = as.matrix(unname(n)),
  cand_a = as.numeric(unlist(cand_a)),
  cand_b = as.numeric(unlist(cand_b)),
  dime = as.numeric(unlist(dime)),
  demo = as.matrix(unname(r)),
  dime_prior = 0.05
)

tego <- multi$sample(
  data = listanera,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)


summary_tego <- as.data.frame(tego$summary())

#Two dimensions with less DIME constraint

listabianca <- list(
  T = ncol(h_clean),
  C = 12,
  G = 100,
  M = 2,
  K = 2,
  share = as.matrix(unname(n)),
  cand_a = as.numeric(unlist(cand_a)),
  cand_b = as.numeric(unlist(cand_b)),
  dime = as.numeric(unlist(dime)),
  demo = as.matrix(unname(r)),
  dime_prior = 1
)

pian <- multi$sample(
  data = listabianca,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)


summary_pian <- as.data.frame(pian$summary())

#SUPERSUMMARY
supersummary <- summary_tego%>%
  left_join(summary_pian, by= "variable",  suffix = c("_tego", "_pian"))%>%
  select(1,2,11)

superplot <- supersummary%>%slice(2:nrow(supersummary))

plot(superplot$mean_tego, superplot$mean_pian,
     main = "Correlation of two estimates",
     xlab = "x",
     ylab = "y",
     pch = 19, col = "blue")


pian_wide <- summary_pian %>%
  select(1,2)%>%
  slice(3:nrow(supersummary))%>%
  separate(variable,
           into = c("unit", "dimension"),
           sep = ",",
           extra = "merge",   # if there are >1 commas, everything after 1st goes into after_comma
           fill = "right") %>%
  pivot_wider(names_from = dimension,
              values_from = mean,
              names_prefix = "value_")

pian_prec <- pian_wide%>%slice(1:100)

plot(pian_prec$`value_1]`, pian_prec$`value_2]`)

pian_cand <- pian_wide%>%slice(101:112)

plot(pian_cand$`value_1]`, pian_cand$`value_2]`)
text(pian_cand$`value_1]`, pian_cand$`value_2]`, labels = cand_long$candidate_name, pos = 4, cex = 0.8)

#Let's take it more radical
listand <- list(
  T = ncol(h_clean),
  C = 12,
  G = 100,
  M = 2,
  K = 2,
  share = as.matrix(unname(n)),
  cand_a = as.numeric(unlist(cand_a)),
  cand_b = as.numeric(unlist(cand_b)),
  dime = as.numeric(unlist(dime)),
  demo = as.matrix(unname(r)),
  dime_prior = 2
)

ber <- multi$sample(
  data = listand,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)


summary_ber <- as.data.frame(ber$summary())


ber_wide <- summary_ber %>%
  select(1,2)%>%
  slice(3:nrow(supersummary))%>%
  separate(variable,
           into = c("unit", "dimension"),
           sep = ",",
           extra = "merge",   # if there are >1 commas, everything after 1st goes into after_comma
           fill = "right") %>%
  pivot_wider(names_from = dimension,
              values_from = mean,
              names_prefix = "value_")

ber_prec <- ber_wide%>%slice(1:100)

plot(ber_prec$`value_1]`, ber_prec$`value_2]`)

ber_cand <- ber_wide%>%slice(101:112)

plot(ber_cand$`value_1]`, ber_cand$`value_2]`)
text(ber_cand$`value_1]`, ber_cand$`value_2]`, labels = cand_long$candidate_name, pos = 4, cex = 0.8)


#EXTREME
ext <- file.path("~/Electoral Behaviour/Bayesian estimation/NoDime.stan")
reme <- cmdstan_model(ext)

listreme <- list(
  T = ncol(h_clean),
  C = 12,
  G = 100,
  M = 2,
  K = 2,
  share = as.matrix(unname(n)),
  cand_a = as.numeric(unlist(cand_a)),
  cand_b = as.numeric(unlist(cand_b)),
  dime = as.numeric(unlist(dime)),
  demo = as.matrix(unname(r))
)

unc <- reme$sample(
  data = listreme,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)


summary_unc <- as.data.frame(unc$summary())


unc_wide <- summary_unc %>%
  select(1,2)%>%
  slice(3:nrow(supersummary))%>%
  separate(variable,
           into = c("unit", "dimension"),
           sep = ",",
           extra = "merge",   # if there are >1 commas, everything after 1st goes into after_comma
           fill = "right") %>%
  pivot_wider(names_from = dimension,
              values_from = mean,
              names_prefix = "value_")

unc_prec <- unc_wide%>%slice(1:100)

plot(unc_prec$`value_1]`, unc_prec$`value_2]`)

unc_cand <- unc_wide%>%slice(101:112)

plot(unc_cand$`value_1]`, unc_cand$`value_2]`)
text(unc_cand$`value_1]`, unc_cand$`value_2]`, labels = cand_long$candidate_name, pos = 4, cex = 0.8)

