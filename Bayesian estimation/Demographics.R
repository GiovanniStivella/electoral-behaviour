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
dem1 <- dem%>%select(-c(1, 5, 6, 7, 8, 9))

m <- as.matrix(unname(h_clean)) - 0.5

n <-m[1:100,]

r <-dem1[1:100,]

#One dimension

one <- file.path("~/Electoral Behaviour/Bayesian estimation/Demographics.stan")
dimension <- cmdstan_model(one)

listry <- list(
  T = ncol(h_clean),
  C = 12,
  G = 100,
  M = 4,
  share = as.matrix(unname(n)),
  cand_a = as.numeric(unlist(cand_a)),
  cand_b = as.numeric(unlist(cand_b)),
  dime = as.numeric(unlist(dime)),
  demo = as.matrix(unname(r)),
  dime_prior = 0.05
)

george <- dimension$sample(
  data = listry,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)


summary_george <- as.data.frame(george$summary())

#write.csv(summary_george, "summary_demo_0.05.csv")


#Linear model
educ <- lets %>%
  filter(GEOID20 %in% dem$GEOID20) %>%
  mutate(y = (share_1-1/2)/diff_comp)
educ <- educ %>% 
  mutate(bach = (per_B15003_022 + per_B15003_023 + per_B15003_024 + per_B15003_025))

ed_simple <- lmer(y ~ per_vap_hisp+per_vap_white+per_vap_black+per_vap_aian+per_vap_asian+per_vap_nhpi+per_vap_other+per_vap_two+bach+(1|GEOID20),
                  data = educ)

ranef_ed_simple <- ranef(ed_simple)$GEOID20 %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("GEOID20") %>% 
  rename(ranef_ed_simple = `(Intercept)`)

fixed_effects_ed <- broom.mixed::tidy(ed_simple, effects = "fixed") %>%
  select(term, estimate)

#Linear model with first 100 precincts
restrict <- educ[1:700, ]

rest <- lmer(y ~ per_vap_hisp+per_vap_white+per_vap_black+per_vap_aian+per_vap_asian+per_vap_nhpi+per_vap_other+per_vap_two+bach+(1|GEOID20),
             data = restrict)

ranef_rest <- ranef(rest)$GEOID20 %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("GEOID20") %>% 
  rename(ranef_rest = `(Intercept)`)

fixed_effects_rest <- broom.mixed::tidy(rest, effects = "fixed") %>%
  select(term, estimate)

#COMPARISON
summary_george_c <- summary_george[3:102, 2]
scat <- cbind(ranef_rest, summary_george_c)

summary_george_f <- summary_george[116:124, 1:2]
fscat <- cbind(fixed_effects_rest, summary_george_f)

# Scatterplot
plot(scat$ranef_rest, scat$summary_george_c,
     main = "Correlation of two estimates",
     xlab = "x",
     ylab = "y",
     pch = 19, col = "blue")

# Add correlation in title
cor_value <- cor(scat$ranef_rest, scat$summary_george_c)
mtext(paste("Correlation =", cor_value))







#Bayesian estimation with all precincts
listry <- list(
  T = ncol(h_clean),
  C = 12,
  G = nrow(h_clean),
  M = 9,
  share = as.matrix(unname(h_clean)),
  cand_a = as.numeric(unlist(cand_a)),
  cand_b = as.numeric(unlist(cand_b)),
  dime = as.numeric(unlist(dime)),
  demo = as.matrix(unname(dem1))
)

all <- dimension$sample(
  data = listry,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)


summary_all <- as.data.frame(all$summary())