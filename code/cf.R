library(tidyverse)
library(grf)

dat <- readRDS("processed_data/dat.RDS")
df <- dat %>% 
  mutate(fire_type_bin = case_when(
    fire_type == "Prescribed Fire" ~ 1,
    fire_type == "Wildfire" ~ 0,
    fire_type == "Unknown" ~ NA
  )) %>% 
  mutate(state_num = case_when(
    state == "CA" ~ 1,
    state == "FL" ~ 2,
    state == "GA" ~ 3
  )) %>%
  mutate(duration = FDate - IDate) %>%
  mutate(daily_pop_smokePM = total_pop_smokePM / as.numeric(duration))
df <- df[complete.cases(df),] %>%
  filter(burn_severity_mode != 6)
  # 759 complete cases
df <- df[df$daily_pop_smokePM != Inf,] # remove 4 obs where daily_pop_smokePM = Inf -> 755 obs

Y1 <- df$total_pop_smokePM # outcome 1: total smoke severity
Y2 <- df$daily_pop_smokePM # outcome 2: daily smoke severity
W <- df$fire_type_bin # treatment: fire type
B <- as.data.frame(df$burn_severity_mode) # burn severity
X <- df[,9:24] # confounders
V <- df[,c(8:24)] # burn severity + confounders
state <- df$state_num

set.seed(905)
Y <- Y2
# estimate expected outcome model m(x) = E[Y|B,X]
Y.forest <- regression_forest(V, Y)
Y.hat <- predict(Y.forest)$predictions

# estimate propensity score model e(x) = E[W|X]
W.forest <- regression_forest(X, W)
W.hat <- predict(W.forest)$predictions

# causal forest
cf_all <- causal_forest(V, Y, W, Y.hat = Y.hat, W.hat = W.hat)
varimp <- variable_importance(cf_all) 
selected <- which(varimp > mean(varimp)) 

cf <- causal_forest(V[,selected], Y, W, Y.hat = Y.hat, W.hat = W.hat)
test_calibration(cf) 
att <- average_treatment_effect(cf, target.sample = "overlap") 


