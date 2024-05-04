library(tidyverse)
library(grf)

# data prep -----------------------------------------------------------------------------------
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


# causal forest -------------------------------------------------------------------------
cf_func <- function(df, outcome, clus = "Y") {
  if (outcome == "total") {
    Y <- df$total_pop_smokePM # outcome 1: total smoke severity
  } else if (outcome == "daily") {
    Y <- df$daily_pop_smokePM # outcome 2: daily smoke severity
  }
  W <- df$fire_type_bin # treatment: fire type
  B <- as.data.frame(df$burn_severity_mode) # burn severity
  X <- df[,9:24] # confounders
  V <- df[,c(8:24)] # burn severity + confounders
  state <- df$state_num
  
  # estimate expected outcome model m(x) = E[Y|B,X]
  if (clus == "Y") {
    Y.forest <- regression_forest(V, Y, clusters = state)
  } else {
    Y.forest <- regression_forest(V, Y)
  }
  Y.hat <- predict(Y.forest)$predictions
  
  # estimate propensity score model e(x) = E[W|X]
  if (clus == "Y") {
    W.forest <- regression_forest(X, W, clusters = state)
  } else {
    W.forest <- regression_forest(X, W)
  }
  W.hat <- predict(W.forest)$predictions
  
  # causal forest 
  if (clus == "Y") {
    cf <- causal_forest(V, Y, W, Y.hat = Y.hat, W.hat = W.hat, clusters = state)
  } else {
    cf <- causal_forest(V, Y, W, Y.hat = Y.hat, W.hat = W.hat)
  }
  varimp <- variable_importance(cf)  
  selected <- which(varimp > mean(varimp)) 
  att <- average_treatment_effect(cf, target.sample = "overlap") 
  
  return(list(cf = cf, 
              var = colnames(V[,selected]), 
              var_imp = varimp[selected], 
              att = att))
}

set.seed(1); cf_func(df, outcome = "total")
set.seed(1); cf_func(df, outcome = "daily")

# plot TOC ------------------------------------------------------------------------------
# CA 
df_CA <- df %>% filter(state == "CA") # 400 obs
set.seed(1); cf_CA <- cf_func(df_CA, outcome = "total", clus = "N")
rate <- rank_average_treatment_effect(cf_CA$cf, 
                                      df_CA$forest, 
                                      subset = !(cf_CA$cf$W.hat %in% c(0, 1)))
plot(rate)

# FL + GA
df_east <- df %>% filter(state %in% c("FL","GA")) # 355 obs
set.seed(1); cf_east <- cf_func(df_east, outcome = "total", clus = "N")
rate <- rank_average_treatment_effect(cf_east$cf, 
                                      df_east$forest, 
                                      subset = !(cf_east$cf$W.hat %in% c(0, 1)))
plot(rate)

