#devtools::install_github("grf-labs/grf", subdir = "r-package/grf") 
library(tidyverse)
library(grf)
library(patchwork)
library(table1)
library(kableExtra)
library(sf)

# Data prep -----------------------------------------------------------------------------------
dat <- readRDS("processed_data/dat.RDS")

df <- dat %>% 
  mutate(fire_type_bin = case_when(
    fire_type == "Prescribed Fire" ~ 1,
    fire_type == "Wildfire" ~ 0,
    fire_type == "Unknown" ~ NA
  )) %>% 
  # fire indicator (1: fire, 0: no fire)
  mutate(fire = 1) %>% 
  mutate(state_num = case_when(
    state == "CA" ~ 1,
    state == "FL" ~ 2,
    state == "GA" ~ 3
  )) %>%
  mutate(duration = FDate - IDate) %>%
  mutate(daily_pop_smokePM = total_pop_smokePM / as.numeric(duration),
         km2_pop_smokePM = total_pop_smokePM / (as.numeric(polygon_area)/(10^6)),
         km2_smokePM = total_smokePM / (as.numeric(polygon_area)/(10^6))) %>%
  mutate(daily_pop_smokePM = ifelse(daily_pop_smokePM == Inf, total_pop_smokePM, daily_pop_smokePM)) %>%
  mutate(forest = forest*100,
         shrubland = shrubland*100,
         herb = herb*100,
         other = other*100) %>%
  rename(`burn severity` = burn_severity_mode,
         `aspect sine` = aspect_sin,
         `aspect cosine` = aspect_cos,
         `forest coverage` = forest,
         `shrubland coverage` = shrubland,
         `herbaceous coverage` = herb,
         `other landcover` = other,
         `precipitation` = pr,
         `wind direction` = th,
         `wind velocity` = vs,
         `vapor pressure deficit` = vpd,
         `min. temperature` = tmmn,
         `max. temperature` = tmmx,
         `min. relative humidity` = rmin,
         `max. relative humidity` = rmax)
df <- df[complete.cases(df),] %>%
  filter(`burn severity` != 6)

# Labels to use in tables
label(df$elevation) <- "Elevation (m)"
label(df$slope) <- "Slope (deg)"
label(df$`aspect sine`) <- "Aspect sine (deg)"
label(df$`aspect cosine`) <- "Aspect cosine (deg)"
label(df$`precipitation`) <- "Precipitation (mm)"
label(df$`min. relative humidity`) <- "Min. relative humidity (%)"
label(df$`max. relative humidity`) <- "Max. relative humidity (%)"
label(df$`wind direction`) <- "Wind direction (deg)"
label(df$`min. temperature`) <- "Min. temperature (K)"
label(df$`max. temperature`) <- "Max. temperature (K)"
label(df$`wind velocity`) <- "Wind velocity (m/s)"
label(df$`vapor pressure deficit`) <- "Vapor pressure deficit (kPa)"
label(df$`forest coverage`) <- "Forest coverage (%)"
label(df$`shrubland coverage`) <- "Shrubland coverage (%)"
label(df$`herbaceous coverage`) <- "Herbaceous coverage (%)"

# Duplicate data --> set fire = 0, smoke severity = 0 in duplicate
df_all <- rbind(df, df) 
df_all[1:(nrow(df_all)/2),]$fire <- rep(0, nrow(df_all)/2)
df_all[1:(nrow(df_all)/2),]$total_pop_smokePM <- rep(0, nrow(df_all)/2)
df_all[1:(nrow(df_all)/2),]$total_smokePM <- rep(0, nrow(df_all)/2)
df_all[1:(nrow(df_all)/2),]$daily_pop_smokePM <- rep(0, nrow(df_all)/2)
df_all[1:(nrow(df_all)/2),]$km2_pop_smokePM <- rep(0, nrow(df_all)/2)
df_all[1:(nrow(df_all)/2),]$km2_smokePM <- rep(0, nrow(df_all)/2)

# Log transform smoke severity: log(y+1)
df_all <- df_all %>%
  mutate(log_total_pop = log(total_pop_smokePM + 1),
         log_total = log(total_smokePM + 1),
         log_daily_pop = log(daily_pop_smokePM + 1),
         log_km2_pop = log(km2_pop_smokePM + 1),
         log_km2 = log(km2_smokePM + 1))

df_pres <- df_all %>% filter(fire_type == "Prescribed Fire")
df_wild <- df_all %>% filter(fire_type == "Wildfire")


# Exploratory analyses ------------------------------------------------------------------
# CA, FL and GA sample size by fire type
table(df[df$state == "CA",]$fire_type) # pres: 4, wild: 475
table(df[df$state == "FL",]$fire_type) # pres: 544, wild: 139
table(df[df$state == "GA",]$fire_type) # pres: 24, wild: 27

# CA, FL and GA maps showing fire boundaries and sample size by fire type
fire_dat <- readRDS("processed_data/fire_dat.RDS") %>%
  dplyr::filter(Event_ID %in% df$mtbs_id)
obj <- st_read("raw_data/cb_2020_us_state_500k/cb_2020_us_state_500k.shp")
CA_map <- ggplot() +
  geom_sf(data = fire_dat %>% filter(state == "CA"), aes(fill = Incid_Type), linewidth = 0.01) +
  geom_sf(data = subset(obj, NAME == "California") %>% st_transform(crs = 4326), fill = NA) +
  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
  labs(title = "California", subtitle = "Prescribed fire: 4 \nWildfire: 475") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")) 
FL_map <- ggplot() +
  geom_sf(data = fire_dat %>% filter(state == "FL"), aes(fill = Incid_Type), linewidth = 0.01) +
  geom_sf(data = subset(obj, NAME == "Florida") %>% st_transform(crs = 4326), fill = NA) +
  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
  labs(title = "Florida", subtitle = "Prescribed fire: 544 \nWildfire: 139") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")) 
GA_map <- ggplot() +
  geom_sf(data = fire_dat %>% filter(state == "GA"), aes(fill = Incid_Type), linewidth = 0.01) +
  geom_sf(data = subset(obj, NAME == "Georgia") %>% st_transform(crs = 4326), fill = NA) +
  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
  labs(title = "Georgia", subtitle = "Prescribed fire: 24 \nWildfire: 27") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")) 
state_map <- CA_map + FL_map + GA_map
ggsave("figures/state_map.pdf", state_map, width = 18, height = 13, units = "cm")

# Duration by fire type
df %>%
  group_by(fire_type) %>%
  summarise(mean_days = mean(duration),
            med_days = median(duration),
            min_days = min(duration),
            max_days = max(duration))

# Polygon area (km^2) by fire type 
df %>%
  group_by(fire_type) %>%
  summarise(mean_area = mean(polygon_area)/10^6,
            med_area = median(polygon_area)/10^6,
            min_area = min(polygon_area)/10^6,
            max_area = max(polygon_area)/10^6)

# Topography, climate and landcover characteristics, and per km^2 smoke severity by fire type
tab1 <- table1(~ elevation + slope + `aspect sine` + `aspect cosine` + 
                 `precipitation` + `wind direction` + `wind velocity` + `vapor pressure deficit` + `min. temperature` + `max. temperature` + `min. relative humidity` + `max. relative humidity` +  
                 `forest coverage` + `shrubland coverage` + `herbaceous coverage` + km2_pop_smokePM + km2_smokePM| fire_type, data = df)
writeLines(t1kable(tab1, format = "latex"), "tables/tab1.tex") # need to make some tweaks to .tex to make it look better 

# Burn severity by fire type
df %>%
  group_by(fire_type, `burn severity`) %>%
  summarise(n = n()) %>%
  mutate(perc = prop.table(n) * 100)


# Causal forest -------------------------------------------------------------------------
confounders <- c("elevation", "slope", "aspect sine", "aspect cosine",
                 "forest coverage", "shrubland coverage", "herbaceous coverage", "other landcover",
                 "precipitation", "wind direction", "wind velocity", "vapor pressure deficit", "min. temperature", "max. temperature", "min. relative humidity", "max. relative humidity")
cf_func <- function(df, outcome = c("total_pop", "daily_pop", "km2_pop", "km2"), target = c("all", "treated", "overlap"), ps = NA, clus = "Y") {
  if (outcome == "total_pop") {
    Y <- df$log_total_pop 
  } else if (outcome == "daily_pop") {
    Y <- df$log_daily_pop 
  } else if (outcome == "km2_pop") {
    Y <- df$log_km2_pop
  } else if (outcome == "km2") {
    Y <- df$log_km2
  }
  A <- df$fire # "treatment": fire indicator (denoted as T in manuscript)
  X <- df[,confounders] 
  V <- df[,c("burn severity", confounders)] 
  V_names <- colnames(V)
  state <- df$state_num
  
  # Estimate expected outcome model m(v) = E[Y|polygon area, V]
  if (clus == "Y") {
    Y.forest <- regression_forest(V, Y, clusters = state)
  } else {
    Y.forest <- regression_forest(V, Y)
  }
  Y.hat <- predict(Y.forest)$predictions
  
  # If propensity scores are not provided, estimate PS model e(x) = E[A|X]
  if (length(ps) == 1) {
    if (clus == "Y") {
      A.forest <- regression_forest(X, A, clusters = state)
    } else {
      A.forest <- regression_forest(X, A)
    }
    A.hat <- predict(A.forest)$predictions
  } else {
    A.hat <- ps
  }
  
  # Causal forest 
  if (clus == "Y") {
    cf <- causal_forest(V, Y, A, Y.hat = Y.hat, W.hat = A.hat, clusters = state)
  } else {
    cf <- causal_forest(V, Y, A, Y.hat = Y.hat, W.hat = A.hat)
  }
  varimp <- variable_importance(cf)  
  #ate <- average_treatment_effect(cf, subset = (A.hat >= 0.1 & A.hat <= 0.9)) 
  ate <- average_treatment_effect(cf, target.sample = target) 
  
  return(list(cf = cf, 
              var = V_names,
              var_imp = varimp, 
              ate = ate))
}

# Prescribed fire causal forest
set.seed(1); pres_km2_pop <- cf_func(df_pres, outcome = "km2_pop", target = "treated")
set.seed(1); pres_km2 <- cf_func(df_pres, outcome = "km2", target = "treated")

# Wildfire causal forest
set.seed(1); wild_km2_pop <- cf_func(df_wild, outcome = "km2_pop", target = "treated")
set.seed(1); wild_km2 <- cf_func(df_wild, outcome = "km2", target = "treated")


# Variable importance -------------------------------------------------------------------

# Prescribed fires per km^2 smoke severity (person mu*g/m^3)
keep <- order(pres_km2_pop$var_imp, decreasing = T)
keep_var <- pres_km2_pop$var[keep]
keep_varimp <- pres_km2_pop$var_imp[keep]
d <- tibble(var = keep_var, varimp = keep_varimp)
pres_km2_pop_varimp <- ggplot(data = d[1:10,], aes(x = varimp, y = reorder(var, varimp))) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme_minimal() +
  xlab("variable importance") +
  ylab("") +
  ggtitle(expression(paste("Per ", km^{2}, " smoke severity (person ", mu*g/m^3, ")"))) +
  theme(plot.title = element_text(size = 7), axis.title = element_text(size = 7),
        axis.text = element_text(size = 7)) 

# Prescribed fires per km^2 smoke severity (mu*g/m^3)
keep <- order(pres_km2$var_imp, decreasing = T)
keep_var <- pres_km2$var[keep]
keep_varimp <- pres_km2$var_imp[keep]
d <- tibble(var = keep_var, varimp = keep_varimp)
pres_km2_varimp <- ggplot(data = d[1:10,], aes(x = varimp, y = reorder(var, varimp))) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme_minimal() +
  xlab("variable importance") +
  ylab("") +
  ggtitle(expression(paste("Per ", km^{2}, " smoke severity (", mu*g/m^3, ")"))) +
  theme(plot.title = element_text(size = 7), axis.title = element_text(size = 7),
        axis.text = element_text(size = 7)) 

# Wildfires per km^2 smoke severity (person mu*g/m^3)
keep <- order(wild_km2_pop$var_imp, decreasing = T)
keep_var <- wild_km2_pop$var[keep]
keep_varimp <- wild_km2_pop$var_imp[keep]
d <- tibble(var = keep_var, varimp = keep_varimp)
wild_km2_pop_varimp <- ggplot(data = d[1:10,], aes(x = varimp, y = reorder(var, varimp))) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme_minimal() +
  xlab("variable importance") +
  ylab("") +
  ggtitle(expression(paste("Per ", km^{2}, " smoke severity (person ", mu*g/m^3, ")"))) +
  theme(plot.title = element_text(size = 7), axis.title = element_text(size = 7),
        axis.text = element_text(size = 7)) 

# Wildfires per km^2 smoke severity (mu*g/m^3)
keep <- order(wild_km2$var_imp, decreasing = T)
keep_var <- wild_km2$var[keep]
keep_varimp <- wild_km2$var_imp[keep]
d <- tibble(var = keep_var, varimp = keep_varimp)
wild_km2_varimp <- ggplot(data = d[1:10,], aes(x = varimp, y = reorder(var, varimp))) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme_minimal() +
  xlab("variable importance") +
  ylab("") +
  ggtitle(expression(paste("Per ", km^{2}, " smoke severity (", mu*g/m^3, ")"))) +
  theme(plot.title = element_text(size = 7), axis.title = element_text(size = 7),
        axis.text = element_text(size = 7)) 

p1 <- (pres_km2_pop_varimp | pres_km2_varimp) & plot_annotation("(a) Prescribed fires", theme = theme(plot.title = element_text(size = 8)))
p2 <- (wild_km2_pop_varimp | wild_km2_varimp) & plot_annotation("(b) Wildfires", theme = theme(plot.title = element_text(size = 8)))
varimp <- wrap_elements(p1) / wrap_elements(p2)
ggsave("figures/varimp.pdf", varimp, width = 18, height = 18, units = "cm")


# TOC & RATE -----------------------------------------------------------------------------------
rate <- function(data, outcome = c("total_pop", "daily_pop", "km2_pop", "km2"), target = c("all", "treated", "overlap"), forest, var_rank, sta = c("CA", "FL", "GA")) {
  set.seed(1) 
  cf_fit <- cf_func(data %>% filter(state == sta), 
                    outcome = outcome, 
                    target = target,
                    ps = forest$cf$W.hat[data$state == sta], # plug in estimated PS from causal forest with 3 states 
                    clus = "N")
  rate_est <- rank_average_treatment_effect(cf_fit$cf, 
                                            -data[data$state == sta, forest$var[order(forest$var_imp, decreasing = T)][var_rank]], # add negative sign if rank units from low to high
                                            target = "AUTOC",
                                            subset = (cf_fit$cf$W.hat >= 0.1 & cf_fit$cf$W.hat <= 0.9))
  return(rate_est)
}


# Prescribed fires (FL)
pres_km2pop_AUTOC1 <- rate(data = df_pres, target = "treated", outcome = "km2_pop", forest = pres_km2_pop, var_rank = 1, sta = "FL") 
pres_km2pop_AUTOC2 <- rate(data = df_pres, target = "treated", outcome = "km2_pop", forest = pres_km2_pop, var_rank = 2, sta = "FL") 
pres_km2_AUTOC1 <- rate(data = df_pres, target = "treated", outcome = "km2", forest = pres_km2, var_rank = 1, sta = "FL") 
pres_km2_AUTOC2 <- rate(data = df_pres, target = "treated", outcome = "km2", forest = pres_km2, var_rank = 2, sta = "FL") 
pdf("figures/TOC_plots/FL_pres.pdf", width = 8, height = 8, pointsize = 11)
  par(mfrow = c(2,2))
  plot(pres_km2pop_AUTOC1, xlab = "Sampled fraction", ylab = expression(paste("Smoke severity difference in ", "log(person ", mu*g/m^3, ")")), main = "TOC evaluated on min. relative humidity")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2pop_AUTOC1$estimate - 1.96*pres_km2pop_AUTOC1$std.err, 3)")), ",", eval(parse(text = "round(pres_km2pop_AUTOC1$estimate + 1.96*pres_km2pop_AUTOC1$std.err, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(a)", side = 3, line = 2, cex = 1.5, adj = -0.1)
  plot(pres_km2pop_AUTOC2, xlab = "Sampled fraction", ylab = expression(paste("Smoke severity difference in ", "log(person ", mu*g/m^3, ")")), main = "TOC evaluated on slope")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2pop_AUTOC2$estimate - 1.96*pres_km2pop_AUTOC2$std.err, 3)")), ",", eval(parse(text = "round(pres_km2pop_AUTOC2$estimate + 1.96*pres_km2pop_AUTOC2$std.err, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(b)", side = 3, line = 2, cex = 1.5, adj = -0.1)
  plot(pres_km2_AUTOC1, xlab = "Sampled fraction", ylab = expression(paste("Smoke severity difference in ", "log(", mu*g/m^3, ")")), main = "TOC evaluated on min. relative humidity")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2_AUTOC1$estimate - 1.96*pres_km2_AUTOC1$std.err, 3)")), ",", eval(parse(text = "round(pres_km2_AUTOC1$estimate + 1.96*pres_km2_AUTOC1$std.err, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(c)", side = 3, line = 2, cex = 1.5, adj = -0.1)
  plot(pres_km2_AUTOC2, xlab = "Sampled fraction", ylab = expression(paste("Smoke severity difference in ", "log(", mu*g/m^3, ")")), main = "TOC evaluated on wind velocity")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2_AUTOC2$estimate - 1.96*pres_km2_AUTOC2$std.err, 3)")), ",", eval(parse(text = "round(pres_km2_AUTOC2$estimate + 1.96*pres_km2_AUTOC2$std.err, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(d)", side = 3, line = 2, cex = 1.5, adj = -0.1)
dev.off()

# Wildfires (FL)
wild_km2pop_AUTOC1 <- rate(data = df_wild, target = "treated", outcome = "km2_pop", forest = wild_km2_pop, var_rank = 1, sta = "FL") 
wild_km2pop_AUTOC2 <- rate(data = df_wild, target = "treated", outcome = "km2_pop", forest = wild_km2_pop, var_rank = 2, sta = "FL") 
wild_km2_AUTOC1 <- rate(data = df_wild, target = "treated", outcome = "km2", forest = wild_km2, var_rank = 1, sta = "FL") 
wild_km2_AUTOC2 <- rate(data = df_wild, target = "treated", outcome = "km2", forest = wild_km2, var_rank = 2, sta = "FL") 
pdf("figures/TOC_plots/FL_wild.pdf", width = 8, height = 8, pointsize = 11)
  par(mfrow = c(2,2))
  plot(wild_km2pop_AUTOC1, xlab = "Sampled fraction", ylab = expression(paste("Smoke severity difference in ", "log(person ", mu*g/m^3, ")")), main = "TOC evaluated on forest coverage")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2pop_AUTOC1$estimate - 1.96*wild_km2pop_AUTOC1$std.err, 3)")), ",", eval(parse(text = "round(wild_km2pop_AUTOC1$estimate + 1.96*wild_km2pop_AUTOC1$std.err, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(a)", side = 3, line = 2, cex = 1.5, adj = -0.1)
  plot(wild_km2pop_AUTOC2, xlab = "Sampled fraction", ylab = expression(paste("Smoke severity difference in ", "log(person ", mu*g/m^3, ")")), main = "TOC evaluated on vapor pressure deficit")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2pop_AUTOC2$estimate - 1.96*wild_km2pop_AUTOC2$std.err, 3)")), ",", eval(parse(text = "round(wild_km2pop_AUTOC2$estimate + 1.96*wild_km2pop_AUTOC2$std.err, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(b)", side = 3, line = 2, cex = 1.5, adj = -0.1)
  plot(wild_km2_AUTOC1 , xlab = "Sampled fraction", ylab = expression(paste("Smoke severity difference in ", "log(", mu*g/m^3, ")")), main = "TOC evaluated on forest coverage")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2_AUTOC1$estimate - 1.96*wild_km2_AUTOC1$std.err, 3)")), ",", eval(parse(text = "round(wild_km2_AUTOC1$estimate + 1.96*wild_km2_AUTOC1$std.err, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(c)", side = 3, line = 2, cex = 1.5, adj = -0.1)
  plot(wild_km2_AUTOC2, xlab = "Sampled fraction", ylab = expression(paste("Smoke severity difference in ", "log(", mu*g/m^3, ")")), main = "TOC evaluated on vapor pressure deficit")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2_AUTOC2$estimate - 1.96*wild_km2_AUTOC2$std.err, 3)")), ",", eval(parse(text = "round(wild_km2_AUTOC2$estimate + 1.96*wild_km2_AUTOC2$std.err, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(d)", side = 3, line = 2, cex = 1.5, adj = -0.1)
dev.off()


# Comparisons between prescribed and wildfires -----------------------------------------------
# E[Y|pres, V = overall median] - E[Y|wild, V = overall median]
V_pres <- V_wild <- matrix(apply(df_all[df_all$fire == 1, c("burn severity", confounders)], 2, median), nrow = 1)
pred_pres <- predict(pres_km2_pop$cf, V_pres, estimate.variance = TRUE)
pred_wild <- predict(wild_km2_pop$cf, V_wild, estimate.variance = TRUE) 
(exp(pred_pres$predictions)-1) - (exp(pred_wild$predictions)-1) 
((exp(pred_pres$predictions)-1) - (exp(pred_wild$predictions)-1)) + 
  c(-1,1)*1.96*sqrt(pred_pres$variance.estimates*exp(2*pred_pres$predictions) + pred_wild$variance.estimates*exp(2*pred_wild$predictions)) 

# E[Y|pres, V = pres median] - E[Y|wild, V = wild median]
V_pres <- matrix(apply(df_pres[df_pres$fire == 1, c("burn severity", confounders)], 2, median), nrow = 1)
V_wild <- matrix(apply(df_wild[df_wild$fire == 1, c("burn severity", confounders)], 2, median), nrow = 1)
pred_pres <- predict(pres_km2_pop$cf, V_pres, estimate.variance = TRUE)
pred_wild <- predict(wild_km2_pop$cf, V_wild, estimate.variance = TRUE) 
(exp(pred_pres$predictions)-1) - (exp(pred_wild$predictions)-1) 
((exp(pred_pres$predictions)-1) - (exp(pred_wild$predictions)-1)) + 
  c(-1,1)*1.96*sqrt(pred_pres$variance.estimates*exp(2*pred_pres$predictions) + pred_wild$variance.estimates*exp(2*pred_wild$predictions)) 
