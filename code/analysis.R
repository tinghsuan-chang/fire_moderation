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
  mutate(fire = 1) %>% # fire indicator (1: fire, 0: no fire)
  mutate(CA_ind = ifelse(state == "CA", 1, 0),
         FL_ind = ifelse(state == "FL", 1, 0)) %>%
  mutate(yr = lubridate::year(ig_date)) %>%
  mutate(state_yr = as.factor(paste0(state, yr))) %>%
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
         `other land cover` = other,
         `precipitation` = pr,
         `wind direction` = th,
         `wind velocity` = vs,
         `vapor pressure deficit` = vpd,
         `min. temperature` = tmmn,
         `max. temperature` = tmmx,
         `min. relative humidity` = rmin,
         `max. relative humidity` = rmax) %>%
  # log transform smoke exposure: log(y+1)
  mutate(log_total_pop = log(total_pop_smokePM + 1),
         log_total = log(total_smokePM + 1),
         log_daily_pop = log(daily_pop_smokePM + 1),
         log_km2_pop = log(km2_pop_smokePM + 1),
         log_km2 = log(km2_smokePM + 1))
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

# Duplicate data --> set fire = 0, smoke exposure = 0 in duplicate
df_all <- rbind(df, df) 
df_all[1:(nrow(df_all)/2),]$fire <- rep(0, nrow(df_all)/2)
df_all[1:(nrow(df_all)/2),]$total_pop_smokePM <- rep(0, nrow(df_all)/2)
df_all[1:(nrow(df_all)/2),]$total_smokePM <- rep(0, nrow(df_all)/2)
df_all[1:(nrow(df_all)/2),]$daily_pop_smokePM <- rep(0, nrow(df_all)/2)
df_all[1:(nrow(df_all)/2),]$km2_pop_smokePM <- rep(0, nrow(df_all)/2)
df_all[1:(nrow(df_all)/2),]$km2_smokePM <- rep(0, nrow(df_all)/2)
df_all[1:(nrow(df_all)/2),]$log_km2_pop <- rep(0, nrow(df_all)/2)
df_all[1:(nrow(df_all)/2),]$log_km2 <- rep(0, nrow(df_all)/2)

df_pres <- df_all %>% filter(fire_type == "Prescribed Fire")
df_wild <- df_all %>% filter(fire_type == "Wildfire")

topography <- c("elevation", "slope", "aspect sine", "aspect cosine")
landcover <- c("forest coverage", "shrubland coverage", "herbaceous coverage", "other land cover")
climate <- c("precipitation", "wind direction", "wind velocity", "vapor pressure deficit", "min. temperature", "max. temperature", "min. relative humidity", "max. relative humidity")
confounders <- c(topography, landcover, climate)

# Exploratory analyses ------------------------------------------------------------------
# CA, FL and GA sample size by fire type
table(df[df$state == "CA",]$fire_type) # pres: 4, wild: 481
table(df[df$state == "FL",]$fire_type) # pres: 551, wild: 142
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

# Number of fire events in each state-year
print(xtable::xtable(cbind(table(df_pres$state_yr), table(df_wild$state_yr)), type = "latex"), file = "tables/clusters.tex") # need to make some tweaks to .tex to make it look better 

# Duration by fire type 
df %>%
  mutate(duration = ifelse(duration == 0, 1, duration)) %>%
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

# Topography, climate and land cover by fire type
base <- table1(~ elevation + slope + `aspect sine` + `aspect cosine` + 
                 `precipitation` + `wind direction` + `wind velocity` + `vapor pressure deficit` + `min. temperature` + `max. temperature` + `min. relative humidity` + `max. relative humidity` +  
                 `forest coverage` + `shrubland coverage` + `herbaceous coverage`| fire_type, 
               data = df, render.continuous = c(.="Median [Min, Max]"))
writeLines(t1kable(base, format = "latex"), "tables/base.tex") # need to make some tweaks to .tex to make it look better 

# Burn severity by fire type
df %>%
  group_by(fire_type, `burn severity`) %>%
  summarise(n = n()) %>%
  mutate(perc = prop.table(n) * 100)


# Causal forest -------------------------------------------------------------------------
cf_func <- function(df, outcome = c("km2_pop", "km2", "log_km2_pop", "log_km2"), target = "treated", ps = NA, clus = "Y") {
  if (outcome == "km2_pop") {
    Y <- df$km2_pop_smokePM 
  } else if (outcome == "km2") {
    Y <- df$km2_smokePM
  } else if (outcome == "log_km2_pop") {
    Y <- df$log_km2_pop
  } else if (outcome == "log_km2") {
    Y <- df$log_km2
  }
  A <- df$fire # "treatment": fire indicator (denoted as T in manuscript)
  X <- df[,confounders] 
  V <- df[,c("burn severity",confounders)] 
  V_names <- colnames(V)
  cl <- df$state_yr
  
  # Estimate expected outcome model m(v) = E[Y|V]
  if (clus == "Y") {
    Y.forest <- regression_forest(V, Y, clusters = cl)
  } else {
    Y.forest <- regression_forest(V, Y)
  }
  Y.hat <- predict(Y.forest)$predictions
  
  # If propensity scores are not provided, estimate PS model e(x) = E[A|X]
  if (length(ps) == 1) {
    if (clus == "Y") {
      A.forest <- regression_forest(X, A, clusters = cl)
    } else {
      A.forest <- regression_forest(X, A)
    }
    A.hat <- predict(A.forest)$predictions
  } else {
    A.hat <- ps
  }
  
  # Causal forest 
  if (clus == "Y") {
    cf <- causal_forest(V, Y, A, Y.hat = Y.hat, W.hat = A.hat, clusters = cl)
  } else {
    cf <- causal_forest(V, Y, A, Y.hat = Y.hat, W.hat = A.hat)
  }
  varimp <- variable_importance(cf)  
  ate <- average_treatment_effect(cf, target.sample = target) 
  
  return(list(cf = cf, 
              var = V_names,
              var_imp = varimp, 
              ate = ate))
}

# Prescribed fire causal forest
#set.seed(1); cf_func(df_pres, outcome = "km2_pop")
#set.seed(1); cf_func(df_pres, outcome = "km2")
set.seed(1); pres_km2_pop <- cf_func(df_pres, outcome = "log_km2_pop")
set.seed(1); pres_km2 <- cf_func(df_pres, outcome = "log_km2")

# Wildfire causal forest
#set.seed(1); cf_func(df_wild, outcome = "km2_pop")
#set.seed(1); cf_func(df_wild, outcome = "km2")
set.seed(1); wild_km2_pop <- cf_func(df_wild, outcome = "log_km2_pop")
set.seed(1); wild_km2 <- cf_func(df_wild, outcome = "log_km2")


# Variable importance -------------------------------------------------------------------

# Prescribed fires population smoke exposure (person mu*g/m^3)
keep <- order(pres_km2_pop$var_imp, decreasing = T)
keep_var <- pres_km2_pop$var[keep]
keep_varimp <- pres_km2_pop$var_imp[keep]
d <- tibble(var = keep_var, varimp = keep_varimp)
pres_km2_pop_varimp <- ggplot(data = d[1:10,], aes(x = varimp, y = reorder(var, varimp))) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme_minimal() +
  xlab("variable importance") +
  ylab("") +
  labs(title = "Prescribed fires", subtitle = expression(paste("Population smoke exposure per ", km^{2}))) +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 9))

# Wildfires population smoke exposure (person mu*g/m^3)
keep <- order(wild_km2_pop$var_imp, decreasing = T)
keep_var <- wild_km2_pop$var[keep]
keep_varimp <- wild_km2_pop$var_imp[keep]
d <- tibble(var = keep_var, varimp = keep_varimp)
wild_km2_pop_varimp <- ggplot(data = d[1:10,], aes(x = varimp, y = reorder(var, varimp))) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme_minimal() +
  xlab("variable importance") +
  ylab("") +
  labs(title = "Wildfires", subtitle = expression(paste("Population smoke exposure per ", km^{2}))) +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 9))

varimp_pop <- pres_km2_pop_varimp | wild_km2_pop_varimp
ggsave("figures/varimp_pop.pdf", varimp_pop, width = 18, height = 10, units = "cm")
  
# Prescribed fires area smoke exposure (mu*g/m^3)
keep <- order(pres_km2$var_imp, decreasing = T)
keep_var <- pres_km2$var[keep]
keep_varimp <- pres_km2$var_imp[keep]
d <- tibble(var = keep_var, varimp = keep_varimp)
pres_km2_varimp <- ggplot(data = d[1:10,], aes(x = varimp, y = reorder(var, varimp))) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme_minimal() +
  xlab("variable importance") +
  ylab("") +
  labs(title = "Prescribed fires", subtitle = expression(paste("Area smoke exposure per ", km^{2}))) +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 9))

# Wildfires area smoke exposure (mu*g/m^3)
keep <- order(wild_km2$var_imp, decreasing = T)
keep_var <- wild_km2$var[keep]
keep_varimp <- wild_km2$var_imp[keep]
d <- tibble(var = keep_var, varimp = keep_varimp)
wild_km2_varimp <- ggplot(data = d[1:10,], aes(x = varimp, y = reorder(var, varimp))) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  theme_minimal() +
  xlab("variable importance") +
  ylab("") +
  labs(title = "Wildfires", subtitle = expression(paste("Area smoke exposure per ", km^{2}))) +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 9))

varimp_area <- pres_km2_varimp | wild_km2_varimp
ggsave("figures/varimp_area.pdf", varimp_area, width = 18, height = 10, units = "cm")


# TOC curves -----------------------------------------------------------------------------------
rate <- function(data, forest, var_rank) {
  set.seed(1) 
  rate_est <- rank_average_treatment_effect(forest$cf, 
                                            -data[, forest$var[order(forest$var_imp, decreasing = T)][var_rank]], # add negative sign if rank units from low to high
                                            target = "AUTOC")
  return(list(rate_est = rate_est,
              CI_l = rate_est$estimate - 1.96*rate_est$std.err,
              CI_u = rate_est$estimate + 1.96*rate_est$std.err))
}


# Prescribed fire population smoke exposure
pres_km2pop_AUTOC1 <- rate(data = df_pres, forest = pres_km2_pop, var_rank = 1) # CI: 0.17, 0.66 *
pres_km2pop_AUTOC2 <- rate(data = df_pres, forest = pres_km2_pop, var_rank = 2) # CI: -0.72, 0.086
pres_km2pop_AUTOC3 <- rate(data = df_pres, forest = pres_km2_pop, var_rank = 3) # CI: -0.76, 0.11
pres_km2pop_AUTOC4 <- rate(data = df_pres, forest = pres_km2_pop, var_rank = 4) # CI: -0.53, 0.07
pres_km2pop_AUTOC5 <- rate(data = df_pres, forest = pres_km2_pop, var_rank = 5) # CI: -0.09, 0.60
pres_km2pop_AUTOC6 <- rate(data = df_pres, forest = pres_km2_pop, var_rank = 6) # CI: -0.48, 0.24
pres_km2pop_AUTOC7 <- rate(data = df_pres, forest = pres_km2_pop, var_rank = 7) # CI: -0.51, 0.04
pres_km2pop_AUTOC8 <- rate(data = df_pres, forest = pres_km2_pop, var_rank = 8) # CI: 0.0007, 0.24 *
pres_km2pop_AUTOC9 <- rate(data = df_pres, forest = pres_km2_pop, var_rank = 9) # CI: -0.17, 0.42
pres_km2pop_AUTOC10 <- rate(data = df_pres, forest = pres_km2_pop, var_rank = 10) # CI: -0.02, 0.26
# main figure
pdf("figures/TOC_plots/pres_pop_main.pdf", width = 8, height = 5, pointsize = 11)
  par(mfrow = c(1,2))
  # min. relative humidity
  plot(pres_km2pop_AUTOC1$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on min. relative humidity")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2pop_AUTOC1$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2pop_AUTOC1$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(a)", side = 3, line = 2, cex = 1.2, adj = -0.23)
  # precipitation
  plot(pres_km2pop_AUTOC8$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on precipitation")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2pop_AUTOC8$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2pop_AUTOC8$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.5)
  mtext("(b)", side = 3, line = 2, cex = 1.2, adj = -0.1)
dev.off()
# supp figure
pdf("figures/TOC_plots/pres_pop_supp.pdf", width = 7, height = 12, pointsize = 11)
  par(mfrow = c(5,2))
  # min. relative humidity
  plot(pres_km2pop_AUTOC1$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on min. relative humidity")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2pop_AUTOC1$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2pop_AUTOC1$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(a)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # slope
  plot(pres_km2pop_AUTOC2$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on slope")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2pop_AUTOC2$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2pop_AUTOC2$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(b)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # elevation
  plot(pres_km2pop_AUTOC3$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on elevation")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2pop_AUTOC3$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2pop_AUTOC3$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(c)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # vpd
  plot(pres_km2pop_AUTOC4$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on vapor pressure deficit")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2pop_AUTOC4$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2pop_AUTOC4$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(d)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # other land cover
  plot(pres_km2pop_AUTOC5$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on other land cover")
  mtext("(developed, barren, planted/cultivated, wetlands)", side = 3, line = 0.5, cex = 0.7, adj = 0.5)
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2pop_AUTOC5$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2pop_AUTOC5$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(e)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # forest coverage
  plot(pres_km2pop_AUTOC6$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on forest coverage")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2pop_AUTOC6$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2pop_AUTOC6$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(f)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # wind direction
  plot(pres_km2pop_AUTOC7$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on wind direction")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2pop_AUTOC7$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2pop_AUTOC7$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(g)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # precipitation
  plot(pres_km2pop_AUTOC8$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on precipitation")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2pop_AUTOC8$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2pop_AUTOC8$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.5)
  mtext("(h)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # min. temperature
  plot(pres_km2pop_AUTOC9$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on min. temperature")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2pop_AUTOC9$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2pop_AUTOC9$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(i)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # herbaceous coverage
  plot(pres_km2pop_AUTOC10$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on herbaceous coverage")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2pop_AUTOC10$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2pop_AUTOC10$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(j)", side = 3, line = 2, cex = 1.2, adj = -0.1)
dev.off()

# Prescribed fires area smoke exposure
pres_km2_AUTOC1 <- rate(data = df_pres, forest = pres_km2, var_rank = 1) # CI: 0.08, 0.34 *
pres_km2_AUTOC2 <- rate(data = df_pres, forest = pres_km2, var_rank = 2) # CI: -0.33, 0.02
pres_km2_AUTOC3 <- rate(data = df_pres, forest = pres_km2, var_rank = 3) # CI: -0.29, 0.015
pres_km2_AUTOC4 <- rate(data = df_pres, forest = pres_km2, var_rank = 4) # CI: -0.32, 0.086
pres_km2_AUTOC5 <- rate(data = df_pres, forest = pres_km2, var_rank = 5) # CI: -0.27, 0.079
pres_km2_AUTOC6 <- rate(data = df_pres, forest = pres_km2, var_rank = 6) # CI: -0.046, 0.36
pres_km2_AUTOC7 <- rate(data = df_pres, forest = pres_km2, var_rank = 7) # CI: 0.005, 0.12 *
pres_km2_AUTOC8 <- rate(data = df_pres, forest = pres_km2, var_rank = 8) # CI: -0.21, 0.10
pres_km2_AUTOC9 <- rate(data = df_pres, forest = pres_km2, var_rank = 9) # CI: -0.15, 0.20
pres_km2_AUTOC10 <- rate(data = df_pres, forest = pres_km2, var_rank = 10) # CI: -0.036, 0.25
# supp figure
pdf("figures/TOC_plots/pres_area_supp.pdf", width = 7, height = 12, pointsize = 11)
  par(mfrow = c(5,2))
  # min. relative humidity
  plot(pres_km2_AUTOC1$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on min. relative humidity")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2_AUTOC1$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2_AUTOC1$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(a)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # slope
  plot(pres_km2_AUTOC2$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on slope")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2_AUTOC2$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2_AUTOC2$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(b)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # vpd
  plot(pres_km2_AUTOC3$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on vapor pressure deficit")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2_AUTOC3$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2_AUTOC3$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(c)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # elevation
  plot(pres_km2_AUTOC4$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on elevation")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2_AUTOC4$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2_AUTOC4$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(d)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # forest coverage
  plot(pres_km2_AUTOC5$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on forest coverage")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2_AUTOC5$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2_AUTOC5$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(e)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # other land cover
  plot(pres_km2_AUTOC6$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on other land cover")
  mtext("(developed, barren, planted/cultivated, wetlands)", side = 3, line = 0.5, cex = 0.7, adj = 0.5)
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2_AUTOC6$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2_AUTOC6$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(f)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # precipitation
  plot(pres_km2_AUTOC7$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on precipitation")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2_AUTOC7$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2_AUTOC7$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.8)
  mtext("(g)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # wind direction
  plot(pres_km2_AUTOC8$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on wind direction")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2_AUTOC8$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2_AUTOC8$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(h)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # min. temperature
  plot(pres_km2_AUTOC9$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on min. temperature")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2_AUTOC9$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2_AUTOC9$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(i)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # max. relative humidity
  plot(pres_km2_AUTOC10$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on max. relative humidity")
  mtext(paste("95% CI: [", eval(parse(text = "round(pres_km2_AUTOC10$CI_l, 3)")), ",", eval(parse(text = "round(pres_km2_AUTOC10$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(j)", side = 3, line = 2, cex = 1.2, adj = -0.1)
dev.off()


# Wildfire population smoke exposure
wild_km2pop_AUTOC1 <- rate(data = df_wild, forest = wild_km2_pop, var_rank = 1) # CI: -1.02, -0.46 *
wild_km2pop_AUTOC2 <- rate(data = df_wild, forest = wild_km2_pop, var_rank = 2) # CI: -0.90, -0.14 * 
wild_km2pop_AUTOC3 <- rate(data = df_wild, forest = wild_km2_pop, var_rank = 3) # CI: -0.59, -0.0016 *
wild_km2pop_AUTOC4 <- rate(data = df_wild, forest = wild_km2_pop, var_rank = 4) # CI: -0.49, 0.18
wild_km2pop_AUTOC5 <- rate(data = df_wild, forest = wild_km2_pop, var_rank = 5) # CI: -0.45, 0.25
wild_km2pop_AUTOC6 <- rate(data = df_wild, forest = wild_km2_pop, var_rank = 6) # CI: -0.39, 0.12
wild_km2pop_AUTOC7 <- rate(data = df_wild, forest = wild_km2_pop, var_rank = 7) # CI: -0.63, -0.059 *
wild_km2pop_AUTOC8 <- rate(data = df_wild, forest = wild_km2_pop, var_rank = 8) # CI: -0.17, 0.22
wild_km2pop_AUTOC9 <- rate(data = df_wild, forest = wild_km2_pop, var_rank = 9) # CI: 0.01, 0.35 *
wild_km2pop_AUTOC10 <- rate(data = df_wild, forest = wild_km2_pop, var_rank = 10) # CI: -0.06, 0.36
# supp figure
pdf("figures/TOC_plots/wild_pop_supp.pdf", width = 7, height = 12, pointsize = 11)
  par(mfrow = c(5,2))
  # forest coverage
  plot(wild_km2pop_AUTOC1$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on forest coverage")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2pop_AUTOC1$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2pop_AUTOC1$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(a)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # slope
  plot(wild_km2pop_AUTOC2$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on slope")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2pop_AUTOC2$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2pop_AUTOC2$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(b)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # vpd
  plot(wild_km2pop_AUTOC3$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on vapor pressure deficit")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2pop_AUTOC3$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2pop_AUTOC3$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(c)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # herbaceous coverage
  plot(wild_km2pop_AUTOC4$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on herbaceous coverage")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2pop_AUTOC4$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2pop_AUTOC4$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(d)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # other land cover
  plot(wild_km2pop_AUTOC5$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on other land cover")
  mtext("(developed, barren, planted/cultivated, wetlands)", side = 3, line = 0.5, cex = 0.7, adj = 0.5)
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2pop_AUTOC5$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2pop_AUTOC5$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(e)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # shrubland coverage
  plot(wild_km2pop_AUTOC6$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on shrubland coverage")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2pop_AUTOC6$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2pop_AUTOC6$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(f)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # elevation
  plot(wild_km2pop_AUTOC7$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on elevation")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2pop_AUTOC7$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2pop_AUTOC7$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(g)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # aspect sine
  plot(wild_km2pop_AUTOC8$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on aspect sine")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2pop_AUTOC8$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2pop_AUTOC8$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(h)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # min. relative humidity
  plot(wild_km2pop_AUTOC9$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on min. relative humidity")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2pop_AUTOC9$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2pop_AUTOC9$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(i)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # wind direction
  plot(wild_km2pop_AUTOC10$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in population smoke exposure per ", km^2)), main = "TOC evaluated on wind direction")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2pop_AUTOC10$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2pop_AUTOC10$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(j)", side = 3, line = 2, cex = 1.2, adj = -0.1)
dev.off()

# Wildfire area smoke exposure
wild_km2_AUTOC1 <- rate(data = df_wild, forest = wild_km2, var_rank = 1) 
wild_km2_AUTOC2 <- rate(data = df_wild, forest = wild_km2, var_rank = 2) 
wild_km2_AUTOC3 <- rate(data = df_wild, forest = wild_km2, var_rank = 3) 
wild_km2_AUTOC4 <- rate(data = df_wild, forest = wild_km2, var_rank = 4) 
wild_km2_AUTOC5 <- rate(data = df_wild, forest = wild_km2, var_rank = 5) 
wild_km2_AUTOC6 <- rate(data = df_wild, forest = wild_km2, var_rank = 6)  
wild_km2_AUTOC7 <- rate(data = df_wild, forest = wild_km2, var_rank = 7)  
wild_km2_AUTOC8 <- rate(data = df_wild, forest = wild_km2, var_rank = 8)  
wild_km2_AUTOC9 <- rate(data = df_wild, forest = wild_km2, var_rank = 9)  
wild_km2_AUTOC10 <- rate(data = df_wild, forest = wild_km2, var_rank = 10)  
# supp figure
pdf("figures/TOC_plots/wild_area_supp.pdf", width = 7, height = 12, pointsize = 11)
  par(mfrow = c(5,2))
  # forest coverage
  plot(wild_km2_AUTOC1$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on forest coverage")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2_AUTOC1$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2_AUTOC1$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(a)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # elevation
  plot(wild_km2_AUTOC2$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on elevation")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2_AUTOC2$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2_AUTOC2$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(b)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # slope
  plot(wild_km2_AUTOC3$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on slope")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2_AUTOC3$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2_AUTOC3$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(c)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # vpd
  plot(wild_km2_AUTOC4$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on vapor pressure deficit")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2_AUTOC4$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2_AUTOC4$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(d)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # max. relative humidity
  plot(wild_km2_AUTOC5$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on max. relative humidity")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2_AUTOC5$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2_AUTOC5$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(e)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # other land cover
  plot(wild_km2_AUTOC6$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on other land cover")
  mtext("(developed, barren, planted/cultivated, wetlands)", side = 3, line = 0.5, cex = 0.7, adj = 0.5)
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2_AUTOC6$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2_AUTOC6$CI_u, 3)")),"]"), side = 3, line = -2, cex = 0.7, adj = 0.9)
  mtext("(f)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # max temp
  plot(wild_km2_AUTOC7$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on max. temperature")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2_AUTOC7$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2_AUTOC7$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(g)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # herbaceous coverage
  plot(wild_km2_AUTOC8$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on herbaceous coverage")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2_AUTOC8$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2_AUTOC8$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(h)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # min. relative humidity
  plot(wild_km2_AUTOC9$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on min. relative humidity")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2_AUTOC9$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2_AUTOC9$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(i)", side = 3, line = 2, cex = 1.2, adj = -0.1)
  # shrubland coverage
  plot(wild_km2_AUTOC10$rate_est, xlab = "Sampled fraction", ylab = expression(paste("Difference in area smoke exposure per ", km^2)), main = "TOC evaluated on shrubland coverage")
  mtext(paste("95% CI: [", eval(parse(text = "round(wild_km2_AUTOC10$CI_l, 3)")), ",", eval(parse(text = "round(wild_km2_AUTOC10$CI_u, 3)")),"]"), side = 1, line = -2, cex = 0.7, adj = 0.9)
  mtext("(j)", side = 3, line = 2, cex = 1.2, adj = -0.1)
dev.off()


# Comparisons between prescribed and wildfires -----------------------------------------------
Y <- df$km2_pop_smokePM # population smoke exposure 
A <- df$fire_type_bin # fire type (1: prescribed fire, 0: wildfire)
X <- df[,c("CA_ind", "FL_ind", confounders)] # add state to account for unmeasured state-level differences
V <- df[,c("burn severity", "CA_ind", "FL_ind", confounders)] 
cl <- df$state_yr
set.seed(1)
Y.forest <- regression_forest(V, Y, clusters = cl)
Y.hat <- predict(Y.forest)$predictions
A.forest <- regression_forest(X, A, clusters = cl)
A.hat <- predict(A.forest)$predictions
cf <- causal_forest(V, Y, A, Y.hat = Y.hat, W.hat = A.hat, clusters = cl)
ate <- average_treatment_effect(cf, target.sample = "overlap") 
ate[1] + c(-1,1)*1.96*ate[2]

Y <- df$km2_smokePM # area smoke exposure 
set.seed(1)
Y.forest <- regression_forest(V, Y, clusters = cl)
Y.hat <- predict(Y.forest)$predictions
A.forest <- regression_forest(X, A, clusters = cl)
A.hat <- predict(A.forest)$predictions
cf <- causal_forest(V, Y, A, Y.hat = Y.hat, W.hat = A.hat, clusters = cl)
ate <- average_treatment_effect(cf, target.sample = "overlap") 
ate[1] + c(-1,1)*1.96*ate[2]
