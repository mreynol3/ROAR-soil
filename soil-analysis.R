#new script

library(dplyr)
library(stars)
library(tidyverse)
library(tidyr)
library(sf)
library(tigris)
library(StreamCatTools)
library(ggplot2)
library(spmodel)
library(corrplot)
library(remotes)
library(units)
library(fs)
library(readr)
library(janitor)
library(ggpmisc)

# kansas soil data from lena project -------------------------------------------

soilmets <- read_csv("kansas-soil-health.csv")

soilmets <- soilmets |>
  clean_names()

# scary huge corr plot ---------------------------------------------------------

soil_clean <- soilmets |>
  select(!where(is.character))

cormat_pred <- cor(soil_clean, use = "pairwise.complete.obs", method = "spearman")

rhos <- as.vector(cormat_pred)

pos_str_rhos <- length(unique(rhos[rhos > 0.5 & rhos < 1]))
neg_str_rhos <- length(unique(rhos[rhos < -0.5 & rhos > -1]))

corrplot(cormat_pred, method = "ellipse", type = "lower", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.75)

# cor with nitrogen

n_soil <- soilmets |>
  select(no3_n_h2o, nh4_n_h2o, on_h2o_mg_kg, cn_h2o,
         n_h2o, on_release_ppm, h3a_no3, h3a_nh4, 
         x2n_k_cl_no3_n_ppm_n, k_cl_nh4_n_ppm, ace_protein_g_kg, percent_n_acidified,
         x15n_acidified, percent_n_unacidified) 

n_corr <- cor(n_soil, use = "pairwise.complete.obs", method = "spearman")

rhos <- as.vector(n_corr)

pos_str_rhos <- length(unique(rhos[rhos > 0.5 & rhos < 1]))
neg_str_rhos <- length(unique(rhos[rhos < -0.5 & rhos > -1]))

colnames(n_corr) <- soil_name
rownames(n_corr) <- soil_name


corrplot(n_corr, method = "ellipse", type = "lower", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.75)

# haney + other metrics

haney <- soilmets |>
  select(contains("h3a"), on_release_ppm, on_reserve_ppm, n_h2o, nh4_n_h2o, no3_n_h2o,
         x2n_k_cl_no3_n_ppm_n, k_cl_nh4_n_ppm) |>
  mutate(across(everything(), as.numeric)) |>
  select(-c(h3a_in, on_reserve_ppm))

h3_corr <- cor(haney, use = "pairwise.complete.obs", method = "spearman")

rhos <- as.vector(h3_corr)



pos_str_rhos <- length(unique(rhos[rhos > 0.5 & rhos < 1]))
neg_str_rhos <- length(unique(rhos[rhos < -0.5 & rhos > -1]))

corrplot(h3_corr, method = "ellipse", type = "lower", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.75)

# line plots

# H3A_ICAPmg and aa_ca

ggplot(soil_clean, aes(m3_s, h3a_icaps)) +
  geom_point(size = 0.8) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "P", "adj.R2")),
               label.y = .98,
               label.x = 0.02) +
  # scale_y_continuous(trans = "log10") +
  # scale_x_continuous(trans = "log10") +
  ylab("H3A extracted phosphorus") +
  xlab("Mehlich P-III phosphorus") + 
  theme_bw()

ggsave("H3A_P_LINE.jpeg", width = 4, height = 4, device = 'jpeg', units = 'in', dpi = 700)

# corr carbon ------------------------------------------------------------------
carbon <- soilmets |>
  select(cn_h2o, c_n_unacidified, c_n_acidified, x13c_acidified, ace_protein_g_kg, biomass,
         percent_mac, co2_24hr, oc_h2o_mg_kg, om_percent) |>
  mutate(across(everything(), as.numeric)) 

carbon_names <- c(
  'cn_h2o' = 'Water Extractable Organic C:N', 
  'c_n_unacidified' = 'C:N Ratio Acidified', 
  'c_n_acidified' ='C:N Ratio Unacidified', 
  'x13c_acidified' = 'Carbon-13 Isotopes',
  'ace_protein_g_kg' = 'ACE Protein', 
  'biomass' = 'Total Microbial Biomass',
  'percent_mac' = '% Microbially Active Carbon', 
  'co2_24hr' = '24 Hour CO2 Respiration', 
  'oc_h2o_mg_kg' = 'Water Extractable Organic Carbon', 
  'om_percent' = '% Organic Matter')

carbon_corr <- cor(carbon, use = "pairwise.complete.obs", method = "spearman")

colnames(carbon_corr) <- carbon_names
rownames(carbon_corr) <- carbon_names


rhos <- as.vector(carbon_corr)

pos_str_rhos <- length(unique(rhos[rhos > 0.5 & rhos < 1]))
neg_str_rhos <- length(unique(rhos[rhos < -0.5 & rhos > -1]))

corrplot(carbon_corr, method = "ellipse", type = "lower", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.75)

soil_name <- c(
  'no3_n_h2o' = 'Water Extractable Nitrate',
  'nh4_n_h2o' = 'Water Extractable Ammonium',
  'on_h2o_mg_kg' = 'Water Extractable Organic N',
  'cn_h2o' = 'Water Extractable C:N',
  'n_h2o' = 'Water Extractable Total N',
  'on_release_ppm' = 'Organic Nitrogen Release N',
  'h3a_no3' = 'Haney Extracted Nitrate',
  'h3a_nh4' = 'Haney Extracted Ammonium',
  'x2n_k_cl_no3_n_ppm_n' = 'Potassium Chloride Extracted Nitrate',
  'k_cl_nh4_n_ppm' = 'Potassium Chloride Extracted Ammonium',
  'ace_protein_g_kg' = 'ACE Protein',
  'percent_n_acidified' = '% N Acidified',
  'x15n_acidified' = 'Nitrogen-15 Isotopes',
  'percent_n_unacidified' = '% N Unacidified'
)

# PEATS CREEK BY PALMER HUC 12 : 102500170501

library(nhdplusTools)
library(sf)

# Pull a single HUC12
peats <- get_huc(id = "102500170501", type = "huc12")

