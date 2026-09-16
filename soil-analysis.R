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

# kansas soil data from lena project -------------------------------------------

soilmets <- read_csv("kansas-soil-health.csv")

soilmets <- soilmets |>
  clean_names()

# cor plot? --------------------------------------------------------------------

soil_clean <- soilmets |>
  select(!where(is.character))

cormat_pred <- cor(soil_clean, use = "pairwise.complete.obs", method = "spearman")

colnames(cormat_pred) <- var_names
rownames(cormat_pred) <- var_names

# How many pairwise comparisons are strongly correlated?
rhos <- as.vector(cormat_pred)

pos_str_rhos <- length(unique(rhos[rhos > 0.5 & rhos < 1]))
neg_str_rhos <- length(unique(rhos[rhos < -0.5 & rhos > -1]))

corrplot(cormat_pred, method = "ellipse", type = "lower", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.75,
         col = redblue_palette(100))

