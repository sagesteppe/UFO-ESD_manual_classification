library(tidyverse)
set.seed(138)

read.csv('../data/raw/Plot_Tracking_ESDs.csv') %>% 
  filter(PANEL == 2018 & STATUS == 'SAMPLED') %>% 
  select(PLOT.ID:PANEL) %>% 
  mutate(EVALUATOR = sample(c('R', 'K', 'P'), size = 36, replace = T)) %>% 
  write.csv(., '../data/processed/2018ESDs_Assigned.csv', row.names = F)

