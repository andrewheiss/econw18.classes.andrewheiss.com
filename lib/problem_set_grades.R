library(tidyverse)
library(here)

problem_sets <- read_csv(file.path(here(), "private", "problem_sets.csv"))

problem_set_summary <- problem_sets %>% 
  gather(key, value, -c(`Last Name`, `First Name`, `Net ID`)) %>% 
  mutate(name = paste(`First Name`, `Last Name`)) %>% 
  group_by(name) %>% 
  summarize(n = sum(!is.na(value))) %>% 
  arrange(desc(n))
