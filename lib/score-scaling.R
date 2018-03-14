library(tidyverse)
library(patchwork)
library(fuzzyjoin)
library(here)

grades <- tribble(
  ~grade, ~lower, ~upper,
  "A",    93,     150,
  "A-",   90,     93,
  "B+",   87,     90,
  "B",    83,     87,
  "B-",   80,     83,
  "C+",   77,     80,
  "C",    73,     77,
  "C-",   70,     73,
  "D+",   67,     70,
  "D",    63,     67,
  "D-",   60,     63,
  "F",    0,      60
) %>% 
  mutate_at(vars(lower, upper), funs(. * 0.01)) %>% 
  mutate(grade = fct_inorder(grade, ordered = TRUE))

midterm_1_raw <- read_csv(file.path(here(), "private", "midterm_1.csv"))

midterm_1_adjusted <- midterm_1_raw %>% 
  rename(score = `Midterm 1`) %>% 
  mutate(pct = score / 150) %>% 
  mutate(adjusted = score + 3,
         pct_new = adjusted / 150) %>% 
  fuzzy_left_join(grades, by = c("pct_new" = "lower", "pct_new" = "upper"),
                  match_fun = list(`>=`, `<`))

plot_midterm <- midterm_1_adjusted %>% 
  count(grade)

midterm_grades <- ggplot(midterm_1_adjusted, aes(x = adjusted)) +
  geom_histogram(binwidth = 2) +
  theme_light()

midterm_scores <- ggplot(plot_midterm, aes(x = fct_rev(grade), y = n)) + 
  geom_bar(stat = "identity") +
  theme_light()

midterm_grades + midterm_scores + plot_layout(ncol = 1)
