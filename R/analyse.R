library(tidyverse)
library(lubridate)

slogs_unnested <- read_csv("output/slogs_unnested.csv")
(finished <- filter(slogs_unnested, str_detect(log, "Finished job 0")))
(finished_logs <- semi_join(slogs_unnested, finished, by = "path"))
(unfinished_logs <- anti_join(slogs_unnested, finished, by = "path"))
unfinished_logs_nested <- unfinished_logs %>% 
  group_by(path) %>% 
  nest()

finished_logs <- finished_logs %>% 
  group_by(path) %>% 
  summarise(start = log[grep("\\[", log)[1]],
          start = str_replace_all(start, "\\[|\\]", ""),
          end = log[grep("Finished job 0", log) - 1],
          end = str_replace_all(end, "\\[|\\]", ""),
          rule = log[grep("\\[", log)[1] + 1],
          rule = str_replace_all(rule, "^rule |:$", "")) %>% 
  .[-3759,] %>% 
  separate(start, c("wd", "m", "d", "t", "y"), sep = "\\s+") %>% 
  unite(start, "y", "m", "d", "t", sep = "-") %>% 
  select(-wd) %>% 
  separate(end, c("wd", "m", "d", "t", "y"), sep = "\\s+") %>% 
  unite(end, "y", "m", "d", "t", sep = "-") %>% 
  select(-wd) %>% 
  mutate_at(vars(start, end), ymd_hms)

runtime <- finished_logs %>% 
  mutate(runtime = difftime(end, start, units = "mins"),
         run = str_extract(path, "SRR\\d+")) %>% 
  filter(runtime > 0) %>% 
  select(run, everything())

runtime %>% 
  filter(runtime > 11) %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = runtime), bins = 30) + 
  facet_wrap(~rule, scales = "free")

runtime %>% 
  filter(runtime < 150, rule == "repeatmasker") %>%
  summarise_at("runtime", funs(min, mean, median, max))

run_size <- inner_join(rename(runtime, sample = run), samples)

run_size %>% 
  ggplot(mapping = aes(x = read_count, y = runtime)) +
  geom_point(aes(color = sample), position = position_jitter(width = 0.1)) +
  geom_smooth(method = "lm") +
  facet_wrap(~rule, scales = "free") +
  scale_y_continuous() +
  scale_colour_discrete(guide = FALSE)

run_size %>% 
  group_by(rule, big = size > 6e8) %>% 
  summarise_at("runtime", funs(mean, median, max))

