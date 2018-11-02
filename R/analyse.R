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
  summarise(start = log[12],
            start = str_replace_all(start, "\\[|\\]", ""),
            end = log[grep("Finished job 0", log) - 1],
            end = str_replace_all(end, "\\[|\\]", ""),
            rule = log[13],
            rule = str_replace_all(rule, "^rule |:$", ""),
            input = log[grep("input", log)],
            input = str_replace(input, "^input: ", "")) %>% 
  separate(start, c("wd", "m", "d", "t", "y"), sep = "\\s+") %>% 
  unite(start, "y", "m", "d", "t", sep = "-") %>% 
  select(-wd) %>% 
  separate(end, c("wd", "m", "d", "t", "y"), sep = "\\s+") %>% 
  unite(end, "y", "m", "d", "t", sep = "-") %>% 
  select(-wd) %>% 
  mutate_at(vars(start, end), ymd_hms)

runtime <- finished_logs %>% 
  mutate(runtime = difftime(end, start, units = "mins"),
         run = str_extract(input, "SRR\\d+")) %>% 
  filter(runtime > 0)

runtime %>% 
  filter(runtime > 11) %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = runtime), bins = 30) + 
  facet_wrap(~rule, scales = "free")

runtime %>% 
  filter(runtime < 150, rule == "repeatmasker") %>%
  summarise_at("runtime", funs(min, mean, median, max))

fastq <- read_csv("output/fastq_size.csv")
fastq <- mutate(fastq, run = str_replace_all(dirname(path), "^[[:punct:]]+", ""))

run_size <- select(runtime, run, rule, runtime) %>% 
  inner_join(select(fastq, run, size))

run_size %>% 
  ggplot(mapping = aes(x = size, y = runtime)) +
  geom_point(aes(color = run), position = position_jitter(width = 0.1)) +
  geom_smooth(method = "lm") +
  facet_wrap(~rule, scales = "free") +
  scale_y_continuous() +
  scale_colour_discrete(guide = FALSE)

run_size %>% 
  group_by(rule, big = size > 6e8) %>% 
  summarise_at("runtime", funs(mean, median, max))

#' ## Unfinished jobs
#' 
unfinished_start <- unfinished_logs %>% 
  group_by(path) %>%
  summarise(start = log[12],
            start = str_replace_all(start, "\\[|\\]| wildcard.+", "")) %>% 
  separate(start, c("wd", "m", "d", "t", "y"), sep = "\\s+") %>% 
  unite(start, "y", "m", "d", "t", sep = "-") %>% 
  select(-wd) %>% 
  mutate_at(vars(start), ymd_hms)

unfinished_rule <- unfinished_logs %>% 
  filter(str_detect(log, "^rule")) %>% 
  mutate(rule = str_replace_all(log, "^rule |:$", "")) %>% 
  select(path, rule)

unfinished_cause <- unfinished_logs %>% 
  filter(str_detect(log, "CANCELLED AT")) %>% 
  mutate(cancelled = str_extract(log, "20.+"),
         cause = str_extract(cancelled, "DUE.+"),
         cancelled = str_replace(cancelled, "DUE.+", "")) %>% 
  select(path, cancelled, cause) %>% 
  mutate_at(vars(cancelled, cause), str_replace_all, " |\\*|DUE TO", "") %>% 
  mutate_at("cancelled", ymd_hms)

inner_join(unfinished_start, unfinished_rule) %>% 
  inner_join(unfinished_cause) %>% 
  mutate(runtime = difftime(cancelled, start, units = "mins")) %>% 
  filter(cause == "TIMELIMIT", runtime > 5) %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = runtime), bins = 30) +
  facet_wrap(~ rule, scales = "free")
