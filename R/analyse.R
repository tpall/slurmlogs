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
  summarise(start = log[grep("\\[",log)[1]],
            start = str_replace_all(start, "\\[|\\]", ""),
            end = log[grep("Finished job 0", log) - 1],
            end = str_replace_all(end, "\\[|\\]", ""),
            rule = log[grep("\\[",log)[1] + 1],
            rule = str_replace_all(rule, "^rule |:$", ""),
            sample = unique(str_extract(log[grep("input", log)], "SRR\\d+"))[1]) %>% 
  .[-3759,] %>% 
  separate(start, c("wd", "m", "d", "t", "y"), sep = "\\s+") %>% 
  unite(start, "y", "m", "d", "t", sep = "-") %>% 
  select(-wd) %>% 
  separate(end, c("wd", "m", "d", "t", "y"), sep = "\\s+") %>% 
  unite(end, "y", "m", "d", "t", sep = "-") %>% 
  select(-wd) %>% 
  mutate_at(vars(start, end), ymd_hms)

runtime <- finished_logs %>% 
  mutate(runtime = difftime(end, start, units = "mins")) %>% 
  filter(runtime > 0) %>% 
  select(sample, start:runtime)

runtime %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = runtime), bins = 30) + 
  facet_wrap(~rule, scales = "free") +
  scale_x_continuous()

runtime %>% 
  filter(rule == "repeatmasker") %>%
  summarise_at("runtime", funs(min, mean, median, max))

runs <- read_tsv("output/samples_remote.tsv")

run_size <- inner_join(runtime, select(runs, sample, read_count, base_count, fq1_bytes)) %>% 
  mutate(runtime = parse_number(runtime)) %>% 
  distinct()

run_size %>% 
  select(fq1_bytes) %>% 
  distinct() %>% 
  ggplot() +
  geom_histogram(aes(fq1_bytes / 1048576))

filter(run_size, (fq1_bytes / 1048576) < 500) %>% 
  select(fq1_bytes) %>% 
  distinct() %>% 
  summarise_all(funs(mean, sd, max))


run_size %>% 
  ggplot(mapping = aes(x = fq1_bytes, y = runtime)) +
  geom_point(aes(color = sample), position = position_jitter(width = 0.1)) +
  geom_smooth(method = "lm") +
  facet_wrap(~ rule, scales = "free") +
  scale_y_continuous() +
  scale_colour_discrete(guide = FALSE)

run_size %>% 
  group_by(rule, big = fq1_bytes > 6e8) %>% 
  summarise_at("runtime", funs(mean, median, max))

#' Fit models for each program
models <- run_size %>% 
  group_by(rule) %>% 
  nest() %>% 
  filter(map_lgl(data, ~nrow(.x) > 10)) %>% 
  mutate(mod = map(data, ~broom::tidy(lm(runtime ~ fq1_bytes, data = .x))))

models %>% 
  select(rule, mod) %>% 
  unnest() %>% 
  select(rule, term, estimate) %>% 
  spread(key = term, value = estimate) %>% 
  rename_all(str_replace_all, "\\)|\\(", "") %>% 
  filter(fq1_bytes > 0) %>% 
  mutate(mb300 = Intercept + (300 * 1048576) * fq1_bytes,
         mb3000 = Intercept + (3000 * 1048576) * fq1_bytes) %>% 
  mutate_at(vars(starts_with("mb")), ~ .x / 60)

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
