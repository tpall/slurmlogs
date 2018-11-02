library(tidyverse)
slogs <- list.files("logs/slurm", full.names = TRUE)
slogs <- data_frame(path = slogs)
slogs <- mutate(slogs, log = map(path, read_lines))
slogs_unnested <- unnest(slogs)
write_csv(slogs_unnested, "slogs_unnested.csv")

paths <- data_frame(dir = list.files(pattern = "SRR", full.names = TRUE))
fastq <- paths %>% 
  mutate(subdir = map(dir, list.files, full.names = TRUE)) %>% 
  unnest() %>% 
  gather(key = dir, value = path) %>% 
  filter(str_detect(path, "fastq.gz")) %>% 
  mutate(size = map_dbl(path, file.size))
write_csv(fastq, "fastq_size.csv")
