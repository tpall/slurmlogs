library(httr)
library(tidyverse)
library(glue)

project <- "PRJNA361402"
study <- "SRP100518"

#' Retrieve fastq file info and urls
fq <- GET(glue("http://www.ebi.ac.uk/ena/data/warehouse/filereport?accession={study}&result=read_run"))
fq_ftp <- content(fq, encoding = "UTF-8") %>% read_delim(delim = "\t")

samples <- select(fq_ftp, experiment_title, run_accession, library_name, read_count, base_count, fastq_bytes, fastq_md5, fastq_ftp)

ftp_paths <- samples %>% 
  separate(fastq_ftp, into = c("fq1", "fq2"), sep = ";") %>% 
  separate(fastq_bytes, into = c("fq1_bytes", "fq2_bytes"), sep = ";") %>%
  separate(fastq_md5, into = c("fq1_md5", "fq2_md5"), sep = ";") %>% 
  rename(sample = run_accession)
