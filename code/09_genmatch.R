## this can be run locally or on NYU's HPC. Set option in next step
## option allowed because of how long GenMatch can take

on_nyu <- T

if(on_nyu){
  library(Matching)
  library(data.table)
  library(snow)
  library(parallel)
  library(scales)
  library(kableExtra)
  library(tidyverse)
  
  setwd("/scratch/km3815/matching")
  
  NodeFile = Sys.getenv("MY_HOSTFILE")
  
  
  cl <- makeCluster(c(readLines(NodeFile)), type="SOCK")
}else{
  source("./code/misc/AutoCluster4.R")
  cl <- NCPUS(detectCores() - 1)
}

load("./temp/treated_control.rdata")

nys_roll <- readRDS("./temp/pre_gen_roll.rds") 

samp <- rbind(
  filter(nys_roll, treated == 1),
  filter(nys_roll, !treated) %>% 
    sample_frac(0.01)
)

match_data <- samp %>% 
  select(-LALVOTERID, -treated, -starts_with("General"))

genout <- GenMatch(Tr = samp$treated, X = match_data, replace = T, cluster = cl,
                   exact = c(rep(F, 4), T, rep(F, 5)))
saveRDS(genout, "./temp/genout_t1.rds")
