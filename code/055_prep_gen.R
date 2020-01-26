## this can be run locally or on NYU's HPC. Set option in next step
## option allowed because of how long GenMatch can take

on_nyu <- F

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

nys_roll <- readRDS("./temp/nys_slim.rds") %>% 
  select(LALVOTERID, Voters_Gender, Voters_Age, Parties_Description, CommercialData_EstimatedHHIncome,
         CommercialData_Education, US_Congressional_District, EthnicGroups_EthnicGroup1Desc) %>% 
  mutate(gender = Voters_Gender == "F",
         age = Voters_Age,
         dem = Parties_Description == "Democrat",
         college = grepl("Bach|Grad", CommercialData_Education),
         white = EthnicGroups_EthnicGroup1Desc == "European",
         black = EthnicGroups_EthnicGroup1Desc == "Likely African-American",
         latino = EthnicGroups_EthnicGroup1Desc == "Hispanic and Portuguese",
         asian = EthnicGroups_EthnicGroup1Desc == "East and South Asian",
         cd = as.integer(US_Congressional_District)) %>% 
  select(LALVOTERID, gender, age, dem, college, CommercialData_EstimatedHHIncome,
         cd, white, black, latino, asian)

nys_roll <- cSplit(nys_roll, "CommercialData_EstimatedHHIncome", direction = "wide",
                   type.convert = F, sep = "-") %>% 
  mutate(income = (as.integer(gsub("[$]|[+]", "", CommercialData_EstimatedHHIncome_1)) +
                     as.integer(CommercialData_EstimatedHHIncome_2)) / 2,
         income = ifelse(CommercialData_EstimatedHHIncome_1 == "$250000+", 250000, income)) %>% 
  select(-starts_with("Commercial"))

nys_roll$income <- ifelse(is.na(nys_roll$income), mean(nys_roll$income, na.rm = T), nys_roll$income)

nys_roll$treated <- ifelse(nys_roll$LALVOTERID %in% treated1$LALVOTERID, 1, NA)
nys_roll$treated <- ifelse(nys_roll$LALVOTERID %in% control$LALVOTERID, 0, nys_roll$treated)

nys_roll <- filter(nys_roll, !is.na(treated))

#### read history

hist <- readRDS("./temp/nys_history_full.rds") %>% 
  mutate_at(vars(starts_with("General")), ~ 1 * (. == "Y"))

nys_roll <- left_join(nys_roll, hist, by = "LALVOTERID")

#####
nys_roll <- filter(nys_roll, !is.na(cd))

saveRDS(nys_roll, "./temp/pre_gen_roll.rds")
