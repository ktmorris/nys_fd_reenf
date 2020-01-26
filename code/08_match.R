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
  library(splitstackshape)
  
  setwd("/scratch/km3815/matching")
  
  NodeFile = Sys.getenv("MY_HOSTFILE")
  
  
  cl <- makeCluster(c(readLines(NodeFile)), type="SOCK")
}else{
  source("./code/misc/AutoCluster4.R")
  cl <- NCPUS(detectCores() - 1)
}



#####
nys_roll <- readRDS("./temp/pre_gen_roll.rds")

##########

ids <- nys_roll %>% 
  mutate(id = row_number()) %>% 
  select(id, LALVOTERID)

X <- nys_roll %>%
  dplyr::select(-LALVOTERID, -treated, -starts_with("General"))


genout <- readRDS("./temp/genout_t1.rds")

mout <- Matchby(Tr = nys_roll$treated, X = X, by = c(X$cd), estimand = "ATT", Weight.matrix = genout, M = 3)

save(mout, file = "./temp/mout_t1.RData")

load("./temp/mout_t1.RData")


m1 <- data.frame("match" = mout[["index.treated"]],
                 "voter" = mout[["index.control"]],
                 "treated" = F)

m2 <- data.frame("match" = unique(m1$match),
                 "voter" = unique(m1$match),
                 "treated" = T)
matches <- bind_rows(m1, m2)

matches <- left_join(matches, ids, by = c("voter" = "id")) %>% 
  select(-voter)

######

l1 <- melt(select(nys_roll, LALVOTERID, starts_with("GENERAL")),
       id.vars = "LALVOTERID")

nys_roll_long <- left_join(l1,
  select(nys_roll, -starts_with("GENERAL"))) %>% 
  mutate(year = as.integer(substring(as.character(variable), 9, 12))) %>% 
  rename(voted = value) %>% 
  select(-variable)

roll_long <- left_join(matches, nys_roll_long, by = c("LALVOTERID", "treated"))
roll_long$midterm <- roll_long$year %% 4 == 2

saveRDS(roll_long, "./temp/nys_roll_long.rds")


#####

summary(glm(voted ~ treated * I(year == 2018) + midterm, data = roll_long))

summary(glm.cluster(formula = voted ~ treated * I(year == 2018) + treated * midterm +
                      gender + age + dem + college + white + black + latino +
                      asian + income + as.factor(cd),
                    data = roll_long, cluster = roll_long$match))


#####

ll <- roll_long %>% 
  group_by(treated, year) %>% 
  summarize(v = mean(voted))

ggplot(ll, aes(x = year, y = v)) + geom_line(aes(color = as.factor(treated)))



