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

# mout <- Matchby(Tr = nys_roll$treated, X = X, by = c(X$cd), estimand = "ATT", Weight.matrix = genout, M = 3)
# 
# save(mout, file = "./temp/mout_t1.RData")

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
roll_long <- readRDS("./temp/nys_roll_long.rds")

roll_long$weight <- ifelse(roll_long$treated, 3, 1)
roll_long$pres <- 1 - roll_long$midterm

model1 <- glm(voted ~ treated * pres + treated * I(year == 2018), data = roll_long, weight = weight)

model1_ses <- summary(glm.cluster(formula = voted ~ treated * pres + treated * I(year == 2018),
                    data = roll_long, cluster = roll_long$match, weight = roll_long$weight))[ ,2]

model2 <- glm(voted ~ treated * pres + treated * I(year == 2018) +
                gender + age + dem + college + white + black + latino +
                asian + income + as.factor(cd), data = roll_long, weight = weight)

model2_ses <- summary(glm.cluster(formula = voted ~ treated * pres + treated * I(year == 2018) +
                                    gender + age + dem + college + white + black + latino +
                                    asian + income + as.factor(cd),
                                  data = roll_long, cluster = roll_long$match, weight = roll_long$weight))[ ,2]

save(model1, model1_ses, model2, model2_ses, file = "./temp/reg1.rdata")

##### charts

m1 <- as.data.frame(confint.default(glm(voted ~ as.factor(year), data = filter(roll_long, treated == 1)))) %>% 
  mutate(year = seq(2010, 2018, 2))

colnames(m1) <- c("lower", "upper", "year")

m1 <- m1 %>% 
  mutate(t = max((year == 2010) * (lower + upper) / 2),
         est = ifelse(year == 2010, t, t + (lower + upper) / 2),
         lower = ifelse(year == 2010, lower, lower + t),
         upper = ifelse(year == 2010, upper, upper + t),
         treated = "Treated Group") %>% 
  select(-t)

m2 <- as.data.frame(confint.default(glm(voted ~ as.factor(year), data = filter(roll_long, treated == 0)))) %>% 
  mutate(year = seq(2010, 2018, 2))

colnames(m2) <- c("lower", "upper", "year")

m2 <- m2 %>% 
  mutate(t = max((year == 2010) * (lower + upper) / 2),
         est = ifelse(year == 2010, t, t + (lower + upper) / 2),
         lower = ifelse(year == 2010, lower, lower + t),
         upper = ifelse(year == 2010, upper, upper + t),
         treated = "Actual Control Group") %>% 
  select(-t)




fl <- melt(select(filter(nys_roll, treated == 0), treated, starts_with("General")), id.vars = "treated") %>% 
  mutate(year = as.integer(substring(as.character(variable), 9, 12))) %>% 
  rename(voted = value)

m3 <- as.data.frame(confint.default(glm(voted ~ as.factor(year), data = fl))) %>% 
  mutate(year = seq(2010, 2018, 2))

colnames(m3) <- c("lower", "upper", "year")

m3 <- m3 %>% 
  mutate(t = max((year == 2010) * (lower + upper) / 2),
         est = ifelse(year == 2010, t, t + (lower + upper) / 2),
         lower = ifelse(year == 2010, lower, lower + t),
         upper = ifelse(year == 2010, upper, upper + t),
         treated = "Potential Control Group") %>% 
  select(-t)


chart <- bind_rows(m1, m2, m3)


plot <- ggplot(chart, aes(x = year, y = est, group = treated, linetype = treated, shape = treated)) + 
  geom_line() +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_y_continuous(labels = percent) +
  labs(x = "Year", y = "Turnout Among Registered Voters",
       caption = "Notes: 95% confidence intervals shown.") +
  theme_minimal() + theme(legend.title = element_blank(),
                          plot.caption = element_text(hjust = 0),
                          text = element_text(family = "LM Roman 10"))
saveRDS(plot, "./temp/timeline.rds")


###########

load("./temp/mout_t1.RData")
order <- fread("./raw_data/misc/var_orders.csv")

match_full <- left_join(matches, nys_roll)

means_prematch <- nys_roll %>% 
  group_by(treated) %>% 
  summarize_at(vars("gender", "age", "dem", "college", "white", "black", "latino",
                    "asian", "income"), mean)

means_postmatch <- match_full %>% 
  group_by(treated) %>% 
  summarize_at(vars("gender", "age", "dem", "college", "white", "black", "latino",
                    "asian", "income"), mean)

qqs_post <- lapply(c("gender", "age", "dem", "college", "white", "black", "latino",
                     "asian", "income"), function(var){
  j <- select(match_full, var, treated)
  colnames(j) <- c("t", "treated")
  
  qqout  <- qqstats(j$t[j$treated == T], j$t[j$treated == F])
  return(qqout)
})

qqs_pre <- lapply(c("gender", "age", "dem", "college", "white", "black", "latino",
                    "asian", "income"), function(var){
  j <- select(nys_roll, var, treated)
  colnames(j) <- c("t", "treated")
  
  qqout  <- qqstats(j$t[j$treated == T], j$t[j$treated == F])
  return(qqout)
})


TrMean <- c()
PreMean <- c()
PreQQmed <- c()
PreQQmean <- c()
PreQQmax <- c()
PostMean <- c()
PostQQmed <- c()
PostQQmean <- c()
PostQQmax <- c()

i = 1
for(var in c("gender", "age", "dem", "college", "white", "black", "latino",
             "asian", "income")){
  TrMean <- unlist(c(TrMean, filter(means_prematch, treated == T) %>% select(var) %>% pull()))
  PreMean <- unlist(c(PreMean, filter(means_prematch, treated == F) %>% select(var) %>% pull()))
  
  PreQQmed <- unlist(c(PreQQmed, qqs_pre[[i]][["mediandiff"]]))
  PreQQmean <- unlist(c(PreQQmean, qqs_pre[[i]][["meandiff"]]))
  PreQQmax <- unlist(c(PreQQmax, qqs_pre[[i]][["maxdiff"]]))
  
  PostMean <- unlist(c(PostMean, filter(means_postmatch, treated == F) %>% select(var) %>% pull()))
  PostQQmed <- unlist(c(PostQQmed, qqs_post[[i]][["mediandiff"]]))
  PostQQmean <- unlist(c(PostQQmean, qqs_post[[i]][["meandiff"]]))
  PostQQmax <- unlist(c(PostQQmax, qqs_post[[i]][["maxdiff"]]))
  
  i = i + 1
}



varnames <- c("gender", "age", "dem", "college", "white", "black", "latino",
              "asian", "income")

df <- data.frame("TrMean" = TrMean,
                 "TrMean2" = TrMean,
                 "PreMean" = PreMean,
                 "PreQQmed" = PreQQmed,
                 "PreQQmean" = PreQQmean,
                 "PreQQmax" = PreQQmax,
                 "PostMean" = PostMean,
                 "PostQQmed" = PostQQmed,
                 "PostQQmean" = PostQQmean,
                 "PostQQmax" = PostQQmax,
                 "names" = varnames) %>%
  mutate(change_mean = 1 - (abs(TrMean - PostMean) / abs(TrMean - PreMean)),
         change_eqqmed = 1 - abs(PostQQmed / PreQQmed),
         change_eqqmean = 1 - abs(PostQQmean / PreQQmean),
         change_eqqmax = 1 - abs(PostQQmax / PreQQmax)) %>%
  mutate_at(vars(TrMean, PreMean, TrMean2, PostMean), funs(comma(round(., 2), accuracy = .01))) %>%
  mutate_at(vars(change_mean, change_eqqmed, change_eqqmean, change_eqqmax), funs(round(. * 100, 2)))

df <- full_join(df, order, by = c("names" = "variable")) %>%
  arrange(order) %>%
  select(name, TrMean, PreMean, TrMean2, PostMean, change_mean, change_eqqmed, change_eqqmean, change_eqqmax) %>%
  filter(!is.na(TrMean))

colnames(df) <- c("", "Treated", "Control", "Treated", "Control", "Mean Diff", "eQQ Med", "eQQ Mean", "eQQ Max")

saveRDS(df, "./temp/match_table_t1.rds")
