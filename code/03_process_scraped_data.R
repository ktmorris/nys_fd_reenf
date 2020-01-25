
### AFTER THE WEBSCRAPER FINISHES, THIS PROGRAM LOOKS TO SEE WHO'S HAD THEIR RIGHTS RESTORED

m <- fread("./temp/nys_parole_codes.csv")

ids <- m %>% 
  filter(V1 == "DIN:") %>% 
  group_by(V2) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  mutate(id = row_number()) %>% 
  select(V2, id)

full <- full_join(m, ids, by = "V2")


for (i in 1:100){
  full <- full %>% 
    mutate(id = if_else(is.na(id), lag(id), id))
}

full <- full %>% 
  select(-V3) %>% 
  group_by(V1, id) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  mutate(V1 = gsub(" ", "_", gsub(" /", "", gsub(":","",V1)))) %>% 
  spread(V1, V2) %>% 
  mutate(Release_to_parole_supervision = as.Date(Release_to_parole_supervision, "%m/%d/%Y"),
         restored = tolower(Voting_pardon_issued) == "yes") %>% 
  select(din = DIN, restored, prison_release = Release_to_parole_supervision,
         status = Parole_status, effective_date = Effective_date)
full$effective_date <- as.Date(full$effective_date, "%m/%d/%Y")

rm(m, ids, i)

###############


new_potential <- full %>% 
  filter(status == "Active" | effective_date >= "2018-11-06",
         prison_release <= "2018-11-06")


all_data <- readRDS("./temp/newly_enf.rds") %>% 
  filter(din %in% new_potential$din)


all_data <- inner_join(all_data, new_potential) %>% 
  group_by(din) %>% 
  filter(row_number() == 1) %>% 
  ungroup()


saveRDS(all_data, "./temp/new_reenf_with_rest.rds")
