

nys_roll <- readRDS("./temp/nys_slim.rds")

ids <- readRDS("./temp/din_nys_parolees.rds")


nys_roll$parolee <- nys_roll$Voters_StateVoterID %in% ids$nys_id


hist <- readRDS("./temp/nys_history_full.rds") %>% 
  mutate_at(vars(starts_with("General")), ~ 1 * (. == "Y"))


nys_roll <- left_join(nys_roll, hist)

#####

voting_parolees <- filter(nys_roll, General_2018_11_06 == 1, parolee)

nonvoting_parolees <- filter(nys_roll, General_2018_11_06 == 0, parolee)

####

treated1 <- inner_join(
  filter(nys_roll, !parolee),
  select(voting_parolees, Residence_Addresses_AddressLine, Residence_Addresses_City)
) %>% 
  select(LALVOTERID)

control1 <- inner_join(
  filter(nys_roll, !parolee),
  select(nonvoting_parolees, Residence_Addresses_AddressLine, Residence_Addresses_City)
) %>% 
  select(LALVOTERID)


control2 <- filter(nys_roll, !parolee, !(LALVOTERID %in% treated1$LALVOTERID)) %>% 
  select(LALVOTERID)

save(treated1, control1, control2, file = "./temp/treated_control.rdata")
