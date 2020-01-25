

nys_roll <- readRDS("./temp/nys_slim.rds")

nys_roll$voted <- nys_roll$LALVOTERID %in%
  filter(readRDS("./temp/nys_history_full.rds"), General_2018_11_06 == "Y")$LALVOTERID

nys_roll <- nys_roll %>% 
  mutate(Voters_LastName = gsub("[[:punct:]]| ", "", ifelse(Voters_LastName == "", NA, toupper(Voters_LastName))),
         Residence_Addresses_Latitude = round(as.numeric(Residence_Addresses_Latitude), 4),
         Residence_Addresses_Longitude = round(as.numeric(Residence_Addresses_Longitude), 4))

nys_roll$parole <- nys_roll$Voters_StateVoterID %in% readRDS("./temp/din_nys_parolees.rds")$nys_id

##### treatment definition 1
parolee_addresses <- nys_roll %>% 
  filter(parole, voted) %>% 
  select(Residence_Addresses_AddressLine)


treated1 <- nys_roll %>% 
  filter(!parole,
         Residence_Addresses_AddressLine %in% parolee_addresses$Residence_Addresses_AddressLine) %>% 
  select(LALVOTERID)
#########

parolee_locs <- nys_roll %>% 
  filter(parole, voted) %>% 
  select(Residence_Addresses_Latitude,
         Residence_Addresses_Longitude,
         Voters_LastName)

treated2 <- inner_join(
  nys_roll %>% 
    filter(!parole),
  parolee_locs,
  by = c("Residence_Addresses_Latitude",
         "Residence_Addresses_Longitude",
         "Voters_LastName")) %>% 
  select(LALVOTERID)

########
control <- nys_roll %>% 
  filter(!(LALVOTERID %in% treated1$LALVOTERID),
         !(LALVOTERID %in% treated2$LALVOTERID),
         !parole) %>% 
  select(LALVOTERID)


save(treated1, treated2, control, file = "./temp/treated_control.rdata")
