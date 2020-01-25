

############# READ IN NYS VOTER FILES #############
nys_roll <- readRDS("./temp/nys_slim.rds") %>% 
  select(last_name = Voters_LastName,
         first_name = Voters_FirstName,
         middle_name = Voters_MiddleName,
         dob = Voters_BirthDate,
         nys_id = Voters_StateVoterID)
  
nys_roll <- cSplit(nys_roll, splitCols = "first_name", type.convert = F, sep = " ") %>% 
  mutate(first_name = first_name_1,
         middle_name = ifelse(is.na(first_name_2), middle_name, first_name_2)) %>% 
  select(-starts_with("first_name_"))

nys_roll <- nys_roll %>%
  mutate_at(vars(last_name, first_name, middle_name),
            ~ gsub("[[:punct:]]| ", "", ifelse(. == "", NA, toupper(.)))) %>% 
  mutate(dob = as.Date(dob, "%m/%d/%Y"))

#### MATCH DOC TO VOTER FILE ####

doccs_to_rolls <- readRDS("./temp/new_reenf_with_rest.rds")
doccs_to_rolls <- doccs_to_rolls %>% 
  rename(last_name = last,
         first_name = first,
         middle_name = middle) %>% 
  mutate_at(vars(last_name, first_name, middle_name),
            ~ gsub("[[:punct:]]| ", "", ifelse(. == "", NA, toupper(.))))

merge_list <- match_rolls_to_doc(doccs_to_rolls, din, nys_roll, nys_id)

small <- merge_list[[1]] %>% 
  select(nys_id, din) %>% 
  filter(!is.na(nys_id), !is.na(din))
  
saveRDS(small, "./temp/din_nys_parolees.rds")
rm(small, merge_list)

#### plus minus 35 ####

doccs_to_rolls <- doccs_to_rolls %>% 
  mutate(dob = dob + 35)

merge_list <- match_rolls_to_doc(doccs_to_rolls, din, nys_roll, nys_id)

small <- merge_list[[1]] %>% 
  select(nys_id, din) %>% 
  filter(!is.na(nys_id), !is.na(din))

saveRDS(nrow(small), "./temp/p35_2.rds")
rm(small, merge_list)
##
doccs_to_rolls <- doccs_to_rolls %>% 
  mutate(dob = dob - 70)

merge_list <- match_rolls_to_doc(doccs_to_rolls, din, nys_roll, nys_id)

small <- merge_list[[1]] %>% 
  select(nys_id, din) %>% 
  filter(!is.na(nys_id), !is.na(din))

saveRDS(nrow(small), "./temp/m35_2.rds")
rm(small, merge_list)

cleanup()