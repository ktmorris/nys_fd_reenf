### INDIVIDUALS ON PAROLE AS OF 2018 ELECTION
names <- read.csv("./raw_data/parolee_column_names.csv", header = F, stringsAsFactors=FALSE)$V1 # VARIABLE NAMES SET IN CSV 'CAUSE I'M LAZY

parolees <- read_fwf(
  file = "./raw_data/parolee_data/Parolee.011419.txt",
  fwf_widths(c(1, 9, 3, 10, 3, 7, 3, 30, 3, 6, 3, 10, 3, 15, 3,
               19, 3, 11, 3, 1, 3, 22, 3, 10, 3, 15, 2))
)

colnames(parolees) <- names
rm(names)

parolees <- select(parolees, -starts_with("filler"))

###
names <- read.csv("./raw_data/parolee_crimes_column_names.csv", header = F, stringsAsFactors=FALSE)$V1 # VARIABLE NAMES SET IN CSV 'CAUSE I'M LAZY

parolee_crimes <- read_fwf(
  file = "./raw_data/parolee_data/Parole.Crimes.011419.txt",
  fwf_widths(c(1, 9, 3, 7, 2, 3, 2, 44, 3, 1, 3, 11, 2))
)

colnames(parolee_crimes) <- names
rm(names)

parolee_crimes <- parolee_crimes[parolee_crimes$crime_class %in% c("A", "B", "C", "D", "E"), ]$din

parolees_felons <- parolees %>% 
  filter(din %in% parolee_crimes,
         release_date <= "2018-11-06",
         (status == "ACTIVE" | (status_date > "2018-11-06")))

parolees_felons <- cSplit(parolees_felons, "name", ",")
parolees_felons <- cSplit(parolees_felons, "name_2", " ")

parolees_felons <- parolees_felons %>% 
  select(-name_2_3) %>% 
  rename(last = name_1,
         first = name_2_1,
         middle = name_2_2)

rm(parolee_crimes, parolees)

parolees_felons <- parolees_felons %>% 
  select(din, dob, first, last, middle, race, release_date)

saveRDS(parolees_felons, "./temp/newly_enf.rds")

fwrite(select(parolees_felons, din), "./temp/parolee_dins.csv")
