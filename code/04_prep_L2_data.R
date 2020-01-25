
f <- c("D:/national/l2-11-11-2019-delivery/VM2--NY--2019-10-09/VM2--NY--2019-10-09-DEMOGRAPHIC.tab")

k <- sqldf::read.csv.sql(f, sep = "\t",
                         sql = "select LALVOTERID, 
        Voters_Active, 
        Voters_StateVoterID, 
        Voters_FirstName, 
        Voters_MiddleName, 
        Voters_LastName, 
        Voters_NameSuffix, 
        Residence_Addresses_AddressLine, 
        Residence_Addresses_ExtraAddressLine, 
        Residence_Addresses_City, 
        Residence_Addresses_State, 
        Residence_Addresses_Zip, 
        Residence_Addresses_ZipPlus4, 
        Residence_Addresses_HouseNumber, 
        Residence_Addresses_PrefixDirection, 
        Residence_Addresses_StreetName, 
        Residence_Addresses_Designator, 
        Residence_Addresses_SuffixDirection, 
        Residence_Addresses_ApartmentNum, 
        Residence_Addresses_CensusTract, 
        Residence_Addresses_CensusBlockGroup, 
        Residence_Addresses_CensusBlock, 
        Residence_Addresses_Latitude, 
        Residence_Addresses_Longitude,
        Voters_Gender, 
        Voters_Age, 
        Voters_BirthDate, 
        DateConfidence_Description, 
        Parties_Description,
        US_Congressional_District,
        Ethnic_Description, 
        EthnicGroups_EthnicGroup1Desc,
        US_Congressional_District, 
        State_Senate_District, State_House_District, 
        State_Legislative_District, County, Voters_FIPS, 
        CommercialData_EstimatedHHIncome,
        CommercialData_Education,
        Precinct from file")      

saveRDS(k, "./temp/nys_slim.rds")


##### history full

f <- c("D:/national/l2-11-11-2019-delivery/VM2--NY--2019-10-09/VM2--NY--2019-10-09-VOTEHISTORY.tab")

k <- sqldf::read.csv.sql(f, sep = "\t",
                         sql = "select LALVOTERID, 
                                General_2018_11_06,
                                General_2016_11_08,
                                General_2014_11_04,
                                General_2012_11_06,
                                General_2010_11_02
                                from file")      

saveRDS(k, "./temp/nys_history_full.rds")
