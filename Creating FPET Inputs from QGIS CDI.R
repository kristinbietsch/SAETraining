# RCode to combine QGIS Results

library(tidyverse)
#library(readxl)
#library(zip)
#library(stringr)
#library(stringi)
#library(readr)
#library(openxlsx)
#library(xlsx)
library(zoo)

options(scipen = 999)

# Country ISO Code
# Change to your country's ISO code
# Burkina Faso: 854
# Cote d'Ivoire: 384
# Senegal: 686
iso <- 384

# Change this year to the earliest survey you created PrevR files for
first_survey <- 1994 

#########################################################################################################################################
# Change the file location to where you have the National Population file (used in FPET) stored

pop_nat <- read.csv("C:/Users/KristinBietsch/Avenir Health Dropbox/Kristin kbietsch@avenirhealth.org/Avenir Track20/Training Materials/Small Area Estimates 2026/Burkina Faso/Data/Population/UNPD 2024 Population 091225.csv")

#########################################################################################################################################
# Change the file location to where you have the National Survey file (used in FPET) stored

nat_survey <- read.csv("C:/Users/KristinBietsch/Avenir Health Dropbox/Kristin kbietsch@avenirhealth.org/Avenir Track20/Training Materials/Small Area Estimates 2026/Burkina Faso/Data/Track20 2025 Database for FPET2 052925.csv") 

##########################################################################################################
national <- nat_survey %>% filter(division_numeric_code==iso) 

# Run this portion of code and confirm that it has all the surveys (and no extras) that you created PrevR files for
year_info <- national %>% 
  select(-contraceptive_use_any, -contraceptive_use_modern, -contraceptive_use_traditional, -unmet_need_any, -region_code, -is_in_union, -group_type_relative_to_baseline) %>% 
  mutate(se_modern=NA, 
         se_traditional=NA,
         se_unmet_need=NA,
         se_log_r_modern_no_use=NA,
         se_log_r_traditional_no_use=NA,
         se_log_r_unmet_no_need=NA) %>%
  group_by(start_date) %>%
  summarise(end_date=first(end_date),
            division_numeric_code=first(division_numeric_code),
            end_date=first(end_date),
            age_range=first(age_range),
            data_series_type=first(data_series_type),
            unmet_need_modern=first(unmet_need_modern),
            is_pertaining_to_methods_used_since_last_pregnancy=first(is_pertaining_to_methods_used_since_last_pregnancy),
            pertaining_to_methods_used_since_last_pregnancy_reason=first(pertaining_to_methods_used_since_last_pregnancy_reason),
            has_geographical_region_bias=first(has_geographical_region_bias),
            geographical_region_bias_reason=first(geographical_region_bias_reason),
            has_non_pregnant_and_other_positive_biases=first(has_non_pregnant_and_other_positive_biases),
            non_pregnant_and_other_positive_biases_reason=first(non_pregnant_and_other_positive_biases_reason),
            age_group_bias=first(age_group_bias),
            modern_method_bias=first(modern_method_bias),
            has_traditional_method_bias=first(has_traditional_method_bias),
            traditional_method_bias_reason=first(traditional_method_bias_reason),
            has_absence_of_probing_questions_bias=first(has_absence_of_probing_questions_bias),
            se_modern=first(se_modern),
            se_traditional=first(se_traditional),
            se_unmet_need=first(se_unmet_need),
            se_log_r_modern_no_use=first(se_log_r_modern_no_use),
            se_log_r_traditional_no_use=first(se_log_r_traditional_no_use),
            se_log_r_unmet_no_need=first(se_log_r_unmet_no_need),
            source_id=first(source_id),
            record_id=first(record_id),
            possible_outlier=first(possible_outlier),
            possible_outlier_userinput=first(possible_outlier_userinput)) %>% 
  mutate(Year =floor(start_date)) %>%
  filter(data_series_type=="DHS") %>%
  filter(Year>=first_survey)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Change this directory to where your QGIS results are saved
setwd("C:/Users/KristinBietsch/Avenir Health Dropbox/Kristin kbietsch@avenirhealth.org/Avenir Track20/Training Materials/Small Area Estimates 2026/Cote dIvoire/Data/QGIS Results")


filelist <- as.data.frame(list.files(path = ".", pattern = NULL, all.files = FALSE,
                                     full.names = FALSE, recursive = FALSE,
                                     ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))  %>%
  rename(CSV=1) 

# Change this to match the name of the first file in your folder
example_dataset <- "Results1.csv"

results_long <- read.csv(example_dataset) 

# Now that you read in the dataset, look how many columns there are
# In the Burkina Faso, there are 29 columns.  
# If there are a different number of columns, change the "29" that appears in the line of code below and twice in the loop to the number of columns you have

results_long1 <- results_long %>% rename(Value=27) %>% mutate(Variable="") %>% filter(Value==100000000)

# Highlight the entire loop and run all together

for (row in 1:nrow(filelist)) {
  csv_file <-  filelist[row, "CSV"]
  
  data <- read.csv(csv_file) 
  
  name_list <- names(data)
  varname <- name_list[27]  
  
  data_clean <- data %>% rename(Value=27) %>%
    mutate(Variable=varname)
  
  results_long1 <- bind_rows(results_long1, data_clean)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# We may need to edit this code for your country based off of how things were named in QGIS

# first open your dataset and look for the clean admin variable
# Mine is adm2_name 
# Change the second line of code match the name

results_long1_clean <- results_long1 %>%
  rename(region_code=adm2_name) %>%
  select(region_code, Variable, Value) %>%
  mutate(Dash_Count=str_count(Variable, pattern = "_")) %>%
  mutate(Variable=case_when(Dash_Count==3 ~ paste("pop_", Variable, sep=""),
                            TRUE ~ Variable)) %>% # adding a _ in front so they have the same number of columns
  separate(Variable, c("V", "P", "P2", "Y", "S")) %>%
  mutate(Year=case_when(as.numeric(Y) > 80 ~ 1900 + as.numeric(Y),
                        TRUE ~ 2000 + as.numeric(Y))) %>%
  mutate(Variable_type=V) %>%
  mutate(Pop_type=P) %>%
  select(region_code, Year, Variable_type, Pop_type, Value) %>%
  spread(Variable_type, Value) %>%
  mutate(contraceptive_use_modern= mcpr/pop,
         contraceptive_use_traditional= tcpr/pop,
         unmet_need_any= unmet/pop) %>%
  mutate(group_type_relative_to_baseline=case_when(Pop_type=="mar" ~ "MW",
                                                   Pop_type=="unmar" ~ "UW")) %>%
  mutate(region_code=case_when(region_code=="Yakass\xe9-Attobrou" ~ "Yakasse Attobrou",
                               region_code=="Adiak\xe9"   ~ "Adiake",
                               region_code=="Ferkess\xe9dougou" ~ "Ferkessedougou",
                               region_code=="Gu\xe9yo" ~ "Gueyo",
                               region_code== "Adzop\xe9" ~ "Adzope",
                               region_code== "Ti\xe9bissou" ~ "Tiebissou",
                               region_code=="S\xe9gu\xe9lon"  ~ "Seguelon",
                               region_code=="M'Bengu\xe9" ~ "M'Bengue",
                               region_code== "Atti\xe9gouakro" ~ "Attiegouakro",
                               region_code=="B\xe9tti\xe9"  ~ "Bettie",
                               region_code== "Danan\xe9" ~ "Danane",
                               region_code== "Bouak\xe9" ~ "Bouake",
                               region_code== "B\xe9oumi" ~ "Beoumi",
                               region_code== "Zu\xe9noula"   ~ "Zuenoula",
                               region_code=="Akoup\xe9"  ~ "Akoupe",
                               region_code=="Al\xe9p\xe9"  ~ "Alepe",
                               region_code=="Agnibil\xe9krou"  ~ "Agnibilekrou",
                               region_code== "Bouafl\xe9"   ~ "Bouafle",
                               region_code=="M\xe9agui" ~ "Meagui",
                               region_code==  "Oum\xe9"   ~ "Oume",
                               region_code=="S\xe9gu\xe9la"  ~ "Seguela",
                               region_code=="Blol\xe9quin"  ~ "Blolequin",
                               region_code=="San P\xe9dro" ~ "San Pedro",
                               region_code=="Soubr\xe9"  ~ "Soubre",
                               region_code==  "Tiassal\xe9" ~ "Tiassale",
                               region_code== "Ta\xef"   ~ "Tai",
                               region_code=="Du\xe9kou\xe9" ~ "Duekoue",
                               region_code== "Sin\xe9matiali"  ~ "Sinematiali",
                               region_code== "T\xe9hini"  ~ "Tehini",
                               TRUE ~ region_code))

                                              
                                                       
                                                     
                                           
              
#########################################################################################################
# FPET files
# You should not need to edit this piece of code
# We are now merging our QGIS results with the FPET survey data from the National Surveys

fpet_survey_data <- results_long1_clean  %>%
  select(region_code, Year, group_type_relative_to_baseline, contraceptive_use_modern, contraceptive_use_traditional, unmet_need_any) %>%
  mutate(contraceptive_use_any=contraceptive_use_modern + contraceptive_use_traditional,
         is_in_union=case_when(group_type_relative_to_baseline=="MW" ~ "Y",
                               group_type_relative_to_baseline=="UW" ~ "N")) %>%
  left_join(year_info, by="Year") %>%
  select(division_numeric_code	,
         start_date	,
         end_date	,
         is_in_union	,
         age_range	,
         data_series_type	,
         group_type_relative_to_baseline	,
         contraceptive_use_modern	,
         contraceptive_use_traditional	,
         contraceptive_use_any	,
         unmet_need_modern	,
         unmet_need_any	,
         is_pertaining_to_methods_used_since_last_pregnancy	,
         pertaining_to_methods_used_since_last_pregnancy_reason	,
         has_geographical_region_bias	,
         geographical_region_bias_reason	,
         has_non_pregnant_and_other_positive_biases	,
         non_pregnant_and_other_positive_biases_reason	,
         age_group_bias	,
         modern_method_bias	,
         has_traditional_method_bias	,
         traditional_method_bias_reason	,
         has_absence_of_probing_questions_bias	,
         se_modern	,
         se_traditional	,
         se_unmet_need	,
         se_log_r_modern_no_use	,
         se_log_r_traditional_no_use	,
         se_log_r_unmet_no_need	,
         source_id	,
         record_id	,
         region_code	,
         possible_outlier	,
         possible_outlier_userinput	) %>%
  bind_rows(national) %>%
  mutate(region_code=case_when(is.na(region_code) ~ "National",
                               TRUE ~ region_code))

# # # # # # # # # # # # # # # # # ##  # # # 
# Population data
# You should not need to edit this piece of code

nat_pop_clean <- pop_nat %>% filter(unit_numeric_code==iso) %>% select(-region_code)


pop_distribution <- results_long1_clean %>%
  mutate(is_in_union=case_when(group_type_relative_to_baseline=="MW" ~ "Y",
                               group_type_relative_to_baseline=="UW" ~ "N")) %>%
  select(region_code, Year, is_in_union, pop) %>%
  group_by(Year, is_in_union) %>%
  mutate(Share=pop/sum(pop)) %>%
  ungroup() %>%
  select(-pop) %>%
  rename(year=Year) 

year <- seq(1970, 2030, 1)
year.df <- data.frame(year) %>% mutate(n=1)

is_in_union <- c("Y",   "N")
Var1.df <- data.frame(is_in_union) %>% mutate(n=1)

region_code <- as.data.frame(levels(as.factor(results_long1_clean$region_code)))
Var2.df <- region_code %>% mutate(n=1) %>% rename(region_code=1)


full_year_df <- full_join(year.df, Var1.df, by="n") %>% 
  full_join(Var2.df, by="n") %>% select(-n)  %>%
  full_join(pop_distribution, by=c("region_code", "year", "is_in_union")) %>%  
  group_by(region_code, is_in_union) %>%
  arrange(region_code, is_in_union, year) %>%
  mutate(Value_Interp=na.approx(Share,  maxgap = Inf, rule = 2,  na.rm = FALSE)) %>% 
  ungroup() %>%
  select(-Share) 

fpet_pop_data <- full_join(nat_pop_clean, full_year_df, by=c("year", "is_in_union"))  %>%
  mutate(population_count=population_count * Value_Interp) %>% select(-Value_Interp)

# Change the file location and file names to where you want to save your files and what you want them saved as
write.csv(fpet_survey_data, "C:/Users/KristinBietsch/Avenir Health Dropbox/Kristin kbietsch@avenirhealth.org/Avenir Track20/Training Materials/Small Area Estimates 2026/Cote dIvoire/Cote dIvoire Adm2 Survey Data.csv", row.names = F, na="")
write.csv(fpet_pop_data, "C:/Users/KristinBietsch/Avenir Health Dropbox/Kristin kbietsch@avenirhealth.org/Avenir Track20/Training Materials/Small Area Estimates 2026/Cote dIvoire/Cote dIvoire Adm2 Pop Data.csv", row.names = F, na="")