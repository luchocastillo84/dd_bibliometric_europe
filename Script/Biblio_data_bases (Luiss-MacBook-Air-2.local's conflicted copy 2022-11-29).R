# Load the necessary packages
library(tidyverse)
library(here)
library(bibliometrix)
library(devtools)
library(readtext)
library(tidyr)
library(bib2df)
library(readr)
library(visdat)
library(stringr)
library(dplyr)
library(bibtex)
library(readxl)
library(janitor)
library(naniar)
library(dimensionsR)
library(jsonlite)

#### Web of Science ####

# Creating a vector with two files downloaded by the Web of Science data base
wos_file <- c("eu_wos_1_500.txt", 
              "eu_wos_501_1032.txt")

# Converting the data into a data frame readable in the bibliometrix package
raw_W <- convert2df(here("Data", # raw data frame 
                           "Raw", 
                           wos_file), 
                      dbsource = "wos", 
                      format = "plaintext")
col_names_wos <- as.data.frame(colnames(raw_W))
write_excel_csv(col_names_wos, here("Data", "Tags_fields", "col_names_wos.xls"))
W <- convert2df(here("Data", # processed data frame for bibliometrics
                     "Raw", 
                     wos_file), 
                dbsource = "wos", 
                format = "plaintext")

# this code bring the country of each author into the column AU_CO
W <- metaTagExtraction(W, 
                       Field = "AU_CO", 
                       sep = ";")

PF<- rep("wos", 1032) # PF means platform to distinguish docs from scopus and dimensions

W <- cbind(W, PF) # binding the vector PF to M makes M to lose the attributes for biblioshiny

# According to the bibliometrix manual these are the key column to conduct
# a bibliometric analysis. 
# http://www.bibliometrix.org/vignettes/Data-Importing-and-Converting.html
# The reduction was from 73 columns to 23 
# also check the metaTagExtraction command which allow to extract more columns
W <- W[ , c(1, 59, 56, 36, 21, 19, 33, 4, 11, 
            51, 3, 58, 49, 52, 62, 67, 68, 69, 
            70, 71, 72, 73,20,9,64,66,48,28, 74)]

W$DI <- ifelse(is.na(W$DI), W$BN, W$DI)

colnames(W) # check the column names

vis_miss(W, sort_miss = T) #  visualize missing values 

W <- W %>% filter(!is.na(CR)) # filters out NA values in the citations references column

W <-  W %>% distinct(DI, .keep_all = T) # keeping only the unique docs by DI or DOI

W[, c(5, 23)] <- lapply(W[, c(5,23)], tolower)

W_by_DT <- W %>% # dist contains a summary of documents type DT by unique DOI and BN
        group_by(DT) %>% # group by document type
        summarize(count = n(), # summary by count by DT 
                  DOI = n_distinct(DI), # unique DOI
                  BN = n_distinct(BN), # unique book number
                  na_doi = sum(is.na(DI)), # number of NA in DOI
                  na_bn = sum(is.na(BN)), #  number of NA in BN
                  TI = n_distinct(TI)) %>%  # unique titles 
        adorn_totals("row") # total 


#### Scopus ####

raw_S <- convert2df(here("Data", # raw data frame
                              "Raw", 
                              "eu_scopus_1_1786.csv"),
                         dbsource = "scopus", 
                         format = "csv")

S <- convert2df(here("Data",  # processed data frame for bibliometrics 37 columns
                      "Raw", 
                      "eu_scopus_1_1786.csv"),
                 dbsource = "scopus", 
                 format = "csv")

# this code bring the country of each author into the column AU_CO 38 columns
S <- metaTagExtraction(S, 
                        Field = "AU_CO", 
                        sep = ";")


S <- S %>% mutate("SC" = "", # add research areas (empty column) 
                  "WC" = "", # add WoS categories (empty column)
                  "Z9" = "") %>% # add Total Times Cited Count (empty column) total 42 columns
           rename("TI" = "TIs", # renaming title according to WoS tags
                  "ID" = "Indexed.Keywords",# keywords
                  "PU" = "Publisher", 
                  "BN" = "URL")

PF<- rep("sco", 1786) # PF means platform to distinguish docs from scopus and dimensions 39 columns
S <- cbind(S, PF) # binding the vector PF to M makes M to lose the attributes for biblioshiny

S <- S[, c(1, 4, 6, 31, 26, 
             20, 19, 18, 17, 24, 
             23, 14, 5, 39, 30, 
             29, 33, 34, 35, 36, 
             37, 38, 13, 15, 40, 
             41, 25, 21,42)] # match tag and number of columns from the Wos 29 columns

S <- S %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the S
S <- S %>% filter(!is.na(CR)) # filters out NA values in the citations references column
S <- S %>% filter(!is.na(AU_CO))
S <-  S %>% distinct(DI, .keep_all = T) # keeping only the unique docs by DI or DOI


countrydf[which(countrydf$split == "USA"),]
S <- S[-1316, ]

S[, c(5, 23)] <- lapply(S[, c(5,23)], tolower) # to lower DT and DI

s_au_countries <- as.character(S$AU_CO) # convert the AU_CO into a chr vector
s_au_country <- as.data.frame(s_au_countries) # converting the AU_CO vector into df

## selecting the unique values such as USA;USA... of AU_CO for each document row 
countrydf <-  s_au_country %>% 
  mutate(split = str_split(s_au_countries, ";")) %>% # split
  mutate(split = map(.$split, ~ unique(.x))) %>% # drop duplicates
  mutate(split = map_chr(.$split, ~paste(.x, collapse = ";"))) # recombine


S_by_DT <- S %>% # dist contains a summary of documents type DT by unique DOI and BN
  group_by(DT) %>% # group by document type
  summarize(count = n(), # summary by count by DT 
            DOI = n_distinct(DI), # unique DOI
            BN = n_distinct(BN), # unique book number
            na_doi = sum(is.na(DI)), # number of NA in DOI
            na_bn = sum(is.na(BN)), #  number of NA in BN
            TI = n_distinct(TI)) %>%  # unique titles 
  adorn_totals("row") # total 



#### Dimensions ####
# https://cran.r-project.org/web/packages/dimensionsR/vignettes/A_Brief_Example.html
# Creating a vector with two files downloaded by the Dimensions data base
dim_file <- c("dim_1_1467.csv", 
              "dim_1468_3108.csv")

# Converting the data into a data frame readable in the bibliometrix package
raw_D <- convert2df(here("Data", # raw data frame 
                         "Raw", 
                         dim_file), 
                    dbsource = "dimensions", 
                    format = "csv")

D <- convert2df(here("Data", # raw data frame 
                     "Raw", 
                     dim_file), 
                dbsource = "dimensions", 
                format = "csv")

D <- D[, -c(22:24)] # dropping two empty columns AU_CO, AU_UN and AU1_CO

D <-  D %>% 
  rename("AU_UN" = "Authors.Affiliations.Name.of.Research.organization",
         "AU_CO" = "Authors.Affiliations.Country.of.Research.organization",
         "BN" = "Dimensions.URL") %>% 
  mutate("C1" =  "" , 
         "SC" =  "" , 
         "AU_UN_NR" =  "" , 
         "WC" =  "" , 
         "Z9" =  "" , 
         "PU" =  "" , 
         "FU" =  "" ) %>% 
  replace_with_na_all(condition = ~. == "") %>% 
  filter(!is.na(CR) & 
           !is.na(AU_CO))

PF<- rep("dim", 1713)
D <- cbind(D, PF) # binding the vector PF to M makes M to lose the attributes for biblioshiny

D <-  D[, c(10, 3, 23, 24, 17, 20, 19, 4, 28, 
            21, 16, 14, 6, 29, 1, 25, 11, 22, 
            30, 26, 27, 12, 2, 13, 31, 32, 33, 
            34,35)]

D <- D %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the D
vis_miss(D)

au_countries <- as.character(D$AU_CO) # convert the AU_CO into a chr vector
au_country <- as.data.frame(au_countries) # converting the AU_CO vector into df

## selecting the unique values such as USA;USA... of AU_CO for each document row 
countrydf <-  au_country %>% 
  mutate(split = str_split(au_countries, "; ")) %>% # split
  mutate(split = map(.$split, ~ unique(.x))) %>% # drop duplicates
  mutate(split = map_chr(.$split, ~paste(.x, collapse = ";"))) # recombine

country_split <- as.data.frame(countrydf$split) ## converting the split vector into df
## split the vector AU_CO into a df
## count the split for every ; semicolon that appears in the line resulting number of columns
ncols <- max(stringr::str_count(country_split$`countrydf$split`, ";")) + 1

colmn <- paste("country_", 1:ncols, sep ="") # create and paste the name of the colums

#3 convert the splits into columns 
authors_countries <-
  tidyr::separate(
    data = country_split,
    col = `countrydf$split`,
    sep = ";",
    into = colmn,
    remove = T
  )

vis_miss(authors_countries)
## filters the unique countries in the database
tot_countries <- as.data.frame(cbind(
  c(unique(authors_countries$country_1),
    unique(authors_countries$country_2),
    unique(authors_countries$country_3),
    unique(authors_countries$country_4),
    unique(authors_countries$country_5),
    unique(authors_countries$country_6),
    unique(authors_countries$country_7),
    unique(authors_countries$country_8))))

## filters out the NA values that appear in the column 
tot_countries <- unique(tot_countries) %>% filter(!is.na(V1))
# write_excel_csv(tot_countries, "country_list.xls") # save in xls the country list
## load the countries, codes and regions 
country_code_reg <- read_excel(here("Data", "Processed", "country_code_region.xlsx"))

## merging 
country_code_reg <- country_code_reg %>% 
  select(name, 
         region) %>% 
  rename("Continent" = "region",
         "Country" = "name")


au_con_reg <- authors_countries %>% 
              left_join(y = country_code_reg[ , c(1,2)], 
                        by = c("country_1" = "Country")) %>% 
              rename("Continent_1" = "Continent") %>% 
              left_join(y = country_code_reg[ , c(1,2)], 
                        by = c("country_2" = "Country")) %>% 
              rename("Continent_2" = "Continent") %>% 
              left_join(y = country_code_reg[ , c(1,2)], 
                        by = c("country_3" = "Country")) %>% 
              rename("Continent_3" = "Continent") %>% 
              left_join(y = country_code_reg[ , c(1,2)], 
                        by = c("country_4" = "Country")) %>% 
              rename("Continent_4" = "Continent")  %>% 
              left_join(y = country_code_reg[ , c(1,2)], 
                        by = c("country_5" = "Country")) %>% 
              rename("Continent_5" = "Continent") %>% 
              left_join(y = country_code_reg[ , c(1,2)], 
                        by = c("country_6" = "Country")) %>% 
              rename("Continent_6" = "Continent") %>% 
              left_join(y = country_code_reg[ , c(1,2)], 
                        by = c("country_7" = "Country")) %>% 
              rename("Continent_7" = "Continent") %>% 
              left_join(y = country_code_reg[ , c(1,2)], 
                        by = c("country_8" = "Country")) %>% 
              rename("Continent_8" = "Continent")

au_con_reg[au_con_reg == "UNITED STATES"] <- "USA" 
namebyrow<- as.data.frame(rownames(D)) 
D_2filter <-  cbind(namebyrow, au_con_reg)


## filtering the columns continet that contains Europe 
D_2filter <- D_2filter %>% filter(Continent_1 == "Europe" |
                                    Continent_2 == "Europe" |
                                    Continent_3 == "Europe" |
                                    Continent_4 == "Europe" |
                                    Continent_5 == "Europe" |
                                    Continent_6 == "Europe" |
                                    Continent_7 == "Europe" |
                                    Continent_8 == "Europe")

## filter by the row index so the df does not lose the attributes 
D_index <- D_2filter$`rownames(D)`
D <- D[D_index ,]

D[, c(5, 23)] <- lapply(D[, c(5,23)], tolower) # to lower DT and DI

DW <- anti_join(D, W, by = "DI") 

WD <- rbind(W, DW)

WDS <- anti_join(S, WD, by= "DI")

M <-  rbind(WD, WDS)
M <- M %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the D
M <- M %>% replace_with_na_all(condition = ~. == "NA") # converting "" into NA within the D
vis_miss(M)

M$AU_CO <- str_replace_all(M$AU_CO, "UNITED STATES", "USA")
M$AU <- str_replace_all(M$AU, "VAN DIJK JAGM", "VAN DIJK J")


M_by_PF <- M %>% # dist contains a summary of documents type DT by unique DOI and BN
  group_by(PF) %>% # group by document type
  summarize(count = n(), # summary by count by DT 
            DI = n_distinct(DI),
            NA_DI = sum(duplicated(DI))) %>%  # unique titles 
  adorn_totals("row") # total 

M[, c(5, 23)] <- lapply(M[, c(5,23)], tolower)

M <-  M %>% distinct(DI, .keep_all = T) # keeping only the unique docs by TI title
M <-  M %>% distinct(TI, .keep_all = T) # keeping only the unique docs by TI title

results <- biblioAnalysis(M)
summary(results, k=20, pause=F, width=130)

M_by_DT <- M %>% # dist contains a summary of documents type DT by unique DOI and BN
  group_by(DT) %>% # group by document type
  dplyr::summarize(count = n(), # summary by count by DT 
            DOI = n_distinct(DI), # unique DOI
            BN = n_distinct(BN), # unique book number
            kw_na = sum(is.na(SO)), # number of NA in DOI
            na_SO = sum(is.na(SO)), #  number of NA in BN
            TI = n_distinct(TI)) %>%  # unique titles 
  adorn_totals("row") # total 

M <- M %>% arrange(PY) %>% filter(PY != 2022)

write_csv(M, file = here("Data", "Processed", "M_EU.csv"))

AU_UN <- as.data.frame(gsub("UNIVERSITY OF OXFORD", "UNIV OXFORD", AU_UN))

M_by_DT <- M %>% # dist contains a summary of documents type DT by unique DOI and BN
  group_by(PF) %>% # group by document type
  dplyr::summarize(count = n()) %>%  # unique titles 
  adorn_totals("row") # total 


