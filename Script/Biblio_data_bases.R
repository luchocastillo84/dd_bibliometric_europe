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
library(purrr)
library(readr)
library(qdap)
#### Load environment ####
load(here("Script", "Environments", "Biblio_data_bases.RData"))

#### Web of Science ####

# Creating a vector with two files downloaded by the Web of Science data base
wos_file <- c("wos_020123_1_500.txt", "wos_010223_501_1148.txt")

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

PF<- rep("wos", nrow(W)) # PF means platform to distinguish docs from scopus and dimensions

W <- cbind(W, PF) # binding the vector PF to M makes M to lose the attributes for biblioshiny

# According to the bibliometrix manual these are the key column to conduct
# a bibliometric analysis. 
# http://www.bibliometrix.org/vignettes/Data-Importing-and-Converting.html
# The reduction was from 73 columns to 23 
# also check the metaTagExtraction command which allow to extract more columns
W <- W[ , c(col_names_M)]

W$DI <- ifelse(is.na(W$DI), W$BN, W$DI)

colnames(W) # check the column names

vis_miss(W) #  visualize missing values 

W <- W %>% filter(!is.na(CR)) # filters out NA values in the citations references column

W <-  W %>% distinct(DI, .keep_all = T) # keeping only the unique docs by DI or DOI

W[, c(5, 23)] <- lapply(W[, c(5,23)], tolower) # to lower DT and DI

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
                              "scopus_020123_1_1187.csv"),
                         dbsource = "scopus", 
                         format = "csv")

S <- convert2df(here("Data",  # processed data frame for bibliometrics 37 columns
                      "Raw", 
                      "scopus_020123_1_1187.csv"),
                 dbsource = "scopus", 
                 format = "csv")

# this code bring the country of each author into the column AU_CO 38 columns
S <- metaTagExtraction(S, 
                        Field = "AU_CO", 
                        sep = ";")


S <- S %>% mutate("SC" = "", # add research areas (empty column) 
                  "WC" = "", # add WoS categories (empty column)
                  "Z9" = "") %>% # add Total Times Cited Count (empty column) total 42 columns
           rename("TI" = "TI", # renaming title according to WoS tags
                  "ID" = "ID",# keywords
                  "PU" = "Publisher", 
                  "BN" = "URL")

PF<- rep("sco", nrow(S)) # PF means platform to distinguish docs from scopus and dimensions 39 columns
S <- cbind(S, PF) # binding the vector PF to M makes M to lose the attributes for biblioshiny

S <- S[, col_names_M] # match tag and number of columns from the Wos 29 columns

S <- S %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the S
S <- S %>% filter(!is.na(CR)) # filters out NA values in the citations references column
S <- S %>% filter(!is.na(AU_CO))
S <-  S %>% distinct(DI, .keep_all = T) # keeping only the unique docs by DI or DOI
rownames(S) <- S$SR # using SR as row names
vis_miss(S)

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

DIsco <- data.frame(DIsco = paste("OR"," ", "DOI", "(",all_sco$DI,")", sep = ""))
DIsco[c(1,2)]
write_delim(DIsco,  here("Data","Processed", "DIsco.txt"), delim = "")

#### Dimensions ####
# https://cran.r-project.org/web/packages/dimensionsR/vignettes/A_Brief_Example.html
# Creating a vector with two files downloaded by the Dimensions data base
dim_file <- c("dim_131222_1_882cr.csv",
              "dim_131222_1_1265cr.csv",
              "dim_020123_1_1916cr.csv")

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
vis_miss(D)

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

PF<- rep("dim", nrow(D))
D <- cbind(D, PF) # binding the vector PF to M makes M to lose the attributes for biblioshiny

# D <-  D[, c(10, 3, 23, 24, 17, 20, 19, 4, 28, 
#             21, 16, 14, 6, 29, 1, 25, 11, 22, 
#             30, 26, 27, 12, 2, 13, 31, 32, 33, 
#             34,35)]

D <- D[, col_names_M]

D <- D %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the D
vis_miss(D)

au_countries <- as.character(D$AU_CO) # convert the AU_CO into a chr vector
au_country <- as.data.frame(au_countries) # converting the AU_CO vector into df

## selecting the unique values such as USA;USA... of AU_CO for each document row 
countrydf <-  au_country %>% 
  mutate(split = str_split(au_countries, "; ")) %>% # split
  mutate(split = purrr::map(.$split, ~ unique(.x))) %>% # drop duplicates
  mutate(split = purrr::map_chr(.$split, ~paste(.x, collapse = ";"))) # recombine


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

country_code_reg <- read_excel(here("Data", ## load the countries, codes and regions 
                                    "Tags_fields", 
                                    "country_code_region.xlsx"))

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


## filtering the columns continent that contains Europe 
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
D <- D %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the D
D <- D %>% replace_with_na_all(condition = ~. == "NA") # converting "" into NA within the D
vis_miss(D)
col_i <- which(colnames(D) %in% c("SO", "C1", "DE", "ID", "RP", "SC", "FU")) # get the column indexes
D <- D[ , -col_i] # excluding the column indexes from D
D1_D <- left_join(D, D1_SO, by= "DI", keep= F) # joining the API data with the GUI data
# col_names_M <- colnames(M) # getting the column names of M
D <- D1_D[, col_names_M] # reorganizing the columns joined in D1_D

D <- metaTagExtraction(D, Field = "SR", sep = ";") # getting the author year journal column
D <- metaTagExtraction(D, Field = "AU_UN", sep = ";") # getting the author year journal column
vis_miss(D)

rownames(D) <- D$SR # using SR as row names


DW <- anti_join(D, W, by = c("DI", "TI")) # filtering docs that only appear in dimensions and !WOS

WD <- rbind(W, DW) # binding WOS and Dimensions

SDW <- anti_join(S, WD, by= c("DI", "TI")) # filtering docs that only appear in SCOPUS and !WOS & DIM

# B is the name of the df that binds WOS, SCO and DIM
B <-  rbind(WD, SDW) # binding the three data bases into B
B <- B %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the D
B <- B %>% replace_with_na_all(condition = ~. == "NA") # converting "" into NA within the D
vis_miss(B)

B$AU_CO <- str_replace_all(B$AU_CO, 
                           "UNITED STATES", 
                           "USA") # changing UNITED STATES with USA to avoid double counting

B$AU <- str_replace_all(B$AU, 
                        "VAN DIJK JAGM", 
                        "VAN DIJK J") # changing VAN DIJK JAGM with VAN DIJK J to avoid double counting

B$AU_UN <- str_replace_all(B$AU_UN,
                           "UNIVERSITY", 
                           "UNIV") # changing UNIVERSITY with UNIV to avoid double counting

B$AU_UN <- str_replace_all(B$AU_UN,
                           "UNIV OF", 
                           "UNIV")

B$AU_UN <- str_replace_all(B$AU_UN,
                           "MILANO", 
                           "MILAN")

B[, c(5, 23)] <- lapply(B[, c(5,23)], tolower)

B <-  B %>% distinct(DI, .keep_all = T) # keeping only the unique docs by DOI
B <-  B %>% distinct(TI, .keep_all = T) # keeping only the unique docs by TI title
B <-  B %>% distinct(SR, .keep_all = T) # keeping only the unique docs by SR title


B <- B %>% arrange(PY) %>% filter(PY >= 2000 & PY< 2023) # excluding 2023


indices <- which(apply(B[, c("TI", "DE")], 1, # filters only TI, DE with the pattern
                       function(x) any(grepl("digital divide|digital gap|digital inequalit", 
                                             x, ignore.case = TRUE)))) # I excluded ID 

B <- B[indices, ]

write_csv(B, file = here("Data", 
                         "Processed", 
                         "B_EU.csv")) # writing as CSV to make readable in biblioshiny

################################################################################
####################  BINDING THE CLEAN CR FROM WOS, SCO AND DIM ###############
################################################################################

M <- rbind(all_isi, all_sco, all_dim)

M <- M %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the df
M <- M %>% replace_with_na_all(condition = ~. == "NA") # converting "" into NA within the df
vis_miss(M)

M <- M %>% filter(!is.na(CR)) # filter out NA values in the CR column

M_AU<- # adds a SO column
 trimws(unlist(lapply(strsplit(M$AU,
                               ';', # separated by comma
                               fixed = TRUE),
                      '[', 1))) # extract the third string in the array

M$DI <- tolower(M$DI)
M_doi <- paste("DOI", M$DI)


M$SRDI <- paste(M_AU, M$PY, ifelse(M_doi == "DOI NA", "", M_doi), sep = ", ")
M$SRDI <- trimws(M$SRDI, whitespace = ", ")


rownames(M) <- M$SRDI # using SR as row names

write_csv(M, file = here("Data", 
                         "Processed", 
                         "M_EU.csv")) # writing as CSV to make readable in biblioshiny

N <- M

N$DB <- "ISI"
N$SR <- N$SRDI
rownames(N) <- N$SRDI # using SR as row names

write_csv(N, file = here("Data", 
                         "Processed", 
                         "N_EU.csv")) # writing as CSV to make readable in biblioshiny
class(N) <- c("bibliometrixDB", "data.frame")
save(N, file = "N_EU.rda")

N$LABEL <- paste(N_AU, N$PY, sep = ", ")
N$LABEL <- LABEL
n_1p <- N %>% filter(PY <= 2008)
n_1p = n_1p[order(n_1p$PY), ]
save(n_1p, file = "n1_EU.rda")

n_2p <- N %>% filter(PY > 2008 & PY <= 2015) # sub-setting the second period
n_2p = n_2p[order(n_2p$PY), ]
save(n_2p, file = "n2_EU.rda")


n_3p <- N %>% filter(PY > 2016 & PY <= 2022) # sub-setting the second period
save(n_3p, file = "n3_EU.rda")






