# Load the necessary packages
library(prettydoc)
library(tidyverse)
library(rio)
library(here)
library(bibliometrix)
library(devtools)
library(readtext)
library(tidyr)
library(bib2df)
library(readr)
library(visdat)
library(stringr)
library(purrr)
library(dplyr)
library(bibtex)
install_formats()
library(bib2df)
library(RefManageR)
library(rbibutils)
library(rscopus)
library(readxl)
library(naniar)
library(Matrix)
library(ggrepel)
library(ggtext)
biblioshiny()

# loading the data from wos using \t tab as separator
# The query bellow brought 3338 documents in the WoS platform
# "Digital divide$" (Title) OR "digital divide$" (Author Keywords) OR 
# "Digital gap$" (Author Keywords) OR "Digital gap$" (Title) OR 
# "Digital inequalit$" (Title) OR "Digital inequalit$" (Author Keywords) and 
# 2022 (Exclude – Publication Years) and Article or Proceeding Paper or Book Chapters or


# Review Article (Document Types)
wos_file <- c("nwos_1_1000.txt", "nwos_1001_2000.txt", "nwos_2001_2616.txt")


#tot_wos <- convert2df(here("Data", "Raw", wos_file))
#vis_miss(tot_wos)

#save(tot_wos, file = "tot_wos.rda")

# export the files as .txt to upload it in biblioshiny 
#write_delim(tot_wos_3337, 
            #file = here("Data", "Processed", "text_m.txt"), 
            #col_names= F, quote =  "none", escape = "none")

# Converting the data into a data frame readable in the bibliometrx package
raw_wos <- convert2df(here("Data", "Raw", wos_file), 
                      dbsource = "wos", format = "plaintext")

M <- convert2df(here("Data", "Raw", wos_file), 
                dbsource = "wos", format = "plaintext")

# this code bring the country of each author into the column AU_CO
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")


### dimensionality reduction process by choosing key columns for bibliometrix  ###
tags <- colnames(M) # extracting the colnames into a vector
col_names <- as.data.frame(tags) # convert the vector into a df
write_excel_csv(col_names, "col_names_wos.xls") #export colnames to excel
per_na <- colSums(is.na(M)/ nrow(M)) # percentage of na for every column 
na_tags <- as.data.frame(per_na) # convert the vector into df
write_excel_csv(na_tags, "na_tags.xls") # export the percentages to excel

# According to the bibliometrix manual these are the key column to conduct
# a bibliometric analysis. 
# http://www.bibliometrix.org/vignettes/Data-Importing-and-Converting.html
# The reduction was from 73 columns to 23 
# also check the metaTagExtraction command which allow to extract more columns
M <- M[ , c(1, 59, 56, 36, 21, 19, 33, 4, 11, 
            51, 3, 58, 49, 52, 62, 67, 68, 69, 
            70, 71, 72, 73,20,64,66,48,28)]

vis_miss(M, sort_miss = T)


M <- M %>% filter(!is.na(CR)) # filters out NA values in the citations references column
M <- M %>% filter(!is.na(AU_CO)) # filters out NA values in the author's country


head(M$AU_CO) # view how the countries are separated by ;
au_countries <- as.character(M$AU_CO) # convert the AU_CO into a chr vector
au_country <- as.data.frame(au_countries) # converting the AU_CO vector into df

## selecting the unique values such as USA;USA... of AU_CO for each document row 
countrydf <-  au_country %>% 
  mutate(split = str_split(au_countries, ";")) %>% # split
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
                              c(unique(df$country_1),
                              unique(df$country_2),
                              unique(df$country_3),
                              unique(df$country_4),
                              unique(df$country_5),
                              unique(df$country_6),
                              unique(df$country_7))))

## filters out the NA values that appear in the column 
tot_countries <- unique(tot_countries) %>% filter(!is.na(V1))
write_excel_csv(tot_countries, here("Data", "Processed","country_list_dim.xls")) # save in xls the country list
## load the countries, codes and regions 
country_code_reg <- read_excel(here("Data", "Processed", "country_code_region.xlsx"))

## merging 
sel_col <- country_code_reg %>% select(Country, Continent) %>% rename("Continent_1" = "Continent")
M_plus <- M_plus %>% inner_join(y= sel_col,  by= c( "country_1" = "Country"))

au_con_reg <- authors_countries %>% 
  inner_join(y = country_code_reg[ , c(1,4)], by = c("country_1" = "Country")) %>% 
  rename("Continent_1" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_2" = "Country")) %>% 
  rename("Continent_2" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_3" = "Country")) %>% 
  rename("Continent_3" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_4" = "Country")) %>% 
  rename("Continent_4" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_5" = "Country")) %>% 
  rename("Continent_5" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_6" = "Country")) %>% 
  rename("Continent_6" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_7" = "Country")) %>% 
  rename("Continent_7" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_8" = "Country")) %>% 
  rename("Continent_8" = "Continent")

## filtering the columns continet that contains Europe 
eu_authors <- au_con_reg %>% filter(Continent_1 == "Europe" |
                                      Continent_2 == "Europe" |
                                      Continent_3 == "Europe" |
                                      Continent_4 == "Europe" |
                                      Continent_5 == "Europe" |
                                      Continent_6 == "Europe" |
                                      Continent_7 == "Europe" |
                                      Continent_8 == "Europe")


namebyrow<- as.data.frame(rownames(M)) %>% mutate(i_num = row_number())
M_2filter <-  cbind(namebyrow, au_con_reg)


M_2filter <- M_2filter %>% filter(Continent_1 == "Europe" |
                                   Continent_2 == "Europe" |
                                   Continent_3 == "Europe" |
                                   Continent_4 == "Europe" |
                                   Continent_5 == "Europe" |
                                   Continent_6 == "Europe" |
                                   Continent_7 == "Europe" |
                                   Continent_8 == "Europe")

## filter by the row index so the df does not lose the attributes 
M_index <- M_2filter$`rownames(M)`
M <- M[M_index ,]
save(M, file = "M_EU.rda") # save as rda file for bibliometrix


glimpse(M)
class(M_eu)



# this code will filter out the NA values in book number BN, DIO = DI and cited references CR
# & is.na(BN) & is.na(DI)
na_adress <- M %>% filter(is.na(C1))
m_redux <- M %>% filter(!is.na(CR))
m_redux <- m_redux %>% filter(!is.na(BN) | !is.na(DI))
sum(is.na(m_redux$C1))





M1 <- convert2df(here("Data", "Raw", "scopus_1_1786.csv"),
                      dbsource = "scopus", format = "csv")

M1 <- metaTagExtraction(M1, Field = "AU_CO", sep = ";")
M1 <- M1 %>% mutate("SC" = "", "WC" = "", "Z9" = "")
M1 <- M1 %>% rename("TI" = "TIs", "ID" = "Indexed.Keywords", "PU" = "Publisher")
n_distinct(M1$URL)
duplicated(M$DI)
dup <- M[duplicated(M$DI), ]
n_distinct(dup$DI)

M1 <- M1[, c(1, 4, 6, 31, 26, 20, 19, 18, 17, 
             24, 23, 14, 5, 39, 30, 29, 33, 34, 
             35, 36, 37, 38, 13, 40, 41, 25, 21)]

M1 <- M1 %>% replace_with_na_all(condition = ~. == "")

colnames_W <- colnames(M)
colnames_S <- colnames(M1)
colnames_WS <- as.data.frame(paste(colnames_W, colnames_S))
M1$DI[which(M1$DI == "")] <- NA
M1 %>% group_by(DT) %>% summarize(DOI = n_distinct(DI), NA_ = sum(is.na(DI)))
n_distinct(M$TI)
dist <- M %>% group_by(DT) %>% summarize(count = n(), 
                                       DOI = n_distinct(DI),
                                       BN = n_distinct(BN),
                                       na_doi = sum(is.na(DI)),
                                       na_bn = sum(is.na(BN)),
                                       TI = n_distinct(TI)) %>% adorn_totals("row")

sum(is.na(M$BN))

M <-  M %>% distinct(TI, .keep_all = T)
vis_miss(M)
unique(raw_wos$BN)
n_distinct(raw_wos$D2)
M_merged <- rbind(M, M1)
n_distinct(M_merged$TI)

glimpse(M1)

vis_miss(M1)
save(M1, file = "M1_EU.rda")

results1 <- biblioAnalysis(M)
summary(results1, k=20, pause=F, width=130)

### dimensionality reduction process by choosing key columns for bibliometrix  ###
tags_sco <- colnames(M1) # extracting the colnames into a vector
col_names_sco <- as.data.frame(tags_sco) # convert the vector into a df
write_excel_csv(col_names_sco, "col_names_scopus.xls") #export colnames to excel
per_na_sco <- colSums(is.na(M1)/ nrow(M1)) # percentage of na for every column 
na_tags_sco <- as.data.frame(per_na_sco) # convert the vector into df
write_excel_csv(na_tags_sco, "na_tags_sco.xls") # export the percentages to excel




M1 <- bib2df(here("Data", "Raw", "SD_1_100.bib"), separate_names = F)

M2 <- convert2df(here("Data", "Raw", "dim_1_2347.csv"),
                 dbsource = "dimensions", format = "csv")


# trying to connect the rsopus packages with the Scopus API but
# there is a authentication error 
Elsevier_API = "4d6189c9319472288662446281dffa1c"
hdr = inst_token_header(token)
hdr = inst_token_header(token)
res = author_df(last_name = "Castillo Tellez", first_name = "Luis Carlos", 
                verbose = FALSE, general = FALSE, headers = hdr, api_key = Elsevier_API)
elsevier_authenticate(api_key = 'abb3c9ab7a77484fbc524a4b240e8199', api_key_error = F, 
                      choice = NULL, verbose = F, headers = "X-ELS-APIKey")



# this code will filter out the NA values in book number BN, DIO = DI and cited references CR
# & is.na(BN) & is.na(DI)
na_adress <- M %>% filter(is.na(C1))
m_redux <- M %>% filter(!is.na(CR))
m_redux <- m_redux %>% filter(!is.na(BN) | !is.na(DI))
sum(is.na(m_redux$C1))




save(m_col_redux, file = "m_col_redux.rda")

vis_miss(na_doi)
na_filter <- M %>% filter(is.na(AU_CO))
here("Data", "Processed", save(na_filter, file = "na_filter.rda"))
write_csv(na_abs, "na_abs.csv")
r <- tempfile(fileext = ".R")
bibConvert("na_filter.rda", "bibtex", "r", "bibtex")
glimpse(M)
length(unique(M$DI))





na_doi <- M %>% filter(is.na(DI))
sum(is.na(na_doi$BN))
na_booknum <- na_doi %>% filter(is.na(BN))

save(na_doi, file = "na_doi.rda")
is.BibEntry(na_doi)
as.BibEntry(na_doi)
toBibtex(na_filter)
na_abs <- M %>% filter(is.na(AB))

# Summary report of bibliometric analysis in the sample 3337 documents
results <- biblioAnalysis(M_eu)
summary(results, k=20, pause=F, width=130)

## observe the observation ## 6, 27 using as sep = ; 
country <- strsplit(M$AU_CO, ";") [27][1]
country

## splitting the string vectors of the authors' country
countries <-  sapply(strsplit(as.character(M$AU_CO), ";"), `[`, 6)
country1 <- as.data.frame(countries)
country1 %>% group_by(countries) %>% summarize(count= n()) %>% arrange(desc(count))

#unique(M$PA)
# visualization of missing data AU_CO= 4.02%, AU_UN = 6.86% , DOI DI = 37.43%
vis_miss(M, sort_miss = TRUE)

# converting the authors country AU_CO vector as character and data frame
au_countries <- as.character(M$AU_CO)
au_country <- as.data.frame(au_countries)

## selecting the unique values of AU_CO for each document 
df1 <- au_country %>%
  mutate(split = str_split(au_countries, ";")) %>% # split
  mutate(split = map(.$split, ~ unique(.x))) %>% # drop duplicates
  mutate(split = map_chr(.$split, ~paste(.x, collapse = ";"))) # recombine

df2 <- as.data.frame(df1$split)
au_cou <-  data.frame(do.call("rbind", 
                              strsplit(as.character(df2), ";", fixed = T)))

ncols <- max(stringr::str_count(df2$`df1$split`, ";")) + 1

colmn <- paste("col", 1:ncols)

df <-
  tidyr::separate(
    data = df2,
    col = `df1$split`,
    sep = ";",
    into = colmn,
    remove = T
  )

vis_miss(df)

count_co <- df %>% group_by(`col 1`,`col 2`,`col 3`) %>% 
  summarize(count= n()) %>% arrange(desc(count))
count_co
unique(df$`col 5`)

country_au <- lapply(df2,  data.frame(do.call("rbind", 
                                    strsplit(as.character(df2), ";"))))
  
  
p <- lapply(df2, str_split( ";"))



unique(df2)

# Creating a vector with two files downloaded by the Web of Science data base
dim_file <- c("dim_131222_1_882cr.csv",
              "dim_131222_1_1265cr.csv",
              "dim_131222_1_1551cr.csv")
dim_500 <- "dim_500.csv"

# Converting the data into a data frame readable in the bibliometrix package
raw_D <- convert2df(here("Data", # raw data frame 
                         "Raw", 
                         dim_500), 
                    dbsource = "dimensions", 
                    format = "csv")

Dn <- convert2df(here("Data", # raw data frame 
                     "Raw", 
                     dim_file), 
                dbsource = "dimensions", 
                format = "csv")
D <- D[, -c(22:24)] # dropping two empty columns AU_CO, AU_UN and AU1_CO


colD <- colnames(D)
col_rawD <- colnames(raw_D)
col_D <- as.data.frame(append(colD,col_rawD))
write_excel_csv(col_D, here("Data", "Processed", "col_names_dim.xls")) #export colnames to excel
setdiff(colD, col_rawD)

n_distinct(D$TI)



# this code bring the country of each author into the column AU_CO
D <- metaTagExtraction(D, 
                       Field = "AU_CO", 
                       sep = ";")
vis_miss(D)

D_TI <- as.data.frame(D$DI) %>%  rename( "DI" = "D$DI") 
W_TI <- as.data.frame(W$DI) %>%  rename( "DI" = "W$DI")
S_TI <- as.data.frame(S$DI) %>%  rename( "DI" = "S$DI")

data_DWS <- rbind(D_TI, W_TI, S_TI)


n_distinct(data_DWS)
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
             "FU" =  "" , 
             "PF" =  "") %>% 
      replace_with_na_all(condition = ~. == "") %>% 
      filter(!is.na(CR) & 
             !is.na(AU_CO))


D <-  D[, c(10, 3, 23, 24, 17, 20, 19, 4, 28, 
            21, 16, 14, 6, 29, 1, 25, 11, 22, 
            30, 26, 27, 12, 2, 13, 31, 32, 33, 
            34,35)]


vis_miss(D)

head(M$AU_CO) # view how the countries are separated by ;
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

au_con_reg <- au_con_reg 

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_3" = "Country")) %>% 
  rename("Continent_3" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_4" = "Country")) %>% 
  rename("Continent_4" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_5" = "Country")) %>% 
  rename("Continent_5" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_6" = "Country")) %>% 
  rename("Continent_6" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_7" = "Country")) %>% 
  rename("Continent_7" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_8" = "Country")) %>% 
  rename("Continent_8" = "Continent")

## filtering the columns continet that contains Europe 
eu_authors <- au_con_reg %>% filter(Continent_1 == "Europe" |
                                      Continent_2 == "Europe" |
                                      Continent_3 == "Europe" |
                                      Continent_4 == "Europe" |
                                      Continent_5 == "Europe" |
                                      Continent_6 == "Europe" |
                                      Continent_7 == "Europe" |
                                      Continent_8 == "Europe")




search <- c("**Searched Fields**", "**Searched Text**", "**Document Types**", 
            "**Web of Science Categories**", "**Region**", "**Time Frame**", 
            "**Total Document**", "**Query Link**", "**Downloaded Data Type**")

desc <- c("Keywords and Title", 
          '"digital divide*" "OR" "digital inequalit*" "OR" "digital gap*"',
          "Articles, proceedings book chapters, review articles and early access", "Computer science and technology, management, business and economics", "Countries in Europe", "2000 - 2021", "1034", 
          "[Go to Web of Science Query Link](https://www.webofscience.com/wos/woscc/summary/4811505d-f079-4eb4-85b5-0efb1bba0b49-53a1a627/relevance/1)", ".txt")

wos_look <- data.frame(search, desc)
knitr::kable(wos_look, 
             "simple", 
             col.names = c("**Search Criteria**", 
                           "**Description**"),
             align = c("l", "c"),
             linesep = c("\\addlinespace"))


min(as.numeric(MM$PY), na.rm = F)
min(MM$PY)



string <- "HUGGINS R, 2002, LOCAL ECON"
parts <- strsplit(string, ", ")[[1]]
result <- paste(parts[1], parts[2], sep=", ")
result

string <- "VAN DIJK J.A. G.M, 2005, DEEPENING DIVIDE INE, DOI DOI 10.1080/15205430701528655"
# Replace "VAN DIJK J.A. G.M" with "VAN DIJK J", ignoring case
string <- gsub("(?i)VAN DIJK J.A. G.M", "VAN DIJK J", string)

# Print the modified string
print(string)

results <- sub(pattern = ".", replacement = "", 
                            x = string, fixed = TRUE, perl = TRUE, useBytes = FALSE)
string <-  sub(".*,", "", string)
string

topic <-   which(grepl("van dijk", all_isi$AU, ignore.case = TRUE))
m_cited <- head(order(-M$TC), 10)

van <- CRsplit[topic,]

van$ref <- str_replace_all(van$ref, 
                        "^VAN DIJK J\\..?", 
                        "VAN DIJK J")
i <- topic
toJSON(all_isi[i, c(1)], pretty= T)


string <- "VAN DIJK J.A. G.M"

# Replace the string "VAN DIJK J.A. G.M" with "VAN DIJK J" using a regular expression
CR$ref <- sub("^VAN DIJK J\\..*", "VAN DIJK J", CR$ref)

print(string)  # Output: "VAN DIJK J"


v <- "VAN DIJK J.A. G.M, 2005, DEEPENING DIVIDE INE, DOI DOI 10.1080/15205430701528655"
extract_first_two <- function(v) {
  split_v <- unlist(strsplit(v , ","))
  first <- sub("^VAN DIJK J\\..*", "VAN DIJK J", v)
  first_two <- split_v[2:4]
  paste(first, first_two, collapse = ",")
}

cr_ncols <- max(stringr::str_count(CR$ref, ",")) 

cr_colmn <- paste("field", 1:ncols, sep ="") # create and paste the name of the colums

CRsplit <-
  tidyr::separate(
    data = CR,
    col = ref,
    sep = ",",
    into = colmn,
    remove = T
  )

van <- CRsplit[topic,]
# "^VAN DIJK J\\S*"




van$field1 <- str_replace_all(van$field1, "VAN DIJK", "VAN DIJK J") # this worked
van$field1 <- sub("VAN DIJK\\s+J.*$", "VAN DIJK J", van$field1) # this worked
van$field1 <- str_replace_all(van$field1, "BUREAU", "") # this worked

CRpaste <- data.frame(paste())


df <- data.frame(col1 = c("a", "b", "c"), col2 = c(" ", "e", "f"), col3 = c("g", "h", "i"))

# Paste all columns together
df$pasted <- apply(df, 1, function(x) paste(x, collapse = ","))

df


string <- "INTRONA LD, 2000, INF SOC"

# Use the str_extract function from stringr to extract the desired string using a regex pattern
result <- str_extract(string, "^[^,]*")

# Print the result
print(result)


CRsco <- CRsco %>% filter(!grepl("^[(0-9]", CRsco$ref))

out <- which(grepl("^\\([0-9]{4}\\)[^ ]", CRsco$AU))



df <- data.frame(col1= c("luisct, duvan, felipe", "carlos df, carolina t", 
                         "andresac, martu a", "castillo lc", 
                         "david ce", "john ct"),
                 col2=c(1, 1, 2, 2, 3, 3))

df1 <- data.frame(col1= c("luisct, duvan, felipe; carlos df, carolina t", 
                         "andresac, martu a; castillo lc", 
                         "david ce; john ct"),
                 col2=c(1, 2, 3))


df <- df %>% 
  separate(col1, into = c("col1_1", "col1_2"), sep = ", ", remove = TRUE)


df1 <- data.frame(col1=c("luis c", "carlos d", 
                        "andres a", "castillo l", "david c", "john c"),
                 col2=c(1, 1, 2, 2, 3, 3))

nchar <- rep(NA, nrow(df))

for (i in 1:nrow(df)) {
  nchar[i] <- tail(unlist(gregexpr(" ", df$col1[i])), 1) + 1
}


df$col1 <- substr(df$col1, start = 1, stop = nchar)


M$CR <- trim.leading(trimES(gsub("\\[,||\\[||\\]|| \\.\\. || \\. ","",M$CR)))

string <- "CHANG BL"
word(string,1)

string <- "Luis Carlos Perea"
nchar <-   tail(unlist(gregexpr(" ", string)), 1) +1
substring <- substr(string, start = 1, stop = nchar)

nchar <- rep(NA, nrow(df))

for (i in 1:nrow(df)) {
  nchar[i] <- tail(unlist(gregexpr(" ", df$col1[i])), 1) + 1
}

for (i in 1:nrow(df)) {
  # Check if the value in the first column of the current row is equal to the value in the second column of the previous row
  if ( nchar[i] == 0) {
    # If it is, assign the value in the first column of the current row to a variable
    df$col1 <- df$col1
  } else {
    # If it is not, call the function and assign the result to a variable
    df$col1 <- substr(df[i, 1], start = 1, stop = nchar)
    }
  # Do something with the value of x here
  # For example, you could print it or assign it to a new column in the data frame
  print(x)
}

df$col1 <- ifelse(nchar == 0, df$col1, substr(df$col1, start = 1, stop = nchar))


short <- CRsplit_dim[short_rows_d,]
df$col1[c(1,3)] <- "duvan T G"

topic <-   which(grepl("VAN DIJCK J", CRsco$AU, ignore.case = TRUE))

pattern <- "\\d{4}"  # Regular expression to match four consecutive digits
matches <- which(grepl(pattern, short$field1))

# Subset the data frame to remove the rows that match the pattern
short$field1 <- short[-matches ]


string <- c(" ADELL, J., CASTAÑEDA, L., TECNOLOGÍAS EMERGENTES, 
            ¿PEDAGOGÍAS EMERGENTES? (2012) EDUCACIÓN Y TECNOLOGÍA, PP. 13-32. , 
            HTTPS://DIGITUM.UM.ES/JSPUI/BITSTREAM/10201/29916/1/ADELL_CASTANEDA_EMERGENTES2012.PDF, 
            HERNÁNDEZ, J., PENNESI, M., SOBRINO, D., VÁZQUEZ, A.")
# Next, split the vector into a list of individual strings
strings <- strsplit(string, split = "\\.,")[[1]]

# Use the str_length() function to count the number of characters in each string
vec <- sapply(strings, str_length)

df <- data.frame(strings = c(" ADELL, J., CASTAÑEDA, L., TECNOLOGÍAS EMERGENTES, ¿PEDAGOGÍAS EMERGENTES? (2012) EDUCACIÓN Y TECNOLOGÍA, PP. 13-32. , HTTPS://DIGITUM.UM.ES/JSPUI/BITSTREAM/10201/29916/1/ADELL_CASTANEDA_EMERGENTES2012.PDF, HERNÁNDEZ, J., PENNESI, M., SOBRINO, D., VÁZQUEZ, A.",
                             "FOO, B., BAR, C., BAZ, D., QUX, E.",
                             "HELLO, WORLD!"))

# Use sapply() to apply strsplit() to each element in the 'strings' column
df$strings_split <- sapply(df$strings, function(x) strsplit(x, split = "\\.,")[[1]])

df$string_lengths <- sapply(df$strings_split, function(x) sapply(x, str_length))

string_lengths <- lapply(df$strings_split, length)

# Create a new column called 'filtered_strings' to store the filtered strings
df$filtered_strings <- lapply(df$strings_split, function(x) {
  # Initialize an empty list to store the filtered strings
  filtered_strings <- list()
  
  # Loop through each element in the list
  for (i in 1:length(x)) {
    # If the element is shorter than 25 characters, append it to the list of filtered strings
    if (nchar(x[[i]]) <= 25) {
      filtered_strings <- c(filtered_strings, x[[i]])
    } else {
      # If the element is greater than 25 characters, stop looping and return the list of filtered strings
      break
    }
  }
  return(filtered_strings)
})

df$filtered_strings_pasted <- sapply(df$filtered_strings, function(x) paste(x, collapse = "; "))


string <- c("ROWELL, L. 2030. CAN THE $100 LAPTOP",
            ", ARUMUGAM, P., (2023) A SURVEY ON RURAL INTERNET CONNECTIVITY IN INDIA.",
            "WWW.GOOGLE.COM/LOON, PROJECT LOON ACCESSED ON 12 DEC 2016")

string <- paste0(string, " ")

string <- c("ROWELL, L. 20I7. CAN THE $100 200I",
            ", ARUMUGAM, P., 202I A SURVEY ON 20I0 INTERNET 20I4 IN INDIA.",
            "WWW.GOOGLE.COM/LOON, 202I LOON ACCESSED ON 12 DEC 201I")



string <- str_replace_all(string, " 20I", " 201")
string <- str_replace_all(string, " 201I", " 2011")
string <- str_replace_all(string, " 202I", " 2021")
string <- str_replace_all(string, " 200I", " 2001")


str_extract(string, "\\d{4}")
year <- unlist(str_extract_all(string, "19\\d{2}|20\\d{2}"))


data_frame[which(data_frame$column1 == "value1"), 
           c("column2", 
             "column3", 
             "column4")] <- ifelse(data_frame$column1 == "value1",
                                   "new_value", 
                                   data_frame[which(data_frame$column1 == "value1"), 
                                              c("column2", "column3", "column4")])

class(all_sco) <- c("bibliometrixDB", "data.frame")
WA <- cocMatrix(all_sco, Field = "AU", type = "sparse", sep= ",")
WCR <- cocMatrix(all_sco, Field = "CR", type = "sparse", sep = ";")
CRA <- crossprod(WCR, WA)
NetMatrix <- crossprod(CRA, CRA)
net= networkPlot(NetMatrix, n = 20, 
                 Title = "Coupling", 
                 type = "fruchterman", 
                 size.cex=TRUE, size=20, 
                 remove.multiple=T, 
                 labelsize=1,edgesize = 10, edges.min=5)

(Field %in% c("ID", "DE", "TI", "TI_TM", "AB", "AB_TM"))
Field <- "CR"
type <- "sparse"
binary <- "FALSE"
short <- "FALSE"
m <- N[c(131:140), ]
size<-dim(m)
rownames(m) <- m$SRDI
RowNames <- row.names(m)
m$CR <- as.character(m$CR)

Fi <- strsplit(m[,Field], split = ";")
# Fi <- (do.call(rbind, Fi))
TERMS <- data.frame(item = trimws(unlist(Fi)), SR = rep(m$SR,lengths(Fi)))



TERMS <- TERMS %>% 
  group_by(.data$SR) %>%
  summarize(item = paste(.data$item, collapse=";"))

m <- m %>% 
  left_join(TERMS, by="SR")
m[,Field] <- m$item

row.names(m) <- RowNames

allField <- unlist(Fi)
allField <- allField[!is.na(allField)]


tabField <- sort(table(allField), decreasing = TRUE)
uniqueField <- names(tabField)	
class(tabField)

co_occur_matrix <- table(TERMS$SR,TERMS$item)
g <- graph.adjacency(co_occur_matrix, mode = "directed", weighted = TRUE)

if (!is.null(n)) {
  uniqueField <- uniqueField[1:n]
} else if (isTRUE(short)){
  uniqueField <- names(tabField[tabField>1])  # remove items with frequency<2
}
WF<- Matrix(0,size[1],length(uniqueField))


colnames(WF)<-uniqueField
rownames(WF)<-rownames(m)

for (i in 1:size[1]){
  if (length(Fi[[i]])>0 & !is.na(Fi[[i]][1])) {
    #print(i)
    #if (Field=="CR"){Fi[[i]]=reduceRefs(Fi[[i]])}
    if (isTRUE(binary)){
      ## binary counting
      ind <- uniqueField %in% Fi[[i]]
      if (sum(ind) > 0){
        WF[i, ind] <- 1
      }
    }
    else{
      ## full counting
      tab=table(Fi[[i]])
      name <- names(tab)[names(tab) %in% uniqueField]
      name <- name[nchar(name)>0]
      if (length(name)>0){
        WF[i,name] <- tab[name]
      }
    }
  }}

ind <- uniqueField %in% Fi[[i]]
if (sum(ind) > 0){
  WF[i, ind] <- 1
}
WF <- Matrix(WF)

tab=table(Fi[[i]])
name <- names(tab)[names(tab) %in% uniqueField]
name <- name[nchar(name)>0]
if (length(name)>0){
  WF[1,name] <- tab[name]
}

WF <- WF[,!is.na(uniqueField)]
ind <- which(colnames(WF)=="NA")
if (length(ind)>0) {WF <- WF[,-ind]}

return(WF)





indices <- which(WF > 1, arr.ind = TRUE)
head(indices)

library(igraph)

co_matrix <- matrix(c(2, 3, 2, 1, 2, 1, 2, 1, 2), nrow = 3)
sparse_matrix <- Matrix::sparseMatrix(i = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
                                      j = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                                      x = co_matrix,
                                      dims = c(3, 3))


WAm <- cocMatrix(N, Field = "AU", type = "sparse", sep = ";")
WCRm <- cocMatrix(N, Field = "CR", type = "sparse", sep = ";")
CRAm <- crossprod(WCRm, WAm)
NetMatrixm <- crossprod(CRAm, CRAm)



net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", 
                type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=FALSE, 
                labelsize=1, edgesize = 10, edges.min=5)


df <- data.frame(ID = c(1,2,3, 4, 5, 6, 7, 8, 9, 10), 
                 V1 = c("England", "England", "England", 
                        "France", "France","France",
                        "Germany", "Germany", "Germany",
                        "Austria"),
                 V2 = c("Greece", "Spain", "Portugal", 
                        "England", "Germany", "Spain",
                        "Italy", "Greece", "England",
                        "Poland"))

df1 <- as.data.frame(table(df))
df2 <- subset(df1, Freq>0)
net_a <- graph_from_data_frame(df2,directed = F, vertices = NULL)
E(net_a)$weight <- E(net_a)$Freq
net_a

# igraph summary
gsize(net_a)
gorder(net_a)

# Node list

V(net_a)

# Edgelist

E(net_a)

# Adjancency matrix
net_a[c(1:10), c(1:10)]



a <- as.matrix(network(df[-1],directed = FALSE))
a
i <- as.matrix(as_adj(graph_from_data_frame(df[-1], FALSE)))
b <- table(df[, -1])

netm <- crossprod(b, b)
class(a) <- c("bibliometrixDB", "data.frame")
set.seed(1001)

colnames(net_a)<-df$V2
rownames(net_a)<-df$V2

plot(net_a, vertex.size=7, vertex.label=NA, vertex.color="orange", edge.size=15, edge.color="blue", edge.arrow.size=0.4)
title(main="Graph layout nicely", cex.main=0.8)


net=networkPlot(net_a, n = 5, Title = "Co-Citation Network", 
                type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=FALSE, 
                labelsize=1, edgesize = 10, edges.min=5)


dfc <- data.frame(ID = c(1,2,3, 4), 
                  SR = c("England, 2015", "France, 2012", "Germany, 2002",
                         "Austria, 2006"),
                  CR = c("Greece; Spain; Portugal", 
                         "England; Germany; Spain",
                         "Italy; Greece; England",
                         "Poland"))

Field <- "CR"
type <- "sparse"
binary <- "TRUE"
short <- "FALSE"
p <- n_1p
size<-dim(p)
rownames(p) <- p$SR
RowNames <- row.names(p$SR)
p$CR <- as.character(p$CR)

Fi <- strsplit(p[,Field], split = ";")
# Fi <- (do.call(rbind, Fi))
TERMS <- data.frame(item = trimws(unlist(Fi)), SR = rep(p$SR,lengths(Fi)))



TERMS <- TERMS %>% 
  group_by(.data$SR) %>%
  summarize(item = paste(.data$item, collapse=";"))

p <- p %>% 
  left_join(TERMS, by="SR")
p[,Field] <- p$item

row.names(p) <- RowNames

allField <- unlist(Fi)
allField <- allField[!is.na(allField)]


tabField <- sort(table(allField), decreasing = TRUE)
uniqueField <- names(tabField)	
class(tabField)

co_occur_matrix <- table(TERMS$SR,TERMS$item)
g <- graph.adjacency(co_occur_matrix, mode = "directed", weighted = TRUE)

if (!is.null(n)) {
  uniqueField <- uniqueField[1:n]
} else if (isTRUE(short)){
  uniqueField <- names(tabField[tabField>1])  # remove items with frequency<2
}
WF<- Matrix(0,size[1],length(uniqueField))


colnames(WF)<-uniqueField
rownames(WF)<-rownames(p)

for (i in 1:size[1]){
  if (length(Fi[[i]])>0 & !is.na(Fi[[i]][1])) {
    #print(i)
    #if (Field=="CR"){Fi[[i]]=reduceRefs(Fi[[i]])}
    if (isTRUE(binary)){
      ## binary counting
      ind <- uniqueField %in% Fi[[i]]
      if (sum(ind) > 0){
        WF[i, ind] <- 1
      }
    }
    else{
      ## full counting
      tab=table(Fi[[i]])
      name <- names(tab)[names(tab) %in% uniqueField]
      name <- name[nchar(name)>0]
      if (length(name)>0){
        WF[i,name] <- tab[name]
      }
    }
  }}

ind <- uniqueField %in% Fi[[i]]
if (sum(ind) > 0){
  WF[i, ind] <- 1
}
WF <- Matrix(WF)

tab=table(Fi[[i]])
name <- names(tab)[names(tab) %in% uniqueField]
name <- name[nchar(name)>0]
if (length(name)>0){
  WF[1,name] <- tab[name]
}

WF <- WF[,!is.na(uniqueField)]
ind <- which(colnames(WF)=="NA")
if (length(ind)>0) {WF <- WF[,-ind]}

return(WF)

class(dfc) <- c("bibliometrixDB", "data.frame")
net_p <- crossprod(WF,WF)
net_p <- cocMatrix(n_1p, Field = "AU", type = "sparse", sep = ";", binary = T)

net=networkPlot(net_p, n = 30, Title = "Co-Citation Network", 
                type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=FALSE, 
                labelsize=1, edgesize = 10, edges.min=5)

net=networkPlot(net_p, n = 30, Title = "Co-Citation Network", 
                type = "fruchterman", size.cex=TRUE, size=20, 
                remove.multiple=FALSE, labelsize=2,edgesize = 10, edges.min=5)



test <- labelShort(n_1p, db= "isi")
LABEL <- removeDuplicatedlabels(N$LABEL)


cbind(cited[1:20])
cited <- citations(N, field = "author", sep = ";")      
      
      
x <- c("Becker S; Miron-Shatz T; Schumacher N; Krocza J; Diamantidis C; Albrecht U")

# split the string by ";"
y <- strsplit(x, split = ";")

# Get the number of authors
n_authors <- length(y[[1]])

# if n_authors > 2 take the first two authors
if(n_authors > 2){
  short_author <- paste(y[[1]][1], y[[1]][2], sep = ";", collapse = " ")
  short_author <- paste(short_author,"et al.")
}else{
  short_author <- x
}

# Create a new dataframe with original vector and short author
df <- data.frame(original_vector = x, short_author = short_author)



df <- data.frame(authors = c("Becker S; Miron-Shatz T; Schumacher N; Krocza J; Diamantidis C; Albrecht U", 
                             "Smith J; Johnson T; Williams M", 
                             "Jones B; Brown R; Garcia L"),  PY = c(2000, 2010,2015))

# Create a new column 'short_authors'
df$short_authors <- lapply(df$authors, function(x) {
  y <- strsplit(x, split = ";")
  n_authors <- length(y[[1]])
  if(n_authors > 2){
    short_author <- paste(y[[1]][1], y[[1]][2], sep = ";", collapse = " ")
    short_author <- paste(short_author,"et al.")
    short_author <- paste(short_author, df$PY, sep = ", ")
  }else{
    short_author <- paste(x, df$PY, sep = ", ")
  }
  return(short_author)
})      
      

Y <- as.numeric(substr(Sys.time(),1,4))
listAU <- (strsplit(MM$AU, ";"))
nAU <- lengths(listAU)
df <- data.frame(AU=trimws(unlist(listAU)), SR=rep(MM$SR,nAU)) 
AU <- df %>% 
  group_by(AU) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% head(15)

df <- df %>% 
  right_join(AU, by = "AU") %>%
  left_join(MM, by = "SR") %>% 
  select(AU.x,PY, TI, SO, DI, TC) %>% 
  mutate(TCpY = TC/(Y-PY+1)) %>%
  group_by(AU.x) %>% 
  mutate(n = length(AU.x)) %>% 
  ungroup() %>% 
  rename(Author = AU.x,
         year = PY,
         DOI = DI) %>% 
  arrange(desc(n), desc(year)) %>% 
  select(-n)


df2 <- dplyr::group_by(df, Author,year) %>%
  dplyr::summarise(freq=length(year),TC=sum(TC),TCpY=sum(TCpY)) %>% 
  as.data.frame() %>% arrange(desc(freq))
df2$Author <- factor(df2$Author,levels=AU$AU[1:15]) 
x <- c(0.5,1.5*15/10)
y <- c(min(df$year),min(df$year)+diff(range(df2$year))*0.125)

summary_df <- df2 %>% group_by(Author) %>% 
  summarise(total_freq = sum(freq)) %>% arrange(desc(total_freq))

g <- ggplot(df2, aes(x= Author, y=year, round(TCpY,2))) +
  geom_point(aes(alpha=TCpY,size = freq), color="dodgerblue4")+ 
  scale_size(range=c(2,6))+
  scale_alpha(range=c(0.3,1))+
  scale_y_continuous(breaks = seq(min(df2$year),max(df2$year), by=2))+
  guides(size = guide_legend(order = 1, "N.Articles"), 
         alpha = guide_legend(order = 2, "TC per Year"))+
  theme(legend.position = 'right'
        ,text = element_text(color = "#444444")
        ,panel.background = element_rect(fill = '#FFFFFF')
        ,plot.title = element_text(size = 24)
        ,axis.title = element_text(size = 14, color = '#555555')
        ,axis.title.y = element_text(vjust = 1, angle = 90)
        ,axis.title.x = element_text(hjust = .5)
        ,axis.text.x = element_text(face="bold")
        ,axis.text.y = element_text(face="bold")
        ,axis.line.x = element_line(color="grey50", size=0.5)
        ,panel.grid.major.x = element_blank() 
        ,panel.grid.major.y = element_line( size=.2, color="grey90")) +
  labs(title="Authors' Production over Time", x="Authors", y ="Years") + 
  theme(axis.title.y = element_text(vjust = 1)) +
  geom_line(data=df2,aes(x = Author, y = year, 
                         group=Author),size=1.0, color="firebrick4", alpha=0.3 )+
  coord_flip() + geom_text(data=summary_df, aes(x=Author, y=Inf, label= total_freq),
                         hjust=1, vjust=0.5, size=4, color="grey30") +
  scale_x_discrete(limits = rev(levels(df2$Author)))

g 


citions <- citations(MM, field = "author", sep = ";")
citions$Cited[1:10]
H <- Hindex(MM, field = "author", sep = ";", years = Inf)
head(H$CitationList[[2]])
H$CitationList
M_1p
M_2p
M_3p

VD <- df %>% dplyr::filter(AU == "VAN DIJK J") %>% arrange(desc(TC))



Y <- as.numeric(substr(Sys.time(),1,4))
listAU <- (strsplit(MM$AU, ";"))
nAU <- lengths(listAU)
df <- data.frame(AU=trimws(unlist(listAU)), SR=rep(MM$SR,nAU), TC=rep(MM$TC,nAU)) 
AUcite <- df %>% 
  group_by(AU) %>% 
  summarise(tot_cite= sum(TC)) %>% 
  arrange(desc(tot_cite)) %>% 
  ungroup() %>% head(30)

dfcite <- df %>% 
  right_join(AUcite, by = "AU") %>%
  left_join(MM, by = "SR") %>% 
  select(AU.x,PY, TI, SO, DI, TC.x) %>% 
  mutate(TCpY = TC.x/(Y-PY+1)) %>%
  group_by(AU.x) %>% 
  mutate(n = length(AU.x)) %>% 
  ungroup() %>% 
  rename(Author = AU.x,
         year = PY,
         DOI = DI,
         TC = TC.x) %>% 
  arrange(desc(n), desc(year)) %>% 
  select(-n)

df2cite <- dplyr::group_by(dfcite, Author,year) %>%
  dplyr::summarise(TC=sum(TC),TCpY=sum(TCpY)) %>% 
  as.data.frame() %>% arrange(desc(TC))
df2cite$Author <- factor(df2cite$Author,levels=AUcite$AU[1:30]) 
x <- c(0.5,1.5*30/10)
y <- c(min(dfcite$year),min(dfcite$year)+diff(range(df2cite$year))*0.125)

summary_dfcite <- df2cite %>% group_by(Author) %>% 
  summarise(total_TC = sum(TC)) %>% arrange(desc(total_TC))

h <- ggplot(df2cite, aes(x= Author, y=year, round(TCpY,2))) +
  geom_point(aes(alpha=TCpY,size = TC), color="dodgerblue4")+ 
  scale_size(range=c(2,6))+
  scale_alpha(range=c(0.3,1))+
  scale_y_continuous(breaks = seq(min(df2cite$year),max(df2cite$year), by=2))+
  guides(size = guide_legend(order = 1, "TC"), 
         alpha = guide_legend(order = 2, "TC per Year"))+
  theme(legend.position = 'right'
        ,text = element_text(color = "#444444")
        ,panel.background = element_rect(fill = '#FFFFFF')
        ,plot.title = element_text(size = 24)
        ,axis.title = element_text(size = 14, color = '#555555')
        ,axis.title.y = element_text(vjust = 1, angle = 90)
        ,axis.title.x = element_text(hjust = .5)
        ,axis.text.x = element_text(face="bold")
        ,axis.text.y = element_text(face="bold")
        ,axis.line.x = element_line(color="grey50", size=0.5)
        ,panel.grid.major.x = element_blank() 
        ,panel.grid.major.y = element_line( size=.2, color="grey90")) +
  labs(title="Authors' Production over Time", x="Authors", y ="Years") + 
  theme(axis.title.y = element_text(vjust = 1)) +
  geom_line(data=df2cite,aes(x = Author, y = year, 
                         group=Author),size=1.0, color="firebrick4", alpha=0.3 )+
  coord_flip() + geom_text(data=summary_dfcite, aes(x=Author, y=Inf, label= total_TC),
                           hjust=1, vjust=0.5, size=4, color="grey30") +
  scale_x_discrete(limits = rev(levels(df2cite$Author)))

h 


#### first period 

Y <- as.numeric(substr(Sys.time(),1,4))
listAU1 <- (strsplit(M_1p$AU, ";"))
nAU <- lengths(listAU1)
df1 <- data.frame(AU=trimws(unlist(listAU1)), SR=rep(M_1p$SR,nAU), TC=rep(M_1p$TC,nAU)) 
AUcite1 <- df1 %>% 
  group_by(AU) %>% 
  summarise(tot_cite= sum(TC)) %>% 
  arrange(desc(tot_cite)) %>% 
  ungroup() %>% head(10)

dfcite1 <- df1 %>% 
  right_join(AUcite1, by = "AU") %>%
  left_join(M_1p, by = "SR") %>% 
  select(AU.x,PY, TI, SO, DI, TC.x) %>% 
  mutate(TCpY = TC.x/(Y-PY+1)) %>%
  group_by(AU.x) %>% 
  mutate(n = length(AU.x)) %>% 
  ungroup() %>% 
  rename(Author = AU.x,
         year = PY,
         DOI = DI,
         TC = TC.x) %>% 
  arrange(desc(n), desc(year)) %>% 
  select(-n)

df2cite1 <- dplyr::group_by(dfcite1, Author,year) %>%
  dplyr::summarise(TC=sum(TC),TCpY=sum(TCpY)) %>% 
  as.data.frame() %>% arrange(desc(TC))
df2cite1$Author <- factor(df2cite1$Author,levels=AUcite1$AU[1:10]) 
x <- c(0.5,1.5*10/10)
y <- c(min(dfcite1$year),min(dfcite1$year)+diff(range(df2cite1$year))*0.125)

summary_dfcite1 <- df2cite1 %>% group_by(Author) %>% 
  summarise(total_TC = sum(TC)) %>% arrange(desc(total_TC))


#### second period

Y <- as.numeric(substr(Sys.time(),1,4))
listAU2 <- (strsplit(M_2p$AU, ";"))
nAU <- lengths(listAU2)
df2 <- data.frame(AU=trimws(unlist(listAU2)), SR=rep(M_2p$SR,nAU), TC=rep(M_2p$TC,nAU)) 
AUcite2 <- df2 %>% 
  group_by(AU) %>% 
  summarise(tot_cite= sum(TC)) %>% 
  arrange(desc(tot_cite)) %>% 
  ungroup() %>% head(10)

dfcite2 <- df2 %>% 
  right_join(AUcite2, by = "AU") %>%
  left_join(M_2p, by = "SR") %>% 
  select(AU.x,PY, TI, SO, DI, TC.x) %>% 
  mutate(TCpY = TC.x/(Y-PY+1)) %>%
  group_by(AU.x) %>% 
  mutate(n = length(AU.x)) %>% 
  ungroup() %>% 
  rename(Author = AU.x,
         year = PY,
         DOI = DI,
         TC = TC.x) %>% 
  arrange(desc(n), desc(year)) %>% 
  select(-n)

df2cite2 <- dplyr::group_by(dfcite2, Author,year) %>%
  dplyr::summarise(TC=sum(TC),TCpY=sum(TCpY)) %>% 
  as.data.frame() %>% arrange(desc(TC))
df2cite2$Author <- factor(df2cite2$Author,levels=AUcite2$AU[1:10]) 
x <- c(0.5,1.5*10/10)
y <- c(min(dfcite2$year),min(dfcite2$year)+diff(range(df2cite2$year))*0.125)

summary_dfcite2 <- df2cite2 %>% group_by(Author) %>% 
  summarise(total_TC = sum(TC)) %>% arrange(desc(total_TC))



#### third period


Y <- as.numeric(substr(Sys.time(),1,4))
listAU3 <- (strsplit(M_3p$AU, ";"))
nAU <- lengths(listAU3)
df3 <- data.frame(AU=trimws(unlist(listAU3)), SR=rep(M_3p$SR,nAU), TC=rep(M_3p$TC,nAU)) 
AUcite3 <- df3 %>% 
  group_by(AU) %>% 
  summarise(tot_cite= sum(TC)) %>% 
  arrange(desc(tot_cite)) %>% 
  ungroup() %>% head(10)

dfcite3 <- df3 %>% 
  right_join(AUcite3, by = "AU") %>%
  left_join(M_3p, by = "SR") %>% 
  select(AU.x,PY, TI, SO, DI, TC.x) %>% 
  mutate(TCpY = TC.x/(Y-PY+1)) %>%
  group_by(AU.x) %>% 
  mutate(n = length(AU.x)) %>% 
  ungroup() %>% 
  rename(Author = AU.x,
         year = PY,
         DOI = DI,
         TC = TC.x) %>% 
  arrange(desc(n), desc(year)) %>% 
  select(-n)

df2cite3 <- dplyr::group_by(dfcite3, Author,year) %>%
  dplyr::summarise(TC=sum(TC),TCpY=sum(TCpY)) %>% 
  as.data.frame() %>% arrange(desc(TC))
df2cite3$Author <- factor(df2cite3$Author,levels=AUcite3$AU[1:10]) 
x <- c(0.5,1.5*10/10)
y <- c(min(dfcite3$year),min(dfcite3$year)+diff(range(df2cite3$year))*0.125)

summary_dfcite3 <- df2cite3 %>% group_by(Author) %>% 
  summarise(total_TC = sum(TC)) %>% arrange(desc(total_TC))


#### total  

dfcite_tot <- rbind(dfcite1,dfcite2,dfcite3)


df2cite_tot <- rbind(df2cite1, df2cite2, df2cite3)

summary_dfcite_t <- df2cite_tot %>% group_by(Author) %>% 
  summarise(total_TC = sum(TC)) %>% arrange(desc(total_TC))


h <- ggplot(df2cite_tot, aes(x= Author, y=year, round(TCpY,2))) +
  geom_point(aes(alpha=TCpY,size = TC), color="dodgerblue4")+ 
  geom_vline(xintercept = c(2008, 2015), linetype = "dotted", color = "blue") +
  scale_size(range=c(2,6))+
  scale_alpha(range=c(0.3,1))+
  scale_y_continuous(breaks = seq(min(df2cite_tot$year),max(df2cite_tot$year), by=2))+
  guides(size = guide_legend(order = 1, "TC"), 
         alpha = guide_legend(order = 2, "TC per Year"))+
  theme(legend.position = 'right'
        ,text = element_text(color = "#444444")
        ,panel.background = element_rect(fill = '#FFFFFF')
        ,plot.title = element_text(size = 24)
        ,axis.title = element_text(size = 14, color = '#555555')
        ,axis.title.y = element_text(vjust = 1, angle = 90)
        ,axis.title.x = element_text(hjust = .5)
        ,axis.text.x = element_text(face="bold")
        ,axis.text.y = element_text(face="bold")
        ,axis.line.x = element_line(color="grey50", size=0.5)
        ,panel.grid.major.x = element_blank() 
        ,panel.grid.major.y = element_line( size=.2, color="grey90")) +
  labs(title="Authors' Citations over Time", x="Authors", y ="Years") + 
  theme(axis.title.y = element_text(vjust = 1)) +
  geom_line(data=df2cite_tot,aes(x = Author, y = year, 
                             group=Author),size=1.0, color="firebrick4", alpha=0.3 )+
  coord_flip() + geom_text(data=summary_dfcite_t, aes(x=Author, y=Inf, label= total_TC),
                           hjust=1, vjust=0.5, size=4, color="grey30") +
  scale_x_discrete(limits = rev(levels(df2cite_tot$Author))) 
h 


#### H- index

AU = MM$AU
AU <- trimES(gsub(","," ",AU))
listAU <- strsplit(AU, split=";")
l <- lengths(listAU)
index= rep(row.names(MM), l)
df <- MM[index,]
df$AUs <- unlist(listAU)
df$AUs <- trim(df$AUs)

h_calc <- function(x){
  h <- tail(which(1:length(x) <= sort(x,decreasing = T)),1) #[1]-1
  return(h)
}


H <- df %>% 
  group_by(AUs) %>% 
  summarize(#Element = .data$AUs[1],
    h_index = h_calc(TC),
    PY_start = min(PY),
    TC = sum(TC),
    NP = length(AUs)) %>%
  rename(AU = AUs) %>%
  as.data.frame() %>% arrange(desc(h_index))



#### Dot chart for the moes influential articles 

# P1 <- ggdotchart(PD1p, x = "Paper", y = "TC",
#                  title = "Most Influencial Papers",
#                  color = "#005997",
#                  palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
#                  sorting = "descending",                       # Sort value in descending order
#                  add = "segments",                             # Add segments from y = 0 to dots
#                  rotate = TRUE,                                # Order by groups
#                  dot.size = 9,                                 # Large dot size
#                  label = round(PD1p$TC),                        # Add mpg values as dot labels
#                  font.label = list(color = "white", size = 9, 
#                                    vjust = 0.5),               # Adjust label parameters
#                  ggtheme = theme_classic()                        # ggplot2 theme
# ) 

# P2 <- ggdotchart(PD2p, x = "Paper", y = "TC",
#            title = "Most Influencial Papers",
#            color = "#005997",
#            palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
#            sorting = "descending",                       # Sort value in descending order
#            add = "segments",                             # Add segments from y = 0 to dots
#            rotate = TRUE,                                # Order by groups
#            dot.size = 9,                                 # Large dot size
#            label = round(PD2p$TC),                        # Add mpg values as dot labels
#            font.label = list(color = "white", size = 9, 
#                              vjust = 0.5),               # Adjust label parameters
#            ggtheme = theme_classic()                        # ggplot2 theme
#            ) 

# P3 <- ggdotchart(PD3p, x = "Paper", y = "TC",
#            title = "Most Influencial Papers",
#            color = "#005997",
#            palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
#            sorting = "descending",                       # Sort value in descending order
#            add = "segments",                             # Add segments from y = 0 to dots
#            rotate = TRUE,                                # Order by groups
#            dot.size = 9,                                 # Large dot size
#            label = round(PD3p$TC),                        # Add mpg values as dot labels
#            font.label = list(color = "white", size = 9, 
#                              vjust = 0.5),               # Adjust label parameters
#            ggtheme = theme_classic()                        # ggplot2 theme
#            ) 

PA <- kable(desc_a,"latex") %>% 
  kable_styling(latex_options="scale_down") %>% 
  row_spec(0, bold=T)



df <- data.frame(reference = c("NORRIS P, 2001, DOI 10.1017/cbo9781139164887",
                               "SMITH J, 2005, J EXP BIOL, V208, P2409",
                               "BROWN M, 2010, J CHEM PHYS, V132, DOI 10.1063/1.3374669"))

# separate reference column into author, year, and DOI columns
df <- separate(df, col = reference, into = c("AU", "PY", "DOI"), sep = ",\\s+|\\s+DOI\\s+")

# view output
df


journals <- c("Journal 1", "Journal 2", "Journal 3", "Journal 4")
times_cited <- c(200, 300, 150, 250)
published_documents <- c(20, 25, 30, 35)
df <- data.frame(journals, times_cited, published_documents)

# create the scatter plot
ggplot(df, aes(x = times_cited, y = published_documents)) +
  geom_point(size = 3) +
  geom_text(aes(label = journals), hjust = -0.1, vjust = 0.5) +
  labs(x = "Times Cited", y = "Published Documents")


# create example data
journal <- c("Journal 1", "Journal 2", "Journal 3", "Journal 4", "Journal 5")
pub_count <- c(100, 200, 300, 400, 500)
cite_count <- c(500, 400, 300, 200, 100)

# create data frame
df <- data.frame(journal, pub_count, cite_count)

ggplot(df, aes(x = journal, fill= pub_count)) +
  geom_bar(aes(y = pub_count, fill= "Publication COunt"), fill = "darkblue", alpha = 0.7, stat = "identity") +
  geom_bar(aes(y = -cite_count), fill = "darkred", alpha = 0.7, stat = "identity") +
  scale_y_continuous(labels = abs) +
  labs(title = "Publication and Citation Counts by Journal",
       y = "Count",
       x = "Journal") +
  theme(legend.position = "right") +
  theme_classic() +
  coord_flip()


ggplot(SO, aes(x = SO)) +
  geom_bar(aes(y = PD, fill = "Publication Count"),
           alpha = 0.7, stat = "identity") +
  geom_bar(aes(y = -TC, fill = "Citation Count"),
           alpha = 0.7, stat = "identity") +
  scale_y_continuous(labels = abs) +
  labs(title = "Publication and Citation Counts by Journal",
       y = "Count",
       x = "Journal",
       fill = "") +
  scale_fill_manual(values = c("darkblue", "darkred"),
                    labels = c("Publications", "Citations")) +
  theme(legend.position = "left") +
  theme_classic() +
  coord_flip()


#### Network analysis ####

# Create example data
df <- data.frame(
  country = c("USA", "Canada", "Mexico"),
  docs_published = c(2000, 1500, 1000),
  times_cited = c(5000, 4000, 3000)
)

# Create the plot
ggplot(df, aes(x = country)) +
  geom_col(aes(y = docs_published), fill = "blue", width = 0.4) +
  geom_col(aes(y = times_cited), fill = "red", width = -0.4) +
  scale_y_continuous(labels = abs) +
  labs(x = "Country", y = "Count") +
  ggtitle("Two-Sided Bar Chart") +
  theme_classic()










