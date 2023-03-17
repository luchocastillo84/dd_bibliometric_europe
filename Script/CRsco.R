#### Load the packages ####
library(tidyverse)
library(tidyr)
library(stringr)
library(bibliometrix)
library(tidytext)
library(here)

load(here("Script", "Environments", "CRsco_env.RData"))
#### Loading the data base Citing docs ####

m_bdf <- convert2df(here("Data",  # processed data frame for bibliometrics 30 columns
                         "Processed", 
                         "B_EU.csv"),
                    dbsource = "scopus", 
                    format = "csv")

all_sco <- m_bdf %>% filter(PF %in% c("SCO")) # filter only scopus documents

all_sco = all_sco[order(all_sco$PY), ] # this makes a descendant order of the year

#all_sco <- all_sco %>% # this will add a column paper with the consecutive starting
#  mutate(Paper = row_number() + 574) # from 574
all_sco$Paper <- 1:nrow(all_sco)
all_sco_orig <- all_sco # creates the original df

#all_sco <- all_sco %>% # this will add a column nLALBEL with the consecutive starting
#  mutate(nLABEL = row_number() + 574)# from 574
all_sco$nLABEL <- 1:nrow(all_sco)

all_sco$CR <- str_replace_all(all_sco$CR, ";;", ";") # replace patterns 
all_sco$CR <- str_replace_all(all_sco$CR, "; :,", "") # replace patterns 
all_sco$CR <- str_replace_all(all_sco$CR, ". ; () ", "") # replace patterns 

all_sco_AU <- all_sco %>% select(AU) # this selects the AU column from all_sco

sco_AU <- strsplit(all_sco$AU, 
                   split = ";") # this code splits the AU and create a list

sco_AU <- lapply(seq_along(sco_AU), function(i) { # this function creates a data frame 
  l <- data.frame(AU = sco_AU[[i]], # for the number of CR 
                  paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

sco_AU <- (do.call(rbind, sco_AU)) # this binds all the CR of each paper into a df format

nchar <- rep(NA, nrow(sco_AU))

for (i in 1:nrow(sco_AU)) {
  nchar[i] <- tail(unlist(gregexpr(" ", sco_AU$AU[i])), 1) + 1
}

sco_AU$AU <- ifelse(nchar == 0,
                    sco_AU$AU,
                       substr(sco_AU$AU,
                              start = 1,
                              stop = nchar)) # make sure to run the nchar

sco_AU <- sco_AU %>% group_by(paper) %>% 
  dplyr::summarise(AU = paste(AU, collapse = "; "))

all_sco$AU <- sco_AU$AU


#### Loading the data base cited docs or CR ####

CRsco_d <- convert2df(here("Data", # loading CR raw files as bibliometric df
                            "Raw", 
                            "CRsco_di.csv"),
                       dbsource = "scopus",
                       format = "csv")

CRsco1 <- read_csv(here("Data", # loading CR raw files as csv files 
                        "Raw", 
                        "CRsco_di.csv"),
                   col_names = T)

year0 <- which(CRsco1$Year == "0000")
CRsco1 <- CRsco1[-year0,]

CRsco1[, c(1,3, 5)] <- lapply(CRsco1[, c(1,3, 5)], # to upper case AU, TI and SO
                                toupper)
CRsco1$Authors <- str_replace_all(CRsco1$Authors, 
                                    "\\.,", ";") # replace patterns in AU column
CRsco1$Authors <- str_replace_all(CRsco1$Authors, 
                                    "\\,", "") # replace patterns in AU column
CRsco1$Authors <- str_replace_all(CRsco1$Authors, 
                                    "\\.", "") # replace patterns in AU column
CRsco1$paper <- 1:nrow(CRsco1) # create a column with the paper number

CRsco1_AU <- CRsco1 %>% dplyr::select(Authors) # create a df with only the AU

CRsco1_AU <- strsplit(CRsco1_AU$Authors, 
                  split = ";") # this code splits the CR and create a list

CRsco1_AU <- lapply(seq_along(CRsco1_AU), function(i) { # this function creates a data frame 
  l <- data.frame(AU = CRsco1_AU[[i]], # for the number of CR 
                  paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

CRsco1_AU <- (do.call(rbind, CRsco1_AU)) # this binds all the CR of each paper into a df format

CRsco1_AU$AU <- trimws(CRsco1_AU$AU)

CRsco1_AU$AU <- str_replace_all(CRsco1_AU$AU, # replace patterns in AU column
                                    "\\[NO AUTHOR NAME AVAILABLE]"
                                    ,"NOT-AVAILABLE") 

nchar <- rep(NA, nrow(CRsco1_AU))

for (i in 1:nrow(CRsco1_AU)) {
  nchar[i] <- tail(unlist(gregexpr(" ", CRsco1_AU$AU[i])), 1) + 1
}

CRsco1_AU$AU <- ifelse(nchar == 0,
                      CRsco1_AU$AU,
                        substr(CRsco1_AU$AU,
                               start = 1,
                               stop = nchar)) # make sure to run the nchar


CRsco1_AU <- CRsco1_AU %>% group_by(paper) %>% 
  dplyr::summarise(AU = paste(AU, collapse = ", "))

CRsco1_AU <- inner_join(CRsco1_AU, CRsco1[, c(3, 4, 5 ,13, 20)], by = "paper")

names(CRsco1_AU) <- c("Paper", "AU", "TI", "PY", "SO", "DI")

CRsco1_AU$SR <- # creates a column 
  paste(CRsco1_AU$AU, ", ", 
        CRsco1_AU$PY, 
        sep = "")

CRsco1_AU <- CRsco1_AU%>%  distinct(SR, SO, .keep_all = T)

vis_miss(CRsco1)

#### Scopus LCS 361 obs ####

CRsco2 <- strsplit(all_sco$CR, 
                  split = ";") # this code splits the CR and create a list

CRsco2 <- lapply(seq_along(CRsco2), function(i) { # this function creates a data frame 
  l <- data.frame(ref = CRsco2[[i]], # for the number of CR 
                  paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

CRsco2 <- (do.call(rbind, CRsco2)) # this binds all the CR of each paper into a df format

short_rows <- which(nchar(CRsco2$ref) < 50) # choose the ref index rows below 50 char

CRsco2 <- CRsco2[-short_rows,] # exclude the rows below 50 char

# Get only author's names 


nchar <- rep(NA, nrow(CRsco2))

for (i in 1:nrow(CRsco2)) {
  nchar[i] <- tail(unlist(gregexpr("\\.,", CRsco2$ref[i])), 1) +1
}

CRsco2$AU <- ifelse(nchar <= 0,
                      CRsco2$ref,
                      substr(CRsco2$ref,
                             start = 1,
                             stop = nchar)) # make sure to run the nchar
CRsco2$AU <- trimws(CRsco2$AU) # trimming leading white spaces in AU column
ind_par <- grep("^\\(", CRsco2$AU) # getting the indices of rows that starts with (
CRsco2 <- CRsco2[-ind_par,]

CRsco2$AU <- str_replace_all(CRsco2$AU, 
                                    "\\.,", ";") # replace patterns in AU column
CRsco2$AU <- str_replace_all(CRsco2$AU,
                                    "\\,", "") # replace patterns in AU column
CRsco2$AU <- str_replace_all(CRsco2$AU,
                                    "\\.", "") # replace patterns in AU column

CRsco2_AU <- CRsco2 %>% dplyr::select(AU) # Selecting the AU column from CRsco
CRsco2_AU$AU <-  as.character(CRsco2_AU$AU) # converting AU to character
# CRsco2_AU$paper <- 1:nrow(CRsco2_AU)

CRsco2_AU <- strsplit(CRsco2_AU$AU, 
                  split = ";") # this code splits the CR and create a list

CRsco2_AU <- lapply(seq_along(CRsco2_AU), function(i) { # this function creates a data frame 
  l <- data.frame(AU = CRsco2_AU[[i]], # for the number of CR 
                  cr_paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

CRsco2_AU <- (do.call(rbind, CRsco2_AU)) # this binds all the CR of each paper into a df format
CRsco2_AU$AU <- trimws(CRsco2_AU$AU) # trimming leading white spaces in AU column

nchar <- rep(NA, nrow(CRsco2_AU)) # create a vector with CRsco2_AU row number

for (i in 1:nrow(CRsco2_AU)) { # Loop through the rows of the CRsco2_AU data frame and 
  nchar[i] <- tail(unlist(gregexpr(" ", # compute the number of characters in the AU column of each row
                                   CRsco2_AU$AU[i])), 1) +1 
}

CRsco2_AU$AU <- ifelse(nchar == 0 | nchar > 30 | nchar <=2,
                       CRsco2_AU$AU,
                       substr(CRsco2_AU$AU,
                              start = 1,
                              stop = nchar)) # make sure to run the nchar

CRsco2_AU$nchar <- nchar # add column nchar to the df

CRsco2_AU <- CRsco2_AU %>% filter(nchar != 0 & nchar < 37)
CRsco2_AU <- CRsco2_AU[,-3] # removing the nchar col from the df

ind_par <- grep("^\\(", CRsco2_AU$AU) # getting the indices of rows that starts with (
CRsco2_AU <- CRsco2_AU[-ind_par,]



# Replace all occurrences of "VAN DIJK" with "VAN DIJK J" 
# in the AU column of the CRsco2_AU data frame
CRsco2_AU$AU <- str_replace_all(CRsco2_AU$AU, 
                                "VAN DIJK", 
                                "VAN DIJK J") 

CRsco2_AU$AU <- sub("VAN DIJK\\s+J.*$", 
                    "VAN DIJK J", 
                    CRsco2_AU$AU)# standardize

CRsco2_AU$AU <- sub("VAN DEURSEN\\s+A.*$", 
                    "VAN DEURSEN A", 
                    CRsco2_AU$AU) # standardize the name of this author

CRsco2$cr_paper <- 1:nrow(CRsco2)

CRsco2_AU <- CRsco2_AU %>% group_by(cr_paper) %>% 
  dplyr::summarise(AU = paste(AU, collapse = ", ")) 


# Use sapply() to apply strsplit() to each element in the 'strings' column
CRsco2_AU$split <- sapply(CRsco2_AU$AU, function(x) strsplit(x, split = "\\,")[[1]])

# Create a new column called 'filtered_strings' to store the filtered strings
CRsco2_AU$filtered_strings <- lapply(CRsco2_AU$split, function(x) {
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

CRsco2_AU$AUclean <- sapply(CRsco2_AU$filtered_strings, function(x) paste(x, collapse = ","))

CRsco2_AU <- CRsco2_AU[,-c(3,4)]

CRsco2_AU$AUclean <- ifelse(CRsco2_AU$AUclean == "", CRsco2_AU$AU, CRsco2_AU$AUclean)
CRsco2_AU <- CRsco2_AU[,-c(2)]
names(CRsco2_AU) <- c("cr_paper", "AU")



CRsco2 <- inner_join(CRsco2[,c(1,2,4)], CRsco2_AU, by= "cr_paper")

CRsco2 <- CRsco2[,-3]

#CRsco <- CRsco %>% # create a column paper with the consecutive 
#  mutate(paper = paper + 574) # starting in 574


# CRsco2$AU1 <- sub(pattern = ",", # this deletes the first "," in the array
#                  replacement = "", # and replace with ""
#                  x = CRsco2$AU,
#                  fixed = TRUE,
#                  perl = TRUE,
#                  useBytes = FALSE)


CRsco2$ref <- trimws(CRsco2$ref)
CRsco2$AU1 <- # creates a AU column
  trimws(gsub("[ ]{2,}", # extract the first string 2 spaces to separate the last name 
              "", # the names are place after the last name space
              (gsub("\\.", 
                    "", 
                    unlist(lapply(strsplit(CRsco2$AU, 
                                           ',', # split the strings by "," separator 
                                           fixed = TRUE), 
                                  '[', 1)))))) # extract the first string of the array

# CRsco$AU <- str_replace_all(CRsco$AU, 
#                             "VAN DIJK", 
#                             "VAN DIJK J") 
# 
# CRsco$AU <- sub("VAN DIJK\\s+J.*$", 
#                 "VAN DIJK J", 
#                 CRsco$AU)# standardize

# indexes c(1958, 1959) have misspell Jan van Dijk
# index_s <-   which(grepl("VAN DIJCK J", CRsco2$AU, ignore.case = TRUE))
# CRsco2$AU[c(3450, 3451)] <- str_replace_all(CRsco2$AU[c(3450, 3451)] ,"VAN DIJCK J", 
#                          "VAN DIJK J")

# CRsco2$AU <- sub("VAN DEURSEN\\s+A.*$", 
#                 "VAN DEURSEN A", 
#                 CRsco2$AU) # standardize the name of this author

CRsco2$ref <- gsub("[^[:alnum:][:space:]]", # look for all the punctuation 
                  " ", # and replace it with a white space
                  CRsco2$ref) # in the column ref from the CRsco2 df
CRsco2$ref <- paste0(CRsco2$ref, #  In the column ref from the CRsco2 df
                    " ") # adds a whitespace at the end of each string

CRsco2$ref <- str_replace_all(CRsco2$ref, " 20I", " 201")
CRsco2$ref <- str_replace_all(CRsco2$ref, " 201I", " 2011")
CRsco2$ref <- str_replace_all(CRsco2$ref, " 202I", " 2021")
CRsco2$ref <- str_replace_all(CRsco2$ref, " 200I", " 2001")

CRsco2$AU <- trimws(CRsco2$AU) # it trims leading white spaces in the field1 column 
CRsco2$PY <- # creates a PY column that contains the year
  str_extract(string = CRsco2$ref, # extract a string from the selected column  
              pattern = "\\s(19\\d{2}|20[0-2]\\d)\\s") #"\\s19\\d{2}\\s|\\s20\\d{2}\\s")
                
                # "\\((\\d{4})\\) | \\b\\d{4}\\b$ | 19\\d{2}|20\\d{2}") # this pastern search for the year
sum(is.na(CRsco2$PY))


# CRsco2$PY <- gsub("[()]", # it will look for parenthesis 
#                  "", # and replace it 
#                  CRsco2$PY) # in the column PY


CRsco2$AU <- trimws(CRsco2$AU)
CRsco2$PY <- trimws(CRsco2$PY)
CRsco2$SR <- # creates a column 
  paste(CRsco2$AU, ", ", 
        CRsco2$PY, 
        sep = "")


#### Merging the citing and cited references ####

CRsco <- left_join(CRsco2[,c(1,2,4,5,6)], 
                    CRsco1_AU[, c(3,5,6,7)],
                    by= "SR")





sum(is.na(CRsco$DI))
############################################################
############# Authors, titles and DOI edition ##############
############################################################
CRsco$SR <- str_replace_all(CRsco$SR, 
                            "VAN DEURSEN A, VAN DIJK J, 2013",
                            "VAN DEURSEN A, VAN DIJK J, 2014")

van_dijk_2014 <- which(grepl("VAN DEURSEN A, VAN DIJK J, 2014", 
                             CRsco$SR, ignore.case = TRUE))
SR_van <- "VAN DEURSEN A, VAN DIJK J, 2014"
new_van <- CRsco[4075, c("TI", "SO", "DI")]

new_van <- data.frame()
for (i in 1:41) {
  new_van <- rbind(new_van, CRsco[4075, c("TI", "SO", "DI")])
}

CRsco[van_dijk_2014, 
      c("TI", "SO", "DI")] <- 
  if (all(CRsco$SR[van_dijk_2014] == SR_van )) {
    new_van
} else {
  CRsco[van_dijk_2014, c("TI", "SO", "DI")]
}


van_dijk_2005 <- which(grepl("VAN DIJK J, 2005", CRsco$SR, ignore.case = TRUE))
SR_van05 <- "VAN DIJK J, 2005"
new_van05 <- CRsco[1060, c("TI", "SO", "DI")]

new_van05 <- data.frame()
for (i in 1:18) {
  new_van05 <- rbind(new_van05, CRsco[1060, c("TI", "SO", "DI")])
}

new_van05$SO <- "SAGE Publications, Inc."

CRsco[van_dijk_2005, 
      c("TI", "SO", "DI")] <- 
  if (all(CRsco$SR[van_dijk_2005] == SR_van05 )) {
    new_van05
  } else {
    CRsco[van_dijk_2005, c("TI", "SO", "DI")]
  }


van_dijk_2012 <- which(grepl(" THE DIGITAL DIVIDE TURNS TO INEQUALITY OF SKILLS AND USAGE", 
                       CRsco$ref, ignore.case = TRUE))


new_van_dijk_2012 <- CRsco[5501, c("TI", "SO", "DI")]

new_van_dijk_2012 <- data.frame()
for (i in 1:6) {
  new_van_dijk_2012 <- rbind(new_van_dijk_2012, CRsco[5501, c("TI", "SO", "DI")])
}

ref_matchesvan12 <- grepl(" THE DIGITAL DIVIDE TURNS TO INEQUALITY OF SKILLS AND USAGE", 
                        CRsco$ref[van_dijk_2012], ignore.case = TRUE)
if (all(ref_matchesvan12)) {
  CRsco[van_dijk_2012, c("TI", "SO", "DI")] <- new_van_dijk_2012
} else {
  CRsco[van_dijk_2012, c("TI", "SO", "DI")]
}


CRsco$ref <- str_replace_all(CRsco$ref, "   ", " ")
CRsco$ref <- str_replace_all(CRsco$ref, "  ", " ")
selwyn06 <- which(grepl("SELWYN N DIGITAL DIVISION OR DIGITAL DECISION A ", 
                        CRsco$ref, ignore.case = TRUE))


new_sel06 <- CRsco[2088, c("TI", "SO", "DI")]

new_sel06 <- data.frame()
for (i in 1:10) {
  new_sel06 <- rbind(new_sel06, CRsco[2088, c("TI", "SO", "DI")])
}

ref_matches <- grepl("SELWYN N DIGITAL DIVISION OR DIGITAL DECISION A ", 
                     CRsco$ref[selwyn06], ignore.case = TRUE)
if (all(ref_matches)) {
  CRsco[selwyn06, c("TI", "SO", "DI")] <- new_sel06
} else {
  CRsco[selwyn06, c("TI", "SO", "DI")]
}

selwyn04 <- which(grepl("SELWYN N RECONSIDERING POLITICAL AND POPULAR", 
                        CRsco$ref, ignore.case = TRUE))


new_sel04 <- CRsco[990, c("TI", "SO", "DI")]

new_sel04 <- data.frame()
for (i in 1:51) {
  new_sel04 <- rbind(new_sel04, CRsco[990, c("TI", "SO", "DI")])
}

ref_matches04 <- grepl("SELWYN N RECONSIDERING POLITICAL AND POPULAR", 
                     CRsco$ref[selwyn04], ignore.case = TRUE)
if (all(ref_matches04)) {
  CRsco[selwyn04, c("TI", "SO", "DI")] <- new_sel04
} else {
  CRsco[selwyn04, c("TI", "SO", "DI")]
}



selwyn04a <- which(grepl("SELWYN N THE INFORMATION AGED A QUALITATIVE", 
                         CRsco$ref, ignore.case = TRUE))


new_sel04a <- CRsco[1493, c("TI", "SO", "DI")]

new_sel04a <- data.frame()
for (i in 1:3) {
  new_sel04a <- rbind(new_sel04a, CRsco[1493, c("TI", "SO", "DI")])
}

ref_matches04a <- grepl("SELWYN N THE INFORMATION AGED A QUALITATIVE", 
                       CRsco$ref[selwyn04a], ignore.case = TRUE)
if (all(ref_matches04a)) {
  CRsco[selwyn04a, c("TI", "SO", "DI")] <- new_sel04a
} else {
  CRsco[selwyn04a, c("TI", "SO", "DI")]
}


hargi03 <- which(grepl("SECOND LEVEL DIGITAL DIVIDE DIFFERENCES ", 
                         CRsco$ref, ignore.case = TRUE))


new_hargi03 <- CRsco[180, c("TI", "SO", "DI")]

new_hargi03 <- data.frame()
for (i in 1:50) {
  new_hargi03 <- rbind(new_hargi03, CRsco[180, c("TI", "SO", "DI")])
}

ref_matchesh03 <- grepl("SECOND LEVEL DIGITAL DIVIDE DIFFERENCES ", 
                        CRsco$ref[hargi03], ignore.case = TRUE)
if (all(ref_matchesh03)) {
  CRsco[hargi03, c("TI", "SO", "DI")] <- new_hargi03
} else {
  CRsco[hargi03, c("TI", "SO", "DI")]
}



CRsco$DI <- tolower(CRsco$DI)
crsco_doi <- paste("DOI", CRsco$DI)

CRsco$LABEL_FULL <- paste(CRsco$SR, ifelse(crsco_doi == "DOI NA", "", crsco_doi), sep = ", ")
CRsco$LABEL_FULL <- trimws(CRsco$LABEL_FULL, whitespace = ", ")
colnames(CRsco)[which(colnames(CRsco) == "AU1")] <- "AU"

CRsco <- CRsco %>% distinct(paper, LABEL_FULL, .keep_all = T)
crsco_doi <- paste("DOI", CRsco$DI)
CRsco$LABEL <- paste(CRsco$AU, CRsco$PY,  ifelse(crsco_doi == "DOI NA", "", crsco_doi), sep = ", ")
CRsco$LABEL <- trimws(CRsco$LABEL, whitespace = ", ")


# 
# all_sco$LABEL <- all_sco$SR_FULL
# all_sco$LABEL[10]
# 
# all_sco$LABEL <- str_extract(all_sco$LABEL, "^[^,]*") # gets the first string of LABEL
# all_sco$LABEL[1]

#s <- "HUGGINS R, 2002, LOCAL ECON"
#extract_first_two <- function(s) {
#  split_s <- unlist(strsplit(s , ","))
#  first_two <- split_s[1:2]
#  paste(first_two, collapse = ",")
#}
# nchar <- rep(NA, nrow(all_sco))
# 
# for (i in 1:nrow(all_sco)) {
#   nchar[i] <- tail(unlist(gregexpr(" ", all_sco$LABEL[i])), 1) + 1
# }
# 
# all_sco$LABEL <- ifelse(nchar == 0, 
#                         all_sco$LABEL, 
#                         substr(all_sco$LABEL, 
#                                start = 1, 
#                                stop = nchar)) # make sure to run the nchar 
# 
# all_sco$DI <- tolower(all_sco$DI)
# sco_doi <-  ifelse(is.na(all_sco$DI), "", paste("DOI", all_sco$DI))
# 
# all_sco$LABEL <- paste(all_sco$AU, all_sco$PY, sco_doi, sep = ", ")
# 


CRS <- CRsco %>% group_by(paper) %>% 
  dplyr::summarise(CR = paste(LABEL, collapse = "; ")) %>% rename("Paper" = "paper")

all_sco <- all_sco[, -11]

all_sco <- left_join(all_sco, CRS, by= c("Paper"))

all_sco <- all_sco[, col_names_M]

write_csv(all_sco, file =  here("Data", "Processed", "all_sco.csv"))




all_sco = all_sco[order(all_sco$PY), ] # this makes a descendant order of the year

#all_sco <- all_sco %>% # this will add a column paper with the consecutive starting
#  mutate(Paper = row_number() + 574) # from 574
all_sco$Paper <- 1:nrow(all_sco)
all_sco_orig <- all_sco # creates the original df

#all_sco <- all_sco %>% # this will add a column nLALBEL with the consecutive starting
#  mutate(nLABEL = row_number() + 574)# from 574
all_sco$nLABEL <- 1:nrow(all_sco)

CRsco2 <- strsplit(all_sco$CR, 
                   split = ";") # this code splits the CR and create a list

CRsco2 <- lapply(seq_along(CRsco2), function(i) { # this function creates a data frame 
  l <- data.frame(ref = CRsco2[[i]], # for the number of CR 
                  paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

CRsco2 <- (do.call(rbind, CRsco2)) # this bin



Lsco <- inner_join(all_sco, # 132 matches were found 
                   CRsco[,c(2,1,9)],
                   by=c("LABEL")) # better to do an inner join 

Lsco <- Lsco[!is.na(Lsco$paper),] # no need to run if there is an inner join
#The expression all_sco$LABEL[L$paper] selects the values in the LABEL column of 
#the all_sco data frame for which the corresponding elements in the paper column #
# of the L data frame are TRUE.
Lsco$CITING <- all_sco$LABEL[Lsco$paper] # this brings paper No. of the label to citing column
Lsco$CITING[1]
Lsco$nCITING <- all_sco$nLABEL[Lsco$paper] # this create a column nCITING adding LC
Lsco$CIT_PY <- all_sco$PY[Lsco$paper] # this create a col with the PY of the citing doc

LCSsco <- Lsco %>% group_by(.data$nLABEL) %>% # create a LCS df
  summarize(LABEL = .data$LABEL[1],
            n = length(.data$nLABEL)) %>%
  as.data.frame()

all_sco$LCS <- 0
all_sco[LCSsco$nLABEL, "LCS"] <- LCSsco$n
n_distinct(all_sco$LABEL)
all_sco_orig$LCS <- all_sco$LCS

histData_sco <- all_sco[c("LABEL","TI","DE","ID","PY","LCS","TC")]
names(histData_sco) <- c("Paper","Title","Author_Keywords","KeywordsPlus","Year","LCS","GCS")


# because not all the papers published in the same year by the same author can not be
# identify the the LCR column has in some rows repeated references
# this suggest that the citing paper has cited one of this references
CITING_sco <- Lsco %>% group_by(.data$CITING) %>%
  summarize(
    LCR = paste(.data$LABEL, # 
                collapse = ";"),
    PY = .data$CIT_PY[1],
    Paper = .data$paper[1]
  ) %>%
  ungroup() %>%
  arrange(.data$PY) %>% as.data.frame()

all_sco_orig$LCR <- NA
all_sco_orig$LCR[CITING_sco$Paper] <- CITING_sco$LCR
all_sco_orig$LABEL <- all_sco$LABEL
all_sco <- all_sco_orig

