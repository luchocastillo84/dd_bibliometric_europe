#### Load the packages ####
library(tidyverse)
library(tidyr)
library(stringr)
library(bibliometrix)
library(here)
load(here("Script", "Environments", "CRwos_env.RData"))
#### Loading the data base ####

m_bdf <- convert2df(here("Data",  # processed data frame for bibliometrics 30 columns
                         "Processed", 
                         "B_EU.csv"),
                    dbsource = "scopus", 
                    format = "csv")




all_isi <- m_bdf %>% filter(PF %in% c("WOS"))

all_isi_AU <- all_isi %>% select(AU) # this selects the AU column from all_sco

wos_AU <- strsplit(all_isi$AU, 
                   split = ";") # this code splits the AU and create a list

wos_AU <- lapply(seq_along(wos_AU), function(i) { # this function creates a data frame 
  l <- data.frame(AU = wos_AU[[i]], # for the number of CR 
                  paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

wos_AU <- (do.call(rbind, wos_AU)) # this binds all the CR of each paper into a df format

nchar <- rep(NA, nrow(wos_AU))

for (i in 1:nrow(wos_AU)) {
  nchar[i] <- tail(unlist(gregexpr(" ", wos_AU$AU[i])), 1) + 1
}

wos_AU$AU <- ifelse(nchar == 0,
                    wos_AU$AU,
                    substr(wos_AU$AU,
                           start = 1,
                           stop = nchar)) # make sure to run the nchar

wos_AU <- wos_AU %>% group_by(paper) %>% 
  dplyr::summarise(AU = paste(AU, collapse = "; "))

all_isi$AU <- wos_AU$AU



#### Web of Science ####
all_isi = all_isi[order(all_isi$PY), ] # this makes a descendant order of the year
all_isi$Paper <- 1:nrow(all_isi) # this create a new column paper with row number
all_isi_orig <- all_isi
all_isi$nLABEL <- 1:nrow(all_isi) # this create a new column nLABLE with row number
all_isi$CR <- str_replace_all(all_isi$CR, ";2-", "") # it deletes that pattern in CRwos


CRwos <- strsplit(all_isi$CR, 
               split = ";") # this code splits the CRwos and create a list

CRwos <- lapply(seq_along(CRwos), function(i) { # this function creates a data frame 
  l <- data.frame(ref = CRwos[[i]], # for the number of CRwos 
                  paper = i, # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

CRwos <- (do.call(rbind, CRwos)) # this binds all the CRwos of each paper into a df format
CRwos$ref <- trimws(CRwos$ref) # trim leading white spaces in the CRwos column ref
CRwos$ref <- gsub("^\\*", "", CRwos$ref) # delete leading asterisks in the CRwos column ref
CRwos$ref <- gsub("^\\?", "", CRwos$ref) # delete leading questions marks in the CRwos column ref

cr_ncols <- max(stringr::str_count(CRwos$ref, # it returns the max number of strings
                                   ",")) # that are separated by ","

cr_colmn <- paste("field", 1:cr_ncols, sep ="") # create and paste the name of the colums

CRwossplit <- # creates a df that splits the columns 
  tidyr::separate(
    data = CRwos,
    col = ref,
    sep = ",", # that are separated by ","
    into = cr_colmn, # using the column's names 
    remove = T
  )


CRwossplit$field1 <- trimws(CRwossplit$field1)
CRwossplit$field1 <- gsub("\\*", "",CRwossplit$field1) # deletes all the * in column one

CRwossplit$field1 <- gsub("\\.[A-Z]", "", # delete all the dots 
                       gsub("\\. [A-Z]", "", # delete all the dots
                            gsub("\\.", "",CRwossplit$field1))) # delete all the dots

CRwossplit$field1 <- gsub("VANDIJK*", "VAN DIJK J", 
                       gsub("VANSDIJK J", "VAN DIJK J", 
                            gsub("VAN-DIJCK JAN", "VAN DIJK J",
                                 CRwossplit$field1))) # standardize the name of this author

CRwossplit$field1 <- str_replace_all(CRwossplit$field1, 
                                  "VAN DIJK", 
                                  "VAN DIJK J") # standardize the name of this author
CRwossplit$field1 <- sub("VAN DIJK\\s+J.*$", 
                      "VAN DIJK J", 
                      CRwossplit$field1) # standardize the name of this author
CRwossplit$field1 <- str_replace_all(CRwossplit$field1, 
                                  "BUREAU", 
                                  "") # delete this word within author's names

CRwossplit$field1 <- sub("VAN DEURSEN\\s+A.*$", 
                      "VAN DEURSEN A", 
                      CRwossplit$field1) # standardize the name of this author

CRwossplit$field1 <- gsub("VANDEURSEN A", 
                       "VAN DEURSEN A",
                       CRwossplit$field1) # standardize the name of this author

CRwossplit$field1 <- sub("SILVERSTONE\\s+R.*$", 
                      "SILVERSTONE R", 
                      CRwossplit$field1) # standardize the name of this author

CRwossplit$field1 <- sub("SELWYN\\s+N.*$", 
                      "SELWYN N", 
                      CRwossplit$field1) # standardize the name of this author

CRwossplit <- CRwossplit %>% filter(field1 != "ANONYMOUS") # delete this word in author's column 

CRwossplit$field1 <- trimws(CRwossplit$field1) # it trims leading white spaces in the field1 column 

pattern <- "\\d{4}"  # Regular expression to match four consecutive digits
matches <- which(grepl(pattern, CRwossplit$field1)) # indices of four consecutive digits

CRwossplit <- CRwossplit[-matches,] # exclude all the AU that contains numbers

nchar <- rep(NA, nrow(CRwossplit))

for (i in 1:nrow(CRwossplit)) {
  nchar[i] <- tail(unlist(gregexpr(" ", CRwossplit$field1[i])), 1) + 1
}

CRwossplit$field1 <- ifelse(nchar == 0, 
                         CRwossplit$field1, 
                         substr(CRwossplit$field1, 
                                start = 1, 
                                stop = nchar)) # make sure to run the nchar 


CRwos <- data.frame(ref= apply(CRwossplit[,c(1:8)], # this pastes the CRwossplit df 
                            1, function(x) paste(x, 
                                                 collapse = ","))) %>% 
  cbind(paper= CRwossplit$paper) # and bind the last column with the paper number

CRwos$ref <- str_replace_all(CRwos$ref, ",NA*", "") # it removes all the NAs after pasting CRwossplit
CRwos$ref <- trimws(CRwos$ref) # it trims leading white spaces in the ref column 




  

# CRwos$DI[is.na(CRwos$DI) | CRwos$DI=="NA"] <- "" # converts NA into empty values ""

CRwos$AU <- # creates a AU column
  trimws(gsub("[ ]{2,}", # extract the first string 2 spaces to separate the last name 
              "", # the names are place after the last name space
              (gsub("\\.", 
                    " ", 
                    unlist(lapply(strsplit(CRwos$ref, 
                                           ',', # split the strings by "," separator 
                                           fixed = TRUE), 
                                  '[', 1)))))) # extract the first string of the array

nchar <- rep(NA, nrow(CRwos)) # create a vector with CRwos row number

for (i in 1:nrow(CRwos)) { # Loop through the rows of the CRwos data frame and 
  nchar[i] <- nchar(CRwos$AU[i])
}  

CRwos$nchar <- nchar # add column nchar to the df
CRwos <- CRwos %>% filter(nchar > 3 & nchar < 30) # exclude characters below 3 or above 30
CRwos <- CRwos[,-4] 


CRwos$PY <- # adds a PY column 
  trimws(unlist(lapply(strsplit(CRwos$ref, 
                                ',', # separated by comma
                                fixed = TRUE), 
                       '[', 2))) # extract the second string in the array


nchar <- rep(NA, nrow(CRwos)) # create a vector with CRwos row number

for (i in 1:nrow(CRwos)) { # Loop through the rows of the CRwos data frame and 
  nchar[i] <- nchar(CRwos$PY[i])
}  

CRwos$nchar <- nchar # add column nchar to the df
CRwos <- CRwos %>% filter(nchar > 3 & nchar < 5) # exclude characters below 3 or above 4
CRwos <- CRwos[,-5] 

CRwos$ref <- str_replace_all(CRwos$ref, "DOI DOI", "DOI")

CRwos$DI <-
  trimws(unlist(lapply(strsplit(CRwos$ref, # this splits the array using
                                'DOI', # as separator the DOI word
                                fixed = TRUE),
                       '[', 2))) # returs the second string after DOI
CRwos$DI <- trimws(CRwos$DI, whitespace = ",")

nchar <- rep(NA, nrow(CRwos)) # create a vector with CRwos row number

for (i in 1:nrow(CRwos)) { # Loop through the rows of the CRwos data frame and 
  nchar[i] <- tail(unlist(gregexpr(", ", # compute the number of characters in the AU column of each row
                                   CRwos$DI[i])), 1)  -1
}

CRwos$nchar <- nchar

CRwos$DI <- ifelse(nchar < 0 | is_na(nchar),
                       CRwos$DI,
                       substr(CRwos$DI,
                              start = 1,
                              stop = nchar)) # make sure to run the nchar
CRwos <- CRwos[,-6]

############################################################
############# Authors, titles and DOI edition ##############
############################################################

CRwos$ref <- str_replace_all(CRwos$ref, "THE DEEPENING DIVIDE", "DEEPENING DIVIDE")

van_dijk_2004 <- which(grepl("VAN DIJK J, 2005, DEEPENING DIV", 
                             CRwos$ref, ignore.case = TRUE))


new_van_dijk_doi <- "10.4135/9781452229812"

new_van_dijk_2004 <- data.frame()
for (i in 1:140) {
  new_van_dijk_2004 <- rbind(new_van_dijk_2004, new_van_dijk_doi)
  names(new_van_dijk_2004)[1] <- "ref"
}

ref_van2004 <- grepl("VAN DIJK J, 2005, DEEPENING DIVIDE", 
                          CRwos$ref[van_dijk_2004], ignore.case = TRUE)
if (all(ref_van2004)) {
  CRwos[van_dijk_2004, c("DI")] <- new_van_dijk_2004
} else {
  CRwos[van_dijk_2004, c("DI")]
}

CRwos$DI <- tolower(CRwos$DI)
crwos_doi <- paste("DOI", CRwos$DI)
CRwos$LABEL <- paste(CRwos$AU, 
                     CRwos$PY,  ifelse(crwos_doi == "DOI NA", "",
                                       crwos_doi), sep = ", ")
CRwos$LABEL <- trimws(CRwos$LABEL, whitespace = ", ")

CRwos$SR <- 
  paste(CRwos$AU, 
        CRwos$PY,
        sep = ", ")

CRW <- CRwos %>% group_by(paper) %>% # pasting all the CR grouped by paper 
  dplyr::summarise(CR = paste(LABEL, collapse = "; ")) %>% rename("Paper" = "paper")

all_isi <- all_isi[, -11]

all_isi <- left_join(all_isi, CRW, by= c("Paper"))

all_isi <- all_isi[, col_names_M]

write_csv(all_isi, file =  here("Data", "Processed", "all_isi.csv"))








#CRwos$SO <- # adds a SO column 
#  trimws(unlist(lapply(strsplit(CRwos$ref, 
#                                ',', # separated by comma
#                                fixed = TRUE), 
#                       '[', 3))) # extract the third string in the array



all_isi <- all_isi %>% replace_with_na_all(condition = ~. == "") # converting "" into NA 
all_isi <- all_isi %>% replace_with_na_all(condition = ~. == "NA") # converting "" into NA

# all_isi$LABEL <- paste(all_isi$SR_FULL)
# all_isi$LABEL[1]
# # all_isi$LABEL <- gsub(regex(", [^,]+$"), "", all_isi$LABEL)
# all_isi$LABEL <- str_extract(all_isi$LABEL, "^[^,]*")
# all_isi$LABEL[1]

nchar <- rep(NA, nrow(all_isi))

for (i in 1:nrow(all_isi)) {
  nchar[i] <- head(unlist(gregexpr(",", all_isi$AU[i])), 1) 
}

all_isi$LABEL <- ifelse(nchar <= 0, 
                        all_isi$AU, 
                         substr(all_isi$AU, 
                                start = 1, 
                                stop = nchar)) # make sure to run the nchar 
all_isi$LABEL <- trimws(all_isi$LABEL, whitespace = ",")


all_isi$DI <- tolower(all_isi$DI)
all_isi_doi <- paste("DOI", all_isi$DI)

all_isi$LABEL <- paste(all_isi$LABEL, 
                       all_isi$PY, sep = ", ")
all_isi$LABEL <- paste(all_isi$LABEL, ifelse(all_isi_doi == "DOI NA", "", all_isi_doi), sep = ", ")
all_isi$LABEL <- trimws(all_isi$LABEL, whitespace = ", ")                       


CRW <- CRwos %>% group_by(paper) %>% # pasting all the CR grouped by paper 
  dplyr::summarise(CR = paste(LABEL, collapse = "; "))

CRW$CT <- all_isi$LABEL[CRW$paper]



Lwos <- inner_join(all_isi, #1480 matches were found
                   CRwos[,c(2,1,7)],
                   by=c("LABEL")) # better to do an inner join 
# L_w <- left_join( all_isi, CRwos[,c(2, 1,6)], by= c("LABEL"))
Lwos$unique <- 0
Lwos$unique <- ifelse(Lwos$Paper == Lwos$nLABEL & Lwos$paper == Lwos$nCITING & 
                        Lwos$LABEL == Lwos$CITING, 
                      "NO", "SI")

unique <- Lwos %>% dplyr::select(Paper, LABEL, CITING, nCITING) %>%  
  dplyr::group_by( CITING) %>%  
  summarize(LABELu=  unique(LABEL))

vis_miss(Lwos)
Lwos <- Lwos %>% replace_with_na_all(condition = ~. == "NA")

Lwos <- Lwos[!is.na(Lwos$paper),] # no need to run if there is an inner join
#The expression all_isi$LABEL[L$paper] selects the values in the LABEL column of 
#the all_isi data frame for which the corresponding elements in the paper column #
# of the L data frame are TRUE.
Lwos$CITING <- all_isi$LABEL[Lwos$paper] # this brings paper No. of the label to citing column
Lwos$nCITING <- all_isi$nLABEL[Lwos$paper] # this create a column nCITING adding LC
Lwos$CIT_PY <- all_isi$PY[Lwos$paper] # this create a col with the PY of the citing doc
n_distinct(Lwos$CITING)

LCSwos <- Lwos %>% group_by(.data$nLABEL) %>% # create a LCS df
  summarize(LABEL = .data$LABEL[1],
            n = length(.data$nLABEL)) %>%
  as.data.frame()

all_isi$LCS <- 0
all_isi[LCSwos$nLABEL, "LCS"] <- LCSwos$n
n_distinct(all_isi$LABEL)
all_isi_orig$LCS <- all_isi$LCS

histData_wos <- all_isi[c("LABEL","TI","DE","ID","PY","LCS","TC")]
names(histData_wos) <- c("Paper","Title","Author_Keywords","KeywordsPlus","Year","LCS","GCS")


# because not all the papers published in the same year by the same author can not be
# identify the the LCRwos column has in some rows repeated references
# this suggest that the citing paper has cited one of this references
CITING_wos <- Lwos %>% group_by(.data$CITING) %>%
  summarize(
    LCR = paste(.data$LABEL, # 
                collapse = ";"),
    PY = .data$CIT_PY[1],
    Paper = .data$paper[1]
  ) %>%
  ungroup() %>%
  arrange(.data$PY) %>% as.data.frame()

all_isi_orig$LCR <- NA
all_isi_orig$LCR[CITING_wos$Paper] <- CITING_wos$LCR
all_isi_orig$LABEL <- all_isi$LABEL
all_isi <- all_isi_orig

st<-i<-0
while(st==0){
  ind <- which(duplicated(all_isi$LABEL))
  if (length(ind)>0){
    i <- i+1
    all_isi$LABEL[ind]=paste0(all_isi$LABEL[ind],
                              "-",letters[i],sep="")}else{st <- 1}}

row.names(all_isi) <- all_isi$LABEL 
WLCR <- cocMatrix(all_isi, "LCR", sep = ";")

missingLABEL <- setdiff((all_isi$LABEL), colnames(WLCR))
colLab <- c(colnames(WLCR), missingLABEL)
WLCR <- cbind(WLCR, matrix(0, nrow(WLCR), length(missingLABEL)))
WLCR <- as.data.frame(as.matrix(WLCR), stringsAsFactors = FALSE)
colnames(WLCR) <- colLab
LABEL <- (row.names(WLCR))
WLCR <- as.matrix(WLCR[LABEL])

results <-
  list(
    NetMatrix = WLCR,
    histData = histData,
    all_isi = all_isi_orig,
    LCS = all_isi$LCS
  )
return(results)
results






