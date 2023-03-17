#### Load the packages ####
library(tidyverse)
library(tidyr)
library(stringr)
library(bibliometrix)
library(here)
load(here("Script", "Environments", "CRdim_env.RData"))

#### Loading the data base ####

m_bdf <- convert2df(here("Data",  # processed data frame for bibliometrics 30 columns
                         "Processed", 
                         "B_EU.csv"),
                    dbsource = "scopus", 
                    format = "csv")




#### Dimensions ####

all_dim <- m_bdf %>% filter(PF %in% c("DIM"))

all_dim = all_dim[order(all_dim$PY), ] # this makes a descendant order of the year
#all_dim <- all_dim %>%
#  mutate(paper = row_number() + 935)
all_dim$Paper <- 1:nrow(all_dim)
all_dim_orig <- all_dim
all_dim$nLABEL <- 1:nrow(all_dim)
#all_dim <- all_dim %>%
#  mutate(nLABEL = row_number() + 935)

dim_AU <- strsplit(all_dim$AU, 
                   split = ";") # this code splits the AU and create a list

dim_AU <- lapply(seq_along(dim_AU), function(i) { # this function creates a data frame 
  l <- data.frame(AU = dim_AU[[i]], # for the number of CR 
                  paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

dim_AU <- (do.call(rbind, dim_AU)) # this binds all the CR of each paper into a df format

nchar <- rep(NA, nrow(dim_AU))

for (i in 1:nrow(dim_AU)) {
  nchar[i] <- tail(unlist(gregexpr(" ", dim_AU$AU[i])), 1) + 1
}

dim_AU$AU <- ifelse(nchar == 0,
                    dim_AU$AU,
                    substr(dim_AU$AU,
                           start = 1,
                           stop = nchar)) # make sure to run the nchar

dim_AU <- dim_AU %>% group_by(paper) %>% 
  dplyr::summarise(AU = paste(AU, collapse = "; "))

all_dim$AU <- dim_AU$AU


#### Cited references DIM ####

CRdim <- strsplit(all_dim$CR, 
                  split = ";") # this code splits the CR and create a list

CRdim <- lapply(seq_along(CRdim), function(i) { # this function creates a data frame 
  l <- data.frame(ref = CRdim[[i]], # for the number of CR 
                  paper = i, # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

CRdim <- (do.call(rbind, CRdim)) # this binds all the CR of each paper into a df format

#CRdim <- CRdim %>%
#  mutate(paper = paper + 935)

short_rows_d <- which(nchar(CRdim$ref) < 20)
CRdim <- CRdim[-short_rows_d,]

cr_ncold <- max(stringr::str_count(CRdim$ref, ",")) 

cr_cold <- paste("field", 1:cr_ncold, sep ="") # create and paste the name of the colums


CRsplit_dim <- CRdim %>% 
  tidyr::separate(
    col = ref,
    sep = ",",
    into = cr_cold,
    remove = T
  )

CRsplit_dim$field1 <- gsub("\\*", "",CRsplit_dim$field1) # deletes all the * in column one
CRsplit_dim$field1 <- gsub("\\. ", "",CRsplit_dim$field1)
CRsplit_dim$field1 <- trimws(CRsplit_dim$field1)
short_rows_d <- which(nchar(CRsplit_dim$field1) <=3)
CRsplit_dim <- CRsplit_dim[-short_rows_d,]

nchar <- rep(NA, nrow(CRsplit_dim))

for (i in 1:nrow(CRsplit_dim)) {
  nchar[i] <- tail(unlist(gregexpr(" ", CRsplit_dim$field1[i])), 1) + 1
}


CRsplit_dim$field1 <- ifelse(nchar == 0 , CRsplit_dim$field1,
                             substr(CRsplit_dim$field1, 
                                    start = 1, stop = nchar))

index <-   which(grepl("!2000", CRsplit_dim$field2, ignore.case = TRUE))

CRsplit_dim$field2[index] <- sub("!2000", 
                                 "2000", 
                                 CRsplit_dim$field2[index])
CRsplit_dim[, c(1:15)] <- lapply(CRsplit_dim[, c(1:15)], trimws )

ind_par <- rep(NA, nrow(CRsplit_dim)) # Initializing an empty index vector
for (i in 6:12) {  # Loop through columns 6 and 7
  ind_par_temp <- grep("^10", CRsplit_dim[,i]) # Finding the index of the values that match the pattern
  ind_par[ind_par_temp] <- ind_par_temp  # Assigning the values to index vector
  CRsplit_dim$DI[ind_par_temp] <- CRsplit_dim[ind_par_temp,i] 
}



CRsplit_dim <- CRsplit_dim[, c(1,2,17,16)]

CRsplit_dim$DI <- tolower(CRsplit_dim$DI)

crdim_doi <- paste("DOI", CRsplit_dim$DI)
CRsplit_dim$ref <- paste(CRsplit_dim$field1, 
                        CRsplit_dim$field2,  ifelse(crdim_doi == "DOI NA", "",
                                       crdim_doi), sep = ", ")
CRsplit_dim$ref <- trimws(CRsplit_dim$ref, whitespace = ", ")


# CRdim <- data.frame(ref= apply(CRsplit_dim[,c(1:3)], 
#                                1, function(x) paste(x, 
#                                                     collapse = ", "))) %>% 
#   cbind(paper= CRsplit_dim$paper)

CRdim <- CRsplit_dim[,c(5,4)]

CRdim$ref <- str_replace_all(CRdim$ref, ", NA*", "") # it removes all the NAs after pasting CRsplit
CRdim$ref <- trimws(CRdim$ref) # it trims leading white spaces in the ref column 
CRdim$paper <- as.numeric(CRdim$paper)

CRD <- CRdim %>% group_by(paper) %>% 
  dplyr::summarise(CR = paste(ref, collapse = "; ")) %>% rename("Paper" = "paper") 

all_dim <- all_dim[, -11]

all_dim <- left_join(all_dim, CRD, by= c("Paper"))
# col_names_M <- colnames(m_bdf[, 1:29])
all_dim <- all_dim[, col_names_M]

write_csv(all_dim, file =  here("Data", "Processed", "all_dim.csv"))


#The expression all_dim$LABEL[L$paper] selects the values in the LABEL column of 
#the all_dim data frame for which the corresponding elements in the paper column #
# of the L data frame are TRUE.
Ldim$CITING <- all_dim$LABEL[Ldim$paper] # this brings paper No. of the label to citing column

