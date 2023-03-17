#### 0. Loading Packages ####
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
load(here("Script", "Environments", "Normalize_env.RData"))

################################################################################
####################  BINDING THE CLEAN CR FROM WOS, SCO AND DIM ###############
################################################################################

all_isi <- read_csv(here("Data", "Processed", "all_isi.csv"), 
                    col_names = T, show_col_types = F)
all_sco <- read_csv(here("Data", "Processed", "all_sco.csv"), 
                    col_names = T, show_col_types = F)
all_dim <- read_csv(here("Data", "Processed", "all_dim.csv"), 
                    col_names = T, show_col_types = F)


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



M$paper <- 1:nrow(M)


Gcr <- strsplit(M$CR, 
                split = ";") # this code splits the CR and create a list

Gcr <- lapply(seq_along(Gcr), function(i) { # this function creates a data frame 
  l <- data.frame(ref = Gcr[[i]], # for the number of CR 
                  paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

Gcr <- (do.call(rbind, Gcr)) # this binds all the CR 
Gcr$ref <- trimws(Gcr$ref)

norris_2001 <- which(grepl("norris p, 2001", 
                           Gcr$ref, ignore.case = TRUE))
norris_2001_doi <- which(grepl("NORRIS P, 2001, DOI 10.1017/cbo9781139164887", 
                           Gcr$ref, ignore.case = TRUE))

norris_01_doi <- Gcr[18878, c("ref")]

norris_01_doi <- data.frame()
for (i in 1:204) {
  norris_01_doi <- rbind(norris_01_doi, Gcr[18878, c("ref")])
  names(norris_01_doi)[1] <- "ref"
}

ref_norris01 <- grepl("norris p, 2001", 
                      Gcr$ref[norris_2001], ignore.case = TRUE)
if (all(ref_norris01)) {
  Gcr[norris_2001, c("ref")] <- norris_01_doi
} else {
  Gcr[norris_2001, c("ref")]
}


dimaggio_2001a <- which(grepl("DIMAGGIO P, 2001, DOI 10.1086/ahr/69.3.805|10.1002/bem.20484", 
                             Gcr$ref, ignore.case = TRUE))


dimaggio_01_doi <- which(grepl("DIMAGGIO P, 2001, DOI 10.1146/annurev.soc.27.1.307", 
                               Gcr$ref, ignore.case = TRUE))

dimaggio_01_doi <- data.frame()
for (i in 1:23) {
  dimaggio_01_doi <- rbind(dimaggio_01_doi, Gcr[906, c("ref")])
  names(dimaggio_01_doi)[1] <- "ref"
}

ref_dimaggio01 <- grepl("DIMAGGIO P, 2001, DOI 10.1086/ahr/69.3.805|10.1002/bem.20484", 
                        Gcr$ref[dimaggio_2001a], ignore.case = TRUE)
if (all(ref_dimaggio01)) {
  Gcr[dimaggio_2001a, c("ref")] <- dimaggio_01_doi
} else {
  Gcr[dimaggio_2001a, c("ref")]
}







hargi_2002a <- which(grepl("HARGITTAI E, 2002, DOI 10.5210/fm.v7i4.942", 
                             Gcr$ref, ignore.case = TRUE))

hargi_2002 <- which(Gcr$ref == "HARGITTAI E, 2002")


hargi_02_doi <- Gcr[1121, c("ref")]

hargi_02_doi <- data.frame()
for (i in 1:45) {
  hargi_02_doi <- rbind(hargi_02_doi, Gcr[1121, c("ref")])
  names(hargi_02_doi)[1] <- "ref"
}

ref_hargi02 <- Gcr$ref[hargi_2002] == "HARGITTAI E, 2002"
if (all(ref_hargi02)) {
  Gcr[hargi_2002, c("ref")] <- hargi_02_doi
} else {
  Gcr[hargi_2002, c("ref")]
}



vdeur_14a <- which(grepl("VAN DEURSEN A, 2014, DOI 10.1057/9781137437037", 
                           Gcr$ref, ignore.case = TRUE))

vdeur_14 <- which(Gcr$ref == "VAN DEURSEN A, 2014")

vdeur_14_doi <- Gcr[21215, c("ref")]

vdeur_14_doi <- data.frame()
for (i in 1:28) {
  vdeur_14_doi <- rbind(vdeur_14_doi, Gcr[21215, c("ref")])
  names(vdeur_14_doi)[1] <- "ref"
}

ref_vdeur_14 <- Gcr$ref[vdeur_14] == "VAN DEURSEN A, 2014"
if (all(ref_vdeur_14)) {
  Gcr[vdeur_14, c("ref")] <- vdeur_14_doi
} else {
  Gce[vdeur_14, c("ref")]
}






rogers_95 <- which(Gcr$ref == "ROGERS E, 1995")


rogers_95a <- which(Gcr$ref == "ROGERS E, 1995, DOI 10.1007/978-3-642-79868-9_2")


rogers_95_doi <- Gcr[11457, c("ref")]

rogers_95_doi <- data.frame()
for (i in 1:37) {
  rogers_95_doi <- rbind(rogers_95_doi, Gcr[11457, c("ref")])
  names(rogers_95_doi)[1] <- "ref"
}

ref_rogers_95 <- Gcr$ref[rogers_95] == "ROGERS E, 1995"
if (all(ref_rogers_95)) {
  Gcr[rogers_95, c("ref")] <- rogers_95_doi
} else {
  Gce[rogers_95, c("ref")]
}



van_di_03 <- which(Gcr$ref == "VAN DIJK J, 2003, DOI 10.1080/01972240309487")

van_di_2003 <- which(grepl("VAN DIJK J, 2003", 
                               Gcr$ref, ignore.case = TRUE))

van_di_03_doi <- data.frame()
for (i in 1:114) {
  van_di_03_doi <- rbind(van_di_03_doi, Gcr[39434, c("ref")])
  names(van_di_03_doi)[1] <- "ref"
}

ref_van_di_03 <- grepl("VAN DIJK J, 2003", 
                        Gcr$ref[van_di_2003], ignore.case = TRUE)
if (all(ref_van_di_03)) {
  Gcr[van_di_2003, c("ref")] <- van_di_03_doi
} else {
  Gce[van_di_2003, c("ref")]
}

CRM <- Gcr %>% group_by(paper) %>% 
  dplyr::summarise(CR = paste(ref, collapse = "; ")) 

M <- M[, -11]

M <- left_join(M, CRM, by= c("paper"))

M <- M[, col_names_M]

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






