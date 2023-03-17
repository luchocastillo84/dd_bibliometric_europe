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

load(here("Script", "Environments", "LocalCitations.RData"))
############################ 1. Most Cited References ##########################

M <- read_csv(file = here("Data", 
                          "Processed", 
                          "M_EU.csv"))


Lcr <- strsplit(M$CR, 
                split = ";") # this code splits the CR and create a list

Lcr <- lapply(seq_along(Lcr), function(i) { # this function creates a data frame 
  l <- data.frame(ref = Lcr[[i]], # for the number of CR 
                  paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

Lcr <- (do.call(rbind, Lcr)) # this binds all the CR 
Lcr$ref <- trimws(Lcr$ref)


most_Lcr <- Lcr %>% group_by(ref) %>% summarize(TC= n()) %>% 
  dplyr::arrange(desc(TC)) %>% head(10)

df_Lcr <- separate(most_Lcr, col = ref, 
               into = c("AU", "PY", "DOI"), sep = ",\\s+|\\s+DOI\\s+")
df_Lcr$Rank <- seq(nrow(df_Lcr))

df_Lcr$AU[5] <- paste(df_Lcr$AU[5], "HACKER K", sep = ", ")
df_Lcr$AU[7] <- paste(df_Lcr$AU[7], "HARGITTAI E", "CELESTE C", "SHAFER S", sep = ", ")
df_Lcr$AU[8] <- paste(df_Lcr$AU[8], "VAN DIJK J", sep = ", ")
df_Lcr$AU[9] <- paste(df_Lcr$AU[9], "HARGITTAI E", sep = ", ")
df_Lcr$AU[10] <- paste(df_Lcr$AU[10], "HINNANT A", sep = ", ")


df_Lcr$TI <- c("Digital Divide Civic Engagement, Information Poverty, and the Internet Worldwide", 
                 "The Deepening Divide: Inequality in the Information Society", 
                 "Second-Level Digital Divide: Differences in People's Online Skills", 
                 "Digital divide research, achievements and shortcomings", 
                 "The Digital Divide as a Complex and Dynamic Phenomenon",
                 "Reconsidering Political and Popular Understandings of the Digital Divide",
                 "From Unequal Access to Differentiated Use: A Literature Review and Agenda for Research on Digital Inequality",
                 "The digital divide shifts to differences in usage",
                 "Digital Distinction: Status-Specific Types of Internet Usage",
                 "Digital Inequality: Differences in Young Adults' Use of the Internet")

df_Lcr <- df_Lcr %>% mutate(Article = paste0(AU, " (",PY,") ", "-",TI))
df_Lcr <- df_Lcr %>% select(Rank, Article, TC)
df_Lcr$Article <- str_to_title(df_Lcr$Article)


##### 1.2. 2000 to 2007 first period ####
M_1p <- M %>% filter(PY <= 2007) # sub-setting the first period

Lcr1 <- strsplit(M_1p$CR, 
                split = ";") # this code splits the CR and create a list

Lcr1 <- lapply(seq_along(Lcr1), function(i) { # this function creates a data frame 
  l <- data.frame(ref = Lcr1[[i]], # for the number of CR 
                  paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

Lcr1 <- (do.call(rbind, Lcr1)) # this binds all the CR 
Lcr1$ref <- trimws(Lcr1$ref)


most_Lcr1 <- Lcr1 %>% group_by(ref) %>% summarize(freq= n()) %>% 
  dplyr::arrange(desc(freq)) %>% head(5)

##### 1.3. 2008 to 2015: second period ####

M_2p <- M %>% filter(PY > 2007 & PY <= 2015) # sub-setting the second period

Lcr2 <- strsplit(M_2p$CR, 
                 split = ";") # this code splits the CR and create a list

Lcr2 <- lapply(seq_along(Lcr2), function(i) { # this function creates a data frame 
  l <- data.frame(ref = Lcr2[[i]], # for the number of CR 
                  paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

Lcr2 <- (do.call(rbind, Lcr2)) # this binds all the CR 
Lcr2$ref <- trimws(Lcr2$ref)


most_Lcr2 <- Lcr2 %>% group_by(ref) %>% summarize(freq= n()) %>% 
  dplyr::arrange(desc(freq)) %>% head(5)

##### 1.4. 2015 to 2022: third period ####

M_3p <- M %>% filter(PY > 2015 & PY <= 2022) # sub-setting the third period

Lcr3 <- strsplit(M_3p$CR, 
                 split = ";") # this code splits the CR and create a list

Lcr3 <- lapply(seq_along(Lcr3), function(i) { # this function creates a data frame 
  l <- data.frame(ref = Lcr3[[i]], # for the number of CR 
                  paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

Lcr3 <- (do.call(rbind, Lcr3)) # this binds all the CR 
Lcr3$ref <- trimws(Lcr3$ref)


most_Lcr3 <- Lcr3 %>% group_by(ref) %>% summarize(freq= n()) %>% 
  dplyr::arrange(desc(freq)) %>% head(5)





m <- M

m = m[order(m$PY), ] # this makes a descendant order of the year

#m <- m %>% # this will add a column paper with the consecutive starting
#  mutate(Paper = row_number() + 574) # from 574
m$Paper <- 1:nrow(m)
m_orig <- m # creates the original df

#m <- m %>% # this will add a column nLALBEL with the consecutive starting
#  mutate(nLABEL = row_number() + 574)# from 574
m$nLABEL <- 1:nrow(m)

crm <- strsplit(m$CR, 
                   split = ";") # this code splits the CR and create a list

crm <- lapply(seq_along(crm), function(i) { # this function creates a data frame 
  l <- data.frame(LABEL = crm[[i]], # for the number of CR 
                  paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

crm <- (do.call(rbind, crm)) # this bin
crm$LABEL <- trimws(crm$LABEL)


Lm <- inner_join(m, # 132 matches were found 
                   crm,
                   by=c("SRDI"="LABEL")) # better to do an inner join 

Lm <- Lm[!is.na(Lm$paper),] # no need to run if there is an inner join
#The expression m$LABEL[L$paper] selects the values in the LABEL column of 
#the m data frame for which the corresponding elements in the paper column #
# of the L data frame are TRUE.
Lm$CITING <- m$SRDI[Lm$paper] # this brings paper No. of the label to citing column
Lm$CITING[1]
Lm$nCITING <- m$nLABEL[Lm$paper] # this create a column nCITING adding LC
Lm$CIT_PY <- m$PY[Lm$paper] # this create a col with the PY of the citing doc

LCSm <- Lm %>% group_by(.data$nLABEL) %>% # create a LCS df
  summarize(SRDI = .data$SRDI[1],
            n = length(.data$nLABEL)) %>%
  as.data.frame()

m$LCS <- 0
m[LCSm$nLABEL, "LCS"] <- LCSm$n
n_distinct(m$SRDI)
m_orig$LCS <- m$LCS

histData_m <- m[c("SRDI","TI","DE","ID","PY","LCS","TC")]
names(histData_m) <- c("Paper","Title","Author_Keywords","KeywordsPlus","Year","LCS","GCS")


# because not all the papers published in the same year by the same author can not be
# identify the the LCR column has in some rows repeated references
# this suggest that the citing paper has cited one of this references
CITING_sco <- Lm %>% group_by(.data$CITING) %>%
  summarize(
    LCR = paste(.data$LABEL, # 
                collapse = ";"),
    PY = .data$CIT_PY[1],
    Paper = .data$paper[1]
  ) %>%
  ungroup() %>%
  arrange(.data$PY) %>% as.data.frame()

m_orig$LCR <- NA
m_orig$LCR[CITING_sco$Paper] <- CITING_sco$LCR
m_orig$LABEL <- m$LABEL
m <- m_orig