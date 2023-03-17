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
library(ggrepel)
library(ggtext)
library(gridExtra)

load(here("Script", "Environments", "Tables&Graphs.RData"))


### 1. Descriptive Analysis ######

MM <- read_csv(here("Data", # reading the bibliographic data
                    "Processed", 
                    "M_EU.csv"))
MM[, c(5, 23)] <- lapply(MM[, c(5,23)], tolower) # to lower DT and DI


##### 1.1. Total ####
Details <- c("Timespan", "Journals", # creating details vector
             "Annual growth rate", "Average citations per doc", 
             "Total published documents", "Articles","Book chapters",
             "Porceeding papers","Conference papers") # 11 items

tot_period <-paste(min(MM$PY, # the studied period in the bibliometric
                       na.rm = T) , 
                   "-", max(MM$PY, 
                            na.rm = T))

Y=table(MM$PY) # number of published docs by year
ny=dim(Y)[1] # returns how many years 
CAGR<-as.numeric(
  round(((Y[ny]/Y[1])^(1/(ny-1))-1)*100,2)) # Compound annual growth rate
sources <- n_distinct(MM$SO) # counts the number of journals in MM
n_docs <- nrow(MM) # counts the number of published documents in MM
avg_cite <- round(sum(MM$TC)/nrow(MM), 2) # returns the average citation 


arti <- as_vector(  # counting and merging articles with reviewed articles
        nrow(MM[MM$DT=='article', ]) + 
        nrow(MM[MM$DT=='review', ])) 

bchap <- as_vector(
         nrow(MM[MM$DT=='book chapter',]) + # counting and merging book chapters 
         nrow(MM[MM$DT=='article; book chapter',])) # with arti & and book chapters

procee <- as_vector(
          nrow(MM[MM$DT=='proceedings paper',]) + 
          nrow(MM[MM$DT=='article; proceedings paper', 
                  ])) # counting and merging proceedings 

confe <- as_vector(nrow(MM[MM$DT=='conference paper', 
                           ])) # counting conference papers


Total <- c(tot_period, sources, CAGR, # creating a total vector
           avg_cite, n_docs, arti, bchap, 
           procee, confe)

##### 1.2. 2000 to 2007 first period ####

M_1p <- MM %>% filter(PY <= 2007) # sub-setting the first period

tot_period1 <-paste(min(M_1p$PY, # this returns the first period 
                        na.rm = T) , # studied in the bibliometric
                    "-", max(M_1p$PY, 
                             na.rm = T))
Y1=table(M_1p$PY) # number of published docs by year
ny1=dim(Y1)[1] # returns how many years 
CAGR1<-as.numeric(
       round(((Y1[ny1]/Y1[1])^(1/(ny1-1))-1)*100,
             2)) # Compound annual growth rate

sources1 <- n_distinct(M_1p$SO) # counts the number of journals
n_docs1 <- nrow(M_1p) # counts the number of published documents
avg_cite1 <- round(sum(M_1p$TC)/nrow(M_1p), 2)  # returns the average citation 

arti1 <- as_vector(
         nrow(M_1p[M_1p$DT=='article', ]) + # counting and merging 
         nrow(M_1p[M_1p$DT=='review', ])) # articles with reviewed articles

bchap1 <- as_vector(
          nrow(M_1p[M_1p$DT=='book chapter',]) + 
          nrow(M_1p[M_1p$DT=='article; book chapter',
                    ])) # counting and merging book chapters 

procee1 <- as_vector(
           nrow(M_1p[M_1p$DT=='proceedings paper',]) + 
           nrow(M_1p[M_1p$DT=='article; proceedings paper', 
                     ])) # counting and merging proceedings 

confe1 <- as_vector(
          nrow(M_1p[M_1p$DT=='conference paper', 
                    ])) # counting conference papers

"Period 1" <- c(tot_period1, sources1, # creating a vector with the first period
                CAGR1, avg_cite1, n_docs1, arti1, 
                bchap1, procee1, confe1 )

##### 1.3. 2008 to 2015: second period ####

M_2p <- MM %>% filter(PY > 2007 & PY <= 2015) # sub-setting the second period

tot_period2 <-paste(min(M_2p$PY, # this returns the second period 
                        na.rm = T) , # studied in the bibliometric
                    "-", max(M_2p$PY,
                             na.rm = T))
Y2=table(M_2p$PY) # number of published docs by year
ny2=dim(Y2)[1] # returns how many years 
CAGR2<-as.numeric(
       round(((Y2[ny2]/Y2[1])^(1/(ny1-1))-1)*100,
             2)) # Compound annual growth rate

sources2 <- n_distinct(M_2p$SO) # counts the number of journals
n_docs2 <- nrow(M_2p) # counts the number of published documents
avg_cite2 <- round(sum(M_2p$TC)/nrow(M_2p), 2) # returns the average citation 

arti2 <- as_vector(
         nrow(M_2p[M_2p$DT=='article', ]) + # counting and merging 
         nrow(M_2p[M_2p$DT=='review', ])) # articles with reviewed articles

bchap2 <- as_vector(
          nrow(M_2p[M_2p$DT=='book chapter',]) + 
          nrow(M_2p[M_2p$DT=='article; book chapter',
                    ])) # counting and merging book chapters 

procee2 <- as_vector(
           nrow(M_2p[M_2p$DT=='proceedings paper',]) + 
           nrow(M_2p[M_2p$DT=='article; proceedings paper',
                     ])) # counting and merging proceedings 

confe2 <- as_vector(
          nrow(M_2p[M_2p$DT=='conference paper',
                    ])) # counting conference papers

"Period 2" <- c(tot_period2, sources2, # creating a vector with the second period
                CAGR2, avg_cite2, n_docs2, 
                arti2, bchap2, procee2, confe2 )

##### 1.4. 2015 to 2022: third period ####

M_3p <- MM %>% filter(PY > 2015 & PY <= 2022) # sub-setting the third period

tot_period3 <-paste(min(M_3p$PY, # this returns the second period 
                        na.rm = T) , # studied in the bibliometric
                    "-", max(M_3p$PY,
                             na.rm = T))
Y3=table(M_3p$PY) # number of published docs by year
ny3=dim(Y3)[1] # returns how many years 
CAGR3<-as.numeric(
       round(((Y3[ny3]/Y3[1])^(1/(ny3-1))-1)*100,
             2)) # Compound annual growth rate

sources3 <- n_distinct(M_3p$SO) # counts the number of journals 
n_docs3 <- nrow(M_3p) # counts the number of published documents 
avg_cite3 <- round(sum(M_3p$TC)/nrow(M_3p), 2) # returns the average citation 

arti3 <- as_vector(
         nrow(M_3p[M_3p$DT=='article', ]) + # counting and merging 
         nrow(M_3p[M_3p$DT=='review', ])) # articles with reviewed articles

bchap3 <- as_vector(
          nrow(M_3p[M_3p$DT=='book chapter',]) + 
          nrow(M_3p[M_3p$DT=='article; book chapter',
                    ])) # counting and merging book chapters 

procee3 <- as_vector(
           nrow(M_3p[M_3p$DT=='proceedings paper',]) + 
           nrow(M_3p[M_3p$DT=='article; proceedings paper', 
                     ])) # counting and merging proceedings 

confe3 <- as_vector(nrow(M_3p[M_3p$DT=='conference paper', 
                              ])) # counting conference papers

"Period 3" <- c(tot_period3, sources3, # creating a vector with the third period
                CAGR3, avg_cite3, n_docs3, 
                arti3, bchap3, procee3, confe3 )

##### 1.5. Table with totals and periods ####

tot_docs <- data.frame(Details, # Table with descriptive analysis
                       `Period 1`, 
                       `Period 2`, 
                       `Period 3`, 
                       Total)

write_csv(tot_docs, file = here("Data", 
                                "Processed", 
                                "desc_analysis.csv")) # writing as CSV for Rmd


### 2. Line chart: publications and citations per year ####

MM %>% group_by(PY) %>% 
  summarise(totc = sum(TC)) %>%  
  ggplot(aes(x= PY, y= totc)) + geom_line() + geom_vline(xintercept = c(2008, 2015), 
                                                         linetype = "dotted",
                                                         color = "blue") + 
  labs(title = "Total Citations per Year" ,
       y = "Citations", x ="Years") + 
  theme(text = element_text(size = 14)) 

MM %>% group_by(PY) %>% 
  summarise(totd = n()) %>%  
  ggplot(aes(x= PY, y= totd)) + geom_line() + geom_vline(xintercept = c(2008, 2015), 
                                                         linetype = "dotted",
                                                         color = "blue") + 
  labs(title = "Total Publications per Year" ,
       y = "Publications", x ="Years") + 
  theme(text = element_text(size = 14)) 


### 3. Most influential articles ####

##### 3.1 Total #####
influ_arti <- MM %>% select(AU,PY, TI, TC) %>% # select columns with most cited art
              arrange(desc(TC)) %>% # order by citations descending order 
              mutate(Rank = 1:nrow(MM)) %>%  # add a rank column
              select( c(5, 1:4)) %>% # re order
              head(10) # select first 10 observations

write_csv(influ_arti, file = here("Data", 
                                "Processed", 
                                "influ_arti.csv")) # writing as CSV for Rmd

influ_arti <- influ_arti %>% mutate(Paper = paste(AU, PY, TI, sep = ", "))

##### 3.1. 2000 to 2007: first period ####

influ_arti_1p <- M_1p %>% select(AU,PY, TI, TC) %>% # select columns with most cited art
                 arrange(desc(TC)) %>% # order by citations descending order 
                 mutate(Rank = 1:nrow(M_1p)) %>% # add a rank column
                 select(c(5, 1:4)) %>% # re order
                 head(5) # select first 10 observations

write_csv(influ_arti_1p, file = here("Data", 
                                  "Processed", 
                                  "influ_arti_1p.csv")) # writing as CSV for Rmd


##### 3.2. 2008 to 2015: second period ####

influ_arti_2p <- M_2p %>% select(AU,PY, TI, TC) %>% # select columns with most cited art
                arrange(desc(TC)) %>% # order by citations descending order 
                mutate(Rank = 1:nrow(M_2p)) %>%  # add a rank column
                select(c(5, 1:4)) %>% # re order
                head(5) # select first 10 observations

write_csv(influ_arti_2p, file = here("Data", 
                                     "Processed", 
                                     "influ_arti_2p.csv")) # writing as CSV for Rmd


##### 3.3. 2016 to 2022: third period ####

influ_arti_3p <- M_3p %>% select(AU,PY, TI, TC) %>% # select columns with most cited art
                 arrange(desc(TC)) %>% # order by citations descending order 
                 mutate(Rank = 1:nrow(M_3p)) %>%  # add a rank column
                 select(c(5, 1:4)) %>% # re order
                 head(5) # select first 10 observations

write_csv(influ_arti_3p, file = here("Data", 
                                     "Processed", 
                                     "influ_arti_3p.csv")) # writing as CSV for Rmd


##### 3.4. Binding the three periods ####

influ_arti_bind <- rbind(influ_arti_1p, influ_arti_2p, influ_arti_3p)


influ_arti_bind <- influ_arti_bind %>% mutate(Article = paste0(AU, " (",PY,") ", "-",TI))
influ_arti_bind <- influ_arti_bind %>% select(Rank, Article, TC)
influ_arti_bind$Article <- str_to_title(influ_arti_bind$Article)


write_csv(influ_arti_bind, file = here("Data", 
                                     "Processed", 
                                     "influ_arti_bind.csv")) # writing as CSV for Rmd



##### 3.4. Local most cited articles ####
# How the most cited articles must be conduct?
# with a global sample or by periods ?

CR <- citations(MM, field = "article", sep = ";")
cbind(CR$Cited[1:10 ])
dfCR <- head(data.frame(Article=(unlist(CR$Cited))),10)
dfCR$Article.CR <- str_replace_all(as.character(dfCR$Article.CR), 
                          pattern = ", ", 
                          repl= ",") # convert the AU_CO into a chr vector


n_colscr <- max(stringr::str_count(dfCR$Article.CR, 
                                   ",")) # count the number of strings separated by ","
colmna_cr <- paste("field", # naming the columns 
                   1:n_colscr, 
                   sep ="")

# cr_dfs <- separate(dfCR, # create a df with the local CR 
#                    Article.CR, 
#                    into = colmna_cr, 
#                    sep = ",",
#                    remove = TRUE) %>% 
#           select(c(1:3,6)) %>% 
#           rename("AU"= field1,
#                  "PY"= field2,
#                  "SO"= field3,
#                  "Total LC"= Article.Freq)
# 
# 
# write_csv(influ_arti_3p, file = here("Data", 
#                                      "Processed", 
#                                      "influ_arti_3p.csv")) # writing as CSV for Rmd
# 

##### 3. 5. Most cited references ####


Lcr <- strsplit(MM$CR, 
                split = ";") # this code splits the CR and create a list

Lcr <- lapply(seq_along(Lcr), function(i) { # this function creates a data frame 
  l <- data.frame(ref = Lcr[[i]], # for the number of CR 
                  paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

Lcr <- (do.call(rbind, Lcr)) # this binds all the CR 
Lcr$ref <- trimws(Lcr$ref)


most_Lcr <- Lcr %>% group_by(ref) %>% dplyr::summarize(TC= n()) %>% 
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

write_csv(df_Lcr, file = here("Data", 
                                       "Processed", 
                                       "most_Lcr.csv")) # writing as CSV for Rmd



### 4. Most influential journals  ####

SO <- MM %>% filter(!is.na(SO)) %>%  # filter out NA
    group_by(SO) %>% # group by journal 
  dplyr::summarise(TC = sum(TC), # sum citations
                   PD= n()) %>%   # sum No of articles
  arrange(desc(TC)) %>% # order by descending order
  head(10)

names(SO) <- c("Journals", "TC", "PD")
SO$Rank <- seq(nrow(SO))
SO <- SO %>% select(Rank, Journals, TC, PD)
SO$Journals <- str_to_title(SO$Journals)

write_csv(SO, file = here("Data", 
                              "Processed", 
                              "influ_SO.csv")) # writing as CSV for Rmd


### 5. Most productive countries ####

##### 5.1. Single country publications ####

head(MM$AU_CO)
auco_v <- str_replace_all(as.character(MM$AU_CO), # convert the AU_CO into a chr vector
                          pattern = "; ", # deleting spaces between string elements
                          repl= ";") 

auco_df <- data.frame(country=auco_v) # converting the AU_CO vector into df
auco_df_u <-  auco_df %>% 
  mutate(split = str_split(auco_v, ";")) %>% # split
  mutate(split = purrr::map(.$split, ~ unique(.x))) %>% # drop duplicates
  mutate(split = purrr::map_chr(.$split, ~paste(.x, collapse = ";"))) %>% # recombine
  select(split)

n_colsco <- max(stringr::str_count(auco_df_u$split, 
                                   ";")) # count how many columns to split
colmna <- paste("country_", 1:n_colsco,
                sep ="") # naming the split columns 
auco_dfs <- separate(auco_df_u, # create df with authors country affiliation
                     split, # selecting the column
                     into = colmna, # choosing the names of the columns
                     sep = ";", # choosing separator 
                     remove = TRUE)





##### 5.2. Multiple country publications ####
# The max number of country collaboration was 7
# To get the total number of published documents by multiple countries
# we need to count the documents by column, pass the results to rbind and then
# get then group by coutnry

auco_mcp <- auco_dfs %>%
  pivot_longer(cols = starts_with("country"), values_to = "country") %>%
  drop_na(country) %>%
  group_by(country) %>%
  summarise(tot_docs = n()) %>%
  arrange(desc(tot_docs)) %>% rename(CO= country, PD= tot_docs)



cite_CO <- data.frame(cbind(CO= auco_df_u$split,TC= MM$TC))

cite_au_co <- strsplit(cite_CO$CO, 
                 split = ";") # this code splits the CR and create a list

cite_au_co <- lapply(seq_along(cite_au_co), function(i) { # this function creates a data frame 
  l <- data.frame(CO = cite_au_co[[i]], # for the number of CR 
                  paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

cite_au_co <- (do.call(rbind, cite_au_co)) # this binds all the CR 
cite_au_co$CO <- trimws(cite_au_co$CO)


listCO <- (strsplit(cite_CO$CO, ";"))
nCO <- lengths(listCO)
df_CO <- data.frame(CO=trimws(unlist(listCO)), SR=rep(MM$SR,nCO), TC=rep(MM$TC,nCO)) 
cite_CO <- df_CO %>% 
  group_by(CO) %>% 
  summarise(TC= sum(TC)) %>% 
  arrange(desc(TC)) %>% 
  ungroup() 


CO_perf <- inner_join(cite_CO, auco_mcp, by= "CO") %>%  head(10)
CO_perf$Rank <- seq(nrow(CO_perf)) 
CO_perf <- CO_perf %>% rename(Country = CO) %>% 
  select(Rank, Country, TC, PD)
CO_perf$Country <- str_to_title(CO_perf$Country)

write_csv(CO_perf, file = here("Data", 
                                  "Processed", 
                                  "country_perf.csv")) # writing as CSV for Rmd


g.mid<-ggplot(CO_perf,aes(x=1,y= reorder(Country, TC)))+geom_text(aes(label= Country), size =4)+
  geom_segment(aes(x=0.84,xend=0.96,yend=Country))+
  geom_segment(aes(x=1.14,xend=1.065,yend=Country))+
  ggtitle("")+
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA),
        plot.margin = unit(c(1,-4,1,-3), "mm"))

g.mid


co_tc <- ggplot(CO_perf, aes(x= TC, y= reorder(Country, TC))) + 
  geom_bar(stat = "identity") + ggtitle("Countries' Citations") +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        plot.margin = unit(c(1,-1,1,0), "mm")) + scale_x_reverse() +
  theme(plot.title = element_text(hjust = 0.5))

co_pd <- ggplot(CO_perf, aes(x= PD, y= reorder(Country, TC))) + 
  geom_bar(stat = "identity", fill = "#005997") + ggtitle("Countries' Published Docs") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,0,1,0), "mm")) +
  theme(plot.title = element_text(hjust = 0.5))
co_pd

gg1 <- ggplot_gtable(ggplot_build(co_tc))
gg2 <- ggplot_gtable(ggplot_build(co_pd))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/9,1/6,4/9))



##### 5.3. Table SCP and MCP and total publications by country ####
count_prod <- full_join(auco_1df, # make a full join with SCP
                        auco_mcp, # and MCP 
                        by= c("country_1" = "country_7")) %>% 
  replace(is.na(.), 0) %>% # raplacing NA with 0
  mutate(PD = tot_docs.x + tot_docs.y) %>% # total publications by country PD published Docs
  rename( "SCP" = tot_docs.x, # total SCP
          "MCP" = tot_docs.y,
          "Country"= country_1) %>% # total MCP
  arrange(desc(PD)) %>% # order in descendent order by total
  head(10) # select the first 10 observations 


write_csv(count_prod, file = here("Data", 
                                     "Processed", 
                                     "country_prod.csv")) # writing as CSV for Rmd

class(MM) <- c("bibliometrixDB", "data.frame")
# CW <- cocMatrix(MM, Field = "DE", type="matrix", sep=";")
# 
# DE_TM <- termExtraction(MM,Field="DE"
#                         ,remove.numbers=TRUE, 
#                         stemming=stemming, 
#                         language="english", 
#                         remove.terms = remove.terms, 
#                         synonyms = synonyms,
#                         keep.terms=NULL, verbose=FALSE)


### 6. Author's production and citations over time ####

##### 6.1 Total production  #####
listAU <- (strsplit(MM$AU, ";")) # split the columns AU
nAU <- lengths(listAU) # count the number of AU per doc
df <- data.frame(AU=trimws(unlist(listAU)), 
                 SR=rep(MM$SR,nAU)) # create a df with AU and docs
prod_AU <- df %>% # create a df called AU
  group_by(AU) %>% # group by AU
  count() %>% # count the docs per AU
  arrange(desc(n)) %>% # order in desc
  ungroup() %>% head(10) # print the first 10 

write_csv(prod_AU, file = here("Data", 
                                  "Processed", 
                                  "prod_AU.csv")) # writing as CSV for Rmd


##### 6.1.2. Author's Production over time dot chart #####
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

p <- ggplot(df2, aes(x= Author, y=year, round(TCpY,2))) +
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
  labs( x="Authors", y ="Years") + # title="Authors' Production over Time",
  theme(axis.title.y = element_text(vjust = 1)) +
  geom_line(data=df2,aes(x = Author, y = year, 
                         group=Author),size=1.0, color="firebrick4", alpha=0.3 )+
  coord_flip() + geom_text(data=summary_df, aes(x=Author, y=Inf, label= total_freq),
                           hjust=1, vjust=0.5, size=4, color="grey30") +
  scale_x_discrete(limits = rev(levels(df2$Author)))

p 


##### 6.2. Total citations over time  #####

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

c_tot <- ggplot(df2cite, aes(x= Author, y=year, round(TCpY,2))) +
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
  labs( x="Authors", y ="Years") + # title="Authors' Production over Time",
  theme(axis.title.y = element_text(vjust = 1)) +
  geom_line(data=df2cite,aes(x = Author, y = year, 
                             group=Author),size=1.0, color="firebrick4", alpha=0.3 )+
  coord_flip() + geom_text(data=summary_dfcite, aes(x=Author, y=Inf, label= total_TC),
                           hjust=1, vjust=0.5, size=4, color="grey30") +
  scale_x_discrete(limits = rev(levels(df2cite$Author)))

c_tot


##### 6.2.1. 2000 to 2007: first period ####

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


##### 6.2.2. 2008 to 2015: second period ####

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

##### 6.2.3. 2016 to 2022: third period ####

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



##### 6.2.4. Binding the three periods  ####

dfcite_tot <- rbind(dfcite1,dfcite2,dfcite3)


df2cite_tot <- rbind(df2cite1, df2cite2, df2cite3)

summary_dfcite_t <- df2cite_tot %>% group_by(Author) %>% 
  summarise(total_TC = sum(TC)) %>% arrange(desc(total_TC))


c_bind <- ggplot(df2cite_tot, aes(x= Author, y=year, round(TCpY,2))) +
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
  labs( x="Authors", y ="Years") + # title="Authors' Citations over Time",
  theme(axis.title.y = element_text(vjust = 1)) +
  geom_line(data=df2cite_tot,aes(x = Author, y = year, 
                                 group=Author),size=1.0, color="firebrick4", alpha=0.3 )+
  coord_flip() + geom_text(data=summary_dfcite_t, aes(x=Author, y=Inf, label= total_TC),
                           hjust=1, vjust=0.5, size=2.5, color="grey30") +
  scale_x_discrete(limits = rev(levels(df2cite_tot$Author))) 

c_bind
  



#### 6.3. Author's H- index ####

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
  summarise(Element = .data$AUs[1],
    h_index = h_calc(TC),
    PY_start = min(PY),
    TC = sum(TC),
    NP = length(AUs)) %>%
  rename(AU = AUs) %>%
  as.data.frame() %>% arrange(desc(h_index)) %>% head(10)

H_au <- ggdotchart(H, x = "AU", y = "h_index",
                 title = "Author's Impact",
                 color = "#005997",
                 palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
                 sorting = "descending",                       # Sort value in descending order
                 add = "segments",                             # Add segments from y = 0 to dots
                 rotate = TRUE,                                # Order by groups
                 dot.size = 9,                                 # Large dot size
                 label = round(H$h_index),                        # Add mpg values as dot labels
                 font.label = list(color = "white", size = 9,
                                   vjust = 0.5),               # Adjust label parameters
                 ggtheme = theme_classic()                        # ggplot2 theme
)
H_au



### 7. Most productive Universities ####


MM$AU_UN <- str_replace_all(MM$AU_UN, 
                            "\\bBRUNEL UNIV\\b|\\bBRUNEL UNIVERSITY LONDON\\b",
                            "BRUNEL UNIV LONDON")
MM$AU_UN <- str_replace_all(MM$AU_UN,
                           "UNIVERSITY", 
                           "UNIV") # changing UNIVERSITY with UNIV to avoid double counting

MM$AU_UN <- str_replace_all(MM$AU_UN,
                           "UNIV OF", 
                           "UNIV")

MM$AU_UN <- str_replace_all(MM$AU_UN, 
                            "UNIV LONDON LONDON SCH ECON AND POLIT SCI|LONDON SCHOOL OF ECONOMICS AND POLITICAL SCIENCE|LONDON SCHOOL OF ECONOMICS|LONDON SCH ECON AND POLIT SCI AND POLITICAL SCIENCE",
                            "LONDON SCH ECON AND POLIT SCI")

MM$AU_UN <- str_replace_all(MM$AU_UN, 
                            "UNIV. OF BREMEN",
                            "UNIV BREMEN")


MM$AU_UN <- str_replace_all(MM$AU_UN, 
                            "LOUGHBOROUGH UNIV TECHNOL|LOUGHBOROUGH UNIV",
                            "UNIV LOUGHBOROUGH")

MM$AU_UN <- str_replace_all(MM$AU_UN, 
                            "COMPLUTENSE UNIV MADRID|UCM UNIV COMPLUTENSE MADRID|COMPLUTENSE UNIV MADRID UCM|UNIVERSIDAD COMPLUTENSE DE MADRID",
                            "UNIV COMPLUTENSE MADRID")




au_UNs <- as.character(MM$AU_UN) # convert the AU_CO into a chr vector
au_UN <- as.data.frame(au_UNs) # converting the AU_CO vector into df

## selecting the unique values such as USA;USA... of AU_CO for each document row 
univ_df <-  au_UN %>% 
  mutate(split = str_split(au_UNs, ";")) %>% # split
  mutate(split = purrr::map(.$split, ~ unique(.x))) %>% # drop duplicates
  mutate(split = purrr::map_chr(.$split, ~paste(.x, collapse = "; "))) # recombine

univ_df <- univ_df %>% rename(AU_UN = split)
MM$AU_UN <- univ_df$AU_UN

univ <- strsplit(MM$AU_UN, 
                split = ";") # this code splits the CR and create a list

univ <- lapply(seq_along(univ), function(i) { # this function creates a data frame 
  l <- data.frame(UN = univ[[i]], # for the number of CR 
                  paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

univ <- (do.call(rbind, univ)) # this binds all the CR 
univ$UN <- trimws(univ$UN)

univ <- univ %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the D
univ <- univ %>% replace_with_na_all(condition = ~. == "NA") # converting "" into NA within the D
univ <- univ %>% replace_with_na_all(condition = ~. == "NOTREPORTED")


prod_univ <- univ %>% 
  filter(!is.na(UN)) %>% 
  group_by(UN) %>% 
  summarise(PD = n()) %>% 
  dplyr::arrange(desc(PD)) 

listUN <- (strsplit(MM$AU_UN, ";"))
nUN <- lengths(listUN)
df_un <- data.frame(UN=trimws(unlist(listUN)), SR=rep(MM$SR,nUN), TC=rep(MM$TC,nUN)) 
cite_univ <- df_un %>% 
  group_by(UN) %>% 
  summarise(TC= sum(TC)) %>% 
  arrange(desc(TC)) %>% 
  ungroup() 

univ_perf <- inner_join(cite_univ, prod_univ, by= "UN") %>%  head(10)
univ_perf$Rank <- seq(nrow(univ_perf)) 
univ_perf <- univ_perf %>% rename(University = UN) %>% 
  select(Rank, University, TC, PD)
univ_perf$University <- str_to_title(univ_perf$University)



write_csv(univ_perf, file = here("Data", 
                               "Processed", 
                               "univ_perf.csv")) # writing as CSV for Rmd








