
## table for calculations

M_by_PF <- M %>% # dist contains a summary of documents type DT by unique DOI and BN
  group_by(PF) %>% # group by document type
  summarize(count = n(), # summary by count by DT 
            DI = n_distinct(DI),
            NA_DI = sum(duplicated(DI))) %>%  # unique titles 
  adorn_totals("row") # total 



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





AU_UN <- as.data.frame(gsub("UNIVERSITY OF OXFORD", "UNIV OXFORD", AU_UN))

M_by_DT <- M %>% # dist contains a summary of documents type DT by unique DOI and BN
  group_by(PF) %>% # group by document type
  dplyr::summarize(count = n()) %>%  # unique titles 
  adorn_totals("row") # total 

# To see what the sep is
M$CR[1]

# To obtain the most frequent cited manuscripts:
CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])
CRf <- data.frame(CR=unlist(CR))
CRf <- ldply (CR, data.frame)

# To obtain the most frequent cited first authors:
CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:10])


dfCR <- data.frame(Article=(unlist(CR$Cited))) 
listCR <- strsplit(as.character(dfCR$Article.CR), ",")



topAU <- authorProdOverTime(M, k = 10, graph = T)

AU_CO <- M$AU_CO
MM <- as.data.frame(AU_CO)
A <- cocMatrix(MM, Field = "AU_CO", sep = ";")
au_co <- unlist(sort(Matrix::colSums(A), decreasing = TRUE))

TCm <- format(mean(as.numeric(M$TC), na.rm=TRUE),digits = 4)
TCmy <- format(mean(as.numeric(M$MostCitedPapers$TCperYear), na.rm = TRUE), digits = 4) 
CollIndex <- format(object$AuMultiAuthoredArt/sum(object$nAUperPaper>1),digits=3)  # Collaboration Index
MYfP <- as.numeric(substr(Sys.time(),1,4))-mean(M$PY,na.rm = TRUE)

Y=table(M$PY)
ny=dim(Y)[1]
CAGR<-as.numeric(round(((Y[ny]/Y[1])^(1/(ny-1))-1)*100,2))


results <- biblioAnalysis(M)
summary(results, k=20, pause=F, width=100)
sum(M$TC) / nrow(M)

AU1_CO <- metaTagExtraction(M, Field = "AU1_CO", sep = ";", aff.disamb = F)

# Most influential authors 
listAU <- (strsplit(M$AU, ";"))
nAU <- lengths(listAU)
df <- data.frame(AU=trimws(unlist(listAU)), 
                 SR=rep(M$SR,nAU), 
                 TC=rep(M$TC, nAU)) 

AU <- df %>% 
  group_by(AU) %>% 
  dplyr::summarise(Total_citations= sum(TC), 
                   Total_articles= n()) %>% 
  arrange(desc(Total_citations)) %>%
  mutate(Citations_per_article =round(Total_citations / Total_articles, 2)) %>% 
  ungroup() 

# Most influential articles 

influ_arti <- M %>% select(AU,PY, TI, TC) %>% arrange(desc(TC)) %>% 
              mutate(Rank = 1:nrow(M))
influ_arti <- head(influ_arti[, c(5, 1:4)],5)

# Local most cited articles

CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])
dfCR <- head(data.frame(Article=(unlist(CR$Cited))),5)

# Most influential journals 

SO <- M %>% filter(!is.na(SO)) %>%  
  group_by(SO) %>% 
  dplyr::summarise(TC = sum(TC), AR= n()) %>%  
  arrange(desc(TC)) %>% head(10)

# Most productive universities 

vis_miss(M)

auuni_v <- str_replace_all(as.character(M$AU_UN), 
                          pattern = "; ", 
                          repl= ";") # convert the AU_CO into a chr vector

auuni_df <- data.frame(univ=auuni_v) # converting the AU_CO vector into df
auuni_df_u <-  auuni_df %>% 
  mutate(split = str_split(auuni_v, ";")) %>% # split
  mutate(split = purrr::map(.$split, ~ unique(.x))) %>% # drop duplicates
  mutate(split = purrr::map_chr(.$split, ~paste(.x, collapse = ";"))) %>% # recombine
  select(split)

n_colsu <- max(stringr::str_count(auuni_df_u$split, ";")) 
colmnam <- paste("univ_", 1:n_colsu, sep ="")
auuni_dfs <- separate(auuni_df_u, 
                     split, 
                     into = colmnam, 
                    sep = ";",
                     remove = TRUE) %>% replace_with_na_all(condition = ~. == "NA")

auuni_1df <- auuni_dfs %>% 
  filter(!is.na(univ_1)) %>% # filtering by NA country_2 to get SCP
  select(univ_1) %>% 
  group_by(univ_1) %>% 
  dplyr::summarise(tot_docs= n()) %>% 
  arrange(desc(tot_docs))


# Most productive countries
# Single country publications
head(M$AU_CO)
auco_v <- str_replace_all(as.character(M$AU_CO), # convert the AU_CO into a chr vector
                          pattern = "; ", # deleting spaces between string elements
                          repl= ";") 

auco_df <- data.frame(country=auco_v) # converting the AU_CO vector into df
auco_df_u <-  auco_df %>% 
  mutate(split = str_split(auco_v, ";")) %>% # split
  mutate(split = purrr::map(.$split, ~ unique(.x))) %>% # drop duplicates
  mutate(split = purrr::map_chr(.$split, ~paste(.x, collapse = ";"))) %>% # recombine
  select(split)

n_cols <- max(stringr::str_count(auco_df_u$split, ";")) 
colmna <- paste("country_", 1:n_cols, sep ="")
auco_dfs <- separate(auco_df_u, 
                     split, 
                     into = colmna, 
                     sep = ";",
                     remove = TRUE)

auco_1df <- auco_dfs %>% 
            filter(is.na(country_2)) %>% # filtering by NA country_2 to get SCP
            select(country_1) %>% 
            group_by(country_1) %>% 
            dplyr::summarise(tot_docs= n()) %>% 
            arrange(desc(tot_docs))


# Multiple country publications
auco_2df <- auco_dfs %>% 
  select(country_2) %>% 
  group_by(country_2) %>% 
  drop_na(country_2) %>%
  dplyr::summarise(tot_docs= n()) %>% 
  arrange(desc(tot_docs))

auco_3df <- auco_dfs %>% 
  select(country_3) %>% 
  group_by(country_3) %>% 
  drop_na(country_3) %>%
  dplyr::summarise(tot_docs= n()) %>% 
  arrange(desc(tot_docs)) %>% 
  rbind(setNames(auco_2df, names(auco_3df)))

auco_4df <- auco_dfs %>%
  select(country_4) %>% 
  group_by(country_4) %>% 
  drop_na(country_4) %>%
  dplyr::summarise(tot_docs= n()) %>% 
  arrange(desc(tot_docs)) %>% 
  rbind(setNames(auco_3df, names(auco_4df)))

auco_5df <- auco_dfs %>%
  select(country_5) %>% 
  group_by(country_5) %>% 
  drop_na(country_5) %>%
  dplyr::summarise(tot_docs= n()) %>% 
  arrange(desc(tot_docs)) %>% 
  rbind(setNames(auco_4df, names(auco_5df)))

auco_6df <- auco_dfs %>%
  select(country_6) %>% 
  group_by(country_6) %>% 
  drop_na(country_6) %>%
  dplyr::summarise(tot_docs= n()) %>% 
  arrange(desc(tot_docs)) %>% 
  rbind(setNames(auco_5df, names(auco_6df)))

auco_7df <- auco_dfs %>%
  select(country_7) %>% 
  group_by(country_7) %>% 
  drop_na(country_7) %>%
  dplyr::summarise(tot_docs= n()) %>% 
  arrange(desc(tot_docs)) %>% 
  rbind(setNames(auco_6df, names(auco_7df)))

auco_mcp <- auco_7df %>% 
            group_by(country_7) %>% 
            dplyr::summarise(tot_docs= sum(tot_docs)) %>% 
            arrange(desc(tot_docs))


# Table SCP and MCP and total publications by country          
count_prod <- full_join(auco_1df, 
                        auco_mcp, 
                        by= c("country_1" = "country_7")) %>% 
              replace(is.na(.), 0) %>% 
              mutate(tot_docs = tot_docs.x + tot_docs.y) %>% 
              rename( "scp" = tot_docs.x, "mcp" = tot_docs.y) %>% 
              arrange(desc(tot_docs)) %>% 
              head(10)

library(jsonlite)

topic <- which(grepl("industry|corporate|firms|business| SMEs\\b", MM$TI, ignore.case = T))
topic <- which(grepl("\\bdigital economy\\b", MM$TI, ignore.case = TRUE))
m_cited <- head(order(-MM$TC), 10)
# corporate|firms|business   industry|corporate|firms|business
# 10  219 1173 1355 1580 1714 corporate, economy, industry
# 556  618 1024 1044 1120 1176 1231 1361 1414 1487 1573 1597 1648 1723 1860 business
# 308  379r  556  593  999 1006 1189 1573 1580 1600 1679 1874 firms

i <- topic
toJSON(MM[i, c(2, 1, 3, 17, 14,25, 13, 6, 8, 12, 22, 29, 23)], pretty= T, )
toJSON(M[i, c(11)], pretty= T)

corp_dd_lit <- MM[topic,]
corp_dd_lit <- corp_dd_lit[order(corp_dd_lit$PY),]
n_1p = n_1p[order(n_1p$PY), ]


# delete this doi 10.1145/1242572.1242583
# 10.1108/073788309
# c("10.1145/1242572.1242583", "10.1108/073788309",
#  )





M %>% group_by(PY) %>% 
  summarise(totc = sum(TC)) %>%  
  ggplot(aes(x= PY, y= totc)) + geom_line() + geom_vline(xintercept = c(2007, 2014), 
                                                         linetype = "dotted",
                                                         color = "blue")

M %>% group_by(PY) %>% 
      summarise(totd = n()) %>%  
      ggplot(aes(x= PY, y= totd)) + geom_line() + geom_vline(xintercept = c(2007, 2014), 
                                                         linetype = "dotted",
                                                         color = "blue")

Details <- c("Timespan", "Sources (Journals, Books, etc)", "Annual growth rate %", 
         "Average citations per doc", "Total published documents", "Articles", 
         "Book chapters", "Porceeding papers", "Conference papers") # 11 items

tot_period <-paste(min(M$PY, na.rm = T) , "-", max(M$PY, na.rm = T))
Y=table(M$PY)
ny=dim(Y)[1]
CAGR<-as.numeric(round(((Y[ny]/Y[1])^(1/(ny-1))-1)*100,2))
sources <- n_distinct(M$SO)
n_docs <- nrow(M)
avg_cite <- round(sum(M$TC)/nrow(M), 2)

arti <- as_vector(nrow(M[M$DT=='article', ]) + 
                  nrow(M[M$DT=='review', ])) 

bchap <- as_vector(nrow(M[M$DT=='book chapter',]) + 
                   nrow(M[M$DT=='article; book chapter',]))

procee <- as_vector(nrow(M[M$DT=='proceedings paper',]) + 
                      nrow(M[M$DT=='article; proceedings paper', ]))

confe <- as_vector(nrow(M[M$DT=='conference paper', ]))

arti+ bchap+ procee + confe

Total <- c(tot_period, sources, CAGR,
           avg_cite, n_docs, arti, bchap, procee, confe)



M_1p <- M %>% filter(PY <= 2007)
unique(M_1p$PY)


tot_period1 <-paste(min(M_1p$PY, na.rm = T) , "-", max(M_1p$PY, na.rm = T))
Y1=table(M_1p$PY)
ny1=dim(Y1)[1]
CAGR1<-as.numeric(round(((Y1[ny1]/Y1[1])^(1/(ny1-1))-1)*100,2))
sources1 <- n_distinct(M_1p$SO)
n_docs1 <- nrow(M_1p)
avg_cite1 <- round(sum(M_1p$TC)/nrow(M_1p), 2)

arti1 <- as_vector(nrow(M_1p[M_1p$DT=='article', ]) + 
                    nrow(M_1p[M_1p$DT=='review', ])) 

bchap1 <- as_vector(nrow(M_1p[M_1p$DT=='book chapter',]) + 
                     nrow(M_1p[M_1p$DT=='article; book chapter',]))

procee1 <- as_vector(nrow(M_1p[M_1p$DT=='proceedings paper',]) + 
                      nrow(M_1p[M_1p$DT=='article; proceedings paper', ]))

confe1 <- as_vector(nrow(M_1p[M_1p$DT=='conference paper', ]))

"Period 1" <- c(tot_period1, sources1, CAGR1,
           avg_cite1, n_docs1, arti1, bchap1, procee1, confe1 )

listAU1 <- (strsplit(M_1p$AU, ";"))
nAU1 <- lengths(listAU1)
df1 <- data.frame(AU=trimws(unlist(listAU1)), SR=rep(M_1p$SR,nAU1), TC=rep(M_1p$TC, nAU1)) 
AU1 <- df1 %>% 
  group_by(AU) %>% 
  dplyr::summarise(Total_citations= sum(TC), 
                   Total_articles= n()) %>% 
  arrange(desc(Total_citations)) %>%
  mutate(Citations_per_article =round(Total_citations / Total_articles, 2)) %>% 
  ungroup() 

m_cited <- head(order(-M_1p$TC), 10)
i <- m_cited
toJSON(M_1p[i, c(2, 1, 3, 17, 13, 6, 8, 12, 22, 29, 23)], pretty= T)

results <- biblioAnalysis(M_1p)
summary(results, k=20, pause=F, width=100)

M_2p <- M %>% filter(PY > 2007 & PY <= 2014)
unique(M_2p$PY)

tot_period2 <-paste(min(M_2p$PY, na.rm = T) , "-", max(M_2p$PY, na.rm = T))
Y2=table(M_2p$PY)
ny2=dim(Y2)[1]
CAGR2<-as.numeric(round(((Y2[ny2]/Y2[1])^(1/(ny1-1))-1)*100,2))
sources2 <- n_distinct(M_2p$SO)
n_docs2 <- nrow(M_2p)
avg_cite2 <- round(sum(M_2p$TC)/nrow(M_2p), 2)

arti2 <- as_vector(nrow(M_2p[M_2p$DT=='article', ]) + 
                    nrow(M_2p[M_2p$DT=='review', ])) 

bchap2 <- as_vector(nrow(M_2p[M_2p$DT=='book chapter',]) + 
                     nrow(M_2p[M_2p$DT=='article; book chapter',]))

procee2 <- as_vector(nrow(M_2p[M_2p$DT=='proceedings paper',]) + 
                      nrow(M_2p[M_2p$DT=='article; proceedings paper', ]))

confe2 <- as_vector(nrow(M_2p[M_2p$DT=='conference paper', ]))

"Period 2" <- c(tot_period2, sources2, CAGR2,
            avg_cite2, n_docs2, arti2, bchap2, procee2, confe2 )

listAU2 <- (strsplit(M_2p$AU, ";"))
nAU2 <- lengths(listAU2)
df2 <- data.frame(AU=trimws(unlist(listAU2)), SR=rep(M_2p$SR,nAU2), TC=rep(M_2p$TC, nAU2)) 
AU2 <- df2 %>% 
  group_by(AU) %>% 
  dplyr::summarise(Total_citations= sum(TC), 
                   Total_articles= n()) %>% 
  arrange(desc(Total_citations)) %>%
  mutate(Citations_per_article =round(Total_citations / Total_articles, 2)) %>% 
  ungroup() 

m_cited <- head(order(-M_2p$TC), 10)
i <- m_cited
toJSON(M_2p[i, c(2, 1, 3, 17, 13, 6, 8, 12, 22, 29, 23)], pretty= T)


results <- biblioAnalysis(M_2p)
summary(results, k=20, pause=F, width=100)

M_3p <- M %>% filter(PY > 2014 & PY <= 2021)
unique(M_3p$PY)

tot_period3 <-paste(min(M_3p$PY, na.rm = T) , "-", max(M_3p$PY, na.rm = T))
Y3=table(M_3p$PY)
ny3=dim(Y3)[1]
CAGR3<-as.numeric(round(((Y3[ny3]/Y3[1])^(1/(ny3-1))-1)*100,2))
sources3 <- n_distinct(M_3p$SO)
n_docs3 <- nrow(M_3p)
avg_cite3 <- round(sum(M_3p$TC)/nrow(M_3p), 2)

arti3 <- as_vector(nrow(M_3p[M_3p$DT=='article', ]) + 
                    nrow(M_3p[M_3p$DT=='review', ])) 

bchap3 <- as_vector(nrow(M_3p[M_3p$DT=='book chapter',]) + 
                     nrow(M_3p[M_3p$DT=='article; book chapter',]))

procee3 <- as_vector(nrow(M_3p[M_3p$DT=='proceedings paper',]) + 
                      nrow(M_3p[M_3p$DT=='article; proceedings paper', ]))

confe3 <- as_vector(nrow(M_3p[M_3p$DT=='conference paper', ]))

"Period 3" <- c(tot_period3, sources3, CAGR3,
            avg_cite3, n_docs3, arti3, bchap3, procee3, confe3 )

tot_docs <- data.frame(Details, `Period 1`, `Period 2`, `Period 3`, Total)




### Authors 
listAU3 <- (strsplit(M_3p$AU, ";"))
nAU3 <- lengths(listAU3)
df3 <- data.frame(AU=trimws(unlist(listAU3)), 
                  SR=rep(M_3p$SR,nAU3), 
                  TC=rep(M_3p$TC, nAU3)) 
AU3 <- df3 %>% 
  group_by(AU) %>% 
  dplyr::summarise(Total_citations= sum(TC), 
                   Total_articles= n()) %>% 
  arrange(desc(Total_citations)) %>%
  mutate(Citations_per_article =round(Total_citations / Total_articles, 2)) %>% 
  ungroup() 

m_cited <- head(order(-M_3p$TC), 10)
i <- m_cited
toJSON(M_3p[i, c(2, 1, 3, 17, 13, 6, 8, 12, 22, 29, 23)], pretty= T)


results <- biblioAnalysis(M_3p)
summary(results, k=20, pause=F, width=100)
dominance(results, k= 10)


CR1 <- which(grepl("no title capture", M$CR, ignore.case = TRUE))
i <- 1450
toJSON(M[i, c(2, 1, 3, 17, 13, 6, 8, 12, 22, 29, 23, 11)], pretty= T)

ISI <- M %>% filter(DB == "ISI")
ISI$CR[1000]
SCOPUS <- M %>% filter(DB == "SCOPUS")
SCOPUS$CR[8]


CRisi <- citations(ISI, field = "article", sep = ";")
CRsco <- citations(SCOPUS, field = "article", sep = ";")


all_isi <- m_bdf
all_isi <- m_bdf %>% filter(PF %in% c("WOS"))
all_isi$CR[400]
all_isi$DB <- str_replace_all(all_isi$DB,
                              "SCOPUS", 
                              "ISI")
m_bdf <- convert2df(here("Data",  # processed data frame for bibliometrics 37 columns
                         "Processed", 
                         "M_EU.csv"),
                    dbsource = "scopus", 
                    format = "csv")

histResults <- histNetwork(m_bdf, sep = ";")

WLCR <- cocMatrix(M, "LCR", sep = ";")


results <- biblioAnalysis(M)
summary(results, k=20, pause=F, width=100)

all_isi <- m_bdf
all_isi <- m_bdf %>% filter(PF %in% c("WOS"))
all_isi$CR[400]

all_isi = all_isi[order(all_isi$PY), ]
all_isi$Paper <- 1:nrow(all_isi)
all_isi_orig <- all_isi
all_isi$nLABEL <- 1:nrow(all_isi)


CR <- strsplit(all_isi$CR, split = ";")

CR <- lapply(seq_along(CR), function(i) {
  l <- data.frame(ref = CR[[i]],
                  paper = i,
                  stringsAsFactors = FALSE)})

CR <- (do.call(rbind, CR))

CR$DI <-
  trimws(unlist(lapply(
    strsplit(CR$ref, 'DOI', fixed = TRUE), '[', 2
  )))
CR$DI[is.na(CR$DI) | CR$DI=="NA"] <- ""
CR$AU <-
  trimws(gsub("[ ]{2,}", "", (gsub(
    "\\.", " ", unlist(lapply(strsplit(CR$ref, ',', fixed = TRUE), '[', 1))
  ))))

CR$PY <-
  trimws(unlist(lapply(strsplit(CR$ref, ',', fixed = TRUE), '[', 2)))
CR$SO <-
  trimws(unlist(lapply(strsplit(CR$ref, ',', fixed = TRUE), '[', 3)))
CR$SR <- paste(CR$AU, ", ", CR$PY, ", ", CR$SO, sep = "")

all_isi$LABEL <- paste(all_isi$SR_FULL)

CR$LABEL <- paste(CR$SR) 


L <- left_join(all_isi,CR,by=c("LABEL"))

L <- L[!is.na(L$paper),]
L$CITING <- all_isi$LABEL[L$paper]
L$nCITING <- all_isi$nLABEL[L$paper]
L$CIT_PY <- all_isi$PY[L$paper]

LCS <- L %>% group_by(.data$nLABEL) %>%
  summarize(LABEL = .data$LABEL[1],
            n = length(.data$nLABEL)) %>%
  as.data.frame()

all_isi$LCS <- 0
all_isi[LCS$nLABEL, "LCS"] <- LCS$n
all_isi_orig$LCS <- all_isi$LCS

histData <- all_isi[c("LABEL","TI","DE","ID","PY","LCS","TC")]
names(histData) <- c("Paper","Title","Author_Keywords","KeywordsPlus","Year","LCS","GCS")

CITING <- L %>% group_by(.data$CITING) %>%
  summarize(
    LCR = paste(.data$LABEL, collapse = ";"),
    PY = .data$CIT_PY[1],
    Paper = .data$paper[1]
  ) %>%
  ungroup() %>%
  arrange(.data$PY) %>% as.data.frame()

all_isi_orig$LCR <- NA
all_isi_orig$LCR[CITING$Paper] <- CITING$LCR
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
missingLABEL <- setdiff((M$LABEL), colnames(WLCR))
colLab <- c(colnames(WLCR), missingLABEL)
WLCR <- cbind(WLCR, matrix(0, nrow(WLCR), length(missingLABEL)))
WLCR <- as.data.frame(as.matrix(WLCR), stringsAsFactors = FALSE)
colnames(WLCR) <- colLab
LABEL <- (row.names(WLCR))
WLCR <- as.matrix(WLCR[LABEL])
  
NetMatrix <- biblioNetwork(data.frame(M), analysis = "collaboration",
                           network = "countries", sep = ";")

# TAM acceptance model\\b
# Adoption diffusion\\b Quantitative research|Descriptive statistics|Inferential|equation|cluster|network analysis
# quanti quantitative|regression|logistic|equation|measuring
# quali "Qualitative research\\b|Case study\\b|Content analysis\\b|Thematic analysis\\b|Grounded theory\\b|Phenomenology\\b|Ethnography\\b|Discourse analysis\\b|interviews\\b|Focus groups\\b|Observations\\b|Field notes\\b|Reflexivity\\b|Triangulation\\b"
topic <- which(grepl("Quantitative research\\b|Descriptive statistics\\b|Inferential statistics\\b|Hypothesis testing\\b|Experimental design\\b|Control group\\b|Treatment group\\b|Random assignment\\b|Random sampling\\b|Correlational research\\b|Regression analysis\\b|Analysis of variance\\b|Chi-square test\\b|t-test\\b|Effect size\\b|Confidence intervals\\b|Statisti\\b|Survey research\\b|Closed-ended questions\\b|Likert scale\\b|Factor analysis\\b|Reliability\\b|Validity\\b|cluster analyisis\\b", N$AB, ignore.case = T))
topic <- which(grepl("dividend", M$DE, ignore.case = T))
i <- topic
toJSON(M[i, c(2, 1, 3, 17, 14,25, 13, 6, 8, 12, 22, 29, 23)], pretty= T)
explore <- N[topic,]
explore <- explore[, c(25, 13, 12, 1, 2, 8, 23)]
explore <- explore[, c(2 ,4, 6)]



journals_data <- data.frame(
  Rank = 1:10,
  Journals = c(
    "New Media & Soc.",
    "Information Soc.",
    "Info. Communication & Soc.",
    "Gov. Information Quarterly",
    "Telecomm. Policy",
    "Poetics",
    "Telematics & Informatics",
    "Comp. In Human Behavior",
    "Universal Access In Info. Society",
    "Eur. Journal Of Communication"
  ),
  TC = c(4427, 2066, 1165, 965, 961, 945, 921, 837, 590, 447),
  PD = c(42, 26, 43, 15, 34, 6, 21, 15, 15, 6)
)

# Normalize the TC and PD data for easier comparison
journals_data$TC_norm <- journals_data$TC / max(journals_data$TC)
journals_data$PD_norm <- journals_data$PD / max(journals_data$PD)

# Create a bar chart for Times Cited (TC)
plot1 <- ggplot(journals_data, aes(x = Journals, y = TC_norm, fill = "TC")) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Times Cited (TC)", limits = c(0, 1), sec.axis = sec_axis(~ ., name = "Published Documents (PD)")) +
  labs(title = "Journal Performance", x = "Journals") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)

# Create a bar chart for Published Documents (PD)
plot2 <- ggplot(journals_data, aes(x = Journals, y = PD_norm, fill = "PD")) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_void() +
  guides(fill = FALSE)

# Align the two plots into a single plot
aligned_plots <- align_plots(plot1, plot2, align = "hv", axis = "b")
plot_grid(aligned_plots[[1]], aligned_plots[[2]], ncol = 1)


#### 6.3. Author's H- index ####

AU = M_3p$AU
AU <- str_to_title(AU)
AU <- trimES(gsub(","," ",AU))
listAU <- strsplit(AU, split=";")
l <- lengths(listAU)
index= rep(row.names(M_3p), l)
df <- M_3p[index,]
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
  as.data.frame() %>% arrange(desc(h_index)) %>% head(15)

H_au <- ggdotchart(H, x = "AU", y = "h_index",
                   #title = "Author's Impact",
                   color = "#005997",
                   palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
                   sorting = "descending",                       # Sort value in descending order
                   add = "segments",                             # Add segments from y = 0 to dots
                   rotate = TRUE,                                # Order by groups
                   dot.size = 9,                                 # Large dot size
                   xlab = "Authors",
                   ylab = "H-index",
                   label = round(H$h_index),                        # Add mpg values as dot labels
                   font.label = list(color = "white", size = 9,
                                     vjust = 0.5),               # Adjust label parameters
                   ggtheme = theme_classic()                        # ggplot2 theme
)
H_au



# Create a sample graph
g <- erdos.renyi.game(10, 0.4)

# Perform clustering (you can replace this with your own clustering method)
wc <- cluster_walktrap(g)

cluster1_keywords <- c("Topic1", "Topic2", "Topic3")
cluster2_keywords <- c("Topic4", "Topic5", "Topic6")

colors <- rainbow(max(membership(wc)))
V(g)$color <- colors[membership(wc)]

plot(g, vertex.label=NA)

# Create legend labels
legend_labels <- c(paste(cluster1_keywords, collapse = ", "),
                   paste(cluster2_keywords, collapse = ", "))

# Plot the legend
legend("topleft", legend = legend_labels, col = colors[1:2], pch = 20, bty = "n", cex = 0.8)


g <- graph_from_adjacency_matrix(COLun1_mat, mode = "undirected", weighted = TRUE)
wc <- cluster_louvain(g)

cluster1_keywords <- c("Topic1", "Topic2", "Topic3")
cluster2_keywords <- c("Topic4", "Topic5", "Topic6")

colors <- rainbow(max(membership(wc)))
V(g)$color <- colors[membership(wc)]
V(g)$label <- V(g)$name
V(g)$label.color <- "black"
V(g)$size <- 6

plot(g, vertex.label=NA)

# Create legend labels
legend_labels <- c(paste(cluster1_keywords, collapse = ", "),
                   paste(cluster2_keywords, collapse = ", "))

# Plot the legend
legend("topleft", legend = legend_labels, col = colors[1:3], pch = 20, bty = "n", cex = 0.8)








