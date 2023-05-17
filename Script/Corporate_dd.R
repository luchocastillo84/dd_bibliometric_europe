library(jsonlite)
library(tidyverse)
library(here)
library(bibliometrix)
library(devtools)
library(readtext)
library(tidyr)
library(readr)
library(visdat)
library(stringr)
library(dplyr)
library(readxl)
library(Matrix)
library(MASS)
library(igraph)
library(RColorBrewer)
library(rstudioapi)

M <- read_csv(file = here("Data", 
                          "Processed", 
                          "M_EU.csv"))
class(M) <- c("bibliometrixDB", "data.frame")

M = M[order(M$PY), ]

# corporate|firms|business   industry|corporate|firms|business
topic <- which(grepl("industry|corporate|firm\\b|business| SMEs\\b", M$TI, ignore.case = T))
i <- topic
toJSON(M[i, c(2, 1, 3, 17, 14,25, 13, 6, 8, 12, 22, 29, 23)], pretty= T)


corp_dd_lit <- M[topic,]
corp_dd_lit <- corp_dd_lit[order(corp_dd_lit$PY),]

corp_DD_redux <- corp_dd_lit[, c(13, 12, 1, 2, 8, 23)]
corp_DD_redux <- corp_DD_redux %>% mutate(Author = paste0(AU, " (",PY,") ")) %>% 
  rename(Title = TI, Abstract= AB)
corp_DD_redux$Abstract <- tolower(corp_DD_redux$Abstract)
corp_DD_redux$Author <- str_to_title(corp_DD_redux$Author)
corp_DD_redux$Title <- str_to_title(corp_DD_redux$Title)
toJSON(corp_DD_redux[, c(7, 4, 5)], pretty= T)

CDD_1p <- corp_dd_lit %>% filter(PY < 2008)


CDD_2p <- corp_DD_redux %>% filter(PY >= 2008 & PY <= 2015) # sub-setting the second period



CDD_3p <- corp_DD_redux %>% filter(PY >= 2016 & PY <= 2022) # sub-setting the second period



CC_corp <- cocMatrix(corp_dd_lit, Field = "CR", type = "sparse", sep = ";")
CC_corp_mat <- crossprod(CC_corp, CC_corp)

CC_corp_mat <- CC_corp_mat[nchar(colnames(CC_corp_mat)) != 0, nchar(colnames(CC_corp_mat)) != 0]

CC_corp_mat_labels <- colnames(CC_corp_mat)
LABEL <- removeDuplicatedlabels(trimws(CC_corp_mat_labels))

colnames(CC_corp_mat) <- rownames(CC_corp_mat) <-  LABEL

set.seed(1001)
net=netPlot(CC_corp_mat, n = 10, Title = "Co-Citation Network,  2000 - 2007", # Louvain
            type = "fruchterman", size.cex=TRUE, size=20, 
            remove.multiple=FALSE, labelsize=0.9,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain", curved = F, remove.isolates = F)



co_cite1 <- head(net$cluster_res,35)

co_cite1 <- data.frame(net$cluster_res)
groups1 <- data.frame(cluster = net$community_obj[[3]])
degree1 <- net$nodeDegree

co_cite_df <- inner_join(co_cite1, degree1, by= c("vertex"= "node"))
co_cite_df <- co_cite_df[, -c(4,5)]
write_csv(co_cite_df, file = here("Output", 
                                  "Data", 
                                  "co_ci1.csv")) 









