# Load the necessary packages
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

load(here("Script", "Environments", "Net4Mat.RData"))

################################################################################
####################### Loading and splitting the data #########################
################################################################################

M <- read_csv(file = here("Data", 
                          "Processed", 
                          "M_EU.csv"))

class(M) <- c("bibliometrixDB", "data.frame")
N <- M

N$DB <- "ISI"
N$SR <- N$SRDI
rownames(N) <- N$SRDI # using SR as row names

write_csv(N, file = here("Data", 
                         "Processed", 
                         "N_EU.csv")) # writing as CSV to make readable in biblioshiny
class(N) <- c("bibliometrixDB", "data.frame")
save(N, file = "N_EU.rda")

# N$LABEL <- paste(N_AU, N$PY, sep = ", ")
# N$LABEL <- LABEL
n_1p <- N %>% filter(PY < 2008)
n_1p = n_1p[order(n_1p$PY), ]
save(n_1p, file = "n1_EU.rda")

n_2p <- N %>% filter(PY >= 2008 & PY <= 2015) # sub-setting the second period
n_2p = n_2p[order(n_2p$PY), ]
save(n_2p, file = "n2_EU.rda")


n_3p <- N %>% filter(PY >= 2016 & PY <= 2022) # sub-setting the second period
n_3p = n_3p[order(n_3p$PY), ]
save(n_3p, file = "n3_EU.rda")


################################################################################
########################## Country Collaboration Network #######################
################################################################################

CC_CO <- cocMatrix(N, Field = "AU_CO", type = "sparse", sep = ";")
CC_CO_mat <- crossprod(CC_CO, CC_CO)

CC_CO_mat <- CC_CO_mat[nchar(colnames(CC_CO_mat)) != 0, nchar(colnames(CC_CO_mat)) != 0]

CC_CO_mat_labels <- colnames(CC_CO_mat)
LABEL <- removeDuplicatedlabels(CC_CO_mat_labels)
colnames(CC_CO_mat) <- rownames(CC_CO_mat) <-  LABEL

set.seed(1001)
col_CO <- invisible(net_CO <- netPlot(CC_CO_mat, n = 30, Title = "Co-Citation Network Total louvain", 
            type = "circle", size.cex=TRUE, size=20, 
            remove.multiple=FALSE, labelsize=0.9,edgesize = 4, edges.min=1, label = T,
            cluster = "none"))



################################################################################
############################# Co-Citation Analysis #############################
################################################################################

CC <- cocMatrix(N, Field = "CR", type = "sparse", sep = ";")
CC_mat <- crossprod(CC, CC)

CC_mat <- CC_mat[nchar(colnames(CC_mat)) != 0, nchar(colnames(CC_mat)) != 0]

CC_mat_labels <- colnames(CC_mat)
LABEL <- removeDuplicatedlabels(CC_mat_labels)
colnames(CC_mat) <- rownames(CC_mat) <-  LABEL

set.seed(1001)
net=netPlot(CC_mat, n = 50, Title = "Co-Citation Network Total louvain", 
            type = "auto", size.cex=TRUE, size=20, 
            remove.multiple=FALSE, labelsize=0.9,edgesize = 5, edges.min=1, label = T,
            cluster = "louvain")

# cluster is a character. It indicates the type of cluster to perform among 
#("none", "optimal", "louvain","leiden", "infomap","edge_betweenness","walktrap", 
#"spinglass", "leading_eigen", "fast_greedy")

################################### Period 1  ##################################
# n_1p <- metaTagExtraction(n_1p, Field = "CR_AU", sep = ";" )
CC1 <- cocMatrix(n_1p, Field = "CR", type = "sparse", sep = ";")
CC1_mat <- crossprod(CC1, CC1)

CC1_mat <- CC1_mat[nchar(colnames(CC1_mat)) != 0, nchar(colnames(CC1_mat)) != 0]

CC1_mat_labels <- colnames(CC1_mat)
LABEL <- removeDuplicatedlabels(CC1_mat_labels)
colnames(CC1_mat) <- rownames(CC1_mat) <-  LABEL

set.seed(1001)
net=netPlot(CC1_mat, n = 35, Title = "Co-Citation Network,  2000 - 2007", # Louvain
                type = "auto", size.cex=TRUE, size=20, 
                remove.multiple=FALSE, labelsize=0.9,edgesize = 4, edges.min=1, label = T,
            cluster = "optimal", curved = F, )



co_cite1 <- head(net$cluster_res,30)


################################### Period 2  ##################################

CC2 <- cocMatrix(n_2p, Field = "CR", type = "sparse", sep = ";")
CC2_mat <- crossprod(CC2, CC2)

CC2_mat <- CC2_mat[nchar(colnames(CC2_mat)) != 0, nchar(colnames(CC2_mat)) != 0]


CC2_mat_labels <- colnames(CC2_mat)
LABEL <- removeDuplicatedlabels(CC2_mat_labels)
colnames(CC2_mat) <- rownames(CC2_mat) <-  LABEL

set.seed(1002)
net=netPlot(CC2_mat, n = 30, Title = "Co-Citation Network, 2008 - 2015", 
            type = "fruchterman", size.cex=TRUE, size=20, 
            remove.multiple=FALSE, labelsize=0.9,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain")

co_cite2 <- head(net$cluster_res,30)

################################### Period 3  ##################################

CC3 <- cocMatrix(n_3p, Field = "CR", type = "sparse", sep = ";")
CC3_mat <- crossprod(CC3, CC3)

CC3_mat <- CC3_mat[nchar(colnames(CC3_mat)) != 0, nchar(colnames(CC3_mat)) != 0]

CC3_mat_labels <- colnames(CC3_mat)
LABEL <- removeDuplicatedlabels(CC3_mat_labels)
colnames(CC3_mat) <- rownames(CC3_mat) <-  LABEL

set.seed(1003)
net=netPlot(CC3_mat, n = 30, Title = "Co-Citation Network P3 louvain", 
            type = "auto", size.cex=TRUE, size=20, label.color = F,
            remove.multiple=FALSE, labelsize=0.7,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain")

co_cite3 <- head(net$cluster_res,30)


################################################################################
############################# Bibliographic Coupling ###########################
################################################################################


################################### Period 1  ##################################
BC1 <- Matrix::t(cocMatrix(n_1p, Field = "CR", type = "sparse", sep = ";"))
BC1_mat <- crossprod(BC1, BC1)

BC1_mat <- BC1_mat[nchar(colnames(BC1_mat)) != 0, nchar(colnames(BC1_mat)) != 0]

BC1_mat_labels <- colnames(BC1_mat)
AU <- # creates a AU column
  trimws(gsub("[ ]{2,}", # extract the first string 2 spaces to separate the last name 
              "", # the names are place after the last name space
              (gsub("\\.", 
                    "", 
                    unlist(lapply(strsplit(BC1_mat_labels, 
                                           ',', # split the strings by "," separator 
                                           fixed = TRUE), 
                                  '[', 1)))))) # extract the first string of the array

PY <- # creates a AU column
  trimws(gsub("[ ]{2,}", # extract the first string 2 spaces to separate the last name 
              "", # the names are place after the last name space
              (gsub("\\.", 
                    "", 
                    unlist(lapply(strsplit(BC1_mat_labels, 
                                           ',', # split the strings by "," separator 
                                           fixed = TRUE), 
                                  '[', 2)))))) # extract the first string of the array
LABEL <- paste(str_to_title(AU), PY, sep = ", ")
LABEL <- removeDuplicatedlabels(LABEL)

colnames(BC1_mat) <- rownames(BC1_mat) <-  LABEL

set.seed(1001)
net=netPlot(BC1_mat, n = 30, Title = "Bibliographic Copling P1 louvain", 
            type = "auto", size.cex=TRUE, size=20, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain", remove.isolates = T)

head(net$cluster_res,30)


################################### Period 2  ##################################
BC2 <- Matrix::t(cocMatrix(n_2p, Field = "CR", type = "sparse", sep = ";"))
BC2_mat <- crossprod(BC2, BC2)

BC2_mat <- BC2_mat[nchar(colnames(BC2_mat)) != 0, nchar(colnames(BC2_mat)) != 0]

BC2_mat_labels <- colnames(BC2_mat)
AU <- # creates a AU column
  trimws(gsub("[ ]{2,}", # extract the first string 2 spaces to separate the last name 
              "", # the names are place after the last name space
              (gsub("\\.", 
                    "", 
                    unlist(lapply(strsplit(BC2_mat_labels, 
                                           ',', # split the strings by "," separator 
                                           fixed = TRUE), 
                                  '[', 1)))))) # extract the first string of the array

PY <- # creates a AU column
  trimws(gsub("[ ]{2,}", # extract the first string 2 spaces to separate the last name 
              "", # the names are place after the last name space
              (gsub("\\.", 
                    "", 
                    unlist(lapply(strsplit(BC2_mat_labels, 
                                           ',', # split the strings by "," separator 
                                           fixed = TRUE), 
                                  '[', 2)))))) # extract the first string of the array
LABEL <- paste(str_to_title(AU), PY, sep = ", ")
LABEL <- removeDuplicatedlabels(LABEL)

colnames(BC2_mat) <- rownames(BC2_mat) <-  LABEL

set.seed(1001)
net=netPlot(BC2_mat, n = 35, Title = "Bibliographic Copling P2 louvain", 
            type = "auto", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 3, edges.min=1, label = T,
            cluster = "optimal")

head(net$cluster_res,30)


################################### Period 3  ##################################
BC3 <- Matrix::t(cocMatrix(n_3p, Field = "CR", type = "sparse", sep = ";"))
BC3_mat <- crossprod(BC3, BC3)

BC3_mat <- BC3_mat[nchar(colnames(BC3_mat)) != 0, nchar(colnames(BC3_mat)) != 0]

BC3_mat_labels <- colnames(BC3_mat)
AU <- # creates a AU column
  trimws(gsub("[ ]{2,}", # extract the first string 2 spaces to separate the last name 
              "", # the names are place after the last name space
              (gsub("\\.", 
                    "", 
                    unlist(lapply(strsplit(BC3_mat_labels, 
                                           ',', # split the strings by "," separator 
                                           fixed = TRUE), 
                                  '[', 1)))))) # extract the first string of the array

PY <- # creates a AU column
  trimws(gsub("[ ]{2,}", # extract the first string 2 spaces to separate the last name 
              "", # the names are place after the last name space
              (gsub("\\.", 
                    "", 
                    unlist(lapply(strsplit(BC3_mat_labels, 
                                           ',', # split the strings by "," separator 
                                           fixed = TRUE), 
                                  '[', 2)))))) # extract the first string of the array
LABEL <- paste(str_to_title(AU), PY, sep = ", ")
LABEL <- removeDuplicatedlabels(LABEL)

colnames(BC3_mat) <- rownames(BC3_mat) <-  LABEL

set.seed(1001)
net=netPlot(BC3_mat, n = 20, Title = "Bibliographic Copling louvain", 
            type = "fruchterman", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 3, edges.min=1, label = T,
            cluster = "louvain")

head(net$cluster_res,30)


################################################################################
############################# Conceptual Structure #############################
################################################################################

rem <- c("Digital Divide", "Divide", "Paper")

WCo1 <- cocMatrix(n_1p, Field = "DE", type = "sparse", sep = ";", remove.terms = rem)
# WCo1 <- cocMat(n_1p, Field = "ID", type = "sparse", sep = ";", remove.terms = rem)

WCo1_mat <- crossprod(WCo1, WCo1)

WCo1_mat <- WCo1_mat[nchar(colnames(WCo1_mat)) != 0, nchar(colnames(WCo1_mat)) != 0]

WCo1_mat_labels <- colnames(WCo1_mat)

net=netPlot(WCo1_mat, n = 30, Title = "Co-Word Network 1P louvain", 
            type = "fruchterman", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain")



WCo2 <- cocMat(n_2p, Field = "DE", type = "sparse", sep = ";")

WCo2_mat <- crossprod(WCo2, WCo2)

WCo2_mat <- WCo2_mat[nchar(colnames(WCo2_mat)) != 0, nchar(colnames(WCo2_mat)) != 0]

WCo2_mat_labels <- colnames(WCo2_mat)

net=netPlot(WCo2_mat, n = 30, Title = "Co-Word Network 2P louvain", 
            type = "fruchterman", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 3, edges.min=5, label = T,
            cluster = "louvain")


WCo3 <- cocMat(n_3p, Field = "DE", type = "sparse", sep = ";", remove.terms = rem)

WCo3_mat <- crossprod(WCo3, WCo3)

WCo3_mat <- WCo3_mat[nchar(colnames(WCo3_mat)) != 0, nchar(colnames(WCo3_mat)) != 0]

WCo3_mat_labels <- colnames(WCo3_mat)

net=netPlot(WCo3_mat, n = 30, Title = "Co-Word Network 3P louvain", 
            type = "fruchterman", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain")


################################################################################
################################# Collaboration  ###############################
################################################################################

################################### Total ######################################






COLco1 <- cocMat(n_3p, Field = "AU_CO", type = "sparse", sep = ";")


COLco1_mat <- crossprod(COLco1, COLco1)

COLco1_mat <- COLco1_mat[nchar(colnames(COLco1_mat)) != 0, nchar(colnames(COLco1_mat)) != 0]

COLco1_mat_labels <- colnames(COLco1_mat)

net=netPlot(COLco1_mat, n = 20, Title = "Country Collaboration louvain", 
            type = "circle", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 3, edges.min=1, label = T,
            cluster = "louvain")





COLun1 <- cocMat(N, Field = "AU_UN", type = "sparse", sep = ";")


COLun1_mat <- crossprod(COLun1, COLun1)

COLun1_mat <- COLun1_mat[nchar(colnames(COLun1_mat)) != 0, nchar(colnames(COLun1_mat)) != 0]

COLun1_mat_labels <- colnames(COLun1_mat)

net=netPlot(COLun1_mat, n = 20, Title = "University Collaborations louvain", 
            type = "circle", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 3, edges.min=1, label = T,
            cluster = "louvain")



CCcorp <- cocMatrix(corp_dd_lit, Field = "CR", type = "sparse", sep = ";")
CCcorp_mat <- crossprod(CCcorp, CCcorp)

CCcorp_mat <- CCcorp_mat[nchar(colnames(CCcorp_mat)) != 0, nchar(colnames(CCcorp_mat)) != 0]

CCcorp_mat_labels <- colnames(CCcorp_mat)
LABEL <- removeDuplicatedlabels(CCcorp_mat_labels)
colnames(CCcorp_mat) <- rownames(CCcorp_mat) <-  LABEL

set.seed(1001)
net=netPlot(CCcorp_mat, n = 20, Title = "Co-Citation Network,  2000 - 2007", # Louvain
            type = "auto", size.cex=TRUE, size=20, 
            remove.multiple=FALSE, labelsize=0.9,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain", curved = F, remove.isolates = T )




