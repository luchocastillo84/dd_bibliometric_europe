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
library(rstudioapi)

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

coutries_clus <- list(c("Cluster 1"),
                      c("Cluster 2"),
                      c("Cluster 3"))

set.seed(40993)
col_CO <- invisible(net_CO <- netPlot(CC_CO_mat, n = 40, Title = "Country Collaboration Network", 
            type = "fruchterman", size.cex=TRUE, size=20, 
            remove.multiple=FALSE, labelsize=0.9,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain", remove.isolates = T,
            legend.position = "bottomright", legend.keywords = coutries_clus))



################################################################################
############################# Co-Citation Analysis #############################
################################################################################

CC <- cocMatrix(N, Field = "CR", type = "sparse", sep = ";")
CC_mat <- crossprod(CC, CC)

CC_mat <- CC_mat[nchar(colnames(CC_mat)) != 0, nchar(colnames(CC_mat)) != 0]

CC_mat_labels <- colnames(CC_mat)
LABEL <- removeDuplicatedLabels(trimws(CC_mat_labels))
colnames(CC_mat) <- rownames(CC_mat) <-  LABEL

set.seed(1001)
net=netPlot(CC_mat, Title = "Co-Citation Network Total louvain", 
            type = "auto", size.cex=TRUE, size=20, 
            labelsize=0.9,edgesize = 5, edges.min=1, label = T,
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
LABEL <- removeDuplicatedLabels(trimws(CC1_mat_labels))

colnames(CC1_mat) <- rownames(CC1_mat) <-  LABEL

keywords1 <- list(c("1. Global DD", "Access", "Civic Engagement"),
                 c("2. Skills", "Usage", "Complexities"),
                 c("3. Methodology", "Measurment", "Social Inclusion"),
                 c("4. Network Society", "Sociology", "Technology Effects"))
keywords0 <- c("")
set.seed(1001)
net=netPlot(CC1_mat, n= 100, Title = "Co-Citation Network,  2000 - 2007", # Louvain
                type = "fruchterman", size.cex=TRUE, size=20, 
                remove.multiple= FALSE, labelsize=0.9,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain", curved = F,
            legend.position = "bottomleft", legend.keywords = keywords0)



co_cite1 <- head(net$cluster_res,35)

co_cite1 <- data.frame(net$cluster_res)
groups1 <- data.frame(cluster = net$community_obj[[3]])
degree1 <- net$nodeDegree

co_cite_df <- inner_join(co_cite1, degree1, by= c("vertex"= "node"))
co_cite_df <- co_cite_df[, -c(4,5)]
write_csv(co_cite_df, file = here("Output", 
                         "Data", 
                         "co_ci1.csv")) 



################################### Period 2  ##################################

CC2 <- cocMatrix(n_2p, Field = "CR", type = "sparse", sep = ";")
CC2_mat <- crossprod(CC2, CC2)

CC2_mat <- CC2_mat[nchar(colnames(CC2_mat)) != 0, nchar(colnames(CC2_mat)) != 0]


CC2_mat_labels <- colnames(CC2_mat)
LABEL <- removeDuplicatedLabels(trimws(CC2_mat_labels))
colnames(CC2_mat) <- rownames(CC2_mat) <-  LABEL

keywords2 <- list(c("1. Global DD", "Access", "Civic Engagement", "Complexities"),
                  c("2. Skills", "Usage", "Age Groups", "DD Shortcomings"),
                  c("3. Cross-Country", "Regional", "Access", "Measurment"))

set.seed(100211)
net=netPlot(CC2_mat, n = 30, Title = "Co-Citation Network, 2008 - 2015", 
            type = "fruchterman", size.cex=TRUE, size=20, 
            remove.multiple=FALSE, labelsize=0.9,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain",
            legend.position = "bottomleft", legend.keywords = keywords2)

co_cite2 <- head(net$cluster_res,40)

co_cite2 <- data.frame(net$cluster_res)
groups2 <- data.frame(cluster = net$community_obj[[3]])
degree2 <- net$nodeDegree

co_cite_df2 <- inner_join(co_cite2, degree2, by= c("vertex"= "node"))
co_cite_df2 <- co_cite_df2[, -c(4,5)]
write_csv(co_cite_df2, file = here("Output", 
                                  "Data", 
                                  "co_ci2.csv")) 

################################### Period 3  ##################################

CC3 <- cocMatrix(n_3p, Field = "CR", type = "sparse", sep = ";")
CC3_mat <- crossprod(CC3, CC3)

CC3_mat <- CC3_mat[nchar(colnames(CC3_mat)) != 0, nchar(colnames(CC3_mat)) != 0]

CC3_mat_labels <- colnames(CC3_mat)
LABEL <- removeDuplicatedLabels(trimws(CC3_mat_labels))
colnames(CC3_mat) <- rownames(CC3_mat) <-  LABEL


keywords3 <- list(c( "1. Skills", "Usage", "Returns", "Inequalities"),
                  c("2. Global DD", "Access", "Civic Engagement", "Complexities"),
                  c("3. Age Groups", "Knowledge Gaps", "Skills"))

set.seed(1003)
net=netPlot(CC3_mat, n = 30, Title = "Co-Citation Network,  2016 - 2022", 
            type = "fruchterman", size.cex=TRUE, size=20, label.color = F,
            remove.multiple=FALSE, labelsize=0.7,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain", legend.position = "bottomright", 
            legend.keywords = keywords3)

co_cite3 <- head(net$cluster_res,40)
co_cite3 <- data.frame(net$cluster_res)
groups3 <- data.frame(cluster = net$community_obj[[3]])
degree3 <- net$nodeDegree

co_cite_df3 <- inner_join(co_cite3, degree3, by= c("vertex"= "node"))
co_cite_df3 <- co_cite_df3[, -c(4,5)]
write_csv(co_cite_df3, file = here("Output", 
                                   "Data", 
                                   "co_ci3.csv"))


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
LABEL <- removeDuplicatedLabels(trimws(LABEL))

colnames(BC1_mat) <- rownames(BC1_mat) <-  LABEL

keywords4 <- list(c( "1. Methodology", "Political", "Urban vs Rural", "Inequalities"),
                  c("2. Theories", "Shortcoming", "Age", "Socio-economic"),
                  c("3. Multidimentional", "Digital gaps", "Locations"))

set.seed(9001)
net=netPlot(BC1_mat, n = 30, Title = "Bibliographic Coupling Network, 2000-2007", 
            type = "fruchterman", size.cex=TRUE, size=20, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain", remove.isolates = T, legend.position = "bottomright", 
            legend.keywords = keywords4)



bibco1 <- head(net$cluster_res,40)
bibco1 <- data.frame(net$cluster_res)
groups3 <- data.frame(cluster = net$community_obj[[3]])
degree_bc1 <- net$nodeDegree

bibco_df1 <- inner_join(bibco1, degree_bc1, by= c("vertex"= "node"))
bibco_df1 <- bibco_df1[, -c(4,5)]
bibco_df1$vertex <- trimws(bibco_df1$vertex)
# write_csv(bibco_df1, file = here("Output", 
#                                    "Data", 
#                                    "bc1.csv"))


n_1pAU<- # adds a SO column
  str_to_title(trimws(unlist(lapply(strsplit(n_1p$SRDI,
                                ', DOI', # separated by comma
                                fixed = TRUE),
                       '[', 1)))) # extract the third string in the array

n_1p <- cbind(n_1p, LABEL)

bibco_df1 <- inner_join(bibco_df1, n_1p[ , c(31, 2, 8)], by= c("vertex" = "LABEL"))

bibco_df1 <- bibco_df1[, c(1, 5, 2:4, 6)]

write_csv(bibco_df1, file = here("Output", 
                                 "Data", 
                                 "bc1.csv"))


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
LABEL <- removeDuplicatedLabels(trimws(LABEL))

colnames(BC2_mat) <- rownames(BC2_mat) <-  LABEL

keywords5 <- list(c( "1. Cross-country", "Measures", "Index", "Trade-offs"),
                  c("2. Education", "Social", "Cultural", "Dimentions"),
                  c("3. Digital skills", "Usage", "Demographic groups", "Netherlands"))

set.seed(90001)
net=netPlot(BC2_mat, n = 30, Title = "Bibliographic Coupling Network, 2008-2015", 
            type = "fruchterman", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain", legend.position = "topleft", 
            legend.keywords = keywords5)

bibco2 <- head(net$cluster_res,40)
bibco2 <- data.frame(net$cluster_res)
groups2 <- data.frame(cluster = net$community_obj[[3]])
degree_bc2 <- net$nodeDegree

bibco_df2 <- inner_join(bibco2, degree_bc2, by= c("vertex"= "node"))
bibco_df2 <- bibco_df2[, -c(4,5)]
bibco_df2$vertex <- trimws(bibco_df2$vertex)
# write_csv(bibco_df2, file = here("Output", 
#                                    "Data", 
#                                    "bc2.csv"))


n_2pAU<- # adds a SO column
  str_to_title(trimws(unlist(lapply(strsplit(n_2p$SRDI,
                                             ', DOI', # separated by comma
                                             fixed = TRUE),
                                    '[', 1)))) # extract the third string in the array

n_2p <- cbind(n_2p, LABEL)

bibco_df2 <- left_join(bibco_df2, n_2p[ , c(31, 2, 8)], by= c("vertex" = "LABEL"))

bibco_df2 <- bibco_df2[, c(1, 5, 2:4, 6)]

write_csv(bibco_df2, file = here("Output", 
                                 "Data", 
                                 "bc02.csv"))

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
LABEL <- removeDuplicatedLabels(trimws(LABEL))

colnames(BC3_mat) <- rownames(BC3_mat) <-  LABEL

keywords6 <- list(c( "1. Empirical research", "1&2-DD levels", "Digital capital"),
                  c("2. Country level", "3 DD level", "Digital skills", "Internet Usage"),
                  c("3. van Dijk framework", "test", "types of access"))

set.seed(1001)
net=netPlot(BC3_mat, n = 30, Title = "Bibliographic Coupling Network, 2016-2022", 
            type = "fruchterman", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 3, edges.min=1, label = T,
            cluster = "louvain", legend.position = "bottomright", 
            legend.keywords = keywords6)

bibco3 <- head(net$cluster_res,40)
bibco3 <- data.frame(net$cluster_res)
groups3 <- data.frame(cluster = net$community_obj[[3]])
degree_bc3 <- net$nodeDegree

bibco_df3 <- inner_join(bibco3, degree_bc3, by= c("vertex"= "node"))
bibco_df3 <- bibco_df3[, -c(4,5)]
bibco_df3$vertex <- trimws(bibco_df3$vertex)
# write_csv(bibco_df3, file = here("Output", 
#                                    "Data", 
#                                    "bc3.csv"))


n_3pAU<- # adds a SO column
  str_to_title(trimws(unlist(lapply(strsplit(n_3p$SRDI,
                                             ', DOI', # separated by comma
                                             fixed = TRUE),
                                    '[', 1)))) # extract the third string in the array

n_3p <- cbind(n_3p, LABEL)


bibco_df3 <- inner_join(bibco_df3, n_3p[ , c(31, 2, 8)], by= c("vertex" = "LABEL"))

bibco_df3 <- bibco_df3[, c(1, 5, 2:4, 6)]

write_csv(bibco_df3, file = here("Output", 
                                 "Data", 
                                 "bc3.csv"))

################################################################################
############################# Conceptual Structure #############################
################################################################################

rem <- c("Digital Divide", "Divide", "Paper", "Need", "Way", "Research", "Paper",
         "Analysis", "Findings", "Results", "Study","Group", "Project", "View", "Area",
         "Values", "Purpose", "Originality/Value", "Article", "Model", "Approach","Years",
         "Terms", "Information", "Technology", "Communication Technologies", "Internet",
         "Time")

################################### Total  #####################################
WCo <- cocMatrix(N, Field = "DE", type = "sparse", sep = ";", remove.terms = rem)
# WCo1 <- cocMat(n_1p, Field = "ID", type = "sparse", sep = ";", remove.terms = rem)

WCo_mat <- crossprod(WCo, WCo)

WCo_mat <- WCo_mat[nchar(colnames(WCo_mat)) != 0, nchar(colnames(WCo_mat)) != 0]

WCo_mat_labels <- colnames(WCo_mat)

net=netPlot(WCo_mat, n = 50, Title = "Co-Word Network 2000-2022", 
            type = "fruchterman", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain")


################################### Period 1  ##################################



WCo1 <- cocMatrix(n_1p, Field = "DE", type = "sparse", sep = ";", remove.terms = rem)
# WCo1 <- cocMat(n_1p, Field = "ID", type = "sparse", sep = ";", remove.terms = rem)

WCo1_mat <- crossprod(WCo1, WCo1)

WCo1_mat <- WCo1_mat[nchar(colnames(WCo1_mat)) != 0, nchar(colnames(WCo1_mat)) != 0]

WCo1_mat_labels <- colnames(WCo1_mat)

net=netPlot(WCo1_mat, n = 40, Title = "Co-Word Network 2000-2007", 
            type = "fruchterman", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain")

################################### Period 2  ##################################


WCo2 <- cocMatrix(n_2p, Field = "DE", type = "sparse", sep = ";", remove.terms = rem)

WCo2_mat <- crossprod(WCo2, WCo2)

WCo2_mat <- WCo2_mat[nchar(colnames(WCo2_mat)) != 0, nchar(colnames(WCo2_mat)) != 0]

WCo2_mat_labels <- colnames(WCo2_mat)

net=netPlot(WCo2_mat, n = 30, Title = "Co-Word Network 2008-2015", 
            type = "fruchterman", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 3, edges.min=5, label = T,
            cluster = "louvain")

################################### Period 3  ##################################


WCo3 <- cocMatrix(n_3p, Field = "DE", type = "sparse", sep = ";", remove.terms = rem)

WCo3_mat <- crossprod(WCo3, WCo3)

WCo3_mat <- WCo3_mat[nchar(colnames(WCo3_mat)) != 0, nchar(colnames(WCo3_mat)) != 0]

WCo3_mat_labels <- colnames(WCo3_mat)

net=netPlot(WCo3_mat, n = 30, Title = "Co-Word Network 2016-2022", 
            type = "fruchterman", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain")


################################################################################
################################# Collaboration  ###############################
################################################################################

################################### Total ######################################






COLco1 <- cocMatrix(n_3p, Field = "AU_CO", type = "sparse", sep = ";")


COLco1_mat <- crossprod(COLco1, COLco1)

COLco1_mat <- COLco1_mat[nchar(colnames(COLco1_mat)) != 0, nchar(colnames(COLco1_mat)) != 0]

COLco1_mat_labels <- colnames(COLco1_mat)

net=netPlot(COLco1_mat, n = 20, Title = "Country Collaboration louvain", 
            type = "circle", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 3, edges.min=1, label = T,
            cluster = "louvain")





COLun1 <- cocMatrix(N, Field = "AU_UN", type = "sparse", sep = ";")


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




