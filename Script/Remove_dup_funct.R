# 
# biblioNetwork <-
#   function(M,
#            analysis = "coupling",
#            network = "authors",
#            n = NULL,
#            sep = ";",
#            short = FALSE,
#            shortlabel = TRUE,
#            remove.terms = NULL,
#            synonyms = NULL) {
#     crossprod <- Matrix::crossprod
#     NetMatrix <-  NA
#     
#     if (analysis == "coupling") {
#       switch(
#         network,
#         authors = {
#           WA <- cocMatrix(M, Field = "AU", type = "sparse", n,sep,short=short)
#           WCR <- cocMatrix(M, Field = "CR", type = "sparse", n,sep,short=short)
#           CRA <- crossprod(WCR, WA)
#           NetMatrix <- crossprod(CRA, CRA)
#         },
#         references = {
#           WCR <- Matrix::t(cocMatrix(M, Field = "CR", type = "sparse", n,sep,short=short))
#           NetMatrix <- crossprod(WCR, WCR)
#         },
#         sources = {
#           WSO <- cocMatrix(M, Field = "SO", type = "sparse", n, sep,short=short)
#           WCR <- cocMatrix(M, Field = "CR", type = "sparse", n, sep,short=short)
#           CRSO <- crossprod(WCR, WSO)
#           NetMatrix <- crossprod(CRSO, CRSO)
#         },
#         countries = {
#           WCO <- cocMatrix(M, Field = "AU_CO", type = "sparse", n, sep,short=short)
#           WCR <- cocMatrix(M, Field = "CR", type = "sparse", n, sep,short=short)
#           CRCO <- crossprod(WCR, WCO)
#           NetMatrix <- crossprod(CRCO, CRCO)
#         }
#       )
#     }
#     
#     if (analysis == "co-occurrences") {
#       switch(
#         network,
#         authors = {
#           WA <- cocMatrix(M, Field = "AU", type = "sparse", n, sep,short=short)
#         },
#         keywords = {
#           WA <- cocMatrix(M, Field = "ID", type = "sparse", n, sep,short=short, remove.terms=remove.terms, synonyms = synonyms)
#         },
#         author_keywords = {
#           WA <- cocMatrix(M, Field = "DE", type = "sparse", n, sep,short=short, remove.terms=remove.terms, synonyms = synonyms)
#         },
#         titles = {
#           WA <- cocMatrix(M, Field = "TI_TM", type = "sparse", n, sep,short=short, remove.terms=remove.terms, synonyms = synonyms)
#         },
#         abstracts = {
#           WA <- cocMatrix(M, Field = "AB_TM", type = "sparse", n, sep,short=short, remove.terms=remove.terms, synonyms = synonyms)
#         },
#         sources = {
#           WA <- cocMatrix(M, Field = "SO", type = "sparse", n, sep,short=short)
#         }
#       )
#       NetMatrix <- crossprod(WA, WA)
#       
#     }
#     
#     
#     
#     if (analysis == "co-citation") {
#       switch(
#         network,
#         authors = {
#           WA <- cocMatrix(M, Field = "CR_AU", type = "sparse", n, sep,short=short)
#         },
#         references = {
#           WA <- cocMatrix(M, Field = "CR", type = "sparse", n, sep,short=short)
#         },
#         sources = {
#           WA <- cocMatrix(M, Field = "CR_SO", type = "sparse", n, sep,short=short)
#         }
#       )
#       NetMatrix <- crossprod(WA, WA)
#       
#     }
#     
#     if (analysis == "collaboration") {
#       switch(
#         network,
#         authors = {
#           WA <- cocMatrix(M, Field = "AU", type = "sparse", n, sep,short=short)
#           
#         },
#         universities = {
#           WA <- cocMatrix(M, Field = "AU_UN", type = "sparse", n, sep,short=short)
#           
#         },
#         countries = {
#           WA <- cocMatrix(M, Field = "AU_CO", type = "sparse", n, sep,short=short)
#         }
#       )
#       NetMatrix <- crossprod(WA, WA)
#       
#     }
#     
#     
#     # delete empty vertices
#     NetMatrix <- NetMatrix[nchar(colnames(NetMatrix)) != 0, nchar(colnames(NetMatrix)) != 0]
#     
#   
#     return(NetMatrix)
#   }

### shortlabel
labelShort <- function(NET,db="isi"){
  LABEL <- colnames(NET)
  YEAR <- suppressWarnings(as.numeric(sub('.*(\\d{4}).*', '\\1', LABEL)))
  YEAR[is.na(YEAR)] <- ""
  switch(db,
         isi={
           AU <- strsplit(LABEL," ")
           AU <- unlist(lapply(AU, function(l){paste(l[1]," ",l[2],sep="")}))
           LABEL <- paste0(AU, " ", YEAR, sep="")
         },
         scopus={
           AU <- strsplit(LABEL,"\\. ")
           AU <- unlist(lapply(AU, function(l){l[1]}))
           LABEL <- paste0(AU, ". ", YEAR, sep="")
         })
  
  return(LABEL)
}


removeDuplicatedLabels <- function(LABEL){
  ## assign a unique name to each label
  tab <- sort(table(LABEL), decreasing = T)
  dup <- names(tab[tab > 1])
  
  for (i in 1:length(dup)){
    ind <- which(LABEL %in% dup[i])
    
    if (length(ind) > 0){
      # Generate lowercase alphabetic sequence based on the length of 'ind'
      alphabet_seq <- c(letters, paste0(rep(letters, each = 26), letters))
      alphabet_seq <- alphabet_seq[1:length(ind)]
      
      # Replace the original line with the new lowercase alphabetic sequence code
      LABEL[ind] <- paste0(LABEL[ind], "-", alphabet_seq, sep = "")
    }
  }
  
  return(LABEL)
}



# removeDuplicatedLabels <- function(LABEL){
#   ## assign a unique name to each label
#   tab <- sort(table(LABEL), decreasing = T)
#   dup <- names(tab[tab > 1])
#   
#   for (i in 1:length(dup)){
#     ind <- which(LABEL %in% dup[i])
#     
#     if (length(ind) > 0){
#       # Generate alphabetic sequence based on the length of 'ind'
#       alphabet_seq <- c(LETTERS, paste0(rep(LETTERS, each = 26), LETTERS))
#       alphabet_seq <- alphabet_seq[1:length(ind)]
#       
#       # Replace the original line with the new alphabetic sequence code
#       LABEL[ind] <- paste0(LABEL[ind], "-", alphabet_seq, sep = "")
#     }
#   }
#   
#   return(LABEL)
# }



# removeDuplicatedlabels <- function(LABEL){
#   ## assign an unique name to each label
#   tab <- sort(table(LABEL),decreasing=T)
#   dup <- names(tab[tab>1])
#   for (i in 1:length(dup)){
#     ind <- which(LABEL %in% dup[i])
#     if (length(ind)>0){
#       LABEL[ind] <- paste0(LABEL[ind],"-",as.character(1:length(ind)),sep="")
#     }
#   }
#   return(LABEL)
# }