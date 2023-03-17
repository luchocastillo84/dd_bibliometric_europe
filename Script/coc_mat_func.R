





cocMat<-function(M, Field = "AU", type = "sparse", n=NULL, sep = ";",binary=TRUE, 
                    short = FALSE, remove.terms = NULL, synonyms = NULL){
  #
  # The function creates co-occurrences data between Works and Field
  #
  # type indicates the output format of co-occurrences:
  #   "matrix" argument generates a W x Field sparse matrix
  #   "sparse" argument generates a compact representation of the matrix (using the package Matrix)
  #    it represents a compact representation of a co-occurrences matrix.
  # Field indicates the ISI Tag
  # if Field is AU -> WA (Works x Authors)
  # if Field is CR -> WR (Works x References)
  # if Field is DE -> WK (Works x Keywords)
  # etc.
  #crossprod <- Matrix::crossprod
  size<-dim(M)
  RowNames <- row.names(M)
  
  ### REMOVE TERMS AND MERGE SYNONYMS
  if (Field %in% c("ID", "DE", "TI", "TI_TM", "AB", "AB_TM")){
    # Crete df with all terms
    
    Fi <- strsplit(M[,Field], sep)
    TERMS <- data.frame(item = trimws(unlist(Fi)), SR = rep(M$SR,lengths(Fi)))
    
    # # Merge synonyms in the vector synonyms
    # if (length(synonyms)>0 & is.character(synonyms)){
    #   s <- strsplit(toupper(synonyms),";")
    #   snew <- trimws(unlist(lapply(s,function(l) l[1])))
    #   sold <- (lapply(s,function(l){
    #     l <- trimws(l[-1])
    #   }))
    #   
    #   for (i in 1:length(s)){
    #     TERMS$item[TERMS$item %in% unlist(sold[[i]])] <- snew[i]
    #   }
    # }
    
    TERMS <- TERMS %>% 
      anti_join(data.frame(item=trimws(toupper(remove.terms))), by="item") %>% 
      mutate(item = trimws(.data$item))
    
    TERMS <- TERMS %>% 
      group_by(.data$SR) %>%
      summarize(item = paste(.data$item, collapse=";"))
    
    M <- M %>% 
      left_join(TERMS, by="SR")
    M[,Field] <- M$item
    
  }
  row.names(M) <- RowNames
  # if (Field=="CR"){M$CR<-gsub("DOI;","DOI ",as.character(M$CR))}
  # 
  # if (Field %in% names(M)){
  #   Fi<-strsplit(M[,Field],sep)} else{return(print(paste("Field",Field,"is not a column name of input data frame")))}
  # Fi<-lapply(Fi,trim.leading)
  # if (Field=="CR"){Fi<-lapply(Fi,function(l) l<-l[nchar(l)>10])}  ## delete not congruent references
  
  ## Scelta dell'informazione contenuta in CR da utilizzare (Reference, Autore, Affiliation, ecc.)
  
  # vector of unique units
  allField <- unlist(Fi)
  allField <- allField[!is.na(allField)]
  
    tabField <- sort(table(allField), decreasing = TRUE)
  uniqueField <- names(tabField)			     
  # select n items
  if (!is.null(n)) {
    uniqueField <- uniqueField[1:n]
  } else if (isTRUE(short)){
    uniqueField <- names(tabField[tabField>1])  # remove items with frequency<2
  }
  
  if (length(uniqueField)<1){
    print("Matrix is empty!!")
    return(NA)
  }
  
  if (type=="matrix" | !isTRUE(binary)){
    # Initialization of WA matrix
    WF<-matrix(0,size[1],length(uniqueField))
  } else if (type=="sparse"){
    WF<-Matrix(0,size[1],length(uniqueField))
  } else {
    print("error in type argument")
    return()
  }
  colnames(WF)<-uniqueField
  rownames(WF)<-rownames(M)
  # Population of WA matrix
  for (i in 1:size[1]){
    if (length(Fi[[i]])>0 & !is.na(Fi[[i]][1])) {
      #print(i)
      #if (Field=="CR"){Fi[[i]]=reduceRefs(Fi[[i]])}
      if (isTRUE(binary)){
        ## binary counting
        ind <- uniqueField %in% Fi[[i]]
        if (sum(ind) > 0){
          WF[i, ind] <- 1
        }
      }
      else{
        ## full counting
        tab=table(Fi[[i]])
        name <- names(tab)[names(tab) %in% uniqueField]
        name <- name[nchar(name)>0]
        if (length(name)>0){
          WF[i,name] <- tab[name]
        }
      }
    }
  }
  
  if (type=="sparse" & !isTRUE(binary)){
    WF <- Matrix(WF)
  }
  
  
  WF <- WF[,!is.na(uniqueField)]
  ind <- which(colnames(WF)=="NA")
  if (length(ind)>0) {WF <- WF[,-ind]}
  
  return(WF)
}


