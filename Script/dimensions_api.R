# load the necessary packages
library(dimensionsR)

token <- dsAuth(key = "CF3CE274AD384E72AE2F1404F3C2F398")

# https://docs.dimensions.ai/dsl/datasource-publications.html
# https://github.com/massimoaria/dimensionsR

query <- dsQueryBuild(item = "publications", 
                      words = c("digital divide*", 
                                "digital inquality*", 
                                "digital gap*"),
                      words_boolean_op = "OR",
                      type = "article ; chapter", 
                      categories = "management; 
                                     economics; 
                                     commerce; 
                                     tourisim and services;
                                     information and computer science",
                      start_year = 1990, 
                      end_year = 2022,
                      output_fields = c("basics", 
                                        "extras", 
                                        "authors", 
                                        "concepts", 
                                        "book_doi",
                                        "abstract",
                                        "isbn",
                                        "reference_ids",
                                        "referenced_pubs",
                                        "categories",
                                        "funders"))


res <- dsApiRequest(token = token, query = query, limit = 0)

res$total_count

D_api <- dsApiRequest(token = token, query = query, step = 700, limit = res$total_count)

D1 <- dsApi2df(D_api)
D[, c(26)] <- lapply(D[, c(26)], tolower)
D1_SO <- D1 %>% select(DI, SO, C1, DE, ID, RP, SC, FU)
D1_SO <- D1_SO %>% replace_with_na_all(condition = ~. == "") # converting "" into NA within the D
D1_SO <- D1_SO %>% replace_with_na_all(condition = ~. == "NA,NA") # converting "" into NA within the D

vis_miss(D1_SO)
D1_D <- left_join(D, D1_SO, by= "DI", keep= F)
vis_miss(D1_D)
