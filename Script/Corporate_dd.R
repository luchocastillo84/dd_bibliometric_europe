library(jsonlite)





# corporate|firms|business   industry|corporate|firms|business
topic <- which(grepl("industry|corporate|firms|business| SMEs\\b", MM$TI, ignore.case = T))
i <- topic
toJSON(MM[i, c(2, 1, 3, 17, 14,25, 13, 6, 8, 12, 22, 29, 23)], pretty= T)


corp_dd_lit <- MM[topic,]
corp_dd_lit <- corp_dd_lit[order(corp_dd_lit$PY),]

corp_DD_redux <- corp_dd_lit[, c(13, 12, 1, 2, 8, 23)]
corp_DD_redux <- corp_DD_redux %>% mutate(Author = paste0(AU, " (",PY,") ")) %>% 
  rename(Title = TI, Abstract= AB)
corp_DD_redux$Abstract <- tolower(corp_DD_redux$Abstract)
corp_DD_redux$Author <- str_to_title(corp_DD_redux$Author)
corp_DD_redux$Title <- str_to_title(corp_DD_redux$Title)
toJSON(corp_DD_redux[, c(7, 4, 5)], pretty= T)

CDD_1p <- corp_DD_redux %>% filter(PY < 2008)


CDD_2p <- corp_DD_redux %>% filter(PY >= 2008 & PY <= 2015) # sub-setting the second period



CDD_3p <- corp_DD_redux %>% filter(PY >= 2016 & PY <= 2022) # sub-setting the second period












