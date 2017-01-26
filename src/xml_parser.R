library(data.table)
library(xml2)


data_xml <- 
  read_xml("../data/CD000004RawData.xml")

RD_DATA <- xml_find_all(db, "//RD_DATA")
RD_OUT <- xml_find_all(db, "//RD_OUT")

p_RD_OUT <- "RD_OUT\\[([0-9]+)\\]"
p_RD_DATA <- "(RD_DATA\\[)([0-9]+)(\\]$)"

path_RD_DATA <- xml_path(RD_DATA)

r1 <- gregexpr(p_RD_DATA, path_RD_DATA)
L1 <-   regmatches(path_RD_DATA, r1)

r2 <- gregexpr(p_RD_OUT, path_RD_DATA)
L2 <-   regmatches(path_RD_DATA, r1)

xml_ns(db)

coln <- "TMEAN"
length(xml_attr(y,coln ))
xml_attr(y,coln )

xml_attr(y, 'STUDY_ID')

x <- 
  xml_find_all(db, "//NAME")
  xml_find_all(db, "//TITLE")
help(package = 'xml2')
xml_attr
xml_path(y)[1]


gsub(p_RD_DATA, "\\1", xml_path(y))


which(is.na(L1))

lapply(unique(unlist(lapply(xml_attrs(y), names))), 
       function(x){print(x) xml_attr(y,x))})

z <-  data.table(RD_OUT = unlist(L2), RD_data = list_with_na(L1))


list_with_na <- 
  function(L1){
     unlist(lapply(L1, function(x) ifelse(is.null(x), NA, x)))
  }

