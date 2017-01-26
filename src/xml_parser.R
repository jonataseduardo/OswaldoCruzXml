library(data.table)
library(xml2)

help(package = 'xml2')

raw_xml <- 
  read_xml("../data/CD000004RawData.xml")

RD_DATA <- xml_find_all(raw_xml, "//RD_DATA")
RD_OUT <- xml_find_all(raw_xml, "//RD_OUT")

p_RD_OUT <- "RD_OUT\\[([0-9]+)\\]"
p_RD_DATA <- "(RD_DATA\\[)([0-9]+)(\\]$)"

path_RD_DATA <- xml_path(RD_DATA)

r1 <- gregexpr(p_RD_DATA, path_RD_DATA)
L1 <-   regmatches(path_RD_DATA, r1)

r2 <- gregexpr(p_RD_OUT, path_RD_DATA)
L2 <-   regmatches(path_RD_DATA, r2)

xml_data <- 
  data.table(RD_OUT = unlist(L2), RD_data = list_with_na(L1))

xml_data

attr_list <- 
  sort(unique(unlist(lapply(xml_attrs(RD_DATA), names))))

trash <- 
  lapply(attr_list, 
         function(attr_col){ 
           xml_data[, c(attr_col) := xml_attr(RD_DATA, attr_col), with = FALSE]
           NULL
         })

tn <- 
  xml_text(xml_find_all(raw_xml, "//NAME"))

text_NAME <- tn[tn != ""]

rep_sizes <- 
  xml_data[, .N, by = RD_OUT][, N]

NAME_list <- 
  unlist(
    mapply(rep, 
           text_NAME[2:length(text_NAME)], 
           rep_sizes)
    )

xml_data[, NAME := NAME_list]
xml_data

  xml_find_all(db, "//TITLE")
xml_attr
xml_path(y)[1]


gsub(p_RD_DATA, "\\1", xml_path(y))


which(is.na(L1))

lapply(), 
       function(x){print(x) xml_attr(y,x))})



list_with_na <- 
  function(L1){
     unlist(lapply(L1, function(x) ifelse(is.null(x), NA, x)))
  }

