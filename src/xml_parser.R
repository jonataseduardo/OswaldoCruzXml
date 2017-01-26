library(data.table)
library(xml2)

help(package = 'xml2')
?xml_serialize


list_with_na <-
    lapply(L, function(x) ifelse(is.null(x), NA, x))

raw_xml <- 
  read_xml("../data/CD003407RawData.xml")

RD_DATA <- xml_find_all(raw_xml, "//RD_DATA")
RD_OUT <- xml_find_all(raw_xml, "//RD_OUT")


p_RD_COMP <- "RD_COMP\\[([0-9]+)\\]"
p_RD_OUT <- "RD_OUT\\[([0-9]+)\\]"
p_RD_SUB <- "RD_SUB\\[([0-9]+)\\]"
p_RD_DATA <- "RD_DATA\\[([0-9]+)\\]$"

path_RD_DATA <- xml_path(RD_DATA)
path_RD_DATA[1:50]

r_comp <- gregexpr(p_RD_COMP, path_RD_DATA)
l_comp <- regmatches(path_RD_DATA, r0)

r_out <- gregexpr(p_RD_OUT, path_RD_DATA)
l_out <- regmatches(path_RD_DATA, r2)

r_sub <- gregexpr(p_RD_SUB, path_RD_DATA)
l_sub <- regmatches(path_RD_DATA, r2)

r_data <- gregexpr(p_RD_DATA, path_RD_DATA)
l_data <- regmatches(path_RD_DATA, r1)

xml_data <- 
  data.table(RD_COMP = list
             RD_OUT = unlist(L2), 
             RD_data = list_with_na(L1))

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

tn
length(tn)
text_NAME <- tn[tn != ""]

rep_sizes <- 
  xml_data[, .N, by = RD_OUT][, N]

NAME_list <- 
  unlist(
    mapply(rep, 
           text_NAME[2:length(text_NAME)], 
           rep_sizes)
    )

length(NAME_list)
xml_data[, NAME := NAME_list]
xml_data[, TITLE := xml_text(xml_find_all(db, "//TITLE"))]
xml_data
