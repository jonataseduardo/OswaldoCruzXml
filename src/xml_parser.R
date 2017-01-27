library(data.table)
library(xml2)

help(package = 'xml2')
?xml_serialize


list_with_na <-
    function(L)
      lapply(L, function(x) ifelse(is.null(x), NA, x))

raw_xml <- 
  read_xml("../data/CD001820RawData.xml")

raw_rd_data <- 
  xml_find_all(raw_xml, "//RD_DATA")

raw_rd_data

path_rd_data <- 
  xml_path(raw_rd_data)

path_rd_data

rd_comp <-
  xml_find_all(raw_xml, "//RD_COMP")

rd_out <-
  xml_find_all(raw_xml, "//RD_OUT")

rd_sub <-
  xml_find_all(raw_xml, "//RD_SUB")

rd_data <-
  xml_find_all(raw_xml, "//RD_DATA")

xml_text(rd_out)


r_comp <- gregexpr("RD_COMP\\[([0-9]+)\\]", path_rd_data)
l_comp <- regmatches(path_rd_data, r_comp)

r_out <- gregexpr("RD_OUT\\[([0-9]+)\\]" , path_rd_data)
l_out <- regmatches(path_rd_data, r_out)

r_sub <- gregexpr("RD_SUB\\[([0-9]+)\\]", path_rd_data)
l_sub <- regmatches(path_rd_data, r_sub)

r_data <- gregexpr("RD_DATA\\[([0-9]+)\\]$", path_rd_data)
l_data <- regmatches(path_rd_data, r_data)

xml_data <- 
  data.table(RD_COMP = list_with_na(l_comp),
             RD_OUT = list_with_na(l_out), 
             RD_SUB = list_with_na(l_sub),
             RD_data = list_with_na(l_data))

xml_data

attr_list <- 
  sort(unique(unlist(lapply(xml_attrs(raw_rd_data), names))))

length(attr_list)

xml_data <- data.table()
trash <- 
  lapply(attr_list, 
         function(attr_col){ 
           xml_data[, c(attr_col) := xml_attr(raw_rd_data, attr_col), 
                    with = FALSE]
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
