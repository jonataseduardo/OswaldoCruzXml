library(data.table)
library(xml2)

help(package = 'xml2')


list_with_na <-
    function(L)
      unlist(lapply(L, function(x) ifelse(is.null(x), NA, x)))

rep_name <-
  function(name_list, num_reps)
    unlist(mapply(rep, name_list, num_reps))

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


text_comp <- xml_text(rd_comp)
text_out <- xml_text(rd_out)
text_sub <- xml_text(rd_sub)

l_NAME_COMP <- rep_name(text_comp, xml_data[, .N, by = RD_COMP][,N])
xml_data[, NAME_COMP := l_NAME_COMP]

l_NAME_OUT <- rep_name(text_out, xml_data[, .N, by = .(RD_COMP, RD_OUT)][,N])
xml_data[, NAME_OUT := l_NAME_OUT]

l_NAME_SUB <- rep_name(text_sub, xml_data[, .N, by = .(RD_COMP, RD_OUT, RD_SUB)][,N])
xml_data[, NAME_SUB := l_NAME_SUB]

attr_list <- 
  sort(unique(unlist(lapply(xml_attrs(raw_rd_data), names))))

trash <- 
  lapply(attr_list, 
         function(attr_col){ 
           xml_data[, c(attr_col) := xml_attr(raw_rd_data, attr_col), 
                    with = FALSE]
           NULL
         })

xml_data
