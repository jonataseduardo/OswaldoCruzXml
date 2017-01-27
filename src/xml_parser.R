library(data.table)
library(xml2)

help(package = 'xml2')

list_with_na <-
    function(L)
      unlist(lapply(L, function(x) ifelse(is.null(x), NA, x)))

list_no_gap <-
    function(L, gap_key){
      Lout <- unlist(lapply(L, function(x) ifelse(is.null(x), NA, x)))
      Lout[is.na(Lout)] <- gap_key
      Lout
    }

rep_name <-
  function(name_list, num_reps)
      unlist(mapply(rep, name_list, num_reps))

xml <- read_xml("../data/CD002010RawData.xml")


l_name <- 
  xml_find_all(xml, "//NAME")

path_name <- 
  xml_path(l_name)


## RD_COMP
rd_comp <-
  xml_find_all(xml, "//RD_COMP")

p_rd_comp <- 
  xml_path(rd_comp)

DT_comp <-  data.table(idx = 1:length(p_rd_comp))

DT_comp[, 
         c('root', 'review', 'raw_data', 'rd_comp') := tstrsplit(p_rd_comp, "/")]

DT_comp[, c('idx', 'root') := NULL]

attr_comp <- 
  sort(unique(unlist(lapply(xml_attrs(rd_comp), names))))

trash <- 
  lapply(attr_comp, 
         function(attr_col){ 
           values <- xml_attr(rd_comp, attr_col)
           DT_comp[, c(attr_col) := values]
           NULL
         })

DT_comp


## RD_OUT
rd_out <-
  xml_find_all(xml, "//RD_OUT")

p_rd_out <- 
  xml_path(rd_out)

DT_out <-  data.table(idx = 1:length(p_rd_out))

DT_out[, 
         c('root', 'review', 'raw_data', 'rd_comp', 'rd_out') := tstrsplit(p_rd_out, "/")]

DT_out[, c('idx', 'root') := NULL]

attr_out <- 
  sort(unique(unlist(lapply(xml_attrs(rd_out), names))))

trash <- 
  lapply(attr_out, 
         function(attr_col){ 
           values <- xml_attr(rd_out, attr_col)
           DT_out[, c(attr_col) := values]
           NULL
         })

DT_out


## RD_SUB
rd_sub <-
  xml_find_all(xml, "//RD_SUB")

p_rd_sub <- 
  xml_path(rd_sub)

DT_sub <-  data.table(idx = 1:length(p_rd_sub))

DT_sub[, 
         c('root', 'review', 'raw_sub', 'rd_comp', 'rd_out', 'rd_sub') := tstrsplit(p_rd_sub, "/")]

DT_sub[, c('idx', 'root') := NULL]

attr_sub <- 
  sort(unique(unlist(lapply(xml_attrs(rd_sub), names))))

trash <- 
  lapply(attr_sub, 
         function(attr_col){ 
           values <- xml_attr(rd_sub, attr_col)
           DT_sub[, c(attr_col) := values]
           NULL
         })

DT_sub


## RD_DATA
rd_data <-
  xml_find_all(xml, "//RD_DATA")

p_rd_data <- 
  xml_path(rd_data)

DT_data <-  data.table(idx = 1:length(p_rd_data))

DT_data[, 
         c('root', 'review', 'raw_data', 'rd_comp', 'rd_out', 'rd_sub', 'rd_data') := tstrsplit(p_rd_data, "/")]

DT_data[, c('idx', 'root') := NULL]

DT_data

attr_data <- 
  sort(unique(unlist(lapply(xml_attrs(rd_data), names))))

trash <- 
  lapply(attr_data, 
         function(attr_col){ 
           values <- xml_attr(rd_data, attr_col)
           DT_data[, c(attr_col) := values]
           NULL
         })



grplabel1 <-
  xml_find_all(xml, "//GRPLABEL1")

grplabel2 <-
  xml_find_all(xml, "//GRPLABEL2")

glabel1 <-
  xml_find_all(xml, "//GLABEL1")

glabel2 <-
  xml_find_all(xml, "//GLABEL2")
