library(data.table)
library(xml2)

#help(package = 'xml2')

xml <- read_xml("../data/CD002010RawData.xml")
xml <- read_xml("../data/CD010388RawData.xml")
#xml <- read_xml("../data/CD003407RawData.xml")

## DATA NAMES
ns_name <- 
  xml_find_all(xml, "//NAME")

p_name <- 
  xml_path(ns_name)

l_name <- 
  xml_text(ns_name)
p_name

DT_name <- data.table(idx = 1:length(l_name))
DT_name[, 
        c('root', 'review', 'raw_data', 'rd_comp', 'rd_out', 'rd_sub', 'name') := tstrsplit(p_name, "/")]

DT_name[, c('idx', 'root', 'review', 'raw_data', 'name') := NULL]
DT_name[rd_out == 'NAME', rd_out := NA]
DT_name[rd_sub == 'NAME', rd_sub := NA]

## set comp name
comp_idx <- grepl("RD_COMP(\\[[0-9]+\\])?/NAME$", p_name)

DP_comp <- data.table(idx = p_name[comp_idx], 
                      NAME_COMP = l_name[comp_idx])
DP_comp[, 
        c('root', 'review', 'raw_data', 'rd_comp', 'name') := tstrsplit(idx, "/")]

DP_comp[, c('idx', 'root', 'review', 'raw_data', 'name') := NULL]

## set out name
out_idx <- grepl("RD_OUT(\\[[0-9]+\\])?/NAME$", p_name)

DP_out <- data.table(idx = p_name[out_idx], 
                      NAME_OUT = l_name[out_idx])
DP_out[, 
        c('root', 'review', 'raw_data', 'rd_comp', 'rd_out', 'name') := tstrsplit(idx, "/")]

DP_out[, c('idx', 'root', 'review', 'raw_data', 'name') := NULL]

## set sub name
sub_idx <- grepl("RD_SUB(\\[[0-9]+\\])?/NAME$", p_name)

DP_sub <- data.table(idx = p_name[sub_idx], 
                      NAME_SUB = l_name[sub_idx])
DP_sub[, 
        c('root', 'review', 'raw_data', 'rd_comp', 'rd_out', 'rd_sub', 'name') := tstrsplit(idx, "/")]

DP_sub[, c('idx', 'root', 'review', 'raw_data', 'name') := NULL]

DT_name <- merge(DT_name, DP_comp, 
                 all = TRUE, by = c('rd_comp'))

DT_name <- merge(DT_name, DP_out, 
                 all = TRUE, by = c('rd_comp', 'rd_out'))

DT_name <- merge(DT_name, DP_sub, 
                 all = TRUE, by = c('rd_comp', 'rd_out', 'rd_sub'))


## RD_COMP
rd_comp <-
  xml_find_all(xml, "//RD_COMP")

p_rd_comp <- 
  xml_path(rd_comp)

DT_comp <-  data.table(idx = 1:length(p_rd_comp))

DT_comp[, 
         c('root', 'review', 'raw_data', 'rd_comp') := tstrsplit(p_rd_comp, "/")]

DT_comp[, c('idx', 'root', 'review', 'raw_data') := NULL]

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

DT_out[, c('idx', 'root', 'review', 'raw_data') := NULL]

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
         c('root', 'review', 'raw_data', 'rd_comp', 'rd_out', 'rd_sub') := tstrsplit(p_rd_sub, "/")]

DT_sub[, c('idx', 'root', 'review', 'raw_data') := NULL]

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

DT_data[, c('idx', 'root', 'review', 'raw_data') := NULL]

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

DT <- DT_name

#DT <- merge(DT, DT_comp, 
#            all = TRUE, by = c('rd_comp'))
#
#DT <- merge(DT, DT_out, 
#            all = TRUE, by = c('rd_comp', 'rd_out'))
#
#DT <- merge(DT, DT_sub, 
#            all = TRUE, by = c('rd_comp', 'rd_out', 'rd_sub'))
#
DT <- merge(DT, DT_data, 
            all = TRUE, by = c('rd_comp', 'rd_out', 'rd_sub'))

DT

grplabel1 <-
  xml_find_all(xml, "//GRPLABEL1")

grplabel2 <-
  xml_find_all(xml, "//GRPLABEL2")

glabel1 <-
  xml_find_all(xml, "//GLABEL1")

glabel2 <-
  xml_find_all(xml, "//GLABEL2")


p_glabel1 <- xml_path(glabel1)
p_glabel1
