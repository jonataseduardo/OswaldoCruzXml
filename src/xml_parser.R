library(data.table)
library(xml2)

#help(package = 'xml2')

xml <- read_xml("../data/CD002010RawData.xml")
xml <- read_xml("../data/CD010388RawData.xml")
xml <- read_xml("../data/CD003407RawData.xml")

find_levels <- 
  function(path_list, pattern = '$@#!'){
    if(length(path_list) > 0 ){
      levels_list <- 
        unlist(
          lapply(
            tstrsplit(path_list, '/'),
            function(p_layer){
                xl <- gsub("(^[A-Z]+)(_[A-Z]+)?(\\[[0-9]+\\])?$", "\\1\\2", p_layer)
                unique(xl[xl != pattern & !is.na(xl)])
            }
          )
        )
      if(pattern == '$@#!')
        return(levels_list)
      else
        return(c(levels_list, pattern))
    }else{
      return(NULL)
    }
  }

level_pattern <- 
  function(col_level)
    paste0(col_level, '(\\[[0-9]+\\])?/', node_pattern, "$")

level_index <-
  function(col_level)
    grepl(level_pattern(col_level), ns_path)

data_node_text <- 
  function(node_pattern, xml){

    ns_list <- 
      xml_find_all(xml, paste0("//", node_pattern))

    if(length(ns_list) > 0 ){
      ns_path <- 
        xml_path(ns_list)

      ns_text <- 
        xml_text(ns_list)

      cols_list <- 
        find_levels(ns_path, node_pattern)

      if(cols_list[1] == "") 
        cols_list[1] <- 'dummy'

      eval_level_DT <- 
        function(col_level){
          level_idx <- level_index(col_level)
          if(any(level_idx)){
            DT <- data.table(PATH = ns_path[level_idx])
            c_idx <- which(cols_list == col_level)
            DT[, (cols_list[1:(c_idx + 1)]) := tstrsplit(PATH, "/")]
            col_text <- paste0(col_level, ':', node_pattern)
            DT[, (col_text) := ns_text[level_idx]]
            return(DT[])
          }else{
            return(NA)
          }
        }

      list_DT <- 
        lapply(cols_list, eval_level_DT)

      names(list_DT) <- cols_list
      return(list_DT[!is.na(list_DT)])
    }else{
    return(NULL)
    }
  }


data_attr <- 
  function(xml){

    ns_rd_data <- 
      xml_find_all(xml, "//RD_DATA")

    if(length(ns_rd_data) > 0 ){

      rd_data_path <- 
        xml_path(ns_rd_data)

      levels_list <- 
        find_levels(rd_data_path)

      if(levels_list[1] == "") 
        levels_list[1] <- 'dummy'

      levels_ns <-
        lapply(levels_list, 
               function(ll) xml_find_all(xml, paste0('//', ll)))


      levels_attrs_l <- 
        lapply(levels_ns, 
          function(lns){ 
            ll <- xml_attrs(lns)
            if(length(ll) > 0) 
              return(sort(unique(unlist(lapply(ll, names)))))
            else
              return(NULL)
          }
        )

      names(levels_attrs_l) <- levels_list
      levels_attrs_l


    fill_attr <- 
      function(attr_cols,
      lapply(attr_cols, 
           function(attr_cols){ 
             values <- xml_attr(ns_list, attr_col)
             DT_comp[, c(attr_col) := values]
             NULL
           })

      eval_level_DT <- 
        function(col_level){
          level_idx <- level_index(col_level)
          if(any(level_idx)){
            DT <- data.table(PATH = ns_path[level_idx])
            c_idx <- which(cols_list == col_level)
            DT[, (cols_list[1:(c_idx + 1)]) := tstrsplit(PATH, "/")]
            col_text <- paste0(col_level, ':', node_pattern)
            DT[, (col_text) := ns_text[level_idx]]
            return(DT[])
          }else{
            return(NA)
          }
        }

      list_DT <- 
        lapply(cols_list, eval_level_DT)

      names(list_DT) <- cols_list
      return(list_DT[!is.na(list_DT)])
    }else{
    return(NULL)
    }
  }


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

xml_path(grplabel1)
grplabel1 <-
  xml_find_all(xml, "//GRPLABEL1")

grplabel2 <-
  xml_find_all(xml, "//GRPLABEL2")

glabel1 <-
  xml_find_all(xml, "//COVER_SHEET")

glabel2 <-
  xml_path(xml_find_all(xml, "//TITLE"))


x <- data_node_text("TITLE", xml)
x
x <- NULL
p_glabel1 <- xml_path(glabel2, xml)
p_glabel1
