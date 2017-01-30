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

      level_pattern <- 
        function(col_level)
          paste0(col_level, '(\\[[0-9]+\\])?/', node_pattern, "$")

      level_index <-
        function(col_level)
          grepl(level_pattern(col_level), ns_path)

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
      names(levels_ns) <- levels_list
      
      levels_path <-
        lapply(levels_ns, xml_path)
      names(levels_path) <- levels_list

      levels_attrs_names <- 
        lapply(levels_ns, 
          function(lns){ 
            ll <- xml_attrs(lns)
            if(length(ll) > 0) 
              return(sort(unique(unlist(lapply(ll, names)))))
            else
              return(NULL)
          }
        )
      names(levels_attrs_names) <- levels_list

      fill_attr_DT <- 
        function(level){
          if(length(levels_ns[[level]]) > 0 ){
            DT <- data.table(PATH = levels_path[[level]])
            l_idx <- which(levels_list == level)
            DT[, (levels_list[1:l_idx ]) := tstrsplit(PATH, "/")]
            lapply(levels_attrs_names[[level]], 
                 function(attr_col){ 
                   values <- xml_attr(levels_ns[[level]], attr_col)
                   DT[, paste0(level, ":", attr_col) := values]
                 })
            return(DT[])
          }else{
            return(NA)
          }
        }

      list_DT <- 
        lapply(levels_list, fill_attr_DT)

      names(list_DT) <- cols_list
      return(list_DT[!is.na(list_DT)])
    }else{
    return(NULL)
    }
  }



x <- data_node_text("TITLE", xml)
x
x <- NULL
p_glabel1 <- xml_path(glabel2, xml)
p_glabel1
