library(data.table)
library(xml2)


pg <- read_xml("http://www.ggobi.org/book/data/olive.xml")

db <- read_xml("../data/CD000004RawData.xml")


db

?xml_find_all
recs <- xml_find_all(pg, "//record")

vals <- trimws(xml_text(recs))

labs <- trimws(xml_attr(recs, "label"))

?xml_attr
  cols <- 
    xml_attr(
   xml_find_all(
    pg, 
    "//data/variables/*[self::categoricalvariable or self::realvariable]"
    ),
   "name")

dat <- do.call(rbind(lapply(strsplit(vals, "\ +"), 
                            function(x){
                              data.frame(rbind(setNames(as.numeric(x), cols)))
                            }))
?xml_cdata 
xml_text(xml_children(db)[2])

xml_find_all(x, "//@x")
xml_structure(db)

y <- xml_find_all(db, "//RD_DATA")
xml_path(y)

xml_ns(db)

xml_attr(y, 'STUDY')
xml_attr(y, 'STUDY_ID')

x <- 
  xml_find_all(db, "//NAME")
  xml_find_all(db, "//TITLE")
help(package = 'xml2')
xml_attr
xml_path(y)[1]

p_RD_OUT <- "RD_OUT\\[([0-9]+)\\]"
p_RD_DATA <- "(RD_DATA\\[)([0-9]+)(\\]$)"

gsub(p_RD_DATA, "\\1", xml_path(y))

xml_path(y) 
m <- gregexpr(p_RD_DATA, xml_path(y))
  L1 <-   regmatches(xml_path(y), m)

which(is.na(L1))
?unlist
L2 <- as.numeric(L1)
 unlist(lapply(L1, function(x) ifelse(is.null(x), NA, x)))

