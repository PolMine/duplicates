
#' Encode annotation data
#' 
#' Add structural attributes to CWB corpus based on the annotation data that
#' has been generated.
#' @param x Data.
#' @param corpus ID of CWB corpus.
#' @param method XXX.
#' @importFrom data.table setDT
#' @importFrom cwbtools s_attribute_encode
duplicates_encode <- function(x, corpus, method = "R"){
  
  corpus_obj <- corpus(corpus)
  
  for (s_attr in c("is_duplicate", "duplicates")){
    s_attribute_encode(
      values = as.character(x[[s_attr]]),
      data_dir = corpus_obj@data_dir,
      s_attribute = s_attr,
      corpus = corpus,
      region_matrix = as.matrix(x[, c("cpos_left", "cpos_right")]),
      method = method,
      registry_dir = corpus_obj@registry_dir,
      encoding = corpus_obj@encoding,
      delete = TRUE,
      verbose = TRUE
    )
  }
  invisible(TRUE)
}

.N <- NULL # to avoid warnings

#' Make annotation data
#' 
#' @description
#' Turn `data.table` with duplicates into file with corpus positions and
#' annotation of duplicates.
#' @param drop A character vector of document IDs that will be removed from
#'   the annotation data. Useful for removing known noise that will be 
#'   excluded from the analysis otherwise.
#' @param cols XXX.
#' @param order XXX.
#' @param x Input `data.table`.
#' @param corpus ID of CWB corpus.
#' @param s_attribute Structural attribute to annotate.
#' @importFrom data.table setDT setnames setkeyv
#' @importFrom polmineR corpus
duplicates_as_annotation_data = function(x, corpus, s_attribute, drop = NULL, cols = c("size", "name"), order = c(1L, 1L)){
  
  groups <- as_docgroups()
  
  if (!is.null(drop)){
    groups <- groups[!groups[["name"]] %in% drop]
    groups_n <- groups[, .N, by = "group"]
    groups[groups_n, "group_size" := groups_n[["N"]], on = "group"]
    groups <- groups[groups[["group_size"]] > 1L][, "group_size" := NULL]
  }
  
  original <- groups[,
                     setorderv(x = .SD, cols = cols, order = order)[1,],
                     by = "group", .SDcols = cols
  ][, "is_duplicate" := FALSE]
  groups[original, "is_duplicate" := groups[["is_duplicate"]], on = "name"]
  groups[, "is_duplicate" := ifelse(is.na(groups[["is_duplicate"]]), TRUE, groups[["is_duplicate"]])]
  duplicates_dt <- groups[,
                          list(
                            name = .SD[["name"]],
                            is_duplicate = .SD[["is_duplicate"]],
                            duplicates = sapply(
                              1L:nrow(.SD),
                              function(i) paste(setdiff(.SD[["name"]], .SD[["name"]][i]), collapse = "|")
                            )
                          ),
                          by = "group", .SDcols = c("name", "is_duplicate")
  ][, "group" := NULL]
  
  # get regions ------------------------------------------------------------
  
  corpus_obj <- corpus(corpus)
  x <- corpus(corpus)
  regions <- setDT(
    RcppCWB::s_attribute_decode(
      corpus = corpus,
      data_dir = corpus_obj@data_dir,
      s_attribute = s_attribute,
      encoding = corpus_obj@encoding,
      registry = corpus_obj@registry_dir,
      method = "Rcpp"
    )
  )
  setnames(regions, old = "value", new = s_attribute)
  setkeyv(regions, s_attribute)
  
  # finalize annotation data -----------------------------------------------
  
  setnames(duplicates_dt, old = "name", new = s_attribute)
  anno <- duplicates_dt[regions, on = s_attribute]
  anno[,
       "is_duplicate" := ifelse(
         is.na(anno[["is_duplicate"]]),
         FALSE,
         anno[["is_duplicate"]]
       )
  ]
  anno[,
       "duplicates" := ifelse(
         is.na(anno[["duplicates"]]),
         "",
         anno[["duplicates"]]
       )]
  setcolorder(
    anno,
    c("cpos_left", "cpos_right", s_attribute, "is_duplicate", "duplicates")
  )
  setorderv(anno, cols = "cpos_left")
  anno
}
