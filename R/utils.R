#' Reduce vocabulary to a set of characters
#' 
#' @param x A CWB corpus.
#' @param chars Vector with characters to keep.
#' @param p_attribute A positional attribute.
#' @export
#' @importFrom polmineR p_attributes
#' @importFrom stats na.omit
minimize_vocabulary <- function(x, chars, p_attribute = "word"){
  vocab <- p_attributes(x, p_attribute = p_attribute)
  vocab <- lapply(
    strsplit(vocab, ""),
    function(x) paste(na.omit(ifelse(x %in% chars, x, NA)), collapse = "")
  )
  unlist(vocab, recursive = FALSE)
}

#' Get duplicate groups
#' 
#' @param x A `data.table` with duplicates that have been detected.
#' @importFrom igraph graph_from_data_frame decompose get.vertex.attribute
duplicates_get_groups <- function(x){

  ids <- x[, c("name", "duplicate_name")] |>
    as.data.frame() |>
    igraph::graph_from_data_frame() |>
    igraph::decompose() |>
    lapply(igraph::get.vertex.attribute, name = "name")
  
  dt <- data.table(
    name = unlist(ids),
    group = unlist(
      mapply(rep, seq_along(ids), sapply(ids, length)),
      recursive = FALSE
    )
  )
  
  sizes <- unique(rbindlist(
    list(
      self$duplicates[, c("name", "size")],
      self$duplicates[, c("duplicate_name", "duplicate_size")]
    ),
    use.names = FALSE
  ))
  
  sizes[dt, on = "name"]
}


#' @description
#' Add structural attributes to CWB corpus based on the annotation data that
#' has been generated (data.table in field annotation).
#' @param method XXX.
#' @importFrom data.table setDT
#' @importFrom cwbtools s_attribute_encode
duplicates_encode <- function(x, method = "R"){
  
  x <- corpus(self$corpus)
  
  for (s_attr in c("is_duplicate", "duplicates")){
    s_attribute_encode(
      values = as.character(self$annotation[[s_attr]]),
      data_dir = x@data_dir,
      s_attribute = s_attr,
      corpus = self$corpus,
      region_matrix = as.matrix(self$annotation[, c("cpos_left", "cpos_right")]),
      method = method,
      registry_dir = x@registry_dir,
      encoding = x@encoding,
      delete = TRUE,
      verbose = TRUE
    )
  }
  invisible(TRUE)
}

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
#' @importFrom data.table setDT setnames setkeyv
#' @importFrom polmineR corpus
duplicates_as_annotation_data = function(x, corpus, s_attribute, drop = NULL, cols = c("size", "name"), order = c(1L, 1L)){
  
  groups <- duplicates_get_groups()
  
  if (!is.null(drop)){
    groups <- groups[!groups[["name"]] %in% drop]
    groups[groups[, .N, by = "group"], "group_size" := N, on = "group"]
    groups <- groups[group_size > 1L][, "group_size" := NULL]
  }
  
  original <- groups[,
                     setorderv(x = .SD, cols = cols, order = order)[1,],
                     by = "group", .SDcols = cols
  ][, "is_duplicate" := FALSE]
  groups[original, "is_duplicate" := is_duplicate, on = "name"]
  groups[, "is_duplicate" := ifelse(is.na(is_duplicate), TRUE, is_duplicate)]
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
  
  x <- corpus(self$corpus)
  regions <- setDT(
    RcppCWB::s_attribute_decode(
      corpus = self$corpus,
      data_dir = x@data_dir,
      s_attribute = s_attribute,
      encoding = x@encoding,
      registry = x@registry_dir,
      method = "Rcpp"
    )
  )
  setnames(regions, old = "value", new = s_attribute)
  setkeyv(regions, s_attribute)
  
  # finalize annotation data -----------------------------------------------
  
  setnames(duplicates_dt, old = "name", new = s_attribute)
  self$annotation <- duplicates_dt[regions, on = s_attribute]
  self$annotation[,
                  "is_duplicate" := ifelse(
                    is.na(self$annotation[["is_duplicate"]]),
                    FALSE,
                    self$annotation[["is_duplicate"]]
                  )
  ]
  self$annotation[,
                  "duplicates" := ifelse(
                    is.na(self$annotation[["duplicates"]]),
                    "",
                    self$annotation[["duplicates"]]
                  )]
  setcolorder(
    self$annotation,
    c("cpos_left", "cpos_right", s_attribute, "is_duplicate", "duplicates")
  )
  setorderv(self$annotation, cols = "cpos_left")
  invisible(self$annotation)
}
