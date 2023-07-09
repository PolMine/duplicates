.N <- NULL # to avoid warnings

#' Make annotation data
#' 
#' @description
#' Turn `data.table` with duplicates into file with corpus positions and
#' annotation of duplicates.
#' @param x Input `data.table`.
#' @param corpus ID of CWB corpus.
#' @param s_attribute Structural attribute to annotate.
#' @importFrom data.table setDT setnames setkeyv
#' @importFrom polmineR corpus
#' @export
#' @examples
#' library(polmineR)
#' use(pkg = "duplicates")
#' 
#' charcount <- corpus("REUTERS2") %>% 
#'   charcount(
#'     p_attribute = "word",
#'     char_regex = "[a-zA-Z]",
#'     lowercase = TRUE,
#'     decreasing = FALSE
#'    )
#' 
#' vocab <- corpus("REUTERS2") %>% 
#'   p_attributes(p_attribute = "word") %>% 
#'   charfilter(chars = names(charcount[1:12]))
#' 
#' x <- corpus("REUTERS2") |>
#'   split(s_attribute = "doc_id")
#' 
#' dupl <- docsimil(
#'     x = x,
#'     p_attribute = "word",
#'     s_attribute = "doc_id",
#'     mc = parallel::detectCores() - 2L,
#'     vocab = vocab
#'   )
#' 
#' grps <- docgroups(dupl)
#' 
#' annodata <- duplicates_as_annotation_data(
#'   x = grps,
#'   corpus = "REUTERS2", 
#'   s_attribute = "doc_id"
#' )
duplicates_as_annotation_data = function(x, corpus, s_attribute){
  
  duplicates_dt <- x[,
    list(
      name = .SD[["name"]],
      is_duplicate = .SD[["is_duplicate"]],
      duplicates = sapply(
        1L:nrow(.SD),
        function(i) paste(setdiff(.SD[["name"]], .SD[["name"]][i]), collapse = "|")
      )
    ),
    by = "group",
    .SDcols = c("name", "is_duplicate")
  ]
  duplicates_dt[, "group" := NULL]
  
  # get regions ------------------------------------------------------------
  
  corpus_obj <- corpus(corpus)
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
      "", anno[["duplicates"]]
    )
  ]
  setcolorder(
    anno,
    c("cpos_left", "cpos_right", s_attribute, "is_duplicate", "duplicates")
  )
  setorderv(anno, cols = "cpos_left")
  anno
}

