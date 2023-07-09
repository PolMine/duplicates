#' @rdname detect_duplicates
setGeneric("detect_duplicates", function(x, ...) standardGeneric("detect_duplicates"))


#' Detect Duplicates
#' 
#' Class for duplicate detection.
#' 
#' The class implements a procedure described by Fritz Kliche, Andre Blessing,
#' Urlich Heid and Jonathan Sonntag in the paper "The eIdentity Text
#' ExplorationWorkbench" presented at LREC 2014
#' (see \url{http://www.lrec-conf.org/proceedings/lrec2014/pdf/332_Paper.pdf}).
#' 
#' To detect duplicates, choices are made as follows:
#' - If two similar articles have been published on the same day, the shorter
#' article will be considered the duplicate;
#' - if two similar articles were published on different days, the article that
#' appeared later will be considered the duplicate.
#' 
#' @param x A `partition_bundle` or `subcorpus_bundle` object with documents to
#'   evaluate.
#' @param n The number of characters to use for shingling (`integer` value),
#'   passed as argument `n` into `polmineR::ngrams()`. Defaults to 5, in 
#'   line with Kliche et al. 2014: 695.
#' @param min_shingle_length An `integer` value with minimum length of
#'   shingles that enter calculation of document similarity. Defaults to
#'   `n`.
#' @return The updated content of slot `$duplicates` is returned invisibly.
#' @param s_attribute The s-attribute providing the date of documents.
#' @param p_attribute The p-attribute to evaluate.
#' @param threshold A `numeric` value (0 < x < 1), the minimum similarity to 
#'   qualify two documents as duplicates
#' @param mc A `logical` value, whether to use multicore.
#' @param verbose A `logical` value, whether to be verbose.
#' @param ... Further arguments (unused).
#' @param vocab Pruned vocabulary.
#' @export detect_duplicates
#' @rdname detect_duplicates
#' @importFrom parallel mclapply
#' @importFrom pbapply pblapply
#' @importFrom stats setNames
#' @importFrom RcppCWB get_region_matrix
#' @importFrom cli cli_progress_step cli_progress_done
#' @importFrom R6 R6Class
#' @importFrom slam simple_triplet_matrix
#' @importFrom Matrix triu
#' @importFrom cli cli_alert_info col_blue cli_alert_danger
#' @importFrom slam row_sums col_sums
#' @importFrom proxyC simil
#' @importFrom polmineR as.sparseMatrix
#' @importFrom methods slot
#' @importFrom polmineR p_attributes weigh as.TermDocumentMatrix s_attributes
#' @import data.table
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
#' dupl <- detect_duplicates(
#'     x = x,
#'     p_attribute = "word",
#'     s_attribute = "doc_id",
#'     mc = parallel::detectCores() - 2L,
#'     vocab = vocab
#'   )
#'   
#' docgrps <- as_docgroups(dupl)
setMethod("detect_duplicates", "partition_bundle",
  function(
    x, n = 5L, min_shingle_length = n,
    p_attribute = "word", s_attribute = "text_date",
    vocab,
    threshold = 0.9,
    verbose = TRUE, mc = FALSE
  ){ 
    started <- Sys.time()
    
    if (verbose) cli_progress_step("get sizes and metadata")
    sizes <- sapply(x@objects, slot, "size")
    s_attr <- s_attributes(x, s_attribute = s_attribute, unique = TRUE)
    s_attr <- lapply(s_attr, `[[`, 1L) # a sanity measure
    
    if (verbose) cli_progress_step("make ngram matrix")
    ngrams <- ngrams(
      x, n = n, char = "", p_attribute = p_attribute, vocab = vocab,
      mc = mc, progress = FALSE, verbose = FALSE
    )
    m <- as.TermDocumentMatrix(ngrams, col = "count", verbose = FALSE) |>
      weigh(method = "tfidf") |>
      as.sparseMatrix()
    
    dt <- detect_duplicates(
      x = m,
      n = n,
      min_shingle_length = min_shingle_length,
      threshold = threshold,
      verbose = verbose
    )
    
    dt[, "size" := sizes[dt[["name"]]]]
    dt[, "duplicate_size" := sizes[dt[["duplicate_name"]]]]
    
    if (nrow(dt) > 0L){
      dt[, (s_attribute) := unlist(s_attr[dt[["name"]]])]
      dt[, (paste("duplicate", s_attribute, sep = "_")) := unlist(s_attr[dt[["duplicate_name"]]])]
    } else {
      dt[, (s_attribute) := character()]
      dt[, (paste("duplicate", s_attribute, sep = "_")) := character()]
    }
    dt
  }
)

#' @param char A `character` vector with characters to keep. Passed into method
#'   `polmineR::ngrams()`.
#' @examples
#' library(polmineR)
#' use(pkg = "duplicates")
#' 
#' x <- corpus("REUTERS2") %>% 
#'   split(s_attribute = "doc_id") %>% 
#'   get_token_stream(p_attribute = "word", collapse = "")
#'   
#' chars <- table(tolower(strsplit(paste(unlist(x), collapse = ""), "")[[1]]))
#' chars <- chars[grep("[a-zA-Z]", names(chars))]
#' char <- names(chars[order(chars, decreasing = FALSE)][1:20])
#' 
#' dupl <- detect_duplicates(x = x, n = 5L, char = char, threshold = 0.6)
#' 
#' docgrps <- as_docgroups(dupl, cols = "name", order = 1L)
#' @rdname detect_duplicates
setMethod("detect_duplicates", "list", function(x, n = 5L, min_shingle_length = n, char = "", threshold = 0.9, verbose = TRUE, mc = FALSE){ 
  started <- Sys.time()
  
  stopifnot(is.character(char))
  
  if (verbose) cli_progress_step("make ngram matrix")
  ngrams <- ngrams(
    x, n = n, char = char,  mc = mc, progress = FALSE, verbose = FALSE
  )
  
  DT <- data.table::rbindlist(ngrams)
  DT[, "j" := unlist(mapply(rep, seq_along(ngrams), lapply(ngrams, function(obj) nrow(obj))))]
  unique_keys <- unique(DT[["ngram"]])
  keys <- setNames(seq_along(unique_keys), unique_keys)
  i <- keys[ DT[["ngram"]] ]
  tdm <- list(
    i = unname(i),
    j = DT[["j"]],
    v = DT[["count"]],
    nrow = length(names(keys)),
    ncol = length(ngrams),
    dimnames = list(
      Terms = names(keys),
      Docs = names(ngrams)
    )
  )
  class(tdm) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  
  m <- weigh(tdm, method = "tfidf") |> as.sparseMatrix()
  
  detect_duplicates(
    x = m,
    n = n,
    min_shingle_length = min_shingle_length,
    threshold = threshold,
    verbose = verbose
  )
}
)

#' @rdname detect_duplicates
#' @export
setMethod("detect_duplicates", "dgCMatrix", function(x, n, min_shingle_length, threshold, verbose){
  
  # Very short documents may result in shingle lengths below n, and this
  # may result in an undesired complete similarity. So drop short 
  # shingles and purge matrix.
  short_shingles <- which(nchar(rownames(x)) < min_shingle_length)
  if (length(short_shingles) > 0L){
    x <- x[-short_shingles,]
    empty_docs <- which(col_sums(x) == 0)
    if (length(empty_docs) > 0L) x <- x[,-empty_docs]
  }
  
  if (verbose) cli_progress_step("compute similarity ({.val {ncol(x)}} docs)")
  sim <- simil(x, margin = 2, method = "cosine", min_simil = threshold, use_nan = FALSE)
  sim_min <- triu(sim, k = 1L)
  if (verbose) cli_progress_done()
  if (verbose) cli_alert_info("no of duplicates: {.val {length(sim_min@x)}}")
  
  data.table(
    name = sim_min@Dimnames[[1]][sim_min@i + 1],
    duplicate_name = sim_min@Dimnames[[2]][sim_min@j + 1],
    similarity = sim_min@x
  )
})


#' Get groups of near-duplicate documents
#' 
#' @param x A `data.table` with duplicates that have been detected.
#' @param drop A character vector of document IDs that will be removed from
#'   the annotation data. Useful for removing known noise that will be 
#'   excluded from the analysis otherwise.
#' @param cols XXX.
#' @param order XXX.
#' @importFrom igraph graph_from_data_frame decompose get.vertex.attribute
#' @export as_docgroups
#' @rdname docgroups
as_docgroups <- function(x, drop = NULL, cols = c("size", "name"), order = c(1L, 1L)){
  
  ids <- x[, c("name", "duplicate_name")] |>
    as.data.frame() |>
    igraph::graph_from_data_frame() |>
    igraph::decompose() |>
    lapply(igraph::get.vertex.attribute, name = "name")
  
  dt <- data.table(
    name = unlist(ids),
    group = unlist(
      mapply(rep, seq_along(ids), sapply(ids, length), SIMPLIFY = FALSE),
      recursive = FALSE
    )
  )
  
  duplcols <- grep("duplicate_", colnames(x), value = TRUE)
  metadata <- unique(rbindlist(
    list(
      x[, setdiff(colnames(x), c(duplcols, "similarity")), with = FALSE],
      x[, duplcols, with = FALSE]
    ),
    use.names = FALSE
  ))
  
  grp <- metadata[dt, on = "name"]
  setcolorder(grp, neworder = c("group", "name"))

  if (!is.null(drop)){
    grp <- grp[!grp[["name"]] %in% drop]
    grp_n <- grp[, .N, by = "group"]
    grp[grp_n, "group_size" := grp_n[["N"]], on = "group"]
    grp <- grp[grp[["group_size"]] > 1L][, "group_size" := NULL]
  }
  
  original <- grp[
    , setorderv(x = .SD, cols = cols, order = order)[1,],
    by = "group",
    .SDcols = cols
  ]
  original[, "is_duplicate" := FALSE]
  grp[original, "is_duplicate" := original[["is_duplicate"]], on = "name"]
  grp[
    , "is_duplicate" := ifelse(
      is.na(grp[["is_duplicate"]]),
      TRUE,
      grp[["is_duplicate"]]
    )
  ]
  
  grp
}
