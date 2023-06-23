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
#' @param date_preprocessor A function used to preprocess dates as extracted
#'   from `s_attribute`.
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
#' 
#' sample_docs <- corpus("RP") %>% 
#'   s_attributes("article_date") %>%
#'   sample(10)
#'   
#' charcount <- corpus("RP") %>% 
#'   subset(article_date %in% sample_docs) %>% 
#'   nchars(
#'     p_attribute = "word",
#'     char_regex = "[a-zA-ZäöüÄÖÜ]",
#'     lowercase = TRUE,
#'     decreasing = FALSE
#'    )
#' 
#' vocab <- minimize_vocabulary(
#'   x = "RP",
#'   chars = names(charcount[1:12]),
#'   p_attribute = "word"
#' )
#' 
#' dates <- seq.Date(as.Date("2014-01-02"), as.Date("2014-02-08"), by = 1L) %>% 
#'   as.character()
#' 
#' x <- corpus("RP") |>
#'   subset(article_date %in% dates) |> 
#'   split(s_attribute = "article_id")
#' 
#' dupl <- detect_duplicates(
#'     x = x,
#'     p_attribute = "word",
#'     s_attribute = "article_date",
#'     mc = parallel::detectCores() - 2L,
#'     vocab = vocab
#'   )
setMethod("detect_duplicates", "partition_bundle",
  function(
    x, n = 5L, min_shingle_length = n,
    p_attribute = "word", s_attribute = "text_date",
    vocab,
    date_preprocessor = NULL, 
    threshold = 0.9,
    verbose = TRUE, mc = FALSE
  ){ 
    started <- Sys.time()
    
    if (verbose) cli_progress_step("get sizes and dates")
    sizes <- sapply(x@objects, slot, "size")
    dates <- s_attributes(x, s_attribute = s_attribute, unique = TRUE)
    dates <- lapply(dates, `[[`, 1L) # a sanity measure
    
    if (verbose) cli_progress_step("make ngram matrix")
    ngrams <- ngrams(
      x, n = n, char = "", p_attribute = p_attribute, vocab = vocab,
      mc = mc, progress = FALSE, verbose = FALSE
    )
    m <- as.TermDocumentMatrix(ngrams, col = "count", verbose = FALSE) |>
      weigh(method = "tfidf") |>
      as.sparseMatrix()
    # Very short documents may result in shingle lengths below n, and this
    # may result in an undesired complete similarity. So drop short 
    # shingles and purge matrix.
    short_shingles <- which(nchar(rownames(m)) < min_shingle_length)
    if (length(short_shingles) > 0L){
      m <- m[-short_shingles,]
      empty_docs <- which(col_sums(m) == 0)
      if (length(empty_docs) > 0L) m <- m[,-empty_docs]
    }
    
    if (verbose) cli_progress_step("compute similarity ({.val {ncol(m)}} docs)")
    sim <- simil(m, margin = 2, method = "cosine", min_simil = threshold, use_nan = FALSE)
    sim_min <- triu(sim, k = 1L)
    if (verbose) cli_progress_done()
    if (verbose) cli_alert_info("no of duplicates: {.val {length(sim_min@x)}}")
    
    dt <- data.table(
      name = sim_min@Dimnames[[1]][sim_min@i + 1],
      duplicate_name = sim_min@Dimnames[[2]][sim_min@j + 1],
      similarity = sim_min@x
    )
    dt[, "size" := sizes[dt[["name"]]]]
    dt[, "duplicate_size" := sizes[dt[["duplicate_name"]]]]
    
    if (nrow(dt) > 0L){
      dt[, "date" := unlist(dates[dt[["name"]]])]
      dt[, "date_duplicate" := unlist(dates[dt[["duplicate_name"]]])]
    } else {
      dt[, "date" := character()][, "date_duplicate" := character()]
    }
    dt
  }
)