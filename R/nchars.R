#' Count the number of characters
#' 
#' @param x Object to process.
#' @param progress A `logical` value.
#' @param char_regex If not NULL, a regular expression - defaults to "[a-zA-Z]".
#' @param lowercase whether to lower tokens
#' @param mc logical
#' @param ... Argument passed into `blapply()`.
#' @param decreasing logical, passed into order call 
#' @importFrom methods as
#' @importFrom polmineR get_token_stream
#' @exportMethod charcount
#' @rdname charcount
setGeneric("charcount", function(x, ...) standardGeneric("charcount"))


#' @param p_attribute the p-attribute
#' @param char_regex if NULL, counts for all charactrs will be returned, else a regex indicating which characters to include in the counting
#' @param lowercase whether to lower tokens
#' @param mc logical
#' @param decreasing logical, passed into order call 
#' @exportMethod charcount
#' @rdname charcount
#' @examples
#' library(polmineR)
#' use("RcppCWB")
#' 
#' partition("REUTERS", id = "127") %>%
#'   charcount()
#'   
#' corpus("REUTERS") %>%
#'   subset(id == "127") %>%
#'   charcount()
#'   
#' corpus("REUTERS") %>%
#'   partition_bundle(s_attribute = "id") %>%
#'   charcount()
#'   
#' corpus("REUTERS") %>%
#'   split(s_attribute = "id") %>%
#'   charcount()
setMethod("charcount", "partition", function(x, p_attribute = "word", char_regex = "[a-zA-Z]", lowercase = TRUE, decreasing = TRUE){
  .Object <- x
  charSoup <- get_token_stream(.Object, p_attribute = p_attribute, collapse = "")
  if (isTRUE(lowercase)) charSoup <- tolower(charSoup)
  charCount <- table(unlist(strsplit(charSoup, "")))
  if(!is.null(char_regex)){
    charCount <- charCount[grep(char_regex, names(charCount))]
  }
  y <- charCount[order(charCount, decreasing = decreasing)]
  setNames(as.integer(y), names(y))
})

#' @rdname charcount
setMethod("charcount", "subcorpus", function(x, p_attribute = "word", char_regex = "[a-zA-Z]", lowercase = TRUE, decreasing = TRUE){
  charcount(
    x = as(x, "partition"),
    p_attribute = p_attribute,
    char_regex = char_regex,
    lowercase = lowercase,
    decreasing = decreasing
  )
})

#' @rdname charcount
setMethod("charcount", "partition_bundle", function(x, mc = FALSE, progress = TRUE, decreasing = TRUE, ...){
  partitionCount <- if (isFALSE(mc)){
    lapply(x@objects, function(obj) charcount(x = obj, ...))
  } else {
    if (progress){
      pblapply(x@objects, function(obj) charcount(x = obj, ...), cl = mc)
    } else {
      mclapply(x@objects, function(obj) charcount(x = obj, ...), mc.cores = mc)
    }
  }
  
  charCount <- tapply(
    unname(unlist(partitionCount)),
    INDEX = unlist(sapply(partitionCount, function(x) names(x))),
    FUN = sum
  )
  
  y <- charCount[order(charCount, decreasing = decreasing)]
  setNames(as.integer(y), names(y))
})

#' @rdname charcount
setMethod("charcount", "subcorpus_bundle", function(x, decreasing = TRUE, mc = FALSE, progress = TRUE, ...){
  charcount(
    x = as(x, "partition_bundle"),
    decreasing = decreasing,
    mc = mc,
    progress = progress
  )
})


#' @param verbose Whether to output progress messages.
#' @rdname charcount
#' @importFrom polmineR decode
#' @importFrom stringi stri_count_fixed stri_opts_fixed
#' @importFrom RcppCWB cl_id2freq cl_lexicon_size
#' @examples
#' library(polmineR)
#' use("RcppCWB")
#' n <- corpus("REUTERS") %>% charcount(decreasing = FALSE)
setMethod("charcount", "corpus", function(x, p_attribute = "word", lowercase = TRUE, char_regex = "[a-zA-Z]", decreasing = TRUE, verbose = FALSE){
  
  if (verbose) cli_progress_step("get frequencies")
  lexsize <- cl_lexicon_size(
    corpus = x@corpus,
    p_attribute = p_attribute,
    registry = x@registry_dir
  )
  
  freqs <- cl_id2freq(
    corpus = x@corpus,
    p_attribute = p_attribute,
    id = 0L:(lexsize - 1L),
    registry = x@registry_dir
  )

  if (verbose) cli_progress_step("get and process lexicon")
  lex <- p_attributes(.Object = x, p_attribute = p_attribute)
  if (lowercase) lex <- tolower(lex)
  lexsplit <- strsplit(lex, split = "")
  
  if (verbose) cli_progress_step("count characters")
  dt <- data.table(
    char = unlist(lexsplit, recursive = FALSE),
    n = unlist(mapply(rep.int, x = freqs, times = nchar(lex)), recursive = TRUE)
  )
  cnt <- dt[, sum(.SD[["n"]]), by = "char"]
  vec <- setNames(cnt[["V1"]], cnt[["char"]])
  
  vec <- vec[grepl(char_regex, names(vec))]
  vec <- if (decreasing) vec[order(vec, decreasing = TRUE)] else vec[order(vec)]
  vec  
})