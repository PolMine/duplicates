NULL


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
#' Different `partition_bundle`-objects can be passed into the
#' `$detect()`-method successively. The field `duplicates` will be
#' appended by the duplicates that are newly detected.
#' 
#' @param x a `partition_bundle` object defining the documents that will be
#'   compared to detect duplicates
#' @param char_regex a regex defining the characters to keep
#' @param s_attribute the s-attribute providing the date
#' @param sample number of documents to define a subset of `partition_bundle` to
#'   speed up character count
#' @param n number of days before and after a document was published
#' @param threshold numeric (0 < x < 1), the minimum similarity to qualify two
#'   documents as duplicates
#' @param mc logical, whether to use multicore
#' @param verbose logical, whether to be verbose
#' @param progress logical, whether to show progress bar
#' @export Duplicates
#' @rdname Duplicates
#' @importFrom parallel mclapply
#' @importFrom pbapply pblapply
#' @importFrom stats setNames
#' @importFrom RcppCWB get_region_matrix
#' @importFrom cli cli_progress_step cli_progress_done
#' @importFrom R6 R6Class
#' @importFrom slam simple_triplet_matrix
#' @importFrom Matrix triu
#' @import data.table
#' @examples
#' library(polmineR)
#' 
#' if ("RP" %in% corpus()$corpus){
#'   D <- Duplicates$new(
#'     corpus = "RP",
#'     char_regex = "[a-zA-ZäöüÄÖÜ]",
#'     p_attribute = "word",
#'     s_attribute = "article_date",
#'     date_preprocessor = NULL,
#'     sample = 50L,
#'     n = 0L,
#'     threshold = 0.6 # default is 0.9
#'   )
#'   
#'   dates <- seq.Date(from = as.Date("2014-01-02"), to = as.Date("2014-02-08"), by = 1L)
#'   dates <- as.character(dates) %>% 
#'     intersect(s_attributes(corpus("RP"), "article_date")) %>% 
#'     as.Date()
#' 
#'   article_bundle <- corpus("RP") |>
#'     subset(article_date %in% as.character(dates)) |> 
#'     split(s_attribute = "article_id")
#' 
#'   D$detect(
#'     x = article_bundle,
#'     mc = 6L,
#'     progress = FALSE,
#'     f = lubridate::floor_date(
#'       as.Date(unlist(s_attributes(article_bundle, "article_date"))),
#'       unit = "month",
#'       week_start = 1L
#'     )
#'   )
#'   
#'   # To inspect result
#'   D$duplicates
#'   
#'   if (interactive()){
#'     for (i in 1L:nrow(D$duplicates)){
#'     
#'       print(i)
#'       
#'       corpus("NADIRASZ") %>%
#'         subset(article_id == !!D$duplicates[i][["name"]]) %>%
#'         read() %>%
#'         show()
#'         
#'       readline()
#'   
#'       corpus("NADIRASZ") %>%
#'         subset(article_id == !!D$duplicates[i][["duplicate_name"]]) %>%
#'         read() %>%
#'         show()
#'         
#'       readline()
#'     }
#'   }
#' }
Duplicates <- R6::R6Class(
  
  "Duplicates",
  
  public = list(
    
    #' @field corpus ID of the CWB corpus (derived from `partition_bundle`).
    corpus = NULL,
    
    #' @field char_regex Regular expression defining the characters to keep.
    char_regex = NULL,
    
    #' @field char_count Count of the characters in the `partition_bundle`.
    char_count = NULL,
    
    #' @field n Number of days before and after a document was published.
    n = NULL,
    
    #' @field p_attribute the p-attribute used (defaults to "word")
    p_attribute = NULL,
    
    #' @field s_attribute the s-attribute of the date of a text in the corpus
    s_attribute = NULL,
    
    #' @field sample size of the sample of the `partition_bundle` that the character count is based on
    sample = NULL,
    
    #' @field threshold Minimum similarity value to consider two texts as
    #'   duplicates.
    threshold = NULL,
    
    #' @field duplicates A `data.table` with documents considered as duplicates.
    duplicates = NULL,
    
    #' @field similarities A `simple_triplet_matrix` with similarities of texts
    similarities = NULL,
    
    #' @field date_preprocessor function to rework dates if not in the DD-MM-YYYY standard format
    date_preprocessor = "function",
    
    #' @field annotation A `data.table` with corpus positions and annotation data.
    annotation = NULL,
    
    #' @field vocabulary A purged version of the vocabulary.
    vocabulary = NULL,
    
    #' @description 
    #' Initialize object of class `Duplicates`.
    #' @param corpus ID of the CWB corpus that will be explored.
    #' @param p_attribute The p-attribute to evaluate.
    #' @param date_preprocessor A function used to preprocess dates as extracted
    #'   from `s_attribute`.
    initialize = function(corpus, char_regex = "[a-zA-Z]", p_attribute = "word", s_attribute = "text_date", date_preprocessor = NULL, sample = 1000L, n = 1L, threshold = 0.9){
      
      stopifnot(isFALSE(missing(corpus)))
      self$corpus <- corpus
      self$char_regex <- char_regex
      self$s_attribute <- s_attribute
      self$p_attribute <- p_attribute
      self$sample <- as.integer(sample)
      self$n <- as.integer(n)
      self$threshold <- threshold
      if (is.null(date_preprocessor)) self$date_preprocessor <- function(x) x
      
    },
    
    
    #' @description
    #' Identify documents that will be compared (based on date of documents).
    #' @param reduce A `logical` value, whether to drop one half of matrix.
    get_comparisons = function(x, reduce = TRUE, verbose = FALSE, progress = TRUE, mc = FALSE){

      if (!self$s_attribute %in% s_attributes(self$corpus)){
        stop("no valid s-attribute in field 's_attribute'")
      }
      
      if (!requireNamespace("chron", quietly = TRUE)){
        stop("the 'chron'-package needs to be installed but is not available")
      }
      
      if (verbose) cli_progress_step("getting docs to be compared")
      dates <- unlist(lapply(
        setNames(x@objects, names(x)),
        function(y) s_attributes(y, self$s_attribute)
      ))
      if (!is.null(self$date_preprocessor)) dates <- sapply(dates, self$date_preprocessor)
      objectSplittedByDate <- split(1L:length(x), f = dates)
      .get_comparisons <- function(i){
        dateOfDoc <- try(as.POSIXct(unname(dates[i])))
        if (is(dateOfDoc)[1] == "try-error"){
          warning(paste("cannot parse date:", dates[i]))
          return(NULL)
        }
        
        if (self$n > 0){
          dateRange <- chron::seq.dates(
            from = strftime(dateOfDoc - 1 - (self$n - 1) * 86400, format = "%m/%d/%Y"),
            to = strftime(dateOfDoc + 1 + (self$n - 1) * 86400, format = "%m/%d/%Y"),
            by = "days", format = "%Y-%m-%d"
          )
        } else {
          dateRange <- dateOfDoc
        }
        datesToGet <- as.character(strftime(dateRange, format = "%Y-%m-%d"))
        unlist(lapply(datesToGet, function(y) objectSplittedByDate[[y]]))
      }
      
      docsToCompare <- pblapply(
        1L:length(x),
        FUN = .get_comparisons, cl = getOption("polmineR.cores")
      )
      
      docsToCompareMatrix <- simple_triplet_matrix(
        i = unlist(docsToCompare),
        j = unlist(lapply(
          1L:length(docsToCompare),
          function(i) rep(i, times = length(docsToCompare[[i]]))
        )),
        v = rep(NA, times = length(unlist(docsToCompare))),
        ncol = length(x),
        nrow = length(x),
        dimnames = list(rows = names(x), columns = names(x))
      )
      if (reduce){
        if (verbose) cli_progress_step("reduction of document comparisons")
        keepOrDrop <- ifelse(docsToCompareMatrix$i < docsToCompareMatrix$j, TRUE, FALSE)
        for (x in c("i", "j", "v")) docsToCompareMatrix[[x]] <- docsToCompareMatrix[[x]][keepOrDrop]
      }
      return( docsToCompareMatrix )
    },
    

    #' @description
    #' Wrapper that implements the entire workflow for duplicate detection.
    #' @param x A `partition_bundle` or `subcorpus_bundle` object.
    #' @param n The number of characters to use for shingling (`integer` value),
    #'   passed as argument `n` into `polmineR::ngrams()`. Defaults to 5, in 
    #'   line with Kliche et al. 2014: 695.
    #' @param f If n == 0, f will be passed into `split()` on vector with dates.
    #'   If `NULL` (default), the dates vector will be split by dates.
    #' @param character_selection Numeric/integer vector used for indexing
    #'   `$char_count` to select the characters to keep. Defaults to 1:12, in
    #'   line with Kliche et al. 2014: 695.
    #' @param min_shingle_length An `integer` value with minimum length of
    #'   shingles that enter calculation of document similarity. Defaults to
    #'   `n`.
    #' @return The updated content of slot `$duplicates` is returned invisibly.
    #' @importFrom cli cli_alert_info col_blue
    #' @importFrom slam row_sums col_sums
    #' @importFrom proxyC simil
    #' @importFrom polmineR as.sparseMatrix
    detect = function(x, n = 5L, f = NULL, character_selection = 1:12, min_shingle_length = n, verbose = TRUE, mc = FALSE, progress = TRUE){
      
      if (x@corpus != self$corpus){
        stop("The corpus ID configured in the Duplicates engine and of the bundle are not identical.")
      }
      
      started <- Sys.time()
      
      sizes <- sapply(x@objects, slot, "size")

      if (is.null(self$char_count)){
        if (verbose) cli_progress_step("counting characters")
        self$char_count <- nchars(
          x = if (is.numeric(self$sample)) sample(x, size = self$sample) else (x),
          p_attribute = self$p_attribute,
          regexCharsToKeep = self$char_regex,
          lowercase = TRUE,
          decreasing = FALSE,
          mc = FALSE, progress = progress
        )
      }
      
      cli::cli_alert_info(
        sprintf(
          "letters used for shingling: %s",
          col_blue(
            paste(names(self$char_count[character_selection]), collapse = "")
          )
        )
      )

      if (is.null(self$vocabulary)){
        if (verbose) cli_progress_step("generate reduced vocabulary")
        char <- names(self$char_count[character_selection])
        vocab <- p_attributes(self$corpus, p_attribute = self$p_attribute)
        vocab <- lapply(
          strsplit(vocab, ""),
          function(x) paste(na.omit(ifelse(x %in% char, x, NA)), collapse = "")
        )
        self$vocabulary <- unlist(vocab, recursive = FALSE)
      }

      if (verbose) cli_progress_step("get data for ngram matrix")
      ngram_bundle <- ngrams(
        x,
        n = n,
        char = "",
        p_attribute = self$p_attribute,
        vocab = self$vocabulary,
        mc = mc,
        progress = progress,
        verbose = FALSE
      )
      
      if (verbose) cli_progress_step("assemble ngram matrix")
      ngram_matrix <- as.TermDocumentMatrix(
        ngram_bundle,
        col = "count",
        verbose = FALSE
        ) |>
        weigh(method = "tfidf") |>
        as.sparseMatrix()
      
      if (verbose) cli_progress_step("get dates")
      dates <- s_attributes(x, s_attribute = self$s_attribute)
      
      if (verbose) cli_progress_step("split data into groups")
      f <- if (!is.null(f)) f else as.factor(unname(unlist(dates)))
      if (length(f) != length(dates))
        cli_alert_danger("length of factor `f` differs from length of bundle")
      groups <- split(
        x = names(dates),
        f = if (!is.null(f)) f else as.factor(unname(unlist(dates)))
      )
      # drop groups with only one id (nothing to compare)
      to_drop <- unname(which(sapply(groups, length) <= 1L))
      if (length(to_drop) > 0) groups <- groups[-to_drop]
      if (verbose) cli_progress_done()
      
      .get_similarities <- function(groupname){
        if (isTRUE(verbose) && isFALSE(mc)){
          cli_alert_info(
            paste(
              "compute similarities for group:",
              col_blue(groupname),
              "(N = {.val {length(groups[[groupname]])}})"
            )
          )
        }
        
        if (verbose) cli_progress_step("subset matrix")
        m <- ngram_matrix[,groups[[groupname]]] # slow!
        
        # Drop ngrams that are not present in this subset of the larger matrix
        if (verbose) cli_progress_step("purge matrix")
        empty_rows <- unname(which(row_sums(m) == 0L))
        if (length(empty_rows) > 0L) m <- m[-empty_rows,]
        
        # Very short documents may result in shingle lengths below n, and this
        # may result in an undesired complete similarity. So drop short 
        # shingles and purge matrix.
        short_shingles <- which(nchar(rownames(m)) < min_shingle_length)
        if (length(short_shingles) > 0L){
          m <- m[-short_shingles,]
          empty_docs <- which(col_sums(m) == 0)
          if (length(empty_docs) > 0L) m <- m[,-empty_docs]
        }
        
        if (verbose)
          cli_progress_step(
            "compute similarity (ncol = {.val {ncol(m)}} / nrow = {.val {nrow(m)}})"
          )
        sim <- simil(
          x = m,
          margin = 2,
          method = "cosine",
          min_simil = self$threshold,
          use_nan = FALSE
        )
        sim_min <- triu(sim, k = 1L)
        
        if (verbose)
          cli_alert_info("no of duplicates detected: {.val {length(sim_min@x)}}")
        
        y <- data.table(
          name = sim_min@Dimnames[[1]][sim_min@i + 1],
          duplicate_name = sim_min@Dimnames[[2]][sim_min@j + 1],
          similarity = sim_min@x
        )
        y[, "size" := sizes[y[["name"]]]]
        y[, "date" := unlist(dates[y[["name"]]])]
        y[, "duplicate_size" := sizes[y[["duplicate_name"]]]]
        y[, "date_duplicate" := unlist(dates[y[["duplicate_name"]]])]
        y
      }
      
      if (progress){
        simlist <- pblapply(names(groups), .get_similarities, cl = mc)
      } else {
        if (mc){
          if (verbose) cli_progress_step("compute similarities")
          simlist <- mclapply(names(groups), .get_similarities, mc.cores = mc)
          if (verbose) cli_progress_done()
        } else {
          simlist <- lapply(names(groups), .get_similarities)
        }
      }
      dt <- unique(rbindlist(simlist))
      
      
      if (nrow(dt) == 0L){
        duplicates_dt <- NULL
        if (verbose) cli_alert_info("no duplicates detected")
      } else {
        cli_alert_info(
          paste(
            "number of duplicates detected:", col_blue(nrow(dt))
          )
        )
      }
      if (verbose) cli_progress_done()
      
      if (is.null(self$duplicates)){
        self$duplicates <- dt
      } else {
        if (verbose)
          cli_alert_info("appending results to existing table with duplicates")
        self$duplicates <- rbind(self$duplicates, dt)
      }
      
      if (verbose) cli_alert_info(
        sprintf(
          "total time for duplicate detection job: %s",
          col_blue(
            paste(
              round(as.numeric(Sys.time() - started, units = "secs"), 2),
              "s", sep = ""
            )
          )
        )
      )
      
      invisible(self$duplicates)
    },
    
    #' @description 
    #' Prepare `data.table` with document ids, sizes, and group.
    #' @importFrom igraph graph_from_data_frame decompose get.vertex.attribute
    get_duplicates_groups = function(){
      if (is.null(self$duplicates)) stop("field 'duplicates' is NULL")
      
      ids <- self$duplicates[, c("name", "duplicate_name")] |>
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
    },
    
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
    make_annotation_data = function(s_attribute, drop = NULL, cols = c("size", "name"), order = c(1L, 1L)){
      
      groups <- self$get_duplicates_groups()
      
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
    },
    
    #' @description
    #' Add structural attributes to CWB corpus based on the annotation data that
    #' has been generated (data.table in field annotation).
    #' @param method XXX.
    #' @importFrom data.table setDT
    #' @importFrom cwbtools s_attribute_encode
    encode = function(method = "R"){
      
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
  )
)

