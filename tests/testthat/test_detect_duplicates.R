library(polmineR)
library(duplicates)
use("duplicates")

testthat::context("detect_duplicates")

test_that(
  "run duplicate detection",
  {
    charcount <- corpus("REUTERS2") %>%
      charcount(
        p_attribute = "word",
        char_regex = "[a-zA-Z]",
        lowercase = TRUE,
        decreasing = FALSE
       )
    
    # --------------------------------------------------------------------------

    vocab <- corpus("REUTERS2") %>% 
      p_attributes(p_attribute = "word") %>% 
      charfilter(chars = names(charcount[1:20]))

    x <- corpus("REUTERS2") |>
      polmineR::split(s_attribute = "doc_id")

    dupl <- detect_duplicates(
        x = x,
        p_attribute = "word",
        s_attribute = "doc_id",
        mc = parallel::detectCores() - 2L,
        vocab = vocab,
        threshold = 0.7
      )
    expect_identical(dupl[name == "127"][["duplicate_name"]], "127b")
    expect_identical(dupl[name == "144"][["duplicate_name"]], "144b")
    
    # Same operation with list -------------------------------------------------
    
    x <- corpus("REUTERS2") %>%
      polmineR::split(s_attribute = "doc_id") %>%
      get_token_stream(p_attribute = "word", collapse = "")

    dupl2 <- detect_duplicates(
      x = x,
      n = 5L,
      char = names(charcount[1:20]),
      threshold = 0.7
    )
    
    data.table::setorderv(dupl2, cols = "similarity", order = -1L)
    data.table::setorderv(dupl, cols = "similarity", order = -1L)
    
    expect_equal(
      dupl[, c("name", "duplicate_name", "similarity")],
      dupl2
    )
    
    # --------------------------------------------------------------------------
    
    docgroups1 <- as_docgroups(dupl)
    docgroups2 <- as_docgroups(dupl2)
    
    expect_identical(docgroups1[["group"]], docgroups2[["group"]])
    expect_identical(docgroups1[["name"]], docgroups2[["name"]])
  }
)
