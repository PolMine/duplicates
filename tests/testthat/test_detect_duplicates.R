library(polmineR)
library(duplicates)
use("duplicates")

testthat::context("detect_duplicates")

test_that(
  "run duplicate detection",
  {
    charcount <- corpus("REUTERS2") %>%
      nchars(
        p_attribute = "word",
        char_regex = "[a-zA-Z]",
        lowercase = TRUE,
        decreasing = FALSE
       )

    vocab <- minimize_vocabulary(
      x = "REUTERS2",
      chars = names(charcount[1:12]),
      p_attribute = "word"
    )

    x <- corpus("REUTERS2") |>
      polmineR::split(s_attribute = "doc_id")

    dupl <- detect_duplicates(
        x = x,
        p_attribute = "word",
        s_attribute = "doc_id",
        mc = parallel::detectCores() - 2L,
        vocab = vocab
      )
    expect_identical(dupl[name == "127"][["duplicate_name"]], "127b")
    expect_identical(dupl[name == "144"][["duplicate_name"]], "144b")
  }
)
