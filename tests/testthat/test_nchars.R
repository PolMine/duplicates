library(polmineR)
library(duplicates)
use("duplicates")

testthat::context("detect_duplicates")

test_that(
  "crosscheck nchars",
  {
    charcount1 <- corpus("REUTERS2") %>%
      nchars(
        p_attribute = "word",
        char_regex = "[a-zA-Z]",
        lowercase = TRUE,
        decreasing = FALSE
       )

    charcount2 <- corpus("REUTERS2") %>%
      get_token_stream(p_attribute = "word", collapse = "", verbose = FALSE) %>%
      tolower() %>% 
      strsplit(split = "") %>% 
      .[[1]] %>%
      grep("[a-zA-Z]", ., value = TRUE) %>% 
      table()

    expect_equal(
      charcount1[tolower(LETTERS)],
      setNames(as.integer(charcount2), names(charcount2))
    )
  }
)
