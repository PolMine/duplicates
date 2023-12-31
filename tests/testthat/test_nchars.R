library(polmineR)
library(duplicates)
use("duplicates")

testthat::context("docsimil")

test_that(
  "crosscheck charcount",
  {
    charcount1 <- corpus("REUTERS2", registry = registry()) %>%
      charcount(
        p_attribute = "word",
        char_regex = "[a-zA-Z]",
        lowercase = TRUE,
        decreasing = FALSE
       )

    charcount2 <- corpus("REUTERS2", registry = registry()) %>%
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
