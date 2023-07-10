library(polmineR)
library(duplicates)
library(cwbtools)
use("duplicates")

testthat::context("encode")

test_that(
  "encode duplicate",
  {
    registry_tmp <- fs::path(tempdir(), "registry_tmp")
    data_dir_tmp <- fs::path(tempdir(), "reuters2_tmp")
    dir.create(registry_tmp)
    dir.create(data_dir_tmp)

    cwbtools::corpus_copy(
      corpus = "REUTERS2",
      registry_dir = system.file(package = "duplicates", "extdata", "cwb", "registry"),
      data_dir = system.file(package = "duplicates", "extdata", "cwb", "indexed_corpora", "reuters2"),
      registry_dir_new = registry_tmp,
      data_dir_new = data_dir_tmp
    )
    
    RcppCWB::cl_load_corpus("REUTERS2", registry = registry_tmp)

    charcount <- corpus("REUTERS2", registry_dir = registry_tmp) %>%
      charcount(
        p_attribute = "word",
        char_regex = "[a-zA-Z]",
        lowercase = TRUE,
        decreasing = FALSE
       )
    
    vocab <- corpus("REUTERS2", registry = registry_tmp) %>% 
      p_attributes(p_attribute = "word") %>% 
      charfilter(chars = names(charcount[1:20]))

    x <- corpus("REUTERS2", registry = registry_tmp) |>
      polmineR::split(s_attribute = "doc_id", verbose = FALSE)

    annodata <- docsimil(
        x = x,
        p_attribute = "word",
        s_attribute = "doc_id",
        mc = parallel::detectCores() - 2L,
        vocab = vocab,
        threshold = 0.7,
        verbose = FALSE
      ) %>% 
      docgroups() %>% 
      duplicates_as_annotation_data(corpus = "REUTERS2", s_attribute = "doc_id", registry = registry_tmp)
    
    
    regdata <- registry_file_parse(corpus = "REUTERS2", registry = registry_tmp)
    
    for (s_attr in c("is_duplicate", "duplicates")){
      s_attribute_encode(
        values = as.character(annodata[[s_attr]]),
        data_dir = data_dir_tmp,
        s_attribute = s_attr,
        corpus = "REUTERS2",
        region_matrix = as.matrix(annodata[, c("cpos_left", "cpos_right")]),
        method = "R",
        registry_dir = registry_tmp,
        encoding = regdata$properties["charset"],
        delete = TRUE,
        verbose = TRUE
      )
    }
    
    RcppCWB::cl_load_corpus("REUTERS2", registry = registry_tmp)
    
    s_attr_new <- corpus("REUTERS2", registry_dir = registry_tmp) %>% 
      s_attributes()
    
    expect_true(all(c("is_duplicate", "duplicates") %in% s_attr_new))
  }
)
