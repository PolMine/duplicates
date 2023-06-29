library(tm)
library(tokenizers)
library(RcppCWB)
library(magrittr)

vrt_dir <- file.path(tempdir(), "reuters")
if (!dir.exists(vrt_dir)) dir.create(vrt_dir)

registry_dir <- "~/Lab/github/duplicates/inst/extdata/cwb/registry"
data_dir <- "~/Lab/github/duplicates/inst/extdata/cwb/indexed_corpora/reuters2"

reuters_fname <- system.file(package = "tm", "texts", "reuters-21578.xml")

reuters <- system.file("texts", "crude", package = "tm") %>% 
  DirSource(reuters_fname, mode = "binary") %>% 
  VCorpus(readerControl = list(reader = readReut21578XMLasPlain))

reuters_tok <- reuters %>% 
  lapply(`[[`, "content") %>% 
  as.character() %>% 
  gsub("[<>]", "", .) %>% 
  tokenize_words(lowercase = FALSE, stopwords = FALSE, strip_punct = FALSE)
  
docnodes <- lapply(
  1L:length(reuters),
  function(i)
    sprintf(
      '<doc id="%s">\n%s\n</doc>',
      names(reuters[i]),
      paste(reuters_tok[[i]], collapse = "\n")
    )
)

docnodes[[length(docnodes) + 1L]] <- gsub("oil", "pepsi", docnodes[[1]]) %>% 
  gsub('<doc id="127">', '<doc id="127b">', .)

docnodes[[length(docnodes) + 1L]] <- gsub("oil", "coca\ncola", docnodes[[2]])  %>% 
  gsub('<doc id="144">', '<doc id="144b">', .)
  
reuters_xml <- sprintf("<xml>\n%s\n</xml>", paste(docnodes, collapse = "\n"))

cat(reuters_xml, file = file.path(vrt_dir, "reuters.vrt"))

file.remove(list.files(data_dir, full.names = TRUE))

cwb_encode(
  corpus = "REUTERS2",
  registry = registry_dir,
  vrt_dir = vrt_dir,
  data_dir = data_dir,
  encoding = "utf8",
  p_attributes = "word",
  s_attributes = list(doc = "id", xml = character()),
  verbose = TRUE
)
cwb_makeall(corpus = "REUTERS2", p_attribute = "word", registry = registry_dir, quietly = TRUE)
cwb_huffcode(corpus = "REUTERS2", p_attribute = "word", registry = registry_dir, quietly = TRUE)
cwb_compress_rdx(corpus = "REUTERS2", p_attribute = "word", registry = registry_dir, quietly = TRUE)


