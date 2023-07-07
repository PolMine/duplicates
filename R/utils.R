#' Reduce characters in vector to a given set of characters
#' 
#' @param x A `character` vector.
#' @param chars Vector with characters to keep.
#' @export
#' @importFrom polmineR p_attributes
#' @examples
#' library(polmineR)
#' use("duplicates") # make REUTERS2 available
#' 
#' vocab <- corpus("REUTERS2") |>
#'   p_attributes(p_attribute = "word") |> 
#'   charfilter(chars = tolower(LETTERS[1:10]))
charfilter <- function(x, chars){
  vocab <- lapply(
    strsplit(x, ""),
    function(str) paste(ifelse(str %in% chars, str, ""), collapse = "")
  )
  unlist(vocab, recursive = FALSE)
}
