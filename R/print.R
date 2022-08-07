#' Print method for explanation objects
#' 
#' Can be used to print the text components of the explanation in an
#' easy-to-read format.
#' 
#' @param explanation The object returned by [explain()]
#' @export
#' 
print.explanation <- function(explanation) {
  explanation$components <- NA_character_
  
  texts <- lapply(
    explanation,
    function(x) if (is.character(x)) return(x) else return(x$explanation)
  )
  texts <- na.omit(unlist(texts))
  cat(paste0(
    texts, collapse = "\n\n"
  ))
  
  invisible(explanation)
}
