#' Transpose a classification object
#' 
#' @note This function can only take one classification (no list of classifications), 
#' see examples.
#' @param class data.frame; a single data.frame holding the data.frame as 
#' returned by \code{\link[taxize]{classification}}.
#' @return a wide data.frame.
#' @export
#' @examples
#' \dontrun{
#' data(samp)
#' # clean taxa names
#' require(taxize)
#' class <- classification(names(samp), db = 'itis')
#' format_class(class[[1]])
#' # many classications
#' require(plyr)
#' rbind.fill(lapply(class, format_class))
#' }
format_class <- function(class){
  levs <- class[ , 'name']
  names(levs) <- class[ , 'rank']
  out <- data.frame(t(levs), stringsAsFactors = FALSE)
  return(out)
}
