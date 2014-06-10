#' Function format a classification object to a wide table
#' @note This function can only take one classification (no list of classifications), 
#' see examples.
#' @param class data.frame; a single data.frame holding the data.frame as returned by classification
#' @return a wide data.frame.
#' @export
#' @examples
#' \dontrun{
#' data(samp)
#' # clean taxa names
#' taxa <- gsub(' sp.', '', names(samp))
#' require(taxize)
#' require(plyr)
#' class <- classification(taxa, db = 'itis')
#' format_class(class[[1]])
#' rbind.fill(lapply(class, format_class))
#' }
format_class <- function(class){
  levs <- class[ , 'name']
  names(levs) <- class[ , 'rank']
  out <- data.frame(t(levs))
  return(out)
}
