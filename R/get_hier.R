#' Add classification information to community data
#' @param x data.frame; community data. Nore this must be in transposed wide format.
#' (taxa in rows, samples in columns). See examples.
#' @param db character; database to retrieve classification data. 
#' See  \code{\link[taxize]{classification}} for more details.
#' @param taxa.var character; column of taxon names.
#' @param ...  Other arguments passed to \code{\link[taxize]{classification}}, 
#' @return A list of class wide_class, with the following elements:
#' \itemize{
#'  \item comm - community data.matrix
#'  \item hier - taxonomic hierarchy
#'  \item taxa.var - column of taxon names.
#' }
#' @import taxize
#' @export
#' @examples
#' \dontrun{
#' data(samp)
#' # transpose data
#' df <- data.frame(t(samp), stringsAsFactors = FALSE)
#' df[ , 'taxon'] <- rownames(df)
#' 
#' # clean taxa names
#' get_hier(df, taxa.var = 'taxon', db = 'itis')
#' }
get_hier <- function(x, taxa.var = 'NULL', db = 'itis', ...){
  # get classification
  hier <- classification(x[ , taxa.var], db, ...)
  # reformat classification
  wide <- rbind.fill(lapply(hier, format_class))
  # convert to char
  wide <- data.frame(lapply(wide, as.character), stringsAsFactors=FALSE)
  hnames <- names(wide)
  # add taxon
  wide[ , taxa.var] <- x[ , taxa.var]
  comm <- merge(wide, x, by = taxa.var)
  
  # restore order
  comm <- comm[match(x[ , taxa.var], comm[ , taxa.var]), ]
  
  hier <- comm[ , c(hnames, taxa.var)]
  comm <- comm[ , names(x)]
  
  out <- list(comm = comm, hier = hier, taxa.var = taxa.var)
  class(out) <- 'wide_class'
  return(out)
}
