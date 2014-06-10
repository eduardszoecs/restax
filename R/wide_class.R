#' Add classification information to community data
#' @param comm data.frame; community data in wide format (samples in rows, species in cols).
#' @param db character; database to retrieve classification data. 
#' See  \code{\link[taxize]{classification}} for more details.
#' @return A list of class wide_class, with the following elements:
#' \itemize{
#'  \item tcomm - the transposed datamatirx with added taxonomic information
#'  \item hnames - the names of the columns added
#' }
#' @import taxize
#' @export
#' @examples
#' \dontrun{
#' data(samp)
#' # clean taxa names
#' wide_class(samp)
#' }
wide_class <- function(comm, db = 'itis'){
  # transpose comm
  df <- data.frame(t(comm), stringsAsFactors = FALSE)
  df[ , 'taxon'] <- rownames(df)
  # get classification
  hier <- classification(df[ , 'taxon'], db)
  # reformat classification
  wide <- rbind.fill(lapply(hier, format_class))
  # convert to char
  wide <- data.frame(lapply(wide, as.character), stringsAsFactors=FALSE)
  hnames <- names(wide)
  # add taxon
  wide[ , 'taxon'] <- df[ , 'taxon']
  tcomm <- merge(wide, df, by = 'taxon')
  out <- list(tcomm = tcomm, hnames = hnames)
  class(out) <- 'wide_class'
  return(out)
}
