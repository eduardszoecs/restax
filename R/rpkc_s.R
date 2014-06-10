#' Remove Parents Keep Children - One sample variant.
#'
#' @param x list; An object of class wide_class as returned by \code{\link[restax]{wide_class}} 
#' @param value.var character; Name of the column holding the abundances
#' @return a list of class 'restax', with the following elements
#' \itemize{
#'  \item comm - resolved community data matrix in wide format.
#'  \item ambp - is the taxon an ambiguous parent?
#'  \item ambc - is the taxon an ambiguous child?
#' }
#' @references Cuffney, T. F., Bilger, M. D. & Haigler, A. M. 
#' Ambiguous taxa: effects on the characterization and interpretation of 
#' invertebrate assemblages. 
#' Journal of the North American Benthological Society 26, 286-307 (2007).
#' @export
#' @examples
#' \dontrun{
#' data(samp)
#' # clean taxa names
#' names(samp) <- gsub(' sp.', '', names(samp))
#' samp_w <- wide_class(samp)
#' rpkc_s(samp_w, value.var = 'A')
#' }

rpkc_s <- function(x, value.var = NULL){
  if(class(x) != 'wide_class')
    stop("Need an object of class 'wide_class'!")
  dfw <- x[[1]]
  hnames <- x[[2]]
  
  dfw <- dfw[c('taxon', hnames, value.var)]
  # rm not diff levels
  keep <- apply(dfw, 2, function(x) any(is.na(x)))
  keep[value.var] <- TRUE
  # keep last level
  keep[rle(keep)$lengths[1]] <- TRUE
  # keep taxon
  keep['taxon'] <- TRUE
  dfw <- dfw[, keep]
  hnames <- hnames[hnames %in% names(keep[keep == TRUE])]
  
  # check amb taxa
  ambc <- rep(FALSE, nrow(dfw)) # children of amb
  ambp <- rep(FALSE, nrow(dfw)) # amb parent
  
  child <- !is.na(dfw[ , 'Species'])
  for(lev in rev(hnames)[-1]){
    parents <- unique(dfw[child, lev])
    ambp <- ambp | dfw[ , lev] %in% parents & !child
    ambc <- ambc | dfw[ , lev] %in% parents & child
    child <- !is.na(dfw[ , lev])
  }
  
  # set amb taxa to zero
  comm <- dfw
  comm[ambp , value.var] <- 0
  
  out <- list(comm = comm, ambp = ambp, ambc = ambc, method = 'RPKC-S')
  class(out) <- 'restax'
  return(out) 
}
