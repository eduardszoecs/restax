#' Remove Parents Keep Children - One sample variant.
#'
#' @param x list; An object of class wide_class as returned by 
#' \code{\link[restax]{wide_class}}. 
#' @param value.var character; Name of the column holding the abundances.
#' @return a list of class 'restax', with the following elements
#' \itemize{
#'  \item comm - resolved community data matrix in wide format.
#'  \item action - what was done with the taxon
#'  \item merged - is the taxon merged
#' }
#' @references Cuffney, T. F., Bilger, M. D. & Haigler, A. M. 
#' Ambiguous taxa: effects on the characterization and interpretation of 
#' invertebrate assemblages. 
#' Journal of the North American Benthological Society 26, 286-307 (2007).
#' @export
#' @examples
#' \dontrun{
#' data(samp)
#' samp_w <- wide_class(samp)
#' rpkc_s(samp_w, value.var = 'A')
#' }

rpkc_s <- function(x, value.var = NULL){
  if(class(x) != 'wide_class')
    stop("Need an object of class 'wide_class'!")
  if(is.null(value.var))
    stop("Must specify value.var!")
  dfw <- x[[1]]
  hnames <- x[[2]]
  if(!value.var %in% names(dfw))
    stop("value.var not found in data")
  
  if(any(is.na(dfw[ , value.var])))
     stop("No NAs in value.var allowed!")
  
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
  
  # check amb parents
  ambp <- rep(FALSE, nrow(dfw)) # amb parent
  
  child <- !is.na(dfw[ , 'Species'])
  for(lev in rev(hnames)[-1]){
    parents <- unique(dfw[child, lev])
    ambp <- ambp | dfw[ , lev] %in% parents & !child
    child <- !is.na(dfw[ , lev])
  }
  
  # set amb taxa to zero
  comm <- dfw
  comm[ambp , value.var] <- 0
  
  out <- list(comm = comm, action = ifelse(ambp, "removed", "keep") , merged = NULL, method = 'RPKC-S')
  class(out) <- 'restax'
  return(out) 
}
