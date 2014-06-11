#' Remove Parent or Merge Child (RPMC) - Group variant.
#' 
#' @param x list; An object of class wide_class as returned by 
#' \code{\link[restax]{wide_class}}. 
#' @param value.var character; Name of the column holding the abundances.
#' @param group character; Names of columns (=samples) for grouping
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
#' rpmc_g(samp_w, value.var = 'S3', group = c('S1', 'S2', 'S3', 'S4'))
#' }
rpmc_g <- function(x, value.var = NULL, group = NULL){
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
  
  
  ## group data
  gg <- rowSums(dfw[, group], na.rm = TRUE) 
  xg <- x
  xg$tcomm <- data.frame(dfw[ , c(hnames, 'taxon')], gg) 
  cg <- rpmc_s(xg, value.var = 'gg')
  
  comm <- dfw[ , c('taxon', hnames, value.var)]
  # rm taxa
  comm[cg$action == 'remove' ,value.var] <- 0
  # merge taxa
  comm_agg <- merge(comm, cg$merged, by = 'taxon')
  agg <- aggregate(comm_agg[, value.var], list(taxon = comm_agg[ , "with"]), sum)
  comm[comm$taxon %in% agg$taxon , value.var] <- agg$x
  
  method = paste0('RPMC-G')
  out <- list(comm = comm, action = cg$action, merged = cg$merged, 
              method = method)
  class(out) <- 'restax'
  return(out) 
}
