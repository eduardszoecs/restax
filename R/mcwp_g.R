#' Merge Children with Parents - Group variant
#' 
#' @param x list; An object of class wide_class as returned by 
#' \code{\link[restax]{wide_class}}. 
#' @param value.var character; Name of the column holding the abundances to resolve
#' @param group character; Names of columns (=samples) for grouping
#' @param level character; Taxonomic level above taxa will be eliminated. 
#'  Should be returned by \code{\link[restax]{wide_class}}, see hnames therein.
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
#' samp_w$hnames
#' mcwp_g(samp_w, value.var = 'S2', group = c('S1', 'S2', 'S3', 'S4'), level = 'Family')
#' mcwp_g(samp_w, value.var = 'S2', group = c('S1', 'S2', 'S3', 'S4'), level = 'Order')
#' mcwp_g(samp_w, value.var = 'S2', group = c('S1', 'S2', 'S3', 'S4'), level = 'Class')
#' }
mcwp_g <- function(x, value.var = NULL, group = NULL, level = 'Family'){
  if(class(x) != 'wide_class')
    stop("Need an object of class 'wide_class'!")
  if(is.null(value.var))
    stop("Must specify value.var!")
  if(is.null(group))
    stop("Must specify group!")
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
  cg <- mcwp_s(xg, value.var = 'gg', level = level)
  
  
  comm <- dfw[ , c('taxon', hnames, value.var)]
  comm_agg <- merge(comm, cg$merged, by = 'taxon')
  agg <- aggregate(comm_agg[, value.var], list(taxon = comm_agg[ , "with"]), sum)
  comm[ , value.var] <- 0
  comm[comm$taxon %in% agg$taxon , value.var] <- agg$x
  
  method = paste0('MCWP-G-', level)
  out <- list(comm = comm, action = cg$action, merged = cg$merged, 
              method = method)
  class(out) <- 'restax'
  return(out) 
}
