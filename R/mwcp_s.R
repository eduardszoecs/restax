#' Merge Children with Parents - One sample variant.
#' 
#' @param x list; An object of class wide_class as returned by 
#' \code{\link[restax]{wide_class}}. 
#' @param value.var character; Name of the column holding the abundances.
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
#' @import plyr
#' @export
#' @examples
#' \dontrun{
#' data(samp)
#' samp_w <- wide_class(samp)
#' samp_w$hnames
#' mcwp_s(samp_w, value.var = 'A', level = 'Family')
#' mcwp_s(samp_w, value.var = 'A', level = 'Order')
#' mcwp_s(samp_w, value.var = 'A', level = 'Class')
#' }
mcwp_s <- function(x, value.var = NULL, level = 'Family'){
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
  
  # check amb taxa
  ambp <- rep(FALSE, nrow(dfw)) # amb parent
  
  child <- !is.na(dfw[ , 'Species'])
  for(lev in rev(hnames)[-1]){
    parents <- unique(dfw[child, lev])
    ambp <- ambp | dfw[ , lev] %in% parents & !child
    child <- !is.na(dfw[ , lev])
  }
  
  # aggregate 
  agg <- aggregate(dfw[ , value.var], list(level = dfw[ , level]), sum)

  # determine highest taxlevel
  dfwc <- dfw[!is.na(dfw[ , level]), ]
  foo <- function(y){
    y[which.max(apply(y, MARGIN = 1, function(x) sum(is.na(x)))), ]
  }
  keep <- ddply(dfwc, level, foo)
  keep[ , value.var] <- agg$x[match(keep[,level], agg$level)]
  
  comm <- dfw
  comm[ , value.var] <- keep[match(comm[ , 'taxon'], keep[ , 'taxon']), value.var]
  comm[ , value.var][is.na(comm[ , value.var])] <- 0
  
  merged <- data.frame(taxon = dfw[ , "taxon"])
  merged$with <- keep$taxon[match(dfw[ , level], keep[, level])]
  
  
  method <- paste0('MCWP-S-', level)
  action <- ifelse(is.na(dfw[ , level]), "removed", 
                   ifelse(merged$taxon == merged$with, 'keep', 
                          'merged'))
  out <- list(comm = comm, action = action, merged = merged, 
              method = method)
  class(out) <- 'restax'
  return(out) 
}
