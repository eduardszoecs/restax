#' Distribute parent among children (DPAC) - One sample variant.
#' 
#' @param x list; An object of class wide_class as returned by 
#' \code{\link[restax]{get_hier}}. 
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
#' @import plyr
#' @export
#' @examples
#' \dontrun{
#' data(samp)
#' samp_w <- wide_class(samp)
#' dpac_s(samp_w, value.var = 'A')
#' }
dpac_s <- function(x, value.var = NULL){
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
  
  # determine amb parents
  tmp <- dfw
  ambp <- rep(FALSE, nrow(dfw)) # amb parent
  child <- !is.na(dfw[ , 'Species'])
  for(lev in rev(hnames)[-1]){
    parents <- unique(dfw[child, lev])
    ambp <- ambp | dfw[ , lev] %in% parents & !child
    child <- !is.na(dfw[ , lev])
  }
  tmp$ambp <- ambp
  # add parent abundance proportianally to childs
  foo <- function(y, value.var){
    childs <- y[y$ambp == FALSE , ]
    parent <- y[y$ambp == TRUE & y[ , value.var] > 0, ]
    if(nrow(parent) > 0 & nrow(childs) > 0){
      new <- childs[ , value.var] + parent[, value.var] * childs[ , value.var] / sum(childs[ , value.var])
      y[y[ , "taxon"] %in% childs[ ,"taxon"]  , value.var] <- new
      y[y[ , "taxon"] %in% parent[ ,"taxon"]  , value.var] <- 0
    }
    return(y)
  }
  
  run <- rev(hnames)[-1]
  for(i in run){
    tmp <- ddply(tmp, i, .fun = foo, value.var)
  }
  # restore order
  tmp <- tmp[match(dfw$taxon, tmp$taxon), ]
  
  comm <- tmp[, names(dfw)]
  
  method <- 'DPAC-S'
  action <- ifelse(tmp$ambp, 'removed', 'added')
  merged <- NULL
  out <- list(comm = comm, action = action, merged = merged, 
              method = method)
  class(out) <- 'restax'
  return(out) 
}
