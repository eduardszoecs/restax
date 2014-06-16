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
#'  \item method - method to resolve taxa
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
  comm <- x[['comm']]
  hier <- x[['hier']]
  taxa.var <- x[['taxa.var']]
  if(!value.var %in% names(comm))
    stop("value.var not found in data")
  if(any(is.na(comm[ , value.var])))
    stop("No NAs in value.var allowed!")
  
  # rm not indiff levels
  keep <- apply(hier, 2, function(x) any(is.na(x)))
  # keep last level
  keep[rle(keep)$lengths[1]] <- TRUE
  # keep taxon
  keep[taxa.var] <- TRUE
  hier <- hier[, keep]
  
  # determine amb parents
  run <- rev(names(hier))
  run <- run[!run %in% c(taxa.var, "Species")]
  ambp <- rep(FALSE, nrow(comm)) # amb parent
  child <- !is.na(hier[ , 'Species'])
  for(lev in run){
    parents <- unique(hier[child, lev])
    ambp <- ambp | hier[ , lev] %in% parents & !child
    child <- !is.na(hier[ , lev])
  }
  # print(ambp)
  
  
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
  
  tmp <- data.frame(comm , hier, ambp)
  for(i in run){
    tmp <- ddply(tmp, i, .fun = foo, value.var)
  }
  # restore order
  tmp <- tmp[match(comm[ , taxa.var], tmp[, taxa.var]), ]
  
  # keep only value.var
  comm <- tmp[ ,c(taxa.var, value.var)]
  
  method <- 'DPAC-S'
  action <- ifelse(tmp$ambp, 'removed', 'added')
  merged <- NULL
  out <- list(comm = comm, action = action, merged = merged, 
              method = method)
  class(out) <- 'restax'
  return(out) 
}
