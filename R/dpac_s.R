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
  
  lev <- rev(names(hier))
  lev <- lev[!lev %in% taxa.var]
  foo <- function(y, value.var){
    childs <- !is.na(y[ , which(names(y) == i) + 1])
    parent <- !childs
    if(sum(y[childs, value.var]) == 0 | all(childs)){
      # no action
      return(y)
    } else {
      # add parents to childs (wighted)
      w <- y[childs, value.var] / sum(y[childs, value.var])
      y[childs, value.var] <- y[childs, value.var]  + y[parent, value.var] * w
      # rm parents
      y[parent, value.var] <- 0
      y$ambp[parent] <- TRUE
      return(y)
    }
  }
  wdf <- cbind(hier, comm)
  wdf$ambp <- FALSE
  for(i in lev[-1]){
    # loop throug all parent - child combinations
    wdf <- ddply(wdf, i, foo,value.var)
  }
      
  # restore order
  wdf <- wdf[match(comm[ , taxa.var], wdf[, taxa.var]), ]
  
  # keep only value.var
  commout <- wdf[ , c(taxa.var, value.var)]
  
  method <- 'DPAC-S'
  action <- ifelse(wdf$ambp, 'removed', 'added')
  merged <- NULL
  out <- list(comm = commout, action = action, merged = merged, 
              method = method)
  class(out) <- 'restax'
  return(out) 
}
