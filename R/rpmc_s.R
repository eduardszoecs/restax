#' Remove Parent or Merge Child (RPMC) - One sample variant.
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
#' @import plyr
#' @export
#' @examples
#' \dontrun{
#' data(samp)
#' samp_w <- wide_class(samp)
#' rpmc_s(samp_w, value.var = 'A')
#' }
rpmc_s <- function(x, value.var = NULL){
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
  
  
  run <- rev(hnames)
  comm <- dfw
  action <- rep(NA, nrow(comm))
  merged <- data.frame(taxon = dfw[ , "taxon"], with = NA)
  
  # loop through each parent-child pair
  for(i in seq_along(run)[-1]){
    p <- run[i]
    ch <- run[i-1]
    #     print(p)
    #     print(ch)
    
    # determine childs and parents
    take <- dfw[!is.na(dfw[ , p]) ,c(p, ch, value.var)]
    #     print(take)
    #   # sum of childres
    sum_c <- ddply(take[!is.na(take[ , ch]), ], p, 
                   .fun = function(x, col) {
                     s = sum(x[ , col])
                     n = length(x[ , col])
                     out = data.frame(s, n)
                   }, 
                   value.var)
    # print(sum_c)
    sum_p <- take[is.na(take[ , ch]), ]
    # compare child abundance with parent abundance
    mm <- merge(sum_p, sum_c)
    if(nrow(mm) == 0)
      next
    ##
    mm$do <- ifelse(mm[, value.var] < mm$s, 'remove', 'merge')
    # print(mm)
    
    #   remove or merge
    for(k in 1:nrow(mm)){
      if(mm[k, 'do'] == 'remove'){
        comm[comm$taxon == mm[k, p], value.var] <- 0
        action[comm$taxon == mm[k, p]] <- 'remove'
      }
      if(mm[k, 'do'] == 'merge'){
        comm[comm[ , p] == mm[k, p] & !is.na(comm[ , p]), value.var] <- 0
        comm[comm$taxon == mm[k, p], value.var] <- mm[ , value.var] + mm[ , "s"]
        action[comm[ , p] == mm[k, p] & !is.na(comm[ , p])] <- 'merge'
        merged[comm[ , p] == mm[k, p] & !is.na(comm[ , p]), 'with'] <- mm[k , p]
      }
    } 
  }
  action[is.na(action)] <- 'keep'
  
  method <- 'RPMC-S'
  out <- list(comm = comm, action = action, merged = merged, 
              method = method)
  class(out) <- 'restax'
  return(out)
}
