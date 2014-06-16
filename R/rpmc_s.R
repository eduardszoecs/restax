#' Remove Parent or Merge Child (RPMC) - One sample variant.
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
#' # transpose data
#' df <- data.frame(t(samp), stringsAsFactors = FALSE)
#' df[ , 'taxon'] <- rownames(df)
#' df_w <- get_hier(df, taxa.var = 'taxon', db = 'itis')
#' rpmc_s(df_w, value.var = 'A')
#' }
rpmc_s <- function(x, value.var = NULL){
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
  
  run <- rev(names(hier))
  run <- run[!run %in% taxa.var]
  
  commout <- comm
  action <- rep(NA, nrow(commout))
  merged <- data.frame(taxon = hier[ , taxa.var], with = NA)
  
  # loop through each parent-child pair
  for(i in seq_along(run)[-1]){
    p <- run[i]
    ch <- run[i-1]
    #     print(p)
    #     print(ch)
    
    # determine childs and parents
    mmm <- merge(hier, comm)  
    take <- mmm[!is.na(mmm[ , p]) , c(p, ch, value.var)]
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
    mm$do <- ifelse(mm[, value.var] < mm$s, 'removed', 'merge')
    # print(mm)
    
    #   remove or merge
    for(k in 1:nrow(mm)){
      if(mm[k, 'do'] == 'removed'){
        comm[comm[ , taxa.var] == mm[k, p], value.var] <- 0
        action[comm[ , taxa.var] == mm[k, p]] <- 'removed'
      }
      if(mm[k, 'do'] == 'merge'){
        comm[hier[ , p] == mm[k, p] & !is.na(hier[ , p]), value.var] <- 0
        comm[comm[ , taxa.var] == mm[k, p], value.var] <- mm[ , value.var] + mm[ , "s"]
        action[hier[ , p] == mm[k, p] & !is.na(hier[ , p])] <- 'merge'
        merged[hier[ , p] == mm[k, p] & !is.na(hier[ , p]), 'with'] <- mm[k , p]
      }
    } 
  }
  action[is.na(action)] <- 'keep'
  
  # keep only value.var
  comm <- comm[ , c(taxa.var, value.var)]
  
  method <- 'RPMC-S'
  out <- list(comm = comm, action = action, merged = merged, 
              method = method)
  class(out) <- 'restax'
  return(out)
}
