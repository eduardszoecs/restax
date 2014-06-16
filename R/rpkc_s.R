#' Remove Parents Keep Children - One sample variant.
#'
#' @param x list; An object of class wide_class as returned by 
#' \code{\link[restax]{get_hier}}. 
#' @param value.var character; Name of the column holding the abundances.
#' @return a list of class 'restax', with the following elements
#' \itemize{
#'  \item comm - resolved community data (one sample)
#'  \item action - what was done with the taxon
#'  \item merged - is the taxon merged
#'  \item method - method to resolve taxa
#' }
#' @references Cuffney, T. F., Bilger, M. D. & Haigler, A. M. 
#' Ambiguous taxa: effects on the characterization and interpretation of 
#' invertebrate assemblages. 
#' Journal of the North American Benthological Society 26, 286-307 (2007).
#' @export
#' @examples
#' \dontrun{
#' data(samp)
#' # transpose data
#' df <- data.frame(t(samp), stringsAsFactors = FALSE)
#' df[ , 'taxon'] <- rownames(df)
#' df_w <- get_hier(df, taxa.var = 'taxon', db = 'itis')
#' rpkc_s(df_w, value.var = 'A')
#' }

rpkc_s <- function(x, value.var = NULL){
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

  # check amb parents
  run <- rev(names(hier))
  run <- run[!run %in% c(taxa.var, "Species")]
  ambp <- rep(FALSE, nrow(comm)) # amb parent
  child <- !is.na(hier[ , 'Species'])
  for(lev in run){
    parents <- unique(hier[child, lev])
    ambp <- ambp | hier[ , lev] %in% parents & !child
    child <- !is.na(hier[ , lev])
  }
  
  # Remove parents
  comm[ambp , value.var] <- 0
  
  # keep only value.var
  comm <- comm[ , c(taxa.var, value.var)]
  
  
  out <- list(comm = comm, action = ifelse(ambp, "removed", "keep") , merged = NULL, method = 'RPKC-S')
  class(out) <- 'restax'
  return(out) 
}
