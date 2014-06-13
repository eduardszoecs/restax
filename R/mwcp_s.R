#' Merge Children with Parents - One sample variant.
#' 
#' @param x list; An object of class wide_class as returned by 
#' \code{\link[restax]{get_hier}}. 
#' @param value.var character; Name of the column holding the abundances.
#' @param level character; Taxonomic level above taxa will be eliminated. 
#'  Should be returned by \code{\link[restax]{get_hier}}, see hnames therein.
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
#' @import plyr
#' @export
#' @examples
#' \dontrun{
#' data(samp)
#' # transpose data
#' df <- data.frame(t(samp), stringsAsFactors = FALSE)
#' df[ , 'taxon'] <- rownames(df)
#' df_w <- get_hier(df, taxa.var = 'taxon', db = 'itis')
#' names(df_w$hier)
#' mcwp_s(df_w, value.var = 'A', level = 'Family')
#' mcwp_s(df_w, value.var = 'A', level = 'Order')
#' mcwp_s(df_w, value.var = 'A', level = 'Class')
#' }
mcwp_s <- function(x, value.var = NULL, level = 'Family'){
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
  
  # check amb taxa
  ambp <- rep(FALSE, nrow(hier)) # amb parent
  
  run <- rev(names(hier))
  run <- run[!run %in% c(taxa.var, "Species")]
  ambp <- rep(FALSE, nrow(comm)) # amb parent
  child <- !is.na(hier[ , 'Species'])
  for(lev in run){
    parents <- unique(hier[child, lev])
    ambp <- ambp | hier[ , lev] %in% parents & !child
    child <- !is.na(hier[ , lev])
  }
  
  # aggregate 
  agg <- aggregate(comm[ , value.var], list(level = hier[ , level]), sum)

  # determine highest taxlevel
  dfwc <- hier[!is.na(hier[ , level]), ]
  foo <- function(y){
    y[which.max(apply(y, MARGIN = 1, function(x) sum(is.na(x)))), ]
  }
  keep <- ddply(dfwc, level, foo)
  keep[ , value.var] <- agg$x[match(keep[ , level], agg$level)]
  
  comm[ , value.var] <- keep[match(comm[ , taxa.var], keep[ , taxa.var]), value.var]
  comm[ , value.var][is.na(comm[ , value.var])] <- 0
  
  merged <- data.frame(taxon = comm[ , taxa.var])
  merged$with <- keep[match(hier[ , level], keep[, level]), taxa.var]
  
  # keep only value.var
  comm <- comm[ ,c(taxa.var, value.var)]
  
  
  method <- paste0('MCWP-S-', level)
  action <- ifelse(is.na(hier[ , level]), "removed", 
                   ifelse(merged$taxon == merged$with, 'keep', 
                          'merged'))
  out <- list(comm = comm, action = action, merged = merged, 
              method = method)
  class(out) <- 'restax'
  return(out) 
}
