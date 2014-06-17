#' Merge Children with Parents - Group variant
#' 
#' @param x list; An object of class wide_class as returned by 
#' \code{\link[restax]{get_hier}}. 
#' @param value.var character; Which sampledf_w$ should be resolved.
#' If \code{NULL} all samples are resolved.
#' @param group character; Names of columns (=samples) for grouping.
#' If \code{NULL} all samples are grouped.
#' @param level character; Taxonomic level above taxa will be eliminated. 
#'  Should be returned by \code{\link[restax]{get_hier}}, see hnames therein.
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
#' @export
#' @examples
#' \dontrun{
#' data(samp)
#' # transpose data
#' df <- data.frame(t(samp), stringsAsFactors = FALSE)
#' df[ , 'taxon'] <- rownames(df)
#' df_w <- get_hier(df, taxa.var = 'taxon', db = 'itis')
#' names(df_w$hier)
#' mcwp_g(df_w, value.var = 'S3', group = c('S1', 'S2', 'S3', 'S4'), level = 'Family')
#' # same as using all samples for grouping
#' mcwp_g(df_w, value.var = 'S3', level = 'Family')
#' # Resolvle all samples 
#' mcwp_g(df_w, level = 'Family')
#' mcwp_g(df_w, value.var = 'S3', group = c('S1', 'S2', 'S3', 'S4'), level = 'Order')
#' mcwp_g(df_w, value.var = 'S3', group = c('S1', 'S2', 'S3', 'S4'), level = 'Class')
#' }
mcwp_g <- function(x, value.var = NULL, group = NULL, level = 'Family'){
  if(class(x) != 'wide_class')
    stop("Need an object of class 'wide_class'!")
  comm <- x[['comm']]
  hier <- x[['hier']]
  taxa.var <- x[['taxa.var']]
  if(is.null(value.var)){
    message("Resovling all samples")
    value.var <-  names(comm)[!names(comm) == taxa.var]
  }
  if(!all(value.var %in% names(comm)))
    stop("value.var not found in data")
  if(any(is.na(comm[ , value.var])))
    stop("No NAs in value.var allowed!")
  if(is.null(group)){
    message("Using all samples as group")
    group <- names(comm)[!names(comm) == taxa.var]
  }
  
  ## group data
  gg <- rowSums(comm[, group], na.rm = TRUE) 
  xg <- x
  xg$comm <- data.frame(comm[taxa.var] , gg)
  cg <- mcwp_s(xg, value.var = 'gg', level = level)
  
  comm_agg <- merge(comm, cg$merged, by = taxa.var)
  agg <- aggregate(comm_agg[value.var], list(taxon = comm_agg[ , "with"]), sum)
  
  # rm taxa
  commout <- comm
  commout[, value.var] <- 0
  
  commout[match(agg$taxon, commout[ , taxa.var]) , value.var]  <- agg[ , value.var]
  
  # restore order
  commout <- commout[match(comm[ , taxa.var], commout[ , taxa.var]), ]
  
  # keep only value.var
  commout <- commout[ , c(taxa.var, value.var)]
  
  method = paste0('MCWP-G-', level)
  out <- list(comm = commout, action = cg$action, merged = cg$merged, 
              method = method)
  class(out) <- 'restax'
  return(out) 
}
