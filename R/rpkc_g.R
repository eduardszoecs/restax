#' Remove Parents Keep Children - Group variant
#' 
#' @param x list; An object of class wide_class as returned by 
#' \code{\link[restax]{wide_class}}. 
#' @param value.var character; Name of the column holding the abundances to resolve.
#' @param group character; Names of columns (=samples) for grouping.
#' @param option character; Currently only C and L options are supported.
#' @return a list of class 'restax', with the following elements
#' \itemize{
#'  \item comm - resolved community data matrix in wide format.
#'  \item removed - is the taxon removed
#'  \item merged - is the taxon merged
#' }
#' @references Cuffney, T. F., Bilger, M. D. & Haigler, A. M. 
#' Ambiguous taxa: effects on the characterization and interpretation of 
#' invertebrate assemblages. 
#' Journal of the North American Benthological Society 26, 286-307 (2007).
#' 
#' @export
#' @examples
#' \dontrun{
#' data(samp)
#' samp_w <- wide_class(samp)
#' rpkc_g(samp_w, value.var = 'S2', group = c('S1', 'S2', 'S3', 'S4'), option = 'L')
#' rpkc_g(samp_w, value.var = 'S2', group = c('S1', 'S2', 'S3', 'S4'), option = 'C')
#' }
rpkc_g <- function(x, value.var = NULL, group = NULL, option = c('C', 'K', 'L')){
  if(option == 'K')
    stop("Option K currently not implemented!")
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
  cg <- rpkc_s(xg, value.var = 'gg')
  
  # set amb taxa to zero
  comm <- dfw[ , c('taxon', hnames, value.var)]
  comm[cg$removed , value.var] <- 0
  
  # check if a amb taxon that is removed has no children
  ambrm <- dfw[cg$removed & dfw[, value.var] != 0, ]
  levl <- ambrm$taxon == ambrm[, hnames]
  ambrm$amblev <- names(ambrm[, hnames])[apply(levl, MARGIN = 1, 
                                               FUN = function(x) which(x))]
  
  for(i in 1:nrow(ambrm)){
    assg <- all(dfw[dfw[ , ambrm$amblev[i]] == ambrm$taxon[i] & 
                      !is.na(dfw[ , ambrm$amblev[i]]) & 
                      !dfw[, "taxon"] ==  ambrm$taxon[i], value.var] == 0)
    if(assg){
      # species where abu should be assigned
      take <- dfw[dfw[ , ambrm$amblev[i]] == ambrm$taxon[i] & 
                    !is.na(dfw[ , ambrm$amblev[i]]) & 
                    !dfw[, "taxon"] ==  ambrm$taxon[i], ]
      # do need to do this calc everstime
      ab <- rowSums(take[ , group])
      occ <- rowSums(take[ , group] > 0)
      # weights
      w <- switch(option,
                  L = ab / sum(ab),
                  # K = ifelse(ab == max(ab), 1, 0),
                  C = ifelse(occ == max(occ), 1, 0))
      # what to assign
      comm[comm[, 'taxon'] %in% take[ , 'taxon'], value.var] <- ambrm[i, value.var] * w
    }
  }
  
  method = paste0("RPKC-G-", option)
  # return
  out <- list(comm = comm, removed = cg$removed, merged = NULL, method = method)
  class(out) <- 'restax'
  return(out) 
}
