#' Remove Parents Keep Children - Group variant
#' 
#' @param x list; An object of class wide_class as returned by 
#' \code{\link[restax]{get_hier}}. 
#' @param value.var character; Which sampledf_w$ should be resolved.
#' If \code{NULL} all samples are resolved.
#' @param group character; Names of columns (=samples) for grouping.
#' If \code{NULL} all samples are grouped.
#' @param option character; Currently only C and L options are supported.
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
#' 
#' @export
#' @examples
#' \dontrun{
#' data(samp)
#' df <- samp[1:4, ]
#' df <- data.frame(t(df), stringsAsFactors = FALSE)
#' df[ , 'taxon'] <- rownames(df)
#' df_w <- get_hier(df, taxa.var = 'taxon', db = 'itis')
#' rpkc_g(df_w, value.var = 'S2', group = c('S1', 'S2', 'S3', 'S4'), option = 'L')
#' # group all samples
#' rpkc_g(df_w, value.var = 'S2', option = 'L')
#' # all samples at once
#' rpkc_g(df_w, option = 'L')
#' rpkc_g(df_w, value.var = 'S2', group = c('S1', 'S2', 'S3', 'S4'), option = 'C')
#' }
rpkc_g <- function(x, value.var = NULL, group = NULL, option = c('C', 'K', 'L')){
  if(option == 'K')
    stop("Option K currently not implemented!")
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
  cg <- rpkc_s(xg, value.var = 'gg')
  
  commout <- comm
  for(k in value.var){
    # ambg parents that are removed
    ambrm <- hier[cg$action == 'removed' & comm[ , k] != 0, ]
    # at which level
    levl <- ambrm[ , taxa.var] == ambrm[ , names(ambrm) != taxa.var]
    ambrm$amblev <- names(ambrm[ , names(ambrm) != taxa.var])[apply(levl, MARGIN = 1, 
                                                 FUN = function(x) which(x))]
    for(i in 1:nrow(ambrm)){
      # check if parent has no childs
      assg <- all(comm[hier[ , ambrm$amblev[i]] == ambrm[i, taxa.var] & 
                        !is.na(hier[ , ambrm$amblev[i]]) & 
                        !hier[ , taxa.var] == ambrm[i, taxa.var], k] == 0)
      if(assg){
        # species where abu should be assigned
        take <- comm[hier[ , ambrm$amblev[i]] == ambrm[i, taxa.var] & 
                       !is.na(hier[ , ambrm$amblev[i]]) & 
                       !hier[ , taxa.var] == ambrm[i, taxa.var], ]
        #! no need to do this calc everstime
        ab <- rowSums(take[ , group])
        occ <- rowSums(take[ , group] > 0)
        # weights
        w <- switch(option,
                    L = ab / sum(ab),
                    # K = ...
                    C = ifelse(occ == max(occ), 1, 0))
        # what to assign
        commout[commout[ , taxa.var] %in% take[ , taxa.var], k] <- commout[ambrm[i, taxa.var] == commout[ , taxa.var] , k] * w
      }
    }
    
    # set amb taxa to zero
    commout[cg$action == 'removed' , k] <- 0
  }
  
  
  # keep only value.var
  commout <- commout[ , c(taxa.var, value.var)]
  
  method = paste0("RPKC-G-", option)
  # return
  out <- list(comm = commout, action = cg$action, merged = NULL, method = method)
  class(out) <- 'restax'
  return(out) 
}
