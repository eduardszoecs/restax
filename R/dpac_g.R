dpac_g <- function(x, value.var = NULL, group = NULL){
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
  gg <- rowSums(comm[ , group], na.rm = TRUE) 
  xg <- x
  xg$comm[ , 'gg'] <- gg
  cg <- rpmc_s(xg, value.var = 'gg')
  comm_agg <- merge(comm, cg$merged, by = taxa.var)
  
}
