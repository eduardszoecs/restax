restax
=============

`restax` is a R package with implements the methods of Cuffney et al. (2007) to
resolve ambiguous taxa.

Currently the following methods are available:

+ Remove parent, keep children (RPKC) : `rpkc_*()`
+ Merge children with parent (MCWP) : `mcwp_*()`
+ Remove parent or merge children (RPMC) : `rpmc_*()`

All methods are available in two variants:

+ `*_s` : Resolve separately for each sample
+ `*_g` : Resolve for a group of samples

### NOTE
This package is currently under development and the code has not been tested extensively!


##### References
Cuffney, T. F., Bilger, M. D. & Haigler, A. M. Ambiguous taxa: effects on the characterization and interpretation of invertebrate assemblages. Journal of the North American Benthological Society 26, 286–307 (2007).
