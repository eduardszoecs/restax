restax
=============


[![Build Status](https://travis-ci.org/EDiLD/restax.png)](https://travis-ci.org/EDiLD/restax)

`restax` is a R package with implements the methods of Cuffney et al. (2007) to
resolve ambiguous taxa.

### Methods
Currently the following functions are available:

+ Remove parent, keep children (RPKC) : `rpkc_*()`
+ Merge children with parent (MCWP) : `mcwp_*()`
+ Remove parent or merge children (RPMC) : `rpmc_*()`

### Variants
All functions are available in two variants:

+ `*_s` : Resolve separately for each sample.
+ `*_g` : Resolve for a group of samples.

### Options
Moreover, some functions have additional options:

##### rpkc_g()
+ `option = 'C'` : if the ambiguous parent has no child in a sample, 
substitute the most frequently occurring child for the parent.
+ `option = 'L'` : if the ambiguous parent has no child in a sample, 
substitute all of the children associated with the parent in the grouped data.

##### mcwp_*()

+ `level = 'Family'` : remove all ambiguous parents above the specified level 
(here family) before resolving ambiguous taxa.


#### NOTE
This package is currently under development and the code has not been tested extensively!

It currently can reproduce the appendix of Cuffney et al. (2007), but may break with other data!

Please use only ITIS as taxonomic backend, as others have not been tested yet.

Please [report any issues or bugs](https://github.com/EDiLD/restax/issues).


##### References
Cuffney, T. F., Bilger, M. D. & Haigler, A. M. Ambiguous taxa: effects on the characterization and interpretation of invertebrate assemblages. Journal of the North American Benthological Society 26, 286–307 (2007).
