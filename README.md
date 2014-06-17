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
+ Distribute parent among children (DPAC) : `dpac_*()`

### Variants
All functions are available in two variants:

+ `*_s` : Resolve separately for each sample.
+ `*_g` : Resolve for a group of samples.

### Options
Moreover, some functions have additional options:

##### rpkc_g() and dpac_g()
+ `option = 'C'` : if the ambiguous parent has no child in a sample, 
substitute the most frequently occurring child for the parent.
+ `option = 'L'` : if the ambiguous parent has no child in a sample, 
substitute all of the children associated with the parent in the grouped data.

##### mcwp_*()

+ `level = 'Family'` : remove all ambiguous parents above the specified level 
(here family) before resolving ambiguous taxa.

Installation
==============
`restax` is currently only available on github. To install `restax` use:

```r
install.packages('devtools')
require(devtools)
install_github('restax', 'EDiLD')
require(restax)
```


Example
=================
Let's say we have our data in the wide format (as commonly used in community ecology).
Here we have data of 5 samples (S1-S4, A) with 12 taxa:

```r
require(restax)
data(samp)
samp
```

```
##    Insecta Ephemeroptera Baetidae Acentrella Acentrella parvula
## S1       0             0        4         10                 34
## S2       6             0        0         30                  0
## S3       0           100        0          5                 26
## S4       0             0        8          0                 11
## A        6           100       12         45                 71
##    Acentrella turbida Baetis Baetis flavistriga Baetis intercalaris
## S1                  3      0                 54                  78
## S2                  0     44                  0                  21
## S3                  0     35                  0                   0
## S4                  9    100                  6                   0
## A                  12    179                 60                  99
##    Baetis pluto Zygoptera Argia
## S1           23         0     0
## S2           23         0     0
## S3            0        10     0
## S4           12       100     8
## A            58       110     8
```


`restax` currently accepts only the transposed wide format. So we need to reformat it

```r
df <- data.frame(t(samp), stringsAsFactors = FALSE)
df[ , 'taxon'] <- rownames(df)
df
```

```
##                     S1 S2  S3  S4   A               taxon
## Insecta              0  6   0   0   6             Insecta
## Ephemeroptera        0  0 100   0 100       Ephemeroptera
## Baetidae             4  0   0   8  12            Baetidae
## Acentrella          10 30   5   0  45          Acentrella
## Acentrella parvula  34  0  26  11  71  Acentrella parvula
## Acentrella turbida   3  0   0   9  12  Acentrella turbida
## Baetis               0 44  35 100 179              Baetis
## Baetis flavistriga  54  0   0   6  60  Baetis flavistriga
## Baetis intercalaris 78 21   0   0  99 Baetis intercalaris
## Baetis pluto        23 23   0  12  58        Baetis pluto
## Zygoptera            0  0  10 100 110           Zygoptera
## Argia                0  0   0   8   8               Argia
```


The first step is to retrieve taxonomic information about our taxa:

This is done via the `get_hier()` function,
which uses [taxize](https://github.com/ropensci/taxize) to query ITIS:

```r
df_w <- get_hier(df, taxa.var = 'taxon', db = 'itis')
df_w
```

```
## $comm
##    S1 S2  S3  S4   A               taxon
## 11  0  6   0   0   6             Insecta
## 10  0  0 100   0 100       Ephemeroptera
## 5   4  0   0   8  12            Baetidae
## 1  10 30   5   0  45          Acentrella
## 2  34  0  26  11  71  Acentrella parvula
## 3   3  0   0   9  12  Acentrella turbida
## 6   0 44  35 100 179              Baetis
## 7  54  0   0   6  60  Baetis flavistriga
## 8  78 21   0   0  99 Baetis intercalaris
## 9  23 23   0  12  58        Baetis pluto
## 12  0  0  10 100 110           Zygoptera
## 4   0  0   0   8   8               Argia
## 
## $hier
##     Kingdom Subkingdom Infrakingdom Superphylum     Phylum Subphylum
## 11 Animalia  Bilateria  Protostomia   Ecdysozoa Arthropoda  Hexapoda
## 10 Animalia  Bilateria  Protostomia   Ecdysozoa Arthropoda  Hexapoda
## 5  Animalia  Bilateria  Protostomia   Ecdysozoa Arthropoda  Hexapoda
## 1  Animalia  Bilateria  Protostomia   Ecdysozoa Arthropoda  Hexapoda
## 2  Animalia  Bilateria  Protostomia   Ecdysozoa Arthropoda  Hexapoda
## 3  Animalia  Bilateria  Protostomia   Ecdysozoa Arthropoda  Hexapoda
## 6  Animalia  Bilateria  Protostomia   Ecdysozoa Arthropoda  Hexapoda
## 7  Animalia  Bilateria  Protostomia   Ecdysozoa Arthropoda  Hexapoda
## 8  Animalia  Bilateria  Protostomia   Ecdysozoa Arthropoda  Hexapoda
## 9  Animalia  Bilateria  Protostomia   Ecdysozoa Arthropoda  Hexapoda
## 12 Animalia  Bilateria  Protostomia   Ecdysozoa Arthropoda  Hexapoda
## 4  Animalia  Bilateria  Protostomia   Ecdysozoa Arthropoda  Hexapoda
##      Class  Subclass  Infraclass         Order   Suborder         Family
## 11 Insecta      <NA>        <NA>          <NA>       <NA>           <NA>
## 10 Insecta Pterygota Palaeoptera Ephemeroptera       <NA>           <NA>
## 5  Insecta Pterygota Palaeoptera Ephemeroptera Pisciforma       Baetidae
## 1  Insecta Pterygota Palaeoptera Ephemeroptera Pisciforma       Baetidae
## 2  Insecta Pterygota Palaeoptera Ephemeroptera Pisciforma       Baetidae
## 3  Insecta Pterygota Palaeoptera Ephemeroptera Pisciforma       Baetidae
## 6  Insecta Pterygota Palaeoptera Ephemeroptera Pisciforma       Baetidae
## 7  Insecta Pterygota Palaeoptera Ephemeroptera Pisciforma       Baetidae
## 8  Insecta Pterygota Palaeoptera Ephemeroptera Pisciforma       Baetidae
## 9  Insecta Pterygota Palaeoptera Ephemeroptera Pisciforma       Baetidae
## 12 Insecta Pterygota Palaeoptera       Odonata  Zygoptera           <NA>
## 4  Insecta Pterygota Palaeoptera       Odonata  Zygoptera Coenagrionidae
##         Genus             Species               taxon
## 11       <NA>                <NA>             Insecta
## 10       <NA>                <NA>       Ephemeroptera
## 5        <NA>                <NA>            Baetidae
## 1  Acentrella                <NA>          Acentrella
## 2  Acentrella  Acentrella parvula  Acentrella parvula
## 3  Acentrella  Acentrella turbida  Acentrella turbida
## 6      Baetis                <NA>              Baetis
## 7      Baetis  Baetis flavistriga  Baetis flavistriga
## 8      Baetis Baetis intercalaris Baetis intercalaris
## 9      Baetis        Baetis pluto        Baetis pluto
## 12       <NA>                <NA>           Zygoptera
## 4       Argia                <NA>               Argia
## 
## $taxa.var
## [1] "taxon"
## 
## attr(,"class")
## [1] "wide_class"
```
We need to specify the column containing the taxon names.
This function returns the original data and accompanying taxonomic information.

To resolve ambiguous taxa using the `RPKC-S` method (remove parents, keep childs - per sample) for sample A we use:

```r
df_res <- rpkc_s(df_w, value.var = 'A')
df_res$comm
```

```
##                  taxon  A
## 11             Insecta  0
## 10       Ephemeroptera  0
## 5             Baetidae  0
## 1           Acentrella  0
## 2   Acentrella parvula 71
## 3   Acentrella turbida 12
## 6               Baetis  0
## 7   Baetis flavistriga 60
## 8  Baetis intercalaris 99
## 9         Baetis pluto 58
## 12           Zygoptera  0
## 4                Argia  8
```
We see that only the 5 Baetidae species and Argia sp. are kept, all others (as they are parents) are set to zero.


Similarly, all samples can be resolved with on call by omitting `value.var`. However, this currently works only with group variants
`*_g()` . If `group` is ommited all samples are used as grouping variables.

Here we resolve all samples simultaneously using all samples as group using the `RPMC-G` variant:


```r
# remove sample A
df <- data.frame(t(samp[1:4, ]), stringsAsFactors = FALSE)
df[ , 'taxon'] <- rownames(df)
# get hierachy
df_w <- get_hier(df, taxa.var = 'taxon', db = 'itis')
# resolve
df_rpmc <- rpmc_g(df_w)
# same as
# rpmc_g(df_w, group = group = c('S1', 'S2', 'S3', 'S4'))
df_rpmc$comm
```

```
##                  taxon S1 S2 S3  S4
## 11             Insecta  0  0  0   0
## 10       Ephemeroptera  0  0  0   0
## 5             Baetidae  0  0  0   0
## 1           Acentrella  0  0  0   0
## 2   Acentrella parvula 34  0 26  11
## 3   Acentrella turbida  3  0  0   9
## 6               Baetis  0  0  0   0
## 7   Baetis flavistriga 54  0  0   6
## 8  Baetis intercalaris 78 21  0   0
## 9         Baetis pluto 23 23  0  12
## 12           Zygoptera  0  0 10 108
## 4                Argia  0  0  0   0
```





NOTES
=============
This package is currently under development and the code has not been tested extensively!
Moreover, there is some work needed to make the package more user friendly!

It currently can reproduce the appendix of Cuffney et al. (2007), but may break with other data!

Please use only ITIS as taxonomic back-end, as others have not been tested yet.

`dpac_g()` and `option='K'` are currently not available.

Please [report any issues or bugs](https://github.com/EDiLD/restax/issues).

TODOs
=============

+ Remove dependency of `mwcp()` and `rpkc()` on `"Species"` in hierarchy.
+ ADD DPAC_g
+ No classification information available?


### References
Cuffney, T. F., Bilger, M. D. & Haigler, A. M. Ambiguous taxa: effects on the characterization and interpretation of invertebrate assemblages. Journal of the North American Benthological Society 26, 286–307 (2007).
