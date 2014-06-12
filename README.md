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

### Installation
`restax` is currently only available on github. To install `restax` use:

```r
install.packages('devtools')
require(devtools)
install_github('restax', 'EDiLD')
require(restax)
```


### Example
Let's say we have our data in the wide format (as commonly used in community ecology):

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

First we need to retrieve taxonomic information about our taxa. 
This is done via the `wide_class()` function, which uses [taxize](https://github.com/ropensci/taxize) to query ITIS:

```r
df <- wide_class(samp, db = 'itis')
df$tcomm
```

```
##                  taxon  Kingdom Subkingdom Infrakingdom Superphylum
## 1           Acentrella Animalia  Bilateria  Protostomia   Ecdysozoa
## 2   Acentrella parvula Animalia  Bilateria  Protostomia   Ecdysozoa
## 3   Acentrella turbida Animalia  Bilateria  Protostomia   Ecdysozoa
## 4                Argia Animalia  Bilateria  Protostomia   Ecdysozoa
## 5             Baetidae Animalia  Bilateria  Protostomia   Ecdysozoa
## 6               Baetis Animalia  Bilateria  Protostomia   Ecdysozoa
## 7   Baetis flavistriga Animalia  Bilateria  Protostomia   Ecdysozoa
## 8  Baetis intercalaris Animalia  Bilateria  Protostomia   Ecdysozoa
## 9         Baetis pluto Animalia  Bilateria  Protostomia   Ecdysozoa
## 10       Ephemeroptera Animalia  Bilateria  Protostomia   Ecdysozoa
## 11             Insecta Animalia  Bilateria  Protostomia   Ecdysozoa
## 12           Zygoptera Animalia  Bilateria  Protostomia   Ecdysozoa
##        Phylum Subphylum   Class  Subclass  Infraclass         Order
## 1  Arthropoda  Hexapoda Insecta Pterygota Palaeoptera Ephemeroptera
## 2  Arthropoda  Hexapoda Insecta Pterygota Palaeoptera Ephemeroptera
## 3  Arthropoda  Hexapoda Insecta Pterygota Palaeoptera Ephemeroptera
## 4  Arthropoda  Hexapoda Insecta Pterygota Palaeoptera       Odonata
## 5  Arthropoda  Hexapoda Insecta Pterygota Palaeoptera Ephemeroptera
## 6  Arthropoda  Hexapoda Insecta Pterygota Palaeoptera Ephemeroptera
## 7  Arthropoda  Hexapoda Insecta Pterygota Palaeoptera Ephemeroptera
## 8  Arthropoda  Hexapoda Insecta Pterygota Palaeoptera Ephemeroptera
## 9  Arthropoda  Hexapoda Insecta Pterygota Palaeoptera Ephemeroptera
## 10 Arthropoda  Hexapoda Insecta Pterygota Palaeoptera Ephemeroptera
## 11 Arthropoda  Hexapoda Insecta      <NA>        <NA>          <NA>
## 12 Arthropoda  Hexapoda Insecta Pterygota Palaeoptera       Odonata
##      Suborder         Family      Genus             Species S1 S2  S3  S4
## 1  Pisciforma       Baetidae Acentrella                <NA> 10 30   5   0
## 2  Pisciforma       Baetidae Acentrella  Acentrella parvula 34  0  26  11
## 3  Pisciforma       Baetidae Acentrella  Acentrella turbida  3  0   0   9
## 4   Zygoptera Coenagrionidae      Argia                <NA>  0  0   0   8
## 5  Pisciforma       Baetidae       <NA>                <NA>  4  0   0   8
## 6  Pisciforma       Baetidae     Baetis                <NA>  0 44  35 100
## 7  Pisciforma       Baetidae     Baetis  Baetis flavistriga 54  0   0   6
## 8  Pisciforma       Baetidae     Baetis Baetis intercalaris 78 21   0   0
## 9  Pisciforma       Baetidae     Baetis        Baetis pluto 23 23   0  12
## 10       <NA>           <NA>       <NA>                <NA>  0  0 100   0
## 11       <NA>           <NA>       <NA>                <NA>  0  6   0   0
## 12  Zygoptera           <NA>       <NA>                <NA>  0  0  10 100
##      A
## 1   45
## 2   71
## 3   12
## 4    8
## 5   12
## 6  179
## 7   60
## 8   99
## 9   58
## 10 100
## 11   6
## 12 110
```
This
+ transposes our data
+ add taxomic information
+ returns an object of class `wide_class`.

To resolve ambiguous taxa using the `RPKC-S` method (remove parents, keep childs - per sample) for sample A we use:


```r
df_res <- rpkc_s(df, value.var = 'A')
df_res$comm
```

```
##                  taxon   Class  Subclass  Infraclass         Order
## 1           Acentrella Insecta Pterygota Palaeoptera Ephemeroptera
## 2   Acentrella parvula Insecta Pterygota Palaeoptera Ephemeroptera
## 3   Acentrella turbida Insecta Pterygota Palaeoptera Ephemeroptera
## 4                Argia Insecta Pterygota Palaeoptera       Odonata
## 5             Baetidae Insecta Pterygota Palaeoptera Ephemeroptera
## 6               Baetis Insecta Pterygota Palaeoptera Ephemeroptera
## 7   Baetis flavistriga Insecta Pterygota Palaeoptera Ephemeroptera
## 8  Baetis intercalaris Insecta Pterygota Palaeoptera Ephemeroptera
## 9         Baetis pluto Insecta Pterygota Palaeoptera Ephemeroptera
## 10       Ephemeroptera Insecta Pterygota Palaeoptera Ephemeroptera
## 11             Insecta Insecta      <NA>        <NA>          <NA>
## 12           Zygoptera Insecta Pterygota Palaeoptera       Odonata
##      Suborder         Family      Genus             Species  A
## 1  Pisciforma       Baetidae Acentrella                <NA>  0
## 2  Pisciforma       Baetidae Acentrella  Acentrella parvula 71
## 3  Pisciforma       Baetidae Acentrella  Acentrella turbida 12
## 4   Zygoptera Coenagrionidae      Argia                <NA>  8
## 5  Pisciforma       Baetidae       <NA>                <NA>  0
## 6  Pisciforma       Baetidae     Baetis                <NA>  0
## 7  Pisciforma       Baetidae     Baetis  Baetis flavistriga 60
## 8  Pisciforma       Baetidae     Baetis Baetis intercalaris 99
## 9  Pisciforma       Baetidae     Baetis        Baetis pluto 58
## 10       <NA>           <NA>       <NA>                <NA>  0
## 11       <NA>           <NA>       <NA>                <NA>  0
## 12  Zygoptera           <NA>       <NA>                <NA>  0
```
We see that only the 5 Baetidae species and Argia sp. are kept, all others (as they are parents) are set to zero.


### NOTE
This package is currently under development and the code has not been tested extensively!
Moreover, there is some work needed to make the package more user friendly!

It currently can reproduce the appendix of Cuffney et al. (2007), but may break with other data!

Please use only ITIS as taxonomic backend, as others have not been tested yet.

`dpac_g()` and `option='K'` are currently not available.

Please [report any issues or bugs](https://github.com/EDiLD/restax/issues).


### References
Cuffney, T. F., Bilger, M. D. & Haigler, A. M. Ambiguous taxa: effects on the characterization and interpretation of invertebrate assemblages. Journal of the North American Benthological Society 26, 286–307 (2007).
