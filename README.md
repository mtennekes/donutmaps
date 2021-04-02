# Donut maps

This is an R package to create donut maps.

## Installation

```r
library(remotes)
install_github("mtennekes/odf")
install_github("mtennekes/donutmaps")
```

Make sure the latest CRAN version of `tmap` is installed.

## Example

Load the pacakges

```r
library(odf)
library(tmap)
library(donutmaps)
```

See the examples inside the documentation:

```r
?bake_donuts
```

## Examples

### Commuting maps of the Netherlands: 

<img src="https://user-images.githubusercontent.com/2444081/113439621-37575500-93eb-11eb-8d77-09016751b3c4.png" alt="Donut Map" width="1000px"/>

https://dashboards.cbs.nl/v1/commutingNL/

Note: this specific map is not fully reproducible yet, but is very similar to the example in `bake_donuts`

### Lockdown maps of France

https://inseefrlab.github.io/lockdown-maps-R/inflows_EN.html

repository: https://github.com/InseeFrLab/lockdown-maps-R



