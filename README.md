svars
=====

[![Build Status](https://travis-ci.org/alexanderlange53/svars.svg?branch=master)](https://travis-ci.org/alexanderlange53/svars) 
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/svars)](https://cran.r-project.org/package=svars) 
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/svars)](https://cran.r-project.org/package=svars)

## Overview

svars is an R package containing data-driven identification methods for structural vector autoregressive (SVAR) models.
Based on an existing VAR model object (provided by e.g. VAR() from the 'vars' package), the structural impact relation matrix 
is obtained via data-driven identification techniques.

The cornerstone functions identify the structural errors

-   `id.cv()` by means of changes in volatility wit exogenous break.
-   `id.dc()` via least dependent innovations based on distance covariance matrix.
-   `id.cvm()` via least dependent innovations based on Cramer-von Mises statistic.
-   `id.ngml()` by means of non-Gaussian maximum likelihood.

These all return an estimated svars object with identified structural shocks, for which the package contains varoius functions for further analysis. For isnatnce, it is possible to calculate impulse reposnes with `imrf()` or to perform a historical decomposition with `hd()`.

## Installation

```r
install.packages("svars")
```

Alternatively, install development version


```r
install.packages("devtools")
devtools::install_github("alexanderlange53/svars")
```


```r
library("svars")
```

