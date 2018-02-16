svars
=====

[![Build Status](https://travis-ci.org/alexanderlange53/svars.svg?branch=master)](https://travis-ci.org/alexanderlange53/svars) 
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/svars)](https://cran.r-project.org/package=svars) 
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/svars)](https://cran.r-project.org/package=svars)

## Overview

svars contains data-driven identification methods for structural vector autoregressive (SVAR) models.
Based on an existing VAR model object (provided by e.g. VAR() from the 'vars' package), the structural impact relation matrix 
is obtained via data-driven identification techniques.

The cornerstone functions identify the structural errors

-   `id.cv()` by means of changes in volatility wit exogenous break.
-   `id.cvm()` via least dependent innovations based on Cramer-von Mises statistic.
-   `id.dc()` via least dependent innovations based on distance covariance matrix.
-   `id.ngml()` by means of non-Gaussian maximum likelihood.

These all return an estimated svars object with identified structural shocks, for which the package contains varoius functions for further analysis. 

It is common practice in the SVAR literatur to calculate confidence bands via bootstrap procedures, where svars provides functions for the wild bootstrap approach (`wild.boot()`) and the moving block bootstrap method (`mb.boot()`). Further helpful tools for SVAR analysis might be e.g. impulse response functions (`imrf()`), forecast error variance decompostions (`fev()`) and historical decompositions (`hd()`). 


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

