\name{svars-package}
\alias{svars-package}
\alias{svars}
\title{Data-driven identification of structural VAR models}
\description{
  This package implements data-driven identification methods for structural vector autoregressive (SVAR) models. Based on an existing VAR model object, the structural impact matrix B may be obtained via changes in volatility, least dependent innovations or non-Gaussian maximum likelihood.
  .\cr
  }
\details{
  The main functions to retrieve B matrices are:
    \tabular{ll}{
      \code{id.cv} \tab Identification via changes in volatility,
    }
  \tabular{ll}{
    \code{id.nglm} \tab Identification via Non-Gaussian maximum likelihood,
  }
  \tabular{ll}{
    \code{id.dc} \tab Independence-based identification of SVAR models based on distance covariances,
  }
  \tabular{ll}{
    \code{id.cvm} \tab Independence-based identification of SVAR models based on Cramer-von Mises distance.
  }

  All of these functions require an estimated var object. Currently the classes 'vars' and 'vec2var' from the \code{vars} package,     'nlVar', whcih includes both VAR and VECM, from the \code{tsDyn} package as well as the list from \code{MTS} package are supported.
  Besides these core functions, some additional tools to calculate confidence bands for impulse response functions using
  bootstrap techniques as well as the Chow-Test for structural change are implemented. The \code{USA} dataset is used to showcase the
  functionalties in examples throughout the package.
}
\author{
    \itemize{
      \item Alexander Lange \email{alexander.lange@stud.uni-goettingen.de}
      \item Bernhard Dalheimer \email{bernhard.dalheimer@uni-goettingen.de}
      \item Helmut Herwartz \email{hherwartz@uni-goettingen.de}
      \item Simone Maxand   \email{smaxand@uni-goettingen.de}

    }
    }



