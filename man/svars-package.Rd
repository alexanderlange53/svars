\name{svars-package}
\alias{svars-package}
\alias{svars}
\title{Data driven SVAR identification of impact matrix for impulse response functions}
\description{
  This package Implements data driven identification methods for SVAR models. Based on an existing VAR model
  objct the B matrix may be obtained via changes in volatility, least dependent innovations or non gaussian maximum likelihood.
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

  All of these functions require an estimated var object. Currently the classes 'vars' and 'ca.jo' are supported.
  Besides these core functions, some additional tools to calculate confidence bands for impulse response functions using
  bootstrapping techniques as well as the Chow-Test for structural change. The USA dataset is used to showcase the functionalities
  in examples throughout the package.
}
\author{
    \itemize{
      \item Alexander Lange \email{alexander.lange@stud.uni-goettingen.de}
      \item Simone Maxand
      \item Bernhard Dalheimer \email{bernhard.dalheimer@uni-goettingen.de}
    }
    }



