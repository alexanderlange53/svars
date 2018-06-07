\name{LN}

\docType{data}

\alias{LN}

\title{Interaction between monetary policy and the stock market}

\description{A five dimensional time series model which is commonly used to analyze the interaction between monetary policy and the stock market.
  \cr
  Monthly observations from 1970M1 to 2007M6:
  \tabular{ll}{
    q \tab Linearly detrended log of an industrial production index \cr
    pi \tab Annual change in the log of consumer prices (CPI index) (x100) \cr
    c \tab annual change in the log of the World Bank (non energy) commodity price index (x100) \cr
    s \tab Log of the real S&P500 stock price index deflated by the consumer price index
    to measure the real stock prices; the series is first differenced to represent monthly returns \cr
    r \tab Interest rate on Federal funds \cr
  }
All series, whith exception of the commodity price index (c), are taken from the FRED database and transformed as in Luetkepohl & Netsunajev (2017). The commodity price index comes from the World Bank. A more detailed description of the data and a corresponding VAR model implementation can be found in Luetkepohl & Netsunajev (2017).
}

\usage{LN}

\format{A \code{data.frame} containing 450 observations on 5 variables.}

\source{ Luetkepohl H., Netsunajev A., 2017. "Structural vector autoregressions with smooth transition in variances." \cr
Journal of Economic Dynamics and Control, 84, 43 - 57. ISSN 0165-1889.}

