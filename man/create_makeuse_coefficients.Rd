% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_makeuse_coefficients.R
\name{create_makeuse_coefficients}
\alias{create_makeuse_coefficients}
\title{Create input-output make and use cofficients from PECAS AA accounts}
\usage{
create_makeuse_coefficients(pecas_makeuse, faf_data, save_to = NULL)
}
\arguments{
\item{pecas_makeuse}{Data frame containing statewide input-output flows in
annual dollars, generated by the PECAS AA module}

\item{faf_data}{Data frame containing the reformatted Freight Analysis 
Framework (FAF) annual flow data}

\item{save_to}{File name for saving the derived make and use coefficients in
comma-separated value format (optional)}
}
\description{
Create input-output make and use cofficients from PECAS AA accounts
}
\details{
This function converts the PECAS input-output flows by sector into
  make and use coefficients, used to map commodities produced or consumed by
  each sector (make and use coefficients, respectively). PECAS already
  splits flows between transportable and non-transportable commodities,
  making our job simpler. We simply have to put the data, which is produced
  for each year PECAS runs, into format we need. The transportable goods are
  defined using Standard Classification of Transportable Goods (SCTG) codes, 
  which are also used in USDOT data, including their Freight Analysis
  Framework (FAF), which we uses to define inter-regional flows. We read the
  FAF data in this function to create a list of each commodity defined within
  it, which is compared to those found in the make-use data. An error is
  thrown if one or more commodities are found in the FAF that are not in the 
  make-use data (although not vice-versa, for there might be local production
  not included in the inter-regional flows). The user can optionally save the
  derived coefficients in a text file in comma-separated value format.
}
\examples{
make_use <- create_makeuse_coefficients(pecas_makeuse, faf_data,
  "make-use-coefficients.csv")
}
