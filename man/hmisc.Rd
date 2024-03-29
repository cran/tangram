% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform-hmisc.R
\docType{data}
\name{summarize_kruskal_horz}
\alias{summarize_kruskal_horz}
\alias{summarize_kruskal_vert}
\alias{summarize_chisq}
\alias{summarize_spearman}
\alias{hmisc}
\title{Style Bundle for Hmisc defaults}
\format{
An object of class \code{list} of length 5.
}
\usage{
summarize_kruskal_horz(
  table,
  row,
  column,
  cell_style,
  pformat = NULL,
  msd = FALSE,
  quant = c(0.25, 0.5, 0.75),
  overall = NULL,
  test = FALSE,
  ...
)

summarize_kruskal_vert(
  table,
  row,
  column,
  cell_style,
  collapse_single = TRUE,
  pformat = NULL,
  msd = FALSE,
  test = FALSE,
  ...
)

summarize_chisq(
  table,
  row,
  column,
  cell_style,
  pformat = NULL,
  collapse_single = TRUE,
  overall = NULL,
  test = FALSE,
  row_percents = FALSE,
  useNA = "no",
  ...
)

summarize_spearman(
  table,
  row,
  column,
  cell_style,
  pformat = NULL,
  test = FALSE,
  ...
)

hmisc
}
\arguments{
\item{table}{The table object to modify}

\item{row}{The row variable object to use (numerical)}

\item{column}{The column variable to use (categorical)}

\item{cell_style}{list; cell styling functions}

\item{pformat}{numeric, character or function; A formatting directive to be applied to p-values}

\item{msd}{logical; Include mean and standard deviation with quantile statistics}

\item{quant}{numeric; Vector of quantiles to include. Should be an odd number since the middle value is highlighted on display.}

\item{overall}{logical or character; Include overall summary statistics for a categorical column. Character values are assumed to be true and used as column header.}

\item{test}{logical or function; include statistical test results. Function signature must be function(row, col, cell_style, ...)}

\item{...}{absorbs additional arugments. Unused at present.}

\item{collapse_single}{logical; default TRUE. Categorical variables with a two values collapse to single row.}

\item{row_percents}{logical; use denominator across rows instead of columns.}

\item{useNA}{character; Specifies whether to include NA counts in the table. The allowed values correspond to never "no" (Default), only if the count is positive "ifany" and even for zero counts "always". An NA column is always excluded.}
}
\value{
The modified table object
}
\description{
List of lists, should contain a "Type" entry with a function to determine type of vector passed in.
Next entries are keyed off returned types from function, and represent the type of a row.
The returned list should contain the same list of types, and represents the type of a column. Thus it now returns
a function to process the intersection of those two types.
There are additionally a list of cell tranforms that can be overridden and a default
footnote if none is specified.
}
\section{\code{summarize_kruskal_horz}}{

Given a row and column object apply a Kruskal test and output
the results horizontally. 1 X (n + no. categories + test statistic)
}

\section{\code{summarize_kruskal_vert}}{

Given a row and column object from the parser apply a Kruskal test and output
the results vertically (#Categories+1) X (N, Summary, Statistic)
}

\section{\code{summarize_chisq}}{

Given a row and column object from the parser apply a chi^2 test and output
the results
}

\section{\code{summarize_spearman}}{

Given a row and column object from the parser apply a Spearman test and output
the results in a 1X3 format.
}

\section{\code{hmisc}}{

\preformatted{hmisc <- list(
 Type        = hmisc_data_type,
 Numerical   = list(
   Numerical   = summarize_spearman,
   Categorical = summarize_kruskal_horz
 ),
 Categorical = list(
   Numerical   = summarize_kruskal_vert,
   Categorical = summarize_chisq
 ),
 Cell        = hmisc_cell,
 Footnote    = "N is the number of non-missing value. ^1^Kruskal-Wallis. ^2^Pearson. ^3^Wilcoxon."
)}
}

\seealso{
\code{\link{hmisc_data_type}}, \code{\link{tangram}}, \code{\link{hmisc_cell}}
}
\keyword{data}
