# Hmisc::impute copied for internal use in tangram
# Copyright (C) 2017-2018 Frank Harrell
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

impute <- function(x, ...) UseMethod("impute")
#' @exportS3Method
impute.default <- function(x, fun=median, ...)
{
  m <- is.na(x)
  k <- sum(m)
  if(k==0)
    return(x)

  nam <- names(x)
  if(!length(nam)) {
    nam <- as.character(1:length(x)); names(x) <- nam
  }

  if(!is.function(fun)) {
    fill <- fun
    if(is.character(fill) && length(fill)==1 && fill=="random")
      fill <- sample(x[!is.na(x)], sum(is.na(x)), replace=TRUE)
  } else if(is.factor(x)) {
    freq <- table(x)
    fill <- names(freq)[freq==max(freq)][1]   #take first if not unique
  } else
    fill <-
      if(missing(fun) && is.logical(x))
        (if(sum(x[!m]) >= sum(!m)/2)
           TRUE
         else
           FALSE)
      else
        fun(x[!m])

  ## median(logical vector) doesn't work - know trying to get median
  ## if fun is omitted.  Get mode.

  if(length(fill)>1 && length(fill)!=k)
    stop("length of vector of imputed values != no. NAs in x")

  ## lab <- label(x)
  ## if(is.null(lab) || lab=="") lab <- name
  ## lab <- paste(lab,"with",sum(m),"NAs imputed to",format(fill))
  ## attr(x, "label") <- lab
  if(is.factor(x)) {
    newlev <- sort(unique(fill))
    if(any(!(z <- newlev %in% levels(x)))) {
      xc <- as.character(x)
      xc[m] <- fill
      x <- factor(xc, c(levels(x), newlev[!z]))
    } else x[m] <- fill
  } else x[m] <- fill

  structure(x, imputed=(1:length(x))[m],
            class=c('impute', attr(x, 'class')))
}

#' @exportS3Method
print.impute <- function(x, ...)
{
  i <- attr(x,"imputed")
  if(!length(i)) {
    print.default(x);
    return(invisible())
  }

  if(is.factor(x))
    w <- as.character(x)
  else
    w <- format(x)

  names(w) <- names(x)
  w[i] <- paste(w[i], "*", sep="")
  attr(w, "label") <- attr(w,"imputed") <- attr(w, "class") <- NULL
  print.default(w, quote=FALSE)
  invisible()
}

#
# summary.impute <- function(object, ...)
# {
#   i <- attr(object, "imputed")
#   oi <- object
#   attr(oi,'class') <- attr(oi,'class')[attr(oi,'class')!="impute"]
#   oi <- oi[i]
#   if(all(oi==oi[1]))
#     cat("\n",length(i),"values imputed to",
#         if(is.numeric(oi))
#           format(oi[1])
#         else
#           as.character(oi[1]),
#         "\n\n")
#   else {
#     cat("\nImputed Values:\n\n")
#     if(length(i)<20)
#       print(oi)
#     else
#       print(describe(oi, descript=as.character(sys.call())[2]))
#
#     cat("\n")
#   }
#
#   NextMethod("summary")
# }

#' @exportS3Method "[" impute
"[.impute" <- function(x, ..., drop=FALSE)
{
  ats <- attributes(x)
  ats$dimnames <- NULL
  ats$dim <- NULL
  ats$names <- NULL
  attr(x,'class') <- NULL
  y <- x[..., drop = drop]
  if(length(y)==0)
    return(y)

  k <- 1:length(x);
  names(k) <- names(x)
  k <- k[...]
  attributes(y) <- c(attributes(y), ats)
  imp <- attr(y, "imputed")
  attr(y, "imputed") <- j <- (1:length(k))[k %in% imp]
  if(length(j)==0) {
    cy <- attr(y,'class')[attr(y,'class')!='impute']
    y <- structure(y, imputed=NULL,
                   class=if(length(cy))
                           cy
                         else
                           NULL)
  }

  y
}


is.imputed <- function(x)
{
  w <- rep(FALSE, if(is.matrix(x))nrow(x) else length(x))
  if(length(z <- attr(x,"imputed")))
    w[z] <- TRUE

  w
}

#' @exportS3Method
as.data.frame.impute <- function(x, row.names = NULL, optional = FALSE, ...)
{
  nrows <- length(x)
  if(!length(row.names)) {
    ## the next line is not needed for the 1993 version of data.class and is
    ## included for compatibility with 1992 version
    if(length(row.names <- names(x)) == nrows &&
                           !anyDuplicated(row.names)) {
    } else if(optional)
      row.names <- character(nrows)
    else
      row.names <- as.character(1:nrows)
  }

  value <- list(x)
  if(!optional)
    names(value) <- deparse(substitute(x))[[1]]

  structure(value, row.names=row.names, class='data.frame')
}
