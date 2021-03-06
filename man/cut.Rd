% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listprocessor.R
\name{cut}
\alias{cut}
\title{cut}
\usage{
cut(lst, start = NULL, end = NULL)
}
\arguments{
\item{lst}{the list from which the elements are returned}

\item{start}{the start index (default: 1st index)}

\item{end}{the end index (default: length of list)}
}
\value{
all elements within the specified range
}
\description{
Cut or slice a list data type from start index to end index
}
\details{
Retrieve elements from start index to end index. If start is not supplied, it will be assumed to be the 1st index. If end is not supplied then, all elements up to the end will be returned.
}
\examples{
\dontrun{
(cut {1 2 3 4} 1 2) => list(1, 2)
(cut {1 2 3 4} :start 2) => list(2, 3, 4)
(cut {1 2 3 4} :end 3) => list(1, 2, 3)
}
}
\author{
Jay Morgan
}
