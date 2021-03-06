% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crypt.R
\name{encrypt}
\alias{encrypt}
\title{Encrypt data}
\usage{
encrypt(x, password)
}
\arguments{
\item{x}{any data object.}

\item{password}{a string specifying the passphrase to en- and decrypt
the data.}
}
\value{
encrypted data
}
\description{
\code{encrypt} is a simplified wrapper for the encryption of R data
objects using the \code{sodium} package
}
\seealso{
\code{\link{decrypt}}
}

