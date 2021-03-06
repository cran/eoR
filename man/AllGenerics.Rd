% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGeneric.R
\name{header}
\alias{header}
\alias{footer}
\alias{content}
\alias{region}
\alias{protocol}
\alias{selectYears}
\alias{selectRegion}
\alias{lt1}
\alias{ex1}
\alias{rd1}
\alias{rd1total}
\alias{re1}
\alias{re1total}
\title{AllGenerics}
\usage{
header(object)

footer(object)

content(object)

region(object)

protocol(object)

selectYears(object, selectYears)

selectRegion(object, selectRegion)

lt1(object)

ex1(object)

rd1(object)

rd1total(object)

re1(object)

re1total(object)
}
\arguments{
\item{object}{object}

\item{selectYears}{vector of integer}

\item{selectRegion}{vector of character}
}
\value{
The \code{header}-method returns the header of an object

The \code{footer}-method returns the footer of an object

The \code{content}-method returns the content of an object

The \code{region}-method returns the region of an object

The \code{protocol}-method returns the protocol of an object

The \code{selectYears}-method returns a subset of an object

The \code{selectRegion}-method returns a subset of an object

The \code{lt1}-method returns the life table lt1 of an LT1 object

The \code{ex1}-method returns the exposure table ex1 of an EX1 object

The \code{rd1}-method returns the regional death count table rd1 of an RD1 object

The \code{rd1}-method returns the total sum death count table rd1total of an RD1 object

The \code{er1}-method returns the regional exposure table re1 of an RE1 object

The \code{er1total}-method returns the regional total sum exposure table re1total of an RE1 object
}
\description{
The \code{header}-method gets the header of an object

The \code{footer}-method gets the footer of an object

The \code{content}-method gets the content of an object

The \code{region}-method gets the region of an object

The \code{protocol}-method gets the protocol of an object

The \code{selectYears}-method gets a subset of an object

The \code{selectRegion}-method gets a subset of an object

The \code{lt1}-method gets the life table lt1 of an LT1 object

The \code{ex1}-method gets the exposure table ex1 of an EX1 object

The \code{rd1}-method gets the regional death count table rd1 of an RD1 object

The \code{rd1total}-method gets the total sum death count table rd1total of an RD1 object

The \code{re1}-method gets the regional exposure table re1 of an RE1 object

The \code{re1total}-method gets the regional total sum exposure table re1total of an RE1 object
}
