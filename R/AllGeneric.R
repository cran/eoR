# Rainer Walke, MPIDR Rostock

#' AllGenerics
#'
#' The \code{header}-method gets the header of an object
#'
#' @param object object
#' @return The \code{header}-method returns the header of an object
#' @rdname AllGenerics
setGeneric("header", function(object) standardGeneric("header"))

#' AllGenerics
#'
#' The \code{footer}-method gets the footer of an object
#'
#' @return The \code{footer}-method returns the footer of an object
#' @rdname AllGenerics
setGeneric("footer", function(object) standardGeneric("footer"))

#' AllGenerics
#'
#' The \code{content}-method gets the content of an object
#'
#' @return The \code{content}-method returns the content of an object
#' @rdname AllGenerics
setGeneric("content", function(object) standardGeneric("content"))

#' AllGenerics
#'
#' The \code{region}-method gets the region of an object
#'
#' @return The \code{region}-method returns the region of an object
#' @rdname AllGenerics
setGeneric("region", function(object) standardGeneric("region"))

#' AllGenerics
#'
#' The \code{protocol}-method gets the protocol of an object
#'
#' @return The \code{protocol}-method returns the protocol of an object
#' @rdname AllGenerics
setGeneric("protocol", function(object) standardGeneric("protocol"))

#' AllGenerics
#'
#' The \code{selectYears}-method gets a subset of an object
#'
#' @param selectYears vector of integer
#' @return The \code{selectYears}-method returns a subset of an object
#' @rdname AllGenerics
setGeneric("selectYears", function(object, selectYears) standardGeneric("selectYears"))

#' AllGenerics
#'
#' The \code{selectRegion}-method gets a subset of an object
#'
#' @param selectRegion vector of character
#' @return The \code{selectRegion}-method returns a subset of an object
#' @rdname AllGenerics
setGeneric("selectRegion", function(object, selectRegion) standardGeneric("selectRegion"))

#' AllGenerics
#'
#' The \code{lt1}-method gets the life table lt1 of an LT1 object
#'
#' @return The \code{lt1}-method returns the life table lt1 of an LT1 object
#' @rdname AllGenerics
setGeneric("lt1", function(object) standardGeneric("lt1"))

#' AllGenerics
#'
#' The \code{ex1}-method gets the exposure table ex1 of an EX1 object
#'
#' @return The \code{ex1}-method returns the exposure table ex1 of an EX1 object
#' @rdname AllGenerics
setGeneric("ex1", function(object) standardGeneric("ex1"))

#' AllGenerics
#'
#' The \code{rd1}-method gets the regional death count table rd1 of an RD1 object
#'
#' @return The \code{rd1}-method returns the regional death count table rd1 of an RD1 object
#' @rdname AllGenerics
setGeneric("rd1", function(object) standardGeneric("rd1"))

#' AllGenerics
#'
#' The \code{rd1total}-method gets the total sum death count table rd1total of an RD1 object
#'
#' @return The \code{rd1}-method returns the total sum death count table rd1total of an RD1 object
#' @rdname AllGenerics
setGeneric("rd1total", function(object) standardGeneric("rd1total"))

#' AllGenerics
#'
#' The \code{re1}-method gets the regional exposure table re1 of an RE1 object
#'
#' @return The \code{er1}-method returns the regional exposure table re1 of an RE1 object
#' @rdname AllGenerics
setGeneric("re1", function(object) standardGeneric("re1"))

#' AllGenerics
#'
#' The \code{re1total}-method gets the regional total sum exposure table re1total of an RE1 object
#'
#' @return The \code{er1total}-method returns the regional total sum exposure table re1total of an RE1 object
#' @rdname AllGenerics
setGeneric("re1total", function(object) standardGeneric("re1total"))

