# Rainer Walke, MPIDR Rostock

#' @importFrom methods new show validObject
NULL
#' @importFrom data.table := fread
NULL
####

## S4 class definitions


#' LT1 class for HMD life-tables 1x1
#'
#' The class \code{LT1} stores HMD life table information in a systematic way.
#'
#' This \code{LT1} class fits to Human Mortality Database (HMD) 1x1 period life tables.
#' See https://www.mortality.org/ for data details.
#' An LT1 instance stores the raw header, the content, 
#' the version protocol and regional information and the
#' life table as a data.table object.
#' The validation adds a pure numeric age AgeLow to the life table.
#' Try demo(LT1) for a demonstration.
#'
#' @slot content describes the content
#' @slot region regional entity
#' @slot header includes the raw header information
#' @slot lt1 stores the life table (data.table)
#' @slot protocol contains the protocol information
#' @examples
#' o1 <- readLT1x1(file.path(system.file(package="eoR"), "extdata", "DEUTNP.fltper_1x1m.txt"))
#' header(o1)
#' lt1(o1)[, table(Year)]
#' @export
#' @rdname LT1-class
setClass("LT1",
         slots=c(
           header="vector", # file header
           lt1="data.table", # life table
           content="character", # content
           region="character", # region
           protocol="character" # protocol version
         )
)

## slot getters

#' LT1 header
#'
#' The \code{header}-method gets the raw header information of an LT1 object
#'
#' @param object LT1-object
#' @return The \code{header}-method returns the raw header information
#' @export
#' @rdname LT1-class
setMethod("header", "LT1", function(object) object@header)

#' LT1 lt1
#'
#' The \code{lt1}-method gets the life table information of an LT1 object
#'
#' @return The \code{lt1}-method returns the life-table
#' @export
#' @rdname LT1-class
setMethod("lt1", "LT1", function(object) object@lt1)

#' LT1 content
#'
#' The \code{content}-method gets the life table information of an LT1 object
#'
#' @return The \code{content}-method returns the content information
#' @export
#' @rdname LT1-class
setMethod("content", "LT1", function(object) object@content)

#' LT1 region
#'
#' The \code{region}-method gets the regional information of an LT1 object
#'
#' @return The \code{region}-method returns the regional information
#' @export
#' @rdname LT1-class
setMethod("region", "LT1", function(object) object@region)

#' LT1 protocol
#'
#' The \code{protocol}-method gets the protocol information of an LT1 object
#'
#' @return The \code{protocol}-method returns the protocol information
#' @export
#' @rdname LT1-class
setMethod("protocol", "LT1", function(object) object@protocol)

## constructor

#' LT1 constructor
#'
#' The \code{LTable1}-constructor creates an LT1 object
#'
#' @param header vector
#' @param lt1 data.table
#' @param content character
#' @param region character
#' @param  protocol character
#' @return The \code{LTable1}-constructor returns an LT1 object
#' @export
#' @rdname LT1-class
LTable1 <- function(header, lt1, content, region, protocol)
  new("LT1", header=header, lt1=lt1, content=content, region=region, protocol=protocol)

## methods

#' LT1 length
#'
#' The \code{length}-method gets the number of rows int the LT1 lt1 data.table
#' @param x LT1-object
#' @return The \code{length}-method returns the number of rows
#' @export
#' @rdname LT1-class
setMethod("length", "LT1", function(x) dim(x@lt1)[1])

#' LT1 show
#'
#' @export
#' @rdname LT1-class
setMethod("show", "LT1",
          function(object)
            cat(class(object), "instance with length", length(object), "\n")
)

#' LT1 selectYears
#'
#' The \code{selectYears}-method gets a subset of an LT1 object
#'
#' @param selectYears vector
#' @return The \code{selectYears}-method returns an LT1 object
#' @export
#' @rdname LT1-class
setMethod("selectYears", "LT1", function(object, selectYears) {
  Year <- NULL # evoid NOTE
  .lt1 <- object@lt1[Year %in% selectYears]
            return(new("LT1", header=object@header, lt1=.lt1, content=object@content, region=object@region, protocol=object@protocol))
          }
)

## validity method

#' @export
setValidity("LT1",
            function(object) {
              msg <- NULL
              valid <- TRUE

              lt1colnames <- c("Year","Age","mx","qx","ax","lx","dx","Lx","Tx","ex")
              if (!all(lt1colnames %in% colnames(object@lt1))) {
                valid <- FALSE
                msg <- c(msg, paste(c("lt1 must contain:",lt1colnames), collapse=" "))
                }

              # add a pure numeric age
              object@lt1[,AgeLow:=as.numeric(sub("\\+","",Age))]

              if(max(lt1(object)$lx > 100000)) {
                valid <- FALSE
                msg <- c(msg,("lx must be not larger than 100000"))
                }

              if (valid) TRUE else msg
            }
)


####
# Class for exposures 1x1

#' EX1 class for HMD eposure files 1x1
#'
#' The class \code{EX1} stores HMD exposure information in a systematic way.
#'
#' This \code{EX1} class fits to Human Mortality Database (HMD) 1x1 period exposure tables.
#' See https://www.mortality.org/ for data details.
#' An EX1 instance stores the raw header, the content, 
#' the version protocol and regional information and the
#' exposure table as a data.table object.
#' The validation adds a pure numeric age AgeLow to the exposure table.
#' Try demo(EX1) for a demonstration.
#'
#' @slot content describes the content
#' @slot region regional entity
#' @slot header includes the raw header information
#' @slot ex1 stores the exposure table (data.table)
#' @slot protocol contains the protocol information
#' @examples
#' e1 <- readEX1x1(file.path(system.file(package="eoR"), "extdata", "DEUTNP.Exposures_1x1m.txt"))
#' header(e1)
#' ex1(e1)[, table(Year)]
#' @export
#' @rdname EX1-class
setClass("EX1",
         slots=c(
           header="vector", # file header
           ex1="data.table", # exposure table
           content="character", # content
           region="character", # region
           protocol="character" # protocol version
         )
)

## slot getters

#' EX1 header
#'
#' The \code{header}-method gets the raw header information of an EX1 object
#'
#' @param object EX1-object
#' @return The \code{header}-method returns the raw header information
#' @export
#' @rdname EX1-class
setMethod("header", "EX1", function(object) object@header)

#' EX1 ex1
#'
#' The \code{ex1}-method gets the exposure information of an EX1 object
#'
#' @return The \code{ex1}-method returns the exposure table
#' @export
#' @rdname EX1-class
setMethod("ex1", "EX1", function(object) object@ex1)


#' EX1 content
#'
#' The \code{content}-method gets the exposure table information of an EX1 object
#'
#' @return The \code{content}-method returns the content information
#' @export
#' @rdname EX1-class
setMethod("content", "EX1", function(object) object@content)

#' EX1 region
#'
#' The \code{region}-method gets the regional information of an EX1 object
#'
#' @return The \code{region}-method returns the regional information
#' @export
#' @rdname EX1-class
setMethod("region", "EX1", function(object) object@region)

#' EX1 protocol
#'
#' The \code{protocol}-method gets the protocol information of an EX1 object
#'
#' @return The \code{protocol}-method returns the protocol information
#' @export
#' @rdname EX1-class
setMethod("protocol", "EX1", function(object) object@protocol)

## constructor

#' EX1 constructor
#'
#' The \code{Exposure1}-constructor creates an EX1 object
#'
#' @param header vector
#' @param ex1 data.table
#' @param content character
#' @param region character
#' @param  protocol character
#' @return The \code{Exposure1}-constructor returns an EX1 object
#' @export
#' @rdname EX1-class
Exposure1 <- function(header, ex1, content, region, protocol)
  new("EX1", header=header, ex1=ex1, content=content, region=region, protocol=protocol)

## methods

#' EX1 length
#'
#' The \code{length}-method gets the number of rows int the EX1 ex1 data.table
#' @param x EX1-object
#' @return The \code{length}-method returns the number of rows
#' @export
#' @rdname EX1-class
setMethod("length", "EX1", function(x) dim(x@ex1)[1])

#' LT1 show
#'
#' @export
#' @rdname EX1-class
setMethod("show", "EX1",
          function(object)
            cat(class(object), "instance with length", length(object), "\n")
)

#' EX1 selectYears
#'
#' The \code{selectYears}-method gets a subset of an EX1 object
#'
#' @param selectYears vector
#' @return The \code{selectYears}-method returns an EX1 object
#' @export
#' @rdname EX1-class
setMethod("selectYears", "EX1", function(object, selectYears) {
  Year <- NULL # evoid NOTE
  .ex1 <- object@ex1[Year %in% selectYears]
  return(new("EX1", header=object@header, ex1=.ex1, content=object@content, region=object@region, protocol=object@protocol))
}
)

# validity method
#' @export
setValidity("EX1",
            function(object) {
              msg <- NULL
              valid <- TRUE

              ex1colnames <- c("Year","Age","Female","Male","Total")
              if (!all(ex1colnames %in% colnames(object@ex1))) {
                valid <- FALSE
                msg <- c(msg, paste(c("ex1 must contain:",ex1colnames), collapse=" "))
              }

              # add a pure numeric age
              object@ex1[,AgeLow:=as.numeric(sub("\\+","",Age))]

              if (valid) TRUE else msg
            }
)


####

# read all information from the file 1x1 period life table
# Human Mortality Database https://www.mortality.org/

#' LT1 readLT1x1
#'
#' The \code{readLT1x1}-method reads all information from the file 1x1 period life table
#'
#' @param infile character file name
#' @return The \code{readLT1x1}-method returns an LT1 object
#' @export
#' @rdname LT1-class
readLT1x1 <- function(infile)
{
  LT0 <- readLines(infile, n=2)
  LT1 <- data.table::fread(infile)

  B1 <- sapply(strsplit(LT0,"[,\t;:]"), trimws)
  region <- B1[[1]][1]
  content <- paste(B1[[1]][2],B1[[1]][3], sep=", ")
  protocol <- B1[[1]][7]

  return(new("LT1", header=LT0, lt1=LT1, content=content, region=region, protocol=protocol))
}


# read all information from the file 1x1 exposure table
# Human Mortality Database https://www.mortality.org/

#' EX1 readEX1x1
#'
#' The \code{readEX1x1}-method reads all information from the file 1x1 exposure table
#'
#' @param infile character file name
#' @return The \code{readEX1x1}-method returns an EX1 object
#' @export
#' @rdname EX1-class
readEX1x1 <- function(infile)
{
  EX0 <- readLines(infile, n=2)
  EX1 <- data.table::fread(infile)

  B1 <- sapply(strsplit(EX0,"[,\t;:]"), trimws)
  region <- B1[[1]][1]
  content <- B1[[1]][2]
  protocol <- B1[[1]][7]

  return(new("EX1", header=EX0, ex1=EX1, content=content, region=region, protocol=protocol))
}

####
