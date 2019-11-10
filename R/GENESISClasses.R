# Rainer Walke, MPIDR Rostock

#' @importFrom methods new show validObject
NULL
#' @importFrom data.table := fread setnames
NULL
####

## S4 class definitions

#' RD1 class for 'GENESIS-Tabelle: 12613-02-02-4'
#'
#' The class \code{RD1} stores GENESIS table information in a systematic way.
#'
#' This \code{RD1} class fits to the GENESIS table 12613-02-02-4.
#' See https://www.regionalstatistik.de/genesis/online for data details.
#' An RD1 instance stores the raw header, the raw footer, the content,
#' the version protocol and regional information and the
#' death count information as a data.table object. Further it stores a second data.table
#' with the total sums information.
#' Try demo(RD1) for a demonstration.
#'
#' @slot content describes the content
#' @slot region regional entity
#' @slot header includes the raw header information
#' @slot footer includes the raw footer information
#' @slot rd1 stores the death count table (data.table)
#' @slot rd1total stores the death count total sums table (data.table)
#' @slot protocol contains the protocol information
#' @examples
#' d1 <- readRegDeath(file.path(system.file(package="eoR"), "extdata", "12613-02-02-4m.csv"))
#' header(d1)
#' footer(d1)
#' rd1(d1)[, table(Year)]
#' @export
#' @rdname RD1-class
setClass("RD1",
         slots=c(
           header="vector", # file header
           footer="vector", # file footer
           rd1="data.table", # exposure table
           rd1total="data.table", # exposure table total sums
           content="character", # content
           region="character", # region
           protocol="character" # protocol version
         )
)

## slot getters

#' RD1 header
#'
#' The \code{header}-method gets the raw header information of an RD1 object
#'
#' @param object RD1-object
#' @return The \code{header}-method returns the raw header information
#' @export
#' @rdname RD1-class
setMethod("header", "RD1", function(object) object@header)

#' RD1 footer
#'
#' The \code{footer}-method gets the raw footer information of an RD1 object
#'
#' @return The \code{footer}-method returns the raw footer information
#' @export
#' @rdname RD1-class
setMethod("footer", "RD1", function(object) object@footer)

#' RD1 rd1
#'
#' The \code{rd1}-method gets the death count table information of an RD1 object
#'
#' @return The \code{rd1}-method returns the death count table
#' @export
#' @rdname RD1-class
setMethod("rd1", "RD1", function(object) object@rd1)

#' RD1 rd1total
#'
#' The \code{rd1total}-method gets the death count total sums table information of an RD1 object
#'
#' @return The \code{rd1total}-method returns the death count total sums table
#' @export
#' @rdname RD1-class
setMethod("rd1total", "RD1", function(object) object@rd1total)

#' RD1 content
#'
#' The \code{content}-method gets the death count table information of an RD1 object
#'
#' @return The \code{content}-method returns the content information
#' @export
#' @rdname RD1-class
setMethod("content", "RD1", function(object) object@content)

#' RD1 region
#'
#' The \code{region}-method gets the regional information of an RD1 object
#'
#' @return The \code{region}-method returns the regional information
#' @export
#' @rdname RD1-class
setMethod("region", "RD1", function(object) object@region)

#' RD1 protocol
#'
#' The \code{protocol}-method gets the protocol information of an RD1 object
#'
#' @return The \code{protocol}-method returns the protocol information
#' @export
#' @rdname RD1-class
setMethod("protocol", "RD1", function(object) object@protocol)

## constructor

#' RD1 constructor
#'
#' The \code{RegDeath1}-constructor creates an RD1 object
#'
#' @param header vector
#' @param footer vector
#' @param rd1 data.table
#' @param rd1total data.table
#' @param content character
#' @param region character
#' @param  protocol character
#' @return The \code{RegDeath1}-constructor returns an RD1 object
#' @export
#' @rdname RD1-class
RegDeath1 <- function(header, footer, rd1, rd1total, content, region, protocol)
  new("RD1", header=header, footer=footer, rd1=rd1, rd1total=rd1total, content=content, region=region, protocol=protocol)

## methods

#' RD1 length
#'
#' The \code{length}-method gets the number of rows int the RD1 rd1 data.table
#' @param x RD1-object
#' @return The \code{length}-method returns the number of rows
#' @export
#' @rdname RD1-class
setMethod("length", "RD1", function(x) dim(x@rd1)[1])

#' RD1 show
#'
#' @export
#' @rdname RD1-class
#' @export
setMethod("show", "RD1",
          function(object)
            cat(class(object), "instance with length", length(object), "\n")
)

#' RD1 selectYears
#'
#' The \code{selectYears}-method gets a subset of an RD1 object
#'
#' @param selectYears vector
#' @return The \code{selectYears}-method returns an RD1 object
#' @export
#' @rdname RD1-class
setMethod("selectYears", "RD1", function(object, selectYears) {
  Year <- NULL # evoid NOTE
  .rd1 <- object@rd1[Year %in% selectYears]
  .rd1total <- object@rd1total[Year %in% selectYears]
  return(new("RD1", header=object@header, footer=object@footer, rd1=.rd1, rd1total=.rd1total, content=object@content, region=object@region, protocol=object@protocol))
}
)

#' RD1 selectRegion
#'
#' The \code{selectRegion}-method gets a subset of an RD1 object
#'
#' @param selectRegion vector
#' @return The \code{selectRegion}-method returns an RD1 object
#' @export
#' @rdname RD1-class
setMethod("selectRegion", "RD1", function(object, selectRegion) {
  Region_Code <- NULL # evoid NOTE
  .rd1 <- object@rd1[Region_Code %in% selectRegion]
  .rd1total <- object@rd1total[Region_Code %in% selectRegion]
  return(new("RD1", header=object@header, footer=object@footer, rd1=.rd1, rd1total=.rd1total, content=object@content, region=object@region, protocol=object@protocol))
}
)

# validity method
#' @export
setValidity("RD1",
            function(object) {
              msg <- NULL
              valid <- TRUE

              rd1colnames <- c("Year","Region_Code","Region_Name","Age_Name","Total1","Male","Female","D_Total","D_Male","D_Female", "AgeLow")
              if (!all(rd1colnames %in% colnames(object@rd1))) {
                valid <- FALSE
                msg <- c(msg, paste(c("ex1 must contain:",rd1colnames), collapse=" "))
              }

              if (valid) TRUE else msg
            }
)


####

#' RE1 class for 'GENESIS-Tabelle: 12411-03-03-4'
#'
#' The class \code{RE1} stores GENESIS table information in a systematic way.
#'
#' This \code{RE1} class fits to the GENESIS table 12411-03-03-4.
#' See https://www.regionalstatistik.de/genesis/online for data details.
#' An RE1 instance stores the raw header, the raw footer, the content,
#' the version protocol and regional information and the
#' exposure information as a data.table object. Further it stores a second data.table
#' with the total sums information.
#' Try demo(RE1) for a demonstration.
#'
#' @slot content describes the content
#' @slot region regional entity
#' @slot header includes the raw header information
#' @slot footer includes the raw footer information
#' @slot re1 stores the exposure table (data.table)
#' @slot re1total stores the exposure total sums table (data.table)
#' @slot protocol contains the protocol information
#' @examples
#' r1 <- readRegExp(file.path(system.file(package="eoR"), "extdata", "12411-03-03-4m.csv"))
#' header(r1)
#' footer(r1)
#' re1(r1)[, table(Year)]
#' @export
#' @rdname RE1-class
setClass("RE1",
         slots=c(
           header="vector", # file header
           footer="vector", # file footer
           re1="data.table", # exposure table
           re1total="data.table", # exposure table total sums
           content="character", # content
           region="character", # region
           protocol="character" # protocol version
         )
)

## slot getters

#' RE1 header
#'
#' The \code{header}-method gets the raw header information of an RE1 object
#'
#' @param object RE1-object
#' @return The \code{header}-method returns the raw header information
#' @export
#' @rdname RE1-class
setMethod("header", "RE1", function(object) object@header)

#' RE1 footer
#'
#' The \code{footer}-method gets the raw footer information of an RE1 object
#'
#' @return The \code{footer}-method returns the raw footer information
#' @export
#' @rdname RE1-class
setMethod("footer", "RE1", function(object) object@footer)

#' RE1 re1
#'
#' The \code{re1}-method gets the exposure table information of an RE1 object
#'
#' @return The \code{re1}-method returns the exposure table
#' @export
#' @rdname RE1-class
setMethod("re1", "RE1", function(object) object@re1)

#' RE1 re1total
#'
#' The \code{re1total}-method gets the exposure total sums table information of an RE1 object
#'
#' @return The \code{re1total}-method returns the exposure total sums table
#' @export
#' @rdname RE1-class
setMethod("re1total", "RE1", function(object) object@re1total)

#' RE1 content
#'
#' The \code{content}-method gets the exposure table information of an RE1 object
#'
#' @return The \code{content}-method returns the content information
#' @export
#' @rdname RE1-class
setMethod("content", "RE1", function(object) object@content)

#' RE1 region
#'
#' The \code{region}-method gets the regional information of an RE1 object
#'
#' @return The \code{region}-method returns the regional information
#' @export
#' @rdname RE1-class
setMethod("region", "RE1", function(object) object@region)

#' RE1 protocol
#'
#' The \code{protocol}-method gets the protocol information of an RE1 object
#'
#' @return The \code{protocol}-method returns the protocol information
#' @export
#' @rdname RE1-class
setMethod("protocol", "RE1", function(object) object@protocol)

## constructor

#' RE1 constructor
#'
#' The \code{RegExp1}-constructor creates an RE1 object
#'
#' @param header vector
#' @param footer vector
#' @param re1 data.table
#' @param re1total data.table
#' @param content character
#' @param region character
#' @param  protocol character
#' @return The \code{RegExp1}-constructor returns an RE1 object
#' @export
#' @rdname RE1-class
RegExp1 <- function(header, footer, re1, re1total, content, region, protocol)
  new("RE1", header=header, footer, re1=re1, re1total=re1total, content=content, region=region, protocol=protocol)

## methods

#' RE1 length
#'
#' The \code{length}-method gets the number of rows int the RE1 re1 data.table
#' @param x RE1-object
#' @return The \code{length}-method returns the number of rows
#' @export
#' @rdname RE1-class
setMethod("length", "RE1", function(x) dim(x@re1)[1])


#' RE1 show
#'
#' @export
#' @rdname RE1-class
#' @export
setMethod("show", "RE1",
          function(object)
            cat(class(object), "instance with length", length(object), "\n")
)

#' RE1 selectYears
#'
#' The \code{selectYears}-method gets a subset of an RE1 object
#'
#' @param selectYears vector
#' @return The \code{selectYears}-method returns an RE1 object
#' @export
#' @rdname RE1-class
setMethod("selectYears", "RE1", function(object, selectYears) {
  Year <- NULL # evoid NOTE
  .re1 <- object@re1[Year %in% selectYears]
  .re1total <- object@re1total[Year %in% selectYears]
  return(new("RE1", header=object@header, footer=object@footer, re1=.re1, re1total=.re1total, content=object@content, region=object@region, protocol=object@protocol))
}
)

#' RE1 selectRegion
#'
#' The \code{selectRegion}-method gets a subset of an RE1 object
#'
#' @param selectRegion vector
#' @return The \code{selectRegion}-method returns an RE1 object
#' @export
#' @rdname RE1-class
setMethod("selectRegion", "RE1", function(object, selectRegion) {
  Region_Code <- NULL # evoid NOTE
  .re1 <- object@re1[Region_Code %in% selectRegion]
  .re1total <- object@re1total[Region_Code %in% selectRegion]
  return(new("RE1", header=object@header, footer=object@footer, re1=.re1, re1total=.re1total, content=object@content, region=object@region, protocol=object@protocol))
}
)

# validity method
#' @export
setValidity("RE1",
            function(object) {
              msg <- NULL
              valid <- TRUE
              re1colnames <-c("Date1","Region_Code","Region_Name","Age_Name","Total1","Male","Female","D_Total","D_Male","D_Female",
                "A_Total","A_Male","A_Female", "Year", "AgeLow")
              if (!all(re1colnames %in% colnames(object@re1))) {
                valid <- FALSE
                msg <- c(msg, paste(c("ex1 must contain:",re1colnames), collapse=" "))
              }

              if (valid) TRUE else msg
            }
)

####

# read all information from 'GENESIS-Tabelle: 12613-02-02-4'
# Statistische &Auml;mter des Bundes und der L&auml;nder, Deutschland, 2019

#' RD1 readRegDeath
#'
#' The \code{readRegDeath}-method reads all information from 'GENESIS-Tabelle: 12613-02-02-4'
#'
#' @param infile character file name
#' @return The \code{readRegDeath}-method returns an RD1 object
#' @export
#' @rdname RD1-class
readRegDeath <- function(infile)
{
  bb <- scan(infile, what=character(), blank.lines.skip = FALSE, encoding="ANSI", sep="\n")

  h2 <- grep(";Insgesamt;m\\xe4nnlich",bb, useBytes = TRUE)
  # 10 x low line \x5f
  f1 <- grep("\\x5f\\x5f\\x5f\\x5f\\x5f\\x5f\\x5f\\x5f\\x5f\\x5f",bb, useBytes = TRUE)
  f2 <- length(bb)

  header <- bb[1:h2]
  footer <- bb[f1:f2]

  rdft <- data.table::fread(infile, skip=h2, nrows=(f1-h2-1))

  data.table::setnames(rdft, c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10"),
           c("Year","Region_Code","Region_Name","Age_Name","Total1","Male","Female","D_Total","D_Male","D_Female"))

  # extract the total sums
  Age_Name <- NULL # to prevent a note
  rd1total <- rdft[Age_Name=="Insgesamt"]

  # merge the lower age limit
  age_code1 <- data.table::data.table(Age_Name=c("unter 1 Jahr","1 bis unter 5 Jahre",
                                     "5 bis unter 10 Jahre","10 bis unter 15 Jahre",
                                     "15 bis unter 20 Jahre","20 bis unter 25 Jahre",
                                     "25 bis unter 30 Jahre","30 bis unter 35 Jahre",
                                     "35 bis unter 40 Jahre","40 bis unter 45 Jahre",
                                     "45 bis unter 50 Jahre","50 bis unter 55 Jahre",
                                     "55 bis unter 60 Jahre","60 bis unter 65 Jahre",
                                     "65 bis unter 70 Jahre","70 bis unter 75 Jahre",
                                     "75 bis unter 80 Jahre","80 bis unter 85 Jahre",
                                     "85 Jahre und mehr"),
                          AgeLow=c(0,1,seq(5,85,by=5)))

  rdft <- merge(rdft,age_code1,by="Age_Name")

  return(new("RD1", header=header, footer=footer, rd1=rdft, rd1total = rd1total, content="GENESIS-Tabelle: 12613-02-02-4", region="Germany", protocol="unknown"))

}


# read all information from 'GENESIS-Tabelle: 12411-03-03-4'
# Statistische &Auml;mter des Bundes und der L&auml;nder, Deutschland, 2019

#' RE1 readRegExp
#'
#' The \code{readRegExp}-method reads all information from 'GENESIS-Tabelle: 12411-03-03-4'
#'
#' @param infile character file name
#' @return The \code{readRegExp}-method returns an RE1 object
#' @export
#' @rdname RE1-class
readRegExp <- function(infile)
{
  bb <- scan(infile, what=character(), blank.lines.skip = FALSE, encoding="ANSI", sep="\n")

  h2 <- grep(";Insgesamt;m\\xe4nnlich",bb, useBytes = TRUE)
  # 10 x low line \x5f
  f1 <- grep("\\x5f\\x5f\\x5f\\x5f\\x5f\\x5f\\x5f\\x5f\\x5f\\x5f",bb, useBytes = TRUE)
  f2 <- length(bb)

  header <- bb[1:h2]
  footer <- bb[f1:f2]

  rexpt <- data.table::fread(infile, skip=h2, nrows=(f1-h2-1))

  data.table::setnames(rexpt, c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13"),
           c("Date1","Region_Code","Region_Name","Age_Name","Total1","Male","Female","D_Total","D_Male","D_Female",
             "A_Total","A_Male","A_Female"))
  # add the year
  Year <- NULL # avoid NOTE
  Date1 <- NULL # avoid NOTE
  rexpt[, Year := as.integer(sub("31.12.","",Date1))]

  Age_Name <- NULL # to prevent a note
  # extract the total sums
  re1total <- rexpt[Age_Name=="Insgesamt"]


  # merge the lower age limit
  age_code2 <- data.table::data.table(Age_Name=c("unter 3 Jahre","3 bis unter 6 Jahre",
                                     "6 bis unter 10 Jahre","10 bis unter 15 Jahre",
                                     "15 bis unter 18 Jahre","18 bis unter 20 Jahre",
                                     "20 bis unter 25 Jahre",
                                     "25 bis unter 30 Jahre","30 bis unter 35 Jahre",
                                     "35 bis unter 40 Jahre","40 bis unter 45 Jahre",
                                     "45 bis unter 50 Jahre","50 bis unter 55 Jahre",
                                     "55 bis unter 60 Jahre","60 bis unter 65 Jahre",
                                     "65 bis unter 70 Jahre","70 bis unter 75 Jahre",
                                     "75 bis unter 80 Jahre","80 bis unter 85 Jahre",
                                     "85 bis unter 90 Jahre","90 Jahre und mehr"),
                          AgeLow=c(0,3,6,10,15,18,seq(20,90,by=5)))

  rexpt <- merge(rexpt, age_code2 ,by="Age_Name")

  return(new("RE1", header=header, footer=footer, re1=rexpt, re1total=re1total, content="GENESIS-Tabelle: 12613-02-02-4", region="Germany", protocol="unknown"))

}

####
