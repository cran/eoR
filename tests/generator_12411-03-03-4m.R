# Rainer Walke, MPIDR Rostock
# create artifical example data
# 12411-03-03-4m.csv

require(data.table)

h1 <- 1
h2 <- 8

f1 <- h2 + 22 * 5 + 1
f2 <- f1 + 27

# low lines
header <- c("example data similar to 12411-03-03-4.csv",
            rep("some text", times=(h2-2)),
            ";;;;Insgesamt;m\xe4nnlich;more;more;;;;;")
header

footer <- c("\x5f\x5f\x5f\x5f\x5f\x5f\x5f\x5f\x5f\x5f",
            rep("some text", times=(f2-f1)))

footer


block1 <- function(mult) {

  V13 <- c(250L,200L,300L,300L,250L,150L,550L,
           700L,700L,850L,800L,700L,500L,400L,
           350L,300L,200L,100L,50L,30L,20L)
  V13 <- as.integer(V13 * mult)

  (V12 <- as.integer(1.4 * V13))
  (V11 <- V12 + V13)

  (V10 <- as.integer(22.1 * V13))
  (V9 <- as.integer(0.9* V10))
  (V8 <- V9 + V10)

  (V7 <- V10 + V13)
  (V6 <- V9 + V12)
  (V5 <- V8 + V11)

  b1 <- data.table(V5=V5, V6=V6, V7=V7, V8=V8, V9=V9, V10=V10, V11=V11, V12=V12, V13=V13)
  b2 <- rbind(b1, t(colSums(b1)))
  return(b2)

}

b2a <- sapply(block1(1.0), paste)

b2b <- sapply(block1(1.474), paste)

b2c <- sapply(block1(2.782), paste)

b2d <- apply(b2c, 2, function(x) gsub(".*", "-", x))

b2e <- sapply(block1(7.532), paste)


age_code2 <- c("unter 3 Jahre","3 bis unter 6 Jahre",
               "6 bis unter 10 Jahre","10 bis unter 15 Jahre",
               "15 bis unter 18 Jahre","18 bis unter 20 Jahre",
               "20 bis unter 25 Jahre",
               "25 bis unter 30 Jahre","30 bis unter 35 Jahre",
               "35 bis unter 40 Jahre","40 bis unter 45 Jahre",
               "45 bis unter 50 Jahre","50 bis unter 55 Jahre",
               "55 bis unter 60 Jahre","60 bis unter 65 Jahre",
               "65 bis unter 70 Jahre","70 bis unter 75 Jahre",
               "75 bis unter 80 Jahre","80 bis unter 85 Jahre",
               "85 bis unter 90 Jahre","90 Jahre und mehr",
               "Insgesamt")

(content <- data.table(
  V1=rep(c("31.12.2011", rep(c("31.12.2016"), 4)), each=22),
  V2=rep(c("01","02","03","04","DX"), each=22),
  V3=rep(paste("Kreis-",seq(1,5), sep=''), each=22),
  V4=rep(age_code2, 5),
  rbind(b2a,b2b,b2c,b2d,b2e)))



cc <- c(header, apply(content, 1, paste, collapse=';'), footer)

# write(cc, file = file.path(".", "12411-03-03-4m.csv"))

# final test

library(eoR)

# compare the files with the eoR file
cc2 <- readLines(file.path(system.file(package="eoR"), "extdata", "12411-03-03-4m.csv"))
stopifnot(all.equal(cc,cc2))



dd <- readRegExp(file.path(system.file(package="eoR"), "extdata", "12411-03-03-4m.csv"))


length((dd))
dd

header(dd)
footer(dd)
re1(dd)
re1total(dd)

##
# descriptive tables and a ranges
re1(dd)[, table(Year)]
re1(dd)[, table(Age_Name)]
re1(dd)[, range(AgeLow)]

content(dd)
region(dd)
protocol(dd)

# select some years
(dd2 <- selectYears(dd, c(2016)))
# descriptive tables and a ranges
re1(dd2)[, table(Year)]
re1(dd2)[, table(Age_Name)]
re1(dd2)[, range(AgeLow)]

re1(dd2)
re1total(dd2)

# select a reagion
(dd3 <- selectRegion(dd2, "03"))
# descriptive tables and a ranges
re1(dd3)[, table(Year)]
re1(dd3)[, table(Age_Name)]
re1(dd3)[, range(AgeLow)]

re1(dd3)
re1total(dd3)

merge(re1(dd3), re1total(dd3)[,c("Region_Code","Total1","Male","Female")], by = "Region_Code")



