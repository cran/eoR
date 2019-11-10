# Rainer Walke, MPIDR Rostock
# create artifical example data
# 12613-02-02-4m.csv

require(data.table)

h1 <- 1
h2 <- 7

f1 <- h2 + 20 * 5 + 1
f2 <- f1 + 53

# low lines
header <- c("example data similar to 12613-02-02-4.csv",
            rep("some text", times=(h2-2)),
            ";;;;Insgesamt;m\xe4nnlich;more;more;;")
header

footer <- c("\x5f\x5f\x5f\x5f\x5f\x5f\x5f\x5f\x5f\x5f",
            rep("some text", times=(f2-f1)))

footer


block1 <- function(mult) {

  V10 <- c(30L,5L,2L,10L,10L,20L,10L,
           30L,40L,90L,200L,300L,450L,600L,
           750L,1300L,2200L,2700L,8200L)
  V10 <- as.integer(V10 * mult)

  (V9 <- as.integer(1.1 * V10))
  (V8 <- V9 + V10)

  (V7 <- as.integer(1.11 * V10))
  (V6 <- as.integer(1.21* V9))
  (V5 <- V6 + V7)

  b1 <- data.table(V5=V5, V6=V6, V7=V7, V8=V8, V9=V9, V10=V10)
  b2 <- rbind(b1, t(colSums(b1)))
  return(b2)

}

b2a <- apply(sapply(block1(1.0), paste), 2, function(x) gsub("^0$", "-", x))

b2b <- apply(sapply(block1(0.174), paste), 2, function(x) gsub("^0$", "-", x))

b2c <- apply(sapply(block1(0.208), paste), 2, function(x) gsub("^0$", "-", x))

b2d <- apply(b2c, 2, function(x) gsub(".*", "-", x))

b2e <- apply(sapply(block1(7.532), paste), 2, function(x) gsub("^0$", "-", x))


age_code1 <- c("unter 1 Jahr","1 bis unter 5 Jahre",
               "5 bis unter 10 Jahre","10 bis unter 15 Jahre",
               "15 bis unter 20 Jahre","20 bis unter 25 Jahre",
               "25 bis unter 30 Jahre","30 bis unter 35 Jahre",
               "35 bis unter 40 Jahre","40 bis unter 45 Jahre",
               "45 bis unter 50 Jahre","50 bis unter 55 Jahre",
               "55 bis unter 60 Jahre","60 bis unter 65 Jahre",
               "65 bis unter 70 Jahre","70 bis unter 75 Jahre",
               "75 bis unter 80 Jahre","80 bis unter 85 Jahre",
               "85 Jahre und mehr",
               "Insgesamt")


(content <- data.table(V1=rep(c("2017", rep(c("2016"), 4)), each=20), V2=rep(c("01","02","03","04","DX"), each=20),
           V3=rep(paste("Kreis-",seq(1,5), sep=''), each=20),
           V4=rep(age_code1, 5),
           rbind(b2a,b2b,b2c,b2d,b2e)))



cc <- c(header, apply(content, 1, paste, collapse=';'), footer)


# write(cc, file = file.path(".", "12613-02-02-4m.csv"))

# final test

library(eoR)

# compare the files with the eoR file
cc2 <- readLines(file.path(system.file(package="eoR"), "extdata", "12613-02-02-4m.csv"))
stopifnot(all.equal(cc,cc2))



dd <- readRegDeath(file.path(system.file(package="eoR"), "extdata", "12613-02-02-4m.csv"))


length((dd))
dd

header(dd)
footer(dd)
rd1(dd)
rd1total(dd)

##
# descriptive tables and a ranges
rd1(dd)[, table(Year)]
rd1(dd)[, table(Age_Name)]
rd1(dd)[, range(AgeLow)]

content(dd)
region(dd)
protocol(dd)

# select some years
(dd2 <- selectYears(dd, c(2016)))
# descriptive tables and a ranges
rd1(dd2)[, table(Year)]
rd1(dd2)[, table(Age_Name)]
rd1(dd2)[, range(AgeLow)]

rd1(dd2)
rd1total(dd2)

# select a reagion
(dd3 <- selectRegion(dd2, "03"))
# descriptive tables and a ranges
rd1(dd3)[, table(Year)]
rd1(dd3)[, table(Age_Name)]
rd1(dd3)[, range(AgeLow)]

rd1(dd3)
rd1total(dd3)

merge(rd1(dd3), rd1total(dd3)[,c("Region_Code","Total1","Male","Female")], by = "Region_Code")



