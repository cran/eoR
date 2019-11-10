# Rainer Walke, MPIDR Rostock
# create artifical example data
# DEUTNP.Exposures_1x1m.txt

require(data.table)

h1 <- 1
h2 <- 2

f1 <- h2 + 111 * 7 + 1
f2 <- f1 + 0

header <- c("Exland, Exposure to risk (period 1x1), 	Last modified: 01 Jan 2018;  Methods Protocol: vY (2017)",
            rep("", times=(h2-1)))
header

footer <- c(rep("t", times=(f2-f1)))

footer


block2 <- function(mult) {

  Female <- c(436000, 437200, 437000, 428000, 414000, 409000, 407000, 418000, 421000, 424000,
              413000, 396000, 395000, 389000, 381000, 381500, 390000, 413000, 477000, 519000,
              555000, 605000, 635000, 656000, 670000, 679000, 687000, 677000, 663000, 650000,
              632000, 611000, 588000, 578000, 566000, 559000, 553000, 551000, 549000, 547000,
              545000, 519000, 476000, 462000, 382000, 439000, 508000, 505000, 554000, 628000,
              644000, 621000, 584000, 563000, 554000, 532000, 469000, 424000, 433000, 456000,
              467000, 465000, 459000, 458500, 473000, 471000, 462000, 469500, 492000, 501000,
              434000, 403000, 232000, 236000, 283000, 360000, 395000, 384000, 359000, 335000,
              321000, 296000, 265000, 234000, 201000, 170000, 144000, 120000, 99000,  77000,
              59000,  45000,  34000,  24000,  17000,  11000,  7000,   5000,   3000,   2000,
              1000, 500, 250, 150, 90, 50, 20, 10, 6, 10,
              1)

  Male <-   c(465000, 460200, 461000, 451000, 436000, 430000, 428000, 440000, 443000, 445000,
              433000, 417000, 417500, 410000, 401000, 403000, 411000, 435000, 501000, 543000,
              578000, 631000, 666000, 691000, 710000, 725000, 734000, 723000, 703000, 687000,
              669000, 648000, 624000, 612000, 597000, 584000, 571000, 566000, 565000, 564000,
              564500, 542000, 499000, 483000, 397000, 455000, 529000, 528000, 578000, 652000,
              666000, 640000, 600000, 575000, 561000, 537000, 471000, 421000, 424000, 439000,
              443000, 433000, 407000, 367500, 343000, 311000, 284000, 285000, 293000, 287000,
              243000, 168000, 127000, 126000, 146000, 180000, 194000, 186000, 169000, 151000,
              138000, 124000, 109000, 93000,  79000,  66000,  53000,  42000,  33000,  24000,
              17000,  12000,  8000,   6000,   4000,   2500,   1600,   1000,   700,    500,
              250, 120, 60, 30, 20, 10, 6, 3, 1, 2,
              0.3)

  Female1 <-  round(Female * mult, 2)
  Male1 <-  round(Male * mult, 2)
  Total1 <- Female1 + Male1


  return( data.table(Female = Female1, Male = Male1, Total = Total1) )

}

b2a <- block2(0.95*pi/3.14)

b2b <- block2(0.92*pi/3.14)

b2c <- block2(0.94*pi/3.14)

b2d <- block2(0.93*pi/3.14)

b2e <- block2(0.89*pi/3.14)

b2f <- block2(0.88*pi/3.14)

b2g <- block2(0.87*pi/3.14)




age_code3 <- c( paste(seq(0,109)," ",sep=""), "110+")


(content <- data.table( Year = rep( paste(seq(2011,2017)), each = 111),
                        Age = rep(age_code3, 7),
                        rbind(b2a,b2b,b2c,b2d,b2e,b2f,b2g)
                        ))

content2 <- apply(content,2, format, justify="right", width=6)

content3 <- paste(content2[,1], "     ", content2[,2], "         ",
                  content2[,3], "     ", content2[,4], "    ",
                  content2[,5])

colnames1 <- "  Year          Age             Female            Male           Total"

cc <- c(header, colnames1, content3, footer)


# write(cc, file = file.path(".", "DEUTNP.Exposures_1x1m.txt"))

# final test

library(eoR)

# compare the files with the eoR file
cc2 <- readLines(file.path(system.file(package="eoR"), "extdata", "DEUTNP.Exposures_1x1m.txt"))
stopifnot(all.equal(cc,cc2))


dd <- readEX1x1(file.path(system.file(package="eoR"), "extdata", "DEUTNP.Exposures_1x1m.txt"))


length((dd))
dd

header(dd)
ex1(dd)

##
# descriptive tables and a ranges
ex1(dd)[, table(Year)]
ex1(dd)[, table(Age)]
ex1(dd)[, range(AgeLow)]

content(dd)
region(dd)
protocol(dd)

# select some years
(dd2 <- selectYears(dd, c(2016)))
# descriptive tables and a ranges
ex1(dd2)[, table(Year)]
ex1(dd2)[, table(Age)]
ex1(dd2)[, range(AgeLow)]

ex1(dd2)


