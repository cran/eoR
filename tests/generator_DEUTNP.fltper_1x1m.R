# Rainer Walke, MPIDR Rostock
# create artifical example data
# DEUTNP.fltper_1x1m.txt

require(data.table)

h1 <- 1
h2 <- 2

f1 <- h2 + 111 * 7 + 1
f2 <- f1 + 0


header <- c("Exland, Life tables (period 1x1), Females	Last modified: 01 Jan 2018;  Methods Protocol: vY (2017)",
            rep("", times=(h2-1)))

header

footer <- c(rep("t", times=(f2-f1)))

footer

####

block3 <- function(mult) {

  # simple Siler model, see for example https://www.demographic-research.org/volumes/vol38/29/38-29.pdf


  alpha_t <- 5.124e-03
  beta1_t <- 2.048
  c_t     <- 1.627e-04
  beta2_t <- 0.1013
  M_t     <- 84.79  * mult


  ltA <- data.table(AgeLow=seq(0,110))
  ltA[, mx := alpha_t * exp(-beta1_t * AgeLow) + c_t + beta2_t * exp(beta2_t * (AgeLow - M_t))][]
  ltA[, ax := c(0.14, rep(0.5, 109), 1/ltA[AgeLow==110,mx])][]

  ltA[, qx := mx/(1 + (1-ax)*mx)][]
  setcolorder(ltA, c("AgeLow", "mx", "qx", "ax"))

  ltA[, px := 1 - qx][]


  # ltA[,pxlag := shift(px, 1L, type="lag")]

  ltA[AgeLow==0, lx := 100000][]

  for (x in seq(1,110)) {

    ltA[AgeLow == x, lx := ltA[AgeLow == (x-1), lx] * ltA[AgeLow == (x-1), px]]

  }

  ltA[AgeLow == 110, dx := lx][]
  ltA[AgeLow == 110, Lx := lx * ax][]

  for (x in seq(109,0)) {

    ltA[AgeLow == x, dx := lx * qx]
    ltA[AgeLow == x, Lx := lx - ( 1 - ax ) * dx]

  }

  ltA[AgeLow == 110, Tx := Lx][]

  for (x in seq(109,0)) {

    ltA[AgeLow == x, Tx := ltA[AgeLow == (x+1), Tx] + Lx]

  }

  ltA[, ex := Tx / lx][]


  ltA[, px := NULL]
  ltA[, AgeLow := NULL]

  ltA[, Age := c( paste(seq(0,109)," ",sep=""), "110+")]
  setcolorder(ltA, "Age")

  # round everything
  ltA[, ':=' (mx = round(mx,5), qx = round(qx,5), ax = round(ax,2),
              lx = ceiling(lx), dx = ceiling(dx), Lx = ceiling(Lx),
              Tx = ceiling(Tx), ex = round(ex,2))][]

  return(ltA)
}


# block3(1.0)


b2a <- block3(0.99)

b2b <- block3(1.00)

b2c <- block3(1.01)

b2d <- block3(1.02)

b2e <- block3(1.03)

b2f <- block3(1.04)

b2g <- block3(1.05)

(content <- data.table( Year = rep( paste(seq(2011,2017)), each = 111),
                        rbind(b2a,b2b,b2c,b2d,b2e,b2f,b2g)
                        ))

content2 <- apply(content,2, format, justify="right", width=6)

content3 <- paste(content2[,1], "       ", content2[,2], "     ",
                  content2[,3], "  ", content2[,4], "",
                  content2[,5], "  ", content2[,6], "  ",
                  content2[,7], "  ", content2[,8], "  ",
                  content2[,9], " ", content2[,10], sep="")

colnames1 <- "  Year          Age         mx       qx    ax      lx      dx      Lx       Tx     ex"

cc <- c(header, colnames1, content3, footer)


# write(cc, file = file.path(".", "DEUTNP.fltper_1x1m.txt"))


# final test

library(eoR)

# compare the files with the eoR file
cc2 <- readLines(file.path(system.file(package="eoR"), "extdata", "DEUTNP.fltper_1x1m.txt"))
stopifnot(all.equal(cc,cc2))


dd <- readLT1x1(file.path(system.file(package="eoR"), "extdata", "DEUTNP.fltper_1x1m.txt"))

length((dd))
dd

header(dd)
lt1(dd)

##
# descriptive tables and a ranges
lt1(dd)[, table(Year)]
lt1(dd)[, table(Age)]
lt1(dd)[, range(AgeLow)]

content(dd)
region(dd)
protocol(dd)

# select some years
(dd2 <- selectYears(dd, c(2016)))
# descriptive tables and a ranges
lt1(dd2)[, table(Year)]
lt1(dd2)[, table(Age)]
lt1(dd2)[, range(AgeLow)]

lt1(dd2)


