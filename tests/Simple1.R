# Rainer Walke, MPIDR Rostock

library(data.table)
library(eoR)


####

# example and trials

# read all information from the file fltper_1x1
# example data similar to data from Human Mortality Database https://www.mortality.org/
# period life tables 1x1 female
infile <- file.path(system.file(package="eoR"), "extdata", "DEUTNP.fltper_1x1m.txt")

o1 <- readLT1x1(infile)

length((o1))
o1

header(o1)
lt1(o1)

# descriptive tables and a ranges
lt1(o1)[, table(Year)]
lt1(o1)[, table(Age)]
lt1(o1)[, range(ex)]

content(o1)
region(o1)
protocol(o1)

# select some years
(o2 <- selectYears(o1, c(2011,2012)))
# descriptive tables and a ranges
lt1(o2)[, table(Year)]
lt1(o2)[, table(Age)]
lt1(o2)[, range(ex)]

lt1(o2)


class(o1)
showClass("LT1")
showMethods("length")


# read all information from the file Exposures_1x1
# example data similar to data from Human Mortality Database https://www.mortality.org/
# period life tables 1x1 female
infile2 <- file.path(system.file(package="eoR"), "extdata", "DEUTNP.Exposures_1x1m.txt")

e1 <- readEX1x1(infile2)

length((e1))
e1

header(e1)
ex1(e1)

# descriptive tables and a ranges
ex1(e1)[, table(Year)]
ex1(e1)[, table(Age)]
ex1(e1)[, range(AgeLow)]

content(e1)
region(e1)
protocol(e1)

# select some years
(e2 <- selectYears(e1, c(2011,2012)))
# descriptive tables and a ranges
ex1(e2)[, table(Year)]
ex1(e2)[, table(Age)]
ex1(e2)[, range(AgeLow)]

ex1(e2)

class(e1)
showClass("EX1")
showMethods("length")

# read all information from 'GENESIS-Tabelle: 12613-02-02-4'
# example data similar to data from
# Statistische Ämter des Bundes und der Länder, Deutschland, 2019
# Gestorbene nach Geschlecht, Nationalität und Altersgruppen -
# Jahressumme - regionale Tiefe: Kreise und krfr. Städte
infile3 <- file.path(system.file(package="eoR"), "extdata", "12613-02-02-4m.csv")

d1 <- readRegDeath(infile3)

length((d1))
d1

header(d1)
footer(d1)
rd1(d1)
rd1total(d1)

# descriptive tables and a ranges
rd1(d1)[, table(Year)]
rd1(d1)[, table(Age_Name)]
rd1(d1)[, range(AgeLow)]

content(d1)
region(d1)
protocol(d1)

# select some years
(d2 <- selectYears(d1, c(2016)))
# descriptive tables and a ranges
rd1(d2)[, table(Year)]
rd1(d2)[, table(Age_Name)]
rd1(d2)[, range(AgeLow)]

rd1(d2)
rd1total(d2)

# select a reagion
(d3 <- selectRegion(d2, "03"))
# descriptive tables and a ranges
rd1(d3)[, table(Year)]
rd1(d3)[, table(Age_Name)]
rd1(d3)[, range(AgeLow)]

rd1(d3)
rd1total(d3)

merge(rd1(d3), rd1total(d3)[,c("Region_Code","Total1","Male","Female")], by = "Region_Code")

class(d1)
showClass("RD1")
showMethods("length")


# read all information from 'GENESIS-Tabelle: 12411-03-03-4'
# example data similar to data from
# Statistische Ämter des Bundes und der Länder, Deutschland, 2019
# Bevölkerung nach Geschlecht, Nationalität und Altersgruppen
infile4 <- file.path(system.file(package="eoR"), "extdata", "12411-03-03-4m.csv")

r1 <- readRegExp(infile4)

length((r1))
r1

header(r1)
footer(r1)
re1(r1)
re1total(r1)

##
# descriptive tables and a ranges
re1(r1)[, table(Year)]
re1(r1)[, table(Age_Name)]
re1(r1)[, range(AgeLow)]

content(r1)
region(r1)
protocol(r1)

# select some years
(r2 <- selectYears(r1, c(2016)))
# descriptive tables and a ranges
re1(r2)[, table(Year)]
re1(r2)[, table(Age_Name)]
re1(r2)[, range(AgeLow)]

re1(r2)
re1total(r2)

# select a reagion
(r3 <- selectRegion(r2, "03"))
# descriptive tables and a ranges
re1(r3)[, table(Year)]
re1(r3)[, table(Age_Name)]
re1(r3)[, range(AgeLow)]

re1(r3)
re1total(r3)

merge(re1(r3), re1total(r3)[,c("Region_Code","Total1","Male","Female")], by = "Region_Code")

class(r1)
showClass("RE1")
showMethods("length")


sessionInfo()
