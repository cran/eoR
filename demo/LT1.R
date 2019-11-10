# Examples eoR package class LT1
# Copyright (C) 2019 Rainer Walke

require(data.table)
require(eoR)

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
