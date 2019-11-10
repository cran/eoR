# Examples eoR package class EX1
# Copyright (C) 2019 Rainer Walke

require(data.table)
require(eoR)

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
