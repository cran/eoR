# Examples eoR package class RE1
# Copyright (C) 2019 Rainer Walke

require(data.table)
require(eoR)

# read all information from 'GENESIS-Tabelle: 12411-03-03-4'
# example data similar to data from
# Statistische &Auml;mter des Bundes und der L&auml;nder, Deutschland, 2019
# Bev&ouml;lkerung nach Geschlecht, Nationalit&auml;t und Altersgruppen
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
