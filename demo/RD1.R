# Examples eoR package class RD1
# Copyright (C) 2019 Rainer Walke

require(data.table)
require(eoR)


# read all information from 'GENESIS-Tabelle: 12613-02-02-4'
# example data similar to data from
# Statistische &Auml;mter des Bundes und der L&auml;nder, Deutschland, 2019
# Gestorbene nach Geschlecht, Nationalit&auml;t und Altersgruppen -
# Jahressumme - regionale Tiefe: Kreise und krfr. St&auml;dte
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
