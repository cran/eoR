# eoR
Data Management Package (Exposure and Occurrence Data in R)

This data management package provides some helper
classes for publicly available data sources (HMD, DESTATIS) in
Demography. We strive to encapsulate data in easy to use S4 objects.

If original data is provided in a text file, the resulting S4
object contains all information from that text file. But the
information is somehow structured (header, footer, etc).
Further the classes provide methods to make a subset for selected
calendar years or selected regions. The resulting subset
objects still contain the original header and footer information.


