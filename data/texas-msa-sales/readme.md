Texas real estate sales data
============================

Variables

* sales, total number of houses sold
* volume, total value of house sold
* price_avg, average house price
* price_med, median house price
* listings, number of houses listed
* inventory, average number of months listed

Broken down by:

* month, from Jan 1990 to April 2009.
* metropolitan area, as defined by the real estate centre (does not correspond to census definition).  `msa-codes.csv` matches the msa code to the name of the city/region.

Data download from the [real estate centre](http://recenter.tamu.edu/Data/datahs.html), Texas A&M University.

Locations
---------

Since the real estate centre does not use standard census place coding, and the places are a mix of metropolitan areas and counties, it was necessary to manually find locations using a (geocode)[http://www.batchgeocode.com/].  (Thanks to Patrick Hausmann for the suggestion!)

The msa codes in `msa-names.csv` are my best guess at the true msa for each location.  Note that the real estate centre has broken up some MSAs (particuarly Houston and Dallas) into multiple pieces.