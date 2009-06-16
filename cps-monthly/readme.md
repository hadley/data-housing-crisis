Current population survey data
===============================

This data comes from the (CPS)[http://www.census.gov/cps/].  All files are listed on http://www.bls.census.gov/cps_ftp.html, but do not have a consistent naming scheme.  The ruby code in `0-get-urls.rb` processes this page to extract urls for the basic monthly CPS, which are then downloaded by the R code in `1-download.r`.

`variables.txt` lists the names and position of variables to be extracted.  Because of the large size of the data, everything is kept in gzip format, so in R, `gzfile()` is necessary to access the data directly.

Variables
=========

* Location: state, county and MSA fips codes
* Date: year and month, from Jan 2001 to Apr 2009
* Type of housing unit
* Highest level of school
* Employment, full time or part time
* Reason for unemployment
* Industry and occupation
