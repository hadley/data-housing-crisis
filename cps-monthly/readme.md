Current population survey data
===============================

This data comes from the (CPS)[http://www.census.gov/cps/].  All files are listed on http://www.bls.census.gov/cps_ftp.html, but do not have a consistent naming scheme.  The ruby code in `0-get-urls.rb` processes this page to extract urls for the basic monthly CPS, which are then downloaded by the R code in `1-download.r`.

`variables.txt` lists the names and position of variables to be extracted.  Because of the large size of the data, everything is kept in gzip format, so in R, `gzfile()` is necessary to access the data directly.