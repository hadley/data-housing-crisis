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


About the CPS survey
=====================

The CPS survey is a monthly survey of about 60,000 households. A Sample of housing units is selected after each 10 year census.  This sample is then used until the next sample is created after the next 10 year census. During the 10 years a small percentage of all newly constructed houses is added to the sample to make up for houses that are destroyed, lost, etc. There is a complex method of rotating which houses in the sample will be visited each month (see below). On any given month, 75% of the housing units in the sample will be visited.  All residents of the unit over the age of 16 will be interviewed.

Rotation of sample. Part of the sample is changed each month. Each monthly sample is divided into eight representative subsamples or rotation groups. A given rotation group is interviewed for a total of 8 months, divided into two equal periods. The group is in the sample for 4 consecutive months, leaves the sample during the following 8 months, and then returns for another 4 consecutive months. In each monthly sample, 1 of the 8 rotation groups is in the first month of enumeration, another rotation group is in the second month, and so on. (The rotation group in the fifth month of enumeration is returning after an 8-month break.) Under this system, 75 percent of the sample is common from month to month and 50 percent is common from year to year for the same month. This procedure provides a substantial amount of month-to-month and year-to-year overlap in the sample, thus yielding better estimates of change and reducing discontinuities in the series of data without burdening sampled households with an unduly long period of inquiry.