USPS Vacancy
================

Clean data includes residential and business vacancies.
Data is organized by:

* Year (06-09)
* Quarter
* State fips code
* County fips code
* Total: Total number of addresses that USPS has in their database 
* Vac: total number of vacant addresses 
* Nostat:Total Count of no-stat Addresses
* vac_days: days addresses vacant
* Nostat_days: days addresses are no-stat  

Original data for 2006-2007 was catagorized by "All" vacancies. Then form 2008-2009, data was seperated into "Residential", "Business" and "Other". Exploration into the data showed that the 2006-2007 "All" included only "Residential" and "Business" data therefore, "Other" was not used to calculate "All" for 2008-2009. These calculations can be seen in 2-clean.r

Data was downloaded using 1-download.r and can be found at [HUDuser](http://www.huduser.org/datasets/usps.html)



