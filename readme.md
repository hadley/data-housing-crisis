Project Overview
==================

The US housing crisis has undermined the world economy in far reaching and poorly understood ways. Although there is a lot of speculation over the causes and the effects of the housing crisis, most hypotheses are not backed up by data. We hope to promote well-informed policy and discussion, and aid exploration and analysis, by making creating an accessible and reproducible repository of data and analysis

Data related to the housing crisis exists in large (up to 10 gb), independent, and often messy data sets. The variety and inconsistency of data creates an obstacle for analysis, and this summer we have working to provide views of this data that are consistent, concise and complete. To ensure that all manipulation is transparent, both data cleaning and analysis have been carried out with the source statistical software [R](http://www.r-project.org). Both code and data are freely licensed and made available on [github](http://www.github.com/hadley/data-housing-crisis). To date we have cleaned and organised 13 data sets related to the housing crisis, and by keeping the code transparent and reproducible, we hope to inspire others to contribute their data and ideas.

This research project is a collaboration between Rice University undergrads, graduate students, and Hadley Wickham, an Assistant Professor of Statistics. It is funded by the NSF's Vertically Integrated Grants for Research and Education in Mathematical Sciences (VIGRE) program, NSF grant DMS-0739420. 

Data Set Overview
======================

* **American Community Survey (ACS):** ACS is a 1% sample of all houses in the USA. It contains 1,293,393 housing records and 2,946,342 people and is collected yearly. We extracted data related to migration and second  housing information.  

* **Case-Shiller Housing Price Index:** Case-Shiller HPI measures average change in prices for single-family homes and is calculated monthly. This data covers 20 Metropolitan Statistical Areas (MSA). See below for the difference between Case-Shiller and Federal Housing Finance Agency house price indices.  

* **Census Housing Units:** Data set is from the U.S. Census. It records the number of housing units by state and county. The data covers years 2000 through 2007. The number of houisng unites in 2000 is from the 2000 census. For ever year after, the estimate was based on July 1st of that year. For a definition of housing unit, please see "Terms" below.  

* **Census Population:** Data set is also from the U.S. Census. Cleaned data set contains information regarding population for U.S. Core Based Statistical Areas (CBSA). Has information such as population estimate, deaths, births, and migration. The data covers years 2001-2008. For a definition of CBSA, please see "Terms" below.  

* **Cenus QuickFacts:** Data set contains basic demographic and geographical information. It includes information for all states and counties, and cities with more than 25,000 people.  

* **City Location:** The clean data set contains information for all major cities for every state. It includes longitude and latitude, and the multiple location codes used throughout the data set. Please see “Terms” for the list and definition of the different types of locations used.  

* **Construction Housing Units:** This information was taken from the U.S. census. It records new residential construction in Metropolitan Statistical Areas. The data is collected monthly and covers years 2000 through 2009.  

* **County Labor Force:** This data set is from the U.S. Bureau of Labor Statistics. It contain information regarding labor force and unemployment for every state and county. It covers years 2000 through 2008.  

* **Federal Housing Finance Agency (FHFA) House Price Index:** Data measures of the movement of single- family house prices. This data is from repeat mortgage transactions purchased or securitized by Fannie Mae or Freddie Mac. This data is collected quarterly and covers years 2000 through 2009.  

* **Metropolitan Gross Domestic Product (GDP):** This data set is from the U.S. Bureau of Economic Analysis. Data records how much money has been made from specific industries such as agriculture and manufacturing. The data covers Metropolitan Statistical Areas and is collected yearly (2001-2006).  

* **Market Rents:** Data is from U.S. Department of Housing and Urban Development. It records the median monthly rent for Metropolitan Statistical Areas and counties.  This data covers years 2003 through 2009.  

* **Texas MSA Sales:** This data was downloaded from Texas A&M University real estate center. Data includes housing variables such as total number of houses sold, average house price, and average number of houses listed. Covers only Texas metropolitan areas as defines by the real estate center. We have matched this to census Metropolitan Statistical Areas as best as possible. This data was collected monthly from January 1990 to April 2009.  

* **United States Postal Service (USPS ) Vacancy:** This data was downloaded from the U.S. Department of Housing and Urban Development. Data includes total number of addresses and number of residential and business vacancies. This data was collected quarterly and covers all states and counties for years 2006 through 2009.  

Terms
=====

* **House Price Index:** A scale representing the average value of specified prices as compared with some reference figure. In our data set this reference figure is usually the value of the first recorded HPI. (HPI Current / HPI index date)*100  

* **Gross Domestic Product:** Amount of money a city's economy generates from a certain industry.  

* **Housing Unit:** A place where a single family lives (includes houses, apartments, condos etc.).  

* **FIPS Code:** "Federal Information Processing Standard" Every state, county, and region has a FIPS code.  

Locations
----------

* MSA (Metropolitan Statistical Area): The Census Bureau defines an MSA as one or more counties that has a population of at least 50,000 people, plus adjacent territory that has a high degree of social and economic similarities.
* CBSA (Core Based Statistical Area): CBSA contains both MSAs and new created micropolitan areas.
* Micropolitan Area: Census Bureau defines micropolitan areas as urban clusters of at least 10,000 and fewer than 50,000 people.
* PUMA (Public Use Microdata Area): PUMA consists of 5% of the population.


Future plans
============

We would like to develop a website that will allow users to easily access the data they are interested in, which would otherwise be a daunting task for those who wish to use a data set of this size. Because our analysis and findings also involve large amounts of information, (such as construction price time series for each US metropolitan area) we are exploring interactive graphical methods for displaying this information.  
