Housing Opportunity Index
=========================

Cleaned Data set contains

* Count of number of homes sold
* Housing Opportunity Index
* Weighted average of fixed and adjustable interest rates for that quarter
* Median (household?) income
* Median price of homes sold
* National and regional rankings of area's HOI

Data is given for

* Quarter from the 1st quarter of 1991 to the last quarter of 2008 (but all of 2002 and the first 3 quarters of 2003 are missing)
* 276 cities recorded by city name, two letter state abbreviation, and msa fips code

Reservations
============
Data set is missing entries between the 1st quarter of 2002 and the last quarter of 2003.
The data set also has a huge missing data problem overall.


The Housing Opportunity Index (HOI) for a given area is defined as the
share of homes sold in that area that would have been affordable to a
family earning the local median income based on standard mortgage
underwriting criteria.  Therefore, there are really two major
components -- income and housing cost.

For income, NAHB uses the annual median family income estimates for
metropolitan areas published by the Department of Housing and Urban
Development.  NAHB assumes that a family can afford to spend 28
percent of its gross income on housing; this is a conventional
assumption in the lending industry.  That share of median income is
then divided by twelve to arrive at a monthly figure.

On the cost side, NAHB receives every month a CD of sales transaction
records from First American Real Estate Solutions (formerly, TRW).
The data include information on state, county, date of sale, and sales
price of homes sold.  The monthly principal and interest that an owner
would pay is based on the assumption of a 30 year fixed rate mortgage,
with a loan for 90 percent of the sales price (i.e., 10 percent
downpayment).  The interest rate is a weighted average of fixed and
adjustable rates during that quarter, as reported by the Federal
Housing Finance Board.  In addition to principal and interest, cost
also includes estimated property taxes and property insurance for that
home.  This is based on metropolitan estimates of tax and insurance
rates from the 2000 Decennial Census, as estimated by NAHB from the
Census Bureau's Public Use Microdata Sample (PUMS).  Mortgage
insurance is not currently a component of the HOI.

Therefore, for each record, there is an estimated monthly cost and
available income share.  The HOI is the share of records in a
metropolitan area for which the monthly income available for housing
is at or above the monthly cost for that unit.