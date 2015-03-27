<!-- README.md is generated from README.Rmd. Please edit that file -->
opencontext: An R API client for the Open Context archeological database
------------------------------------------------------------------------

[Open Context](http://opencontext.org/)

This packages enables browsing and downloading data from Open Context using R.

Installation
------------

Install `opencontext`

``` r
install.packages("devtools")
devtools::install_github("ropensci/opencontext")
```

``` r
library("opencontext")
```

Browse countries
----------------

To browse the countries that Open Context has data on:

``` r
countries <- oc_browse("countries")
```

The result is a data table that include the names of the countries in `countries$labels`, and URLs that we can use to get more information about what projects, etc. are avialable for each country in `countries$id`

------------------------------------------------------------------------

[![ropensci\_footer](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
