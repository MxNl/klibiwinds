
<!-- README.md is generated from README.Rmd. Please edit that file -->

# klibiwinds

<!-- badges: start -->
<!-- badges: end -->

The goal of klibiwinds is to provide the required functionality to
calculate the defined indicators in the project klibiw.

## Installation

You can install the development version of klibiwinds from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("MxNl/klibiwinds")
```

## Example

This is a basic example which shows you how to solve a common problem:

Load the package `klibiwinds`

``` r
library(klibiwinds)
library(ggplot2)
#> Warning: Paket 'ggplot2' wurde unter R Version 4.1.3 erstellt
library(summarytools)
#> Warning: Paket 'summarytools' wurde unter R Version 4.1.3 erstellt
library(dplyr)
#> Warning: Paket 'dplyr' wurde unter R Version 4.1.3 erstellt
#> 
#> Attache Paket: 'dplyr'
#> Die folgenden Objekte sind maskiert von 'package:stats':
#> 
#>     filter, lag
#> Die folgenden Objekte sind maskiert von 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

We use the fake data set `gwl_data_fake` included in this package to
demonstrate the following examples

``` r
gwl_fake_data
#> # A tibble: 3,168 x 4
#>    well_id climate_model_name date         gwl
#>    <chr>   <chr>              <date>     <dbl>
#>  1 1000    climate_model_1    1980-01-15     1
#>  2 1000    climate_model_1    1980-02-15     2
#>  3 1000    climate_model_1    1980-03-15     1
#>  4 1000    climate_model_1    1980-04-15     2
#>  5 1000    climate_model_1    1980-05-15     1
#>  6 1000    climate_model_1    1980-06-15     2
#>  7 1000    climate_model_1    1980-07-15     1
#>  8 1000    climate_model_1    1980-08-15     2
#>  9 1000    climate_model_1    1980-09-15     1
#> 10 1000    climate_model_1    1980-10-15     2
#> # ... with 3,158 more rows
```

Or check out the structure and the content using a summary of the
dataframe

``` r
gwl_fake_data |> 
  dfSummary()
#> Data Frame Summary  
#> gwl_fake_data  
#> Dimensions: 3168 x 4  
#> Duplicates: 0  
#> 
#> -----------------------------------------------------------------------------------------------------------------
#> No   Variable             Stats / Values         Freqs (% of Valid)    Graph                 Valid      Missing  
#> ---- -------------------- ---------------------- --------------------- --------------------- ---------- ---------
#> 1    well_id              1. 1000                1584 (50.0%)          IIIIIIIIII            3168       0        
#>      [character]          2. 1001                1584 (50.0%)          IIIIIIIIII            (100.0%)   (0.0%)   
#> 
#> 2    climate_model_name   1. climate_model_1     1056 (33.3%)          IIIIII                3168       0        
#>      [character]          2. climate_model_2     1056 (33.3%)          IIIIII                (100.0%)   (0.0%)   
#>                           3. historical          1056 (33.3%)          IIIIII                                    
#> 
#> 3    date                 min : 1980-01-15       528 distinct values   : : : : : : : : : :   3168       0        
#>      [Date]               med : 2001-12-30                             : : : : : : : : : :   (100.0%)   (0.0%)   
#>                           max : 2023-12-15                             : : : : : : : : : :                       
#>                           range : 43y 11m 0d                           : : : : : : : : : :                       
#>                                                                        : : : : : : : : : :                       
#> 
#> 4    gwl                  Mean (sd) : 4 (1.8)    1 : 264 ( 8.3%)       I                     3168       0        
#>      [numeric]            min < med < max:       2 : 528 (16.7%)       III                   (100.0%)   (0.0%)   
#>                           1 < 4 < 7              3 : 528 (16.7%)       III                                       
#>                           IQR (CV) : 2.5 (0.4)   4 : 528 (16.7%)       III                                       
#>                                                  5 : 528 (16.7%)       III                                       
#>                                                  6 : 528 (16.7%)       III                                       
#>                                                  7 : 264 ( 8.3%)       I                                         
#> -----------------------------------------------------------------------------------------------------------------
```

This fake data set also shows the structure, that you need to bring your
data into:

-   one column with the name `well_id` containing an identifier for the
    observation well
-   one column with the name `climate_model_name` containing an
    identifier for the climate model. Note: historical (not predicted)
    data has the value “historical” in this column
-   one column with the name `date` containing the dates
-   one column with the name `gwl` containing the groundwater levels

**Note: This dataframe is the only basis for the following processing
steps. So before you continue here with your own data, make sure that
your dataframe is brought into the same shape and structure!**

Now we can add a column containing the reference period for each
corresponding year by using the function `add_reference_period_column()`

``` r
gwl_with_ref_periods <- gwl_fake_data |>
  add_reference_period_column(reference_periods_fake)
```

This function also drops all rows that don’t belong to reference period
as they are not used in the following steps.

Then, we can reduce this dataframe in long format to a dataframe where
we have a distinct combination of the columns `well_id`,
`climate_model_name` and `reference_period` using the function
`make_summary_table()`

``` r
indicators_summary <- gwl_with_ref_periods |>
  make_summary_table()
```

Check out the result

``` r
indicators_summary
#> # A tibble: 10 x 3
#>    well_id climate_model_name reference_period
#>    <chr>   <chr>              <chr>           
#>  1 1000    climate_model_1    Z1              
#>  2 1000    climate_model_1    Z2              
#>  3 1000    climate_model_2    Z1              
#>  4 1000    climate_model_2    Z2              
#>  5 1000    historical         Z1              
#>  6 1001    climate_model_1    Z1              
#>  7 1001    climate_model_1    Z2              
#>  8 1001    climate_model_2    Z1              
#>  9 1001    climate_model_2    Z2              
#> 10 1001    historical         Z1
```

Then, the final step is to calculate the indicators using the function
`add_indicators_all()`

``` r
indicators_summary <- indicators_summary |> 
  add_indicators_all(gwl_with_ref_periods)
```

Again, we can check out the result

``` r
indicators_summary
#> # A tibble: 10 x 18
#>    well_id climate_mod~1 refer~2 indic~3 indic~4 indic~5 indic~6 indic~7 indic~8
#>    <chr>   <chr>         <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1 1000    climate_mode~ Z1            1       2     1.5   0.511       1       2
#>  2 1000    climate_mode~ Z2            1       2     1.5   0.511       1       2
#>  3 1000    climate_mode~ Z1            2       3     2.5   0.511       2       3
#>  4 1000    climate_mode~ Z2            2       3     2.5   0.511       2       3
#>  5 1000    historical    Z1            3       4     3.5   0.511       3       4
#>  6 1001    climate_mode~ Z1            4       5     4.5   0.511       4       5
#>  7 1001    climate_mode~ Z2            4       5     4.5   0.511       4       5
#>  8 1001    climate_mode~ Z1            5       6     5.5   0.511       5       6
#>  9 1001    climate_mode~ Z2            5       6     5.5   0.511       5       6
#> 10 1001    historical    Z1            6       7     6.5   0.511       6       7
#> # ... with 9 more variables: indicator_17 <dbl>, indicator_18 <dbl>,
#> #   indicator_21 <int>, indicator_22 <int>, indicator_23 <int>,
#> #   indicator_24 <int>, mean.x <dbl>, mean.y <dbl>, indicator_33 <list>, and
#> #   abbreviated variable names 1: climate_model_name, 2: reference_period,
#> #   3: indicator_11, 4: indicator_12, 5: indicator_13, 6: indicator_14,
#> #   7: indicator_15, 8: indicator_16
```

This dataframe contains a list column with nested dataframes for the
indicator 3.3 because this indicator comprises multiple values. this
column can be unnested using the function `unnest_indicator_3_3()`

``` r
indicators_summary |> 
  unnest_indicator_3_3()
#> # A tibble: 10 x 51
#>    well_id climate_mod~1 refer~2 indic~3 indic~4 indic~5 indic~6 indic~7 indic~8
#>    <chr>   <chr>         <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1 1000    climate_mode~ Z1            1       2     1.5   0.511       1       2
#>  2 1000    climate_mode~ Z2            1       2     1.5   0.511       1       2
#>  3 1000    climate_mode~ Z1            2       3     2.5   0.511       2       3
#>  4 1000    climate_mode~ Z2            2       3     2.5   0.511       2       3
#>  5 1000    historical    Z1            3       4     3.5   0.511       3       4
#>  6 1001    climate_mode~ Z1            4       5     4.5   0.511       4       5
#>  7 1001    climate_mode~ Z2            4       5     4.5   0.511       4       5
#>  8 1001    climate_mode~ Z1            5       6     5.5   0.511       5       6
#>  9 1001    climate_mode~ Z2            5       6     5.5   0.511       5       6
#> 10 1001    historical    Z1            6       7     6.5   0.511       6       7
#> # ... with 42 more variables: indicator_17 <dbl>, indicator_18 <dbl>,
#> #   indicator_21 <int>, indicator_22 <int>, indicator_23 <int>,
#> #   indicator_24 <int>, indicator_33_mean.x_month1 <dbl>,
#> #   indicator_33_mean.x_month2 <dbl>, indicator_33_mean.x_month3 <dbl>,
#> #   indicator_33_mean.x_month4 <dbl>, indicator_33_mean.x_month5 <dbl>,
#> #   indicator_33_mean.x_month6 <dbl>, indicator_33_mean.x_month7 <dbl>,
#> #   indicator_33_mean.x_month8 <dbl>, indicator_33_mean.x_month9 <dbl>, ...
```
