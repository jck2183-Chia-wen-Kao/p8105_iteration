Iteration
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Do Something Simple

``` r
x_vec <- rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.96228867  1.10130987 -0.07908139 -1.00604211 -0.16444606  0.80448109
    ##  [7] -0.09199523 -1.15139429 -0.69418032 -0.15050038  1.45357506 -0.28039638
    ## [13] -0.43017244  0.74459489 -0.55825354  1.02519238 -1.02285578  0.98759795
    ## [19] -0.34584378 -1.14230305 -2.29884186 -0.56537625  1.29544718  0.73654350
    ## [25]  1.92026483  0.47446540  0.16911661 -0.23450144 -1.11991802  1.58580223

I want a function to compute Z score

``` r
z_score <- function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) <  3) {
    stop("Input must have at least three numbers")
  }
  z = (x - mean(x)) / sd(x)
  
  
  return(z)
}

z_score(x_vec)
```

    ##  [1] -0.96228867  1.10130987 -0.07908139 -1.00604211 -0.16444606  0.80448109
    ##  [7] -0.09199523 -1.15139429 -0.69418032 -0.15050038  1.45357506 -0.28039638
    ## [13] -0.43017244  0.74459489 -0.55825354  1.02519238 -1.02285578  0.98759795
    ## [19] -0.34584378 -1.14230305 -2.29884186 -0.56537625  1.29544718  0.73654350
    ## [25]  1.92026483  0.47446540  0.16911661 -0.23450144 -1.11991802  1.58580223

Try my function on some other things. These should give error.

``` r
z_score(3)
```

    ## Error in z_score(3): Input must have at least three numbers

``` r
z_score("my name is Jeff")
```

    ## Error in z_score("my name is Jeff"): Input must be numeric

``` r
z_score(mtcars)
```

    ## Error in z_score(mtcars): Input must be numeric

``` r
z_score(c(TRUE, FALSE, TRUE, TRUE))
```

    ## Error in z_score(c(TRUE, FALSE, TRUE, TRUE)): Input must be numeric
