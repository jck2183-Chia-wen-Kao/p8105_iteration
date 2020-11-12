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

    ##  [1]  1.130633434  1.102704001 -0.338263499  0.698619639 -2.034033627
    ##  [6] -0.545660985  0.126854078  0.002196000 -1.678578869 -0.094923947
    ## [11]  0.350996623  0.208339043  0.693356414 -0.658470228 -0.538549607
    ## [16]  0.496109055  0.991691310 -0.213716113  0.101530204 -0.868622949
    ## [21]  0.103096904 -0.743848114  1.202974339 -2.426128797  0.534984728
    ## [26]  0.836143973 -0.009687433  2.431190057 -0.271564703 -0.589370929

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

    ##  [1]  1.130633434  1.102704001 -0.338263499  0.698619639 -2.034033627
    ##  [6] -0.545660985  0.126854078  0.002196000 -1.678578869 -0.094923947
    ## [11]  0.350996623  0.208339043  0.693356414 -0.658470228 -0.538549607
    ## [16]  0.496109055  0.991691310 -0.213716113  0.101530204 -0.868622949
    ## [21]  0.103096904 -0.743848114  1.202974339 -2.426128797  0.534984728
    ## [26]  0.836143973 -0.009687433  2.431190057 -0.271564703 -0.589370929

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

## Multiple outputs

``` r
mean_and_sd <- function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) <  3) {
    stop("Input must have at least three numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x, 
    sd = sd_x
  )
  
}
```

Check that the function works.

``` r
x_vec <- rnorm(100, mean = 3, sd = 4)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.18  4.01

## Multiple inputs

I’d like to do this with a function.

``` r
sim_data <-
  tibble(
    x = rnorm(n = 100, mean = 4, sd = 3)
  )

sim_data %>% 
  summarise(
    mean = mean(x), 
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.43  3.18

``` r
sim_mean_sd <- function(samp_size, mu = 3, sigma = 4) {
  sim_data =
  tibble(
    x = rnorm(n = samp_size, mean = mu, sd = sigma)
  )

sim_data %>% 
  summarise(
    mean = mean(x), 
    sd = sd(x)
  )
}

sim_mean_sd(samp_size = 100,mu = 6, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.79  2.96

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.72  3.66
