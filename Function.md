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

    ##  [1] -1.08867175 -0.33716495  0.50141257  1.75851470 -0.63311468 -1.38219434
    ##  [7] -0.16947186  0.23161758  0.53863861  2.20749968  0.99266541  0.90333076
    ## [13] -1.18087622 -0.82478094 -0.41966341 -0.40521985 -0.03747147  1.73729034
    ## [19] -0.53219317  1.32908620  1.23543234 -1.48439777 -1.32472521  0.64032184
    ## [25] -0.12818887 -0.90312326 -0.38330399  0.23311671 -0.46771172 -0.60665326

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

    ##  [1] -1.08867175 -0.33716495  0.50141257  1.75851470 -0.63311468 -1.38219434
    ##  [7] -0.16947186  0.23161758  0.53863861  2.20749968  0.99266541  0.90333076
    ## [13] -1.18087622 -0.82478094 -0.41966341 -0.40521985 -0.03747147  1.73729034
    ## [19] -0.53219317  1.32908620  1.23543234 -1.48439777 -1.32472521  0.64032184
    ## [25] -0.12818887 -0.90312326 -0.38330399  0.23311671 -0.46771172 -0.60665326

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
    ## 1  3.15  4.13

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
    ## 1  3.13  3.25

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
    ## 1  5.93  2.94

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.61  3.96

## Let’s review Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

## What about the next page of review…?

Ler’s turn that code into a function.

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

read_page_reviews = function(url) {
  html = read_html(url)
  
  review_titles = 
  html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)

reviews

}
```

Let me try my function.

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

read_page_reviews(dynamite_url)
```

    ## # A tibble: 10 x 3
    ##    title                  stars text                                            
    ##    <chr>                  <dbl> <chr>                                           
    ##  1 Vote for Pedro!            5 Just watch the movie. Gosh!                     
    ##  2 Just watch the freaki…     5 Its a great movie, gosh!!                       
    ##  3 Great Value                5 Great Value                                     
    ##  4 I LOVE THIS MOVIE          5 THIS MOVIE IS SO FUNNY ONE OF MY FAVORITES      
    ##  5 Don't you wish you co…     5 Watch it 100 times. Never. Gets. Old.           
    ##  6 Stupid, but very funn…     5 If you like stupidly funny '90s teenage movies …
    ##  7 The beat                   5 The best                                        
    ##  8 Hilarious                  5 Super funny! Loved the online rental.           
    ##  9 Love this movie            5 We love this product.  It came in a timely mann…
    ## 10 Entertaining, limited…     4 Entertainment level gets a 5 star but having pr…

Let’s read a few pages of reviews.

``` r
dynamite_url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:5)

all_reviews = bind_rows(
  read_page_reviews(dynamite_urls[1]),
  read_page_reviews(dynamite_urls[2]),
  read_page_reviews(dynamite_urls[3]),
  read_page_reviews(dynamite_urls[4]),
  read_page_reviews(dynamite_urls[5])
)
```
