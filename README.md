# r_reddit_solutions
Contains a list of my solutions to questions asked on the r/rstats subreddit.

# Q1: "How do I create multiple histograms at the same time?"

OP asked how to multiple histograms from a single dataframe. I think this one is pretty straightforward, so I won't run through it in the readme. But long story short...the easiest way in base R looks like this:

```r
set.seed(123)

model_year <- sample(1991:1997, 10000, replace = T)
price <- runif(10000) * 10000
df <- data.frame(model_year, model_price = price)

unique_model_years <- sort(unique(df$model_year))

lapply(unique_model_years, 
       function(year) hist(data = df[model_year == year, ], 
                 x = df[model_year == year, ]$model_price, 
                 main = paste("Price Dist. for Model Year", year, sep = " "), 
                 xlab = "Price", 
                 col = "lightgrey"
                 )
       )
```

And then a tidy solution would look like this:

```r
library(purrr)
library(dplyr)
library(ggplot2)

hist_summary <- function(data, year_col, year){
    plot <- 
        data %>%
        filter(!!sym(year_col) == year) %>% # filter to the year
        ggplot() + # use ggplot2 to make the plot
        aes(x = model_price) + 
        geom_histogram(color = "black", fill = "white") +
        labs(title = paste("Price Dist. for Model Year", year, sep = " "))
    
    return(plot)
}

model_plots <- 
    unique_model_years %>%
    map(~hist_summary(df, "model_year", .x)) 

model_plots %>%
    walk(print) 
```

# Q2: Given a dataframe like the following, how do we filter out rows not between VALUE1 and VALUE2?
The user provided a sample dataframe which looked like this:

```
# A tibble: 6 × 4
      t   off  Z                                     id
  <dbl> <dbl> <chr> <int>
1  27.3 0.485 "{\"type\":\"M\",\"msg\":\"VALUE1\"}   1
2  27.4 0.457  NA                                    1                                      
3  27.5 0.430  NA                                    1                                     
4  27.6 0.402  "{\"type\":\"M\",\"msg\":\"VALUE2\"}  1                                     
5  27.7 0.374  NA                                    1                                     
6  27.8 0.347  NA                                    1  
```

And some R code that looked like this:

```r
tbl[sort(unique(unlist(Map(`:`, grep("VALUE1", tbl$Z), grep("VALUE2", tbl$Z))))),] 
```

To apply this to multiple dataframes, the easiest thing to do is to make it a function! 

```r
 my_filtering_function <- function(data, column_to_filter_on){
     filtered_data <- 
         data[sort(
          unique(
             unlist(
                 Map(
                     `:`, grep("VALUE1", data[[column_to_filter_on]]), 
                     grep("VALUE2", data[[column_to_filter_on]])
                 )
             )
         )
         ), ]

     return(filtered_data)
 }
```

Then we can take that function and apply it to multiple dataframes using `lapply`. 

```r
 col_1 <- c(27.3, 27.4, 27.5, 27.6, 27.7, 27.8)
 col_2 <- c(0.485, 0.457, 0.430, 0.402, 0.374, 0.347)
 col_3 <- c("{\"type\":\"M\",\"msg\":\"VALUE1\"}", 
            NA, 
            NA, 
            "{\"type\":\"M\",\"msg\":\"VALUE2\"}", NA, NA)

  df <- data.frame(col_1, col_2, col_3)
  
 filtered_dfs <- lapply(list_of_dfs, my_filtering_function, "col_3")
 filtered_dfs[1]

#> [[1]]
#>   col_1 col_2                       col_3
#> 1  27.3 0.485 {"type":"M","msg":"VALUE1"}
#> 2  27.4 0.457                        <NA>
#> 3  27.5 0.430                        <NA>
#> 4  27.6 0.402 {"type":"M","msg":"VALUE2"}
```

We could also use `purrr`, depending on how fancy we feel. 

```r
 list_of_dfs %>%
     map(~ my_filtering_function(.x, col_3)
         )
```

## BONUS QUESTION: now that we have this, how can we assign points based on whether an "off" value is positive or negative and then sum these points by the sequence number?

This was a tricky one! I ran into a similar problem in college that I never managed to solve because I fixated too much on how to fill in the `NA` values between the rows.

Let's start with what we know, and a reproducible example

```r
 library(dplyr)
 
 df <-
     tribble(
     ~time,  ~off,     ~Z,
     3,      -0.88,      "randomnumbersVALUE1randomtext",
     4,       0.92,      NA,
     5,       0.90,      NA,
     6,      -0.87,      "randomnumbersVALUE2randomtext",
     10,     -0.87,      "srebmunmodnarVALUE1txetmodnar",
     11,      0.86,      NA,
     12,     -0.84,      "srebmunmodnarVALUE2txetmodnar"
     )
```

We know that if `Z` has an entry containing `VALUE1`, that it starts a sequence. If the column contains `VALUE2`, then that is the end of the sequence. We need a way to identify which rows belong to a sequence. 

My solution here is to use a cumulative sum on the same pattern matching expression we used in OP's original code. `cumsum(grepl("VALUE1", Z))` will return a cumulative sum based on the results of `grepl`. If `VALUE1` is found, we add 1 to the cumulative sum. Otherwise, we add 0. This will be our sequence number, since it will only change when it finds another starting value.

Getting the assigned points is easy enough as well. Here is a tidy solution for a single dataframe: 

```r
df %>%
    mutate(points = case_when(off > 0 ~ 1, 
                              TRUE ~ -1), 
           sequence_number = cumsum(grepl("VALUE1", Z))) %>%
    group_by(sequence_number) %>%
    summarize(total_points = sum(points))
    
#>     # A tibble: 2 × 2
#>   sequence_number total_points
#>             <int>        <dbl>
#> 1               1            0
#> 2               2           -1
```

This was the result OP expected. Applying this to multiple dataframes is as simple as functionalizing our solution and applying it with `purrr`:

```r
point_calc <- function(data){
    data %>%
        mutate(points = case_when(off > 0 ~ 1, 
                                  TRUE ~ -1), 
               sequence_number = cumsum(grepl("VALUE1", Z))) %>%
        group_by(sequence_number) %>%
        summarize(total_points = sum(points))
}

list_of_dfs <- list("dataframe_1" = df, 
                    "dataframe_2" = df2)

list_of_dfs %>%
    imap_dfr(~ point_calc(.x) %>%
                 mutate(name_of_df = .y)
    ) %>%
    relocate(name_of_df)
    
#> # A tibble: 4 × 3
#>   name_of_df  sequence_number total_points
#>   <chr>                 <int>        <dbl>
#> 1 dataframe_1               1            0
#> 2 dataframe_1               2           -1
#> 3 dataframe_2               1            0
#> 4 dataframe_2               2           -1    
```
