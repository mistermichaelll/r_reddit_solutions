# libraries used
library(purrr)
library(dplyr)
library(ggplot2)

set.seed(123)

# reproducible example
# --------------------
# create the dataframe
model_year <- sample(1991:1997, 10000, replace = T)
price <- runif(10000) * 10000
df <- data.frame(model_year, model_price = price)

# define the model years to run through
unique_model_years <- sort(unique(df$model_year))

# for-loop version
for (year in unique_model_years){
    hist(data = df[model_year == year, ], 
         x = df[model_year == year, ]$model_price, 
         main = paste("Price Dist. for Model Year", year, sep = " "), 
         xlab = "Price"
         )
}

# lapply version
# (preferable to for-loop)
lapply(unique_model_years, 
       function(year) hist(data = df[model_year == year, ], 
                 x = df[model_year == year, ]$model_price, 
                 main = paste("Price Dist. for Model Year", year, sep = " "), 
                 xlab = "Price", 
                 col = "lightgrey"
                 )
       )

# purrr version!
# --------------

# create a function that you want to map over multiple elements - in this case, 
#  we want to create multiple histograms for multiple years.
hist_summary <- function(data, year_col, year){
    ## filter to a given year and then return the plot
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
