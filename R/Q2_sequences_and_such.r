## ======================================================
## Q2 Answer
## ======================================================
col_1 <- c(27.3, 27.4, 27.5, 27.6, 27.7, 27.8)
col_2 <- c(0.485, 0.457, 0.430, 0.402, 0.374, 0.347)
col_3 <- c("{\"type\":\"M\",\"msg\":\"VALUE1\"}", 
           NA, 
           NA, 
           "{\"type\":\"M\",\"msg\":\"VALUE2\"}", NA, NA)

df <- data.frame(col_1, col_2, col_3)
df2 <- df

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
        ), ] # note that if I didn't do this "," I got an undefined columns error. 
    
    return(filtered_data)
}

filtered_dfs <- lapply(list_of_dfs, my_filtering_function, "col_3")
filtered_dfs[1]

## ======================================================
## Q2 Bonus Question Answer
## ======================================================
library(dplyr)
library(purrr)

df_real_ex <-
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

df_real_ex_2 <- df_real_ex

df_real_ex %>%
    mutate(points = case_when(off > 0 ~ 1, 
                              TRUE ~ -1), 
           sequence_number = cumsum(grepl("VALUE1", Z))) %>%
    # calculate the total points per sequence
    group_by(sequence_number) %>%
    summarize(total_points = sum(points))

point_calc <- function(data){
    data %>%
        mutate(points = case_when(off > 0 ~ 1, 
                                  TRUE ~ -1), 
               sequence_number = cumsum(grepl("VALUE1", Z))) %>%
        # calculate the total points per sequence
        group_by(sequence_number) %>%
        summarize(total_points = sum(points))
}

list_of_dfs <- list("dataframe_1" = df_real_ex, 
                    "dataframe_2" = df_real_ex_2 <- df_real_ex)

list_of_dfs %>%
    imap_dfr(~ point_calc(.x) %>%
                 mutate(name_of_df = .y)
    ) %>%
    relocate(name_of_df)

list_of_dfs
