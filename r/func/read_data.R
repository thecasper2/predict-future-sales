# Used to read in, and clean, all data provided
library(magrittr)

## Firstly we read each csv and write the data into a list
data <- list()
for(file_name in list.files("data")){
    data[[tools::file_path_sans_ext(file_name)]] <- data.table::fread(
        paste0("data/", file_name)
    )
}

## Then we perform some cleaning
### Fix date format of training data
data$sales_train[, date := lubridate::dmy(date)]

### We construct net revenue, i.e. the total sales revenue per row
data$sales_train[, net_revenue := item_price * item_cnt_day]
