# Here we will store helper functions for making predictions

construct_timeseries <- function(data, target, cutoff_date, date_var="date"){
    # Create timeseries data
    ## Train
    target_data <- data[get(date_var) <= ymd(cutoff_date)][[target]]
    dates <- data[get(date_var) <= ymd(cutoff_date)][[date_var]]
    ts_train <- zoo(
        target_data,
        dates
    )
    ## Test
    test_data <- data[get(date_var) > ymd(cutoff_date)][[target]]
    test_dates <- data[get(date_var) > ymd(cutoff_date)][[date_var]]
    ts_test <- zoo(
        test_data,
        test_dates
    )

    return(list(
        data = data,
        cutoff_date = cutoff_date,
        ts_train = ts_train,
        ts_test = ts_test)
    )
}

attach_bsts_model <- function(
    timeseries, state_specification, niter=500, ping=100, date_var="date",
    attach_predictions = FALSE, horizon=30
){
    # Create model
    bsts_model <- bsts(
        timeseries$ts_train[,1],
        state.specification = state_specification,
        data=timeseries$ts_train,
        niter=niter,
        ping=ping
    )
    p <- predict(bsts_model, horizon=horizon)
    if(attach_predictions){
        timeseries$data[get(date_var) > ymd(timeseries$cutoff_date), `:=` (
            p_mean = p$mean,
            p_lower = p$interval[1,],
            p_upper = p$interval[2,]
        )]
    }
    model_list <- c(timeseries, list(model = bsts_model, predictions = p))
    return(model_list)
}

calculate_error <- function(data, target){
    return(sum(data[!is.na(p_mean)]$p_mean - data[!is.na(p_mean)][[target]]))
}
