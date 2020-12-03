#' @title Calculate number of count of selected dataset of the variable
#'
#' @description Return data table showing number of count by the variable. You can only input 1 variable of the original dataset
#' @param data a data frame
#' @param ... variables or computations to group by.
#' @export
#' @keywords
#' @return dataframe
#' @import dplyr
#' @importFrom magrittr %>%
#' @examples \dontrun{
#'
#' Variable_distribution(data = experiment_data, price, offer)
#'
#' }
#'

variable_distribution<-function(data, ...){

  data%>%
    group_by(...) %>%
    summarise(count = n())
}
