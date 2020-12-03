#' @title Calculate the distribution of score by variables
#'
#' @description Return data table showing distribution of score by variables
#' @param data a data frame
#' @param ... variables or computations to group by.
#' @param score variable which is used for distribution
#' @export
#' @keywords
#' @return dataframe
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#' @examples \dontrun{
#'
#' score_distribution(experiment_data, rtb, answer)
#'
#' }
#'

score_distribution<-function(data, ..., score){

  percent_count<-NULL

  data%>%
    group_by(..., score)%>%
    dplyr::summarise(count = n())%>%
    group_by(...)%>%
    mutate(percent_count = count / sum(count))%>%
    dplyr::select(-count)%>%
    spread(score, percent_count)
}
