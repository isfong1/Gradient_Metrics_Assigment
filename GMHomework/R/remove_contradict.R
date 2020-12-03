#' @title Remove contradict ID
#'
#' @description Return data table that remove respondents provided different answer in the same questions combination
#' @param data a data frame
#' @param ... variables or computations to group by.
#' @param score variable which is used for score
#' @param id key variable of the data frame
#' @export
#' @keywords
#' @return dataframe
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#' @examples \dontrun{
#'
#' remove_contradict(experiment_data, rtb, answer, response_id)
#'
#' }
#'

remove_contradict<-function(data, ..., score, id){

  case<-NULL

  #Check if there is any Duplicated data

  duplicate_df = data%>%
    group_by(...)%>%
    dplyr::summarise(count = n())%>%
    filter(count > 1)

  #Merge the asnwer back to duplicated data

  duplicate_df2<-duplicate_df%>%
    ungroup()%>%
    mutate(case = row_number())%>%
    left_join(data, c(toString(id, ...)))

  #Figure out cases that with contradict answer given same combination

  contradict_case = duplicate_df2%>%
    select(case, score)%>%
    unique()%>%
    group_by(case)%>%
    dplyr::summarise(count = n())%>%
    filter(count > 1)

  if(nrow(contradict_case) == 0 ){
    print("There is no contradict data")
  }else{
    #Select those ID which have contradict answer as they are not reliable
    id_to_remove = duplicate_df2%>%
      filter(case %in% contradict_case$case)%>%
      select(id)%>%
      unique()

    #Remove contradict respondent
    data%>%
      filter(! id %in% id_to_remove$id)

  }

}
