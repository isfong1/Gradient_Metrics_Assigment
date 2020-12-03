#' @title Conjoint lm
#'
#' @description Return data table that remove respondents provided different answer in the same questions combination
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which lm is called
#' @param formula an object of class formula (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @export
#' @keywords
#' @return dataframe
#' @import dplyr
#' @import tidyr
#' @importFrom stats lm
#' @importFrom relaimpo calc.relimp
#' @importFrom magrittr %>%
#' @examples \dontrun{
#'
#' remove_contradict(experiment_data, rtb, answer, response_id)
#'
#' }
#'


conjoint_rImp<-function(formula, data){

  #Build the lm conjoint model
  model <-lm(formula,data = data)

  #Relative Impact Model
  relImp <- calc.relimp(model, type =c("lmg"), rela = TRUE)

  #output the relative impact result
  relImp@lmg%>%data.frame()

}
