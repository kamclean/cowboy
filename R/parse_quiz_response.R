# parse_quiz_response--------------------------------
# Documentation
#' Formats a moodle quiz dataset
#' @description Wrangles files directly extracted from moodle to create a clean dataset for the moodle quiz.
#' @param response Output from readr::read_csv() applied to Moodle Quiz responses CSV file. From the quiz settings, select "Results" then "Responses" then download table as CSV file.
#' @param question_change Default = NULL. Not recommended, but can be used if a question is added after a quiz goes live. Must supply a tibble containing 3 columns: "response" (what question in the dataset that has been added), "change" (the change that has been made - only "new" questions accounted for at present), and "date" (the date the change was made).
#' @return Returns a clean dataset for the moodle quiz
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @importFrom lubridate as_date parse_date_time
#' @export


parse_quiz_response <- function(response, question_change = NULL){
  out <- response %>%
    # Finished only
    dplyr::filter(State == "Finished") %>%

    dplyr::group_by(Username) %>%
    # Most complete survey (e.g. max grade)
    dplyr::mutate(across(.names = "grade", contains("Grade"), function(x){as.numeric(x)})) %>%
    dplyr::filter(grade==max(grade)) %>%

    # Latest survey
    dplyr::mutate(Completed = lubridate::parse_date_time(Completed, "%d %B %Y  %I:%M %p")) %>%
    dplyr::arrange(desc(Completed)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%

    dplyr::select("user_name" = Username, contains("Response"), Completed) %>%
    tidyr::pivot_longer(cols = contains("Response")) %>%
    tidyr::separate_rows(value, sep = "; ") %>%

    dplyr::group_by(user_name, name) %>%
    dplyr::mutate(n = 1:n(),
                  total = n(),
                  n = ifelse(total==1&n==1, "", as.character(n))) %>%
    dplyr::mutate(response = ifelse(n=="", name, paste0(name, "-", n))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value1 = stringr::str_split_fixed(value, ": ", 2)[,1],
                  value2 = stringr::str_split_fixed(value, ": ", 2)[,2]) %>%
    dplyr::mutate(value = ifelse(value2=="", value1, value2)) %>%
    dplyr::select(user_name, Completed, response, value) %>%
    dplyr::mutate(value = stringr::str_to_title(value) %>% iconv(to="ASCII//TRANSLIT") %>%
                    stringr::str_squish() %>% stringr::str_trim()) %>%
    dplyr::filter(value!="")

  if(is.null(question_change)==F){
    question_change <- question_change %>%
      dplyr::mutate(date = lubridate::as_date(date),
                    response_q = stringr::str_remove_all(response, "-([0-9]+)"),
                    response_n_change = stringr::str_extract(response, "-([0-9]+)") %>% stringr::str_remove_all("-")) %>%
      dplyr::select(-response)

    out <- out %>%
      dplyr::mutate(response_q = stringr::str_remove_all(response, "-([0-9]+)"),
                    response_n = stringr::str_extract(response, "-([0-9]+)") %>% stringr::str_remove_all("-") %>% as.numeric()) %>%
      dplyr::left_join(question_change %>% filter(change=="new"), by ="response_q") %>%
      dplyr::mutate(response_n = case_when((response_n>=response_n_change) & (Completed <= date) ~ response_n +1,
                                           TRUE~ response_n)) %>%
      dplyr::mutate(response_change = ifelse(is.na(change)==F, paste0(response_q, "-", response_n), NA),
                    response = ifelse(is.na(response_change)==F, response_change, response))}

  return(out)}
