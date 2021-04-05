# parse_quiz--------------------------------
# Documentation
#' Creates a fully formatted moodle quiz dataset
#' @description Wrangles files directly extracted from moodle to create a fully formatted dataset for a moodle quiz
#' @param question_xml Output from xml2::read_xml() applied to Moodle Quiz XML file. From the quiz settings, select "Question Bank" then "Export" then download "Moodle XML" file.
#' @param question_stats Output from readr::read_csv() applied to Moodle Quiz structure analysis CSV file. From the quiz settings, select "Results" then "Statistics" then download "Quiz structure analysis" table as CSV file.
#' @param response Output from readr::read_csv() applied to Moodle Quiz responses CSV file. From the quiz settings, select "Results" then "Responses" then download table as CSV file.
#' @param question_change Default = NULL. Not recommended, but can be used if a question is added after a quiz goes live. Must supply a tibble containing 3 columns: "response" (what question in the dataset that has been added), "change" (the change that has been made - only "new" questions accounted for at present), and "date" (the date the change was made).
#' @return Returns a fully formatted dataset for the moodle quiz
#' @import dplyr
#' @import xml2
#' @import tibble
#' @import stringr
#' @import tidyr
#' @importFrom purrr map_chr is_empty map_dfr
#' @export

parse_quiz <- function(question_stats, question_xml, response, question_change = NULL){


  # xml_object = Dashboard Courses Academia Career Overview Pre-Module Survey Question bank Export Moodle XML format
  meta <- cowboy::parse_quiz_meta(question_xml = question_xml, question_stats = question_stats)

  var_free_text <- meta %>% filter(class=="shortanswer") %>% pull(response) %>% as.character(response)

  # Dashboard Courses Academia Career Overview Pre-Module Survey Results Responses
  data <- cowboy::parse_quiz_response(data= response, question_change = question_change) %>%
    dplyr::mutate(response_q = stringr::str_remove_all(response, "-([0-9]+)")) %>%
    dplyr::mutate(response = case_when(response_q %in% var_free_text ~ stringr::str_remove_all(response, "-([0-9]+)"),
                                       TRUE ~ response)) %>%
    dplyr::select(-response_q) %>%
    dplyr::group_by(across(-"value")) %>%
    dplyr::summarise(value = paste0(value, collapse = ", "), .groups = "drop") %>%
    dplyr::ungroup()

  # Deal with ranking questions
  if("matching" %in% meta$class){
    var_matching <- meta %>%
      dplyr::filter(class=="matching")

    q_matching <- data %>%
      dplyr::filter(stringr::str_detect(response, paste0(var_matching$response, collapse = "|"))==T) %>%
      dplyr::mutate(response = stringr::str_split_fixed(response, "-", 2)[,1]) %>%
      mutate(item = stringr::str_split_fixed(value, "->", 2)[,1],
             rank = stringr::str_split_fixed(value, "->", 2)[,2]) %>%
      dplyr::mutate(across(c(item, rank), function(x){stringr::str_trim(x)}),
                    rank = as.numeric(rank)) %>%
      dplyr::group_by(user_name, Completed, response, rank) %>%
      dplyr::summarise(item = paste0(item, collapse = "; "), .groups = "drop") %>%
      dplyr::arrange(user_name, response, rank) %>%
      dplyr::group_by(user_name, Completed, response) %>%
      dplyr::mutate(rank = 1:n(),
                    value = paste0(rank, " (", item, ")")) %>%
      dplyr::summarise(value = paste0(value, collapse = "~"),.groups = "drop") %>%
      dplyr::mutate(value = ifelse(value=="1 (-)", NA, value))

    data <- data %>%
      dplyr::filter(stringr::str_detect(response, paste0(var_matching$response, collapse = "|"))==F) %>%
      dplyr::bind_rows(q_matching)}

  if("multichoice" %in% meta$class){
    var_multichoice <- meta %>%
      dplyr::filter(class=="multichoice")

    data <- data %>%
      dplyr::mutate(response = case_when(stringr::str_detect(response,
                                                             paste0(var_multichoice$response, collapse = "|")) ~ stringr::str_remove_all(response, "-[0-9]+"),
                                         TRUE ~ response)) %>%
      dplyr::group_by(user_name, Completed, response) %>%
      dplyr::summarise(value = paste0(value, collapse = "; "), .groups = "drop")}

  data <- data %>%
    dplyr::left_join(meta %>% dplyr::select(name_short, response), by = "response") %>%
    dplyr::mutate(response = factor(response, levels = meta$response)) %>%
    dplyr::arrange(user_name, response) %>%
    tidyr::pivot_wider(id_cols = c("user_name", "Completed"), names_from = "name_short", values_from = "value")

  meta_factor <- meta %>%
    dplyr::filter(name_short %in% names(data)) %>%
    dplyr::filter(! class %in% c("shortanswer", "matching", "multichoice"))

  for(i in 1:nrow(meta_factor)){
    data <- data %>%
      dplyr::mutate(across(c(meta_factor$name_short[i]),
                           function(x){factor(x, levels = unlist(meta_factor$level[i])) %>%
                               finalfit::ff_label(meta_factor$name_long[i])}))}

  data <- data %>%
    dplyr::select(user_name, "completed" = Completed, all_of(meta$name_short))

  return(data)}
