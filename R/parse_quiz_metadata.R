# parse_quiz_metadata--------------------------------
# Documentation
#' Creates a moodle quiz data dictionary
#' @description Wrangles files directly extracted from moodle to create a formatted data dictionary for the moodle quiz
#' @param question_xml Output from xml2::read_xml() applied to Moodle Quiz XML file. From the quiz settings, select "Question Bank" then "Export" then download "Moodle XML" file.
#' @param question_stats Output from readr::read_csv() applied to Moodle Quiz structure analysis CSV file. From the quiz settings, select "Results" then "Statistics" then download "Quiz structure analysis" table as CSV file.
#' @return Returns a formatted data dictionary for the moodle quiz
#' @import dplyr
#' @import xml2
#' @import tibble
#' @import stringr
#' @import tidyr
#' @importFrom purrr map_chr is_empty map_dfr
#' @export

parse_quiz_metadata <- function(question_xml, question_stats){

  parse_quiz_question <- function(question_stats){
    out <- question_stats %>%
      dplyr::filter(suppressWarnings(is.na(as.numeric(`Quiz name`))==F)) %>%
      dplyr::select(response = `Quiz name`, class = `Course name`, name_short = `Number of complete graded first attempts`) %>%
      dplyr::mutate(class = case_when(class=="Embedded answers (Cloze)" ~ "cloze",
                                      class=="Matching" ~ "matching",
                                      class=="Matrix/Kprime" ~ "matrix",
                                      class=="Multiple choice" ~ "multichoice",
                                      class=="Short answer" ~ "shortanswer",
                                      TRUE ~ class)) %>%
      dplyr::mutate(response = paste0("Response ", response))
    return(out)}

  question <- parse_quiz_question(question_stats = question_stats)

  list <- question_xml %>%  xml2::xml_children()

  table <- list %>%
    purrr::map_chr(function(x){xml2::xml_attr(x,"type")}) %>%
    tibble::enframe(name = "n", value = "class") %>%

    dplyr::mutate(name_short = list %>%
                    purrr::map_chr(function(x){y =  xml2::xml_find_all(x, "name") %>%
                      xml2::xml_find_all("text") %>%
                      xml2::xml_text()

                    if(purrr::is_empty(y)==T){y <- NA_character_}
                    return(y)}),
                  name_long = list %>%
                    purrr::map_chr(function(x){y = xml2::xml_find_all(x, "questiontext") %>%
                      xml2::xml_find_all("text")%>%
                      xml2::xml_text()

                    if(purrr::is_empty(y)==T){y <- NA_character_}
                    return(y)})) %>%

    dplyr::mutate(across(c(name_short, name_long),
                         function(x){x %>% stringr::str_remove_all("<.*?>") %>%
                             stringr::str_replace_all("&nbsp;", " ") %>% stringr::str_trim()})) %>%

    dplyr::mutate(level_matching = list %>%
                    purrr::map_chr(function(x){y = x %>%
                      xml2::xml_find_all("subquestion") %>% xml2::xml_find_all("text") %>%
                      xml2::xml_text()

                    if(purrr::is_empty(y)==T){y <- NA_character_}
                    return(paste0(y, collapse = "; "))}),

                  level_multichoice = list %>%
                    purrr::map_chr(function(x){y = x %>%
                      xml2::xml_find_all("answer") %>%
                      xml2::xml_find_all("text") %>%
                      xml2::xml_text()

                    if(purrr::is_empty(y)==T){y <- NA_character_}
                    return(paste0(y, collapse = "; "))})) %>%

    dplyr::mutate(across(c(level_matching, level_multichoice),
                         function(x){ x %>%
                             stringr::str_remove_all("<.*?>|\n|\\]\\]>") %>%
                             stringr::str_replace_all("&nbsp;", " ") %>%
                             stringr::str_replace_all( "NA|\\*", NA_character_)})) %>%

    dplyr::mutate(n = as.character(n)) %>%
    dplyr::filter(! class %in% c("category", "description"))


  table <- table %>%
    dplyr::left_join(question, by = c("class", "name_short")) %>%
    dplyr::filter(is.na(response)==F)



  matrix <- list %>%
    purrr::map_dfr(.id = "n", function(x){y = x %>%
      xml2::xml_find_all("row") %>%
      xml2::xml_find_all("shorttext")%>%
      xml2::xml_text() %>%
      tibble::enframe(name = NULL, value = "name_long_matrix") %>%
      dplyr::mutate(name_long_matrix = stringr::str_trim(name_long_matrix),
                    level_matrix =  x %>%
                      xml2::xml_find_all("col") %>%
                      xml2::xml_find_all("shorttext")%>%
                      xml2::xml_text() %>% stringr::str_trim() %>%
                      paste0(collapse = "; "))})

  cloze <- table %>%
    dplyr::select(-level_multichoice, -level_matching) %>%
    dplyr::filter(class=="cloze") %>%

    # remove periods (.) not at end of sentence
    dplyr::mutate(name_long = stringr::str_replace_all(name_long, paste0(c("e\\.g\\.", "E\\.g\\."), collapse = "|"), "E_g_"),
                  name_long = stringr::str_replace_all(name_long, "\\}Please", "\\}. Please"),
                  name_long = stringr::str_replace_all(name_long,
                                                       paste0(paste0("\\}", c("a", "b", "c", "d", "e", "f"), "\\)\\."), collapse = "|"),
                                                       "\\}\\."),
                  name_long = stringr::str_replace_all(name_long,
                                                       paste0(paste0(":", c("a", "b", "c", "d", "e", "f"), "\\)\\."), collapse = "|"),
                                                       "\\:\\.")) %>%
    tidyr::separate_rows(name_long, sep = "\\.") %>%

    dplyr::filter(stringr::str_detect(name_long, "\\}")) %>%
    dplyr::mutate(name_long = stringr::str_trim(name_long)) %>%

    dplyr::mutate(value = stringr::str_extract_all(name_long, "\\{(.*?)\\}"),
                  name_long = stringr::str_replace_all(name_long, "\\{(.*?)\\}", "___"),
                  value = stringr::str_remove_all(value, "\\{1:MULTICHOICE:=|\\}|~"),
                  name_long = stringr::str_trim(name_long)) %>%
    tidyr::separate_rows(value, sep = "=") %>%
    dplyr::filter(value!="")

  cloze_sum <- cloze %>%
    dplyr::distinct(name_short,name_long) %>%
    dplyr::group_by(name_short) %>%
    dplyr::summarise(name_long = unique(name_long),
                     nq = 1:n(),
                     .groups= "drop")

  cloze <- cloze %>%
    dplyr::left_join(cloze_sum, by = c("name_short", "name_long")) %>%
    dplyr::group_by(name_short,name_long, nq) %>%
    dplyr::summarise(level_cloze = paste0(value, collapse = "; "), .groups = "drop") %>%
    dplyr::rename("name_long_cloze" = name_long) %>%
    dplyr::arrange(name_short,nq) %>%
    dplyr::select(-nq)

  final <- table %>%
    dplyr::left_join(matrix, by = "n") %>%
    dplyr::left_join(cloze, by = "name_short") %>%
    dplyr::mutate(name_long = case_when(class=="matrix" ~ name_long_matrix,
                                        class=="cloze" ~ name_long_cloze,
                                        TRUE ~ name_long)) %>%
    dplyr::mutate(name_long_3char = stringr::str_sub(name_long, nchar(name_long)-2, nchar(name_long)),
                  name_long = ifelse(name_long_3char=="___",#
                                     stringr::str_sub(name_long, 1, nchar(name_long)-3),
                                     name_long)) %>%
    dplyr::select(-name_long_matrix, -name_long_cloze, - name_long_3char,-n) %>%
    tidyr::unite(col = "level", starts_with("level_"), sep = "&&&", na.rm = T) %>%
    dplyr::mutate(level = ifelse(level == "", NA, level),
                  level = stringr::str_to_title(level) %>% iconv(to="ASCII//TRANSLIT") %>%
                    stringr::str_squish() %>% stringr::str_trim(),
                  level =  stringr::str_replace_all(level, "E_g_","E.g."),
                  level = stringr::str_split(level, pattern = "; "))  %>%
    dplyr::group_by(name_short) %>%
    dplyr::mutate(n = 1:n(),
                  total = n(),
                  n = ifelse(n==1&total==1, "", as.character(n)),
                  name_short =ifelse(n=="", name_short, paste0(name_short, "___", n)),
                  response = ifelse(n=="", response, paste0(response, "-", n)),
                  response_n = stringr::str_extract(response, "([0-9]+)") %>% as.numeric()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(response_n,n) %>%
    dplyr::mutate(response = factor(response, levels = unique(response))) %>%
    dplyr::select(-n, -total, -response_n)

  return(final)}
