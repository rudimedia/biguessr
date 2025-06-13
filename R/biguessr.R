#' Automated Gender-Coding for Given Names
#'
#' This function uses a predefined list of given names and genders to infer gender from names. If any given name is not present in the predefined list, the large language model llama3.2 is used to guess the gender. This package is intended for use in the DACH region (Germany, Austria, Switzerland) and is based on historical baby names in those countries.
#'
#' @param name_vector A character vector of given names to be processed.
#' @return A tibble with columns
#'   * `given_name` – the original name in lower case
#'   * `gender`     – a factor with levels 1 (male), and 2 (female)
#'   * `method`     – “KNOWN” if the name was found in the predefined list, “GUESSED” if the coding was done by the LLM
#' @examples
#' \dontrun{
#'   names <- c("Jürgen","Franzi","Rudi","Pepe")
#'   biguessr(names)
#' }
#' @export

biguessr <- function(name_vector) {
  result_list <- list()

  pb <- progress::progress_bar$new(
    total = length(name_vector),
    format = "  Processing [:bar] :percent eta: :eta",
    clear = FALSE, width = 60
  )

  ollamar::pull("llama3.2")

  for (i in seq_along(name_vector)) {
    name <- stringr::str_to_lower(name_vector[i])

    if (name %in% gender_list$given_name) {
      gender <- gender_list$gender[gender_list$given_name == name][1]
      meth <- "KNOWN"
    } else {
      prompt <- paste0(
        "Deine Aufgabe ist es, Vornamen als männlich oder weiblich zu kodieren. ",
        "Gib nur 1 oder 2 aus, keinen weiteren Text. ",
        "1, wenn der Vorname männlich ist und 2, wenn der Vorname weiblich ist. ",
        "Der Vorname ist: '", name, "'"
      )
      resp <- ollamar::generate("llama3.2", prompt)
      gend_raw <- ollamar::resp_process(resp, "text")
      gend_char <- substr(gend_raw, 1, 1)
      gender <- if (gend_char %in% c("1", "2")) as.factor(gend_char) else NA
      meth <- "GUESSED"
    }

    result_list[[length(result_list) + 1]] <- tibble::tibble(given_name = name, gender = gender, method = meth)

    pb$tick()
  }

  result <- dplyr::bind_rows(result_list)
  return(result)
}
