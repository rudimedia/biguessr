#' Add Gender-Coding to a Data Frame
#'
#' This function adds gender-coding to a data frame or tibble based on a given name column.
#' It uses a predefined name-gender list, and if the name is not found, it uses llama3.2 to guess.
#'
#' @param df A data frame or tibble containing the names to be processed.
#' @param name_col The unquoted name of the column containing given names.
#' @return The original data frame with two new columns:
#'   * `gender` - a factor with levels 1 (male) and 2 (female)
#'   * `method` - "KNOWN" if from predefined list, "GUESSED" if predicted by LLM
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#'   df <- tibble::tibble(id = 1:4, name = c("J\u00fcrgen", "Franzi", "Rudi", "Pepe"))
#'   biguessdf(df, name)
#' }
#' @export

biguessdf <- function(df, name_col) {
  name_col_enquo <- rlang::enquo(name_col)
  name_vector <- df %>% dplyr::pull(!!name_col_enquo)

  gender_vec <- vector("list", length(name_vector))
  method_vec <- character(length(name_vector))

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
      method <- "KNOWN"
    } else {
      prompt <- paste0(
        "Deine Aufgabe ist es, Vornamen als m\u00e4nnlich oder weiblich zu kodieren. ",
        "Gib nur 1 oder 2 aus, keinen weiteren Text. ",
        "1, wenn der Vorname m\u00e4nnlich ist und 2, wenn der Vorname weiblich ist. ",
        "Der Vorname ist: '", name, "'"
      )
      resp <- ollamar::generate("llama3.2", prompt)
      gend_raw <- ollamar::resp_process(resp, "text")
      gend_char <- substr(gend_raw, 1, 1)
      gender <- if (gend_char %in% c("1", "2")) as.factor(gend_char) else NA
      method <- "GUESSED"
    }

    gender_vec[[i]] <- gender
    method_vec[i] <- method

    pb$tick()
  }

  df$gender <- unlist(gender_vec)
  df$method <- method_vec
  return(df)
}
