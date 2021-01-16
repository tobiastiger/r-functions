remove_asis <- function(object) {
  # Removes type "AsIs" from dataframe-like object, leaves other types intact.
  # Commonly used for lm() objects with transformed variables.
  for (X in names(object)) {
    if("AsIs" %in% class(object[[X]])) {
      class(object[[X]]) <- class(object[[X]])[-match("AsIs", class(object[[X]]))]
    }
  }
  object
}