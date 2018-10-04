#' Create a dataframe with one row full of missing values.
#'
#' @param .names String giving the names of the dataframe to create.
#'
#' @return A dataframe.
#' @keywords internal
#' @noRd
receiving_df <- function(.names) {
  na <- as.list(rep(NA, length(.names)))
  stats::setNames(data.frame(na), .names)
}
