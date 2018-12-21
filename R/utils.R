# TODO: This functions is certanily not what we want. It creates dataframes
# that will make for loops very slow.

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

# TODO: Move to its own file and test.

#' Path to directory containing example data.
#'
#' @param path Path to a file (with extension) from inst/extdata/.
#'
#' @return Path to directory containing example data.
#'
#' @export
#'
#' @examples
#' agb_example("data")
path2data <- function(path) {
  system.file("extdata", path, package = "AGBfluxes")
}
