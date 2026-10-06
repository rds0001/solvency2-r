#' Read bundled documentation without a browser
#' @param topic Case-sensitive topic: `README`, `API`, `DATA`, `PARAMETERS`,
#'   `RECIPES`, `LIMITATIONS`, `DEVELOPMENT`, `ACCEPTANCE`, `DISCLAIMER`,
#'   `SECURITY`, `THIRD_PARTY_NOTICES` or `CHANGELOG`.
#' @return UTF-8 Markdown text from the installed R package.
#' @examples
#' cat(documentation("DATA"))
#' @export
documentation <- function(topic = "README") {
  .s2_require(.s2_string(topic) && topic %in% c("README", "API", "DATA", "PARAMETERS", "RECIPES",
    "LIMITATIONS", "DEVELOPMENT", "ACCEPTANCE", "DISCLAIMER", "SECURITY", "THIRD_PARTY_NOTICES", "CHANGELOG"),
    "topic", "unknown documentation topic")
  path <- system.file("documentation", paste0(topic, ".md"), package = "solvency2", mustWork = TRUE)
  paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}
