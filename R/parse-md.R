
#' Extract code blocks from a Markdown file that have a label that contains a specific string.
#'
#' @param md_file The markdown file to extract from.
#' @param string_match The string to look for that is inside the label. For
#'   instance, if the label has word "function" inside the label.
#'
#' @return
#' @export
#'
extract_code_block_with_label_string <- function(md_file, string_match) {
  md_as_xml <- md_file |>
    readr::read_lines() |>
    commonmark::markdown_xml(extensions = TRUE) |>
    xml2::read_xml()

  # d1 is the "namespace" of the xml spec, so need this
  # to access the nodes that are called code_block,
  # and within those code blocks, only keep those
  # where the attribute "info" (@ means attribute) has
  # the `pattern` in it.
  label_pattern <- glue::glue(".//d1:code_block[contains(@info, '{string_match}')]")
  md_as_xml |>
    xml2::xml_find_all(
      label_pattern,
      # Need this to force to use `d1` as namespace.
      xml2::xml_ns(md_as_xml)
    ) |>
    xml2::xml_text()
}
