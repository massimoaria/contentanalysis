#' Normalize Unicode ligatures to ASCII equivalents
#'
#' @param x Character string to normalize
#'
#' @return Character string with ligatures replaced
#'
#' @keywords internal
#' @noRd
normalize_ligatures <- function(x) {
  x <- gsub("\ufb04", "ffl", x)
  x <- gsub("\ufb03", "ffi", x)
  x <- gsub("\ufb02", "fl", x)
  x <- gsub("\ufb01", "fi", x)
  x <- gsub("\ufb00", "ff", x)
  x
}

#' Extract titles from PDF table of contents
#'
#' @param toc_node List object from pdftools::pdf_toc()
#'
#' @return Character vector of section titles
#'
#' @keywords internal
#' @noRd
extract_all_titles <- function(toc_node) {
  titles <- character(0)

  if (!is.null(toc_node$title) && nchar(toc_node$title) > 0) {
    titles <- c(titles, toc_node$title)
  }

  if (!is.null(toc_node$children) && length(toc_node$children) > 0) {
    for (child in toc_node$children) {
      titles <- c(titles, extract_all_titles(child))
    }
  }

  return(titles)
}

#' Split document text into sections
#'
#' @description
#' Splits extracted text into logical sections (Introduction, Methods, Results, etc.)
#' using either the PDF's table of contents or common academic section patterns.
#'
#' @param text Character string. Full text of the document.
#' @param file_path Character string or NULL. Path to PDF file for TOC extraction.
#'   If NULL, uses common section names. Default is NULL.
#'
#' @return Named list where each element is a section's text. Always includes
#'   "Full_text" element with complete document.
#'
#' @details
#' The function attempts to:
#' \enumerate{
#'   \item Extract section names from PDF table of contents
#'   \item Fall back to common academic section names if TOC unavailable
#'   \item Match section headers in text using regex patterns
#'   \item Handle duplicate section names
#' }
#'
#' Common sections searched: Abstract, Introduction, Methods, Results,
#' Discussion, Conclusion, References, etc.
#'
#' @examples
#' \dontrun{
#' text <- pdf2txt_auto("paper.pdf", sections = FALSE)
#' sections <- split_into_sections(text, file_path = "paper.pdf")
#' names(sections)
#' }
#'
#' @export
#' @importFrom pdftools pdf_toc
split_into_sections <- function(text, file_path = NULL) {

  # Normalize ligatures in text before any matching
  text <- normalize_ligatures(text)

  common_sections <- c(
    "Abstract", "Introduction", "Related work", "Related Work",
    "Background", "Literature review", "Literature Review",
    "Methodology", "Methods", "Materials and methods", "Materials and Methods",
    "Experimental design", "Experimental Design", "Analysis",
    "Results", "Results and discussion", "Results and Discussion",
    "Discussion", "Comparison study", "Comparison Study",
    "Conclusion", "Conclusions",
    "Acknowledgment", "Acknowledgments", "Acknowledgement", "Acknowledgements",
    "References", "Bibliography", "Appendix"
  )

  section_names <- NULL
  if (!is.null(file_path) && file.exists(file_path)) {
    tryCatch({
      toc <- pdftools::pdf_toc(file_path)
      if (!is.null(toc) && length(toc) > 0) {
        section_names <- extract_all_titles(toc)
        section_names <- section_names[nchar(section_names) > 0]
        if (length(section_names) > 0) {
          # Strip leading number prefixes (e.g. "1 Introduction" -> "Introduction")
          section_names <- sub("^[0-9]+(\\.[0-9]+)*\\s+", "", section_names)
          # Normalize ligatures in TOC names
          section_names <- normalize_ligatures(section_names)
          # Filter out very long entries (likely paper title)
          section_names <- section_names[nchar(section_names) <= 80]
          # Remove empty and deduplicate
          section_names <- section_names[nchar(section_names) > 0]
          section_names <- unique(section_names)
          message(sprintf("Using %d sections from PDF table of contents", length(section_names)))
        } else {
          section_names <- NULL
        }
      }
    }, error = function(e) {
      message("Could not extract TOC from PDF, using common section names")
    })
  }

  if (is.null(section_names) || length(section_names) == 0) {
    section_names <- common_sections
    message("Using common section names for pattern matching")
  }

  sections_found <- list()
  unmatched <- character()

  # Pass 1 (strict): require \n\n before section name
  for (section in section_names) {
    section_escaped <- gsub("([.|?*+^$()\\[\\]{}\\\\])", "\\\\\\1", section, perl = TRUE)
    pattern <- paste0("\\n\\n(?:[0-9]+(?:\\.[0-9]+)*\\.?\\s*\\n\\n)?", section_escaped, "(?=\\s)")
    match <- regexpr(pattern, text, ignore.case = TRUE, perl = TRUE)

    if (match > 0) {
      match_length <- attr(match, "match.length")
      sections_found[[section]] <- list(
        start = as.integer(match),
        length = match_length
      )
    } else {
      unmatched <- c(unmatched, section)
    }
  }

  # Pass 2 (relaxed): allow single \n + optional number prefix inline
  for (section in unmatched) {
    section_escaped <- gsub("([.|?*+^$()\\[\\]{}\\\\])", "\\\\\\1", section, perl = TRUE)
    pattern <- paste0("(?:\\n)\\s*(?:[0-9]+(?:\\.[0-9]+)*\\.?\\s+)?", section_escaped, "(?=[\\s:])")
    match <- regexpr(pattern, text, ignore.case = TRUE, perl = TRUE)

    if (match > 0) {
      match_length <- attr(match, "match.length")
      sections_found[[section]] <- list(
        start = as.integer(match),
        length = match_length
      )
    }
  }

  if (length(sections_found) == 0) {
    message("No clear sections found. Returning full text as single element.")
    return(list("Full_text" = text))
  }

  sections_found <- sections_found[order(sapply(sections_found, function(x) x$start))]
  sections_list <- list()
  section_names_ordered <- names(sections_found)

  first_start <- sections_found[[1]]$start
  if (first_start > 100) {
    preface <- trimws(substr(text, 1, first_start - 1))
    if (nchar(preface) > 50) {
      sections_list[["Preface"]] <- preface
    }
  }

  for (i in seq_along(sections_found)) {
    section_name <- section_names_ordered[i]
    content_start <- sections_found[[i]]$start + sections_found[[i]]$length

    content_end <- if (i < length(sections_found)) {
      sections_found[[i + 1]]$start - 1
    } else {
      nchar(text)
    }

    content <- substr(text, content_start, content_end)
    content <- trimws(content)

    original_name <- section_name
    counter <- 1
    while (section_name %in% names(sections_list)) {
      counter <- counter + 1
      section_name <- paste0(original_name, " (", counter, ")")
    }

    if (nchar(content) > 0) {
      sections_list[[section_name]] <- content
    }
  }

  # rename Bibliography to References if the first exists and the second not
  if ("Bibliography" %in% names(sections_list) &&
      !("References" %in% names(sections_list))) {
    sections_list[["References"]] <- sections_list[["Bibliography"]]
    sections_list[["Bibliography"]] <- NULL
  }

  message(sprintf("Found %d sections: %s",
                  length(sections_list),
                  paste(names(sections_list), collapse = ", ")))

  return(c("Full_text" = text, sections_list))
}

#' Normalize references section formatting
#'
#' @description
#' Normalizes the References section to ensure each reference is separated by
#' double newlines and internal line breaks are removed.
#'
#' @param text_sections Named list from split_into_sections()
#'
#' @return Modified text_sections list with normalized References
#'
#' @details
#' Detects reference start patterns (Author, Initial.) and ensures consistent
#' formatting with \\n\\n separators between references.
#'
#' @examples
#' \dontrun{
#' sections <- split_into_sections(text, "paper.pdf")
#' sections <- normalize_references_section(sections)
#' }
#'
#' @export
#' @importFrom stringr str_trim
normalize_references_section <- function(text_sections) {

  if (!"References" %in% names(text_sections)) {
    return(text_sections)
  }

  ref_text <- text_sections[["References"]]

  ref_start_pattern <- "([A-Z][a-z]+(?:['-][A-Z][a-z]+)?,\\s+[A-Z]\\.)"

  matches <- gregexpr(ref_start_pattern, ref_text, perl = TRUE)[[1]]

  if (matches[1] == -1) {
    return(text_sections)
  }

  start_positions <- as.integer(matches)

  individual_refs <- character()

  for (i in seq_along(start_positions)) {
    start_pos <- start_positions[i]

    if (i < length(start_positions)) {
      end_pos <- start_positions[i + 1] - 1
    } else {
      end_pos <- nchar(ref_text)
    }

    ref_block <- substr(ref_text, start_pos, end_pos)

    ref_block <- gsub("\\n+", " ", ref_block)
    ref_block <- gsub("\\s+", " ", ref_block)
    ref_block <- stringr::str_trim(ref_block)

    individual_refs <- c(individual_refs, ref_block)
  }

  normalized_refs <- paste(individual_refs, collapse = "\n\n")

  text_sections[["References"]] <- normalized_refs

  message(sprintf("Normalized %d references with consistent \\n\\n separators",
                  length(individual_refs)))

  return(text_sections)
}

