#' Normalize Unicode ligatures and special characters to ASCII equivalents
#'
#' @param x Character string to normalize
#'
#' @return Character string with ligatures and special characters replaced
#'
#' @keywords internal
#' @noRd
normalize_ligatures <- function(x) {
  # Ligatures
  x <- gsub("\ufb04", "ffl", x)
  x <- gsub("\ufb03", "ffi", x)
  x <- gsub("\ufb02", "fl", x)
  x <- gsub("\ufb01", "fi", x)
  x <- gsub("\ufb00", "ff", x)
  # Non-breaking spaces
  x <- gsub("\u00a0", " ", x)
  x
}

#' Collapse letter-spaced uppercase headers to normal words
#'
#' Some journal styles (e.g. Wiley) render section headers with decorative
#' letter spacing: "I NTRO D U C TI O N" instead of "INTRODUCTION".
#' This function detects and collapses such sequences.
#'
#' @param text Character string to normalize
#'
#' @return Character string with spaced headers collapsed
#'
#' @keywords internal
#' @noRd
normalize_spaced_headers <- function(text) {
  # Match sequences of uppercase letter groups (1-4 chars) separated by single
  # spaces, with at least 4 groups — a hallmark of letter-spaced headers
  pattern <- "(?<![A-Za-z])([A-Z]{1,4}(?: [A-Z]{1,4}){3,})(?![A-Za-z])"
  m <- gregexpr(pattern, text, perl = TRUE)
  if (m[[1]][1] == -1) return(text)

  matches <- regmatches(text, m)[[1]]
  replacements <- character(length(matches))

  for (i in seq_along(matches)) {
    groups <- strsplit(matches[i], " ")[[1]]
    short_ratio <- sum(nchar(groups) <= 2) / length(groups)
    collapsed <- paste(groups, collapse = "")
    # Only collapse if majority of groups are short (letter-spaced) and result
    # is long enough to be a real section name (filters out "TABLE" etc.)
    if (short_ratio >= 0.5 && nchar(collapsed) >= 7) {
      replacements[i] <- collapsed
    } else {
      replacements[i] <- matches[i]
    }
  }

  regmatches(text, m) <- list(replacements)

  # Insert line breaks before pipe-separated section headers that follow a
  # sentence end (e.g. "in OLPs. 2 | METHODS" -> "in OLPs.\n\n2 | METHODS")
  text <- gsub("(?<=\\.)\\s+(?=[0-9]+(?:\\.[0-9]+)*\\s*\\|\\s*[A-Z])",
               "\n\n", text, perl = TRUE)

  text
}

#' Strip repeated running headers inserted by journals at page breaks
#'
#' Many journals insert a running header (journal name, volume, issue) at each
#' page break. These consume the \\n\\n boundary that section headers need.
#' This function detects and removes such repeated headers.
#'
#' @param text Character string to clean
#'
#' @return Character string with running headers removed
#'
#' @keywords internal
#' @noRd
strip_running_headers <- function(text) {
  # Find all page_number\n\n patterns
  m <- gregexpr("[0-9]+\n\n", text, perl = TRUE)
  if (m[[1]][1] == -1) return(text)

  positions <- as.integer(m[[1]])
  lengths <- attr(m[[1]], "match.length")

  # Extract text after each match (up to 200 chars)
  after_texts <- character(length(positions))
  for (i in seq_along(positions)) {
    start <- positions[i] + lengths[i]
    end <- min(nchar(text), start + 199L)
    if (start <= nchar(text)) {
      after_texts[i] <- substr(text, start, end)
    } else {
      after_texts[i] <- ""
    }
  }

  # Group texts by short prefix (40 chars) to find candidate running headers
  short_prefixes <- substr(after_texts, 1L, 40L)
  prefix_groups <- split(seq_along(after_texts), short_prefixes)

  # Process ALL groups with >= 3 occurrences (handles alternating headers)
  total_stripped <- 0L
  for (group_name in names(prefix_groups)) {
    group <- prefix_groups[[group_name]]
    if (length(group) < 3L) next

    # Extend prefix as far as all group members agree (find exact header length)
    group_texts <- after_texts[group]
    max_len <- min(nchar(group_texts))
    prefix_len <- min(40L, max_len)
    if (max_len > 40L) {
      for (i in 41L:max_len) {
        chars <- substr(group_texts, i, i)
        if (length(unique(chars)) > 1L) break
        prefix_len <- i
      }
    }
    header <- substr(group_texts[1L], 1L, prefix_len)
    escaped <- gsub("([.|?*+^$()\\[\\]{}\\\\])", "\\\\\\1", header, perl = TRUE)
    text <- gsub(paste0("[0-9]+\\n\\n", escaped), "\n\n", text, perl = TRUE)
    total_stripped <- total_stripped + length(group)
    message(sprintf("Stripped running header (%d occurrences, %d chars)",
                    length(group), prefix_len))
  }

  text
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

  # If already a named list (e.g., from pdf2txt_auto with sections=TRUE), return as-is
  if (is.list(text)) {
    return(text)
  }

  # Collapse vector of pages/paragraphs into a single string
  if (length(text) > 1) {
    text <- paste(text, collapse = "\n\n")
  }

  # Normalize text before any matching
  text <- normalize_ligatures(text)
  text <- strip_running_headers(text)
  text <- normalize_spaced_headers(text)

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
    pattern <- paste0("\\n\\n(?:[0-9]+(?:\\.[0-9]+)*\\.?\\s*(?:\\n\\n|\\|\\s*))?", section_escaped, "(?=[\\s.:])")
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
    pattern <- paste0("(?:\\n)\\s*(?:[0-9]+(?:\\.[0-9]+)*\\.?\\s*\\|?\\s+)?", section_escaped, "(?=[\\s.:])")
    match <- regexpr(pattern, text, ignore.case = TRUE, perl = TRUE)

    if (match > 0) {
      match_length <- attr(match, "match.length")
      sections_found[[section]] <- list(
        start = as.integer(match),
        length = match_length
      )
    }
  }

  # Fallback: if TOC names matched nothing, retry with common section names
  if (length(sections_found) == 0 && !identical(section_names, common_sections)) {
    message("TOC section names matched nothing in text; retrying with common section names")
    section_names <- common_sections
    unmatched <- character()

    for (section in section_names) {
      section_escaped <- gsub("([.|?*+^$()\\[\\]{}\\\\])", "\\\\\\1", section, perl = TRUE)
      pattern <- paste0("\\n\\n(?:[0-9]+(?:\\.[0-9]+)*\\.?\\s*(?:\\n\\n|\\|\\s*))?", section_escaped, "(?=[\\s.:])")
      match <- regexpr(pattern, text, ignore.case = TRUE, perl = TRUE)
      if (match > 0) {
        sections_found[[section]] <- list(start = as.integer(match), length = attr(match, "match.length"))
      } else {
        unmatched <- c(unmatched, section)
      }
    }

    for (section in unmatched) {
      section_escaped <- gsub("([.|?*+^$()\\[\\]{}\\\\])", "\\\\\\1", section, perl = TRUE)
      pattern <- paste0("(?:\\n)\\s*(?:[0-9]+(?:\\.[0-9]+)*\\.?\\s*\\|?\\s+)?", section_escaped, "(?=[\\s.:])")
      match <- regexpr(pattern, text, ignore.case = TRUE, perl = TRUE)
      if (match > 0) {
        sections_found[[section]] <- list(start = as.integer(match), length = attr(match, "match.length"))
      }
    }
  }

  # Pass 3 (emergency): specifically look for References/Bibliography if not found
  ref_names <- c("References", "REFERENCES", "Bibliography", "BIBLIOGRAPHY")
  has_refs <- any(ref_names %in% names(sections_found))
  if (!has_refs) {
    for (ref_name in ref_names) {
      # Try multiple patterns from most strict to most permissive
      ref_patterns <- c(
        paste0("\\n\\n\\s*", ref_name, "\\s*\\n"),           # \n\nReferences\n
        paste0("\\n\\s*", ref_name, "\\s*\\n"),               # \nReferences\n
        paste0("\\n\\n\\s*", ref_name, "\\s"),                # \n\nReferences<space>
        paste0("\\n\\s*", ref_name, "\\s"),                   # \nReferences<space>
        paste0("\\s", ref_name, "\\s+(?=[A-Z][a-z])"),       # ...text References Author...
        paste0("\\s", ref_name, "\\s*\\n")                   # ...text References\n
      )
      for (pat in ref_patterns) {
        # Use the LAST occurrence (most likely the actual bibliography, not in-text mentions)
        all_matches <- gregexpr(pat, text, perl = TRUE)[[1]]
        if (all_matches[1] == -1) next

        # Take the last match (bibliography is typically at the end)
        match_idx <- length(all_matches)
        match_pos <- all_matches[match_idx]
        match_len <- attr(all_matches, "match.length")[match_idx]

        # Verify it's followed by reference-like content
        match_end <- match_pos + match_len
        after_text <- substr(text, match_end, min(nchar(text), match_end + 300))
        looks_like_refs <- grepl(
          "^\\s*[A-Z][a-z\u00C0-\u00FF]|^\\s*\\[?\\d+\\]?[.\\s]|^\\s*\\d+\\.\\s",
          after_text, perl = TRUE
        )
        # Also verify it's in the last 30% of the document (bibliography is near the end)
        is_near_end <- match_pos > nchar(text) * 0.5

        if (looks_like_refs && is_near_end) {
          canonical_name <- if (grepl("(?i)biblio", ref_name)) "Bibliography" else "References"
          sections_found[[canonical_name]] <- list(
            start = as.integer(match_pos),
            length = as.integer(match_len)
          )
          break
        }
      }
      if (any(c("References", "Bibliography") %in% names(sections_found))) break
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
  if (is.null(ref_text) || nchar(trimws(ref_text)) < 20) {
    return(text_sections)
  }

  # Pre-processing Step 1: collapse fragmented numbered references
  # Many PDFs produce: "1.\n\nLocker D, Allen F.\n\nWhat do measures..."
  # We need to join "N.\n\n" with the next block when N is a reference number
  # First, rejoin "N.\n\n" followed by author name (not another number)
  ref_text <- gsub(
    "(\\d+)\\.\\s*\\n\\n+\\s*(?=[A-Z][a-z\u00C0-\u00FF])",
    "\\1. ",
    ref_text, perl = TRUE
  )
  # Also rejoin "[N]\n\n" followed by author name
  ref_text <- gsub(
    "(\\[\\d+\\])\\s*\\n\\n+\\s*(?=[A-Z][a-z\u00C0-\u00FF])",
    "\\1 ",
    ref_text, perl = TRUE
  )

  # Pre-processing Step 2: insert \n\n before numbered reference starts that are inline
  # Handles: "...text. 2. Author..." or "...text. [2] Author..."
  ref_text <- gsub(
    "(?<=[.;])\\s+(?=\\d+\\.\\s+[A-Z][a-z])",
    "\n\n",
    ref_text, perl = TRUE
  )
  ref_text <- gsub(
    "(?<=[.;])\\s+(?=\\[\\d+\\]\\s*[A-Z])",
    "\n\n",
    ref_text, perl = TRUE
  )

  # Also insert \n\n before author-year starts that are inline
  # Pattern: end of ref (period/number) followed by new Surname, I.
  ref_text <- gsub(
    "(?<=\\.)\\s+(?=[A-Z][a-z\u00C0-\u00FF]+(?:[-'][A-Z][a-z]+)?,\\s+[A-Z]\\.)",
    "\n\n",
    ref_text, perl = TRUE
  )

  # Detect the reference format to choose the right start-of-reference pattern
  # Format 1: Author-year with parentheses: "Surname, I. (2020)."
  # Format 2: Author-year without parentheses: "Surname, I., 2020."
  # Format 3: Numbered: "[1]" or "1."

  # Split into blocks separated by \n\n
  blocks <- strsplit(ref_text, "\\n\\n+")[[1]]
  blocks <- trimws(blocks)
  blocks <- blocks[nchar(blocks) > 0]

  if (length(blocks) == 0) return(text_sections)

  # Patterns that indicate the START of a new reference
  author_year_start <- "^[A-Z][a-z\u00C0-\u00FF]+(?:[-'][A-Z][a-z]+)?,\\s+[A-Z]\\."
  numbered_bracket_start <- "^\\[\\d+\\]"
  numbered_dot_start <- "^\\d+\\.\\s+[A-Z]"
  # Also detect institutional authors: "AIOM", "WHO", etc.
  institutional_start <- "^[A-Z]{2,}[,.]"

  # Detect dominant format from blocks
  n_author_year <- sum(grepl(author_year_start, blocks, perl = TRUE))
  n_numbered_bracket <- sum(grepl(numbered_bracket_start, blocks, perl = TRUE))
  n_numbered_dot <- sum(grepl(numbered_dot_start, blocks, perl = TRUE))

  # Choose reference start detection function based on dominant format
  is_ref_start <- function(block) {
    if (nchar(block) == 0) return(FALSE)

    # Numbered formats are unambiguous
    if (grepl(numbered_bracket_start, block, perl = TRUE)) return(TRUE)

    # Numbered dot format: "1. Author..." or "12. Author..."
    if (grepl("^\\d+\\.\\s+[A-Z]", block, perl = TRUE)) return(TRUE)

    # Author-year format: must start with Surname, Initial.
    if (grepl(author_year_start, block, perl = TRUE)) {
      # Extra check: block should contain a year (near the start, within first 200 chars)
      prefix <- substr(block, 1, min(200, nchar(block)))
      has_year <- grepl("\\b(1[89]\\d{2}|20[0-2]\\d)[a-z]?\\b", prefix, perl = TRUE)
      # Or block is long enough to be a complete reference
      if (has_year || nchar(block) >= 80) return(TRUE)
    }

    # Author format without comma-initial: "Surname Name (Year)" e.g. "Albuquerque N (2016)"
    if (grepl("^[A-Z][a-z\u00C0-\u00FF]+\\s+[A-Z]", block, perl = TRUE) &&
        grepl("\\b(1[89]\\d{2}|20[0-2]\\d)", block, perl = TRUE) &&
        nchar(block) >= 40) {
      return(TRUE)
    }

    # Institutional author
    if (grepl(institutional_start, block, perl = TRUE) && nchar(block) >= 40) {
      return(TRUE)
    }

    FALSE
  }

  # Re-assemble: merge blocks that are NOT the start of a new reference
  # into the previous reference
  assembled_refs <- character()
  current_ref <- ""

  for (i in seq_along(blocks)) {
    block <- blocks[i]

    if (is_ref_start(block)) {
      # Save previous ref if any
      if (nchar(current_ref) > 0) {
        assembled_refs <- c(assembled_refs, current_ref)
      }
      current_ref <- block
    } else {
      # Append to current ref (continuation)
      if (nchar(current_ref) > 0) {
        current_ref <- paste(current_ref, block)
      } else {
        # No current ref yet - start one anyway
        current_ref <- block
      }
    }
  }
  # Save last ref
  if (nchar(current_ref) > 0) {
    assembled_refs <- c(assembled_refs, current_ref)
  }

  # Cleanup: collapse internal whitespace, trim
  assembled_refs <- gsub("\\s+", " ", assembled_refs)
  assembled_refs <- trimws(assembled_refs)
  assembled_refs <- assembled_refs[nchar(assembled_refs) > 0]

  if (length(assembled_refs) == 0) return(text_sections)

  normalized_refs <- paste(assembled_refs, collapse = "\n\n")
  text_sections[["References"]] <- normalized_refs

  message(sprintf("Normalized %d references with consistent \\n\\n separators",
                  length(assembled_refs)))

  return(text_sections)
}

