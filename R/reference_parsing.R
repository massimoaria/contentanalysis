#' Detect the dominant reference format in a set of reference strings
#'
#' @param ref_texts Character vector of individual reference texts
#'
#' @return Character: "author_year_paren", "author_year_bare", or "numbered"
#'
#' @keywords internal
#' @noRd
detect_reference_format <- function(ref_texts) {
  if (length(ref_texts) == 0) return("author_year_paren")

  # Sample up to 10 refs for detection
  sample_refs <- ref_texts[seq_len(min(10, length(ref_texts)))]

  # Count format indicators
  n_paren_year <- sum(grepl("\\(\\d{4}[a-z]?\\)", sample_refs, perl = TRUE))
  n_bare_year <- sum(grepl(",\\s*\\d{4}[a-z]?\\.", sample_refs, perl = TRUE))
  n_numbered <- sum(grepl("^\\[?\\d+\\]?\\.?\\s", sample_refs, perl = TRUE))

  if (n_numbered >= n_paren_year && n_numbered >= n_bare_year && n_numbered >= 2) {
    return("numbered")
  } else if (n_bare_year > n_paren_year) {
    return("author_year_bare")
  } else {
    return("author_year_paren")
  }
}


#' Extract year from a reference string using multi-format detection
#'
#' @param ref_text Character. A single reference text.
#' @param format Character. The detected format.
#'
#' @return Character string with the year (e.g., "2023", "2023a"), or NA
#'
#' @keywords internal
#' @noRd
extract_year <- function(ref_text, format = "author_year_paren") {
  year <- NA_character_

  if (format == "author_year_paren") {
    # Pattern: (2023) or (2023a)
    year <- stringr::str_extract(ref_text, "(?<=\\()\\d{4}[a-z]?(?=\\))")
  }

  if (is.na(year) && format == "author_year_bare") {
    # Pattern: , 2023. or , 2023a.
    year <- stringr::str_extract(ref_text, "(?<=,\\s)\\d{4}[a-z]?(?=\\.)")
  }

  # Fallback: try both patterns if primary didn't match

  if (is.na(year)) {
    year <- stringr::str_extract(ref_text, "(?<=\\()\\d{4}[a-z]?(?=\\))")
  }
  if (is.na(year)) {
    year <- stringr::str_extract(ref_text, "(?<=,\\s)\\d{4}[a-z]?(?=\\.)")
  }
  if (is.na(year)) {
    # Last resort: find any 4-digit year followed by punctuation
    year <- stringr::str_extract(ref_text, "\\b(1[89]\\d{2}|20[0-2]\\d)[a-z]?(?=[.,;)\\s])")
  }

  # Validate year range
  if (!is.na(year)) {
    year_num <- as.integer(gsub("[a-z]$", "", year))
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    if (is.na(year_num) || year_num < 1800 || year_num > current_year + 1) {
      year <- NA_character_
    }
  }

  year
}


#' Extract the authors section from a reference string
#'
#' @param ref_text Character. A single reference text.
#' @param ref_year Character. The extracted year (used as delimiter).
#' @param format Character. The detected format.
#'
#' @return Character string with the authors section
#'
#' @keywords internal
#' @noRd
extract_authors_section <- function(ref_text, ref_year, format = "author_year_paren") {
  if (is.na(ref_year)) {
    # No year found — take everything before first period-space or first 200 chars
    authors <- stringr::str_extract(ref_text, "^[^.]{0,200}")
    return(stringr::str_trim(authors))
  }

  year_bare <- gsub("[a-z]$", "", ref_year)

  if (format == "author_year_paren") {
    # Everything before (YYYY)
    pattern <- paste0("^(.*?)\\(", year_bare)
    m <- regexpr(pattern, ref_text, perl = TRUE)
    if (m > 0) {
      authors <- substr(ref_text, 1, m + attr(m, "match.length") - nchar(year_bare) - 2)
      return(stringr::str_trim(authors))
    }
  }

  if (format == "author_year_bare") {
    # Everything before , YYYY.
    pattern <- paste0("^(.*?),\\s*", year_bare)
    m <- regexpr(pattern, ref_text, perl = TRUE)
    if (m > 0) {
      authors <- substr(ref_text, 1, m + attr(m, "match.length") - nchar(year_bare) - 2)
      return(stringr::str_trim(authors))
    }
  }

  # Fallback: take text before year occurrence
  year_pos <- regexpr(year_bare, ref_text, fixed = TRUE)
  if (year_pos > 0) {
    authors <- substr(ref_text, 1, year_pos - 1)
    # Remove trailing punctuation and whitespace
    authors <- gsub("[,&\\s]+$", "", authors, perl = TRUE)
    return(stringr::str_trim(authors))
  }

  # Last fallback
  stringr::str_trim(stringr::str_extract(ref_text, "^[^.]{0,200}"))
}


#' Parse authors from an author string into first/second author and count
#'
#' @param authors_text Character. The author section of a reference.
#'
#' @return List with: first_author, second_author, n_authors
#'
#' @keywords internal
#' @noRd
parse_authors <- function(authors_text) {
  result <- list(
    first_author = NA_character_,
    first_author_normalized = NA_character_,
    second_author = NA_character_,
    second_author_normalized = NA_character_,
    n_authors = NA_integer_
  )

  if (is.na(authors_text) || nchar(trimws(authors_text)) == 0) {
    return(result)
  }

  authors_text <- trimws(authors_text)

  # Remove numbered prefix: [1], 1., etc.
  authors_text <- gsub("^\\[?\\d+\\]?\\.?\\s*", "", authors_text)
  authors_text <- trimws(authors_text)

  if (nchar(authors_text) == 0) return(result)

  # Multi-word surname prefixes
  surname_prefixes <- "(?:van\\s+de\\s+|van\\s+der\\s+|van\\s+den\\s+|van\\s+het\\s+|van\\s+|von\\s+|de\\s+la\\s+|de\\s+|del\\s+|di\\s+|da\\s+|du\\s+|le\\s+|la\\s+|el\\s+|al-|Mc|Mac|O')"

  # Extract first author: text up to first comma
  # Handle compound surnames: "Van de Gaer, D." or "Di Maio, A."
  first_author_pattern <- paste0(
    "^(", surname_prefixes, "?",
    "[A-Z\u00C0-\u00FF][a-z\u00E0-\u00FF]+(?:[-'][A-Z\u00C0-\u00FF][a-z\u00E0-\u00FF]+)*)"
  )
  first_author <- stringr::str_extract(authors_text, first_author_pattern)

  if (!is.na(first_author)) {
    result$first_author <- trimws(first_author)
    # Normalize: lowercase, remove accents (simplified)
    normalized <- tolower(first_author)
    normalized <- gsub("[\u00E0\u00E1\u00E2\u00E3\u00E4\u00E5]", "a", normalized)
    normalized <- gsub("[\u00E8\u00E9\u00EA\u00EB]", "e", normalized)
    normalized <- gsub("[\u00EC\u00ED\u00EE\u00EF]", "i", normalized)
    normalized <- gsub("[\u00F2\u00F3\u00F4\u00F5\u00F6]", "o", normalized)
    normalized <- gsub("[\u00F9\u00FA\u00FB\u00FC]", "u", normalized)
    normalized <- gsub("[\u00F1]", "n", normalized)
    normalized <- gsub("[\u00E7]", "c", normalized)
    normalized <- gsub("[^a-z'-]", "", normalized)
    result$first_author_normalized <- normalized
  }

  # Count authors: count "Surname, I." patterns + "&" connectors
  # Strategy: split on "&" or " and " and count comma-initial patterns
  has_etal <- grepl("et\\s+al", authors_text, ignore.case = TRUE)

  if (has_etal) {
    result$n_authors <- 99L
  } else {
    # Count distinct author entries
    # Each author typically has: Surname, I. or Surname, I. I.
    # Look for pattern: uppercase-letter-dot sequences after commas
    # Better approach: count the number of "., " + initial patterns
    # or count & + author units
    parts <- strsplit(authors_text, "\\s*[&]\\s*|\\s+and\\s+", perl = TRUE)[[1]]
    n_from_ampersand <- length(parts)

    # Within each part, count comma-separated authors
    # "Aria, M., Cuccurullo, C." has 2 authors: count "Surname, Initial." pattern
    author_pattern <- paste0(
      "(?:", surname_prefixes, "?",
      "[A-Z\u00C0-\u00FF][a-z\u00E0-\u00FF]+(?:[-'][A-Z\u00C0-\u00FF][a-z\u00E0-\u00FF]+)*",
      ",\\s*[A-Z]\\.)"
    )
    n_from_pattern <- length(gregexpr(author_pattern, authors_text, perl = TRUE)[[1]])
    if (n_from_pattern <= 0) n_from_pattern <- 1

    # Use the larger count (pattern-based is more accurate)
    result$n_authors <- as.integer(max(n_from_pattern, n_from_ampersand))
  }

  # Extract second author
  if (result$n_authors >= 2 || has_etal) {
    # Try after first ", I." or ", I. I." pattern + ", " or " & "
    # Pattern: first author's initial(s), then separator, then second surname
    second_pattern <- paste0(
      "[A-Z]\\.(?:\\s*[A-Z]\\.)*",  # first author's initials
      "\\s*[,&]\\s*",               # separator
      "(", surname_prefixes, "?",
      "[A-Z\u00C0-\u00FF][a-z\u00E0-\u00FF]+(?:[-'][A-Z\u00C0-\u00FF][a-z\u00E0-\u00FF]+)*)"
    )
    second_author <- stringr::str_match(authors_text, second_pattern)

    if (!is.null(second_author) && !is.na(second_author[1, 2])) {
      result$second_author <- trimws(second_author[1, 2])
      result$second_author_normalized <- tolower(result$second_author)
      # Apply same accent normalization
      result$second_author_normalized <- gsub(
        "[\u00E0\u00E1\u00E2\u00E3\u00E4\u00E5]", "a",
        result$second_author_normalized
      )
      result$second_author_normalized <- gsub(
        "[\u00E8\u00E9\u00EA\u00EB]", "e",
        result$second_author_normalized
      )
      result$second_author_normalized <- gsub(
        "[\u00F2\u00F3\u00F4\u00F5\u00F6]", "o",
        result$second_author_normalized
      )
      result$second_author_normalized <- gsub("[^a-z'-]", "", result$second_author_normalized)
    }
  }

  result
}


#' Extract title from a reference string
#'
#' @param ref_text Character. Full reference text.
#' @param ref_year Character. The extracted year.
#' @param format Character. The detected format.
#'
#' @return Character string with the title, or NA
#'
#' @keywords internal
#' @noRd
extract_title <- function(ref_text, ref_year, format = "author_year_paren") {
  if (is.na(ref_year)) return(NA_character_)

  year_bare <- gsub("[a-z]$", "", ref_year)

  # Find the position after the year marker
  if (format == "author_year_paren") {
    pattern <- paste0("\\(", ref_year, "\\)\\.?\\s*")
  } else {
    pattern <- paste0(",?\\s*", ref_year, "\\.\\s*")
  }

  m <- regexpr(pattern, ref_text, perl = TRUE)
  if (m == -1) return(NA_character_)

  title_start <- as.integer(m) + attr(m, "match.length")
  rest <- substr(ref_text, title_start, nchar(ref_text))

  # Title ends at the first period followed by a space and uppercase/journal marker
  # or at a journal name pattern (italic markers, volume info, etc.)
  title_end <- regexpr("\\.\\s+[A-Z]|\\b\\d+\\s*\\(\\d+\\)", rest, perl = TRUE)
  if (title_end > 0) {
    title <- substr(rest, 1, title_end)
  } else {
    # Take first sentence
    title <- stringr::str_extract(rest, "^[^.]+")
  }

  stringr::str_trim(title)
}


#' Extract DOI from a reference string
#'
#' @param ref_text Character. Full reference text.
#'
#' @return Character string with DOI, or NA
#'
#' @keywords internal
#' @noRd
extract_doi <- function(ref_text) {
  # DOI patterns: https://doi.org/10.xxx, doi:10.xxx, or 10.xxxx/xxx
  doi <- stringr::str_extract(
    ref_text,
    "(?:https?://doi\\.org/|doi:\\s*)?10\\.\\d{4,}/[^\\s,]+"
  )
  if (!is.na(doi)) {
    # Clean: remove trailing punctuation
    doi <- gsub("[.,;)]+$", "", doi)
    # Normalize: remove URL prefix
    doi <- gsub("^https?://doi\\.org/", "", doi)
    doi <- gsub("^doi:\\s*", "", doi)
  }
  doi
}


#' Parse references section from text
#'
#' @description
#' Parses a references section into individual entries with extracted metadata
#' including authors, year, and title information. Automatically detects the
#' reference format (APA parenthetical, Harvard bare year, or numbered).
#'
#' @param references_text Character string. Text of references section,
#'   with individual references separated by double newlines.
#'
#' @return Tibble with columns:
#'   \itemize{
#'     \item ref_id: Unique reference identifier (REF_1, REF_2, ...)
#'     \item ref_full_text: Complete reference text
#'     \item ref_authors: Author string (text before year)
#'     \item ref_year: Publication year (e.g., "2023", "2023a")
#'     \item ref_first_author: First author surname
#'     \item ref_first_author_normalized: Lowercase, accent-stripped first author
#'     \item ref_second_author: Second author surname (if present)
#'     \item ref_second_author_normalized: Lowercase second author
#'     \item n_authors: Number of authors (99 = et al.)
#'     \item ref_title: Extracted title (text after year)
#'     \item ref_doi: DOI if found in reference text
#'   }
#'
#' @examples
#' \dontrun{
#' refs_text <- doc$References
#' parsed_refs <- parse_references_section(refs_text)
#' }
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom stringr str_split str_trim str_extract str_count str_detect str_replace_all str_to_lower str_match
parse_references_section <- function(references_text) {

  empty_result <- tibble::tibble(
    ref_id = character(),
    ref_full_text = character(),
    ref_authors = character(),
    ref_year = character(),
    ref_first_author = character(),
    ref_first_author_normalized = character(),
    ref_second_author = character(),
    ref_second_author_normalized = character(),
    n_authors = integer(),
    ref_title = character(),
    ref_doi = character()
  )

  if (is.null(references_text) || length(references_text) == 0 ||
      all(is.na(references_text)) || all(references_text == "")) {
    return(empty_result)
  }

  # Split into individual references (already normalized by normalize_references_section)
  individual_refs <- strsplit(references_text, "\\n\\n+")[[1]]
  individual_refs <- trimws(individual_refs)
  individual_refs <- individual_refs[nchar(individual_refs) > 0]

  # Remove very short fragments (< 15 chars) that are likely artifacts
  individual_refs <- individual_refs[nchar(individual_refs) >= 15]

  if (length(individual_refs) == 0) return(empty_result)

  # Clean each reference: collapse internal whitespace
  individual_refs <- gsub("\\s+", " ", individual_refs)
  individual_refs <- trimws(individual_refs)

  # Detect dominant format
  ref_format <- detect_reference_format(individual_refs)

  # Parse each reference
  ref_ids <- paste0("REF_", seq_along(individual_refs))
  ref_years <- character(length(individual_refs))
  ref_authors_list <- character(length(individual_refs))
  ref_first <- character(length(individual_refs))
  ref_first_norm <- character(length(individual_refs))
  ref_second <- character(length(individual_refs))
  ref_second_norm <- character(length(individual_refs))
  ref_n_auth <- integer(length(individual_refs))
  ref_titles <- character(length(individual_refs))
  ref_dois <- character(length(individual_refs))

  for (i in seq_along(individual_refs)) {
    ref <- individual_refs[i]

    # Extract year
    year <- extract_year(ref, ref_format)
    ref_years[i] <- ifelse(is.null(year), NA_character_, year)

    # Extract authors section
    auth_text <- extract_authors_section(ref, year, ref_format)
    ref_authors_list[i] <- ifelse(is.null(auth_text), NA_character_, auth_text)

    # Parse authors
    auth_info <- parse_authors(auth_text)
    ref_first[i] <- auth_info$first_author
    ref_first_norm[i] <- auth_info$first_author_normalized
    ref_second[i] <- auth_info$second_author
    ref_second_norm[i] <- auth_info$second_author_normalized
    ref_n_auth[i] <- auth_info$n_authors

    # Extract title
    title <- extract_title(ref, year, ref_format)
    ref_titles[i] <- ifelse(is.null(title), NA_character_, title)

    # Extract DOI
    doi <- extract_doi(ref)
    ref_dois[i] <- ifelse(is.null(doi), NA_character_, doi)
  }

  tibble::tibble(
    ref_id = ref_ids,
    ref_full_text = individual_refs,
    ref_authors = ref_authors_list,
    ref_year = ref_years,
    ref_first_author = ref_first,
    ref_first_author_normalized = ref_first_norm,
    ref_second_author = ref_second,
    ref_second_author_normalized = ref_second_norm,
    n_authors = ref_n_auth,
    ref_title = ref_titles,
    ref_doi = ref_dois
  )
}
