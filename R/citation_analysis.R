#' Normalize author name for robust comparison
#'
#' Removes diacritics, converts to lowercase, handles prefixes (van, de, di, etc.),
#' removes hyphens and extra whitespace for better matching
#'
#' @param name Character string with author name
#' @return Normalized name string
#' @keywords internal
normalize_author_name <- function(name) {
  if (is.na(name) || name == "" || is.null(name)) {
    return("")
  }

  # Convert to lowercase and trim
  name <- tolower(trimws(name))

  # Remove diacritics/accents using transliteration
  name <- iconv(name, to = "ASCII//TRANSLIT")

  # If transliteration failed, fall back to character replacement
  if (is.na(name)) {
    name <- tolower(trimws(name))
    # a variants
    name <- gsub(
      "[\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u0101\u0103\u0105]",
      "a",
      name
    )
    # e variants
    name <- gsub("[\u00e8\u00e9\u00ea\u00eb\u0113\u0117\u0119]", "e", name)
    # i variants
    name <- gsub("[\u00ec\u00ed\u00ee\u00ef\u012b\u012f\u0131]", "i", name)
    # o variants
    name <- gsub(
      "[\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8\u014d\u0151]",
      "o",
      name
    )
    # u variants
    name <- gsub("[\u00f9\u00fa\u00fb\u00fc\u016b\u0171\u0173]", "u", name)
    # y variants
    name <- gsub("[\u00fd\u00ff]", "y", name)
    # n variants
    name <- gsub("[\u00f1\u0144]", "n", name)
    # c variants
    name <- gsub("[\u00e7\u0107\u010d]", "c", name)
    # s variants
    name <- gsub("[\u015b\u0161\u015f]", "s", name)
    # z variants
    name <- gsub("[\u017a\u017c\u017e]", "z", name)
    name <- gsub("\u00df", "ss", name)
    name <- gsub("\u00e6", "ae", name)
    name <- gsub("\u0153", "oe", name)
    name <- gsub("\u0142", "l", name)
    name <- gsub("\u0111", "d", name)
  }

  # Remove common prefixes (van, de, di, von, der, den, la, le, el, al-, etc.)
  # These are often inconsistent between sources
  name <- gsub(
    "^(van |de |di |von |der |den |la |le |el |al-|van der |van den |van de |de la |de los |de las )",
    "",
    name
  )

  # Remove hyphens and underscores (often inconsistent)
  name <- gsub("[-_]", " ", name)

  # Remove leading/trailing punctuation and spaces
  name <- gsub("^[^a-z0-9]+|[^a-z0-9]+$", "", name)

  # Collapse multiple spaces into one
  name <- gsub("\\s+", " ", name)

  # Final trim
  name <- trimws(name)

  return(name)
}

#' Compare two author names with fuzzy matching
#'
#' Determines if two author names refer to the same person, accounting for
#' typos, prefix variations, accent differences, and minor spelling variations
#'
#' @param name1 First author name
#' @param name2 Second author name
#' @param threshold Similarity threshold (0-1). Default 0.8
#' @return Logical indicating if names match
#' @keywords internal
author_names_match <- function(name1, name2, threshold = 0.8) {
  # Handle empty or NA values
  if (is.na(name1) || is.na(name2) || name1 == "" || name2 == "") {
    return(FALSE)
  }

  # Normalize both names
  norm1 <- normalize_author_name(name1)
  norm2 <- normalize_author_name(name2)

  # Check if empty after normalization
  if (norm1 == "" || norm2 == "") {
    return(FALSE)
  }

  # Exact match after normalization
  if (norm1 == norm2) {
    return(TRUE)
  }

  # Check if one is substring of the other (handles "eck" vs "van eck")
  if (grepl(norm1, norm2, fixed = TRUE) || grepl(norm2, norm1, fixed = TRUE)) {
    return(TRUE)
  }

  # Check for common typos/transpositions
  # Example: leydesdorff vs leydesdroff
  if (nchar(norm1) == nchar(norm2)) {
    # Count different characters
    chars1 <- strsplit(norm1, "")[[1]]
    chars2 <- strsplit(norm2, "")[[1]]
    n_diff <- sum(chars1 != chars2)

    # Allow up to 2 character differences for names > 5 chars
    if (nchar(norm1) > 5 && n_diff <= 2) {
      return(TRUE)
    }

    # Allow 1 character difference for shorter names
    if (nchar(norm1) <= 5 && n_diff <= 1) {
      return(TRUE)
    }
  }

  # Compute Levenshtein distance for more sophisticated matching
  # Only if names are reasonably similar in length
  len_ratio <- min(nchar(norm1), nchar(norm2)) / max(nchar(norm1), nchar(norm2))

  if (len_ratio >= 0.7) {
    # Use stringdist if available, otherwise use simple approach
    if (requireNamespace("stringdist", quietly = TRUE)) {
      dist <- stringdist::stringdist(norm1, norm2, method = "lv")
      max_len <- max(nchar(norm1), nchar(norm2))
      similarity <- 1 - (dist / max_len)

      if (similarity >= threshold) {
        return(TRUE)
      }
    } else {
      # Simple character overlap similarity
      chars1 <- unique(strsplit(norm1, "")[[1]])
      chars2 <- unique(strsplit(norm2, "")[[1]])
      overlap <- length(intersect(chars1, chars2))
      union_size <- length(union(chars1, chars2))

      if (overlap / union_size >= threshold) {
        return(TRUE)
      }
    }
  }

  return(FALSE)
}

#' Check if author conflict is real or just normalization difference
#'
#' Determines whether a conflict between CrossRef and OpenAlex author names
#' is a genuine conflict (different authors) or just a normalization issue
#'
#' @param crossref_author Author name from CrossRef
#' @param openalex_author Author name from OpenAlex
#' @param doi DOI for logging purposes
#' @param verbose Logical, whether to print conflict messages
#' @return List with:
#'   \itemize{
#'     \item is_real_conflict: Logical, TRUE if authors are different
#'     \item message: Description of the conflict type
#'   }
#' @keywords internal
check_author_conflict <- function(
  crossref_author,
  openalex_author,
  doi = "",
  verbose = TRUE
) {
  # Handle empty values
  if (is.na(crossref_author) || crossref_author == "") {
    return(list(
      is_real_conflict = FALSE,
      message = "CrossRef author missing"
    ))
  }

  if (is.na(openalex_author) || openalex_author == "") {
    return(list(
      is_real_conflict = FALSE,
      message = "OpenAlex author missing"
    ))
  }

  # Check if names match
  if (author_names_match(crossref_author, openalex_author)) {
    if (verbose) {
      message(sprintf(
        "NOT A CONFLICT for DOI %s: CrossRef='%s' vs OpenAlex='%s' - same author (using OpenAlex)",
        doi,
        crossref_author,
        openalex_author
      ))
    }

    return(list(
      is_real_conflict = FALSE,
      message = "Same author (normalization difference)"
    ))
  }

  # Real conflict detected
  if (verbose) {
    message(sprintf(
      "REAL AUTHOR CONFLICT for DOI %s: CrossRef='%s' vs OpenAlex='%s' - keeping CrossRef",
      doi,
      crossref_author,
      openalex_author
    ))
  }

  return(list(
    is_real_conflict = TRUE,
    message = "Different authors"
  ))
}


#' Map citations to document segments or sections
#'
#' @param citations_df Data frame with citations and their positions
#' @param text Text object (string or list with sections)
#' @param use_sections Logical or "auto". Use sections if available
#' @param n_segments Integer. Number of segments if not using sections
#'
#' @return Citations data frame with segment/section information
#'
#' @keywords internal
#'
#' @export
#' @importFrom dplyr mutate
map_citations_to_segments <- function(
  citations_df,
  text,
  use_sections = "auto",
  n_segments = 10
) {
  sections_available <- FALSE
  section_names <- character(0)

  if (is.list(text)) {
    section_names <- setdiff(names(text), c("Full_text", "References"))
    sections_available <- length(section_names) > 0
  }

  use_sections_final <- FALSE
  if (use_sections == "auto") {
    use_sections_final <- sections_available
  } else if (is.logical(use_sections)) {
    if (use_sections && !sections_available) {
      warning("Sections requested but not available. Using segments instead.")
      use_sections_final <- FALSE
    } else {
      use_sections_final <- use_sections
    }
  }

  if (use_sections_final) {
    section_positions <- list()
    cumulative_pos <- 1

    for (sect_name in section_names) {
      sect_text <- text[[sect_name]]
      sect_length <- nchar(sect_text)

      section_positions[[sect_name]] <- list(
        start = cumulative_pos,
        end = cumulative_pos + sect_length - 1,
        name = sect_name
      )

      cumulative_pos <- cumulative_pos + sect_length + 1
    }

    citations_df$segment <- NA_character_
    for (i in 1:nrow(citations_df)) {
      cite_pos <- citations_df$start_pos[i]

      for (sect_name in section_names) {
        sect_info <- section_positions[[sect_name]]
        if (cite_pos >= sect_info$start && cite_pos <= sect_info$end) {
          citations_df$segment[i] <- sect_name
          break
        }
      }
    }

    citations_df$segment <- ifelse(
      is.na(citations_df$segment),
      "Unknown",
      citations_df$segment
    )
    citations_df$segment_type <- "section"
  } else {
    min_pos <- min(citations_df$start_pos, na.rm = TRUE)
    max_pos <- max(citations_df$start_pos, na.rm = TRUE)

    position_range <- max_pos - min_pos
    segment_size <- position_range / n_segments

    citations_df$segment <- paste0(
      "Segment ",
      pmin(
        n_segments,
        ceiling((citations_df$start_pos - min_pos) / segment_size)
      )
    )

    citations_df$segment <- ifelse(
      citations_df$start_pos == min_pos,
      "Segment 1",
      citations_df$segment
    )

    citations_df$segment_type <- "equal_length"
  }

  return(citations_df)
}

#' Enhanced scientific content analysis with citation extraction
#'
#' @description
#' Comprehensive analysis of scientific documents including citation extraction,
#' reference matching, text analysis, and bibliometric indicators.
#'
#' @param text Character string or named list. Document text or text with sections.
#' @param doi Character string or NULL. DOI for CrossRef reference retrieval.
#' @param mailto Character string or NULL. Email for CrossRef API.
#' @param openalex_key Character string or NULL. OpenAlex API key. If NULL (default),
#'   the function looks for the \code{OPENALEX_API_KEY} environment variable.
#'   Get a free key at \url{https://openalex.org/}.
#' @param citation_type Character string. Type of citations to extract:
#'   \itemize{
#'     \item "all": Extract all citation types (default)
#'     \item "numeric_superscript": Only numeric citations (brackets and superscript) + narrative
#'     \item "numeric_bracketed": Only bracketed numeric citations + narrative
#'     \item "author_year": Only author-year citations + narrative
#'   }
#' @param window_size Integer. Words before/after citations for context (default: 10).
#' @param min_word_length Integer. Minimum word length for analysis (default: 3).
#' @param remove_stopwords Logical. Remove stopwords (default: TRUE).
#' @param language Character. Language for stopwords (default: "en").
#' @param custom_stopwords Character vector. Additional stopwords.
#' @param ngram_range Integer vector. N-gram range, e.g. c(1,3) (default: c(1,3)).
#' @param parse_multiple_citations Logical. Parse complex citations (default: TRUE).
#' @param use_sections_for_citations Logical or "auto". Use sections for mapping (default: "auto").
#' @param n_segments_citations Integer. Segments if not using sections (default: 10).
#'
#' @return List with class "enhanced_scientific_content_analysis" containing:
#'   \itemize{
#'     \item text_analytics: Basic statistics and word frequencies
#'     \item citations: All extracted citations with metadata
#'     \item citation_contexts: Citations with surrounding text
#'     \item citation_metrics: Citation type distribution, density, etc.
#'     \item citation_references_mapping: Matched citations to references
#'     \item parsed_references: Structured reference list
#'     \item word_frequencies: Word frequency table
#'     \item ngrams: N-gram frequency tables
#'     \item network_data: Citation co-occurrence data
#'     \item summary: Overall analysis summary
#'   }
#'
#' @details
#' This function performs:
#' \itemize{
#'   \item Citation extraction (numbered, author-year, narrative, parenthetical)
#'   \item Reference parsing (from text or CrossRef API)
#'   \item Citation-reference matching
#'   \item Text analysis (word frequencies, n-grams)
#'   \item Citation context extraction
#'   \item Bibliometric indicators
#' }
#'
#' The citation_type parameter filters which citation patterns to search for,
#' reducing false positives. Narrative citations are always included as they
#' are context-dependent.
#'
#' @examples
#' \dontrun{
#' # For documents with numeric citations
#' doc <- pdf2txt_auto("paper.pdf", citation_type = "numeric_bracketed")
#' analysis <- analyze_scientific_content(
#'   doc,
#'   citation_type = "numeric_bracketed",
#'   doi = "10.xxxx/xxxxx",
#'   mailto = "your@email.com"
#' )
#'
#' # For documents with author-year citations
#' doc <- pdf2txt_auto("paper.pdf", citation_type = "author_year")
#' analysis <- analyze_scientific_content(
#'   doc,
#'   citation_type = "author_year"
#' )
#'
#' summary(analysis)
#' head(analysis$citations)
#' table(analysis$citation_metrics$type_distribution)
#' }
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select filter bind_rows arrange desc left_join count rowwise ungroup slice_head rename row_number
#' @importFrom purrr map_lgl
#' @importFrom stringr str_replace_all str_trim str_locate_all str_sub str_detect str_extract str_split str_extract_all str_length str_count str_remove str_squish str_to_title str_to_lower
#' @importFrom tidytext unnest_tokens
#' @importFrom openalexR oa_fetch
analyze_scientific_content <- function(
  text,
  doi = NULL,
  mailto = NULL,
  openalex_key = NULL,
  citation_type = c(
    "all",
    "numeric_superscript",
    "numeric_bracketed",
    "author_year"
  ),
  window_size = 10,
  min_word_length = 3,
  remove_stopwords = TRUE,
  language = "en",
  custom_stopwords = NULL,
  ngram_range = c(1, 3),
  parse_multiple_citations = TRUE,
  use_sections_for_citations = "auto",
  n_segments_citations = 10
) {
  # Configure OpenAlex API key
  # Priority: 1) explicit parameter, 2) OPENALEX_API_KEY env var, 3) openalexR defaults
  if (!is.null(openalex_key) && nchar(openalex_key) > 0) {
    options(openalexR.apikey = openalex_key)
  } else {
    key <- Sys.getenv("OPENALEX_API_KEY", unset = "")
    if (nchar(key) > 0) {
      options(openalexR.apikey = key)
    }
  }

  # Validate citation_type parameter
  citation_type <- match.arg(citation_type)

  results <- list()

  references_section <- NULL

  # if text is not a list transform it in a list
  if (!is.list(text)) {
    text <- list(Full_text = text)
  }

  # if Full_text is present and the list has more than 1 element
  if (length(text) > 1) {
    sections_to_use <- setdiff(names(text), c("Full_text", "References"))
    clean_text <- paste(text[sections_to_use], collapse = " ")
  } else {
    names(text) <- "Full_text"
    clean_text <- text$Full_text
    sections_to_use <- NULL
  }

  if ("References" %in% names(text)) {
    references_section <- text$References
  }

  clean_text <- clean_text %>%
    stringr::str_replace_all("\\n+", " ") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()

  text_stats <- data.frame(
    total_characters = nchar(clean_text),
    total_words = length(stringr::str_split(clean_text, "\\s+")[[1]]),
    total_sentences = length(stringr::str_split(clean_text, "[.!?]+")[[1]]),
    avg_words_per_sentence = length(stringr::str_split(clean_text, "\\s+")[[
      1
    ]]) /
      length(stringr::str_split(clean_text, "[.!?]+")[[1]])
  )

  # Define ALL citation patterns
  all_citation_patterns <- list(
    # Author-year patterns
    complex_multiple_citations = "\\((?:see\\s+)?(?:e\\.g\\.\\s+)?[A-Z][^)]*(?:\\d{4}[a-z]?[;,][^)]*){2,}\\d{4}[a-z]?[^)]*\\)",
    narrative_four_authors_and = "[A-Z][A-Za-z'-]+,\\s*[A-Z][A-Za-z'-]+,\\s*[A-Z][A-Za-z'-]+,\\s*(?:and|&)\\s*[A-Z][A-Za-z'-]+\\s*\\(\\d{4}[a-z]?\\)",
    narrative_three_authors_and = "[A-Z][A-Za-z'-]+,\\s*[A-Z][A-Za-z'-]+,\\s*(?:and|&)\\s*[A-Z][A-Za-z'-]+\\s*\\(\\d{4}[a-z]?\\)",
    narrative_two_authors_and = "[A-Z][A-Za-z'-]+\\s+(?:and|&)\\s+[A-Z][A-Za-z'-]+\\s*\\(\\d{4}[a-z]?\\)",
    narrative_etal = "[A-Z][A-Za-z'-]+(?:\\s*,\\s*[A-Z][A-Za-z'-]+)*(?:\\s*,?\\s*(?:and|&)?\\s*et\\s+al\\.)?\\s*\\(\\d{4}[a-z]?\\)",
    narrative_multiple_authors = "[A-Z][A-Za-z'-]+(?:\\s*,\\s*[A-Z][A-Za-z'-]+)+(?:\\s*,\\s*&\\s*[A-Z][A-Za-z'-]+)?\\s*\\(\\d{4}[a-z]?\\)",
    narrative_single = "[A-Z][A-Za-z'-]+\\s*\\(\\d{4}[a-z]?\\)",
    multiple_citations_semicolon = "\\([A-Z][A-Za-z'-]+[^)]*\\d{4}[a-z]?(?:\\s*;\\s*[A-Z][^)]*\\d{4}[a-z]?)+[^)]*\\)",
    see_citations = "\\((?:see|e\\.g\\.|cf\\.|compare)\\s+[A-Z][A-Za-z'-]+[^)]+\\d{4}[a-z]?\\)",
    author_year_etal = "\\([A-Z][A-Za-z'-]+\\s+et\\s+al\\.,\\s*\\d{4}[a-z]?\\)",
    author_year_and = "\\([A-Z][A-Za-z'-]+(?:,\\s*[A-Z][A-Za-z'-]+)*,?\\s+(?:and|&)\\s+[A-Z][A-Za-z'-]+,\\s*\\d{4}[a-z]?\\)",
    author_year_ampersand = "\\([A-Z][A-Za-z'-]+[^)]*&[^)]*\\d{4}[a-z]?\\)",
    author_year_basic = "\\([A-Z][A-Za-z'-]+(?:\\s+[A-Z][A-Za-z'-]+)*,\\s*\\d{4}[a-z]?\\)",

    # Numeric patterns
    numbered_simple = "\\[\\d+\\]",
    numbered_multiple = "\\[\\d+(?:[-,;\\s]+\\d+)*\\]",
    superscript = "[\u00b9\u00b2\u00b3\u2074\u2075\u2076\u2077\u2078\u2079\u2070]+",

    # Other
    doi_pattern = "https?://doi\\.org/[\\w\\./\\-]+"
  )

  # Filter patterns based on citation_type
  citation_patterns <- list()

  if (citation_type == "all") {
    citation_patterns <- all_citation_patterns
    message("Extracting all citation types")
  } else if (citation_type == "numeric_superscript") {
    # Include numeric patterns + narrative patterns
    citation_patterns <- all_citation_patterns[c(
      "numbered_simple",
      "numbered_multiple",
      "superscript",
      "narrative_four_authors_and",
      "narrative_three_authors_and",
      "narrative_two_authors_and",
      "narrative_etal",
      "narrative_multiple_authors",
      "narrative_single"
    )]
    message(
      "Extracting numeric citations (bracketed and superscript) and narrative citations"
    )
  } else if (citation_type == "numeric_bracketed") {
    # Include only bracketed numeric + narrative patterns
    citation_patterns <- all_citation_patterns[c(
      "numbered_simple",
      "numbered_multiple",
      "narrative_four_authors_and",
      "narrative_three_authors_and",
      "narrative_two_authors_and",
      "narrative_etal",
      "narrative_multiple_authors",
      "narrative_single"
    )]
    message("Extracting bracketed numeric citations and narrative citations")
  } else if (citation_type == "author_year") {
    # Include all author-year patterns (excluding numeric)
    citation_patterns <- all_citation_patterns[
      !names(all_citation_patterns) %in%
        c("numbered_simple", "numbered_multiple", "superscript")
    ]
    message("Extracting author-year citations only")
  }

  # Inizializza con struttura completa
  all_citations <- tibble::tibble(
    citation_type = character(0),
    citation_text = character(0),
    start_pos = integer(0),
    end_pos = integer(0),
    citation_id = character(0)
  )

  for (pattern_name in names(citation_patterns)) {
    pattern <- citation_patterns[[pattern_name]]
    matches <- stringr::str_locate_all(clean_text, pattern)[[1]]

    if (nrow(matches) > 0) {
      citations_temp <- tibble::tibble(
        citation_type = pattern_name,
        citation_text = stringr::str_sub(
          clean_text,
          matches[, 1],
          matches[, 2]
        ),
        start_pos = matches[, 1],
        end_pos = matches[, 2],
        citation_id = paste0(pattern_name, "_", 1:nrow(matches))
      )

      all_citations <- dplyr::bind_rows(all_citations, citations_temp)
    }
  }

  # Remove duplicates
  all_citations <- all_citations %>%
    dplyr::arrange(start_pos, dplyr::desc(nchar(citation_text)))

  if (nrow(all_citations) > 1) {
    to_remove <- c()

    for (i in 1:(nrow(all_citations) - 1)) {
      if (i %in% to_remove) {
        next
      }

      for (j in (i + 1):nrow(all_citations)) {
        if (j %in% to_remove) {
          next
        }

        cite_i_start <- all_citations$start_pos[i]
        cite_i_end <- all_citations$end_pos[i]
        cite_j_start <- all_citations$start_pos[j]
        cite_j_end <- all_citations$end_pos[j]

        if (cite_j_start >= cite_i_start && cite_j_end <= cite_i_end) {
          to_remove <- c(to_remove, j)
        } else if (cite_i_start >= cite_j_start && cite_i_end <= cite_j_end) {
          to_remove <- c(to_remove, i)
          break
        } else if (cite_j_start < cite_i_end && cite_j_start > cite_i_start) {
          len_i <- cite_i_end - cite_i_start
          len_j <- cite_j_end - cite_j_start
          if (len_j < len_i) {
            to_remove <- c(to_remove, j)
          } else {
            to_remove <- c(to_remove, i)
            break
          }
        }
      }
    }

    if (length(to_remove) > 0) {
      all_citations <- all_citations[-unique(to_remove), ]
    }
  }

  all_citations <- all_citations %>% dplyr::arrange(start_pos)

  # Parse complex citations
  parsed_citations <- tibble::tibble()

  if (parse_multiple_citations && nrow(all_citations) > 0) {
    for (i in 1:nrow(all_citations)) {
      citation <- all_citations[i, ]

      # Handle both complex_multiple_citations and multiple_citations_semicolon
      if (
        citation$citation_type %in%
          c("complex_multiple_citations", "multiple_citations_semicolon")
      ) {
        # Remove opening parenthesis and optional prefixes (see, e.g., cf., etc.)
        inner_text <- stringr::str_replace(
          citation$citation_text,
          "^\\((?:see\\s+)?(?:e\\.g\\.\\s+)?(?:cf\\.\\s+)?(?:compare\\s+)?",
          ""
        )
        # Remove closing parenthesis
        inner_text <- stringr::str_replace(inner_text, "\\)$", "")

        # Split by semicolon to get individual citations
        individual_citations <- stringr::str_split(inner_text, "\\s*;\\s*")[[1]]

        # Create a row for each individual citation
        for (j in seq_along(individual_citations)) {
          indiv_cite <- stringr::str_trim(individual_citations[j])

          # Only process if it contains a year (4 digits)
          if (stringr::str_detect(indiv_cite, "\\d{4}")) {
            parsed_row <- tibble::tibble(
              citation_type = "parsed_from_multiple",
              citation_text = paste0("(", indiv_cite, ")"),
              start_pos = citation$start_pos,
              end_pos = citation$end_pos,
              citation_id = paste0("parsed_multiple_", i, "_", j),
              original_complex_citation = citation$citation_text
            )
            parsed_citations <- dplyr::bind_rows(parsed_citations, parsed_row)
          }
        }
      } else {
        # For non-multiple citations, keep them as is
        citation$original_complex_citation <- NA
        parsed_citations <- dplyr::bind_rows(parsed_citations, citation)
      }
    }
    all_citations <- parsed_citations
  }

  # Process narrative citations
  if (nrow(all_citations) > 0) {
    narrative_citations <- all_citations %>%
      dplyr::filter(stringr::str_detect(citation_type, "narrative")) %>%
      dplyr::mutate(
        author_part = stringr::str_extract(citation_text, "^[^(]+"),
        year_part = stringr::str_extract(citation_text, "\\(\\d{4}[a-z]?\\)"),
        standardized_citation = paste0(
          "(",
          stringr::str_trim(author_part),
          ", ",
          stringr::str_replace_all(year_part, "[()]", ""),
          ")"
        )
      )

    if (nrow(narrative_citations) > 0) {
      all_citations <- all_citations %>%
        dplyr::left_join(
          narrative_citations %>%
            dplyr::select(
              citation_id,
              author_part,
              year_part,
              standardized_citation
            ),
          by = "citation_id"
        ) %>%
        dplyr::mutate(
          citation_text_clean = ifelse(
            !is.na(standardized_citation),
            standardized_citation,
            citation_text
          )
        )
    } else {
      all_citations$citation_text_clean <- all_citations$citation_text
      all_citations$author_part <- NA
      all_citations$year_part <- NA
      all_citations$standardized_citation <- NA
    }
  }

  # Word frequency analysis
  tokens <- tibble::tibble(text = clean_text) %>%
    tidytext::unnest_tokens(word, text) %>%
    dplyr::filter(stringr::str_length(word) >= min_word_length) %>%
    dplyr::filter(!stringr::str_detect(word, "^\\d+$"))

  if (remove_stopwords) {
    data(stop_words, package = "tidytext", envir = environment())
    tokens <- tokens %>%
      dplyr::anti_join(stop_words, by = "word")

    if (!is.null(custom_stopwords)) {
      custom_stops <- tibble::tibble(word = custom_stopwords)
      tokens <- tokens %>%
        dplyr::anti_join(custom_stops, by = "word")
    }
  }

  word_frequencies <- tokens %>%
    dplyr::count(word, sort = TRUE) %>%
    dplyr::mutate(
      frequency = n / sum(n),
      rank = dplyr::row_number()
    )

  # N-gram analysis
  ngrams_results <- list()

  clean_text_filtered <- clean_text %>%
    stringr::str_replace_all("\\n+", " ") %>%
    stringr::str_replace_all("\\d+", " ") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()

  if (remove_stopwords) {
    word_tokens <- tibble::tibble(text = clean_text_filtered) %>%
      tidytext::unnest_tokens(word, text, token = "words") %>%
      dplyr::anti_join(stop_words, by = c("word"))
    clean_text_filtered <- word_tokens %>%
      dplyr::pull(word) %>%
      paste(collapse = " ")
  }

  for (n in ngram_range[1]:ngram_range[2]) {
    if (n == 1) {
      ngrams_results[[paste0(n, "gram")]] <- word_frequencies %>%
        dplyr::slice_head(n = 15)
    } else {
      ngram_tokens <- tibble::tibble(text = clean_text_filtered) %>%
        tidytext::unnest_tokens(ngram, text, token = "ngrams", n = n) %>%
        dplyr::filter(!is.na(ngram)) %>%
        dplyr::count(ngram, sort = TRUE) %>%
        dplyr::slice_head(n = 15) %>%
        dplyr::mutate(frequency = n / sum(n))

      ngrams_results[[paste0(n, "gram")]] <- ngram_tokens
    }
  }

  # Map citations to segments/sections
  if (nrow(all_citations) > 0) {
    all_citations <- map_citations_to_segments(
      citations_df = all_citations,
      text = if (is.list(text)) text else list(Full_text = text),
      use_sections = use_sections_for_citations,
      n_segments = n_segments_citations
    )

    all_citations <- all_citations %>%
      dplyr::rename(section = segment)
  } else {
    all_citations$section <- "Full_text"
    all_citations$segment_type <- "unknown"
  }

  # Citation metrics
  citation_metrics <- list()

  if (nrow(all_citations) > 0) {
    citation_metrics$type_distribution <- all_citations %>%
      dplyr::count(citation_type, sort = TRUE) %>%
      dplyr::mutate(percentage = round(n / sum(n) * 100, 2))

    if (!is.null(sections_to_use) && length(sections_to_use) > 0) {
      citation_metrics$section_distribution <- all_citations %>%
        dplyr::mutate(section = factor(section, levels = sections_to_use)) %>%
        dplyr::count(section, sort = FALSE, .drop = FALSE) %>%
        dplyr::mutate(percentage = round(n / sum(n) * 100, 2))
    } else {
      citation_metrics$section_distribution <- all_citations %>%
        dplyr::count(section, sort = TRUE) %>%
        dplyr::mutate(percentage = round(n / sum(n) * 100, 2))
    }

    citation_metrics$narrative_ratio <- all_citations %>%
      dplyr::summarise(
        total_citations = dplyr::n(),
        narrative_citations = sum(stringr::str_detect(
          citation_type,
          "narrative"
        )),
        parenthetical_citations = sum(
          !stringr::str_detect(citation_type, "narrative")
        ),
        narrative_percentage = round(
          narrative_citations / total_citations * 100,
          2
        )
      )

    if (parse_multiple_citations) {
      citation_metrics$complex_citations <- all_citations %>%
        dplyr::filter(citation_type == "parsed_from_multiple") %>%
        dplyr::count(original_complex_citation, sort = TRUE) %>%
        dplyr::rename(individual_citations_extracted = n)
    }

    citation_metrics$density <- list(
      citations_per_1000_words = round(
        (nrow(all_citations) / text_stats$total_words) * 1000,
        2
      ),
      avg_words_between_citations = if (nrow(all_citations) > 1) {
        round(text_stats$total_words / nrow(all_citations), 2)
      } else {
        text_stats$total_words
      }
    )
  }

  # Citation co-occurrence
  citation_cooccurrence <- NULL

  if (nrow(all_citations) > 1) {
    citation_pairs <- tibble::tibble()

    for (i in 1:(nrow(all_citations) - 1)) {
      for (j in (i + 1):nrow(all_citations)) {
        distance <- all_citations$start_pos[j] - all_citations$end_pos[i]
        if (distance <= 1000) {
          pair <- tibble::tibble(
            citation1 = all_citations$citation_text_clean[i],
            citation2 = all_citations$citation_text_clean[j],
            distance = distance,
            type1 = all_citations$citation_type[i],
            type2 = all_citations$citation_type[j]
          )
          citation_pairs <- dplyr::bind_rows(citation_pairs, pair)
        }
      }
    }

    citation_cooccurrence <- citation_pairs
  }

  # Citation contexts
  citation_contexts <- tibble::tibble()

  if (nrow(all_citations) > 0) {
    for (i in 1:nrow(all_citations)) {
      citation <- all_citations[i, ]
      citation_start <- citation$start_pos
      citation_end <- citation$end_pos

      text_before_citation <- substr(clean_text, 1, citation_start - 1)
      text_after_citation <- substr(
        clean_text,
        citation_end + 1,
        nchar(clean_text)
      )

      words_before_df <- tibble::tibble(text = text_before_citation) %>%
        tidytext::unnest_tokens(word, text, token = "words", to_lower = FALSE)

      n_words_before <- nrow(words_before_df)
      if (n_words_before > 0) {
        start_idx <- max(1, n_words_before - window_size + 1)
        words_before <- words_before_df %>%
          dplyr::slice(start_idx:n_words_before) %>%
          dplyr::pull(word) %>%
          paste(collapse = " ")
      } else {
        words_before <- ""
      }

      next_citations <- all_citations %>%
        dplyr::filter(start_pos > citation_end) %>%
        dplyr::arrange(start_pos)

      if (nrow(next_citations) > 0) {
        next_citation_start <- next_citations$start_pos[1]
        max_end_pos <- next_citation_start - citation_end - 1

        if (max_end_pos > 0) {
          text_after_limited <- substr(text_after_citation, 1, max_end_pos)
        } else {
          text_after_limited <- ""
        }
      } else {
        text_after_limited <- text_after_citation
      }

      words_after_df <- tibble::tibble(text = text_after_limited) %>%
        tidytext::unnest_tokens(word, text, token = "words", to_lower = FALSE)

      n_words_after <- nrow(words_after_df)
      if (n_words_after > 0) {
        end_idx <- min(window_size, n_words_after)
        words_after <- words_after_df %>%
          dplyr::slice(1:end_idx) %>%
          dplyr::pull(word) %>%
          paste(collapse = " ")
      } else {
        words_after <- ""
      }

      full_context <- paste(
        words_before,
        citation$citation_text,
        words_after
      ) %>%
        stringr::str_trim()

      context_word_count <- n_words_before +
        n_words_after +
        length(strsplit(citation$citation_text, "\\s+")[[1]])

      context_row <- tibble::tibble(
        citation_id = citation$citation_id,
        citation_text = citation$citation_text,
        citation_text_clean = citation$citation_text_clean,
        citation_type = citation$citation_type,
        section = citation$section,
        words_before = words_before,
        words_after = words_after,
        full_context = full_context,
        context_word_count = context_word_count,
        citation_position_in_text = citation$start_pos,
        is_narrative = stringr::str_detect(citation$citation_type, "narrative"),
        is_parsed_multiple = citation$citation_type == "parsed_from_multiple"
      )

      if (
        "original_complex_citation" %in%
          names(citation) &&
          !is.na(citation$original_complex_citation)
      ) {
        context_row$original_complex_citation <- citation$original_complex_citation
      }

      citation_contexts <- dplyr::bind_rows(citation_contexts, context_row)
    }
  }

  # Citation-reference mapping
  citation_references_mapping <- NULL
  parsed_references <- NULL
  references_oa <- NULL
  if (is.null(mailto)) {
    mailto <- Sys.getenv("CROSSREF_MAILTO")
    if (mailto == "") {
      mailto <- "your@email.com"
    }
  }

  if (!is.null(doi)) {
    tryCatch(
      {
        message("Attempting to retrieve references from CrossRef...")
        refs_crossref <- get_crossref_references(doi, mailto)

        if (!is.null(refs_crossref) && nrow(refs_crossref) > 0) {
          parsed_references <- refs_crossref %>%
            dplyr::mutate(
              ref_id = paste0("REF_", row_number()),
              ref_full_text = ifelse(!is.na(ref_full), ref_full, NA_character_),
              ref_full_text2 = paste(
                ifelse(!is.na(author), paste0(author, ","), ""),
                ifelse(!is.na(year), paste0("(", year, "),"), ""),
                ifelse(!is.na(article_title), paste0(article_title, "."), ""),
                ifelse(!is.na(journal), paste0(journal, "."), ""),
                sep = " "
              ) %>%
                stringr::str_trim() %>%
                stringr::str_squish(),
              ref_first_author = stringr::str_remove_all(
                author,
                "\\b[A-Z]+\\b\\s*"
              ) %>%
                str_trim() %>%
                stringr::str_to_title(),
              ref_first_author_normalized = stringr::str_to_lower(
                ref_first_author
              ),
              ref_year = as.character(year),
              ref_authors = author,
              ref_journal = journal,
              ref_source = "crossref",
              n_authors = NA_integer_
            ) %>%
            dplyr::select(
              ref_id,
              ref_full_text,
              ref_authors,
              ref_year,
              ref_journal,
              ref_first_author,
              ref_first_author_normalized,
              n_authors,
              doi,
              ref_full_text2,
              ref_source
            )
          string_authors <- str_extract(
            parsed_references$ref_full_text,
            ".*?(?=\\s*\\()"
          )
          parsed_references$n_authors <- as.integer(
            (str_count(string_authors, ",") + 1) / 2
          )

          message(paste(
            "Successfully retrieved",
            nrow(parsed_references),
            "references from CrossRef"
          ))

          # download metadata from openalex
          dois <- parsed_references$doi[!is.na(parsed_references$doi)]
          dois <- unique(dois)
          dois <- tolower(trimws(dois[dois != ""]))

          message(paste(
            "Fetching Open Access metadata for",
            length(dois),
            "DOIs from OpenAlex..."
          ))

          if (!is.null(dois) && length(dois) > 0) {
            references_oa <- safe_oa_fetch(
              entity = "works",
              doi = dois
            )
          }

          if (!is.null(references_oa)) {
            references_oa <- add_reference_info(references_oa)
            parsed_references <- complete_references_from_oa(
              parsed_references,
              references_oa,
              verbose = FALSE
            ) %>%
              # replace ref_full_text with ref_full_text2 when present in OA
              mutate(
                ref_full_text = ifelse(
                  is.na(ref_full_text) | ref_full_text == "",
                  ref_full_text2,
                  ref_full_text
                )
              )
            message(paste(
              "Successfully retrieved metadata for",
              nrow(references_oa),
              "references from OpenAlex"
            ))
          }
        }
      },
      error = function(e) {
        warning(paste("Failed to retrieve from CrossRef:", e$message))
      }
    )
  }

  if (
    is.null(parsed_references) &&
      !is.null(references_section) &&
      references_section != ""
  ) {
    message("Parsing references from text...")
    parsed_references <- parse_references_section(references_section) %>%
      mutate(ref_source = "parsed", doi = NA_character_)
  }

  if (
    !is.null(parsed_references) &&
      nrow(parsed_references) > 0 &&
      nrow(all_citations) > 0
  ) {
    if (citation_type %in% c("numeric_superscript", "numeric_brackets")) {
      ## remove citation with an associated numeric label grater then the maximum
      maximum_label <- nrow(parsed_references)
      # remove all citation with label greater than maximum_label from all_citations data frame

      # Extract numbers from citation_text_clean e filter only numbered_ type citations
      all_citations <- all_citations %>%
        mutate(
          citation_numbers = str_extract_all(citation_text_clean, "\\d+")
        ) %>%
        dplyr::filter(
          !(str_detect(citation_type, "^numbered_") &
            map_lgl(citation_numbers, ~ any(as.numeric(.x) > maximum_label)))
        ) %>%
        select(-citation_numbers)
    }

    citation_references_mapping <- match_citations_to_references(
      citations_df = all_citations %>%
        dplyr::select(
          citation_id,
          citation_text,
          citation_text_clean,
          citation_type
        ),
      references_df = parsed_references
    )
  }

  # Add ref_full_text to citation_contexts
  if (!is.null(citation_references_mapping) && nrow(citation_contexts) > 0) {
    citation_contexts <- citation_contexts %>%
      dplyr::left_join(
        citation_references_mapping %>%
          dplyr::select(
            citation_id,
            matched_ref_id,
            ref_full_text,
            match_confidence
          ),
        by = "citation_id"
      )
  }

  # Compile results
  results$text_analytics <- list(
    basic_stats = text_stats,
    total_citations_found = nrow(all_citations),
    citation_types_found = unique(all_citations$citation_type),
    most_frequent_words = word_frequencies %>% dplyr::slice_head(n = 20)
  )

  results$citations <- all_citations
  results$citation_contexts <- citation_contexts
  results$citation_metrics <- citation_metrics
  results$citation_references_mapping <- citation_references_mapping
  results$parsed_references <- parsed_references
  results$word_frequencies <- word_frequencies
  results$ngrams <- ngrams_results
  results$network_data <- citation_cooccurrence
  results$references_oa <- references_oa

  # Handle section colors safely - check if citation_contexts has data and section column
  if (nrow(citation_contexts) > 0 && "section" %in% names(citation_contexts)) {
    section_values <- unique(citation_contexts$section)
    section_colors <- colorlist()[1:length(section_values)]
    names(section_colors) <- section_values
  } else {
    # Fallback when there are no citations or sections
    section_colors <- c("Full_text" = colorlist()[1])
  }
  results$section_colors <- section_colors

  results$summary <- list(
    total_words_analyzed = nrow(tokens),
    unique_words = nrow(word_frequencies),
    citations_extracted = nrow(all_citations),
    narrative_citations = sum(
      stringr::str_detect(all_citations$citation_type, "narrative"),
      na.rm = TRUE
    ),
    parenthetical_citations = sum(
      !stringr::str_detect(all_citations$citation_type, "narrative"),
      na.rm = TRUE
    ),
    complex_citations_parsed = sum(
      all_citations$citation_type == "parsed_from_multiple",
      na.rm = TRUE
    ),
    lexical_diversity = nrow(word_frequencies) / nrow(tokens),
    average_citation_context_length = if (nrow(citation_contexts) > 0) {
      mean(citation_contexts$context_word_count)
    } else {
      0
    },
    citation_density_per_1000_words = if (nrow(all_citations) > 0) {
      round((nrow(all_citations) / text_stats$total_words) * 1000, 2)
    } else {
      0
    },
    references_parsed = if (!is.null(parsed_references)) {
      nrow(parsed_references)
    } else {
      0
    },
    citations_matched_to_refs = if (!is.null(citation_references_mapping)) {
      sum(
        citation_references_mapping$match_confidence %in%
          c(
            "high",
            "high_second_author",
            "medium_multiple_matches",
            "medium_fuzzy"
          ),
        na.rm = TRUE
      )
    } else {
      0
    },
    match_quality = if (!is.null(citation_references_mapping)) {
      citation_references_mapping %>%
        dplyr::count(match_confidence) %>%
        dplyr::mutate(percentage = round(n / sum(n) * 100, 1))
    } else {
      NULL
    },
    citation_type_used = citation_type
  )

  class(results) <- c("enhanced_scientific_content_analysis", "list")
  return(results)
}

# color palette
colorlist <- function() {
  c(
    "#E41A1C",
    "#377EB8",
    "#4DAF4A",
    "#984EA3",
    "#FF7F00",
    "#A65628",
    "#F781BF",
    "#999999",
    "#66C2A5",
    "#FC8D62",
    "#8DA0CB",
    "#E78AC3",
    "#A6D854",
    "#FFD92F",
    "#B3B3B3",
    "#A6CEE3",
    "#1F78B4",
    "#B2DF8A",
    "#33A02C",
    "#FB9A99",
    "#E31A1C",
    "#FDBF6F",
    "#FF7F00",
    "#CAB2D6",
    "#6A3D9A",
    "#B15928",
    "#8DD3C7",
    "#BEBADA",
    "#FB8072",
    "#80B1D3",
    "#FDB462",
    "#B3DE69",
    "#D9D9D9",
    "#BC80BD",
    "#CCEBC5"
  )
}

safe_oa_fetch <- function(
  entity,
  identifier = NULL,
  doi = NULL,
  attempt = 1,
  sleep_time = 1,
  retry_delay = 2,
  max_retries = 3,
  verbose = FALSE
) {
  if (attempt > 1 && verbose) {
    cat("Retry attempt", attempt, "of", max_retries, "\n")
  }

  # Add delay before API call (except for first attempt)
  if (attempt > 1 || entity == "authors") {
    wait_time <- if (attempt == 1) {
      sleep_time
    } else {
      retry_delay * (2^(attempt - 2))
    }
    if (verbose) {
      cat("Waiting", round(wait_time, 2), "seconds before API call...\n")
    }
    Sys.sleep(wait_time)
  }

  result <- tryCatch(
    {
      if (!is.null(identifier)) {
        openalexR::oa_fetch(
          entity = entity,
          identifier = identifier,
          output = "tibble"
        )
      } else if (!is.null(doi)) {
        openalexR::oa_fetch(
          entity = entity,
          doi = doi,
          output = "tibble"
        )
      }
    },
    error = function(e) {
      error_msg <- as.character(e$message)

      # Check if it's a rate limit error (429)
      if (
        grepl("429", error_msg) ||
          grepl("Too Many Requests", error_msg, ignore.case = TRUE)
      ) {
        if (attempt < max_retries) {
          if (verbose) {
            cat(
              "Rate limit hit (HTTP 429). Retrying with exponential backoff...\n"
            )
          }
          return(safe_oa_fetch(
            entity,
            identifier,
            doi,
            attempt + 1,
            sleep_time,
            retry_delay,
            max_retries,
            verbose
          ))
        } else {
          stop(
            "Rate limit exceeded after ",
            max_retries,
            " attempts. ",
            "Please wait a few minutes or set an OpenAlex API key for higher rate limits. ",
            "Get a free key at: https://openalex.org/"
          )
        }
      }

      # Check for other temporary errors
      if (
        grepl("500|502|503|504", error_msg) ||
          grepl("timeout", error_msg, ignore.case = TRUE)
      ) {
        if (attempt < max_retries) {
          if (verbose) {
            cat("Temporary server error. Retrying...\n")
          }
          return(safe_oa_fetch(
            entity,
            identifier,
            doi,
            attempt + 1,
            sleep_time,
            retry_delay,
            max_retries,
            verbose
          ))
        }
      }

      # If not a retryable error, or max retries reached, throw the error
      stop(e$message)
    }
  )

  return(result)
}

add_reference_info <- function(df) {
  # Helper function to normalize names for comparison
  # Removes accents, diacritics, and converts to lowercase
  normalize_name <- function(name) {
    if (is.na(name) || name == "") {
      return("")
    }

    name <- tolower(trimws(name))

    # a variants
    name <- gsub(
      "[\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u0101\u0103\u0105]",
      "a",
      name
    )
    # e variants
    name <- gsub("[\u00e8\u00e9\u00ea\u00eb\u0113\u0117\u0119]", "e", name)
    # i variants
    name <- gsub("[\u00ec\u00ed\u00ee\u00ef\u012b\u012f\u0131]", "i", name)
    # o variants
    name <- gsub(
      "[\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8\u014d\u0151]",
      "o",
      name
    )
    # u variants
    name <- gsub("[\u00f9\u00fa\u00fb\u00fc\u016b\u0171\u0173]", "u", name)
    # y variants
    name <- gsub("[\u00fd\u00ff]", "y", name)
    # n variants
    name <- gsub("[\u00f1\u0144]", "n", name)
    # c variants
    name <- gsub("[\u00e7\u0107\u010d]", "c", name)
    # s variants
    name <- gsub("[\u015b\u0161\u015f]", "s", name)
    # z variants
    name <- gsub("[\u017a\u017c\u017e]", "z", name)
    # Special characters
    name <- gsub("\u00df", "ss", name)
    name <- gsub("\u00e6", "ae", name)
    name <- gsub("\u0153", "oe", name)
    name <- gsub("\u0142", "l", name)
    name <- gsub("\u0111", "d", name)

    return(name)
  }

  # Helper function to extract initials from given names
  get_initial <- function(full_name) {
    if (is.na(full_name) || full_name == "") {
      return("")
    }

    parts <- strsplit(full_name, " ")[[1]]

    if (length(parts) <= 1) {
      return("")
    }

    # Common surname prefixes in various languages
    surname_prefixes <- c(
      "van",
      "von",
      "de",
      "di",
      "da",
      "del",
      "della",
      "le",
      "la",
      "du",
      "des",
      "van der",
      "van den",
      "von der",
      "von dem",
      "de la",
      "de las",
      "de los"
    )

    # Normalize parts for comparison
    parts_normalized <- sapply(parts, normalize_name)

    # Find where surname starts
    surname_start <- length(parts) # Default: last word only

    for (i in 1:(length(parts) - 1)) {
      # Check single word prefix
      if (parts_normalized[i] %in% surname_prefixes) {
        surname_start <- i
        break
      }

      # Check two-word prefix (e.g., "van der")
      if (i < length(parts) - 1) {
        two_word <- paste(parts_normalized[i], parts_normalized[i + 1])
        if (two_word %in% surname_prefixes) {
          surname_start <- i
          break
        }
      }
    }

    # Extract given names (everything before surname)
    if (surname_start > 1) {
      first_names <- parts[1:(surname_start - 1)]
      initials <- sapply(first_names, function(x) substr(x, 1, 1))
      return(paste0(initials, collapse = "."))
    }

    return("")
  }

  # Helper function to extract surname with prefix support
  get_surname <- function(full_name) {
    if (is.na(full_name) || full_name == "") {
      return(NA)
    }

    parts <- strsplit(full_name, " ")[[1]]

    if (length(parts) == 1) {
      return(parts[1])
    }

    # Common surname prefixes in various languages
    surname_prefixes <- c(
      "van",
      "von",
      "de",
      "di",
      "da",
      "del",
      "della",
      "le",
      "la",
      "du",
      "des",
      "van der",
      "van den",
      "von der",
      "von dem",
      "de la",
      "de las",
      "de los"
    )

    # Normalize parts for comparison (but keep original for output)
    parts_normalized <- sapply(parts, normalize_name)

    # Find if any surname prefix exists
    for (i in 1:(length(parts) - 1)) {
      # Check single word prefix
      if (parts_normalized[i] %in% surname_prefixes) {
        # Surname starts from this prefix (return ORIGINAL, not normalized)
        return(paste(parts[i:length(parts)], collapse = " "))
      }

      # Check two-word prefix (e.g., "van der")
      if (i < length(parts) - 1) {
        two_word <- paste(parts_normalized[i], parts_normalized[i + 1])
        if (two_word %in% surname_prefixes) {
          # Return ORIGINAL, not normalized
          return(paste(parts[i:length(parts)], collapse = " "))
        }
      }
    }

    # If no prefix found, return only the last word
    return(parts[length(parts)])
  }

  # Helper function to create the authors string
  create_authors_string <- function(authorships) {
    if (is.null(authorships) || nrow(authorships) == 0) {
      return("Unknown")
    }

    authors <- authorships$display_name
    n_authors <- length(authors)

    # Format each author as "Surname, I."
    formatted_authors <- sapply(authors, function(author) {
      surname <- get_surname(author)
      initial <- get_initial(author)
      if (initial != "") {
        return(paste0(surname, ", ", initial, "."))
      } else {
        return(surname)
      }
    })

    # Concatenate authors with commas
    return(paste(formatted_authors, collapse = ", "))
  }

  # Helper function to count authors
  count_authors <- function(authorships) {
    if (is.null(authorships) || nrow(authorships) == 0) {
      return(0)
    }
    return(nrow(authorships))
  }

  # Add n_authors column
  df$n_authors <- sapply(df$authorships, count_authors)

  # Add ref_full_name column
  df$ref_full_name <- mapply(
    function(authorships, year, title, source) {
      authors_str <- create_authors_string(authorships)

      # Handle missing values
      year_str <- ifelse(is.na(year), "n.d.", as.character(year))
      title_str <- ifelse(is.na(title) || title == "", "Untitled", title)
      source_str <- ifelse(
        is.na(source) || source == "",
        "Unknown source",
        source
      )

      # Create the complete reference
      paste0(
        authors_str,
        " (",
        year_str,
        ") ",
        title_str,
        ", ",
        source_str,
        "."
      )
    },
    df$authorships,
    df$publication_year,
    df$title,
    df$source_display_name,
    SIMPLIFY = TRUE
  )

  return(df)
}

#' Complete references from OpenAlex with intelligent conflict resolution
#'
#' Merges reference data from CrossRef and OpenAlex, using OpenAlex data when
#' available unless there is a REAL conflict in the first author name.
#' Only genuine author conflicts (different people) trigger CrossRef precedence.
#' Normalization differences (prefixes, accents, typos) are NOT considered conflicts.
#'
#' @param references Data frame with references from CrossRef
#' @param references_oa Data frame with references from OpenAlex
#' @param verbose Logical, whether to print conflict messages. Default TRUE
#' @return Updated references data frame
#' @keywords internal
#' @importFrom dplyr mutate
complete_references_from_oa <- function(
  references,
  references_oa,
  verbose = TRUE
) {
  # Helper function to normalize names for comparison
  # Removes accents, diacritics, and converts to lowercase
  normalize_name <- function(name) {
    if (is.na(name) || name == "") {
      return("")
    }

    name <- tolower(trimws(name))

    # a variants
    name <- gsub(
      "[\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u0101\u0103\u0105]",
      "a",
      name
    )
    # e variants
    name <- gsub("[\u00e8\u00e9\u00ea\u00eb\u0113\u0117\u0119]", "e", name)
    # i variants
    name <- gsub("[\u00ec\u00ed\u00ee\u00ef\u012b\u012f\u0131]", "i", name)
    # o variants
    name <- gsub(
      "[\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8\u014d\u0151]",
      "o",
      name
    )
    # u variants
    name <- gsub("[\u00f9\u00fa\u00fb\u00fc\u016b\u0171\u0173]", "u", name)
    # y variants
    name <- gsub("[\u00fd\u00ff]", "y", name)
    # n variants
    name <- gsub("[\u00f1\u0144]", "n", name)
    # c variants
    name <- gsub("[\u00e7\u0107\u010d]", "c", name)
    # s variants
    name <- gsub("[\u015b\u0161\u015f]", "s", name)
    # z variants
    name <- gsub("[\u017a\u017c\u017e]", "z", name)
    # Special characters
    name <- gsub("\u00df", "ss", name)
    name <- gsub("\u00e6", "ae", name)
    name <- gsub("\u0153", "oe", name)
    name <- gsub("\u0142", "l", name)
    name <- gsub("\u0111", "d", name)

    return(name)
  }

  # Helper function to extract surname with prefix support
  get_surname <- function(full_name) {
    if (is.na(full_name) || full_name == "") {
      return(NA)
    }

    parts <- strsplit(full_name, " ")[[1]]

    if (length(parts) == 1) {
      return(parts[1])
    }

    # Common surname prefixes in various languages
    surname_prefixes <- c(
      "van",
      "von",
      "de",
      "di",
      "da",
      "del",
      "della",
      "le",
      "la",
      "du",
      "des",
      "van der",
      "van den",
      "von der",
      "von dem",
      "de la",
      "de las",
      "de los"
    )

    # Normalize parts for comparison (but keep original for output)
    parts_normalized <- sapply(parts, normalize_name)

    # Find if any surname prefix exists
    for (i in 1:(length(parts) - 1)) {
      # Check single word prefix
      if (parts_normalized[i] %in% surname_prefixes) {
        # Surname starts from this prefix (return ORIGINAL, not normalized)
        return(paste(parts[i:length(parts)], collapse = " "))
      }

      # Check two-word prefix (e.g., "van der")
      if (i < length(parts) - 1) {
        two_word <- paste(parts_normalized[i], parts_normalized[i + 1])
        if (two_word %in% surname_prefixes) {
          # Return ORIGINAL, not normalized
          return(paste(parts[i:length(parts)], collapse = " "))
        }
      }
    }

    # If no prefix found, return only the last word
    return(parts[length(parts)])
  }

  # Helper function to extract initials from first names
  get_initial <- function(full_name) {
    if (is.na(full_name) || full_name == "") {
      return("")
    }

    parts <- strsplit(full_name, " ")[[1]]

    if (length(parts) <= 1) {
      return("")
    }

    # Common surname prefixes in various languages
    surname_prefixes <- c(
      "van",
      "von",
      "de",
      "di",
      "da",
      "del",
      "della",
      "le",
      "la",
      "du",
      "des",
      "van der",
      "van den",
      "von der",
      "von dem",
      "de la",
      "de las",
      "de los"
    )

    # Normalize parts for comparison
    parts_normalized <- sapply(parts, normalize_name)

    # Find where surname starts
    surname_start <- length(parts) # Default: last word only

    for (i in 1:(length(parts) - 1)) {
      # Check single word prefix
      if (parts_normalized[i] %in% surname_prefixes) {
        surname_start <- i
        break
      }

      # Check two-word prefix (e.g., "van der")
      if (i < length(parts) - 1) {
        two_word <- paste(parts_normalized[i], parts_normalized[i + 1])
        if (two_word %in% surname_prefixes) {
          surname_start <- i
          break
        }
      }
    }

    # Extract given names (everything before surname)
    if (surname_start > 1) {
      first_names <- parts[1:(surname_start - 1)]
      initials <- sapply(first_names, function(x) substr(x, 1, 1))
      return(paste0(initials, collapse = "."))
    }

    return("")
  }

  # Helper function to format authors list
  format_authors <- function(authorships) {
    if (is.null(authorships) || nrow(authorships) == 0) {
      return(NA)
    }

    authors <- authorships$display_name
    formatted_authors <- sapply(authors, function(author) {
      surname <- get_surname(author)
      initial <- get_initial(author)
      if (!is.na(initial) && initial != "") {
        return(paste0(surname, ", ", initial, "."))
      } else {
        return(surname)
      }
    })

    return(paste(formatted_authors, collapse = ", "))
  }

  # Helper function to get first author surname
  get_first_author <- function(authorships) {
    if (is.null(authorships) || nrow(authorships) == 0) {
      return(NA)
    }
    return(get_surname(authorships$display_name[1]))
  }

  # Normalize DOIs for matching
  references$doi_clean <- tolower(trimws(gsub(
    "https://doi.org/",
    "",
    references$doi
  )))
  references_oa$doi_clean <- tolower(trimws(gsub(
    "https://doi.org/",
    "",
    references_oa$doi
  )))

  # Loop through each reference
  for (i in 1:nrow(references)) {
    doi_ref <- references$doi_clean[i]

    if (is.na(doi_ref) || doi_ref == "") {
      next
    }

    # Find match in OpenAlex
    match_idx <- which(references_oa$doi_clean == doi_ref)

    if (length(match_idx) == 0) {
      next
    }

    match_idx <- match_idx[1] # Take first match if multiple

    # ============================================
    # Extract data from OpenAlex
    # ============================================
    oa_full_text <- references_oa$ref_full_name[match_idx]
    oa_authors <- format_authors(references_oa$authorships[[match_idx]])
    oa_year <- as.character(references_oa$publication_year[match_idx])
    oa_journal <- references_oa$source_display_name[match_idx]
    oa_first_author <- get_first_author(references_oa$authorships[[match_idx]])
    oa_n_authors <- references_oa$n_authors[match_idx]

    # ============================================
    # Check for author conflict using intelligent matching
    # ============================================
    crossref_first_author <- references$ref_first_author_normalized[i]

    # Check if there's a real conflict
    conflict_check <- check_author_conflict(
      crossref_first_author,
      oa_first_author,
      doi = doi_ref,
      verbose = verbose
    )

    # ============================================
    # Decision logic
    # ============================================
    use_openalex <- FALSE

    if (is.na(crossref_first_author) || crossref_first_author == "") {
      # CASE 1: CrossRef has no first author -> always use OpenAlex
      use_openalex <- TRUE
    } else if (!conflict_check$is_real_conflict) {
      # CASE 2: No real conflict (same author or normalization difference) -> use OpenAlex
      use_openalex <- TRUE
    } else {
      # CASE 3: Real conflict (different authors) -> keep CrossRef
      use_openalex <- FALSE
    }

    # ============================================
    # Apply the decision
    # ============================================
    if (use_openalex) {
      # Use OpenAlex data (more complete and accurate)
      if (!is.na(oa_full_text) && oa_full_text != "") {
        references$ref_full_text[i] <- oa_full_text
      }

      if (!is.na(oa_authors) && oa_authors != "") {
        references$ref_authors[i] <- oa_authors
      }

      if (!is.na(oa_year) && oa_year != "") {
        references$ref_year[i] <- oa_year
      }

      if (!is.na(oa_journal) && oa_journal != "") {
        references$ref_journal[i] <- oa_journal
      }

      if (!is.na(oa_first_author) && oa_first_author != "") {
        references$ref_first_author[i] <- oa_first_author
        # Normalize the first author name for comparison
        references$ref_first_author_normalized[i] <- normalize_name(
          oa_first_author
        )
      }

      if (!is.na(oa_n_authors)) {
        references$n_authors[i] <- oa_n_authors
      }
    } else {
      # Keep CrossRef, but fill in missing fields from OpenAlex
      if (is.na(references$ref_year[i]) || references$ref_year[i] == "") {
        if (!is.na(oa_year) && oa_year != "") {
          references$ref_year[i] <- oa_year
        }
      }

      if (is.na(references$ref_journal[i]) || references$ref_journal[i] == "") {
        if (!is.na(oa_journal) && oa_journal != "") {
          references$ref_journal[i] <- oa_journal
        }
      }

      if (is.na(references$n_authors[i])) {
        if (!is.na(oa_n_authors)) {
          references$n_authors[i] <- oa_n_authors
        }
      }
    }

    # Always save ref_full_text2 from OpenAlex for reference
    references$ref_full_text2[i] <- oa_full_text
  }

  # Clean up temporary column
  references$doi_clean <- NULL

  return(references)
}

# Helper function to extract first name initials
get_initial <- function(full_name) {
  if (is.na(full_name) || full_name == "") {
    return("")
  }
  parts <- strsplit(full_name, " ")[[1]]
  if (length(parts) <= 1) {
    return("")
  }
  first_names <- parts[-length(parts)]
  initials <- sapply(first_names, function(x) substr(x, 1, 1))
  return(paste0(initials, collapse = "."))
}

# Helper function to format authors
format_authors <- function(authorships) {
  if (is.null(authorships) || nrow(authorships) == 0) {
    return(NA)
  }

  authors <- authorships$display_name
  formatted_authors <- sapply(authors, function(author) {
    surname <- get_surname(author)
    initial <- get_initial(author)
    if (!is.na(initial) && initial != "") {
      return(paste0(surname, ", ", initial, "."))
    } else {
      return(surname)
    }
  })

  return(paste(formatted_authors, collapse = ", "))
}

# Helper function to extract the first author
get_first_author <- function(authorships) {
  if (is.null(authorships) || nrow(authorships) == 0) {
    return(NA)
  }
  return(get_surname(authorships$display_name[1]))
}
