#' Match citations to references
#'
#' @description
#' Matches in-text citations to entries in the reference list using author-year
#' matching with multiple disambiguation strategies.
#'
#' @param citations_df Data frame with citation information, must include:
#'   citation_id, citation_text, citation_text_clean, citation_type
#' @param references_df Data frame with parsed references from parse_references_section()
#'
#' @return Tibble with matched citations including columns:
#'   \itemize{
#'     \item citation_id: Citation identifier
#'     \item citation_text: Original citation text
#'     \item citation_text_clean: Cleaned citation text
#'     \item citation_type: Type of citation
#'     \item cite_author: Extracted first author from citation
#'     \item cite_second_author: Second author (if present)
#'     \item cite_year: Extracted year
#'     \item cite_has_etal: Logical, contains "et al."
#'     \item matched_ref_id: ID of matched reference
#'     \item ref_full_text: Full text of matched reference
#'     \item ref_authors: Authors from reference
#'     \item ref_year: Year from reference
#'     \item match_confidence: Quality of match (high, medium, low, no_match)
#'   }
#'
#' @details
#' Matching algorithm:
#' \enumerate{
#'   \item Filter by exact year match
#'   \item Match first author (exact, then fuzzy)
#'   \item Disambiguate using second author or et al. heuristics
#' }
#'
#' Match confidence levels include:
#' high (exact first author + year),
#' high_second_author (disambiguated with second author),
#' medium_multiple_matches, medium_fuzzy, medium_etal_heuristic (various medium confidence scenarios),
#' no_match_year, no_match_author, no_match_missing_info (no suitable reference found).
#'
#' @examples
#' \dontrun{
#' matched <- match_citations_to_references(citations_df, references_df)
#' table(matched$match_confidence)
#' }
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select filter bind_rows rowwise ungroup arrange desc
#' @importFrom stringr str_extract str_replace_all str_detect str_count str_to_lower str_replace str_trim
match_citations_to_references <- function(citations_df, references_df) {
  if (nrow(citations_df) == 0 || nrow(references_df) == 0) {
    return(tibble::tibble(
      citation_id = character(),
      citation_text = character(),
      citation_text_clean = character(),
      citation_type = character(),
      matched_ref_id = character(),
      ref_full_text = character(),
      match_confidence = character(),
      ref_authors = character(),
      ref_year = character()
    ))
  }

  # === FUNCTION TO EXTRACT SURNAME ===
  # CrossRef returns authors in variable formats:
  # - "Surname, I." (e.g., "Zhao, D.")
  # - "I Surname" or "II Surname" (e.g., "DM Blei")
  # This function extracts only the surname
  extract_surname <- function(author_string) {
    if (is.na(author_string)) {
      return(NA_character_)
    }

    # Remove extra spaces and trailing punctuation
    cleaned <- stringr::str_trim(author_string)
    cleaned <- stringr::str_replace(cleaned, "\\.$", "")

    # CASE 1: Format "Surname, I." or "Surname, I.I."
    if (stringr::str_detect(cleaned, ",")) {
      # Extract everything before the comma (the surname)
      surname <- stringr::str_extract(cleaned, "^[\\p{L}'-]+")
      return(stringr::str_to_lower(surname))
    }

    # CASE 2: Format "I Surname" or "II Surname"
    # Extract the last word (the surname) if there are at least 2 words
    parts <- stringr::str_split(cleaned, "\\s+")[[1]]
    if (length(parts) >= 2) {
      surname <- parts[length(parts)]
      return(stringr::str_to_lower(surname))
    }

    # CASE 3: A single word (probably just the surname)
    return(stringr::str_to_lower(cleaned))
  }

  # === FUNCTION TO NORMALIZE NAMES (removes accents) ===
  normalize_name <- function(name) {
    if (is.na(name)) {
      return(NA_character_)
    }

    # Remove common accents using Unicode escape sequences
    # a variants: à á â ã ä å ā ă ą
    name <- stringr::str_replace_all(
      name,
      "[\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u0101\u0103\u0105]",
      "a"
    )
    # e variants: è é ê ë ē ė ę
    name <- stringr::str_replace_all(
      name,
      "[\u00e8\u00e9\u00ea\u00eb\u0113\u0117\u0119]",
      "e"
    )
    # i variants: ì í î ï ī į ı
    name <- stringr::str_replace_all(
      name,
      "[\u00ec\u00ed\u00ee\u00ef\u012b\u012f\u0131]",
      "i"
    )
    # o variants: ò ó ô õ ö ø ō ő
    name <- stringr::str_replace_all(
      name,
      "[\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8\u014d\u0151]",
      "o"
    )
    # u variants: ù ú û ü ū ű ų
    name <- stringr::str_replace_all(
      name,
      "[\u00f9\u00fa\u00fb\u00fc\u016b\u0171\u0173]",
      "u"
    )
    # y variants: ý ÿ
    name <- stringr::str_replace_all(name, "[\u00fd\u00ff]", "y")
    # n variants: ñ ń
    name <- stringr::str_replace_all(name, "[\u00f1\u0144]", "n")
    # c variants: ç ć č
    name <- stringr::str_replace_all(name, "[\u00e7\u0107\u010d]", "c")
    # s variants: ś š ş
    name <- stringr::str_replace_all(name, "[\u015b\u0161\u015f]", "s")
    # z variants: ź ż ž
    name <- stringr::str_replace_all(name, "[\u017a\u017c\u017e]", "z")
    # ß -> ss
    name <- stringr::str_replace_all(name, "\u00df", "ss")
    # æ -> ae
    name <- stringr::str_replace_all(name, "\u00e6", "ae")
    # œ -> oe
    name <- stringr::str_replace_all(name, "\u0153", "oe")
    # ł -> l
    name <- stringr::str_replace_all(name, "\u0142", "l")
    # đ -> d
    name <- stringr::str_replace_all(name, "\u0111", "d")

    return(stringr::str_to_lower(name))
  }

  # === FUNCTION TO REMOVE COMMON CITATION PREFIXES ===
  remove_citation_prefixes <- function(text) {
    # Remove common prefixes like "see", "e.g.", "cf.", "i.e.", etc.
    text <- stringr::str_replace(
      text,
      "^\\(?(see|e\\.g\\.|cf\\.|i\\.e\\.|viz\\.|compare)\\s+",
      ""
    )
    text <- stringr::str_trim(text)
    return(text)
  }

  # === FUNCTION TO EXTRACT COMPOUND SURNAMES ===
  # Handles surnames with prefixes like "Di Maio", "van Raan", "de Nito"
  extract_compound_surname <- function(text) {
    # Pattern to match compound surnames with common prefixes
    # Matches patterns like: "Di Maio", "van Raan", "de Nito", "von der Leyen"
    compound_pattern <- "^([Dd]i|[Dd]e|[Vv]an|[Vv]on|[Dd]er|[Dd]en|[Ll]a|[Ll]e|[Dd]el|[Dd]ella)\\s+([\\p{L}'-]+)"

    if (stringr::str_detect(text, compound_pattern)) {
      # Extract the full compound name
      compound <- stringr::str_extract(text, compound_pattern)
      return(compound)
    }

    # If not a compound name, extract first word
    first_word <- stringr::str_extract(text, "^[\\p{L}'-]+")
    return(first_word)
  }

  # === PREPARING REFERENCES ===
  # Normalize the second author in the references
  if ("ref_second_author" %in% colnames(references_df)) {
    references_df <- references_df %>%
      dplyr::mutate(
        ref_second_author_normalized = stringr::str_to_lower(ref_second_author)
      )
  } else {
    references_df <- references_df %>%
      dplyr::mutate(
        ref_second_author = NA_character_,
        ref_second_author_normalized = NA_character_
      )
  }

  # For references from CrossRef, extract the surname from the ref_authors field
  # and mark info about multiple authors as unreliable
  if ("ref_source" %in% colnames(references_df)) {
    references_df <- references_df %>%
      dplyr::mutate(
        # For CrossRef, extract surname from ref_authors (e.g., "H Abdi" -> "abdi")
        ref_first_author_surname = ifelse(
          ref_source == "crossref",
          sapply(ref_authors, extract_surname), # Use ref_authors instead of ref_first_author
          ref_first_author_normalized
        ),
        # Add normalized version without accents
        ref_first_author_normalized_no_accents = sapply(
          ref_first_author_surname,
          normalize_name
        ),
        # For CrossRef, n_authors is not reliable (always 1)
        n_authors = ifelse(ref_source == "crossref", NA_integer_, n_authors),
        # For CrossRef, we don't have the second author
        ref_second_author = ifelse(
          ref_source == "crossref",
          NA_character_,
          ref_second_author
        ),
        ref_second_author_normalized = ifelse(
          ref_source == "crossref",
          NA_character_,
          ref_second_author_normalized
        )
      )
  } else {
    # If there is no ref_source, use standard normalization
    references_df <- references_df %>%
      dplyr::mutate(
        ref_first_author_surname = ref_first_author_normalized,
        ref_first_author_normalized_no_accents = sapply(
          ref_first_author_normalized,
          normalize_name
        )
      )
  }

  # === SEPARATING NUMBERED AND NARRATIVE CITATIONS ===
  numbered_citations <- citations_df %>%
    dplyr::filter(stringr::str_detect(citation_type, "^numbered_"))

  narrative_citations <- citations_df %>%
    dplyr::filter(!stringr::str_detect(citation_type, "^numbered_"))

  # === MATCHING FOR NUMBERED CITATIONS ===
  matched_numbered <- tibble::tibble()

  if (nrow(numbered_citations) > 0) {
    for (i in 1:nrow(numbered_citations)) {
      cite <- numbered_citations[i, ]

      # Extract the numbers from the citation
      numbers <- stringr::str_extract_all(cite$citation_text_clean, "\\d+")[[1]]

      if (length(numbers) == 0) {
        matched_row <- cite %>%
          dplyr::mutate(
            matched_ref_id = NA_character_,
            ref_full_text = NA_character_,
            match_confidence = "no_match_missing_number",
            ref_authors = NA_character_,
            ref_year = NA_character_,
            cite_author = NA_character_,
            cite_second_author = NA_character_,
            cite_year = NA_character_,
            cite_has_etal = FALSE
          )
        matched_numbered <- dplyr::bind_rows(matched_numbered, matched_row)
        next
      }

      # For multiple citations, create a row for each number
      for (num in numbers) {
        ref_id_to_match <- paste0("REF_", num)

        matched_ref <- references_df %>%
          dplyr::filter(ref_id == ref_id_to_match)

        if (nrow(matched_ref) == 1) {
          matched_row <- cite %>%
            dplyr::mutate(
              matched_ref_id = matched_ref$ref_id,
              ref_full_text = matched_ref$ref_full_text,
              match_confidence = "high_numbered",
              ref_authors = matched_ref$ref_authors,
              ref_year = matched_ref$ref_year,
              cite_author = NA_character_,
              cite_second_author = NA_character_,
              cite_year = matched_ref$ref_year,
              cite_has_etal = FALSE
            )
        } else {
          matched_row <- cite %>%
            dplyr::mutate(
              matched_ref_id = NA_character_,
              ref_full_text = NA_character_,
              match_confidence = "no_match_numbered",
              ref_authors = NA_character_,
              ref_year = NA_character_,
              cite_author = NA_character_,
              cite_second_author = NA_character_,
              cite_year = NA_character_,
              cite_has_etal = FALSE
            )
        }

        matched_numbered <- dplyr::bind_rows(matched_numbered, matched_row)
      }
    }
  }

  # === MATCHING FOR NARRATIVE CITATIONS ===
  matched_narrative <- tibble::tibble()

  if (nrow(narrative_citations) > 0) {
    # Function to extract information from the citation
    extract_citation_info <- function(citation_text) {
      # Remove parentheses
      clean_cite <- stringr::str_replace_all(citation_text, "^\\(|\\)$", "")

      # Remove common citation prefixes (see, e.g., cf., etc.)
      clean_cite <- remove_citation_prefixes(clean_cite)

      # Extract year
      year <- stringr::str_extract(clean_cite, "\\d{4}[a-z]?")

      # Check for et al. and and/&
      has_etal <- stringr::str_detect(clean_cite, "et\\s+al\\.")
      has_and <- stringr::str_detect(clean_cite, "\\s+and\\s+|\\s+&\\s+")

      # Extract first author (handle compound surnames and accents)
      # Use Unicode character class \p{L} to include accented characters
      author <- extract_compound_surname(clean_cite)
      author_normalized <- stringr::str_to_lower(author)

      # Extract second author if present
      second_author <- NA_character_
      if (has_and && !has_etal) {
        # Extract text after "and" or "&"
        after_and <- stringr::str_extract(
          clean_cite,
          "(?:and|&)\\s+([\\p{L}][\\p{L}\\s'-]+?)(?:,|\\s+\\()"
        )

        if (!is.na(after_and)) {
          # Clean up the extracted text
          second_author <- stringr::str_replace(after_and, "^(?:and|&)\\s+", "")
          second_author <- stringr::str_replace(second_author, "[,\\(].*$", "")
          second_author <- stringr::str_trim(second_author)

          # Handle compound surnames for second author too
          second_author <- extract_compound_surname(second_author)
        }
      }
      second_author_normalized <- stringr::str_to_lower(second_author)

      # Estimate number of authors
      n_cite_authors <- if (has_etal) {
        99
      } else if (has_and) {
        stringr::str_count(clean_cite, ",") + 2
      } else {
        1
      }

      list(
        author = author,
        author_normalized = author_normalized,
        second_author = second_author,
        second_author_normalized = second_author_normalized,
        year = year,
        has_etal = has_etal,
        has_and = has_and,
        n_authors = n_cite_authors
      )
    }

    # Extract information from all citations
    citations_info <- narrative_citations %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        cite_info = list(extract_citation_info(citation_text_clean)),
        cite_author = cite_info$author,
        cite_author_normalized = cite_info$author_normalized,
        cite_author_normalized_no_accents = normalize_name(
          cite_info$author_normalized
        ),
        cite_second_author = cite_info$second_author,
        cite_second_author_normalized = cite_info$second_author_normalized,
        cite_year = cite_info$year,
        cite_has_etal = cite_info$has_etal,
        cite_has_and = cite_info$has_and,
        cite_n_authors = cite_info$n_authors
      ) %>%
      dplyr::select(-cite_info) %>%
      dplyr::ungroup()

    # Match each citation
    for (i in 1:nrow(citations_info)) {
      cite <- citations_info[i, ]

      # Check for minimum information
      if (is.na(cite$cite_author_normalized) || is.na(cite$cite_year)) {
        matched_row <- cite %>%
          dplyr::mutate(
            matched_ref_id = NA_character_,
            ref_full_text = NA_character_,
            match_confidence = "no_match_missing_info",
            ref_authors = NA_character_,
            ref_year = NA_character_
          )
        matched_narrative <- dplyr::bind_rows(matched_narrative, matched_row)
        next
      }

      # 1. Filter by year
      year_matches <- references_df %>%
        dplyr::filter(!is.na(ref_year), ref_year == cite$cite_year)

      # If no match is found and the year has a suffix (2011a, 2011b),
      # try removing the suffix
      if (
        nrow(year_matches) == 0 &&
          stringr::str_detect(cite$cite_year, "\\d{4}[a-z]$")
      ) {
        year_no_suffix <- stringr::str_replace(cite$cite_year, "[a-z]$", "")
        year_matches <- references_df %>%
          dplyr::filter(!is.na(ref_year), ref_year == year_no_suffix)
      }

      if (nrow(year_matches) == 0) {
        matched_row <- cite %>%
          dplyr::mutate(
            matched_ref_id = NA_character_,
            ref_full_text = NA_character_,
            match_confidence = "no_match_year",
            ref_authors = NA_character_,
            ref_year = NA_character_
          )
        matched_narrative <- dplyr::bind_rows(matched_narrative, matched_row)
        next
      }

      # 2. Match first author (exact) using ref_first_author_surname
      # This works for both CrossRef (surname only) and parsed references
      author_matches <- year_matches %>%
        dplyr::filter(
          !is.na(ref_first_author_surname),
          ref_first_author_surname == cite$cite_author_normalized
        )

      # 3. If no exact match is found, try fuzzy matching with normalized names (without accents)
      if (nrow(author_matches) == 0) {
        author_matches <- year_matches %>%
          dplyr::filter(
            !is.na(ref_first_author_normalized_no_accents),
            ref_first_author_normalized_no_accents ==
              cite$cite_author_normalized_no_accents
          )
      }

      # 4. If still no match, try fuzzy matching with substring
      if (nrow(author_matches) == 0) {
        fuzzy_matches <- year_matches %>%
          dplyr::filter(!is.na(ref_first_author_surname)) %>%
          dplyr::filter(
            stringr::str_detect(
              ref_first_author_surname,
              cite$cite_author_normalized
            ) |
              stringr::str_detect(
                cite$cite_author_normalized,
                ref_first_author_surname
              )
          )

        if (nrow(fuzzy_matches) > 0) {
          author_matches <- fuzzy_matches
        } else {
          matched_row <- cite %>%
            dplyr::mutate(
              matched_ref_id = NA_character_,
              ref_full_text = NA_character_,
              match_confidence = "no_match_author",
              ref_authors = NA_character_,
              ref_year = NA_character_
            )
          matched_narrative <- dplyr::bind_rows(matched_narrative, matched_row)
          next
        }
      }

      # 5. Disambiguation
      final_match <- NULL
      confidence <- "high"

      if (nrow(author_matches) == 1) {
        # Single match: only check "et al." consistency if we have the info
        final_match <- author_matches[1, ]

        if (
          cite$cite_has_etal &&
            !is.na(final_match$n_authors) &&
            final_match$n_authors < 3
        ) {
          confidence <- "medium_etal_inconsistent"
        }
      } else {
        # Multiple matches: disambiguate

        # A. If the citation has an alphabetical suffix (2011a, 2011b),
        # sort the matches by ref_id and choose based on the suffix
        if (stringr::str_detect(cite$cite_year, "\\d{4}[a-z]$")) {
          suffix <- stringr::str_extract(cite$cite_year, "[a-z]$")
          # Sort by ref_id consistently
          author_matches <- author_matches %>%
            dplyr::arrange(ref_id)

          # Convert suffix to index: 'a'=1, 'b'=2, 'c'=3, etc.
          suffix_index <- which(letters == suffix)

          if (suffix_index <= nrow(author_matches)) {
            final_match <- author_matches[suffix_index, ]
            confidence <- "high_suffix_disambiguated"
          } else {
            # If the suffix is beyond the number of matches, take the last one
            final_match <- author_matches[nrow(author_matches), ]
            confidence <- "medium_suffix_out_of_range"
          }
        }

        # B. Try with second author (only if available in the reference)
        if (
          is.null(final_match) && !is.na(cite$cite_second_author_normalized)
        ) {
          second_match <- author_matches %>%
            dplyr::filter(
              !is.na(ref_second_author_normalized),
              ref_second_author_normalized == cite$cite_second_author_normalized
            )

          if (nrow(second_match) == 1) {
            final_match <- second_match[1, ]
            confidence <- "high_second_author"
          } else if (nrow(second_match) > 1) {
            final_match <- second_match[1, ]
            confidence <- "medium_multiple_with_second"
          }
        }

        # C. "et al." heuristic (only if n_authors is available)
        if (is.null(final_match) && cite$cite_has_etal) {
          etal_candidates <- author_matches %>%
            dplyr::filter(!is.na(n_authors), n_authors >= 3) %>%
            dplyr::arrange(dplyr::desc(n_authors))

          if (nrow(etal_candidates) > 0) {
            final_match <- etal_candidates[1, ]
            confidence <- "medium_etal_heuristic"
          } else {
            # For CrossRef, accept the first match even without n_authors info
            final_match <- author_matches[1, ]
            confidence <- "medium_crossref_etal"
          }
        }

        # D. Fallback: take the first one
        if (is.null(final_match)) {
          final_match <- author_matches[1, ]
          confidence <- "medium_multiple_matches"
        }
      }

      # Create row with match
      matched_row <- cite %>%
        dplyr::mutate(
          matched_ref_id = final_match$ref_id,
          ref_full_text = final_match$ref_full_text,
          match_confidence = confidence,
          ref_authors = final_match$ref_authors,
          ref_year = final_match$ref_year
        )

      matched_narrative <- dplyr::bind_rows(matched_narrative, matched_row)
    }
  }

  # === COMBINE RESULTS ===
  matched_citations <- dplyr::bind_rows(matched_numbered, matched_narrative)

  result <- matched_citations %>%
    dplyr::select(
      citation_id,
      citation_text,
      citation_text_clean,
      citation_type,
      cite_author,
      cite_second_author,
      cite_year,
      cite_has_etal,
      matched_ref_id,
      ref_full_text,
      ref_authors,
      ref_year,
      match_confidence
    )

  return(result)
}
