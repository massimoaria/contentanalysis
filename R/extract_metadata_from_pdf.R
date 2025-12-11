#' Extract DOI and Metadata from PDF
#'
#' This function extracts the Digital Object Identifier (DOI) and other metadata
#' from a PDF file using pdftools::pdf_info(). It searches through all metadata
#' fields including the XMP metadata XML.
#'
#' @param pdf_path Character. Path to the PDF file.
#' @param fields Character vector. Metadata fields to extract. Options are:
#'   "doi", "title", "authors", "journal", "year", "all". Default is "doi".
#' @param return_all_dois Logical. If TRUE, returns all DOIs found; if FALSE (default),
#'   returns only the first article DOI found (excluding journal ISSNs).
#'
#' @return If fields = "doi" (default), returns a character string with the DOI
#'   or NA_character_ if not found. If multiple fields are requested, returns a
#'   named list with the requested metadata. If return_all_dois = TRUE, the DOI
#'   element will be a character vector.
#'
#' @details
#' The function searches for DOIs in:
#' \itemize{
#'   \item All fields in the keys list (prioritizing article DOI fields)
#'   \item The XMP metadata XML field
#' }
#'
#' Journal DOIs/ISSNs (containing "(ISSN)" or from journal-specific fields)
#' are automatically filtered out to return article DOIs.
#'
#' For other metadata:
#' \itemize{
#'   \item Title: extracted from Title field or dc:title in XMP metadata
#'   \item Authors: extracted from dc:creator in XMP metadata or Author/Creator fields
#'   \item Journal: extracted from Subject, prism:publicationName in XMP metadata
#'   \item Year: extracted from prism:coverDate, created/modified dates (avoiding DOI patterns)
#' }
#'
#' Common DOI prefixes are automatically removed. The function uses regex pattern
#' matching to validate DOI format and extract structured data from XMP XML.
#'
#' @examples
#' \dontrun{
#' # Extract only DOI
#' doi <- extract_pdf_metadata("path/to/paper.pdf")
#'
#' # Extract multiple metadata fields
#' meta <- extract_pdf_metadata("path/to/paper.pdf",
#'                              fields = c("doi", "title", "journal"))
#'
#' # Extract all available metadata
#' meta <- extract_pdf_metadata("path/to/paper.pdf", fields = "all")
#' }
#'
#' @seealso \code{\link[pdftools]{pdf_info}}
#'
#' @importFrom pdftools pdf_info
#'
#' @export
extract_pdf_metadata <- function(
  pdf_path,
  fields = "doi",
  return_all_dois = FALSE
) {
  # Check if file exists
  if (!file.exists(pdf_path)) {
    stop("PDF file does not exist: ", pdf_path)
  }

  # Load PDF information
  tryCatch(
    {
      info <- pdftools::pdf_info(pdf_path)
    },
    error = function(e) {
      stop("Unable to read PDF file: ", e$message)
    }
  )

  # If "all" is specified, extract all fields
  if ("all" %in% fields) {
    fields <- c("doi", "title", "authors", "journal", "year")
  }

  # Initialize results list
  results <- list()

  # Extract DOI if requested
  if ("doi" %in% fields) {
    results$doi <- extract_doi_internal(info, return_all_dois)
  }

  # Extract Title if requested
  if ("title" %in% fields) {
    results$title <- extract_title_internal(info)
  }

  # Extract Authors if requested
  if ("authors" %in% fields) {
    results$authors <- extract_authors_internal(info)
  }

  # Extract Journal if requested
  if ("journal" %in% fields) {
    results$journal <- extract_journal_internal(info)
  }

  # Extract Year if requested
  if ("year" %in% fields) {
    results$year <- extract_year_internal(info)
  }

  # Return single value if only DOI requested, otherwise return list
  if (length(fields) == 1 && fields[1] == "doi") {
    return(results$doi)
  } else {
    return(results)
  }
}


#' Internal function to extract DOI
#' @keywords internal
#' @noRd
extract_doi_internal <- function(info, return_all = FALSE) {
  # Regex pattern for DOI
  doi_pattern <- "10\\.\\d{4,9}/[-._;()/:A-Za-z0-9]+"

  # Storage for DOIs with priority
  article_dois <- character(0) # High priority: article DOIs
  other_dois <- character(0) # Lower priority: other DOIs

  # Define fields that typically contain article DOIs (high priority)
  article_fields <- c(
    "WPS-ARTICLEDOI",
    "ARTICLEDOI",
    "ArticleDOI",
    "doi",
    "DOI",
    "dc:identifier",
    "prism:doi",
    "pdfx:doi"
  )

  # Define fields that contain journal DOIs/ISSNs (to skip)
  journal_fields <- c("WPS-JOURNALDOI", "JOURNALDOI", "JournalDOI", "ISSN")

  # First, check article-specific fields with priority
  if (!is.null(info$keys)) {
    # Get all key names
    key_names <- names(info$keys)

    # Sort keys: article fields first, journal fields last
    priority_keys <- c(
      key_names[key_names %in% article_fields],
      key_names[!key_names %in% c(article_fields, journal_fields)],
      key_names[key_names %in% journal_fields]
    )
    priority_keys <- unique(priority_keys)

    for (key_name in priority_keys) {
      field <- info$keys[[key_name]]

      if (!is.null(field) && is.character(field) && nchar(field) > 0) {
        # Skip if field contains ISSN identifier
        if (grepl("\\(ISSN\\)", field, ignore.case = TRUE)) {
          next
        }

        # Remove common prefixes
        field_clean <- gsub(
          "^(doi:|DOI:|https?://doi.org/|https?://dx.doi.org/)",
          "",
          field,
          ignore.case = TRUE
        )

        # Extract DOI using regex
        doi_matches <- regmatches(
          field_clean,
          gregexpr(doi_pattern, field_clean, perl = TRUE)
        )[[1]]

        if (length(doi_matches) > 0) {
          # Filter out ISSNs
          doi_matches <- doi_matches[
            !grepl("\\(ISSN\\)", doi_matches, ignore.case = TRUE)
          ]

          if (length(doi_matches) > 0) {
            # Categorize based on field name
            if (key_name %in% article_fields) {
              article_dois <- c(article_dois, trimws(doi_matches))
            } else if (!key_name %in% journal_fields) {
              other_dois <- c(other_dois, trimws(doi_matches))
            }

            # If not returning all and we found an article DOI, return immediately
            if (!return_all && length(article_dois) > 0) {
              return(trimws(article_dois[1]))
            }
          }
        }
      }
    }
  }

  # Second, check metadata field (XMP XML) if no article DOI found yet
  if (!is.null(info$metadata) && nchar(info$metadata) > 0) {
    # Remove common prefixes
    metadata_clean <- gsub(
      "(doi:|DOI:|https?://doi.org/|https?://dx.doi.org/)",
      "",
      info$metadata,
      ignore.case = TRUE
    )

    # Extract DOI using regex
    doi_matches <- regmatches(
      metadata_clean,
      gregexpr(doi_pattern, metadata_clean, perl = TRUE)
    )[[1]]

    if (length(doi_matches) > 0) {
      # Filter out ISSNs
      doi_matches <- doi_matches[
        !grepl("\\(ISSN\\)", doi_matches, ignore.case = TRUE)
      ]

      if (length(doi_matches) > 0) {
        other_dois <- c(other_dois, trimws(doi_matches))
      }
    }
  }

  # Combine DOIs with priority: article DOIs first
  all_dois <- c(article_dois, other_dois)

  # Return results
  if (return_all) {
    if (length(all_dois) > 0) {
      return(unique(all_dois))
    } else {
      return(NA_character_)
    }
  } else {
    if (length(all_dois) > 0) {
      return(unique(all_dois)[1])
    } else {
      return(NA_character_)
    }
  }
}


#' Internal function to extract title
#' @keywords internal
#' @noRd
extract_title_internal <- function(info) {
  # Check Title field first
  if (!is.null(info$keys$Title) && nchar(info$keys$Title) > 0) {
    return(clean_text_encoding(trimws(info$keys$Title)))
  }

  # Check metadata XML for dc:title
  if (!is.null(info$metadata) && nchar(info$metadata) > 0) {
    # Extract dc:title with multiline support
    title_match <- regmatches(
      info$metadata,
      regexpr("(?s)<dc:title>.*?</dc:title>", info$metadata, perl = TRUE)
    )

    if (length(title_match) > 0) {
      # Try to extract content from rdf:li tags first (structured format)
      li_content <- regmatches(
        title_match[1],
        regexpr("<rdf:li[^>]*>([^<]+)</rdf:li>", title_match[1], perl = TRUE)
      )

      if (length(li_content) > 0) {
        # Remove XML tags
        title <- gsub("<.*?>", "", li_content[1])
        if (nchar(trimws(title)) > 0) {
          return(clean_text_encoding(trimws(title)))
        }
      } else {
        # If no rdf:li, extract text directly from dc:title
        title <- gsub("(?s)<dc:title>\\s*", "", title_match[1], perl = TRUE)
        title <- gsub("\\s*</dc:title>", "", title, perl = TRUE)
        # Remove any remaining XML tags
        title <- gsub("<.*?>", "", title)
        title <- trimws(title)

        if (nchar(title) > 0) {
          return(clean_text_encoding(title))
        }
      }
    }
  }

  # Check other possible fields
  title_fields <- c("title", "TITLE")

  if (!is.null(info$keys)) {
    for (field_name in title_fields) {
      if (field_name %in% names(info$keys)) {
        field <- info$keys[[field_name]]
        if (!is.null(field) && nchar(field) > 0) {
          return(clean_text_encoding(trimws(field)))
        }
      }
    }
  }

  return(NA_character_)
}


#' Internal function to extract authors
#' @keywords internal
#' @noRd
extract_authors_internal <- function(info) {
  # Check metadata XML for dc:creator FIRST (most complete)
  if (!is.null(info$metadata) && nchar(info$metadata) > 0) {
    # Extract dc:creator section (with DOTALL mode to match across newlines)
    creator_section <- regmatches(
      info$metadata,
      regexpr("(?s)<dc:creator>.*?</dc:creator>", info$metadata, perl = TRUE)
    )

    if (length(creator_section) > 0) {
      # Extract all <rdf:li> elements within dc:creator
      author_matches <- regmatches(
        creator_section[1],
        gregexpr("<rdf:li>([^<]+)</rdf:li>", creator_section[1], perl = TRUE)
      )[[1]]

      if (length(author_matches) > 0) {
        # Remove XML tags and combine authors
        authors <- gsub("<.*?>", "", author_matches)
        authors <- trimws(authors)
        # Filter out empty strings
        authors <- authors[nchar(authors) > 0]

        if (length(authors) > 0) {
          # Clean non-ASCII characters
          authors <- clean_text_encoding(authors)
          # Return as semicolon-separated string
          return(paste(authors, collapse = "; "))
        }
      }
    }
  }

  # Check Author field second
  if (!is.null(info$keys$Author) && nchar(info$keys$Author) > 0) {
    return(clean_text_encoding(trimws(info$keys$Author)))
  }

  # Check Creator field
  if (!is.null(info$keys$Creator) && nchar(info$keys$Creator) > 0) {
    # Filter out software names
    creator <- trimws(info$keys$Creator)
    if (
      !grepl(
        "(Adobe|Microsoft|Word|InDesign|LaTeX|TeX|Elsevier|pdfTeX|Springer)",
        creator,
        ignore.case = TRUE
      )
    ) {
      return(clean_text_encoding(creator))
    }
  }

  # Check other possible fields
  author_fields <- c("author", "AUTHOR", "Authors")

  if (!is.null(info$keys)) {
    for (field_name in author_fields) {
      if (field_name %in% names(info$keys)) {
        field <- info$keys[[field_name]]
        if (!is.null(field) && nchar(field) > 0) {
          return(clean_text_encoding(trimws(field)))
        }
      }
    }
  }

  return(NA_character_)
}


#' Internal function to clean text encoding issues
#' @keywords internal
#' @noRd
clean_text_encoding <- function(text) {
  if (length(text) == 0) {
    return(text)
  }
  if (all(is.na(text))) {
    return(text)
  }
  if (!is.character(text)) {
    return(text)
  }

  # Vectorized operations
  # Replace common Unicode characters with ASCII equivalents
  text <- gsub("\u2018|\u2019", "'", text) # Smart single quotes
  text <- gsub("\u201c|\u201d", '"', text) # Smart double quotes
  text <- gsub("\u2013|\u2014", "-", text) # En-dash and em-dash
  text <- gsub("\u2026", "...", text) # Ellipsis
  text <- gsub("\u00a0", " ", text) # Non-breaking space

  # Decode HTML entities
  text <- gsub("&amp;", "&", text)
  text <- gsub("&lt;", "<", text)
  text <- gsub("&gt;", ">", text)
  text <- gsub("&quot;", '"', text)
  text <- gsub("&apos;", "'", text)
  text <- gsub("&#39;", "'", text)

  # Convert to ASCII, transliterating when possible (element by element)
  text <- sapply(
    text,
    function(x) {
      if (is.na(x)) {
        return(x)
      }
      # Try to convert, if it fails keep original
      result <- tryCatch(
        {
          iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = "'")
        },
        error = function(e) x
      )

      # If conversion created weird patterns like ^a', just remove them
      result <- gsub("\\^[a-z]'", "'", result)
      result <- gsub("\\^[A-Z]'", "'", result)

      return(result)
    },
    USE.NAMES = FALSE
  )

  # Clean up any remaining issues
  text <- gsub("[^[:print:]]", "", text) # Remove non-printable characters
  text <- trimws(text)

  return(text)
}


#' Internal function to extract journal name
#' @keywords internal
#' @noRd
extract_journal_internal <- function(info) {
  # Check metadata XML for prism:publicationName first (most reliable)
  if (!is.null(info$metadata) && nchar(info$metadata) > 0) {
    # Extract prism:publicationName
    journal_match <- regmatches(
      info$metadata,
      regexpr(
        "<prism:publicationName>([^<]+)</prism:publicationName>",
        info$metadata,
        perl = TRUE
      )
    )

    if (length(journal_match) > 0) {
      # Remove XML tags
      journal <- gsub("<.*?>", "", journal_match[1])
      if (nchar(trimws(journal)) > 0) {
        return(clean_text_encoding(trimws(journal)))
      }
    }
  }

  # Check Subject field (often contains journal info)
  if (!is.null(info$keys$Subject) && nchar(info$keys$Subject) > 0) {
    subject <- trimws(info$keys$Subject)

    # Try to extract journal name before comma or "doi:"
    journal_match <- regmatches(
      subject,
      regexpr("^[^,]+(?=,|\\s+doi:)", subject, perl = TRUE)
    )

    if (length(journal_match) > 0) {
      return(clean_text_encoding(trimws(journal_match[1])))
    }
  }

  # Check other possible fields
  journal_fields <- c("Journal", "JOURNAL", "PublicationName")

  if (!is.null(info$keys)) {
    for (field_name in journal_fields) {
      if (field_name %in% names(info$keys)) {
        field <- info$keys[[field_name]]
        if (!is.null(field) && nchar(field) > 0) {
          return(trimws(field))
        }
      }
    }
  }

  return(NA_character_)
}


#' Internal function to extract publication year
#' @keywords internal
#' @noRd
extract_year_internal <- function(info) {
  # Check metadata XML for prism:coverDate or prism:coverDisplayDate
  if (!is.null(info$metadata) && nchar(info$metadata) > 0) {
    # Extract prism:coverDate (format: YYYY-MM-DD)
    date_match <- regmatches(
      info$metadata,
      regexpr(
        "<prism:coverDate>([^<]+)</prism:coverDate>",
        info$metadata,
        perl = TRUE
      )
    )

    if (length(date_match) > 0) {
      date_str <- gsub("<.*?>", "", date_match[1])
      year_match <- regmatches(
        date_str,
        regexpr("\\b(19|20)\\d{2}\\b", date_str)
      )
      if (length(year_match) > 0) {
        return(as.integer(year_match[1]))
      }
    }
  }

  # Check Subject field for year (but avoid DOI patterns)
  if (!is.null(info$keys$Subject) && nchar(info$keys$Subject) > 0) {
    subject <- info$keys$Subject
    # Remove DOI part to avoid matching years in DOI
    subject_clean <- gsub("doi:.*$", "", subject, ignore.case = TRUE)

    year_match <- regmatches(
      subject_clean,
      regexpr("\\b(19|20)\\d{2}\\b", subject_clean)
    )
    if (length(year_match) > 0) {
      return(as.integer(year_match[1]))
    }
  }

  # Check created date
  if (!is.null(info$created)) {
    year <- as.integer(format(info$created, "%Y"))
    if (!is.na(year)) {
      return(year)
    }
  }

  # Check modified date
  if (!is.null(info$modified)) {
    year <- as.integer(format(info$modified, "%Y"))
    if (!is.na(year)) {
      return(year)
    }
  }

  return(NA_integer_)
}


#' Extract DOI from PDF Metadata (Legacy Function)
#'
#' Legacy wrapper function for backward compatibility. Use extract_pdf_metadata()
#' for more functionality.
#'
#' @param pdf_path Character. Path to the PDF file.
#' @param return_all Logical. If TRUE, returns all DOIs found.
#'
#' @return Character string with DOI or NA_character_.
#'
#' @seealso \code{\link{extract_pdf_metadata}}
#'
#' @export
extract_doi_from_pdf <- function(pdf_path, return_all = FALSE) {
  extract_pdf_metadata(pdf_path, fields = "doi", return_all_dois = return_all)
}
