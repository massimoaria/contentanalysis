#' Compute adaptive Y-tolerance from PDF word coordinates
#'
#' @param y_values Numeric vector of Y coordinates from pdf_data
#' @return Numeric tolerance value
#' @keywords internal
#' @noRd
compute_y_tolerance <- function(y_values) {
  if (length(y_values) < 10) return(4)
  sorted_y <- sort(unique(y_values))
  gaps <- diff(sorted_y)
  small_gaps <- gaps[gaps <= quantile(gaps, 0.5)]
  if (length(small_gaps) == 0) return(4)
  tol <- max(ceiling(median(small_gaps) * 1.5), 2)
  min(tol, 8)
}


#' Reconstruct text from PDF data with structure preservation
#'
#' @param column_data Data frame from pdftools::pdf_data() for a column/page
#' @param preserve_structure Logical. Preserve paragraph breaks and structure
#' @param y_tolerance Numeric or NULL. Y-coordinate tolerance for line grouping.
#'   If NULL, computed automatically from the data.
#'
#' @return Character string with reconstructed text
#'
#' @keywords internal
#' @noRd
reconstruct_text_structured <- function(
  column_data,
  preserve_structure = TRUE,
  y_tolerance = NULL
) {
  if (nrow(column_data) == 0) {
    return("")
  }

  if (is.null(y_tolerance)) y_tolerance <- compute_y_tolerance(column_data$y)
  column_data$line <- round(column_data$y / y_tolerance) * y_tolerance

  available_cols <- names(column_data)

  if ("height" %in% available_cols) {
    column_data$font_size <- column_data$height
  } else {
    column_data$font_size <- 12
  }

  lines <- split(column_data, column_data$line)

  line_results <- lapply(lines, function(line) {
    line <- line[order(line$x), ]

    line_text <- paste(line$text, collapse = " ")
    line_text <- trimws(line_text)

    avg_font_size <- mean(line$font_size, na.rm = TRUE)
    is_short <- nchar(line_text) < 80
    is_caps <- grepl("^[A-Z\\s\\d\\.\\-]+$", line_text)
    starts_with_number <- grepl("^\\d+\\.", line_text)
    starts_with_section <- grepl("^\\d+\\.\\d+", line_text)

    is_reference_start <- grepl(
      "^[A-Z][a-z]+(?:['-][A-Z][a-z]+)?,\\s+[A-Z]\\.",
      line_text,
      perl = TRUE
    )

    is_title <- (is_short &&
      (is_caps || starts_with_number || starts_with_section))

    return(list(
      text = line_text,
      y = min(line$y),
      is_title = is_title,
      font_size = avg_font_size,
      starts_with_number = starts_with_number,
      is_reference_start = is_reference_start
    ))
  })

  line_results <- line_results[sapply(line_results, function(x) {
    nchar(x$text) > 0
  })]
  line_results <- line_results[order(sapply(line_results, function(x) x$y))]

  if (!preserve_structure) {
    result <- paste(sapply(line_results, function(x) x$text), collapse = " ")
  } else {
    result_parts <- c()

    for (i in seq_along(line_results)) {
      current_line <- line_results[[i]]
      line_text <- current_line$text

      if (nchar(line_text) == 0) {
        next
      }

      if (i == 1) {
        result_parts <- c(result_parts, line_text)
      } else {
        prev_line <- line_results[[i - 1]]

        if (current_line$is_reference_start) {
          result_parts <- c(result_parts, "\n\n", line_text)
        } else if (current_line$is_title) {
          result_parts <- c(result_parts, "\n\n", line_text)
        } else if (prev_line$is_title) {
          result_parts <- c(result_parts, "\n\n", line_text)
        } else if (
          grepl("[.!?]\\s*$", prev_line$text) &&
            grepl("^[A-Z]", line_text) &&
            !grepl("^[A-Z][a-z]+\\s+[a-z]", line_text)
        ) {
          result_parts <- c(result_parts, "\n\n", line_text)
        } else {
          result_parts <- c(result_parts, " ", line_text)
        }
      }
    }

    result <- paste(result_parts, collapse = "")
    result <- gsub("\\s+", " ", result)
    result <- gsub("\\n\\s+", "\n", result)
    result <- gsub("\\n{3,}", "\n\n", result)
  }

  result <- trimws(result)
  return(result)
}


#' Convert superscript citations to bracketed format
#'
#' @description
#' Post-processes extracted text to identify and convert superscript citation
#' numbers into bracketed format for better recognition by text analysis tools.
#'
#' @param text Character string. The extracted text from PDF.
#'
#' @return Character string with superscript citations converted to bracketed format.
#'
#' @details
#' This function uses pattern matching and contextual analysis to identify
#' numeric citations that appear as superscripts in the original PDF.
#' It handles:
#' \itemize{
#'   \item Single numbers after words (e.g., "study 1" becomes "study\code{[1]}")
#'   \item Multiple citations (e.g., "study 1,2,3" becomes "study\code{[1,2,3]}")
#'   \item Range citations (e.g., "study 3-5" becomes "study\code{[3-5]}")
#'   \item Mixed patterns (e.g., "study 1,3-5,7" becomes "study\code{[1,3-5,7]}")
#' }
#'
#' The function is conservative and only converts numbers that clearly
#' appear to be citations based on context.
#'
#' @keywords internal
#' @noRd
convert_superscript_citations <- function(text) {
  # Non-citation words: common words that precede numbers but are NOT citations
  non_cite <- paste0(
    "\\b(?:page|pages|table|tables|figure|figures|fig|figs|",
    "chapter|chapters|section|sections|step|steps|item|items|",
    "part|parts|equation|equations|eq|eqs|line|lines|row|rows|",
    "column|columns|volume|vol|issue|number|no|sample|group|groups|",
    "stage|stages|grade|grades|level|levels|type|types|model|models|",
    "trial|trials|wave|waves|phase|phases|version|day|days|",
    "week|weeks|month|months|year|years|age|aged|appendix|panel|",
    "supplement|experiment|experiments|criterion|criteria|factor|factors|",
    "dimension|dimensions|category|categories|cluster|clusters|",
    "component|components|participant|participants|subject|subjects|",
    "patient|patients|respondent|respondents|hypothesis|hypotheses)\\s+"
  )

  # Step 0: Convert Unicode superscript digits to [n] format
  # Must run before any other pattern
  unicode_supers <- c(
    "\u00b9" = "1", "\u00b2" = "2", "\u00b3" = "3",
    "\u2074" = "4", "\u2075" = "5", "\u2076" = "6",
    "\u2077" = "7", "\u2078" = "8", "\u2079" = "9", "\u2070" = "0"
  )
  # Build alternation of all unicode superscript chars
  super_chars <- paste0("[", paste0(names(unicode_supers), collapse = ""), "]")
  # Replace sequences of unicode superscripts with [n] format
  text <- gsub(
    paste0("(", super_chars, "+)"),
    "SUPER_PLACEHOLDER\\1",
    text,
    perl = TRUE
  )
  # Convert individual chars inside placeholders
  for (uc in names(unicode_supers)) {
    text <- gsub(uc, unicode_supers[uc], text, fixed = TRUE)
  }
  text <- gsub("SUPER_PLACEHOLDER(\\d+)", "[\\1]", text, perl = TRUE)

  # Pattern 1: Single number after punctuation at end of sentence
  # Example: "...as shown before. 15 The next..." -> "...as shown before.[15] The next..."
  text <- gsub(
    "([.!?])\\s+(\\d{1,3})\\s+([A-Z])",
    "\\1[\\2] \\3",
    text,
    perl = TRUE
  )

  # Pattern 3 (before Pattern 2): Multiple citation numbers with various separators
  # Example: "evidence 1, 2, 3" -> "evidence[1,2,3]"
  # Runs first so comma-separated groups are captured as a unit
  text <- gsub(
    paste0(non_cite, "\\d+(?:\\s*[,;]\\s*\\d+)+(*SKIP)(*FAIL)|",
           "([a-z])\\s+(\\d{1,3}(?:\\s*[,;]\\s*\\d{1,3})+)(?=\\s|[.,;!?]|$)"),
    "\\1[\\2]",
    text,
    perl = TRUE
  )

  # Pattern 2: Single or multiple numbers after a word (no space or minimal space)
  # Example: "study 5" or "study5" -> "study[5]"
  # Handles: single numbers, comma-separated, ranges with dash
  text <- gsub(
    paste0(non_cite, "\\d+(?:[,\\-]\\d+)*(*SKIP)(*FAIL)|",
           "([a-z])\\s+(\\d{1,3}(?:[,\\-]\\d{1,3})*)(?=\\s|[.,;!?]|$)"),
    "\\1[\\2]",
    text,
    perl = TRUE
  )

  # Pattern 4: Range citations with "e" (common OCR artifact for dash)
  # Example: "studies 3e5" -> "studies[3-5]"
  text <- gsub(
    "([a-z])\\s+(\\d{1,3}e\\d{1,3})(?=\\s|[.,;!?]|$)",
    "\\1[\\2]",
    text,
    perl = TRUE
  )

  # Pattern 5: Numbers immediately after closing parenthesis or quote
  # Example: "(Smith et al.) 15" -> "(Smith et al.)[15]"
  text <- gsub(
    "([)])\\s+(\\d{1,3}(?:[,\\-]\\d{1,3})*)(?=\\s|[.,;!?]|$)",
    "\\1[\\2]",
    text,
    perl = TRUE
  )

  # Pattern 7: Numbers at line start followed by uppercase (common in PDF extraction)
  # Example: "\n15 The next..." -> "\n[15] The next..."
  text <- gsub(
    "(\\n)(\\d{1,3})\\s+([A-Z])",
    "\\1[\\2] \\3",
    text,
    perl = TRUE
  )

  # Clean up: remove extra spaces inside brackets only
  text <- gsub("\\[\\s+", "[", text, perl = TRUE)
  text <- gsub("\\s+\\]", "]", text, perl = TRUE)
  # Normalize commas only inside [...] brackets using regmatches
  m <- gregexpr("\\[[^]]+\\]", text)
  brackets <- regmatches(text, m)
  brackets <- lapply(brackets, function(b) gsub("\\s*,\\s*", ",", b))
  regmatches(text, m) <- brackets

  # Clean up: convert "e" to "-" inside brackets (OCR artifact)
  text <- gsub("\\[(\\d+)e(\\d+)\\]", "[\\1-\\2]", text, perl = TRUE)

  # Pattern 6: Handle cases where number appears at end of incomplete word
  # This catches cases where PDF extraction merged citation with word
  # Example: "studies15" -> "studies[15]"
  # Non-citation word variant (no trailing \s+)
  non_cite_no_space <- paste0(
    "\\b(?:page|pages|table|tables|figure|figures|fig|figs|",
    "chapter|chapters|section|sections|step|steps|item|items|",
    "part|parts|equation|equations|eq|eqs|line|lines|row|rows|",
    "column|columns|volume|vol|issue|number|no|sample|group|groups|",
    "stage|stages|grade|grades|level|levels|type|types|model|models|",
    "trial|trials|wave|waves|phase|phases|version|day|days|",
    "week|weeks|month|months|year|years|age|aged|appendix|panel|",
    "supplement|experiment|experiments|criterion|criteria|factor|factors|",
    "dimension|dimensions|category|categories|cluster|clusters|",
    "component|components|participant|participants|subject|subjects|",
    "patient|patients|respondent|respondents|hypothesis|hypotheses)"
  )
  text <- gsub(
    paste0(non_cite_no_space, "\\d{1,3}(*SKIP)(*FAIL)|",
           "([a-z]{3,})(\\d{1,3})(?=\\s|[.,;!?]|$)"),
    "\\1[\\2]",
    text,
    perl = TRUE
  )

  return(text)
}


#' Find the gutter x-position between two column centers
#'
#' @param x_positions Numeric vector of x coordinates
#' @param centers Numeric vector of two cluster centers
#' @return Numeric gutter x-position
#' @keywords internal
#' @noRd
find_gutter_x <- function(x_positions, centers) {
  left_center <- min(centers)
  right_center <- max(centers)
  # Only consider x values between the centers (where the gutter is)
  mid_x <- x_positions[x_positions >= left_center & x_positions <= right_center]
  if (length(mid_x) < 2) return(mean(centers))
  breaks <- seq(left_center, right_center, length.out = 50)
  counts <- graphics::hist(mid_x, breaks = breaks, plot = FALSE)$counts
  gutter_idx <- which.min(counts)
  breaks[gutter_idx + 1]
}


#' Validate that a column split produces real columns
#'
#' @param page_data Data frame with x, y columns
#' @param threshold Numeric x-coordinate threshold
#' @return Logical TRUE if valid two-column layout
#' @keywords internal
#' @noRd
validate_columns <- function(page_data, threshold) {
  left <- page_data[page_data$x < threshold, ]
  right <- page_data[page_data$x >= threshold, ]

  if (nrow(left) < 5 || nrow(right) < 5) return(FALSE)

  # Check Y-range overlap: columns must share vertical space
  left_range <- range(left$y)
  right_range <- range(right$y)
  overlap <- min(left_range[2], right_range[2]) - max(left_range[1], right_range[1])
  total <- max(left_range[2], right_range[2]) - min(left_range[1], right_range[1])
  if (total == 0 || overlap / total < 0.3) return(FALSE)

  # Check that both sides have multiple lines
  y_tol <- compute_y_tolerance(page_data$y)
  left_lines <- length(unique(round(left$y / y_tol) * y_tol))
  right_lines <- length(unique(round(right$y / y_tol) * y_tol))
  if (left_lines < 3 || right_lines < 3) return(FALSE)

  TRUE
}


#' Detect column interleaving in extracted text
#'
#' @param text Character string of extracted text
#' @return Logical TRUE if interleaving is detected
#' @keywords internal
#' @noRd
detect_interleaving <- function(text) {
  sentences <- unlist(strsplit(text, "(?<=[.!?])\\s+", perl = TRUE))
  sentences <- sentences[nchar(sentences) > 20]
  if (length(sentences) < 10) return(FALSE)

  median_len <- median(nchar(sentences))
  short_ratio <- mean(nchar(sentences) < median_len * 0.3)
  lower_start_ratio <- mean(grepl("^[a-z]", sentences))

  short_ratio > 0.4 || lower_start_ratio > 0.3
}


#' Extract text from multi-column PDF with structure preservation
#'
#' @description
#' Extracts text from PDF files handling multi-column layouts, with options
#' for structure preservation and automatic column detection. This version
#' includes post-processing to convert superscript citation numbers based on
#' the specified citation type.
#'
#' @param file Character string. Path to the PDF file.
#' @param n_columns Integer or NULL. Number of columns to detect. If NULL,
#'   attempts automatic detection. Default is NULL.
#' @param column_threshold Numeric or NULL. X-coordinate threshold for column
#'   separation. If NULL and n_columns is NULL, calculated automatically.
#' @param preserve_structure Logical. If TRUE, preserves paragraph breaks and
#'   section structure. If FALSE, returns continuous text. Default is TRUE.
#' @param citation_type Character string. Type of citations in the document:
#'   \itemize{
#'     \item "numeric_superscript": Numeric citations in superscript (converted to [n])
#'     \item "numeric_bracketed": Numeric citations already in brackets [n] (no conversion)
#'     \item "author_year": Author-year citations like (Smith, 2020) (no conversion)
#'     \item "none": No citation conversion
#'   }
#'   Default is "none".
#'
#' @return Character string with extracted text.
#'
#' @details
#' This function uses `pdftools::pdf_data()` for precise text extraction with
#' spatial coordinates. It handles:
#' \itemize{
#'   \item Multi-column layouts (2+ columns)
#'   \item Section detection and paragraph preservation
#'   \item Hyphenation removal
#'   \item Title and heading identification
#'   \item Superscript citation number conversion (only if citation_type = "numeric_superscript")
#' }
#'
#' If `pdf_data()` fails, falls back to `pdftools::pdf_text()`.
#'
#' @examples
#' \dontrun{
#' # Extract from 2-column paper with superscript citations
#' text <- pdf2txt_multicolumn_safe("paper.pdf", n_columns = 2,
#'                                   citation_type = "numeric_superscript")
#'
#' # Extract paper with author-year citations (no conversion)
#' text <- pdf2txt_multicolumn_safe("paper.pdf", citation_type = "author_year")
#' }
#'
#' @export
#' @importFrom pdftools pdf_data pdf_length pdf_text poppler_config
#' @importFrom stats kmeans quantile
pdf2txt_multicolumn_safe <- function(
  file,
  n_columns = NULL,
  column_threshold = NULL,
  preserve_structure = TRUE,
  citation_type = c(
    "none",
    "numeric_superscript",
    "numeric_bracketed",
    "author_year"
  )
) {
  # Validate citation_type parameter
  citation_type <- match.arg(citation_type)

  has_poppler_config <- exists(
    "poppler_config",
    where = asNamespace("pdftools"),
    mode = "function"
  )

  if (has_poppler_config) {
    if (!pdftools::poppler_config()$has_pdf_data) {
      message(
        "Pdf import feature requires a recent version of libpoppler. Please install it."
      )
      return(NA)
    }
  }

  tryCatch(
    {
      data_list <- pdftools::pdf_data(file)
      all_text <- c()

      for (page_num in seq_along(data_list)) {
        page_data <- data_list[[page_num]]
        if (nrow(page_data) == 0) {
          next
        }

        if (!is.null(n_columns)) {
          if (n_columns == 1) {
            y_tol <- compute_y_tolerance(page_data$y)
            page_data <- page_data[order(page_data$y, page_data$x), ]
            page_text <- reconstruct_text_structured(
              page_data,
              preserve_structure,
              y_tolerance = y_tol
            )
          } else if (n_columns >= 2) {
            x_positions <- page_data$x
            y_tol <- compute_y_tolerance(page_data$y)

            tryCatch(
              {
                clusters <- kmeans(
                  x_positions,
                  centers = n_columns,
                  nstart = 20
                )
                cluster_centers <- sort(clusters$centers[, 1])

                thresholds <- numeric(n_columns - 1)
                for (i in 1:(n_columns - 1)) {
                  thresholds[i] <- find_gutter_x(
                    x_positions,
                    c(cluster_centers[i], cluster_centers[i + 1])
                  )
                }

                # Validate column split
                valid_columns <- TRUE
                for (i in seq_along(thresholds)) {
                  if (!validate_columns(page_data, thresholds[i])) {
                    valid_columns <- FALSE
                    break
                  }
                }

                if (!valid_columns) {
                  # Fall back to single-column for this page
                  page_data <- page_data[order(page_data$y, page_data$x), ]
                  page_text <- reconstruct_text_structured(
                    page_data,
                    preserve_structure,
                    y_tolerance = y_tol
                  )
                } else {
                  columns <- list()
                  page_data$column <- cut(
                    page_data$x,
                    breaks = c(-Inf, thresholds, Inf),
                    labels = FALSE
                  )

                  for (col in 1:n_columns) {
                    col_data <- page_data[page_data$column == col, ]
                    if (nrow(col_data) > 0) {
                      col_data <- col_data[order(col_data$y, col_data$x), ]
                      columns[[col]] <- reconstruct_text_structured(
                        col_data,
                        preserve_structure,
                        y_tolerance = y_tol
                      )
                    } else {
                      columns[[col]] <- ""
                    }
                  }

                  if (preserve_structure) {
                    page_text <- paste(columns, collapse = "\n\n")
                  } else {
                    page_text <- paste(columns, collapse = " ")
                  }
                }
              },
              error = function(e) {
                message(
                  "K-means clustering failed for ",
                  n_columns,
                  " columns: ",
                  e$message
                )
                # Fallback: divide page width equally
                page_width <- max(page_data$x) - min(page_data$x)
                column_width <- page_width / n_columns
                thresholds <- min(page_data$x) +
                  column_width * (1:(n_columns - 1))

                columns <- list()
                page_data$column <- cut(
                  page_data$x,
                  breaks = c(-Inf, thresholds, Inf),
                  labels = FALSE
                )

                for (col in 1:n_columns) {
                  col_data <- page_data[page_data$column == col, ]
                  if (nrow(col_data) > 0) {
                    col_data <- col_data[order(col_data$y, col_data$x), ]
                    columns[[col]] <- reconstruct_text_structured(
                      col_data,
                      preserve_structure,
                      y_tolerance = y_tol
                    )
                  }
                }

                page_text <- paste(
                  columns,
                  collapse = ifelse(preserve_structure, "\n\n", " ")
                )
              }
            )
          }
        } else {
          # Automatic column detection
          y_tol <- compute_y_tolerance(page_data$y)
          if (is.null(column_threshold)) {
            x_positions <- page_data$x
            if (length(unique(x_positions)) > 20) {
              tryCatch(
                {
                  clusters <- kmeans(x_positions, centers = 2, nstart = 10)
                  cluster_centers <- sort(clusters$centers[, 1])
                  column_threshold <- find_gutter_x(x_positions, cluster_centers)
                },
                error = function(e) {
                  column_threshold <- (max(page_data$x) + min(page_data$x)) / 2
                }
              )
            } else {
              column_threshold <- (max(page_data$x) + min(page_data$x)) / 2
            }
          }

          # Validate before treating as two-column
          is_two_col <- validate_columns(page_data, column_threshold)

          if (!is_two_col) {
            # Single column layout
            page_data <- page_data[order(page_data$y, page_data$x), ]
            page_text <- reconstruct_text_structured(
              page_data,
              preserve_structure,
              y_tolerance = y_tol
            )
          } else {
            left_column <- page_data[page_data$x < column_threshold, ]
            right_column <- page_data[page_data$x >= column_threshold, ]

            # Two-column layout
            left_column <- left_column[order(left_column$y, left_column$x), ]
            right_column <- right_column[
              order(right_column$y, right_column$x),
            ]

            left_text <- reconstruct_text_structured(
              left_column,
              preserve_structure,
              y_tolerance = y_tol
            )
            right_text <- reconstruct_text_structured(
              right_column,
              preserve_structure,
              y_tolerance = y_tol
            )

            if (preserve_structure) {
              page_text <- paste(left_text, right_text, sep = "\n\n")
            } else {
              page_text <- paste(left_text, right_text, sep = " ")
            }
          }
        }

        all_text <- c(all_text, page_text)
      }

      # Check for interleaving if multi-column was used
      if (!is.null(n_columns) && n_columns >= 2) {
        combined_check <- paste(all_text, collapse = " ")
        if (detect_interleaving(combined_check)) {
          warning(
            "Column interleaving detected; re-extracting as single column"
          )
          all_text <- c()
          for (page_num in seq_along(data_list)) {
            page_data <- data_list[[page_num]]
            if (nrow(page_data) == 0) next
            y_tol <- compute_y_tolerance(page_data$y)
            page_data <- page_data[order(page_data$y, page_data$x), ]
            page_text <- reconstruct_text_structured(
              page_data,
              preserve_structure,
              y_tolerance = y_tol
            )
            all_text <- c(all_text, page_text)
          }
        }
      }

      # Combine all pages
      if (preserve_structure) {
        txt <- paste(all_text, collapse = "\n\n")
        # Enhance section and heading detection
        txt <- gsub(
          "([0-9]+(?:\\.[0-9]+)*\\.\\s+[A-Z][A-Za-z\\s]{3,50})",
          "\n\n\\1",
          txt
        )
        txt <- gsub("\\s+([A-Z][A-Z\\s]{10,60})\\s+", "\n\n\\1\n\n", txt)
        txt <- gsub("([.!?])\\s+([A-Z][a-z])", "\\1\n\n\\2", txt)
        txt <- gsub("\\n{3,}", "\n\n", txt)
      } else {
        txt <- paste(all_text, collapse = " ")
      }

      # Remove hyphenation at line breaks
      txt <- gsub("-\\s*\n", "", txt)
      txt <- gsub("-\\s+", "", txt)

      # Clean up whitespace
      if (preserve_structure) {
        txt <- gsub("[ \t]+", " ", txt)
        txt <- gsub("\\n ", "\n", txt)
      } else {
        txt <- gsub("\\s+", " ", txt)
      }

      txt <- trimws(txt)

      # Convert superscript citations ONLY if citation_type is "numeric_superscript"
      if (citation_type == "numeric_superscript") {
        txt <- convert_superscript_citations(txt)
        message("Converted superscript citations to bracketed format [n]")
      }

      return(txt)
    },
    error = function(e) {
      message("pdf_data failed, falling back to pdf_text method: ", e$message)

      pages <- pdftools::pdf_length(file)
      txt <- pdftools::pdf_text(file)

      # Fallback text processing for pdf_text method
      if (preserve_structure) {
        # Enhance section and heading detection
        txt <- gsub(
          "([0-9]+(?:\\.[0-9]+)*\\.\\s+[A-Z][A-Za-z\\s]{3,50})",
          "\n\n\\1\n\n",
          txt,
          perl = TRUE
        )
        txt <- gsub("\\n\\s*\\n", "\n\n", txt)
        txt <- gsub("([.!?])\\s*\n\\s*([A-Z])", "\\1\n\n\\2", txt, perl = TRUE)
        txt <- gsub("(?<![.!?\\n])\\n(?![A-Z0-9\\n])", " ", txt, perl = TRUE)
        txt <- paste(txt, collapse = "\n\n")
      } else {
        txt <- gsub("(?<![\\s\\.])\\n(?!\\s)", " ", txt, perl = TRUE)
        txt <- gsub("\n  ", "\n\n", txt)
        txt <- paste(txt, collapse = " ")
      }

      # Remove hyphenation
      txt <- gsub("-\\s", "", txt)

      # Convert superscript citations ONLY if citation_type is "numeric_superscript"
      if (citation_type == "numeric_superscript") {
        txt <- convert_superscript_citations(txt)
        message("Converted superscript citations to bracketed format [n]")
      }

      return(txt)
    }
  )
}

#' Import PDF with Automatic Section Detection
#'
#' @description
#' High-level function that imports PDF files, extracts text while handling multi-column
#' layouts, and optionally splits content into sections. Supports AI-enhanced text
#' extraction using Google Gemini API and includes control over citation format conversion.
#'
#' @param file Character. Path to the PDF file to be processed.
#' @param n_columns Integer or NULL. Number of columns in the PDF layout.
#'   Default is NULL (automatic detection).
#' @param preserve_structure Logical. If TRUE, preserves paragraph structure and formatting.
#'   Default is TRUE.
#' @param sections Logical. If TRUE, splits the document into sections based on headers.
#'   Default is TRUE.
#' @param normalize_refs Logical. If TRUE, normalizes reference formatting in the document.
#'   Default is TRUE.
#' @param citation_type Character. Type of citations used in the document. Options are:
#'   \itemize{
#'     \item \code{"none"}: No citation conversion (default)
#'     \item \code{"numeric_superscript"}: Numeric citations in superscript format,
#'       will be converted to bracket notation
#'     \item \code{"numeric_bracketed"}: Numeric citations already in brackets
#'     \item \code{"author_year"}: Author-year citations (e.g., Smith, 2020)
#'   }
#'   This parameter helps avoid false positives in citation detection. Only specify
#'   \code{"numeric_superscript"} if your document uses superscript numbers for citations.
#' @param enable_ai_support Logical. If TRUE, enables AI-enhanced text extraction using
#'   Google Gemini API. Default is FALSE.
#' @param ai_model Character. The Gemini model version to use for AI processing.
#'   Default is "2.5-flash". See \code{\link{process_large_pdf}} for available models.
#' @param api_key Character or NULL. Google Gemini API key. If NULL, the function
#'   attempts to read from the \code{GEMINI_API_KEY} environment variable.
#'
#' @return
#' If \code{sections = TRUE}, returns a named list where:
#' \itemize{
#'   \item The first element \code{Full_text} contains the complete document text
#'   \item Subsequent elements contain individual sections (Introduction, Methods, etc.)
#' }
#' If \code{sections = FALSE}, returns a character string with the full document text.
#' Returns NA if extraction fails.
#'
#' @details
#' The function attempts multiple extraction methods:
#' \enumerate{
#'   \item First tries multi-column extraction with \code{\link{pdf2txt_multicolumn_safe}}
#'   \item Falls back to standard \code{pdftools::pdf_text} if the first method fails
#'   \item Optionally applies AI-enhanced extraction if \code{enable_ai_support = TRUE}
#' }
#'
#' When AI support is enabled and successful, the function:
#' \itemize{
#'   \item Processes the PDF using \code{\link{process_large_pdf}}
#'   \item Merges text chunks and converts to appropriate format
#'   \item Preserves References/Bibliography section from standard extraction
#'   \item Returns AI-processed content with improved formatting
#' }
#'
#' Citation conversion is applied based on the \code{citation_type} parameter to
#' standardize reference markers throughout the document.
#'
#' @note
#' \itemize{
#'   \item AI support requires a valid Google Gemini API key
#'   \item AI processing may take longer but provides better text extraction quality
#'   \item The function automatically handles hyphenation and line breaks
#'   \item Multi-column layouts are detected and processed appropriately
#' }
#'
#' @examples
#' \dontrun{
#' # Basic import with automatic section detection
#' doc <- pdf2txt_auto("paper.pdf")
#'
#' # Import with superscript citation conversion
#' doc <- pdf2txt_auto(
#'   "paper.pdf",
#'   citation_type = "numeric_superscript"
#' )
#'
#' # Import with AI-enhanced extraction
#' doc <- pdf2txt_auto(
#'   "paper.pdf",
#'   enable_ai_support = TRUE,
#'   ai_model = "2.5-flash",
#'   api_key = Sys.getenv("GEMINI_API_KEY")
#' )
#'
#' # Import paper with author-year citations (no conversion)
#' doc <- pdf2txt_auto(
#'   "paper.pdf",
#'   citation_type = "author_year"
#' )
#'
#' # Simple text extraction without sections or citation processing
#' text <- pdf2txt_auto(
#'   "paper.pdf",
#'   sections = FALSE,
#'   citation_type = "none"
#' )
#'
#' # Access specific sections
#' introduction <- doc$Introduction
#' methods <- doc$Methods
#' }
#'
#' @seealso
#' \code{\link{pdf2txt_multicolumn_safe}} for multi-column extraction,
#' \code{\link{process_large_pdf}} for AI-enhanced processing,
#' \code{\link{split_into_sections}} for section detection
#'
#' @export

pdf2txt_auto <- function(
  file,
  n_columns = NULL,
  preserve_structure = TRUE,
  sections = TRUE,
  normalize_refs = TRUE,
  citation_type = c(
    "none",
    "numeric_superscript",
    "numeric_bracketed",
    "author_year"
  ),
  enable_ai_support = FALSE,
  ai_model = "2.5-flash",
  api_key = NULL
) {
  # Validate citation_type parameter
  citation_type <- match.arg(citation_type)

  result <- pdf2txt_multicolumn_safe(
    file,
    n_columns = n_columns,
    preserve_structure = preserve_structure,
    citation_type = citation_type
  )

  if (is.na(result) || nchar(result) < 100) {
    message(
      "Multi-column method failed or returned short text, trying original method..."
    )

    tryCatch(
      {
        pages <- pdftools::pdf_length(file)
        txt <- pdftools::pdf_text(file)

        if (preserve_structure) {
          txt <- gsub(
            "([0-9]+(?:\\.[0-9]+)*\\.\\s+[A-Za-z][A-Za-z\\s]{3,50})",
            "\n\n\\1\n\n",
            txt,
            perl = TRUE
          )
          txt <- gsub("\\n\\s*\\n", "\n\n", txt)
          txt <- gsub(
            "([.!?])\\s*\n\\s*([A-Z][a-z])",
            "\\1\n\n\\2",
            txt,
            perl = TRUE
          )
          txt <- gsub("(?<![.!?\\n])\\n(?![A-Z0-9\\n])", " ", txt, perl = TRUE)
          txt <- paste(txt, collapse = "\n\n")
        } else {
          txt <- gsub("(?<![\\s\\.])\\n(?!\\s)", " ", txt, perl = TRUE)
          txt <- gsub("\n  ", "\n\n", txt)
          txt <- paste(txt, collapse = " ")
        }

        txt <- gsub("-\\s", "", txt)

        # Convert superscript citations ONLY if citation_type is "numeric_superscript"
        if (citation_type == "numeric_superscript") {
          txt <- convert_superscript_citations(txt)
          message("Converted superscript citations to bracketed format [n]")
        }

        result <- txt
      },
      error = function(e) {
        message("All methods failed: ", e$message)
        return(NA)
      }
    )
  }

  if (sections && !is.na(result)) {
    result <- split_into_sections(result, file_path = file)

    if (normalize_refs && is.list(result)) {
      result <- normalize_references_section(result)
    }
  }

  # Check if AI support is enabled
  if (enable_ai_support) {
    if (is.null(api_key)) {
      api_key <- Sys.getenv("GEMINI_API_KEY")
    }

    if (!isTRUE(nchar(api_key) == 0)) {
      message("Start AI text processing and cleaning")
      pdf_text <- result
      # Execute AI-enhanced text extraction
      text <- process_large_pdf(
        file,
        api_key = api_key,
        pages_per_chunk = 5,
        model = ai_model
      )

      if (!is.null(text)) {
        text_AI <- merge_text_chunks_named(text) %>% as.list()

        text_AI <- text_AI_conversion(
          text_AI,
          citation_type = citation_type
        )

        # Preserve References/Bibliography from original extraction
        if ("Reference" %in% names(pdf_text)) {
          text_AI$References <- pdf_text$References
        } else if ("Bibliography" %in% names(pdf_text)) {
          text_AI$References <- pdf_text$Bibliography
        }

        Full_text <- paste(unlist(text_AI), collapse = "\n\n")
        text_AI <- c(Full_text = Full_text, text_AI)
        result <- text_AI
      } else {
        message(
          "AI processing failed, using deterministic PDF extraction."
        )
      }
    }
  }

  return(result)
}
