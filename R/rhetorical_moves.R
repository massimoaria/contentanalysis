#' Classify Rhetorical Moves in Scientific Text
#'
#' @description
#' Identifies and classifies rhetorical moves (Swales CARS model and extensions)
#' at the sentence level in scientific papers. Supports Introduction (CARS model),
#' Literature Review, and Discussion/Conclusion sections.
#'
#' Uses a hybrid approach: rule-based classification via cue phrase dictionaries,
#' optionally enhanced with LLM classification via Google Gemini API.
#'
#' @param text Named list of sections (from \code{split_into_sections()}) or a
#'   single character string. Section names are used to select the appropriate
#'   rhetorical move taxonomy.
#' @param api_key Character or NULL. Google Gemini API key. If NULL, uses the
#'   \code{GEMINI_API_KEY} environment variable.
#' @param model Character. Gemini model version. Default is "2.5-flash".
#' @param use_llm Logical. If TRUE (default), uses Gemini LLM for classification
#'   in addition to rule-based methods. Set to FALSE for rule-based only.
#' @param batch_size Integer. Number of sentences per Gemini API call. Default is 20.
#'
#' @return A list with class \code{"rhetorical_move_analysis"} containing:
#'   \itemize{
#'     \item \code{sentences}: Tibble with sentence-level classification
#'       (sentence_id, section, text, position_pct, move, step, confidence, method)
#'     \item \code{move_blocks}: Tibble with aggregated consecutive move blocks
#'       (block_id, section, move, step, start_sentence, end_sentence,
#'       n_sentences, text_preview, avg_confidence)
#'     \item \code{summary}: List with move distribution, step distribution,
#'       flow pattern, sections analyzed, total sentences, and method breakdown
#'   }
#'
#' @details
#' The function processes the following section types:
#' \itemize{
#'   \item \strong{Introduction}: Uses Swales' CARS (Create a Research Space) model
#'     with 3 moves: Establishing a territory, Establishing a niche, Occupying the niche
#'   \item \strong{Literature Review / Background / Related Work}: Uses 3 moves:
#'     Establishing context, Reviewing prior work, Evaluating prior work
#'   \item \strong{Discussion / Conclusion}: Uses 3 moves:
#'     Consolidating results, Evaluating the study, Looking forward
#' }
#'
#' Sections matching "Results", "Findings", "References", "Appendix", or
#' "Acknowledgments" are automatically excluded.
#'
#' @examples
#' \dontrun{
#' # From a PDF
#' text <- pdf2txt_auto("paper.pdf")
#' sections <- split_into_sections(text)
#' moves <- classify_rhetorical_moves(sections)
#'
#' # View sentence-level results
#' moves$sentences
#'
#' # View aggregated blocks
#' moves$move_blocks
#'
#' # View summary
#' moves$summary
#'
#' # Rule-based only (no API needed)
#' moves <- classify_rhetorical_moves(sections, use_llm = FALSE)
#' }
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate group_by summarize n arrange filter count
#' @importFrom stringr str_trunc
classify_rhetorical_moves <- function(
  text,
  api_key = NULL,
  model = "2.5-flash",
  use_llm = TRUE,
  batch_size = 20
) {
  # Handle input: convert string to named list

  if (is.character(text) && !is.list(text)) {
    text <- list(Full_text = text)
  }

  # Filter out sections we don't analyze
  exclude_pattern <- "(?i)(results|findings|references|appendix|acknowledg|bibliography|full_text|preface|preamble|method|material|experimental|procedure)"
  section_names <- names(text)
  sections_to_analyze <- section_names[
    !grepl(exclude_pattern, section_names, perl = TRUE)
  ]

  if (length(sections_to_analyze) == 0) {
    message("No analyzable sections found. Returning empty result.")
    return(empty_move_result())
  }

  message(sprintf(
    "Analyzing rhetorical moves in %d sections: %s",
    length(sections_to_analyze),
    paste(sections_to_analyze, collapse = ", ")
  ))

  # Step 1: Segment into sentences
  sentences_df <- segment_sentences(text[sections_to_analyze])

  if (nrow(sentences_df) == 0) {
    message("No sentences extracted. Returning empty result.")
    return(empty_move_result())
  }

  message(sprintf("Segmented %d sentences", nrow(sentences_df)))

  # Step 2: Assign taxonomy type per section
  sentences_df$taxonomy_type <- vapply(
    sentences_df$section,
    map_section_to_taxonomy,
    character(1)
  )

  # Filter out sentences from sections with no taxonomy
  sentences_df <- sentences_df[!is.na(sentences_df$taxonomy_type), ]

  if (nrow(sentences_df) == 0) {
    message("No sentences with assigned taxonomy. Returning empty result.")
    return(empty_move_result())
  }

  # Step 3: Rule-based classification
  sentences_df <- apply_rule_based_moves(sentences_df)

  # Step 4: LLM classification (optional)
  if (use_llm) {
    sentences_df <- apply_llm_based_moves(
      sentences_df,
      api_key = api_key,
      model = model,
      batch_size = batch_size
    )
  } else {
    sentences_df$move_llm <- NA_character_
    sentences_df$step_llm <- NA_character_
    sentences_df$conf_llm <- 0
  }

  # Step 5: Reconcile rule-based and LLM results
  sentences_df <- reconcile_moves(sentences_df)

  # Step 6: Aggregate consecutive same-move sentences into blocks
  move_blocks <- aggregate_moves(sentences_df)

  # Step 7: Build summary
  summary_data <- build_move_summary(sentences_df)

  # Assemble result
  results <- list(
    sentences = sentences_df[, c(
      "sentence_id", "section", "text", "position_pct",
      "move", "step", "confidence", "method"
    )],
    move_blocks = move_blocks,
    summary = summary_data
  )

  class(results) <- c("rhetorical_move_analysis", "list")
  results
}


#' Segment text sections into sentences
#'
#' @param text_sections Named list of section texts
#'
#' @return Tibble with columns: sentence_id, section, text, position_pct
#'
#' @keywords internal
#' @noRd
segment_sentences <- function(text_sections) {
  # Common abbreviations that should NOT trigger sentence splits
  abbrev_protect <- c(
    "et al", "Fig", "Figs", "Tab", "Eq", "Eqs",
    "Dr", "Prof", "Mr", "Mrs", "Ms",
    "vs", "viz", "cf", "al",
    "i\\.e", "e\\.g", "etc",
    "Vol", "vol", "No", "no", "pp",
    "Jan", "Feb", "Mar", "Apr", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
    "Ref", "Refs", "Sec", "App"
  )

  # Build negative lookbehind for abbreviations
  # We protect "et al." "Fig." etc. from being treated as sentence endings
  abbrev_pattern <- paste0("(?<!", abbrev_protect, ")", collapse = "")

  all_sentences <- list()
  global_id <- 0

  for (section_name in names(text_sections)) {
    section_text <- text_sections[[section_name]]

    # Clean text
    section_text <- gsub("\\n+", " ", section_text)
    section_text <- gsub("\\s+", " ", section_text)
    section_text <- trimws(section_text)

    if (nchar(section_text) < 10) next

    # Remove section header if present at beginning
    section_text <- gsub("^##?\\s+[^.!?]+\\n*", "", section_text)
    section_text <- trimws(section_text)

    if (nchar(section_text) < 10) next

    # Split into sentences:
    # Use a simpler, more robust approach:
    # Split on period/question/exclamation followed by space and uppercase letter
    # But protect common abbreviations
    sentences <- split_sentences_safe(section_text, abbrev_protect)

    # Remove very short fragments
    sentences <- sentences[nchar(trimws(sentences)) >= 15]

    if (length(sentences) == 0) next

    # Calculate position percentage
    n_sent <- length(sentences)
    pos_pct <- if (n_sent == 1) {
      50.0
    } else {
      round((seq_len(n_sent) - 1) / (n_sent - 1) * 100, 1)
    }

    for (j in seq_along(sentences)) {
      global_id <- global_id + 1
      all_sentences[[global_id]] <- list(
        sentence_id = global_id,
        section = section_name,
        text = trimws(sentences[j]),
        position_pct = pos_pct[j]
      )
    }
  }

  if (length(all_sentences) == 0) {
    return(tibble::tibble(
      sentence_id = integer(0),
      section = character(0),
      text = character(0),
      position_pct = numeric(0)
    ))
  }

  tibble::tibble(
    sentence_id = vapply(all_sentences, `[[`, integer(1), "sentence_id"),
    section = vapply(all_sentences, `[[`, character(1), "section"),
    text = vapply(all_sentences, `[[`, character(1), "text"),
    position_pct = vapply(all_sentences, `[[`, numeric(1), "position_pct")
  )
}


#' Split text into sentences with abbreviation protection
#'
#' @param text Character string
#' @param abbreviations Character vector of abbreviations to protect
#'
#' @return Character vector of sentences
#'
#' @keywords internal
#' @noRd
split_sentences_safe <- function(text, abbreviations) {
  # Step 1: Temporarily replace abbreviation dots with a placeholder
  protected <- text
  for (abbr in abbreviations) {
    # Match the abbreviation followed by a period
    pattern <- paste0("\\b(", abbr, ")\\.")
    replacement <- paste0("\\1\u00b6")  # pilcrow as placeholder
    protected <- gsub(pattern, replacement, protected, perl = TRUE)
  }

  # Also protect decimal numbers (e.g., 3.14, p < 0.05)
  protected <- gsub("(\\d)\\.(\\d)", "\\1\u00b6\\2", protected, perl = TRUE)

  # Step 2: Split on sentence-ending punctuation followed by space + uppercase
  # or end of string
  parts <- strsplit(protected, "(?<=[.!?])\\s+(?=[A-Z\"])", perl = TRUE)[[1]]

  # Step 3: Restore the original dots
  parts <- gsub("\u00b6", ".", parts)

  # Trim whitespace
  parts <- trimws(parts)

  # Remove empty strings
  parts[nchar(parts) > 0]
}


#' Reconcile rule-based and LLM move classifications
#'
#' @param sentences_df Tibble with move_rule, step_rule, conf_rule,
#'   move_llm, step_llm, conf_llm columns
#'
#' @return Tibble with added columns: move, step, confidence, method
#'
#' @keywords internal
#' @noRd
reconcile_moves <- function(sentences_df) {
  n <- nrow(sentences_df)
  move <- character(n)
  step <- character(n)
  confidence <- numeric(n)
  method <- character(n)

  for (i in seq_len(n)) {
    has_rule <- !is.na(sentences_df$move_rule[i])
    has_llm <- !is.na(sentences_df$move_llm[i])

    if (has_rule && has_llm) {
      # Extract move codes (M1, M2, M3) for comparison
      rule_code <- sub(":.*", "", sentences_df$move_rule[i])
      llm_code <- sub(":.*", "", sentences_df$move_llm[i])

      if (rule_code == llm_code) {
        # Both agree on move → high confidence, prefer LLM step detail
        move[i] <- sentences_df$move_llm[i]
        step[i] <- sentences_df$step_llm[i]
        confidence[i] <- min(1.0, (sentences_df$conf_rule[i] + sentences_df$conf_llm[i]) / 2 + 0.15)
        method[i] <- "rule+llm"
      } else {
        # Disagree → use LLM (generally more accurate for ambiguous cases)
        move[i] <- sentences_df$move_llm[i]
        step[i] <- sentences_df$step_llm[i]
        confidence[i] <- sentences_df$conf_llm[i] * 0.85  # slight penalty
        method[i] <- "llm"
      }
    } else if (has_llm) {
      move[i] <- sentences_df$move_llm[i]
      step[i] <- sentences_df$step_llm[i]
      confidence[i] <- sentences_df$conf_llm[i]
      method[i] <- "llm"
    } else if (has_rule) {
      move[i] <- sentences_df$move_rule[i]
      step[i] <- sentences_df$step_rule[i]
      confidence[i] <- sentences_df$conf_rule[i]
      method[i] <- "rule"
    } else {
      move[i] <- "Unclassified"
      step[i] <- "Unclassified"
      confidence[i] <- 0
      method[i] <- "none"
    }
  }

  sentences_df$move <- move
  sentences_df$step <- step
  sentences_df$confidence <- round(confidence, 3)
  sentences_df$method <- method

  sentences_df
}


#' Aggregate consecutive sentences with the same move into blocks
#'
#' @param sentences_df Tibble with sentence-level classifications
#'
#' @return Tibble with move blocks
#'
#' @keywords internal
#' @noRd
aggregate_moves <- function(sentences_df) {
  if (nrow(sentences_df) == 0) {
    return(tibble::tibble(
      block_id = integer(0),
      section = character(0),
      move = character(0),
      step = character(0),
      start_sentence = integer(0),
      end_sentence = integer(0),
      n_sentences = integer(0),
      text_preview = character(0),
      avg_confidence = numeric(0)
    ))
  }

  blocks <- list()
  block_id <- 0

  current_section <- sentences_df$section[1]
  current_move <- sentences_df$move[1]
  current_step <- sentences_df$step[1]
  block_start <- 1
  block_confidences <- sentences_df$confidence[1]
  block_texts <- sentences_df$text[1]

  for (i in seq_len(nrow(sentences_df))[-1]) {
    same_block <- (
      sentences_df$section[i] == current_section &&
      sentences_df$move[i] == current_move &&
      sentences_df$step[i] == current_step
    )

    if (!same_block) {
      # Close current block
      block_id <- block_id + 1
      blocks[[block_id]] <- list(
        block_id = block_id,
        section = current_section,
        move = current_move,
        step = current_step,
        start_sentence = sentences_df$sentence_id[block_start],
        end_sentence = sentences_df$sentence_id[i - 1],
        n_sentences = i - block_start,
        text_preview = stringr::str_trunc(
          paste(block_texts, collapse = " "), 200
        ),
        avg_confidence = round(mean(block_confidences), 3)
      )

      # Start new block
      current_section <- sentences_df$section[i]
      current_move <- sentences_df$move[i]
      current_step <- sentences_df$step[i]
      block_start <- i
      block_confidences <- sentences_df$confidence[i]
      block_texts <- sentences_df$text[i]
    } else {
      block_confidences <- c(block_confidences, sentences_df$confidence[i])
      block_texts <- c(block_texts, sentences_df$text[i])
    }
  }

  # Close last block
  n <- nrow(sentences_df)
  block_id <- block_id + 1
  blocks[[block_id]] <- list(
    block_id = block_id,
    section = current_section,
    move = current_move,
    step = current_step,
    start_sentence = sentences_df$sentence_id[block_start],
    end_sentence = sentences_df$sentence_id[n],
    n_sentences = n - block_start + 1,
    text_preview = stringr::str_trunc(
      paste(block_texts, collapse = " "), 200
    ),
    avg_confidence = round(mean(block_confidences), 3)
  )

  tibble::tibble(
    block_id = vapply(blocks, `[[`, integer(1), "block_id"),
    section = vapply(blocks, `[[`, character(1), "section"),
    move = vapply(blocks, `[[`, character(1), "move"),
    step = vapply(blocks, `[[`, character(1), "step"),
    start_sentence = vapply(blocks, `[[`, integer(1), "start_sentence"),
    end_sentence = vapply(blocks, `[[`, integer(1), "end_sentence"),
    n_sentences = vapply(blocks, `[[`, integer(1), "n_sentences"),
    text_preview = vapply(blocks, `[[`, character(1), "text_preview"),
    avg_confidence = vapply(blocks, `[[`, numeric(1), "avg_confidence")
  )
}


#' Build summary statistics for rhetorical move analysis
#'
#' @param sentences_df Tibble with sentence-level classifications
#'
#' @return Named list with summary statistics
#'
#' @keywords internal
#' @noRd
build_move_summary <- function(sentences_df) {
  # Move distribution per section
  move_dist <- sentences_df %>%
    dplyr::filter(move != "Unclassified") %>%
    dplyr::count(section, move, name = "n") %>%
    dplyr::group_by(section) %>%
    dplyr::mutate(pct = round(n / sum(n) * 100, 1)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(section, move)

  # Step distribution
  step_dist <- sentences_df %>%
    dplyr::filter(move != "Unclassified") %>%
    dplyr::count(section, move, step, name = "n") %>%
    dplyr::group_by(section) %>%
    dplyr::mutate(pct = round(n / sum(n) * 100, 1)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(section, move, step)

  # Flow pattern per section (sequence of moves)
  flow <- tapply(sentences_df$move, sentences_df$section, function(moves) {
    # Collapse consecutive duplicates
    rle_result <- rle(moves)
    paste(rle_result$values, collapse = " -> ")
  })

  # Method breakdown
  method_breakdown <- sentences_df %>%
    dplyr::count(method, name = "n") %>%
    dplyr::mutate(pct = round(n / sum(n) * 100, 1))

  list(
    move_distribution = move_dist,
    step_distribution = step_dist,
    flow_pattern = as.list(flow),
    sections_analyzed = unique(sentences_df$section),
    total_sentences = nrow(sentences_df),
    method_breakdown = method_breakdown
  )
}


#' Create an empty rhetorical move result
#'
#' @return List with class "rhetorical_move_analysis" and empty tibbles
#'
#' @keywords internal
#' @noRd
empty_move_result <- function() {
  results <- list(
    sentences = tibble::tibble(
      sentence_id = integer(0),
      section = character(0),
      text = character(0),
      position_pct = numeric(0),
      move = character(0),
      step = character(0),
      confidence = numeric(0),
      method = character(0)
    ),
    move_blocks = tibble::tibble(
      block_id = integer(0),
      section = character(0),
      move = character(0),
      step = character(0),
      start_sentence = integer(0),
      end_sentence = integer(0),
      n_sentences = integer(0),
      text_preview = character(0),
      avg_confidence = numeric(0)
    ),
    summary = list(
      move_distribution = tibble::tibble(
        section = character(0), move = character(0),
        n = integer(0), pct = numeric(0)
      ),
      step_distribution = tibble::tibble(
        section = character(0), move = character(0),
        step = character(0), n = integer(0), pct = numeric(0)
      ),
      flow_pattern = list(),
      sections_analyzed = character(0),
      total_sentences = 0L,
      method_breakdown = tibble::tibble(
        method = character(0), n = integer(0), pct = numeric(0)
      )
    )
  )

  class(results) <- c("rhetorical_move_analysis", "list")
  results
}


#' Print method for rhetorical move analysis
#'
#' @param x A rhetorical_move_analysis object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#'
#' @export
print.rhetorical_move_analysis <- function(x, ...) {
  cat("=== Rhetorical Move Analysis ===\n\n")

  cat(sprintf("Sections analyzed: %s\n",
              paste(x$summary$sections_analyzed, collapse = ", ")))
  cat(sprintf("Total sentences: %d\n", x$summary$total_sentences))
  cat(sprintf("Move blocks: %d\n\n", nrow(x$move_blocks)))

  if (nrow(x$summary$move_distribution) > 0) {
    cat("Move distribution:\n")
    for (sect in unique(x$summary$move_distribution$section)) {
      cat(sprintf("  [%s]\n", sect))
      subset <- x$summary$move_distribution[
        x$summary$move_distribution$section == sect,
      ]
      for (j in seq_len(nrow(subset))) {
        cat(sprintf("    %s: %d (%.1f%%)\n",
                    subset$move[j], subset$n[j], subset$pct[j]))
      }
    }
    cat("\n")
  }

  if (length(x$summary$flow_pattern) > 0) {
    cat("Move flow per section:\n")
    for (sect in names(x$summary$flow_pattern)) {
      cat(sprintf("  [%s]: %s\n", sect, x$summary$flow_pattern[[sect]]))
    }
    cat("\n")
  }

  cat(sprintf("Classification methods: %s\n",
              paste(
                sprintf("%s=%d", x$summary$method_breakdown$method,
                        x$summary$method_breakdown$n),
                collapse = ", "
              )))

  invisible(x)
}
