#' Build the LLM prompt for rhetorical move classification
#'
#' @param sentences Character vector of sentences to classify
#' @param taxonomy_type Character string: "introduction", "literature_review", or "discussion"
#'
#' @return Character string with the formatted prompt
#'
#' @keywords internal
#' @noRd
build_move_prompt <- function(sentences, taxonomy_type) {
  taxonomies <- get_move_taxonomies()
  taxonomy <- taxonomies[[taxonomy_type]]

  # Format taxonomy as readable text
  tax_text <- paste0("SECTION TYPE: ", taxonomy$name, "\n\nTAXONOMY:\n")
  for (move_def in taxonomy$moves) {
    tax_text <- paste0(
      tax_text,
      move_def$move, ": ", move_def$move_label, "\n"
    )
    for (step_def in move_def$steps) {
      tax_text <- paste0(tax_text, "  - ", step_def$step, "\n")
    }
  }

  # Format sentences
  sentence_text <- paste0(
    seq_along(sentences), ': "', sentences, '"',
    collapse = "\n"
  )

  prompt <- paste0(
    'You are an expert in academic discourse analysis and rhetorical move analysis ',
    '(Swales CARS model and extensions). ',
    'Classify each sentence below according to its rhetorical move in a scientific paper.\n\n',
    tax_text,
    '\nFor each sentence, determine its rhetorical move and step from the taxonomy above.\n',
    'Return ONLY a valid JSON array (no markdown, no explanation) with objects containing:\n',
    '- "id": the sentence number (integer)\n',
    '- "move": the move code (e.g., "M1", "M2", "M3")\n',
    '- "move_label": the move label (e.g., "Establishing a territory")\n',
    '- "step": the step name (e.g., "Claiming centrality", "Indicating a gap")\n',
    '- "confidence": your confidence from 0.0 to 1.0\n\n',
    'SENTENCES:\n',
    sentence_text,
    '\n\nReturn ONLY the JSON array.'
  )

  prompt
}


#' Parse LLM JSON response for move classification
#'
#' @param response Character string. Raw LLM response.
#' @param n_sentences Integer. Expected number of sentences.
#'
#' @return Data frame with columns: id, move, move_label, step, confidence.
#'   Returns NULL if parsing fails.
#'
#' @keywords internal
#' @noRd
parse_move_response <- function(response, n_sentences) {
  if (is.null(response) || length(response) == 0) return(NULL)
  if (grepl("^ERROR:", response[1])) return(NULL)

  # Clean response: remove markdown code block markers if present
  clean <- gsub("```json\\s*", "", response)
  clean <- gsub("```\\s*", "", clean)
  clean <- trimws(clean)

  # Try to extract JSON array
  # Find the first [ and last ]
  start <- regexpr("\\[", clean)
  end <- max(gregexpr("\\]", clean)[[1]])

  if (start == -1 || end == -1) return(NULL)

  json_str <- substr(clean, start, end)

  parsed <- tryCatch(
    jsonlite::fromJSON(json_str, simplifyDataFrame = TRUE),
    error = function(e) NULL
  )

  if (is.null(parsed) || !is.data.frame(parsed)) return(NULL)

  # Validate required columns

  required <- c("id", "move", "step", "confidence")
  if (!all(required %in% names(parsed))) return(NULL)

  # Ensure proper types
  parsed$id <- as.integer(parsed$id)
  parsed$confidence <- as.numeric(parsed$confidence)
  parsed$confidence <- pmin(1.0, pmax(0.0, parsed$confidence))

  # Add move_label if missing

  if (!"move_label" %in% names(parsed)) {
    parsed$move_label <- NA_character_
  }

  parsed
}


#' Apply LLM-based rhetorical move classification via Gemini
#'
#' Sends sentences in batches to Gemini API for classification.
#'
#' @param sentences_df Tibble with columns: sentence_id, section, text, taxonomy_type
#' @param api_key Character. Gemini API key.
#' @param model Character. Gemini model version.
#' @param batch_size Integer. Number of sentences per API call.
#'
#' @return The input tibble with added columns: move_llm, step_llm, conf_llm
#'
#' @keywords internal
#' @noRd
apply_llm_based_moves <- function(
  sentences_df,
  api_key = NULL,
  model = "2.5-flash",
  batch_size = 20
) {
  if (is.null(api_key) || api_key == "") {
    api_key <- Sys.getenv("GEMINI_API_KEY")
  }

  if (is.null(api_key) || api_key == "") {
    message("No Gemini API key found. Skipping LLM classification.")
    sentences_df$move_llm <- NA_character_
    sentences_df$step_llm <- NA_character_
    sentences_df$conf_llm <- 0
    return(sentences_df)
  }

  # Initialize output columns
  sentences_df$move_llm <- NA_character_
  sentences_df$step_llm <- NA_character_
  sentences_df$conf_llm <- 0

  # Process by taxonomy type to use appropriate prompts
  taxonomy_types <- unique(sentences_df$taxonomy_type)
  taxonomy_types <- taxonomy_types[!is.na(taxonomy_types)]

  for (tax_type in taxonomy_types) {
    idx <- which(sentences_df$taxonomy_type == tax_type)
    if (length(idx) == 0) next

    subset_df <- sentences_df[idx, ]

    # Split into batches
    n_batches <- ceiling(nrow(subset_df) / batch_size)
    batch_indices <- split(
      seq_len(nrow(subset_df)),
      ceiling(seq_len(nrow(subset_df)) / batch_size)
    )

    message(sprintf(
      "LLM classification: %s (%d sentences, %d batches)",
      tax_type, nrow(subset_df), n_batches
    ))

    for (b in seq_along(batch_indices)) {
      batch_idx <- batch_indices[[b]]
      batch_sentences <- subset_df$text[batch_idx]

      prompt <- build_move_prompt(batch_sentences, tax_type)

      # Call Gemini API
      response <- tryCatch(
        gemini_content_ai(
          prompt = prompt,
          model = model,
          api_key = api_key,
          outputSize = "medium"
        ),
        error = function(e) {
          message(sprintf("Gemini API error (batch %d): %s", b, e$message))
          NULL
        }
      )

      # Parse response
      parsed <- parse_move_response(response, length(batch_sentences))

      if (!is.null(parsed)) {
        # Map parsed results back to original indices
        for (j in seq_len(nrow(parsed))) {
          local_idx <- parsed$id[j]
          if (is.na(local_idx) || local_idx < 1 || local_idx > length(batch_idx)) next

          orig_row <- idx[batch_idx[local_idx]]

          move_code <- parsed$move[j]
          move_label <- if (!is.na(parsed$move_label[j])) {
            parsed$move_label[j]
          } else {
            # Look up the label from taxonomy
            lookup_move_label(move_code, tax_type)
          }

          sentences_df$move_llm[orig_row] <- paste0(move_code, ": ", move_label)
          sentences_df$step_llm[orig_row] <- parsed$step[j]
          sentences_df$conf_llm[orig_row] <- parsed$confidence[j]
        }
      } else {
        message(sprintf("  Batch %d/%d: JSON parsing failed, using rule-based fallback", b, n_batches))
      }

      # Rate limiting between batches
      if (b < length(batch_indices)) {
        Sys.sleep(1)
      }
    }
  }

  sentences_df
}


#' Look up move label from taxonomy
#'
#' @param move_code Character (e.g., "M1", "M2")
#' @param taxonomy_type Character
#'
#' @return Character string with the move label, or the move_code if not found
#'
#' @keywords internal
#' @noRd
lookup_move_label <- function(move_code, taxonomy_type) {
  taxonomies <- get_move_taxonomies()
  taxonomy <- taxonomies[[taxonomy_type]]

  if (is.null(taxonomy)) return(move_code)

  for (move_def in taxonomy$moves) {
    if (move_def$move == move_code) {
      return(move_def$move_label)
    }
  }

  move_code
}
