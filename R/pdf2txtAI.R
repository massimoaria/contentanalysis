#' Process Content with Google Gemini AI
#'
#' Send images and/or documents to Google Gemini AI for content analysis and generation.
#' The function supports multiple file types including images (PNG, JPG, etc.) and documents
#' (PDF, TXT, HTML, CSV, RTF).
#'
#' @param image Character vector. Path(s) to image file(s) to be processed. Default is NULL.
#' @param docs Character vector. Path(s) to document file(s) to be processed. Default is NULL.
#' @param prompt Character. The prompt/instruction for the AI model.
#'   Default is "Explain these images".
#' @param model Character. The Gemini model version to use. Default is "2.5-flash".
#'   Options include "2.5-flash", etc.
#' @param image_type Character. The image MIME type. Default is "png".
#' @param retry_503 Integer. Number of retry attempts for HTTP 503 errors. Default is 5.
#' @param api_key Character. Google Gemini API key. If NULL, uses the
#'   GEMINI_API_KEY environment variable.
#' @param outputSize Character. Controls the maximum output tokens. Options are:
#'   \itemize{
#'     \item "small": 8,192 tokens
#'     \item "medium": 16,384 tokens (default)
#'     \item "large": 32,768 tokens
#'     \item "huge": 131,072 tokens
#'   }
#'
#' @return Character vector containing the AI-generated response(s), or an error message
#'   string starting with "ERROR:" if the request fails.
#'
#' @details
#' The function handles various error scenarios including:
#' \itemize{
#'   \item Missing or invalid files
#'   \item Invalid API keys (HTTP 400)
#'   \item Service unavailability (HTTP 503/429) with automatic retry
#'   \item File encoding errors
#' }
#'
#' Supported document types: PDF, TXT, HTML, CSV, RTF
#'
#' @examples
#' \dontrun{
#' # Process an image
#' result <- gemini_content_ai(
#'   image = "path/to/image.png",
#'   prompt = "Describe this image in detail"
#' )
#'
#' # Process a PDF document
#' result <- gemini_content_ai(
#'   docs = "path/to/document.pdf",
#'   prompt = "Summarize this document",
#'   outputSize = "large"
#' )
#'
#' # Process multiple images and documents
#' result <- gemini_content_ai(
#'   image = c("img1.png", "img2.png"),
#'   docs = c("doc1.pdf", "doc2.txt"),
#'   prompt = "Compare these materials"
#' )
#' }
#'
#' @export
#' @importFrom httr2 request req_url_query req_headers req_body_json req_perform resp_body_json
#' @importFrom base64enc base64encode
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_extract
gemini_content_ai <- function(
  image = NULL,
  docs = NULL,
  prompt = "Explain these images",
  model = "2.5-flash",
  image_type = "png",
  retry_503 = 5,
  api_key = NULL,
  outputSize = "medium"
) {
  mime_doc_types <- list(
    pdf = "application/pdf",
    txt = "text/plain",
    html = "text/html",
    csv = "text/csv",
    rtf = "text/rtf"
  )

  switch(
    outputSize,
    "small" = {
      generation_config <- list(
        temperature = 1,
        maxOutputTokens = 8192,
        topP = 0.95,
        topK = 40,
        seed = 1234
      )
    },
    "medium" = {
      generation_config <- list(
        temperature = 1,
        maxOutputTokens = 16384, #8192,
        topP = 0.95,
        topK = 40,
        seed = 1234
      )
    },
    "large" = {
      generation_config <- list(
        temperature = 1,
        maxOutputTokens = 32768, #8192,
        topP = 0.95,
        topK = 40,
        seed = 1234
      )
    },
    "huge" = {
      generation_config <- list(
        temperature = 1,
        maxOutputTokens = 131072, #8192,
        topP = 0.95,
        topK = 40,
        seed = 1234
      )
    }
  )

  # # Default config
  # generation_config <- list(
  #   temperature = 1,
  #   maxOutputTokens = 16384,#8192,
  #   topP = 0.95,
  #   topK = 40,
  #   seed = 1234
  # )

  # Build URL
  model_query <- paste0("gemini-", model, ":generateContent")
  url <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    model_query
  )
  if (is.null(api_key)) {
    api_key <- Sys.getenv("GEMINI_API_KEY")
  }

  # Base structure of parts
  parts <- list(list(text = prompt))

  # Handle images if provided
  if (!is.null(image)) {
    if (!is.vector(image)) {
      image <- as.vector(image)
    }
    mime_type <- paste0("image/", image_type)

    for (img_path in image) {
      if (!file.exists(img_path)) {
        return(paste0("ERROR: Image file does not exist: ", img_path))
      }

      image_data <- tryCatch(
        base64enc::base64encode(img_path),
        error = function(e) {
          return(NULL)
        }
      )

      if (is.null(image_data)) {
        return(paste0("ERROR: Failed to encode image: ", img_path))
      }

      parts <- append(
        parts,
        list(
          list(
            inline_data = list(
              mime_type = mime_type,
              data = image_data
            )
          )
        )
      )
    }
  }

  # Handle documents if provided
  if (!is.null(docs)) {
    if (!is.vector(docs)) {
      docs <- as.vector(docs)
    }
    for (doc_path in docs) {
      if (!file.exists(doc_path)) {
        return(paste0("ERROR: Document file does not exist: ", doc_path))
      }

      doc_data <- tryCatch(
        base64enc::base64encode(doc_path),
        error = function(e) {
          return(NULL)
        }
      )

      if (is.null(doc_data)) {
        return(paste0("ERROR: Failed to encode document: ", doc_path))
      }

      doc_type <- tools::file_ext(doc_path) %>% tolower()

      if (doc_type %in% names(mime_doc_types)) {
        mime_type <- mime_doc_types[[doc_type]]
      } else {
        mime_type <- "application/pdf" # Default to PDF if unknown type
      }

      parts <- append(
        parts,
        list(
          list(
            inline_data = list(
              mime_type = "application/pdf",
              data = doc_data
            )
          )
        )
      )
    }
  }

  # Assemble request body
  request_body <- list(
    contents = list(
      parts = parts
    ),
    generationConfig = generation_config
  )

  # Retry loop
  for (attempt in seq_len(retry_503)) {
    # Build and send request
    req <- request(url) %>%
      req_url_query(key = api_key) %>%
      req_headers("Content-Type" = "application/json") %>%
      req_body_json(request_body)

    resp <- tryCatch(
      req_perform(req),
      error = function(e) {
        return(list(
          status_code = stringr::str_extract(e$message, "(?<=HTTP )\\d+") %>%
            as.numeric(),
          error = TRUE,
          message = paste("ERROR: Request failed with error:", e$message)
        ))
      }
    )

    # # Handle connection-level error
    # if (is.list(resp) && isTRUE(resp$error)) {
    #   return(resp$message)
    # }

    # Retry on HTTP 503 or 429
    if (resp$status_code %in% c(429, 503)) {
      if (attempt < retry_503) {
        message(paste0(
          "WARNING: HTTP 503 (Service Unavailable) - retrying in 2 seconds (attempt ",
          attempt,
          "/",
          retry_503,
          ")..."
        ))
        Sys.sleep(2)
        next
      } else {
        return(
          paste0(
            "ERROR: HTTP 503: Service Unavailable.\n",
            "The Google Gemini servers are currently overloaded or under maintenance.\n",
            "All retry attempts failed (",
            retry_503,
            "). Please try again in a few minutes. Alternatively, consider using a different AI model with lower latency."
          )
        )
      }
    }

    # HTTP errors
    # 400 api key not valid
    if (resp$status_code == 400) {
      msg <- tryCatch(
        {
          parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp))
          parsed$error$message
        },
        error = function(e) {
          "Please check your API key. It seems to be not valid!"
        }
      )
      return(paste0("ERROR: HTTP ", resp$status_code, ": ", msg))
    }
    # Other HTTP errors
    if (resp$status_code != 200) {
      msg <- tryCatch(
        {
          parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp))
          parsed$error$message
        },
        error = function(e) {
          "Service unavailable or unexpected error. Please check your API key and usage limit."
        }
      )

      return(paste0("ERROR: HTTP ", resp$status_code, ": ", msg))
    }

    # Successful response
    candidates <- httr2::resp_body_json(resp)$candidates
    outputs <- unlist(lapply(candidates, \(c) c$content$parts))
    return(outputs[1])
  }
}

#' Process Large PDF Documents with Google Gemini AI
#'
#' Split a large PDF into chunks and process each chunk with Google Gemini AI to extract
#' and format text content. Particularly useful for PDFs that exceed the token limit
#' of a single API request.
#'
#' @param pdf_path Character. Path to the PDF file to be processed.
#' @param api_key Character. Google Gemini API key.
#' @param pages_per_chunk Integer. Number of pages to include in each chunk. Default is 4.
#'   Lower values may help with very dense documents or API rate limits.
#' @param model Character. The Gemini model version to use. Options are:
#'  "2.5-flash" and "2.5-flash-lite. Default is "2.5-flash".
#'
#' @return List of character vectors, one element per chunk, containing the extracted
#'   and formatted text in markdown format. Returns NULL if processing fails.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates input parameters and PDF file
#'   \item Splits the PDF into chunks based on \code{pages_per_chunk}
#'   \item Processes each chunk sequentially with Gemini AI
#'   \item Extracts text while:
#'     \itemize{
#'       \item Removing repeated headers
#'       \item Maintaining hierarchical structure
#'       \item Preserving reference numbers in bracket notation
#'       \item Formatting output as markdown
#'       \item Handling sections that span multiple chunks
#'     }
#'   \item Returns a list of extracted text, one element per chunk
#' }
#'
#' The function includes comprehensive error handling for:
#' \itemize{
#'   \item Invalid or missing PDF files
#'   \item Missing or invalid API keys
#'   \item PDF processing errors
#'   \item Gemini AI service errors
#'   \item File system operations
#' }
#'
#' Rate limiting: The function includes a 1-second delay between chunks to respect
#' API rate limits.
#'
#' @note
#' \itemize{
#'   \item Requires \code{pdftools} package for PDF manipulation
#'   \item Uses temporary files that are automatically cleaned up
#'   \item Progress messages are printed for each chunk
#'   \item All warnings are captured and reported
#' }
#'
#' @examples
#' \dontrun{
#' # Process a large PDF with default settings
#' result <- process_large_pdf(
#'   pdf_path = "large_document.pdf",
#'   api_key = Sys.getenv("GEMINI_API_KEY")
#' )
#'
#' # Process with smaller chunks and specific model
#' result <- process_large_pdf(
#'   pdf_path = "very_large_document.pdf",
#'   api_key = Sys.getenv("GEMINI_API_KEY"),
#'   pages_per_chunk = 3,
#'   model = "2.5-flash"
#' )
#'
#' # Combine all chunks into single text
#' if (!is.null(result)) {
#'   full_text <- paste(unlist(result), collapse = "\n\n")
#' }
#' }
#'
#' @seealso \code{\link{gemini_content_ai}} for the underlying AI processing function
#'
#' @export
#' @importFrom pdftools pdf_info pdf_subset
process_large_pdf <- function(
  pdf_path,
  api_key,
  pages_per_chunk = 4,
  model = c("2.5-flash", "2.5-flash-lite")
) {
  # Wrap entire function in tryCatch
  tryCatch(
    {
      # Validate inputs
      if (is.null(pdf_path) || !file.exists(pdf_path)) {
        warning("PDF file does not exist: ", pdf_path)
        return(NULL)
      }

      if (is.null(api_key) || api_key == "") {
        warning("API key is missing or empty")
        return(NULL)
      }

      if (!is.numeric(pages_per_chunk) || pages_per_chunk < 1) {
        warning("Invalid pages_per_chunk value. Must be a positive integer.")
        return(NULL)
      }

      prompt_first <- "Analyze this PDF document and extract all the text following these instructions:
1. Identify and remove repeated headers on each page
2. Organize the content into logical sections
3. Maintain the hierarchical structure (titles, subtitles)
4. Return the result in markdown format with:
   - # for main titles
   - ## for subtitles
   - Paragraphs separated by blank lines
5. IMPORTANT: Mantain the superscript numbers for references in the text reporting them as [1], [2], etc.
6. IMPORTANT: This PDF is a chunk of a larger file. The first page might contain part of a section that started in a previous request. Include all sections in the output, even if they are incomplete or start mid-section. Do not skip content from sections that appear to have started earlier.
Do not include page numbers or repetitive header elements."

      prompt_continue <- "Analyze this PDF document and extract all the text following these instructions:
  1. Identify and remove repeated headers on each page
  2. Organize the content into logical sections
  3. Maintain the hierarchical structure (titles, subtitles)
  4. Return the result in markdown format with:
     - # for main titles
     - ## for subtitles
     - Paragraphs separated by blank lines
  5. IMPORTANT: Mantain the superscript numbers for references in the text reporting them as [1], [2], etc.
  6. IMPORTANT: This PDF is a chunk of a larger file. The first page might contain part of a section that started in a previous request. Include all sections in the output, even if they are incomplete or start mid-section. Do not skip content from sections that appear to have started earlier.
  7. Remove titles including journal name (year), author names, etc. from the page if they appear to be part of a previous section.
  Do not include page numbers or repetitive header elements."

      # Get PDF info with error handling
      pdf_info_result <- tryCatch(
        {
          pdftools::pdf_info(pdf_path)
        },
        error = function(e) {
          warning("Failed to read PDF info: ", e$message)
          return(NULL)
        }
      )

      if (is.null(pdf_info_result)) {
        return(NULL)
      }

      n_pages <- pdf_info_result$pages

      if (is.null(n_pages) || n_pages < 1) {
        warning("PDF has no pages or invalid page count")
        return(NULL)
      }

      # Divide PDF file in chunks
      chunks <- split(1:n_pages, ceiling(1:n_pages / pages_per_chunk))
      all_text <- list()

      # Process each chunk with error handling
      for (i in seq_along(chunks)) {
        message(sprintf("Processing PDF chunk %d/%d", i, length(chunks)))

        # Extract temp chunk with error handling
        temp_pdf <- tempfile(fileext = ".pdf")

        subset_result <- tryCatch(
          {
            pdftools::pdf_subset(
              pdf_path,
              pages = chunks[[i]],
              output = temp_pdf
            )
            TRUE
          },
          error = function(e) {
            warning(
              "Failed to create PDF subset for chunk ",
              i,
              ": ",
              e$message
            )
            return(FALSE)
          }
        )

        if (!subset_result || !file.exists(temp_pdf)) {
          warning("Failed to create temporary PDF for chunk ", i)
          # Clean up and return NULL
          if (file.exists(temp_pdf)) {
            unlink(temp_pdf)
          }
          return(NULL)
        }

        # Select prompt based on chunk position
        if (i > 1) {
          prompt <- prompt_continue
        } else {
          prompt <- prompt_first
        }

        # Process with Gemini AI with error handling
        text <- tryCatch(
          {
            gemini_content_ai(
              image = NULL,
              docs = temp_pdf,
              prompt = prompt,
              model = model,
              image_type = "png",
              retry_503 = 5,
              api_key = api_key,
              outputSize = "huge"
            )
          },
          error = function(e) {
            warning("Gemini AI failed for chunk ", i, ": ", e$message)
            return(NULL)
          }
        )

        # Check if text extraction was successful
        if (is.null(text)) {
          warning("Text extraction returned NULL for chunk ", i)
          unlink(temp_pdf)
          return(NULL)
        }

        # Check if the result is an error message (starts with ERROR:
        if (
          is.character(text) && length(text) > 0 && grepl("^ERROR:", text[1])
        ) {
          warning("Gemini AI error for chunk ", i, ": ", text[1])
          unlink(temp_pdf)
          return(NULL)
        }

        # Store successful result
        all_text[[i]] <- text

        # Clean up temporary file
        unlink(temp_pdf)

        # Rate limiting
        Sys.sleep(1)
      }

      # Verify we have results for all chunks
      if (length(all_text) != length(chunks)) {
        warning("Not all chunks were processed successfully")
        return(NULL)
      }

      # Return the collected text
      return(all_text)
    },
    error = function(e) {
      # Catch-all error handler
      warning("Unexpected error in process_large_pdf: ", e$message)
      return(NULL)
    }
  )
}


#' Merge Text Chunks into Named Sections
#'
#' @description Takes a list of markdown text chunks and merges them into named sections.
#'   Each section name is extracted from the markdown header (# Title).
#'
#' @param text_chunks A list of character strings with markdown text from sequential PDF chunks
#' @param remove_tables Logical. If TRUE, removes all table content including captions. Default is FALSE.
#' @param remove_figure_captions Logical. If TRUE, removes figure captions. Default is FALSE.
#'
#' @return A named character vector where:
#'   - Names are section titles (without the # symbol)
#'   - Values are complete section contents (including the title line)
#'
#' @export
merge_text_chunks_named <- function(
  text_chunks,
  remove_tables = TRUE,
  remove_figure_captions = TRUE
) {
  # Combine all chunks into a single text
  full_text <- paste(text_chunks, collapse = "\n")

  # Remove markdown code block markers
  full_text <- remove_code_blocks(full_text)

  # Remove tables if requested
  if (remove_tables) {
    full_text <- remove_all_tables(full_text)
  }

  # Remove figure captions if requested
  if (remove_figure_captions) {
    full_text <- remove_figure_caps(full_text)
  }

  # Split into lines
  lines <- strsplit(full_text, "\n", fixed = TRUE)[[1]]

  # Find section headers (lines starting with "#" or "##")
  section_starts <- grep("^##? [A-Z]", lines)

  # Check section validity (#): First row or previous row empty
  is_valid <- !grepl("^# ", lines[section_starts]) |
    section_starts == 1 |
    c(FALSE, lines[section_starts[-1] - 1] == "")

  section_starts <- section_starts[is_valid]

  if (length(section_starts) == 0) {
    warning("No section headers found. Returning full content.")
    return(c("Full Content" = trimws(full_text)))
  }

  # Initialize named vector
  sections <- character()

  # Extract each section
  for (i in seq_along(section_starts)) {
    start_idx <- section_starts[i]

    # Determine end index (before next section starts)
    if (i < length(section_starts)) {
      end_idx <- section_starts[i + 1] - 1
    } else {
      end_idx <- length(lines)
    }

    # Extract title (remove leading "# " and trim)
    title_line <- lines[start_idx]
    title <- gsub("^# ", "", title_line)
    title <- gsub("^## ", "", title)
    title <- trimws(title)

    # Extract full section content
    section_lines <- lines[start_idx:end_idx]
    section_text <- paste(section_lines, collapse = "\n")

    # Clean up excessive blank lines (more than 2 consecutive)
    section_text <- gsub("\n{3,}", "\n\n", section_text)

    # Trim leading/trailing whitespace
    section_text <- trimws(section_text)

    # Add to named vector
    sections[title] <- section_text
  }

  # Handle preamble (content before first section)
  if (section_starts[1] > 1) {
    preamble_lines <- lines[1:(section_starts[1] - 1)]
    preamble <- paste(preamble_lines, collapse = "\n")
    preamble <- trimws(preamble)

    # Only include preamble if it contains substantial content
    if (nchar(preamble) > 10) {
      sections <- c("Preamble" = preamble, sections)
    }
  }

  return(sections)
}


# Helper function to remove code block markers
remove_code_blocks <- function(text) {
  #' Remove Markdown Code Block Markers
  #'
  #' @param text Character string containing markdown text
  #' @return Character string with ```markdown and ``` markers removed

  # Remove ```markdown, ```r, ```python, etc. and closing ```
  text <- gsub("```[a-zA-Z]*\n?", "", text)
  text <- gsub("```", "", text)

  return(text)
}


# Helper function to remove all types of tables
remove_all_tables <- function(text) {
  #' Remove All Types of Tables (Markdown and Plain Text)
  #'
  #' @param text Character string containing text with tables
  #' @return Character string with all tables and table captions removed

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]

  # Identify different types of table indicators
  markdown_table_lines <- grepl("\\|", lines) # Markdown tables with |
  table_separator_lines <- grepl("^\\s*[-|]+\\s*$", lines) # Separator lines like ---|---
  table_caption_lines <- grepl(
    "^\\s*(\\*\\*)?Table\\s+\\d+",
    lines,
    ignore.case = TRUE
  )

  # Detect plain text tables by looking for aligned columns
  # Typically have patterns like "word | word" or "---|---" or multiple spaces for alignment
  potential_table_lines <- grepl("\\s{3,}", lines) | grepl("[-]{3,}", lines)

  lines_to_remove <- logical(length(lines))
  in_table <- FALSE
  empty_line_counter <- 0

  for (i in seq_along(lines)) {
    current_line <- trimws(lines[i])

    # Check if this is a table caption
    if (table_caption_lines[i]) {
      in_table <- TRUE
      lines_to_remove[i] <- TRUE
      empty_line_counter <- 0
      next
    }

    # Check if we're in a markdown table
    if (markdown_table_lines[i] || table_separator_lines[i]) {
      in_table <- TRUE
      lines_to_remove[i] <- TRUE
      empty_line_counter <- 0
      next
    }

    # If we're in a table
    if (in_table) {
      # Empty line might signal end of table, but wait for confirmation
      if (nchar(current_line) == 0) {
        lines_to_remove[i] <- TRUE
        empty_line_counter <- empty_line_counter + 1

        # After 2 empty lines, table is definitely over
        if (empty_line_counter >= 2) {
          in_table <- FALSE
          empty_line_counter <- 0
        }
      } else if (potential_table_lines[i] || markdown_table_lines[i]) {
        # Check if line looks like table content (aligned data)
        lines_to_remove[i] <- TRUE
        empty_line_counter <- 0
      } else if (!grepl("^\\s*$", current_line)) {
        # Regular text line after table content - table has ended
        in_table <- FALSE
        empty_line_counter <- 0
      }
    }
  }

  # Second pass: remove isolated table-like structures
  # (tables without explicit captions)
  for (i in seq_along(lines)) {
    if (lines_to_remove[i]) {
      next
    }

    # If a line has the markdown table separator pattern
    if (
      grepl("^\\s*[-:|]+\\s*$", lines[i]) ||
        grepl("^[\\s]*[|][-:|\\s]+[|][\\s]*$", lines[i])
    ) {
      # Mark previous and next lines if they look like table content
      if (i > 1 && !lines_to_remove[i - 1] && grepl("\\|", lines[i - 1])) {
        lines_to_remove[i - 1] <- TRUE
      }
      lines_to_remove[i] <- TRUE
      if (
        i < length(lines) &&
          !lines_to_remove[i + 1] &&
          grepl("\\|", lines[i + 1])
      ) {
        lines_to_remove[i + 1] <- TRUE
      }
    }
  }

  # Remove identified lines
  cleaned_lines <- lines[!lines_to_remove]

  # Reconstruct text
  cleaned_text <- paste(cleaned_lines, collapse = "\n")

  return(cleaned_text)
}


# Helper function to remove figure captions
remove_figure_caps <- function(text) {
  #' Remove Figure Captions
  #'
  #' @param text Character string containing markdown text
  #' @return Character string with figure captions removed

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]

  lines_to_remove <- logical(length(lines))
  in_caption <- FALSE

  for (i in seq_along(lines)) {
    line <- trimws(lines[i])

    # Check if line starts a figure caption
    # Patterns: "Figure N:", "Fig. N:", "Figure N.", "Fig N."
    if (
      grepl(
        "^\\s*(\\*\\*)?(Fig\\.|Figure)\\s+\\d+[:.\\s]",
        line,
        ignore.case = TRUE
      )
    ) {
      lines_to_remove[i] <- TRUE
      in_caption <- TRUE

      # Check if caption ends on the same line
      if (grepl("[.!]\\s*$", line) || nchar(line) < 100) {
        in_caption <- FALSE
      }
    } else if (in_caption) {
      # Continue removing lines if we're in a multi-line caption
      lines_to_remove[i] <- TRUE

      # Caption ends at sentence end or empty line
      if (grepl("[.!]\\s*$", line) || nchar(line) == 0) {
        in_caption <- FALSE
      }
    }
  }

  # Remove identified lines
  cleaned_lines <- lines[!lines_to_remove]

  # Reconstruct text
  cleaned_text <- paste(cleaned_lines, collapse = "\n")

  return(cleaned_text)
}

text_AI_conversion <- function(x, citation_type = "author_year") {
  convert_superscript_citations <- function(text) {
    # Pattern 1: Single number after punctuation at end of sentence
    # Example: "...as shown before. 15 The next..." -> "...as shown before.[15] The next..."
    text <- gsub(
      "([.!?])\\s+(\\d{1,3})\\s+([A-Z])",
      "\\1[\\2] \\3",
      text,
      perl = TRUE
    )

    # Pattern 2: Single or multiple numbers after a word (no space or minimal space)
    # Example: "study 5" or "study5" -> "study[5]"
    # Handles: single numbers, comma-separated, ranges with dash
    text <- gsub(
      "([a-z])\\s+(\\d{1,3}(?:[,\\-]\\d{1,3})*)(?=\\s|[.,;!?]|$)",
      "\\1[\\2]",
      text,
      perl = TRUE
    )

    # Pattern 3: Multiple citation numbers with various separators
    # Example: "evidence 1, 2, 3" -> "evidence[1,2,3]"
    text <- gsub(
      "([a-z])\\s+(\\d{1,3}(?:\\s*[,;]\\s*\\d{1,3})+)(?=\\s|[.,;!?]|$)",
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

    # Clean up: remove extra spaces inside brackets
    # Example: "[1, 2, 3]" -> "[1,2,3]"
    text <- gsub("\\[\\s+", "[", text, perl = TRUE)
    text <- gsub("\\s+\\]", "]", text, perl = TRUE)
    text <- gsub("\\s*,\\s*", ",", text, perl = TRUE)

    # Clean up: convert "e" to "-" inside brackets (OCR artifact)
    text <- gsub("\\[(\\d+)e(\\d+)\\]", "[\\1-\\2]", text, perl = TRUE)

    # Pattern 6: Handle cases where number appears at end of incomplete word
    # This catches cases where PDF extraction merged citation with word
    # Example: "studies15" -> "studies[15]"
    text <- gsub(
      "([a-z]{3,})(\\d{1,3})(?=\\s|[.,;!?]|$)",
      "\\1[\\2]",
      text,
      perl = TRUE
    )

    return(text)
  }
  ind <- names(x)

  for (i in ind) {
    txt <- x[[i]]

    tryCatch({
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

      txt <- gsub("-\\s", "", txt)

      # Convert superscript citations ONLY if citation_type is "numeric_superscript"
      if (citation_type == "numeric_superscript") {
        txt <- convert_superscript_citations(txt)
      }
      x[[i]] <- txt
    })
  }

  return(x)
}
