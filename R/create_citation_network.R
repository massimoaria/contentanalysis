#' Create Citation Co-occurrence Network
#'
#' @description
#' Creates an interactive network visualization of citation co-occurrences within a document.
#' Citations that appear close to each other are connected, with the strength of the connection
#' based on their distance (in characters). Nodes are colored by the document section where
#' citations primarily appear.
#'
#' @param citation_analysis_results A list object returned by citation analysis functions,
#'   containing at least two elements:
#'   \itemize{
#'     \item \code{network_data}: A data frame with columns \code{citation1}, \code{citation2},
#'       and \code{distance} representing pairs of co-occurring citations
#'     \item \code{citations}: A data frame with columns \code{citation_text_clean} and
#'       \code{section} containing citation text and section information
#'     \item \code{section_colors}: A named vector of colors for each section
#'   }
#' @param max_distance Numeric. Maximum distance (in characters) between citations to be
#'   considered connected. Default is 1000.
#' @param min_connections Integer. Minimum number of connections a citation must have to be
#'   included in the network. Default is 1.
#' @param show_labels Logical. Whether to show citation labels on the network nodes.
#'   Default is TRUE.
#'
#' @return A \code{visNetwork} object representing the interactive citation network, or NULL
#'   if no valid network can be created. The returned object has an additional \code{stats}
#'   attribute containing:
#'   \itemize{
#'     \item \code{n_nodes}: Number of nodes in the network
#'     \item \code{n_edges}: Number of edges in the network
#'     \item \code{avg_distance}: Average distance between connected citations
#'     \item \code{max_distance}: Maximum distance parameter used
#'     \item \code{section_distribution}: Distribution of citations across sections
#'     \item \code{multi_section_citations}: Citations appearing in multiple sections
#'     \item \code{section_colors}: Color mapping for sections
#'   }
#'
#' @details
#' The function creates a network where:
#' \itemize{
#'   \item \strong{Nodes} represent unique citations
#'   \item \strong{Node size} is proportional to the number of connections
#'   \item \strong{Node color} indicates the primary section where the citation appears
#'   \item \strong{Node border} is thicker (3px) for citations appearing in multiple sections
#'   \item \strong{Edges} connect citations that co-occur within the specified distance
#'   \item \strong{Edge width} decreases with distance (closer citations = thicker edges)
#'   \item \strong{Edge color} indicates distance: red (<=300 chars), blue (<=600 chars),
#'     gray (>600 chars)
#' }
#'
#' The network uses the Fruchterman-Reingold layout algorithm for optimal node positioning.
#' Interactive features include zooming, panning, node dragging, and highlighting of
#' nearest neighbors on hover.
#'
#' @examples
#' \dontrun{
#' # Assuming you have citation_analysis_results from a previous analysis
#' network <- create_citation_network(
#'   citation_analysis_results,
#'   max_distance = 800,
#'   min_connections = 2,
#'   show_labels = TRUE
#' )
#'
#' # Display the network
#' network
#'
#' # Access network statistics
#' stats <- attr(network, "stats")
#' print(stats$n_nodes)
#' print(stats$section_distribution)
#' }
#'
#' @import dplyr
#' @import visNetwork
#' @import stringr
#' @importFrom igraph layout_nicely
#' @importFrom grDevices col2rgb
#' @importFrom stats na.omit
#'
#'
#' @export
create_citation_network <- function(
  citation_analysis_results,
  max_distance = 1000,
  min_connections = 1,
  show_labels = TRUE
) {
  network_data <- citation_analysis_results$network_data

  if (is.null(network_data) || nrow(network_data) == 0) {
    warning("No citation co-occurrence data found.")
    return(NULL)
  }

  # Filter by distance
  network_data_filtered <- network_data %>%
    filter(abs(distance) <= max_distance)

  if (nrow(network_data_filtered) == 0) {
    warning("No citation pairs found within the specified maximum distance.")
    return(NULL)
  }

  # Get unique citations
  all_citation_texts <- unique(c(
    network_data_filtered$citation1,
    network_data_filtered$citation2
  ))

  # Get section information - aggregate by citation
  citations_with_sections <- citation_analysis_results$citations %>%
    select(citation_text_clean, section) %>%
    group_by(citation_text_clean) %>%
    summarise(
      sections = paste(unique(section), collapse = ", "),
      n_sections = n_distinct(section),
      primary_section = first(section),
      .groups = "drop"
    )

  # Create nodes
  nodes <- data.frame(
    id = 1:length(all_citation_texts),
    citation_text = all_citation_texts,
    label = if (show_labels) str_trunc(all_citation_texts, 25) else "",
    stringsAsFactors = FALSE
  )

  # Add section information to nodes
  nodes <- nodes %>%
    left_join(
      citations_with_sections,
      by = c("citation_text" = "citation_text_clean")
    ) %>%
    mutate(
      sections = replace_na(sections, "Unknown"),
      primary_section = replace_na(primary_section, "Unknown"),
      n_sections = replace_na(n_sections, 1)
    )

  # Calculate connections
  node_connections <- rbind(
    data.frame(
      citation = network_data_filtered$citation1,
      stringsAsFactors = FALSE
    ),
    data.frame(
      citation = network_data_filtered$citation2,
      stringsAsFactors = FALSE
    )
  ) %>%
    count(citation, name = "connections")

  nodes$connections <- sapply(nodes$citation_text, function(cite) {
    conn <- node_connections$connections[node_connections$citation == cite]
    if (length(conn) == 0) {
      return(0)
    }
    return(conn[1])
  })

  # Filter by connections
  nodes <- nodes[nodes$connections >= min_connections, ]
  valid_citations <- nodes$citation_text
  network_data_filtered <- network_data_filtered %>%
    filter(citation1 %in% valid_citations & citation2 %in% valid_citations)

  if (nrow(network_data_filtered) == 0) {
    warning("No valid connections after filtering.")
    return(NULL)
  }

  # Set node properties
  nodes$size <- pmax(15, pmin(40, 15 + nodes$connections * 3)) / 2

  # Assign colors dynamically using section colors
  section_colors <- citation_analysis_results$section_colors

  # Ensure "Unknown" always has a gray color (add if missing, override if present)
  section_colors["Unknown"] <- "#CCCCCC"

  nodes$group <- nodes$primary_section

  # Add transparency to node colors (85% opacity)
  nodes$color <- sapply(
    section_colors[nodes$primary_section],
    function(hex_color) {
      # Convert hex to rgba with transparency
      rgb_vals <- col2rgb(hex_color)
      sprintf("rgba(%d, %d, %d, 0.85)", rgb_vals[1], rgb_vals[2], rgb_vals[3])
    }
  )

  # Add special border for multi-section nodes
  nodes$borderWidth <- ifelse(nodes$n_sections > 1, 3, 1)
  nodes$borderWidthSelected <- ifelse(nodes$n_sections > 1, 5, 2)

  # Create title with complete information
  nodes$title <- paste0(
    nodes$citation_text,
    "\n<br><b>Section(s):</b> ",
    nodes$sections,
    ifelse(
      nodes$n_sections > 1,
      paste0(" (", nodes$n_sections, " sections)"),
      ""
    ),
    "\n<br><b>Connections:</b> ",
    nodes$connections
  )

  nodes <- nodes %>%
    mutate(font.size = size, font.vadjust = -0.7 * font.size)

  # Create edges
  edges <- data.frame(
    from = sapply(network_data_filtered$citation1, function(cite) {
      nodes$id[nodes$citation_text == cite][1]
    }),
    to = sapply(network_data_filtered$citation2, function(cite) {
      nodes$id[nodes$citation_text == cite][1]
    }),
    distance = abs(network_data_filtered$distance),
    stringsAsFactors = FALSE
  )

  edges <- edges[!is.na(edges$from) & !is.na(edges$to), ]

  # Reduce edge width and add transparency
  edges$width <- pmax(0.5, 3 - (edges$distance / 200))

  # Colors with transparency (30% opacity for greater transparency)
  edges$color <- ifelse(
    edges$distance <= 300,
    "rgba(255, 111, 111, 0.3)",
    ifelse(
      edges$distance <= 600,
      "rgba(127, 179, 213, 0.3)",
      "rgba(204, 204, 204, 0.25)"
    )
  )

  edges$title <- paste("Distance:", edges$distance, "characters")

  # Create network with Fruchterman-Reingold layout
  network <- visNetwork(
    nodes,
    edges,
    type = "full",
    smooth = TRUE,
    physics = FALSE
  ) %>%
    visIgraphLayout(layout = "layout_nicely", type = "full")

  # Configure options
  network <- network %>%
    visOptions(highlightNearest = TRUE) %>%
    visInteraction(
      dragNodes = TRUE,
      dragView = TRUE,
      zoomView = TRUE,
      zoomSpeed = 0.2
    ) %>%
    visPhysics(enabled = FALSE) %>%
    visNodes(
      borderWidth = 1,
      borderWidthSelected = 2
    )

  # Add stats
  n_nodes <- nrow(nodes)
  n_edges <- nrow(edges)
  avg_distance <- round(mean(edges$distance), 1)

  # Statistics by section and multi-section
  section_stats <- nodes %>%
    count(primary_section) %>%
    arrange(desc(n))

  multi_section_citations <- nodes %>%
    filter(n_sections > 1) %>%
    select(citation_text, sections, n_sections)

  attr(network, "stats") <- list(
    n_nodes = n_nodes,
    n_edges = n_edges,
    avg_distance = avg_distance,
    max_distance = max_distance,
    section_distribution = section_stats,
    multi_section_citations = multi_section_citations,
    section_colors = section_colors
  )

  return(network)
}


#' Describe Citation Clusters by Section Using Reference Title N-grams
#'
#' @description
#' Generates textual descriptions of citation clusters grouped by document section.
#' For each section, the function extracts the most representative unigrams and bigrams
#' from the titles of cited references, ranked by TF-IDF score. This helps understand
#' the thematic focus of each section's bibliography.
#'
#' @param citation_analysis_results A list object returned by \code{analyze_scientific_content()},
#'   containing at least:
#'   \itemize{
#'     \item \code{citations}: A tibble with \code{citation_text_clean} and \code{section} columns
#'     \item \code{citation_references_mapping}: A tibble linking citations to references with
#'       \code{citation_text_clean}, \code{ref_full_text}, and \code{matched_ref_id}
#'     \item \code{parsed_references}: A tibble with \code{ref_id} and \code{ref_full_text}
#'   }
#' @param top_n Integer. Number of top terms to return per section. Default is 10.
#' @param min_word_length Integer. Minimum character length for tokens to be included.
#'   Default is 3.
#' @param remove_stopwords Logical. Whether to remove English stopwords. Default is TRUE.
#' @param ngram_range Integer vector of length 2. Range of n-gram sizes to extract.
#'   Default is \code{c(1, 2)} for unigrams and bigrams.
#' @param custom_stopwords Character vector. Additional stopwords to remove beyond the
#'   default English stopwords. Default is NULL.
#'
#' @return A list of class \code{"citation_cluster_description"} with:
#'   \itemize{
#'     \item \code{cluster_descriptions}: A tibble with columns \code{section}, \code{ngram},
#'       \code{n}, \code{tf}, \code{idf}, \code{tf_idf}, and \code{ngram_size} (1 or 2)
#'     \item \code{cluster_summary}: A tibble with one row per section containing
#'       \code{section}, \code{n_references}, and \code{top_terms} (comma-separated string)
#'     \item \code{cluster_references}: A tibble mapping \code{section} to \code{ref_full_text}
#'   }
#'
#' @details
#' The function works by:
#' \enumerate{
#'   \item Joining citations with their matched references, grouped by section
#'   \item Extracting the title portion from each reference's full text
#'   \item Tokenizing titles into unigrams and bigrams
#'   \item Computing TF-IDF scores where each section is treated as a document
#'   \item Selecting the top terms per section ranked by TF-IDF
#' }
#'
#' Title extraction uses the pattern after \code{(YYYY).} up to the next period.
#' If no year pattern is found, text after the first period is used as fallback.
#'
#' @examples
#' \dontrun{
#' results <- analyze_scientific_content(pdf_path)
#' cluster_desc <- describe_citation_clusters(results)
#'
#' # View top terms per section
#' cluster_desc$cluster_summary
#'
#' # View detailed TF-IDF scores
#' cluster_desc$cluster_descriptions
#' }
#'
#' @import dplyr
#' @importFrom tidytext unnest_tokens bind_tf_idf
#' @importFrom tidyr drop_na
#' @importFrom tibble tibble
#'
#' @export
describe_citation_clusters <- function(
    citation_analysis_results,
    top_n = 10,
    min_word_length = 3,
    remove_stopwords = TRUE,
    ngram_range = c(1, 2),
    custom_stopwords = NULL
) {
  # --- Input validation ---
  if (is.null(citation_analysis_results$citations) ||
      nrow(citation_analysis_results$citations) == 0) {
    warning("No citations data found in the analysis results.")
    return(NULL)
  }

  if (is.null(citation_analysis_results$citation_references_mapping) ||
      nrow(citation_analysis_results$citation_references_mapping) == 0) {
    warning("No citation-reference mapping found. Cannot describe clusters.")
    return(NULL)
  }

  # --- Step 1: Join citations to references by section ---
  citations_by_section <- citation_analysis_results$citations %>%
    select(citation_text_clean, section) %>%
    distinct()

  section_refs <- citations_by_section %>%
    inner_join(
      citation_analysis_results$citation_references_mapping %>%
        select(citation_text_clean, ref_full_text),
      by = "citation_text_clean"
    ) %>%
    select(section, ref_full_text) %>%
    distinct() %>%
    filter(!is.na(section), !is.na(ref_full_text))

  if (nrow(section_refs) == 0) {
    warning("No references could be mapped to sections.")
    return(NULL)
  }

  # Save cluster_references before modifying section_refs
  cluster_references <- section_refs

  # --- Step 2: Extract title portion from ref_full_text ---
  section_refs <- section_refs %>%
    mutate(
      ref_title = sapply(ref_full_text, extract_title_from_ref, USE.NAMES = FALSE)
    ) %>%
    filter(!is.na(ref_title), nchar(trimws(ref_title)) > 0)

  if (nrow(section_refs) == 0) {
    warning("No titles could be extracted from references.")
    return(NULL)
  }

  # --- Step 3: Clean title text BEFORE n-gram generation ---
  # Build the set of words to remove
  words_to_remove <- character(0)
  if (remove_stopwords) {
    words_to_remove <- tidytext::stop_words$word
    if (!is.null(custom_stopwords)) {
      words_to_remove <- c(words_to_remove, tolower(custom_stopwords))
    }
  }

  # Tokenize each title into individual words, remove noise, then
  # rebuild clean titles so that n-grams are generated from clean text only
  title_words <- section_refs %>%
    select(section, ref_full_text, ref_title) %>%
    tidytext::unnest_tokens(word, ref_title, token = "words") %>%
    filter(
      !word %in% words_to_remove,          # stopwords + custom
      nchar(word) >= min_word_length,       # short words
      !grepl("^[0-9]+$", word)             # pure numbers
    )

  if (nrow(title_words) == 0) {
    warning("No words remaining after filtering stopwords and noise.")
    return(NULL)
  }

  # Rebuild cleaned titles by pasting remaining words back together
  # (grouped by the original ref_full_text + section to preserve identity)
  clean_titles <- title_words %>%
    group_by(section, ref_full_text) %>%
    summarise(ref_title_clean = paste(word, collapse = " "), .groups = "drop")

  # --- Step 4: Generate n-grams from cleaned titles ---
  all_ngrams <- tibble()

  for (n in seq(ngram_range[1], ngram_range[2])) {
    ngrams_n <- clean_titles %>%
      select(section, ref_title_clean) %>%
      tidytext::unnest_tokens(ngram, ref_title_clean, token = "ngrams", n = n) %>%
      mutate(ngram_size = n)

    all_ngrams <- bind_rows(all_ngrams, ngrams_n)
  }

  if (nrow(all_ngrams) == 0) {
    warning("No n-grams could be extracted from reference titles.")
    return(NULL)
  }

  # --- Step 4: Compute TF-IDF ---
  ngram_counts <- all_ngrams %>%
    count(section, ngram, ngram_size)

  ngram_tfidf <- ngram_counts %>%
    tidytext::bind_tf_idf(ngram, section, n)

  # Select top_n per section
  cluster_descriptions <- ngram_tfidf %>%
    group_by(section) %>%
    arrange(desc(tf_idf), .by_group = TRUE) %>%
    slice_head(n = top_n) %>%
    ungroup() %>%
    arrange(section, desc(tf_idf))

  # --- Step 5: Build summary ---
  cluster_summary <- cluster_descriptions %>%
    group_by(section) %>%
    summarise(
      top_terms = paste(ngram, collapse = ", "),
      .groups = "drop"
    )

  # Add reference counts per section
  ref_counts <- cluster_references %>%
    group_by(section) %>%
    summarise(n_references = n_distinct(ref_full_text), .groups = "drop")

  cluster_summary <- cluster_summary %>%
    left_join(ref_counts, by = "section") %>%
    select(section, n_references, top_terms)

  # --- Build result ---
  result <- list(
    cluster_descriptions = cluster_descriptions,
    cluster_summary = cluster_summary,
    cluster_references = cluster_references
  )

  class(result) <- "citation_cluster_description"
  return(result)
}


#' Extract title from a reference full text string
#'
#' @param ref_text A single reference full text string
#' @return The extracted title, or NA if extraction fails
#' @keywords internal
#' @noRd
extract_title_from_ref <- function(ref_text) {
  if (is.na(ref_text) || nchar(trimws(ref_text)) == 0) {
    return(NA_character_)
  }

  # Try to extract title after year pattern: (YYYY). Title here. Journal...
  # Pattern: text after "(YYYY)." or "(YYYYa)." etc., up to the next period
  year_match <- regmatches(
    ref_text,
    regexpr("\\(\\d{4}[a-z]?\\)\\.\\s*", ref_text)
  )

  if (length(year_match) == 1 && nchar(year_match) > 0) {
    # Find position after the year pattern
    year_end <- regexpr("\\(\\d{4}[a-z]?\\)\\.\\s*", ref_text)
    start_pos <- attr(year_end, "match.length") + year_end
    remaining <- substring(ref_text, start_pos)

    # Title is up to the next period followed by a space or end
    title_match <- regmatches(remaining, regexpr("^[^.]+", remaining))
    if (length(title_match) == 1 && nchar(trimws(title_match)) > 0) {
      return(trimws(title_match))
    }
  }

  # Fallback: text after the first period (skip authors)
  parts <- strsplit(ref_text, "\\. ", fixed = FALSE)[[1]]
  if (length(parts) >= 2) {
    return(trimws(parts[2]))
  }

  return(NA_character_)
}
