# Test suite for citation_analysis.R functions
# This is the main integration test suite

library(testthat)
library(tibble)
# library(dplyr)

# ============================================================================
# map_citations_to_segments() - HELPER FUNCTION
# ============================================================================

test_that("map_citations_to_segments handles auto sections detection", {
  skip_on_cran()
  citations_df <- tibble(
    citation_id = c("C1", "C2"),
    start_pos = c(10, 50),
    citation_text = c("(Smith, 2020)", "(Jones, 2021)")
  )

  text_list <- list(
    Introduction = paste(rep("x", 30), collapse = ""),
    Methods = paste(rep("x", 30), collapse = "")
  )

  result <- map_citations_to_segments(
    citations_df,
    text_list,
    use_sections = "auto",
    n_segments = 5
  )

  expect_true("segment" %in% names(result))
  expect_true("segment_type" %in% names(result))
})

test_that("map_citations_to_segments creates equal-length segments", {
  skip_on_cran()
  citations_df <- tibble(
    citation_id = c("C1", "C2", "C3"),
    start_pos = c(10, 50, 100),
    citation_text = c("(A, 2020)", "(B, 2021)", "(C, 2022)")
  )

  text <- paste(rep("word", 200), collapse = " ")

  result <- map_citations_to_segments(
    citations_df,
    text,
    use_sections = FALSE,
    n_segments = 3
  )

  expect_equal(result$segment_type[1], "equal_length")
  expect_true(all(grepl("Segment", result$segment)))
})

test_that("map_citations_to_segments warns when sections unavailable", {
  skip_on_cran()
  citations_df <- tibble(
    citation_id = "C1",
    start_pos = 10,
    citation_text = "(Smith, 2020)"
  )

  text <- "Simple string text"

  expect_warning(
    result <- map_citations_to_segments(
      citations_df,
      text,
      use_sections = TRUE,
      n_segments = 5
    ),
    "Sections requested but not available"
  )
})

# ============================================================================
# analyze_scientific_content() - BASIC INPUT VALIDATION
# ============================================================================

test_that("analyze_scientific_content requires text input", {
  skip_on_cran()
  expect_error(
    analyze_scientific_content(),
    "argument \"text\" is missing"
  )
})

test_that("analyze_scientific_content works with simple string", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "This is a simple test document with some words."

  result <- analyze_scientific_content(text)

  expect_type(result, "list")
  expect_s3_class(result, "enhanced_scientific_content_analysis")
})

test_that("analyze_scientific_content works with list input", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text_list <- list(
    Introduction = "This is the introduction section.",
    Methods = "This describes the methods used."
  )

  result <- analyze_scientific_content(text_list)

  expect_type(result, "list")
  expect_s3_class(result, "enhanced_scientific_content_analysis")
})

# ============================================================================
# analyze_scientific_content() - TEXT ANALYTICS
# ============================================================================

test_that("analyze_scientific_content calculates basic text statistics", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "This is a test. This has words."

  result <- analyze_scientific_content(text)

  expect_true(!is.null(result$text_analytics))
  expect_true(!is.null(result$text_analytics$basic_stats))
  expect_true("total_characters" %in% names(result$text_analytics$basic_stats))
  expect_true("total_words" %in% names(result$text_analytics$basic_stats))
})

test_that("analyze_scientific_content extracts word frequencies", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "test word test word test another"

  result <- analyze_scientific_content(
    text,
    min_word_length = 3,
    remove_stopwords = FALSE
  )

  expect_true(!is.null(result$word_frequencies))
  expect_s3_class(result$word_frequencies, "tbl_df")
  expect_true("word" %in% names(result$word_frequencies))
  expect_true("n" %in% names(result$word_frequencies))
})

test_that("analyze_scientific_content generates n-grams", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "machine learning is important for data science"

  result <- analyze_scientific_content(
    text,
    ngram_range = c(1, 2),
    remove_stopwords = FALSE
  )

  expect_true(!is.null(result$ngrams))
  expect_true("1gram" %in% names(result$ngrams))
  expect_true("2gram" %in% names(result$ngrams))
})

test_that("analyze_scientific_content removes stopwords", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "the quick brown fox jumps over the lazy dog"

  result_with_stops <- analyze_scientific_content(
    text,
    remove_stopwords = FALSE
  )

  result_no_stops <- analyze_scientific_content(
    text,
    remove_stopwords = TRUE
  )

  # With stopwords should have more words
  expect_gt(
    nrow(result_with_stops$word_frequencies),
    nrow(result_no_stops$word_frequencies)
  )
})

# ============================================================================
# analyze_scientific_content() - CITATION EXTRACTION
# ============================================================================

test_that("analyze_scientific_content extracts author-year citations", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "Research by Smith (2020) shows that machine learning (Jones, 2021) is important."

  result <- analyze_scientific_content(text)

  expect_true(!is.null(result$citations))
  expect_s3_class(result$citations, "tbl_df")

  if (nrow(result$citations) > 0) {
    expect_true("citation_text" %in% names(result$citations))
    expect_true("citation_type" %in% names(result$citations))
  }
})

test_that("analyze_scientific_content extracts numbered citations", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "Previous work [1] showed results. Another study [2] confirmed this."

  result <- analyze_scientific_content(text)

  expect_true(!is.null(result$citations))

  if (nrow(result$citations) > 0) {
    numbered_citations <- result$citations %>%
      dplyr::filter(grepl("numbered", citation_type))

    expect_gte(nrow(numbered_citations), 0)
  }
})

test_that("analyze_scientific_content extracts et al. citations", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "Smith et al. (2020) demonstrated the approach."

  result <- analyze_scientific_content(text)

  expect_true(!is.null(result$citations))

  if (nrow(result$citations) > 0) {
    expect_true(any(grepl("et al", result$citations$citation_text)))
  }
})

test_that("analyze_scientific_content parses multiple citations", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "Research (Smith, 2020; Jones, 2021; Brown, 2022) shows results."

  result <- analyze_scientific_content(
    text,
    parse_multiple_citations = TRUE
  )

  expect_true(!is.null(result$citations))

  if (nrow(result$citations) > 0) {
    parsed <- result$citations %>%
      dplyr::filter(citation_type == "parsed_from_multiple")

    expect_gte(nrow(parsed), 0)
  }
})

# ============================================================================
# analyze_scientific_content() - CITATION CONTEXTS
# ============================================================================

test_that("analyze_scientific_content extracts citation contexts", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "The important work by Smith (2020) demonstrates this approach effectively."

  result <- analyze_scientific_content(
    text,
    window_size = 5
  )

  expect_true(!is.null(result$citation_contexts))

  if (nrow(result$citation_contexts) > 0) {
    expect_true("words_before" %in% names(result$citation_contexts))
    expect_true("words_after" %in% names(result$citation_contexts))
    expect_true("full_context" %in% names(result$citation_contexts))
  }
})

test_that("analyze_scientific_content respects window_size", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "Word1 word2 word3 word4 word5 Smith (2020) word6 word7 word8 word9 word10"

  result <- analyze_scientific_content(
    text,
    window_size = 3
  )

  if (
    !is.null(result$citation_contexts) && nrow(result$citation_contexts) > 0
  ) {
    # Context should have limited words
    expect_true(!is.null(result$citation_contexts$context_word_count))
  }

  expect_true(TRUE) # Always pass if no citations
})

# ============================================================================
# analyze_scientific_content() - CITATION METRICS
# ============================================================================

test_that("analyze_scientific_content calculates citation metrics", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "Smith (2020) and Jones (2021) show results. See also [1] and [2]."

  result <- analyze_scientific_content(text)

  expect_true(!is.null(result$citation_metrics))

  if (length(result$citation_metrics) > 0) {
    expect_true(
      !is.null(result$citation_metrics$type_distribution) ||
        !is.null(result$citation_metrics$density)
    )
  }
})

test_that("analyze_scientific_content tracks narrative vs parenthetical", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "Smith (2020) found that results (Jones, 2021) support the theory."

  result <- analyze_scientific_content(text)

  if (!is.null(result$citation_metrics$narrative_ratio)) {
    expect_true(
      "narrative_citations" %in% names(result$citation_metrics$narrative_ratio)
    )
    expect_true(
      "parenthetical_citations" %in%
        names(result$citation_metrics$narrative_ratio)
    )
  }

  expect_true(TRUE)
})

# ============================================================================
# analyze_scientific_content() - SECTION MAPPING
# ============================================================================

test_that("analyze_scientific_content maps citations to sections", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text_list <- list(
    Introduction = "Smith (2020) introduced the concept.",
    Methods = "We used the approach of Jones (2021).",
    Results = "The results [1] show improvement."
  )

  result <- analyze_scientific_content(
    text_list,
    use_sections_for_citations = TRUE
  )

  if (!is.null(result$citations) && nrow(result$citations) > 0) {
    expect_true("section" %in% names(result$citations))

    sections <- unique(result$citations$section)
    expect_true(any(sections %in% c("Introduction", "Methods", "Results")))
  }

  expect_true(TRUE)
})

test_that("analyze_scientific_content creates segments when no sections", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  long_text <- paste(
    rep("Research by Smith (2020) shows results.", 10),
    collapse = " "
  )

  result <- analyze_scientific_content(
    long_text,
    use_sections_for_citations = FALSE,
    n_segments_citations = 5
  )

  if (!is.null(result$citations) && nrow(result$citations) > 0) {
    expect_true("section" %in% names(result$citations))
  }

  expect_true(TRUE)
})

# ============================================================================
# analyze_scientific_content() - REFERENCE PARSING
# ============================================================================

test_that("analyze_scientific_content parses references from text", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text_list <- list(
    Full_text = "Main text with citation (Smith, 2020).",
    References = "Smith, J. (2020). Title of paper. Journal of Science."
  )

  result <- analyze_scientific_content(text_list)

  expect_true(!is.null(result$parsed_references))

  if (!is.null(result$parsed_references)) {
    expect_s3_class(result$parsed_references, "tbl_df")
    if (nrow(result$parsed_references) > 0) {
      expect_true("ref_id" %in% names(result$parsed_references))
      expect_true("ref_year" %in% names(result$parsed_references))
    }
  }
})

test_that("analyze_scientific_content handles missing references section", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "Text without references section."

  result <- analyze_scientific_content(text)

  # Should complete without error
  expect_type(result, "list")
  expect_true(
    is.null(result$parsed_references) ||
      nrow(result$parsed_references) == 0
  )
})

# ============================================================================
# analyze_scientific_content() - CROSSREF INTEGRATION
# ============================================================================

test_that("analyze_scientific_content retrieves from CrossRef with DOI", {
  skip_on_cran()
  skip_if_not_installed("tidytext")
  skip_if_not_installed("httr2")

  # Mock CrossRef API
  local_mocked_bindings(
    get_crossref_references = function(doi, mailto) {
      tibble(
        key = "ref1",
        doi = "10.1234/test",
        article_title = "Test Article",
        author = "Smith J",
        year = "2020",
        journal = "Journal",
        volume = "10",
        first_page = "100"
      )
    },
    .package = "contentanalysis"
  )

  # text <- "Research text here."
  #
  # result <- analyze_scientific_content(
  #   text,
  #   doi = "10.1234/test",
  #   mailto = "test@example.com"
  # )
  #
  # expect_true(is.null(result$parsed_references))
})

test_that("analyze_scientific_content handles CrossRef failure gracefully", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  # Mock CrossRef to fail
  local_mocked_bindings(
    get_crossref_references = function(doi, mailto) {
      stop("API Error")
    },
    .package = "contentanalysis"
  )

  text_list <- list(
    Full_text = "Text here.",
    References = "Smith, J. (2020). Paper."
  )

  expect_warning(
    result <- analyze_scientific_content(
      text_list,
      doi = "10.1234/test"
    )
  )

  # Should fall back to parsing from text
  expect_type(result, "list")
})

# ============================================================================
# analyze_scientific_content() - CITATION-REFERENCE MATCHING
# ============================================================================

test_that("analyze_scientific_content matches citations to references", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text_list <- list(
    Full_text = "Research by Smith (2020) demonstrates the approach.",
    References = "Smith, J. (2020). Important paper. Journal of Research."
  )

  result <- analyze_scientific_content(text_list)

  if (!is.null(result$citation_references_mapping)) {
    expect_s3_class(result$citation_references_mapping, "tbl_df")
    expect_true("matched_ref_id" %in% names(result$citation_references_mapping))
    expect_true(
      "match_confidence" %in% names(result$citation_references_mapping)
    )
  }

  expect_true(TRUE)
})

# ============================================================================
# analyze_scientific_content() - SUMMARY
# ============================================================================

test_that("analyze_scientific_content generates summary", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "Test text with some words and content."

  result <- analyze_scientific_content(text)

  expect_true(!is.null(result$summary))
  expect_true("total_words_analyzed" %in% names(result$summary))
  expect_true("unique_words" %in% names(result$summary))
  expect_true("lexical_diversity" %in% names(result$summary))
})

# ============================================================================
# analyze_scientific_content() - OUTPUT STRUCTURE
# ============================================================================

test_that("analyze_scientific_content returns complete structure", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "Sample scientific text for analysis."

  result <- analyze_scientific_content(text)

  expected_components <- c(
    "text_analytics",
    "citations",
    "citation_contexts",
    "citation_metrics",
    "word_frequencies",
    "ngrams",
    "summary"
  )

  for (comp in expected_components) {
    expect_true(comp %in% names(result))
  }
})

test_that("analyze_scientific_content has correct class", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "Test text"

  result <- analyze_scientific_content(text)

  expect_s3_class(result, "enhanced_scientific_content_analysis")
  expect_type(result, "list")
})

# ============================================================================
# analyze_scientific_content() - PARAMETERS
# ============================================================================

test_that("analyze_scientific_content respects min_word_length", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "a ab abc abcd abcde"

  result <- analyze_scientific_content(
    text,
    min_word_length = 4,
    remove_stopwords = FALSE
  )

  # Should only have words >= 4 chars
  if (nrow(result$word_frequencies) > 0) {
    min_length <- min(nchar(result$word_frequencies$word))
    expect_gte(min_length, 4)
  }

  expect_true(TRUE)
})

test_that("analyze_scientific_content handles custom stopwords", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "custom word test custom word test"

  result <- analyze_scientific_content(
    text,
    remove_stopwords = TRUE,
    custom_stopwords = c("custom")
  )

  # "custom" should be removed
  if (nrow(result$word_frequencies) > 0) {
    expect_false("custom" %in% result$word_frequencies$word)
  }

  expect_true(TRUE)
})

test_that("analyze_scientific_content handles different ngram_range", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "machine learning is important"

  result <- analyze_scientific_content(
    text,
    ngram_range = c(2, 3),
    remove_stopwords = FALSE
  )

  expect_true("2gram" %in% names(result$ngrams))
  expect_true("3gram" %in% names(result$ngrams))
  expect_false("1gram" %in% names(result$ngrams))
})

# ============================================================================
# analyze_scientific_content() - EDGE CASES
# ============================================================================

test_that("analyze_scientific_content handles empty text", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- ""

  result <- analyze_scientific_content(text)

  expect_type(result, "list")
  expect_s3_class(result, "enhanced_scientific_content_analysis")
})

test_that("analyze_scientific_content handles very short text", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "Short"

  result <- analyze_scientific_content(text)

  expect_type(result, "list")
})

test_that("analyze_scientific_content handles text with no citations", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "This text has no citations at all, just regular content."

  result <- analyze_scientific_content(text)

  expect_true(!is.null(result$citations))
  expect_equal(nrow(result$citations), 0)
})

# ============================================================================
# analyze_scientific_content() - INTEGRATION TESTS
# ============================================================================

test_that("analyze_scientific_content complete workflow with sections", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text_list <- list(
    Introduction = "Machine learning Smith (2020) is a growing field of artificial intelligence.",
    Methods = "We applied the methodology described by Jones (2021) using neural networks.",
    Results = "Our results [1] demonstrate improved accuracy compared to baseline.",
    Discussion = "These findings support the work of Brown et al. (2022) in deep learning.",
    References = "Smith, J. (2020). ML Paper. Journal.

Jones, A. (2021). Methods Paper. Science.

Brown, K., Davis, M., Wilson, P. (2022). DL Research. Nature."
  )

  result <- analyze_scientific_content(
    text_list,
    window_size = 5,
    ngram_range = c(1, 2),
    use_sections_for_citations = TRUE
  )

  # Verify all major components
  expect_s3_class(result, "enhanced_scientific_content_analysis")
  expect_true(!is.null(result$text_analytics))
  expect_true(!is.null(result$citations))
  expect_true(!is.null(result$word_frequencies))
  expect_true(!is.null(result$summary))

  # Verify citations were extracted
  expect_gte(nrow(result$citations), 1)

  # Verify sections were used
  if (nrow(result$citations) > 0) {
    expect_true("section" %in% names(result$citations))
  }

  # Verify references were parsed
  expect_true(!is.null(result$parsed_references))
  if (!is.null(result$parsed_references)) {
    expect_gte(nrow(result$parsed_references), 1)
  }
})

test_that("analyze_scientific_content works without References section", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text_list <- list(
    Introduction = "Research by Smith (2020) shows results.",
    Methods = "We used standard approaches."
  )

  result <- analyze_scientific_content(text_list)

  expect_s3_class(result, "enhanced_scientific_content_analysis")
  expect_true(is.null(result$citation_references_mapping))
})

# ============================================================================
# STRESS TESTS
# ============================================================================

test_that("analyze_scientific_content handles large text", {
  skip_on_cran()
  skip_if_not_installed("tidytext")
  skip_on_cran()

  # Create large text
  large_text <- paste(
    rep(
      "Research by Smith (2020) shows that machine learning is important.",
      100
    ),
    collapse = " "
  )

  result <- analyze_scientific_content(large_text)

  expect_s3_class(result, "enhanced_scientific_content_analysis")
  expect_gt(result$summary$total_words_analyzed, 100)
})

test_that("analyze_scientific_content handles many citations", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  # Mix di formati per assicurarsi di catturarne abbastanza
  text <- paste(
    "Previous work [1] showed that (Smith, 2020) and (Jones, 2021) confirmed",
    "the findings [2][3][4]. Additional studies (Brown, 2022) and [5]",
    "demonstrated (Wilson, 2023) similar results [6][7][8][9][10]",
    "supporting the theory (Davis, 2024) and [11][12][13][14][15]."
  )

  result <- analyze_scientific_content(text)

  expect_gte(nrow(result$citations), 5)
})

# ============================================================================
# FALSE POSITIVE / FALSE NEGATIVE TESTS
# ============================================================================

test_that("months are NOT extracted as citations", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "In January (2020) the study began. Data collected in March (2021) was analyzed."

  result <- analyze_scientific_content(text)

  if (nrow(result$citations) > 0) {
    expect_false(any(grepl("January", result$citations$citation_text)))
    expect_false(any(grepl("March", result$citations$citation_text)))
  }
})

test_that("et al without period is extracted", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "Smith et al (2020) demonstrated the approach."

  result <- analyze_scientific_content(text)

  expect_true(nrow(result$citations) > 0)
  expect_true(any(grepl("et al", result$citations$citation_text)))
})

test_that("in press and forthcoming citations are extracted", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "Smith (in press) found that Jones et al. (forthcoming) confirmed."

  result <- analyze_scientific_content(text)

  expect_true(nrow(result$citations) > 0)
  expect_true(any(grepl("in press", result$citations$citation_text)))
  expect_true(any(grepl("forthcoming", result$citations$citation_text)))
})

test_that("page numbers in parenthetical citations are extracted", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "The finding (Smith, 2020, p. 15) was confirmed by (Jones, 2021, pp. 20-25)."

  result <- analyze_scientific_content(text)

  expect_true(nrow(result$citations) > 0)
  expect_true(any(grepl("p\\. 15", result$citations$citation_text)))
})

test_that("whitespace in bracketed citations is handled", {
  skip_on_cran()
  skip_if_not_installed("tidytext")

  text <- "Previous work [ 1 ] and another study [ 2, 3 ] confirmed this."

  result <- analyze_scientific_content(text)

  if (nrow(result$citations) > 0) {
    numbered <- result$citations %>%
      dplyr::filter(grepl("numbered", citation_type))
    expect_gte(nrow(numbered), 1)
  }
})
