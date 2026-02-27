# Test file for create_citation_network function
library(testthat)
#library(dplyr)
library(igraph)
library(visNetwork)
library(stringr)

# Helper function to create mock citation analysis results
create_mock_citation_results <- function() {
  list(
    network_data = data.frame(
      citation1 = c(
        "Smith et al. (2020)",
        "Jones et al. (2019)",
        "Smith et al. (2020)",
        "Brown et al. (2021)"
      ),
      citation2 = c(
        "Jones et al. (2019)",
        "Brown et al. (2021)",
        "Brown et al. (2021)",
        "Davis et al. (2022)"
      ),
      distance = c(150, 450, 800, 300),
      stringsAsFactors = FALSE
    ),
    citations = data.frame(
      citation_text_clean = c(
        "Smith et al. (2020)",
        "Jones et al. (2019)",
        "Brown et al. (2021)",
        "Davis et al. (2022)",
        "Smith et al. (2020)"
      ),
      section = c(
        "Introduction",
        "Methods",
        "Results",
        "Discussion",
        "Results"
      ),
      stringsAsFactors = FALSE
    ),
    section_colors = c(
      "Introduction" = "#FF6B6B",
      "Methods" = "#4ECDC4",
      "Results" = "#45B7D1",
      "Discussion" = "#FFA07A"
    )
  )
}

# Test 1: Basic functionality with valid input
test_that("create_citation_network creates valid network with default parameters", {
  skip_on_cran()
  mock_data <- create_mock_citation_results()
  network <- create_citation_network(mock_data)

  expect_s3_class(network, "visNetwork")
  expect_false(is.null(network))
  expect_true("stats" %in% names(attributes(network)))
})

# Test 2: Network statistics are correctly calculated
test_that("network statistics are correctly computed", {
  skip_on_cran()
  mock_data <- create_mock_citation_results()
  network <- create_citation_network(mock_data)
  stats <- attr(network, "stats")

  expect_true(is.list(stats))
  expect_true("n_nodes" %in% names(stats))
  expect_true("n_edges" %in% names(stats))
  expect_true("avg_distance" %in% names(stats))
  expect_true("max_distance" %in% names(stats))
  expect_true("section_distribution" %in% names(stats))

  expect_true(stats$n_nodes > 0)
  expect_true(stats$n_edges > 0)
  expect_equal(stats$max_distance, 1000)
  expect_true(is.numeric(stats$avg_distance))
})

# Test 3: max_distance parameter filters correctly
test_that("max_distance parameter filters citation pairs", {
  skip_on_cran()
  mock_data <- create_mock_citation_results()

  # With max_distance = 500, should exclude the pair with distance 800
  network_500 <- create_citation_network(mock_data, max_distance = 500)
  stats_500 <- attr(network_500, "stats")

  # With max_distance = 200, should only include pair with distance 150
  network_200 <- create_citation_network(mock_data, max_distance = 200)
  stats_200 <- attr(network_200, "stats")

  expect_true(stats_500$n_edges < 4) # Less than total possible edges
  expect_true(stats_200$n_edges <= stats_500$n_edges)
  expect_true(all(stats_200$avg_distance <= 200))
})

# Test 4: min_connections parameter filters nodes
test_that("min_connections parameter filters nodes correctly", {
  skip_on_cran()
  mock_data <- create_mock_citation_results()

  network_min1 <- create_citation_network(mock_data, min_connections = 1)
  network_min2 <- create_citation_network(mock_data, min_connections = 2)

  stats_min1 <- attr(network_min1, "stats")
  stats_min2 <- attr(network_min2, "stats")

  # Higher min_connections should result in fewer nodes
  expect_true(stats_min2$n_nodes <= stats_min1$n_nodes)
})

# Test 5: show_labels parameter works
test_that("show_labels parameter controls label display", {
  skip_on_cran()
  mock_data <- create_mock_citation_results()

  network_with_labels <- create_citation_network(mock_data, show_labels = TRUE)
  network_no_labels <- create_citation_network(mock_data, show_labels = FALSE)

  # Both should create valid networks
  expect_s3_class(network_with_labels, "visNetwork")
  expect_s3_class(network_no_labels, "visNetwork")
})

# Test 6: Handles NULL or empty network_data
test_that("function handles NULL or empty network_data gracefully", {
  skip_on_cran()
  mock_data_null <- list(
    network_data = NULL,
    citations = data.frame(),
    section_colors = c()
  )

  expect_warning(
    result <- create_citation_network(mock_data_null),
    "No citation co-occurrence data found"
  )
  expect_null(result)

  mock_data_empty <- list(
    network_data = data.frame(
      citation1 = character(0),
      citation2 = character(0),
      distance = numeric(0)
    ),
    citations = data.frame(),
    section_colors = c()
  )

  expect_warning(
    result <- create_citation_network(mock_data_empty),
    "No citation co-occurrence data found"
  )
  expect_null(result)
})

# Test 7: Warning when no pairs within max_distance
test_that("function warns when no pairs within max_distance", {
  skip_on_cran()
  mock_data <- create_mock_citation_results()

  expect_warning(
    result <- create_citation_network(mock_data, max_distance = 50),
    "No citation pairs found within the specified maximum distance"
  )
  expect_null(result)
})

# Test 8: Warning when no valid connections after filtering
test_that("function warns when no valid connections after filtering", {
  skip_on_cran()
  mock_data <- create_mock_citation_results()

  # Set very high min_connections that no node can satisfy
  expect_warning(
    result <- create_citation_network(mock_data, min_connections = 100),
    "No valid connections after filtering"
  )
  expect_null(result)
})

# Test 9: Multi-section citations are correctly identified
test_that("multi-section citations are correctly identified", {
  skip_on_cran()
  mock_data <- create_mock_citation_results()
  network <- create_citation_network(mock_data)
  stats <- attr(network, "stats")

  expect_true("multi_section_citations" %in% names(stats))

  # Smith et al. (2020) appears in both Introduction and Results
  multi_section_df <- stats$multi_section_citations
  if (nrow(multi_section_df) > 0) {
    expect_true(any(grepl("Smith et al.", multi_section_df$citation_text)))
  }
})

# Test 10: Section colors are properly applied
test_that("section colors are properly applied", {
  skip_on_cran()
  mock_data <- create_mock_citation_results()
  network <- create_citation_network(mock_data)
  stats <- attr(network, "stats")

  expect_true("section_colors" %in% names(stats))
  expect_true(is.vector(stats$section_colors))
  expect_true(length(stats$section_colors) > 0)
})

# Test 11: Edge properties are correctly set
test_that("edge properties reflect distance correctly", {
  skip_on_cran()
  mock_data <- create_mock_citation_results()
  network <- create_citation_network(mock_data, max_distance = 1000)

  # Check that network has edges
  expect_s3_class(network, "visNetwork")

  # Network should have data structure
  expect_true(!is.null(network$x$edges))
})

# Test 12: Section distribution is calculated
test_that("section distribution is calculated in statistics", {
  skip_on_cran()
  mock_data <- create_mock_citation_results()
  network <- create_citation_network(mock_data)
  stats <- attr(network, "stats")

  expect_true("section_distribution" %in% names(stats))
  expect_s3_class(stats$section_distribution, "data.frame")
  expect_true("primary_section" %in% names(stats$section_distribution))
  expect_true("n" %in% names(stats$section_distribution))
})

# Test 13: Unknown sections are handled
test_that("unknown sections are handled correctly", {
  skip_on_cran()
  mock_data_with_na <- create_mock_citation_results()
  mock_data_with_na$citations$section[1] <- NA

  network <- create_citation_network(mock_data_with_na)

  expect_s3_class(network, "visNetwork")
  stats <- attr(network, "stats")

  # Should have Unknown in section colors
  expect_true("Unknown" %in% names(stats$section_colors))
  expect_equal(unname(stats$section_colors["Unknown"]), "#CCCCCC")
})

# Test 14: Network attributes contain expected components
test_that("returned network has all expected attributes", {
  skip_on_cran()
  mock_data <- create_mock_citation_results()
  network <- create_citation_network(mock_data)

  expect_s3_class(network, "visNetwork")
  expect_true("x" %in% names(network))
  expect_true("nodes" %in% names(network$x))
  expect_true("edges" %in% names(network$x))
  expect_true("options" %in% names(network$x))
})


# ===================================================================
# Tests for describe_citation_clusters()
# ===================================================================

# Helper function to create mock citation analysis results for cluster tests
create_mock_cluster_results <- function() {
  list(
    citations = tibble::tibble(
      citation_text_clean = c(
        "Smith et al. (2020)", "Smith et al. (2020)",
        "Jones et al. (2019)", "Jones et al. (2019)",
        "Brown et al. (2021)", "Davis et al. (2022)",
        "Wilson et al. (2018)"
      ),
      section = c(
        "Introduction", "Methods",
        "Introduction", "Results",
        "Methods", "Results",
        "Discussion"
      )
    ),
    citation_references_mapping = tibble::tibble(
      citation_text_clean = c(
        "Smith et al. (2020)",
        "Jones et al. (2019)",
        "Brown et al. (2021)",
        "Davis et al. (2022)",
        "Wilson et al. (2018)"
      ),
      ref_full_text = c(
        "Smith, J., & Doe, A. (2020). Machine learning approaches for natural language processing tasks. Journal of Artificial Intelligence, 45(2), 123-145.",
        "Jones, R., Brown, T., & Lee, S. (2019). Deep neural networks in computational biology applications. Bioinformatics Review, 33(1), 67-89.",
        "Brown, K., & White, M. (2021). Statistical methods for genomic data analysis and interpretation. Genome Research, 12(4), 234-256.",
        "Davis, L., Chen, W., & Park, H. (2022). Reinforcement learning algorithms for autonomous robotics systems. Robotics and Automation Letters, 8(3), 345-367.",
        "Wilson, P., & Taylor, R. (2018). Evolutionary computation techniques for optimization problems. Computational Intelligence, 22(1), 78-99."
      ),
      matched_ref_id = c("ref1", "ref2", "ref3", "ref4", "ref5")
    ),
    parsed_references = tibble::tibble(
      ref_id = c("ref1", "ref2", "ref3", "ref4", "ref5"),
      ref_full_text = c(
        "Smith, J., & Doe, A. (2020). Machine learning approaches for natural language processing tasks. Journal of Artificial Intelligence, 45(2), 123-145.",
        "Jones, R., Brown, T., & Lee, S. (2019). Deep neural networks in computational biology applications. Bioinformatics Review, 33(1), 67-89.",
        "Brown, K., & White, M. (2021). Statistical methods for genomic data analysis and interpretation. Genome Research, 12(4), 234-256.",
        "Davis, L., Chen, W., & Park, H. (2022). Reinforcement learning algorithms for autonomous robotics systems. Robotics and Automation Letters, 8(3), 345-367.",
        "Wilson, P., & Taylor, R. (2018). Evolutionary computation techniques for optimization problems. Computational Intelligence, 22(1), 78-99."
      )
    )
  )
}

# Test 15: Basic functionality and output structure
test_that("describe_citation_clusters returns correct structure", {
  skip_on_cran()
  mock_data <- create_mock_cluster_results()
  result <- describe_citation_clusters(mock_data)

  expect_s3_class(result, "citation_cluster_description")
  expect_true(is.list(result))
  expect_true("cluster_descriptions" %in% names(result))
  expect_true("cluster_summary" %in% names(result))
  expect_true("cluster_references" %in% names(result))
})

# Test 16: cluster_descriptions has expected columns
test_that("cluster_descriptions tibble has correct columns", {
  skip_on_cran()
  mock_data <- create_mock_cluster_results()
  result <- describe_citation_clusters(mock_data)

  expected_cols <- c("section", "ngram", "n", "tf", "idf", "tf_idf", "ngram_size")
  expect_true(all(expected_cols %in% names(result$cluster_descriptions)))
})

# Test 17: TF-IDF scores are computed (non-zero values)
test_that("TF-IDF scores are computed with non-zero values", {
  skip_on_cran()
  mock_data <- create_mock_cluster_results()
  result <- describe_citation_clusters(mock_data)

  expect_true(nrow(result$cluster_descriptions) > 0)
  expect_true(all(result$cluster_descriptions$tf > 0))
  expect_true(any(result$cluster_descriptions$tf_idf > 0))
})

# Test 18: Stopwords are removed
test_that("stopwords are removed from n-grams", {
  skip_on_cran()
  mock_data <- create_mock_cluster_results()
  result <- describe_citation_clusters(mock_data)

  unigrams <- result$cluster_descriptions %>%
    filter(ngram_size == 1)

  common_stopwords <- c("the", "and", "for", "in", "of", "a", "an")
  expect_false(any(unigrams$ngram %in% common_stopwords))
})

# Test 19: top_n parameter limits output
test_that("top_n parameter limits terms per section", {
  skip_on_cran()
  mock_data <- create_mock_cluster_results()

  result_5 <- describe_citation_clusters(mock_data, top_n = 5)
  result_3 <- describe_citation_clusters(mock_data, top_n = 3)

  terms_per_section_5 <- result_5$cluster_descriptions %>%
    count(section)
  terms_per_section_3 <- result_3$cluster_descriptions %>%
    count(section)

  expect_true(all(terms_per_section_5$n <= 5))
  expect_true(all(terms_per_section_3$n <= 3))
})

# Test 20: Graceful handling when citation_references_mapping is NULL
test_that("function handles NULL citation_references_mapping gracefully", {
  skip_on_cran()
  mock_data <- create_mock_cluster_results()
  mock_data$citation_references_mapping <- NULL

  expect_warning(
    result <- describe_citation_clusters(mock_data),
    "No citation-reference mapping found"
  )
  expect_null(result)
})

# Test 21: Graceful handling when citations is empty
test_that("function handles empty citations gracefully", {
  skip_on_cran()
  mock_data <- create_mock_cluster_results()
  mock_data$citations <- tibble::tibble(
    citation_text_clean = character(0),
    section = character(0)
  )

  expect_warning(
    result <- describe_citation_clusters(mock_data),
    "No citations data found"
  )
  expect_null(result)
})

# Test 22: cluster_summary has one row per section with top terms
test_that("cluster_summary has one row per section", {
  skip_on_cran()
  mock_data <- create_mock_cluster_results()
  result <- describe_citation_clusters(mock_data)

  expect_true("section" %in% names(result$cluster_summary))
  expect_true("n_references" %in% names(result$cluster_summary))
  expect_true("top_terms" %in% names(result$cluster_summary))

  # Each section should appear exactly once
  expect_equal(
    nrow(result$cluster_summary),
    n_distinct(result$cluster_summary$section)
  )
})

# Test 23: cluster_references maps sections to references
test_that("cluster_references maps sections to references", {
  skip_on_cran()
  mock_data <- create_mock_cluster_results()
  result <- describe_citation_clusters(mock_data)

  expect_true("section" %in% names(result$cluster_references))
  expect_true("ref_full_text" %in% names(result$cluster_references))
  expect_true(nrow(result$cluster_references) > 0)
})

# Test 24: Custom stopwords are applied
test_that("custom stopwords are applied", {
  skip_on_cran()
  mock_data <- create_mock_cluster_results()

  result_custom <- describe_citation_clusters(
    mock_data,
    custom_stopwords = c("learning", "methods")
  )

  unigrams <- result_custom$cluster_descriptions %>%
    filter(ngram_size == 1)

  expect_false("learning" %in% unigrams$ngram)
  expect_false("methods" %in% unigrams$ngram)
})

# Test 25: ngram_range parameter controls n-gram sizes
test_that("ngram_range controls which n-gram sizes are produced", {
  skip_on_cran()
  mock_data <- create_mock_cluster_results()

  result_uni <- describe_citation_clusters(mock_data, ngram_range = c(1, 1))
  result_bi <- describe_citation_clusters(mock_data, ngram_range = c(2, 2))

  expect_true(all(result_uni$cluster_descriptions$ngram_size == 1))
  if (nrow(result_bi$cluster_descriptions) > 0) {
    expect_true(all(result_bi$cluster_descriptions$ngram_size == 2))
  }
})

# Test 26: Graceful handling when no titles can be extracted
test_that("function handles references with no extractable titles", {
  skip_on_cran()
  mock_data <- create_mock_cluster_results()
  mock_data$citation_references_mapping$ref_full_text <- rep("", 5)

  expect_warning(
    result <- describe_citation_clusters(mock_data),
    "No titles could be extracted|No references could be mapped"
  )
  expect_null(result)
})


# ===================================================================
# Tests for plot_citation_clusters()
# ===================================================================

# Helper to create a mock cluster description for plot tests
create_mock_cluster_description <- function() {
  mock_data <- create_mock_cluster_results()
  describe_citation_clusters(mock_data, top_n = 5)
}

# Test 27: Returns correct structure and class
test_that("plot_citation_clusters returns a list of class 'citation_cluster_plots' with 3 named elements", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  desc <- create_mock_cluster_description()
  plots <- plot_citation_clusters(desc)

  expect_s3_class(plots, "citation_cluster_plots")
  expect_true(is.list(plots))
  expect_equal(length(plots), 3)

  expect_true("tfidf_bars" %in% names(plots))
  expect_true("tfidf_heatmap" %in% names(plots))
  expect_true("references_per_section" %in% names(plots))
})

# Test 28: Each element is a plotly/htmlwidget object
test_that("each plot element is a plotly htmlwidget", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  desc <- create_mock_cluster_description()
  plots <- plot_citation_clusters(desc)

  expect_s3_class(plots$tfidf_bars, "plotly")
  expect_s3_class(plots$tfidf_bars, "htmlwidget")
  expect_s3_class(plots$tfidf_heatmap, "plotly")
  expect_s3_class(plots$tfidf_heatmap, "htmlwidget")
  expect_s3_class(plots$references_per_section, "plotly")
  expect_s3_class(plots$references_per_section, "htmlwidget")
})

# Test 29: Handles NULL input gracefully
test_that("plot_citation_clusters handles NULL input with warning", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  expect_warning(
    result <- plot_citation_clusters(NULL),
    "cluster_description is NULL"
  )
  expect_null(result)
})

# Test 30: section_colors parameter works without error
test_that("section_colors parameter works correctly", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  desc <- create_mock_cluster_description()
  custom_colors <- c(
    "Introduction" = "#FF0000",
    "Methods" = "#00FF00",
    "Results" = "#0000FF",
    "Discussion" = "#FFFF00"
  )

  plots <- plot_citation_clusters(desc, section_colors = custom_colors)

  expect_s3_class(plots, "citation_cluster_plots")
  expect_s3_class(plots$tfidf_bars, "plotly")
})

# Test 31: top_n limits displayed terms
test_that("top_n parameter limits displayed terms", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  desc <- create_mock_cluster_description()

  # With a very small top_n the plots should still work
  plots_small <- plot_citation_clusters(desc, top_n = 2)

  expect_s3_class(plots_small, "citation_cluster_plots")
  expect_s3_class(plots_small$tfidf_bars, "plotly")
})
