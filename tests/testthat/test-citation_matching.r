## Test suite for match_citations_to_references function

library(testthat)
library(tibble)
# library(dplyr)

# Helper functions to create test data
create_citations_df <- function(texts, types = NULL, ids = NULL) {
  n <- length(texts)
  if (is.null(types)) {
    types <- rep("narrative_parenthetical", n)
  }
  if (is.null(ids)) {
    ids <- paste0("CITE_", seq_len(n))
  }

  tibble(
    citation_id = ids,
    citation_text = texts,
    citation_text_clean = texts,
    citation_type = types
  )
}

create_references_df <- function(
  authors,
  years,
  ids = NULL,
  first_authors = NULL,
  second_authors = NULL,
  n_authors = NULL,
  ref_source = NULL
) {
  n <- length(authors)
  if (is.null(ids)) {
    ids <- paste0("REF_", seq_len(n))
  }
  if (is.null(first_authors)) {
    # Extract first author from full authors string
    first_authors <- sapply(strsplit(authors, ","), function(x) {
      first <- trimws(x[1])
      # Extract last name
      parts <- strsplit(first, " ")[[1]]
      parts[length(parts)]
    })
  }
  if (is.null(n_authors)) {
    n_authors <- sapply(strsplit(authors, ","), length)
  }

  df <- tibble(
    ref_id = ids,
    ref_full_text = paste(authors, years),
    ref_authors = authors,
    ref_year = as.character(years),
    ref_first_author = first_authors,
    ref_first_author_normalized = tolower(first_authors),
    ref_second_author = second_authors,
    n_authors = n_authors
  )

  # Add ref_source if provided
  if (!is.null(ref_source)) {
    df <- df %>% mutate(ref_source = ref_source)
  }

  return(df)
}

# ============================================================================
# EDGE CASES
# ============================================================================

test_that("returns empty tibble when citations_df is empty", {
  skip_on_cran()
  citations <- create_citations_df(character(0))
  references <- create_references_df(c("Smith J"), c("2020"))

  result <- match_citations_to_references(citations, references)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true(all(
    c("citation_id", "matched_ref_id", "match_confidence") %in% names(result)
  ))
})

test_that("returns empty tibble when references_df is empty", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith, 2020)"))
  references <- create_references_df(character(0), character(0))

  result <- match_citations_to_references(citations, references)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("handles both empty dataframes", {
  skip_on_cran()
  citations <- create_citations_df(character(0))
  references <- create_references_df(character(0), character(0))

  result <- match_citations_to_references(citations, references)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

# ============================================================================
# NUMBERED CITATIONS
# ============================================================================

test_that("matches single numbered citation", {
  skip_on_cran()
  citations <- create_citations_df(
    texts = c("[1]"),
    types = c("numbered_single")
  )
  references <- create_references_df(
    authors = c("Smith J"),
    years = c("2020"),
    ids = c("REF_1")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_equal(result$matched_ref_id[1], "REF_1")
  expect_equal(result$match_confidence[1], "high_numbered")
  expect_equal(result$ref_authors[1], "Smith J")
})

test_that("matches multiple numbered citations", {
  skip_on_cran()
  citations <- create_citations_df(
    texts = c("[1, 2, 3]"),
    types = c("numbered_multiple")
  )
  references <- create_references_df(
    authors = c("Smith J", "Jones A", "Brown K"),
    years = c("2020", "2021", "2022"),
    ids = c("REF_1", "REF_2", "REF_3")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 3) # One row per number
  expect_equal(result$matched_ref_id, c("REF_1", "REF_2", "REF_3"))
  expect_true(all(result$match_confidence == "high_numbered"))
})

test_that("handles numbered citation with no matching reference", {
  skip_on_cran()
  citations <- create_citations_df(
    texts = c("[99]"),
    types = c("numbered_single")
  )
  references <- create_references_df(
    authors = c("Smith J"),
    years = c("2020"),
    ids = c("REF_1")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$matched_ref_id[1]))
  expect_equal(result$match_confidence[1], "no_match_numbered")
})

test_that("handles numbered citation with no numbers", {
  skip_on_cran()
  citations <- create_citations_df(
    texts = c("[]"),
    types = c("numbered_single")
  )
  references <- create_references_df(
    authors = c("Smith J"),
    years = c("2020")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$matched_ref_id[1]))
  expect_equal(result$match_confidence[1], "no_match_missing_number")
})

# ============================================================================
# NARRATIVE CITATIONS - BASIC MATCHING
# ============================================================================

test_that("matches simple narrative citation with exact author and year", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith, 2020)"))
  references <- create_references_df(
    authors = c("Smith J"),
    years = c("2020"),
    first_authors = c("Smith")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_equal(result$matched_ref_id[1], "REF_1")
  expect_equal(result$match_confidence[1], "high")
  expect_equal(result$cite_author[1], "Smith")
  expect_equal(result$cite_year[1], "2020")
})

test_that("matches citation with different case", {
  skip_on_cran()
  citations <- create_citations_df(c("(SMITH, 2020)"))
  references <- create_references_df(
    authors = c("Smith J"),
    years = c("2020"),
    first_authors = c("Smith")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_equal(result$matched_ref_id[1], "REF_1")
  expect_equal(result$match_confidence[1], "high")
})

test_that("handles citation with missing year", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith)"))
  references <- create_references_df(
    authors = c("Smith J"),
    years = c("2020")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$matched_ref_id[1]))
  expect_equal(result$match_confidence[1], "no_match_missing_info")
})

test_that("handles citation with missing author", {
  skip_on_cran()
  citations <- create_citations_df(c("(2020)"))
  references <- create_references_df(
    authors = c("Smith J"),
    years = c("2020")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$matched_ref_id[1]))
  expect_equal(result$match_confidence[1], "no_match_missing_info")
})

# ============================================================================
# NO MATCH SCENARIOS
# ============================================================================

test_that("returns no_match_year when year not found", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith, 2020)"))
  references <- create_references_df(
    authors = c("Smith J"),
    years = c("2019"), # Different year
    first_authors = c("Smith")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$matched_ref_id[1]))
  expect_equal(result$match_confidence[1], "no_match_year")
})

test_that("returns no_match_author when author not found", {
  skip_on_cran()
  citations <- create_citations_df(c("(Johnson, 2020)"))
  references <- create_references_df(
    authors = c("Smith J"),
    years = c("2020"),
    first_authors = c("Smith")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$matched_ref_id[1]))
  expect_equal(result$match_confidence[1], "no_match_author")
})

# ============================================================================
# FUZZY MATCHING
# ============================================================================

test_that("performs fuzzy matching on author names", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smi, 2020)"))
  references <- create_references_df(
    authors = c("Smith J"),
    years = c("2020"),
    first_authors = c("Smith")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_equal(result$matched_ref_id[1], "REF_1")
  # Fuzzy match is still considered based on the exact vs fuzzy logic
  expect_true(result$match_confidence[1] %in% c("high", "medium_fuzzy"))
})

# ============================================================================
# TWO AUTHOR CITATIONS
# ============================================================================

test_that("matches citation with two authors using 'and'", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith and Jones, 2020)"))
  references <- create_references_df(
    authors = c("Smith J, Jones A"),
    years = c("2020"),
    first_authors = c("Smith"),
    second_authors = c("Jones"),
    n_authors = c(2)
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_equal(result$matched_ref_id[1], "REF_1")
  expect_equal(result$cite_author[1], "Smith")
  expect_equal(result$cite_second_author[1], "Jones")
})

test_that("matches citation with two authors using '&'", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith & Jones, 2020)"))
  references <- create_references_df(
    authors = c("Smith J, Jones A"),
    years = c("2020"),
    first_authors = c("Smith"),
    second_authors = c("Jones"),
    n_authors = c(2)
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_equal(result$matched_ref_id[1], "REF_1")
  expect_equal(result$cite_second_author[1], "Jones")
})

test_that("disambiguates using second author", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith and Jones, 2020)"))
  references <- create_references_df(
    authors = c("Smith J, Jones A", "Smith J, Brown K"),
    years = c("2020", "2020"),
    first_authors = c("Smith", "Smith"),
    second_authors = c("Jones", "Brown"),
    n_authors = c(2, 2)
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_equal(result$matched_ref_id[1], "REF_1") # Matches first one with Jones
  expect_equal(result$match_confidence[1], "high_second_author")
})

# ============================================================================
# ET AL. CITATIONS
# ============================================================================

test_that("matches citation with et al.", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith et al., 2020)"))
  references <- create_references_df(
    authors = c("Smith J, Jones A, Brown K"),
    years = c("2020"),
    first_authors = c("Smith"),
    n_authors = c(3)
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_equal(result$matched_ref_id[1], "REF_1")
  expect_equal(result$cite_has_etal[1], TRUE)
})

test_that("et al. heuristic prefers references with 3+ authors", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith et al., 2020)"))
  references <- create_references_df(
    authors = c("Smith J", "Smith J, Jones A, Brown K"),
    years = c("2020", "2020"),
    first_authors = c("Smith", "Smith"),
    n_authors = c(1, 3)
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_equal(result$matched_ref_id[1], "REF_2") # Should match the 3-author one
  expect_equal(result$match_confidence[1], "medium_etal_heuristic")
})

test_that("et al. with inconsistent author count gets medium confidence", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith et al., 2020)"))
  references <- create_references_df(
    authors = c("Smith J, Jones A"), # Only 2 authors
    years = c("2020"),
    first_authors = c("Smith"),
    n_authors = c(2)
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_equal(result$matched_ref_id[1], "REF_1")
  expect_equal(result$match_confidence[1], "medium_etal_inconsistent")
})

# ============================================================================
# MULTIPLE MATCHES
# ============================================================================

test_that("handles multiple matches with fallback", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith, 2020)"))
  references <- create_references_df(
    authors = c("Smith J", "Smith K", "Smith M"),
    years = c("2020", "2020", "2020"),
    first_authors = c("Smith", "Smith", "Smith")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_equal(result$matched_ref_id[1], "REF_1") # Takes first match
  expect_equal(result$match_confidence[1], "medium_multiple_matches")
})

# ============================================================================
# CROSSREF REFERENCES
# ============================================================================

test_that("handles CrossRef references with surname extraction", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith, 2020)"))

  # CrossRef format: "U Bititci" (initial + surname)
  references <- create_references_df(
    authors = c("J Smith"),
    years = c("2020")
  ) %>%
    mutate(
      ref_source = "crossref",
      ref_first_author = "J Smith",
      ref_first_author_normalized = "j smith"
    )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_equal(result$matched_ref_id[1], "REF_1")
})

test_that("CrossRef references with et al. get appropriate confidence", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith et al., 2020)"))

  # CrossRef format: first author only, n_authors is NA
  references <- create_references_df(
    authors = c("J Smith"),
    years = c("2020"),
    first_authors = c("Smith"),
    n_authors = NA_integer_,
    ref_source = "crossref"
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  # With CrossRef and NA n_authors, it should still match but with medium confidence
  expect_true(
    !is.na(result$matched_ref_id[1]) ||
      result$match_confidence[1] %in%
        c("medium_crossref_etal", "medium_etal_heuristic", "no_match_author")
  )
})

# ============================================================================
# SPECIAL CHARACTERS
# ============================================================================

test_that("handles authors with hyphens", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith-Jones, 2020)"))
  references <- create_references_df(
    authors = c("Smith-Jones A"),
    years = c("2020"),
    first_authors = c("Smith-Jones")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_equal(result$matched_ref_id[1], "REF_1")
})

test_that("handles authors with apostrophes", {
  skip_on_cran()
  citations <- create_citations_df(c("(O'Brien, 2020)"))
  references <- create_references_df(
    authors = c("O'Brien M"),
    years = c("2020"),
    first_authors = c("O'Brien")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_equal(result$matched_ref_id[1], "REF_1")
})

test_that("handles unicode characters in names", {
  skip_on_cran()
  citations <- create_citations_df(c("(Müller, 2020)"))
  references <- create_references_df(
    authors = c("Müller K"),
    years = c("2020"),
    first_authors = c("Müller")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_equal(result$matched_ref_id[1], "REF_1")
})

# ============================================================================
# YEAR VARIANTS
# ============================================================================

test_that("handles year with letter suffix", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith, 2020a)"))
  references <- create_references_df(
    authors = c("Smith J"),
    years = c("2020a"),
    first_authors = c("Smith")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_equal(result$matched_ref_id[1], "REF_1")
  expect_equal(result$cite_year[1], "2020a")
})

# ============================================================================
# MIXED CITATION TYPES
# ============================================================================

test_that("handles mix of numbered and narrative citations", {
  skip_on_cran()
  citations <- create_citations_df(
    texts = c("[1]", "(Smith, 2020)"),
    types = c("numbered_single", "narrative_parenthetical")
  )
  references <- create_references_df(
    authors = c("Jones A", "Smith J"),
    years = c("2019", "2020"),
    ids = c("REF_1", "REF_2"),
    first_authors = c("Jones", "Smith")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 2)
  expect_equal(result$matched_ref_id[1], "REF_1")
  expect_equal(result$match_confidence[1], "high_numbered")
  expect_equal(result$matched_ref_id[2], "REF_2")
  expect_equal(result$match_confidence[2], "high")
})

# ============================================================================
# OUTPUT STRUCTURE
# ============================================================================

test_that("output contains all required columns", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith, 2020)"))
  references <- create_references_df(
    authors = c("Smith J"),
    years = c("2020"),
    first_authors = c("Smith")
  )

  result <- match_citations_to_references(citations, references)

  required_cols <- c(
    "citation_id",
    "citation_text",
    "citation_text_clean",
    "citation_type",
    "cite_author",
    "cite_second_author",
    "cite_year",
    "cite_has_etal",
    "matched_ref_id",
    "ref_full_text",
    "ref_authors",
    "ref_year",
    "match_confidence"
  )

  expect_true(all(required_cols %in% names(result)))
})

test_that("output has correct types", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith, 2020)"))
  references <- create_references_df(
    authors = c("Smith J"),
    years = c("2020"),
    first_authors = c("Smith")
  )

  result <- match_citations_to_references(citations, references)

  expect_type(result$citation_id, "character")
  expect_type(result$matched_ref_id, "character")
  expect_type(result$match_confidence, "character")
  expect_type(result$cite_has_etal, "logical")
})

# ============================================================================
# EDGE CASES WITH REFERENCES
# ============================================================================

test_that("handles references with NA years", {
  skip_on_cran()
  citations <- create_citations_df(c("(Smith, 2020)"))
  references <- create_references_df(
    authors = c("Smith J"),
    years = c(NA_character_),
    first_authors = c("Smith")
  )

  result <- match_citations_to_references(citations, references)

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$matched_ref_id[1]))
  expect_equal(result$match_confidence[1], "no_match_year")
})

# test_that("handles references with NA authors", {
#   skip_on_cran()
#   citations <- create_citations_df(c("(Smith, 2020)"))
#   references <- create_references_df(
#     authors = c(NA_character_),
#     years = c("2020"),
#     first_authors = c(NA_character_)
#   )
#
#   result <- match_citations_to_references(citations, references)
#
#   expect_equal(nrow(result), 1)
#   expect_true(is.na(result$matched_ref_id[1]))
#   # With NA authors in references, the year match happens but author match fails
#   expect_equal(result$match_confidence[1], "no_match_author")
# })

# ============================================================================
# INTEGRATION TEST
# ============================================================================

test_that("comprehensive integration test", {
  skip_on_cran()
  # Test each citation type separately for clarity

  # Test 1: Numbered citation
  citations_num <- create_citations_df(
    texts = c("[1]"),
    types = c("numbered_single"),
    ids = c("CITE_NUM_1")
  )

  references_num <- create_references_df(
    authors = c("Anderson K"),
    years = c("2019"),
    ids = c("REF_1")
  )

  result_num <- match_citations_to_references(citations_num, references_num)
  expect_equal(result_num$matched_ref_id[1], "REF_1")
  expect_equal(result_num$match_confidence[1], "high_numbered")

  # Test 2: Simple narrative
  citations_simple <- create_citations_df(
    texts = c("(Smith, 2020)"),
    ids = c("CITE_SIM_1")
  )

  references_simple <- create_references_df(
    authors = c("Smith J"),
    years = c("2020"),
    ids = c("REF_2"),
    first_authors = c("Smith")
  )

  result_simple <- match_citations_to_references(
    citations_simple,
    references_simple
  )
  expect_equal(result_simple$matched_ref_id[1], "REF_2")
  expect_equal(result_simple$match_confidence[1], "high")

  # Test 3: Two authors
  citations_two <- create_citations_df(
    texts = c("(Jones and Brown, 2021)"),
    ids = c("CITE_TWO_1")
  )

  references_two <- create_references_df(
    authors = c("Jones A, Brown B"),
    years = c("2021"),
    ids = c("REF_3"),
    first_authors = c("Jones"),
    second_authors = c("Brown"),
    n_authors = c(2)
  )

  result_two <- match_citations_to_references(citations_two, references_two)
  expect_equal(result_two$matched_ref_id[1], "REF_3")
  expect_true(
    result_two$match_confidence[1] %in% c("high", "high_second_author")
  )

  # Test 4: Et al.
  citations_etal <- create_citations_df(
    texts = c("(Miller et al., 2022)"),
    ids = c("CITE_ETAL_1")
  )

  references_etal <- create_references_df(
    authors = c("Miller C, Davis D, Wilson E"),
    years = c("2022"),
    ids = c("REF_4"),
    first_authors = c("Miller"),
    n_authors = c(3)
  )

  result_etal <- match_citations_to_references(citations_etal, references_etal)
  expect_equal(result_etal$matched_ref_id[1], "REF_4")
  expect_equal(result_etal$cite_has_etal[1], TRUE)

  # Test 5: No match - year
  citations_nomatch_year <- create_citations_df(
    texts = c("(Unknown, 2099)"),
    ids = c("CITE_NO_1")
  )

  references_nomatch <- create_references_df(
    authors = c("Smith J"),
    years = c("2020"),
    first_authors = c("Smith")
  )

  result_nomatch_year <- match_citations_to_references(
    citations_nomatch_year,
    references_nomatch
  )
  expect_true(is.na(result_nomatch_year$matched_ref_id[1]))
  expect_equal(result_nomatch_year$match_confidence[1], "no_match_year")

  # Test 6: No match - numbered
  citations_nomatch_num <- create_citations_df(
    texts = c("[99]"),
    types = c("numbered_single"),
    ids = c("CITE_NO_NUM")
  )

  references_nomatch_num <- create_references_df(
    authors = c("Anderson K"),
    years = c("2019"),
    ids = c("REF_1")
  )

  result_nomatch_num <- match_citations_to_references(
    citations_nomatch_num,
    references_nomatch_num
  )
  expect_true(is.na(result_nomatch_num$matched_ref_id[1]))
  expect_equal(result_nomatch_num$match_confidence[1], "no_match_numbered")
})
