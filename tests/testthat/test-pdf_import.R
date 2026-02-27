# Test suite for pdf2txt_multicolumn_safe function
# Required packages: testthat (>= 3.0.0), pdftools

library(testthat)

# Helper function to create mock PDF data
create_mock_pdf_data <- function(
  n_rows = 100,
  n_columns = 2,
  page_width = 600
) {
  if (n_columns == 1) {
    data.frame(
      x = runif(n_rows, 50, page_width - 50),
      y = sort(runif(n_rows, 50, 700)),
      width = runif(n_rows, 20, 100),
      height = rep(12, n_rows),
      text = replicate(
        n_rows,
        paste(sample(letters, 5, replace = TRUE), collapse = "")
      ),
      stringsAsFactors = FALSE
    )
  } else {
    col_width <- page_width / n_columns
    column_data_list <- lapply(1:n_columns, function(col) {
      x_start <- (col - 1) * col_width + 50
      x_end <- col * col_width - 50
      rows_per_col <- n_rows %/% n_columns
      data.frame(
        x = runif(rows_per_col, x_start, x_end),
        y = sort(runif(rows_per_col, 50, 700)),
        width = runif(rows_per_col, 20, 100),
        height = rep(12, rows_per_col),
        text = replicate(
          rows_per_col,
          paste(sample(letters, 5, replace = TRUE), collapse = "")
        ),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, column_data_list)
  }
}

# Test 1: Basic functionality with default parameters
test_that("pdf2txt_multicolumn_safe works with default parameters", {
  skip_on_cran()
  skip_if_not_installed("pdftools")

  mock_data <- list(create_mock_pdf_data(n_rows = 50, n_columns = 2))

  local_mocked_bindings(
    pdf_data = function(file) mock_data,
    poppler_config = function() list(has_pdf_data = TRUE),
    .package = "pdftools"
  )

  result <- pdf2txt_multicolumn_safe("dummy.pdf")

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  expect_false(is.na(result))
})

# Test 2: Single column extraction
test_that("pdf2txt_multicolumn_safe handles single column correctly", {
  skip_on_cran()
  skip_if_not_installed("pdftools")

  mock_data <- list(create_mock_pdf_data(n_rows = 50, n_columns = 1))

  local_mocked_bindings(
    pdf_data = function(file) mock_data,
    poppler_config = function() list(has_pdf_data = TRUE),
    .package = "pdftools"
  )

  result <- pdf2txt_multicolumn_safe("dummy.pdf", n_columns = 1)

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

# Test 3: Two column extraction with explicit parameter
test_that("pdf2txt_multicolumn_safe handles two columns explicitly", {
  skip_on_cran()
  skip_if_not_installed("pdftools")

  mock_data <- list(create_mock_pdf_data(n_rows = 100, n_columns = 2))

  local_mocked_bindings(
    pdf_data = function(file) mock_data,
    poppler_config = function() list(has_pdf_data = TRUE),
    .package = "pdftools"
  )

  result <- pdf2txt_multicolumn_safe("dummy.pdf", n_columns = 2)

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

# Test 4: Three column extraction
test_that("pdf2txt_multicolumn_safe handles three columns", {
  skip_on_cran()
  skip_if_not_installed("pdftools")

  mock_data <- list(create_mock_pdf_data(n_rows = 150, n_columns = 3))

  local_mocked_bindings(
    pdf_data = function(file) mock_data,
    poppler_config = function() list(has_pdf_data = TRUE),
    .package = "pdftools"
  )

  result <- pdf2txt_multicolumn_safe("dummy.pdf", n_columns = 3)

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

# Test 5: Structure preservation TRUE vs FALSE
test_that("preserve_structure parameter affects output format", {
  skip_on_cran()
  skip_if_not_installed("pdftools")

  mock_data <- list(create_mock_pdf_data(n_rows = 50, n_columns = 2))

  local_mocked_bindings(
    pdf_data = function(file) mock_data,
    poppler_config = function() list(has_pdf_data = TRUE),
    .package = "pdftools"
  )

  result_structured <- pdf2txt_multicolumn_safe(
    "dummy.pdf",
    preserve_structure = TRUE
  )
  result_continuous <- pdf2txt_multicolumn_safe(
    "dummy.pdf",
    preserve_structure = FALSE
  )

  # Structured text should have newlines
  expect_true(grepl("\n", result_structured))

  # Both should be valid strings
  expect_type(result_structured, "character")
  expect_type(result_continuous, "character")
})

# Test 6: Custom column threshold
test_that("custom column_threshold is respected", {
  skip_on_cran()
  skip_if_not_installed("pdftools")

  mock_data <- list(create_mock_pdf_data(
    n_rows = 100,
    n_columns = 2,
    page_width = 600
  ))

  local_mocked_bindings(
    pdf_data = function(file) mock_data,
    poppler_config = function() list(has_pdf_data = TRUE),
    .package = "pdftools"
  )

  result <- pdf2txt_multicolumn_safe("dummy.pdf", column_threshold = 300)

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

# Test 7: Empty page handling
test_that("empty pages are handled gracefully", {
  skip_on_cran()
  skip_if_not_installed("pdftools")

  mock_data <- list(
    data.frame(
      x = numeric(0),
      y = numeric(0),
      width = numeric(0),
      height = numeric(0),
      text = character(0),
      stringsAsFactors = FALSE
    )
  )

  local_mocked_bindings(
    pdf_data = function(file) mock_data,
    poppler_config = function() list(has_pdf_data = TRUE),
    .package = "pdftools"
  )

  result <- pdf2txt_multicolumn_safe("dummy.pdf")

  expect_type(result, "character")
  expect_equal(nchar(result), 0)
})

# Test 8: Multiple pages
test_that("multiple pages are processed correctly", {
  skip_on_cran()
  skip_if_not_installed("pdftools")

  mock_data <- list(
    create_mock_pdf_data(n_rows = 50, n_columns = 2),
    create_mock_pdf_data(n_rows = 50, n_columns = 2),
    create_mock_pdf_data(n_rows = 50, n_columns = 2)
  )

  local_mocked_bindings(
    pdf_data = function(file) mock_data,
    poppler_config = function() list(has_pdf_data = TRUE),
    .package = "pdftools"
  )

  result <- pdf2txt_multicolumn_safe("dummy.pdf")

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

# Test 9: Fallback to pdf_text when pdf_data fails
test_that("fallback to pdf_text works when pdf_data fails", {
  skip_on_cran()
  skip_if_not_installed("pdftools")

  mock_text <- c("Page 1 text content", "Page 2 text content")

  local_mocked_bindings(
    pdf_data = function(file) stop("pdf_data error"),
    pdf_length = function(file) 2,
    pdf_text = function(file) mock_text,
    poppler_config = function() list(has_pdf_data = TRUE),
    .package = "pdftools"
  )

  expect_message(
    result <- pdf2txt_multicolumn_safe("dummy.pdf"),
    "pdf_data failed"
  )

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

# Test 10: No pdf_data support (old poppler version)
test_that("returns NA when pdf_data is not supported", {
  skip_on_cran()
  skip_if_not_installed("pdftools")

  local_mocked_bindings(
    poppler_config = function() list(has_pdf_data = FALSE),
    .package = "pdftools"
  )

  expect_message(
    result <- pdf2txt_multicolumn_safe("dummy.pdf"),
    "requires a recent version"
  )

  expect_true(is.na(result))
})

# Test 11: Hyphenation removal
test_that("hyphenation at line breaks is removed", {
  skip_on_cran()
  skip_if_not_installed("pdftools")

  mock_data <- list(
    data.frame(
      x = c(100, 100, 100),
      y = c(100, 115, 130),
      width = c(50, 50, 50),
      height = c(12, 12, 12),
      text = c("exam-", "ple", "text"),
      stringsAsFactors = FALSE
    )
  )

  local_mocked_bindings(
    pdf_data = function(file) mock_data,
    poppler_config = function() list(has_pdf_data = TRUE),
    .package = "pdftools"
  )

  result <- pdf2txt_multicolumn_safe("dummy.pdf", n_columns = 1)

  # Should not contain "exam- ple", should be "example"
  expect_false(grepl("exam-\\s", result))
})

# Test 12: Input validation - file not found
test_that("function handles invalid inputs appropriately", {
  skip_on_cran()
  skip_if_not_installed("pdftools")

  local_mocked_bindings(
    pdf_data = function(file) stop("File not found"),
    pdf_length = function(file) stop("File not found"),
    pdf_text = function(file) stop("File not found"),
    poppler_config = function() list(has_pdf_data = TRUE),
    .package = "pdftools"
  )

  expect_error(
    pdf2txt_multicolumn_safe("nonexistent.pdf")
  )
})

# Test 13: Column detection with sparse data
test_that("automatic column detection handles sparse data", {
  skip_on_cran()
  skip_if_not_installed("pdftools")

  # Only 10 unique x positions (sparse)
  sparse_data <- data.frame(
    x = rep(c(100, 400), each = 5),
    y = sort(runif(10, 50, 700)),
    width = rep(50, 10),
    height = rep(12, 10),
    text = letters[1:10],
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    pdf_data = function(file) list(sparse_data),
    poppler_config = function() list(has_pdf_data = TRUE),
    .package = "pdftools"
  )

  result <- pdf2txt_multicolumn_safe("dummy.pdf")

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

# Test 14: Very few rows in a column (edge case)
test_that("handles columns with very few rows", {
  skip_on_cran()
  skip_if_not_installed("pdftools")

  # Left column has only 2 rows
  sparse_column_data <- data.frame(
    x = c(50, 50, 400, 400, 400, 400),
    y = c(100, 150, 100, 150, 200, 250),
    width = rep(50, 6),
    height = rep(12, 6),
    text = letters[1:6],
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    pdf_data = function(file) list(sparse_column_data),
    poppler_config = function() list(has_pdf_data = TRUE),
    .package = "pdftools"
  )

  result <- pdf2txt_multicolumn_safe("dummy.pdf")

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

# Test 15: K-means with well-separated columns
test_that("k-means clustering works with well-separated columns", {
  skip_on_cran()
  skip_if_not_installed("pdftools")

  # Create data with very clear column separation
  left_data <- data.frame(
    x = runif(50, 50, 250),
    y = sort(runif(50, 50, 700)),
    width = rep(50, 50),
    height = rep(12, 50),
    text = replicate(
      50,
      paste(sample(letters, 5, replace = TRUE), collapse = "")
    ),
    stringsAsFactors = FALSE
  )

  right_data <- data.frame(
    x = runif(50, 350, 550),
    y = sort(runif(50, 50, 700)),
    width = rep(50, 50),
    height = rep(12, 50),
    text = replicate(
      50,
      paste(sample(letters, 5, replace = TRUE), collapse = "")
    ),
    stringsAsFactors = FALSE
  )

  clear_columns <- rbind(left_data, right_data)

  local_mocked_bindings(
    pdf_data = function(file) list(clear_columns),
    poppler_config = function() list(has_pdf_data = TRUE),
    .package = "pdftools"
  )

  result <- pdf2txt_multicolumn_safe("dummy.pdf", n_columns = 2)

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

# Test 16: Test with titles and headers (structure preservation)
test_that("structure preservation identifies titles", {
  skip_on_cran()
  skip_if_not_installed("pdftools")

  # Create data with title-like content
  title_data <- data.frame(
    x = c(100, 100, 100, 100, 100),
    y = c(100, 150, 200, 215, 230),
    width = c(200, 150, 50, 50, 50),
    height = c(18, 14, 12, 12, 12),
    text = c(
      "INTRODUCTION",
      "1. Background",
      "Normal text here.",
      "More text.",
      "Continues here."
    ),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    pdf_data = function(file) list(title_data),
    poppler_config = function() list(has_pdf_data = TRUE),
    .package = "pdftools"
  )

  result <- pdf2txt_multicolumn_safe(
    "dummy.pdf",
    n_columns = 1,
    preserve_structure = TRUE
  )

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  # Should have preserved some structure
  expect_true(grepl("\n", result))
})

# ============================================================================
# convert_superscript_citations() - UNIT TESTS
# ============================================================================

test_that("convert_superscript_citations converts citation after word", {
  result <- convert_superscript_citations("shown before 5 and")
  expect_true(grepl("\\[5\\]", result))
})

test_that("convert_superscript_citations does NOT convert non-citation words", {
  # table, figure, page, chapter should NOT trigger conversion
  result <- convert_superscript_citations("see table 2 for details")
  expect_false(grepl("\\[2\\]", result))

  result <- convert_superscript_citations("in figure 3 we show")
  expect_false(grepl("\\[3\\]", result))

  result <- convert_superscript_citations("on page 15 of the")
  expect_false(grepl("\\[15\\]", result))

  result <- convert_superscript_citations("in chapter 4 the authors")
  expect_false(grepl("\\[4\\]", result))
})

test_that("convert_superscript_citations handles comma-separated groups", {
  result <- convert_superscript_citations("evidence 1, 2, 3 shows")
  expect_true(grepl("\\[1,2,3\\]", result))
})

test_that("convert_superscript_citations converts Unicode superscripts", {
  result <- convert_superscript_citations("some text\u00b9\u00b2\u00b3 more text")
  expect_true(grepl("\\[123\\]", result))

  result <- convert_superscript_citations("word\u2074\u2075 here")
  expect_true(grepl("\\[45\\]", result))
})

test_that("convert_superscript_citations preserves commas in regular text", {
  result <- convert_superscript_citations("Smith, Jones, and Brown studied this")
  expect_true(grepl("Smith, Jones, and Brown", result))
})

test_that("convert_superscript_citations converts numbers at line start", {
  result <- convert_superscript_citations("end of sentence.\n15 The next part")
  expect_true(grepl("\\[15\\]", result))
})
