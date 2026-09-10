# Tests for join_line_tokens(): geometry-aware joining of pdf_data tokens.
# Regression suite for issue #5 (small-caps author names split by poppler).

library(testthat)

# Build one line of tokens with explicit gaps between them.
# `gaps[i]` is the horizontal space left between token i and token i + 1.
make_line <- function(texts, gaps, widths = NULL, heights = NULL) {
  n <- length(texts)
  if (is.null(widths)) widths <- nchar(texts) * 5
  if (is.null(heights)) heights <- rep(10, n)
  if (length(gaps) == 1) gaps <- rep(gaps, max(n - 1, 0))

  x <- numeric(n)
  if (n >= 1) x[1] <- 50
  if (n > 1) {
    for (i in 2:n) x[i] <- x[i - 1] + widths[i - 1] + gaps[i - 1]
  }

  data.frame(
    x = x,
    y = rep(100, n),
    width = widths,
    height = heights,
    text = texts,
    stringsAsFactors = FALSE
  )
}

join <- function(...) contentanalysis:::join_line_tokens(make_line(...))

test_that("small-caps surnames split by poppler are rejoined", {
  # "SMITH" typeset in small caps: full-size "S" + reduced-size "MITH", gap 0
  expect_equal(
    join(c("contrast", "S", "MITH", "1981"),
         gaps = c(4, 0, 4),
         heights = c(10, 10, 8, 10)),
    "contrast SMITH 1981"
  )

  expect_equal(
    join(c("C", "RONIN", "(1981)"), gaps = c(0, 4), heights = c(10, 8, 10)),
    "CRONIN (1981)"
  )

  # A small amount of tracking inside the small-caps run is still a merge
  expect_equal(
    join(c("D", "URKHEIM"), gaps = 2, heights = c(10, 8)),
    "DURKHEIM"
  )
})

test_that("small-caps section headings are rejoined", {
  expect_equal(
    join(c("I", "NTRODUCTION"), gaps = 0, heights = c(12, 9)),
    "INTRODUCTION"
  )
})

test_that("a real word space is never removed", {
  # Word spaces measure roughly 0.4-0.5 of the font height
  expect_equal(join(c("Random", "Forest"), gaps = 4), "Random Forest")
  expect_equal(join(c("A", "NEW", "APPROACH"), gaps = 4), "A NEW APPROACH")
})

test_that("known false positives of the naive regex are not merged", {
  # "A NEW APPROACH" in a heading carries a real word space, so it survives
  expect_equal(
    join(c("A", "NEW", "APPROACH"), gaps = 4, heights = rep(12, 3)),
    "A NEW APPROACH"
  )

  # Right token must start with two uppercase letters
  expect_equal(join(c("A", "Better"), gaps = 0, heights = c(10, 8)), "A Better")
  expect_equal(join(c("R", "Core", "Team"), gaps = c(0, 4)), "R Core Team")
  expect_equal(join(c("AT&T", "Bell"), gaps = 0), "AT&T Bell")

  # Single-letter right token (table/panel labels, spaced initials)
  expect_equal(join(c("A", "B"), gaps = 0, heights = c(10, 8)), "A B")
  expect_equal(join(c("J", "R", "R"), gaps = 0, heights = c(10, 8, 8)), "J R R")
})

test_that("superscript citation markers stay detached", {
  # gap is 0 here, but the left token is not a single uppercase letter:
  # gluing would hide the marker from convert_superscript_citations()
  expect_equal(
    join(c("permutation.", "3"), gaps = 0, heights = c(10, 6)),
    "permutation. 3"
  )
  expect_equal(join(c("(MDS)", "2"), gaps = 0, heights = c(10, 6)), "(MDS) 2")
})

test_that("overlapping or far-apart tokens keep their space", {
  # Large gap: different column or block, must never be glued
  expect_equal(
    join(c("S", "MITH"), gaps = 70, heights = c(10, 8)),
    "S MITH"
  )
  # Strong negative gap: interleaved lines, treated as an anomaly
  expect_equal(
    join(c("S", "MITH"), gaps = -6, heights = c(10, 8)),
    "S MITH"
  )
})

test_that("equal heights still merge when the glyphs actually abut", {
  # A gap of zero means the glyphs touch on the page: whatever the font
  # sizes say, the reader sees one word, so joining is the faithful result.
  # This is reachable through letter-spaced words, where every token keeps
  # the same nominal size.
  expect_equal(join(c("S", "MITH"), gaps = 0, heights = c(10, 10)), "SMITH")
})

test_that("a taller right token is not small caps", {
  expect_equal(
    join(c("S", "MITH"), gaps = 0, heights = c(8, 10)),
    "S MITH"
  )
})

test_that("degenerate input is handled", {
  empty <- make_line(character(0), gaps = numeric(0))
  expect_equal(contentanalysis:::join_line_tokens(empty), "")

  expect_equal(join("Alone", gaps = numeric(0)), "Alone")

  # Missing geometry falls back to the legacy space-separated join
  no_geom <- data.frame(
    y = c(100, 100),
    height = c(10, 8),
    text = c("S", "MITH"),
    stringsAsFactors = FALSE
  )
  expect_equal(contentanalysis:::join_line_tokens(no_geom), "S MITH")

  # Missing height column must not error
  no_height <- data.frame(
    x = c(50, 55),
    y = c(100, 100),
    width = c(5, 20),
    text = c("S", "MITH"),
    stringsAsFactors = FALSE
  )
  expect_type(contentanalysis:::join_line_tokens(no_height), "character")
})

test_that("reconstruct_text_structured applies the geometric join", {
  line <- make_line(
    c("contrast", "S", "MITH", "and", "C", "RONIN"),
    gaps = c(4, 0, 4, 4, 0),
    heights = c(10, 10, 8, 10, 10, 8)
  )
  result <- contentanalysis:::reconstruct_text_structured(line, TRUE)

  expect_match(result, "SMITH", fixed = TRUE)
  expect_match(result, "CRONIN", fixed = TRUE)
  expect_false(grepl("S MITH", result, fixed = TRUE))
  expect_false(grepl("C RONIN", result, fixed = TRUE))
})
