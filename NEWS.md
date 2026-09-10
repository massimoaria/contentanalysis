# contentanalysis (development version)

* Fixed PDF import of names typeset in small capitals (#5). Poppler splits such
  a word into a full-size initial and a reduced-size remainder (`"S"` + `"MITH"`),
  and the extracted line was rebuilt by joining every word token with a space,
  inserting a space that does not exist in the document. Tokens are now joined
  using their horizontal geometry: fragments that abut on the page and carry the
  typographic signature of small capitals are glued back together, so author
  names such as `SMITH` and `CRONIN` are matched correctly.

# contentanalysis 1.1.1

# contentanalysis 1.1.0

* New `classify_rhetorical_moves()` function: classifies rhetorical moves at the sentence level in scientific text, based on Swales' CARS model and extensions for Literature Review and Discussion/Conclusion sections. Uses a hybrid rule-based approach, optionally enhanced with Google Gemini LLM classification
* `analyze_scientific_content()` gains `rhetorical_moves` and `rhetorical_model` arguments to optionally include rhetorical move classification in the analysis output
* Improved PDF import: better handling of multi-column layouts and automatic removal of running headers and first-page headers/footers
* Improved reference parsing with format-aware detection (parenthetical, bare-year, and numbered styles) and CrossRef enrichment of references extracted from the PDF
* Fixed R CMD check portability NOTE by replacing non-ASCII characters in `R/pdf_import.R` with Unicode escapes
* Fixed R CMD check NOTE about non-standard top-level files by adding `test_rhetorical_moves.R`, `tasks`, and `revdep` to `.Rbuildignore`

# contentanalysis 1.0.0

* Improved PDF import module to identify 
* Improved `plot_citation_clusters()`: TF-IDF bar chart now uses a 2-column grid layout with color-coded section title annotations
* Fixed R CMD check NOTE about hidden `.claude` directory by adding it to `.Rbuildignore`
* Fixed README: cluster plots are now rendered as static PNG images visible on GitHub
* Fixed README: word distribution plot no longer appears twice

# contentanalysis 0.2.1

* Improved reference matching by normalizing first author surnames
* Removed old Gemini models 1.5 and 2.0
* Updated documentation to reflect changes in reference matching and model removal


# contentanalysis 0.2.0

* Initial CRAN submission.
