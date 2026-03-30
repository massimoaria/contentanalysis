# =============================================================
# Test script: Rhetorical Move Detection
# =============================================================
# Questo script testa classify_rhetorical_moves() in 3 modalità:
#   1. Solo rule-based (senza API key)
#   2. Ibrido rule + Gemini LLM
#   3. Integrato in analyze_scientific_content()
# =============================================================

devtools::load_all(".")

# --- 0. Carica un paper di esempio ----------------------------

# Opzione A: usa il paper di esempio incluso nel pacchetto
paper_path <- get_example_paper()
# pdf2txt_auto con sections=TRUE (default) ritorna già una named list di sezioni
sections <- pdf2txt_auto(
  paper_path,
  sections = TRUE,
  citation_type = "author_year"
)

# Opzione B: usa un tuo PDF
# sections <- pdf2txt_auto("path/to/your/paper.pdf")

# Opzione B2: se hai testo grezzo (non sezionato), usa split_into_sections
# text <- pdf2txt_auto("path/to/your/paper.pdf", sections = FALSE)
# sections <- split_into_sections(text)

# Opzione C: testo sintetico per test rapido (senza PDF)
sections_test <- list(
  Introduction = paste(
    "Climate change is one of the most pressing challenges facing humanity today.",
    "Extensive research has demonstrated the impact of rising temperatures on biodiversity.",
    "Several studies have investigated the relationship between CO2 emissions and ocean acidification.",
    "However, little is known about the long-term effects on deep-sea ecosystems.",
    "Previous studies have failed to account for synergistic effects between temperature and pH changes.",
    "This study aims to investigate the combined effects of warming and acidification on deep-sea coral communities.",
    "We hypothesize that the interaction between these two stressors produces nonlinear effects.",
    "The paper is organized as follows: Section 2 reviews the literature, Section 3 describes the methodology, and Section 4 presents the results."
  ),
  Discussion = paste(
    "The results of this study confirm that warming and acidification have synergistic negative effects on deep-sea corals.",
    "These findings are consistent with previous work by Roberts et al. (2020) who reported similar patterns in shallow-water species.",
    "However, our results differ from Chen et al. (2019) regarding the threshold temperature for coral bleaching.",
    "A possible explanation for this discrepancy is the different experimental duration used in our study.",
    "One limitation of this study is the relatively small sample size of coral colonies examined.",
    "Furthermore, the laboratory conditions may not fully replicate the complexity of natural deep-sea environments.",
    "Future research should focus on in-situ experiments with larger sample sizes.",
    "These findings have important implications for marine conservation policy in the context of climate change."
  )
)


# =============================================================
# 1. TEST RULE-BASED ONLY (nessuna API key necessaria)
# =============================================================
cat("\n========== TEST 1: Rule-based only ==========\n\n")

moves_rules <- classify_rhetorical_moves(
  text = sections_test,
  use_llm = FALSE
)

# Stampa il sommario
print(moves_rules)

# Esplora i risultati nel dettaglio
cat("\n--- Sentence-level classification ---\n")
print(moves_rules$sentences[, c(
  "sentence_id",
  "section",
  "move",
  "step",
  "confidence"
)])

cat("\n--- Aggregated move blocks ---\n")
print(moves_rules$move_blocks[, c(
  "block_id",
  "section",
  "move",
  "step",
  "n_sentences",
  "avg_confidence"
)])

cat("\n--- Move distribution ---\n")
print(moves_rules$summary$move_distribution)

cat("\n--- Flow pattern ---\n")
cat(paste(moves_rules$summary$flow_pattern, collapse = " -> "), "\n")


# =============================================================
# 2. TEST IBRIDO: Rule-based + Gemini LLM
# =============================================================
cat("\n========== TEST 2: Hybrid (rules + Gemini LLM) ==========\n\n")

# Assicurati di avere la API key impostata:
# Sys.setenv(GEMINI_API_KEY = "your-api-key-here")

if (Sys.getenv("GEMINI_API_KEY") != "") {
  moves_hybrid <- classify_rhetorical_moves(
    text = sections_test,
    use_llm = TRUE,
    model = "2.5-flash",
    batch_size = 20
  )

  print(moves_hybrid)

  # Confronta i metodi
  cat("\n--- Method breakdown ---\n")
  print(moves_hybrid$summary$method_breakdown)

  # Confronta rule-only vs hybrid per la stessa frase
  cat("\n--- Confronto rule-only vs hybrid ---\n")
  comparison <- merge(
    moves_rules$sentences[, c("sentence_id", "section", "move", "confidence")],
    moves_hybrid$sentences[, c("sentence_id", "move", "confidence")],
    by = "sentence_id",
    suffixes = c("_rules", "_hybrid")
  )
  comparison$agree <- comparison$move_rules == comparison$move_hybrid
  print(comparison)

  cat(
    "\nAgreement rate: ",
    round(mean(comparison$agree, na.rm = TRUE) * 100, 1),
    "%\n"
  )
} else {
  cat("Skipped: GEMINI_API_KEY not set.\n")
  cat("Set it with: Sys.setenv(GEMINI_API_KEY = 'your-key')\n")
}


# =============================================================
# 3. TEST CON PDF REALE (via analyze_scientific_content)
# =============================================================
cat("\n========== TEST 3: Full pipeline con PDF ==========\n\n")

if (file.exists(paper_path) && Sys.getenv("GEMINI_API_KEY") != "") {
  result <- analyze_scientific_content(
    text = sections,
    rhetorical_moves = TRUE,
    rhetorical_model = "3.0-flash-preview" # attiva la classificazione dei moves
  )

  # I moves sono nell'output
  if (!is.null(result$rhetorical_moves)) {
    cat("Rhetorical moves detected!\n\n")
    print(result$rhetorical_moves)
  }
} else {
  cat("Skipped: need example PDF + GEMINI_API_KEY.\n")

  # Fallback: testa comunque con rule-based sul paper
  if (file.exists(paper_path)) {
    cat("Running rule-based on example paper...\n\n")
    moves_paper <- classify_rhetorical_moves(sections, use_llm = FALSE)
    print(moves_paper)
  }
}

cat("\n========== Done! ==========\n")
