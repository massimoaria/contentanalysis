#' Get rhetorical move taxonomies for different section types
#'
#' Returns cue phrase dictionaries organized by section type, move, and step.
#' Each entry contains regex-ready patterns and their base weight.
#'
#' @return Named list of taxonomies (introduction, literature_review, discussion)
#'
#' @keywords internal
#' @noRd
get_move_taxonomies <- function() {
  list(
    introduction = list(
      name = "Introduction (CARS Model)",
      moves = list(
        list(
          move = "M1",
          move_label = "Establishing a territory",
          steps = list(
            list(
              step = "Claiming centrality",
              cues = c(
                "\\bimportant\\b", "\\bsignificant\\b", "\\bessential\\b",
                "\\bcrucial\\b", "\\bwidely\\b", "\\bgrowing interest\\b",
                "\\bhas attracted\\b", "\\bplays a key role\\b",
                "\\bfundamental\\b", "\\bcentral\\b", "\\bcritical\\b",
                "\\bvital\\b", "\\breceived considerable attention\\b",
                "\\bof great importance\\b", "\\bincreasingly important\\b",
                "\\bhas become\\b", "\\brapidly growing\\b",
                "\\bmajor concern\\b", "\\bwidely recognized\\b",
                "\\bextensively studied\\b"
              ),
              weight = 0.8
            ),
            list(
              step = "Topic generalization",
              cues = c(
                "\\bit is well known\\b", "\\bresearch has shown\\b",
                "\\bstudies have demonstrated\\b", "\\bit is generally accepted\\b",
                "\\bthere is growing evidence\\b", "\\bit has been established\\b",
                "\\bprevious research\\b", "\\bprior studies\\b",
                "\\ba large body of\\b", "\\bthe literature suggests\\b",
                "\\bit is widely accepted\\b", "\\bscholars have noted\\b",
                "\\bresearchers have found\\b", "\\bevidence suggests\\b",
                "\\bit has been shown\\b", "\\bit is commonly assumed\\b",
                "\\bthere is a consensus\\b"
              ),
              weight = 0.8
            ),
            list(
              step = "Reviewing previous research",
              cues = c(
                "\\bfound that\\b", "\\breported that\\b",
                "\\bshowed that\\b", "\\bdemonstrated that\\b",
                "\\binvestigated\\b", "\\bexamined\\b",
                "\\banalyzed\\b", "\\bstudied\\b",
                "\\bproposed\\b", "\\bdeveloped\\b",
                "\\bintroduced\\b", "\\bexplored\\b",
                "\\bobserved that\\b", "\\bnoted that\\b",
                "\\bargued that\\b", "\\bconcluded that\\b",
                "\\bconfirmed that\\b", "\\bsuggested that\\b",
                "\\bidentified\\b", "\\brevealed that\\b"
              ),
              weight = 0.7
            )
          )
        ),
        list(
          move = "M2",
          move_label = "Establishing a niche",
          steps = list(
            list(
              step = "Counter-claiming",
              cues = c(
                "\\bhowever\\b", "\\balthough\\b",
                "\\bnevertheless\\b", "\\bin contrast\\b",
                "\\bon the other hand\\b", "\\bcontradicts\\b",
                "\\bchallenges\\b", "\\bwhile\\b.*\\b(some|most|many)\\b",
                "\\bdespite\\b", "\\byet\\b",
                "\\bconversely\\b", "\\bnonetheless\\b",
                "\\bcontrary to\\b"
              ),
              weight = 0.7
            ),
            list(
              step = "Indicating a gap",
              cues = c(
                "\\blittle is known\\b", "\\bno study\\b",
                "\\bfew studies\\b", "\\bremains unclear\\b",
                "\\bhas not been\\b", "\\bpoorly understood\\b",
                "\\black of\\b", "\\bgap\\b",
                "\\blimited research\\b", "\\boverlooked\\b",
                "\\bunderexplored\\b", "\\bunder-explored\\b",
                "\\binsufficient\\b", "\\bscarcely\\b",
                "\\bno attempt\\b", "\\bhas yet to\\b",
                "\\bremain(s)? unknown\\b", "\\bhas been neglected\\b",
                "\\binsufficiently\\b", "\\bnot yet been\\b",
                "\\blittle attention\\b", "\\bless attention\\b",
                "\\brarely\\b.*\\b(addressed|studied|examined|investigated)\\b"
              ),
              weight = 0.9
            ),
            list(
              step = "Raising questions",
              cues = c(
                "\\bwhether\\b", "\\bit is unclear\\b",
                "\\bremains to be seen\\b", "\\bopen question\\b",
                "\\buncertain\\b", "\\bdebated\\b",
                "\\bcontroversial\\b", "\\bremains? an open\\b",
                "\\bthe question\\b.*\\barises\\b",
                "\\bstill unclear\\b", "\\bnot well understood\\b"
              ),
              weight = 0.8
            )
          )
        ),
        list(
          move = "M3",
          move_label = "Occupying the niche",
          steps = list(
            list(
              step = "Announcing purpose",
              cues = c(
                "\\bthis study\\b", "\\bthis paper\\b",
                "\\bwe aim\\b", "\\bthe purpose\\b",
                "\\bthe goal\\b", "\\bwe propose\\b",
                "\\bwe investigate\\b", "\\bthe objective\\b",
                "\\bin this work\\b", "\\bthis research\\b",
                "\\bthe present study\\b", "\\bwe examine\\b",
                "\\bwe explore\\b", "\\bwe address\\b",
                "\\bwe seek to\\b", "\\bwe attempt to\\b",
                "\\bthe aim of\\b", "\\bthis article\\b",
                "\\bherein\\b", "\\bwe present\\b",
                "\\bout aim\\b", "\\bwe focus on\\b",
                "\\bthe current study\\b"
              ),
              weight = 0.9
            ),
            list(
              step = "Announcing findings",
              cues = c(
                "\\bwe found\\b", "\\bour results\\b",
                "\\bwe show\\b", "\\bfindings indicate\\b",
                "\\bresults demonstrate\\b", "\\bour analysis\\b",
                "\\bwe demonstrate\\b", "\\bour findings\\b",
                "\\bwe report\\b", "\\bwe observe\\b"
              ),
              weight = 0.8
            ),
            list(
              step = "Indicating structure",
              cues = c(
                "\\bis organized as follows\\b", "\\bthe remainder\\b",
                "\\bsection\\s+\\d+\\b", "\\bthis paper is structured\\b",
                "\\bthe rest of (this|the) paper\\b",
                "\\bwe first\\b.*\\bthen\\b",
                "\\bthe following sections\\b",
                "\\bwe begin by\\b", "\\bin the next section\\b",
                "\\bthe outline\\b"
              ),
              weight = 0.9
            )
          )
        )
      )
    ),
    literature_review = list(
      name = "Literature Review",
      moves = list(
        list(
          move = "M1",
          move_label = "Establishing context",
          steps = list(
            list(
              step = "Defining concepts",
              cues = c(
                "\\bis defined as\\b", "\\brefers to\\b",
                "\\bcan be described as\\b", "\\bis characterized by\\b",
                "\\bis understood as\\b", "\\bthe term\\b",
                "\\bthe concept of\\b", "\\baccording to\\b",
                "\\bdenotes\\b", "\\bthe definition\\b",
                "\\bis commonly referred\\b", "\\bcan be classified\\b"
              ),
              weight = 0.8
            ),
            list(
              step = "Historical overview",
              cues = c(
                "\\bhistorically\\b", "\\btraditionally\\b",
                "\\bsince\\b.*\\b(19|20)\\d{2}\\b", "\\bdates back\\b",
                "\\bearly work\\b", "\\bpioneered\\b",
                "\\boriginated\\b", "\\bearly studies\\b",
                "\\bhas evolved\\b", "\\bover the past\\b",
                "\\bhas a long history\\b", "\\bwas first\\b",
                "\\binitially\\b.*\\b(proposed|developed|introduced)\\b"
              ),
              weight = 0.8
            )
          )
        ),
        list(
          move = "M2",
          move_label = "Reviewing prior work",
          steps = list(
            list(
              step = "Summarizing studies",
              cues = c(
                "\\bfound that\\b", "\\breported\\b",
                "\\bshowed\\b", "\\bdemonstrated\\b",
                "\\bobserved\\b", "\\bconcluded\\b",
                "\\bsuggested\\b", "\\brevealed\\b",
                "\\bconfirmed\\b", "\\bestablished\\b",
                "\\bnoted\\b", "\\bhighlighted\\b",
                "\\bpointed out\\b", "\\bdetermined\\b"
              ),
              weight = 0.7
            ),
            list(
              step = "Comparing approaches",
              cues = c(
                "\\bcompared to\\b", "\\bin contrast to\\b",
                "\\bunlike\\b", "\\bsimilarly\\b",
                "\\bconsistent with\\b", "\\bdiffers from\\b",
                "\\bin comparison\\b", "\\bwhereas\\b",
                "\\bwhile\\b.*\\b(found|showed|used)\\b",
                "\\brelative to\\b", "\\bas opposed to\\b",
                "\\bon the contrary\\b"
              ),
              weight = 0.8
            ),
            list(
              step = "Identifying trends",
              cues = c(
                "\\bincreasingly\\b", "\\brecent studies\\b",
                "\\bemerging\\b", "\\btrend\\b",
                "\\bshift toward\\b", "\\bgrowing body\\b",
                "\\brecent years\\b", "\\brecent research\\b",
                "\\bmore recently\\b", "\\ba number of studies\\b",
                "\\brecent advances\\b", "\\bseveral studies\\b"
              ),
              weight = 0.7
            )
          )
        ),
        list(
          move = "M3",
          move_label = "Evaluating prior work",
          steps = list(
            list(
              step = "Noting strengths",
              cues = c(
                "\\beffectively\\b", "\\bsuccessfully\\b",
                "\\badvantage\\b", "\\bstrength\\b",
                "\\brobust\\b", "\\bsuperior\\b",
                "\\boutperform\\b", "\\bpromising\\b",
                "\\bsignificant improvement\\b",
                "\\bstate.of.the.art\\b", "\\befficient\\b"
              ),
              weight = 0.7
            ),
            list(
              step = "Noting limitations",
              cues = c(
                "\\bhowever\\b", "\\blimitation\\b",
                "\\bdrawback\\b", "\\bfails to\\b",
                "\\bdoes not account for\\b", "\\brestricted to\\b",
                "\\bsuffers from\\b", "\\bshortcoming\\b",
                "\\bweakness\\b", "\\binadequate\\b",
                "\\bcannot handle\\b", "\\bpoorly\\b",
                "\\binsufficient\\b", "\\bnarrow\\b"
              ),
              weight = 0.8
            ),
            list(
              step = "Synthesizing",
              cues = c(
                "\\btaken together\\b", "\\boverall\\b",
                "\\bin summary\\b", "\\bcollectively\\b",
                "\\bthe body of evidence\\b", "\\bin general\\b",
                "\\bto sum up\\b", "\\bin aggregate\\b",
                "\\bbroadly speaking\\b", "\\bthe consensus\\b",
                "\\bon the whole\\b", "\\bthe above review\\b"
              ),
              weight = 0.8
            )
          )
        )
      )
    ),
    discussion = list(
      name = "Discussion / Conclusion",
      moves = list(
        list(
          move = "M1",
          move_label = "Consolidating results",
          steps = list(
            list(
              step = "Restating findings",
              cues = c(
                "\\bour results show\\b", "\\bwe found that\\b",
                "\\bthe findings indicate\\b", "\\bas shown\\b",
                "\\bthe analysis reveals\\b", "\\bour data suggest\\b",
                "\\bthe present results\\b", "\\bour study found\\b",
                "\\bthe results of this study\\b",
                "\\bour findings show\\b", "\\bwe observed\\b",
                "\\bthe current study found\\b",
                "\\bour analysis shows\\b", "\\bthis study shows\\b"
              ),
              weight = 0.9
            ),
            list(
              step = "Interpreting results",
              cues = c(
                "\\bthis suggests\\b", "\\bthis implies\\b",
                "\\bthis can be explained\\b", "\\ba possible explanation\\b",
                "\\bthis may be due to\\b", "\\bthis indicates\\b",
                "\\bone possible reason\\b", "\\bthis might reflect\\b",
                "\\bthis could be attributed\\b",
                "\\bthis finding suggests\\b", "\\bthis is likely\\b",
                "\\bplausible explanation\\b", "\\bwe interpret\\b",
                "\\bthis points to\\b", "\\bthe reason for\\b"
              ),
              weight = 0.8
            ),
            list(
              step = "Comparing with literature",
              cues = c(
                "\\bconsistent with\\b", "\\bin line with\\b",
                "\\bsupports\\b", "\\bcontradicts\\b",
                "\\bin agreement with\\b", "\\bcontrary to\\b",
                "\\bsimilar to\\b.*\\b(finding|result|study)\\b",
                "\\baligns with\\b", "\\bcorroborates\\b",
                "\\bin accord with\\b", "\\bconfirms\\b.*\\b(previous|earlier|prior)\\b",
                "\\bat odds with\\b", "\\bdiffers from\\b",
                "\\bechoes\\b", "\\breinforces\\b"
              ),
              weight = 0.8
            )
          )
        ),
        list(
          move = "M2",
          move_label = "Evaluating the study",
          steps = list(
            list(
              step = "Acknowledging limitations",
              cues = c(
                "\\blimitation\\b", "\\bhowever,? this study\\b",
                "\\bshould be interpreted with caution\\b",
                "\\bsmall sample\\b", "\\bmay not generalize\\b",
                "\\ba potential weakness\\b", "\\bwe acknowledge\\b",
                "\\bcaution\\b", "\\bgeneralizability\\b",
                "\\brestricted\\b", "\\bconfined to\\b",
                "\\bcannot rule out\\b", "\\bshould be noted\\b",
                "\\bdespite\\b.*\\blimitation\\b",
                "\\bfuture studies should address\\b"
              ),
              weight = 0.9
            ),
            list(
              step = "Claiming significance",
              cues = c(
                "\\bcontributes to\\b", "\\badds to\\b",
                "\\bprovides evidence\\b", "\\bnovel\\b",
                "\\bfirst to\\b", "\\bimportant contribution\\b",
                "\\badvances\\b.*\\bunderstanding\\b",
                "\\bunique contribution\\b", "\\bsignificant contribution\\b",
                "\\bkey contribution\\b", "\\boriginal\\b",
                "\\bpioneering\\b", "\\bgroundbreaking\\b",
                "\\bfills\\b.*\\bgap\\b", "\\bextends\\b.*\\bliterature\\b"
              ),
              weight = 0.8
            )
          )
        ),
        list(
          move = "M3",
          move_label = "Looking forward",
          steps = list(
            list(
              step = "Recommending future research",
              cues = c(
                "\\bfuture research\\b", "\\bfurther investigation\\b",
                "\\bshould explore\\b", "\\bremains to be\\b",
                "\\bwarrants further\\b", "\\bfuture studies\\b",
                "\\bfurther studies\\b", "\\bfuture work\\b",
                "\\bmore research is needed\\b",
                "\\badditional research\\b", "\\bfurther research\\b",
                "\\bit would be valuable\\b", "\\bfuture directions\\b"
              ),
              weight = 0.9
            ),
            list(
              step = "Suggesting applications",
              cues = c(
                "\\bpractical implications\\b", "\\bcan be applied\\b",
                "\\bpractitioners\\b", "\\bpolicymakers\\b",
                "\\brecommendations\\b", "\\bimplications for\\b",
                "\\bhas practical\\b", "\\breal.world\\b",
                "\\bmanagerial implications\\b",
                "\\bclinical implications\\b",
                "\\bpolicy implications\\b",
                "\\bcan inform\\b", "\\bmay help\\b"
              ),
              weight = 0.8
            ),
            list(
              step = "Drawing conclusions",
              cues = c(
                "\\bin conclusion\\b", "\\bto summarize\\b",
                "\\bin sum\\b", "\\boverall\\b",
                "\\btaken together\\b", "\\bin closing\\b",
                "\\bto conclude\\b", "\\bin summary\\b",
                "\\bwe conclude\\b", "\\bthis study demonstrates\\b",
                "\\bout study contributes\\b",
                "\\bthe findings highlight\\b"
              ),
              weight = 0.9
            )
          )
        )
      )
    )
  )
}


#' Map a section name to its taxonomy type
#'
#' @param section_name Character string with the section name
#'
#' @return Character string: "introduction", "literature_review", or "discussion"
#'
#' @keywords internal
#' @noRd
map_section_to_taxonomy <- function(section_name) {
  name_lower <- tolower(section_name)

  if (grepl("introduction|\\bintro\\b", name_lower)) {
    return("introduction")
  }

  if (grepl(
    "literature|related work|background|state of the art|theoretical framework|review",
    name_lower
  )) {
    return("literature_review")
  }

  if (grepl("discussion|conclusion|implication|closing remarks", name_lower)) {
    return("discussion")
  }

  if (grepl("method|material|experimental|procedure|design|data collection", name_lower)) {
    return(NA_character_)
  }

  # Default to introduction taxonomy for unrecognized sections
  "introduction"
}


#' Check if a sentence contains citation patterns
#'
#' @param text Character string
#'
#' @return Logical
#'
#' @keywords internal
#' @noRd
has_citation <- function(text) {
  # Author-year: (Smith, 2020) or Smith (2020) or Smith et al. (2020)
  author_year <- grepl(
    "\\([A-Z][a-z]+.*?(19|20)\\d{2}\\)|[A-Z][a-z]+\\s+(et\\s+al\\.?\\s+)?\\((19|20)\\d{2}\\)",
    text,
    perl = TRUE
  )

  # Numeric: [1], [2,3], [1-5]

  numeric_cite <- grepl("\\[\\d+[,;\\-\\d\\s]*\\]", text, perl = TRUE)

  author_year | numeric_cite
}


#' Apply rule-based rhetorical move classification
#'
#' Classifies sentences using cue phrase dictionaries, position, and citation presence.
#'
#' @param sentences_df Tibble with columns: sentence_id, section, text, position_pct, taxonomy_type
#'
#' @return The input tibble with added columns: move_rule, step_rule, conf_rule
#'
#' @keywords internal
#' @noRd
apply_rule_based_moves <- function(sentences_df) {
  taxonomies <- get_move_taxonomies()

  move_rule <- character(nrow(sentences_df))
  step_rule <- character(nrow(sentences_df))
  conf_rule <- numeric(nrow(sentences_df))

  for (i in seq_len(nrow(sentences_df))) {
    row <- sentences_df[i, ]
    tax_type <- row$taxonomy_type

    # Skip if no taxonomy assigned
    if (is.na(tax_type)) {
      move_rule[i] <- NA_character_
      step_rule[i] <- NA_character_
      conf_rule[i] <- 0
      next
    }

    taxonomy <- taxonomies[[tax_type]]
    text_lower <- tolower(row$text)
    pos_pct <- row$position_pct
    cite_present <- has_citation(row$text)

    best_score <- 0
    best_move <- NA_character_
    best_step <- NA_character_

    for (move_def in taxonomy$moves) {
      for (step_def in move_def$steps) {
        # Count matching cue phrases
        matches <- vapply(step_def$cues, function(cue) {
          grepl(cue, text_lower, ignore.case = TRUE, perl = TRUE)
        }, logical(1))

        n_matches <- sum(matches)

        if (n_matches == 0) next

        # Base score from matches and weight
        score <- min(1.0, n_matches * step_def$weight * 0.5)

        # Position bonus
        score <- score + position_bonus(
          move_def$move, pos_pct, tax_type
        )

        # Citation bonus for reviewing/summarizing steps
        if (cite_present && grepl(
          "Reviewing|Summarizing|Comparing",
          step_def$step
        )) {
          score <- score + 0.15
        }

        score <- min(1.0, score)

        if (score > best_score) {
          best_score <- score
          best_move <- paste0(move_def$move, ": ", move_def$move_label)
          best_step <- step_def$step
        }
      }
    }

    if (best_score > 0) {
      move_rule[i] <- best_move
      step_rule[i] <- best_step
      conf_rule[i] <- round(best_score, 3)
    } else {
      move_rule[i] <- NA_character_
      step_rule[i] <- NA_character_
      conf_rule[i] <- 0
    }
  }

  sentences_df$move_rule <- move_rule
  sentences_df$step_rule <- step_rule
  sentences_df$conf_rule <- conf_rule

  sentences_df
}


#' Calculate position-based bonus for move scoring
#'
#' @param move Character. Move code (M1, M2, M3)
#' @param position_pct Numeric. Position in section (0-100)
#' @param taxonomy_type Character. Taxonomy type
#'
#' @return Numeric bonus value
#'
#' @keywords internal
#' @noRd
position_bonus <- function(move, position_pct, taxonomy_type) {
  bonus <- 0

  if (taxonomy_type == "introduction") {
    # M1 more likely at beginning, M3 at end
    if (move == "M1" && position_pct <= 40) bonus <- 0.15
    if (move == "M2" && position_pct > 30 && position_pct <= 70) bonus <- 0.15
    if (move == "M3" && position_pct > 60) bonus <- 0.20
  } else if (taxonomy_type == "discussion") {
    # M1 more likely at beginning, M3 at end
    if (move == "M1" && position_pct <= 50) bonus <- 0.15
    if (move == "M2" && position_pct > 40 && position_pct <= 80) bonus <- 0.10
    if (move == "M3" && position_pct > 60) bonus <- 0.20
  } else if (taxonomy_type == "literature_review") {
    # M1 at beginning, M3 at end, M2 throughout
    if (move == "M1" && position_pct <= 30) bonus <- 0.15
    if (move == "M3" && position_pct > 70) bonus <- 0.15
  }

  bonus
}
