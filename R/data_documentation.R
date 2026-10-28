#' Cohort and rhyme competition during spoken word recognition
#'
#' Fixation time course data from 8 participants with aphasia and 12 age-matched control participants.
#'
#' @format A data frame with 1600 rows and 7 variables:
#' \describe{
#'  \item{subjID}{subject ID, factor}
#'  \item{Group}{subject group: Control, Broca's aphasia, Wernicke's aphasia}
#'  \item{Time}{time (in ms) since word onset in 100ms bins}
#'  \item{timeBin}{time bin number (1-20)}
#'  \item{Object}{object type: phonologically related Competitor or Unrelated object. Target object data are omitted}
#'  \item{Type}{type of phonological competitor: Cohort (e.g., penny - pencil) or Rhyme (e.g., carrot - parrot)}
#'  \item{FixProp}{fixation proportion: proportion of correct-response trials on which the object was fixated}
#'  }
#' @source Mirman et al. (2011, Brain & Lang., 117:2, 53-68) <http://www.danmirman.org/pdfs/Mirman_etal_2011.pdf>
"CohortRhyme"

#' Fixation of semantically related objects during spoken word comprehension
#'
#' Fixation time course data from a spoken word-to-picture matching study with semantic distractors in the display.
#'
#' @format A data frame with 1530 rows and 5 variables:
#' \describe{
#'  \item{Subject}{subject ID, factor}
#'  \item{Time}{time (in ms) since word onset in 50ms bins}
#'  \item{meanFix}{fixation proportion (subject mean): proportion of correct-response trials on which the object was fixated}
#'  \item{Condition}{semantic relatedness condition: Function (e.g., broom - sponge) or Thematic (e.g., broom - dustpan)}
#'  \item{Object}{object type: Target, semantically related Competitor, or Unrelated}
#'  }
#' @source Kalenine et al. (2012, J. Exp. Psychol. Learn. Mem. Cog., 38:5, 1274-1295) <http://www.danmirman.org/pdfs/Kalenine_etal2012.pdf>
"FunctTheme"

#' Fixation of semantically related objects during spoken word comprehension by participants with left hemisphere stroke
#'
#' Fixation time course data for participants with left hemisphere stroke from a spoken word-to-picture matching study with semantic distractors in the display.
#'
#' @format A data frame with 8199 rows and 8 variables:
#' \describe{
#'  \item{subj}{subject ID, factor}
#'  \item{Condition}{semantic relatedness condition: Function (e.g., broom - sponge) or Thematic (e.g., broom - dustpan)}
#'  \item{Object}{object type: Target, semantically related Competitor, or Unrelated}
#'
#'  \item{Time}{time (in ms) relative to word onset in 50ms bins; negative numbers indicate preview period (visual display before word onset)}
#'  \item{meanFix}{fixation proportion (subject mean): proportion of correct-response trials on which the object was fixated}
#'  \item{sumFix}{fixation sum: number of correct-response trials on which the object was fixated (i.e., numerator for meanFix)}
#'  \item{N}{number of correct-response trials (i.e., denominator for meanFix)}
#'  \item{timeBin}{time bin number}
#'  }
#' @source Kalenine, Mirman, & Buxbaum (2012, Front. Hum. Neurosci., 6:106, 1-12) <http://journal.frontiersin.org/Journal/10.3389/fnhum.2012.00106>
"FunctThemePts"

#' Picture naming during aphasia treatment
#'
#' Picture naming data from individuals with aphasia undergoing a treatment trial. Each participant was tested on a 175-item picture naming test (Philadelphia Naming Test) multiple times over the course of the treatment and responses were coded.
#'
#' @format A data frame with 115 rows and 9 variables:
#' \describe{
#'  \item{SubjectID}{subject ID, factor}
#'  \item{Diagnosis}{participant's aphasia subtype: Anomic, Conduction, or Wernicke's aphasia}
#'  \item{TestTime}{test iteration, 0 is the baseline at the start of treatment}
#'  \item{Correct}{proportion correct naming responses}
#'  \item{Semantic.error}{proportion semantic errors (cat - "dog")}
#'  \item{Mixed.error}{proportion mixed errors (cat - "rat")}
#'  \item{Formal.error}{proportion formal errors (cat - "mat")}
#'  \item{Unrelated.error}{proportion unrelated errors (cat - "log")}
#'  \item{Nonword.error}{proportion nonword errors (cat - "lat")}
#' }
#' @source Moss Aphasia Psycholinguistics Database <mappd.org>
"NamingRecovery"

#' Target fixation data from a typical "visual world paradigm" experiment
#'
#' Time course of target object fixation during a spoken word-to-picture matching (i.e., "visual world paradigm").
#'
#' @format A data frame with 300 rows and 7 variables
#' \describe{
#'  \item{Subject}{subject ID, factor}
#'  \item{Time}{time (in ms) relative to word onset in 50ms bins, 300-1000}
#'  \item{timeBin}{rescaling of Time variable for easier model-fitting, 1-15}
#'  \item{Condition}{word frequency condition: High (e.g., torch, frog) or Low (e.g., class, horse)}
#'  \item{meanFix}{fixation proportion (subject mean): proportion of correct-response trials on which the object was fixated}
#'  \item{sumFix}{fixation sum: number of correct-response trials on which the object was fixated (i.e., numerator for meanFix)}
#'  \item{N}{number of correct-response trials (i.e., denominator for meanFix)}
#'  }
"TargetFix"

#' Effect of transitional probability on novel word learning
#'
#' Accuracy in 2-alternative forced-choice word learning experiment. On each trial, participants see two complex geometric shapes, hear a novel word (e.g., "pibu"), and have to guess which object corresponds to the word. They receive feedback and gradually learn which object goes with which word. There are 6 trials in each of the 10 training blocks. Transitional probability of the novel words (i.e., p("bu"|"pi")) was experimentally manipulated (between-subjects) during an exposure phase immediately preceding the word learning phase.
#'
#' @format A data frame with 560 rows and 4 variables
#' \describe{
#'  \item{Subject}{subject id, N=56}
#'  \item{TP}{transitional probability of words, manipulated between-subjects}
#'  \item{Block}{training block (1-10), each block consists of 6 trials}
#'  \item{Accuracy}{proportion correct responses during the block, chance = 0.5}
#' }
#' @source Partial data from Mirman et al. (2008, Cognition, 108:1, 271-280). <http://www.danmirman.org/pdfs/MirmanMagnusonGrafEstesDixon2008.pdf>
"WordLearnEx"

#' Judgments to cursive vs. type-print words in LDT
#'
#' Aggregate baseline corrected pupillary data for a LDT task that required participants to judge the lexical status of cursive and type-print stimuli.
#' @format A data frame with 4 variables
#' \describe{
#'  \item{subject}{subject ID (edf file name)}
#'  \item{script}{Cursive or Print}
#'  \item{timebins}{trial onset}
#'  \item{aggbaseline}{baseline corrected pupil size}
#'
#' }
"cursive_agg_data"














