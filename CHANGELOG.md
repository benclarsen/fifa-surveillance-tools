# Changelog

This file records changes that affect results, not just code. Entries are
newest first.


## September 2026 — library restructure and defect fixes

The repository has been restructured from a flat folder of scripts into a
shared library plus per-competition project templates, and five defects that
changed results have been fixed.

The previous version remains reachable at commit `893bdc8`.


### If you used an earlier version of this code

Five defects in the published code changed results. If you have run an analysis
with it, the following outputs moved.

- **"All injuries" incidence and burden were too high.** The all-injuries rows
  were calculated from the whole case list without filtering to
  `problem_type == "Injury"`, so illnesses were counted as injuries. The match
  and training rows did filter on `when_occurred`, so the subgroups never
  summed to the total. Illness rows were calculated separately and are
  unaffected.

- **"All injuries" also included cases recorded as occurring outside training
  and matches.** Those cases have no exposure denominator. They sat in the
  all-injuries numerator against total exposure, inflating it further.

- **Player ages were up to a year too high.** `calculate_age()` subtracted a
  year only when the whole comparison date fell before the whole date of birth,
  which is never true for a living player, so the adjustment for a birthday not
  yet reached never happened. This affects the player characteristics table
  only, not any rate.

- **Participation counts were overstated where consent was missing.** Consent
  was tested with `==`, which returns `NA` rather than `FALSE` for a missing
  value. The `NA` survived into `n_distinct()` and was counted as an additional
  team and an additional player.

- **Cases with no subsequent-injury category were silently dropped.**
  Exacerbations were excluded with `subsequent_cat != "exacerbation"`, and
  `NA != "exacerbation"` is `NA`, which `filter()` discards. Case counts were
  therefore too low wherever that field was incomplete.

To check your own results, re-run with the current code and compare the
all-injuries incidence and burden rows, the total case count, the participation
table and the player characteristics table.


### What this repository does not cover

Preparing a competition's source data and building its exposure denominator
have always been competition-specific work, done outside this repository. No
preprocessing or exposure script was ever published here, so nothing in this
changelog speaks to how any particular competition's exposure was calculated.
`templates/scripts/` now includes those steps as a starting point, with the
decisions they require documented in the script headers, but they are expected
to be edited for every competition.


### Fixed

- The all-injuries rows filter to injuries, and cases recorded as occurring
  neither in training nor in a match are excluded from all rates and summary
  tables, reported separately, and never dropped silently. A missing value for
  when the case occurred means gradual onset and is retained.

- `calculate_age()` compares month and day, so a birthday not yet reached
  subtracts a year.

- Consent and exacerbation are tested with `%in%` rather than `==` and `!=`, so
  a missing value is treated as not matching rather than propagating or
  discarding the row.

- The case list and the OSIICS reference both carry `problem_type`. The join is
  now specified on the code alone; an unspecified join keyed on both and
  produced missing taxonomy wherever the two disagreed.


### Changed

- The repository is a shared library. `load_tools.R` is the entry point and
  sources everything in `R/`. Analysis functions, table functions, figures,
  de-identification, OSIICS handling and run notes are separated by purpose.

- Taxonomy orderings are defined once, in `R/osiics.R`, rather than repeated in
  each script.

- Every competition-specific value — paths, filenames, dates, thresholds,
  excluded teams, body area recoding — lives in one `config.R` per competition.
  No other file contains a hard-coded path, date or threshold.

- Disclosure control is explicit and configurable. Counts and count-derived
  rates are shown at any number of cases. Figures derived from individual
  players' time loss are suppressed below configurable thresholds, because
  exposure is published alongside them and the underlying days would otherwise
  be recoverable. The footnote in the report is generated from the same
  settings as the suppression, so the two cannot diverge.

- De-identification uses a per-competition random salt rather than a fixed one.
  Missing identifiers are left missing and collisions raise an error.

- Every analysis step writes a run notes file recording the decisions and
  figures behind it, so a competition's results can be audited without
  re-running the code.

- OSIICS 16 combines hip and groin into one body area. The football-specific
  extension of the IOC consensus statement (Waldén et al., 2023) recommends
  reporting them separately, so codes can be reassigned per competition through
  `config.R`. Aligning the OSIICS reference itself with the consensus statement
  recommendations remains outstanding.

- Missing training exposure is imputed across all draws rather than one, with
  the range across draws reported alongside the value used.


### Added

- `examples/make_example_data.R` generates a complete synthetic competition
  from a fixed seed. Diagnoses are sampled from the OSIICS reference that ships
  with the tools, and body area, tissue and pathology are derived from that
  lookup, so labels are consistent with codes by construction. No real people,
  teams or competitions are represented.

- `examples/run_example.R` builds a throwaway competition project from the
  templates, runs the pipeline end to end on the synthetic data, and checks the
  outputs. It doubles as a regression test.

- `templates/scripts/` holds a blank competition project: `config.R`,
  `run_all.R` and the five numbered analysis scripts. A new competition starts
  from these rather than from a copy of the previous competition, so exclusions
  and recodings do not carry over by accident.


### Removed

- The previous example CSVs, which matched an input contract the pipeline no
  longer reads.

- The Sankey diagram, which is no longer produced. The script is retained under
  `dev/`.

- Medical attention outcomes. Reporting is time loss only, in line with the
  reporting threshold now used across the programme.


### Known gaps

- `templates/report_surveillance_plainlanguage.Rmd` still reads
  `overall_results.csv`, which the current pipeline does not write. The
  plain-language report does not run and is awaiting rebuild.

- `README.md` still describes the previous folder layout.