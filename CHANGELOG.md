# Changelog

This file records changes that affect results, not just code. Entries are
newest first.


## September 2026 — library restructure and defect fixes

The repository has been restructured from a flat folder of scripts into a
shared library plus per-competition project templates, and a number of defects
have been fixed. Several of those defects changed published numbers.

The previous public version remains reachable at commit `893bdc8`.


### If you used an earlier version of this code

Four of the fixes below change results. If you have run an analysis with the
earlier code, the following outputs may have moved:

- **Training exposure, and every rate that uses it.** The surveillance period
  began at each team's first match, so training recorded before the first match
  was discarded. Exposure was therefore too low and training and overall rates
  too high.

- **Match exposure, and match rates.** Match exposure was not restricted to
  consenting players, while the case list was. The match denominator was
  therefore too large and match rates too low.

- **Injury burden.** The burden calculation did not filter to injuries, so time
  loss from illnesses was counted as injury burden. Injury burden was too high.

- **Pattern tables in the report.** The pattern table functions wrote to a
  hard-coded filename, so every pattern table overwrote the last. The report
  displayed whichever table was written most recently under the label of
  another.

To check your own results, re-run the analysis with the current code and
compare total training hours, total match hours, the injury burden estimates,
and the pattern tables. Case counts and incidence denominators for illness are
unaffected.


### Fixed

- **Surveillance period.** Each team's period now runs from their first
  recorded training exposure, not from their first match. Teams arrive at
  different times and record from arrival.

- **Match exposure and consent.** Match exposure is now restricted to
  consenting players, matching the case list. The unrestricted file is retained
  for the potential injury analysis, which observes every player on the pitch
  from broadcast footage and so uses a different denominator from the same
  source.

- **Burden.** The burden calculation now filters to injuries.

- **Pattern tables.** Each pattern table is written to its own file, named by
  the analysis that produced it.

- **Age.** `calculate_age()` did not subtract a year for a birthday not yet
  reached, so ages were up to a year too high in the player characteristics
  table.

- **Consent.** Consent was tested with `==`, which returns `NA` for a missing
  value rather than `FALSE`, so players and teams with no recorded consent were
  counted as consenting. Tested with `%in%` throughout.

- **Cases outside training and matches.** Cases recorded as occurring neither
  in training nor in a match have no exposure denominator. They were included
  in rates. They are now excluded from all rates and summary tables, reported
  separately, and never dropped silently. A missing value means gradual onset
  and is retained.

- **Exacerbations.** Filtered with `%in%` rather than `!=`, so a case with no
  subsequent-injury category is kept rather than silently dropped.

- **Taxonomy join.** The case list and the OSIICS reference both carry
  `problem_type`. An unspecified join keyed on it as well as the code and
  produced missing taxonomy wherever the two disagreed. The join is now on the
  code alone.

- **Imputation of missing training exposure.** The previous code took a single
  imputed dataset and discarded the rest, making the result depend on an
  arbitrary draw. All draws are now used, and the range across draws is
  reported alongside the value used.

- **Team-day training value.** Taken as the median of the players who recorded
  something, rather than the mode, which returns an arbitrary first value when
  players differ.


### Changed

- The repository is now a shared library. `load_tools.R` is the entry point and
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
  reporting them separately, so codes can now be reassigned per competition
  through `config.R`. Aligning the OSIICS reference itself with the consensus
  statement remains outstanding.


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

- Medical attention outcomes. Reporting is time loss only.


### Known gaps

- `templates/report_surveillance_plainlanguage.Rmd` still reads
  `overall_results.csv`, which the current pipeline does not write. The
  plain-language report does not run and is awaiting rebuild.