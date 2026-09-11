# fifa-surveillance-tools

Shared tools, templates and synthetic data for FIFA Player Health Surveillance
analyses. The aim is that every competition is analysed the same way, and that
anyone can see how.

## What this is

A code library, not an analysis. The `R/` folder holds the functions; each
competition is its own project, built from `templates/scripts/`, with a single
`config.R` holding everything specific to it.

```
load_tools.R          entry point - sets up the library, sources everything in R/
R/                    the functions, separated by purpose
data/                 OSIICS 16 reference tables and the figure palette
templates/            report templates and a blank competition project
templates/scripts/    config.R, run_all.R and the five analysis steps
examples/             a synthetic competition and a script that runs it
dev/                  superseded and one-off scripts, kept for reference
```

## Try it

Open the project and run:

```r
source("examples/make_example_data.R")   # generates a synthetic competition
source("examples/run_example.R")         # runs the whole pipeline on it
```

The first script writes six source files to `examples/data/`, generated from a
fixed seed. Diagnoses are sampled from the OSIICS reference in `data/`, and body
area, tissue and pathology are derived from that same lookup, so the labels are
consistent with the codes. No real people, teams or competitions are
represented.

The second builds a throwaway competition project, runs all six analysis steps,
writes a Word report, and checks the outputs. It is also the regression test: if
a change to the library breaks something, this is what catches it.

## Starting a new competition

Copy `templates/scripts/` into a new project folder and work down `config.R`
from the top. Settings marked `SET` must be filled in; settings marked `CHECK`
have defaults worth reviewing. Nothing outside `config.R` should contain a
path, a date, a filename or a threshold.

Start from the template rather than from a copy of the previous competition, so
that exclusions and recodings do not carry over by accident.

Run the steps in this order:

```
03_Analysis/Code/01_prepare_data.R
03_Analysis/Code/02_exposure.R
03_Analysis/Code/03_potential_injuries.R
run_all.R
03_Analysis/Code/04_figures.R
03_Analysis/Code/05_report.R
```

Each step writes a run notes file to `03_Analysis/Results/`, recording the
decisions and the figures behind them, so a competition's results can be
checked without re-running anything.

## What is not here

No competition data, and no keys. The synthetic example is the only data in
this repository.

Preparing a competition's source files and building its exposure denominator
are competition-specific. The source data arrive in a different shape every
time, and the pattern of missing exposure differs too. `02_exposure.R` is a
starting point with the decisions it requires documented in its header, and it
is expected to be edited for every competition.

## Reference data

`data/osiics_16_fifa_version.csv` is the working OSIICS 16 table used by the
analysis. The upstream source files are in `data/reference/`.

OSIICS 16 combines hip and groin into a single body area. The football-specific
extension of the IOC consensus statement (Waldén et al., 2023) recommends
reporting them separately, so codes can be reassigned per competition through
`config.R`. Aligning the reference table itself with that recommendation is
outstanding.

## Changes

See [CHANGELOG.md](CHANGELOG.md). The September 2026 entry lists five defects in
earlier versions of this code that changed results, and what to check if you
used it.

## Citation

Each release is archived on Zenodo with its own DOI, so a paper can cite the
exact version that produced its results. Cite the version you used.

> Clarsen, B. fifa-surveillance-tools. Zenodo. DOI to be added.

## Licence

MIT. See [LICENSE](LICENSE).

## Contact

Ben Clarsen, FIFA Global Player Health Surveillance Programme.