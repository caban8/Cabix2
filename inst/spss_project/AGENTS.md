# AGENTS.md

## Purpose

This repository is an R project for reproducible empirical-study analysis and
report generation. Treat this file as a template: update the project-specific
names, paths, scales, and output formats when starting a similar study.

The main goals are:

1. preserve the raw data and the analyst's manual edits;
2. keep data processing and analyses reproducible through `targets`;
3. make narrow, reviewable changes;
4. verify the smallest useful part of the pipeline before running expensive or
   artifact-heavy steps.

Before editing or running anything read and inspect the following docs and scripts:

1. "docs/data-flow.md"
2. "docs/project-map.md"
3. "docs/source-of-truth.md"
4. "docs/verification-ladder.md"
5. "docs/working-conventions.md"
6. All scripts in the examples/ dir (which serve only as references, they are not part of the project's code)




## Costly Or External Actions

Ask before running steps that can create significant artifacts, consume paid
API tokens, or require external authentication. Examples:

- the full pipeline via `run_pipeline()` or `targets::tar_make()`;
- AI-assisted narration or evaluation targets;
- Google Drive or Google Sheets download helpers;
- report rendering that overwrites deliverables;
- regeneration of analyst-edited interpretation files.

Prefer a targeted `targets` run when it is sufficient. State which targets will
run and which outputs may change.
