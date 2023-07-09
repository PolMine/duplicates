Introducing the duplicates package
================

<!-- badges: start -->

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-green.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R-CMD-check](https://github.com/PolMine/duplicates/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PolMine/duplicates/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/PolMine/duplicates/branch/devel/graph/badge.svg)](https://app.codecov.io/gh/PolMine/duplicates?branch=devel)
<!-- badges: end -->

## About

The package implements a procedure described by Fritz Kliche, Andre
Blessing, Urlich Heid and Jonathan Sonntag in the paper “The eIdentity
Text ExplorationWorkbench” presented at LREC 2014 (see ). The main
function is `detect_duplicates()`.

## Related work

Near duplicate detection is a standard NLP task. There is a wide range
of algorithms that are used for near duplicate detection and there is a
broad set of implementations in the programming languages used for NLP
tasks.

In the R context, the
[textreuse](https://CRAN.R-project.org/package=textreuse) package is the
point of reference for duplicate detection. The use case for the
*duplicates* package is large corpora that have been indexed with the
Corpus Workbench (CWB). The hashing step which is a selling point for
the textreuse package is performed already, and requirements for
tokenizing and hashing the data are not replicated. The scenario for
using the duplicates package is large, CWB-indexed corpora.
