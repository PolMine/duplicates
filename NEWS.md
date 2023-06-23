## v0.0.5

- Significant performance improvement for `nchars()`-method for corpus objects.

## v0.0.2

- `Duplicates$get_comparisons()` dropped. Was necessary when computing
similarities was much less parsimonious. Irrelevant due to the switch to
`proxyC::simil()`.