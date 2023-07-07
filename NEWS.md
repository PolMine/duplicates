## v0.1.3

- Function `minimize_vocabulary()` more generic and renamed as `charfilter()`.
- Method `nchars()` renamed to `charcount()`.
- Function `duplicates_get_groups()` renamed to `as_docgroups()`.
- Argument `s_attribute` of method `detect_duplicates()` used generically. A new
column with the name of the the s-attribute to be used as metadata will be
added.

## v0.1.2

- Bug removed of `nchars()`-method for `corpus` objects. Unit test added.

## v0.0.5

- Significant performance improvement for `nchars()`-method for corpus objects.

## v0.0.2

- `Duplicates$get_comparisons()` dropped. Was necessary when computing
similarities was much less parsimonious. Irrelevant due to the switch to
`proxyC::simil()`.