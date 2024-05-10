## v0.1.6

- `docsimil()` for `list` objects robust when char filtering results in empty docs.

## v0.1.4

- Method `detect_duplicates()` renamed to `docsimil()`.
- Function `as_docgroups()` renamed to `docgroups()`.

## v0.1.3

- Function `minimize_vocabulary()` more generic and renamed as `charfilter()`.
- Method `nchars()` renamed to `charcount()`.
- Function `duplicates_get_groups()` renamed to `as_docgroups()`.
- Argument `s_attribute` of method `detect_duplicates()` used generically. A new
column with the name of the the s-attribute to be used as metadata will be
added.
- Dropped method `duplicates_encode()` - it is better to use
`cwbtools::s_attribute_encode()` without wrapper.

## v0.1.2

- Bug removed of `nchars()`-method for `corpus` objects. Unit test added.

## v0.0.5

- Significant performance improvement for `nchars()`-method for corpus objects.

## v0.0.2

- `Duplicates$get_comparisons()` dropped. Was necessary when computing
similarities was much less parsimonious. Irrelevant due to the switch to
`proxyC::simil()`.
