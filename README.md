# jn.general

This package contains a variety of general functions for working with data

**Working with Vectors**
- `make_outliers_na()` makes all values beyond specified sd from the mean NA (mark for removal)

**Working with Lists**
- `rename_list()` adds an identifier column to (nested) data frames inside a list with the names of the list elements
- `extract_list()` saves data generated from iterative, independent model builds and groups common data outputs for easy extraction

**Working with Data Frames**
- `view_duplicated()` displays duplicated rows by specified columns
- `remove_duplicated()` removes duplicated rows by specified columns
- `merge_mult()` merge multiple data frames
- `to_be()` returns the subset and anti-subset, supply own subsetting function or pre-existing functions (subset, filter, distinct, etc). Useful for splitting data sets and running analyses in parallel on both sets.

**Plots**
- `diagnostic_plots()` generates diagnostic plots for lm and glm objects in ggplot2

## Installation
`devtools::install_github("jennguyen1/jn.general")`
