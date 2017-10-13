# jn.general

This package contains a variety of general functions for working with data

**Working with Lists**
- **rename_list**: adds an identifier column to (nested) data frames inside a list with the names of the list elements
- **extract_list**: saves data generated from iterative, independent model builds and groups common data outputs for easy extraction

**Working with Data Frames**
- **view_duplicated**: displays duplicated rows by specified columns
- **remove_duplicated**: removes duplicated rows by specified columns
- **merge_mult**: merge multiple data frames using data.table merging
- **to_be**: returns the subset and anti-subset, supply own subsetting function or pre-existing functions (subset, filter, distinct, etc). Useful for splitting data sets and running analyses in parallel on both sets.

**Plots**
- **anova_plot**: generates boxplots and summary tables into concise visualization for one-way ANOVA
- **table_plot**: generates a plot object from a data frame
- **diagnostic_plots**: generates diagnostic plots for lm and glm objects in ggplot2

**Random Data Generation**
- **rdata**: generates a random data frame with options to specify a variety of column types
- **rdata_helpers**: helper functions to generate random data

## Installation
`devtools::install_github("jnguyen92/jn.general")`
