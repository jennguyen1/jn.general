# jn.general

## Installation
devtools::install_github("jnguyen92/jn.general")

## Functions include:
- **anova plot**: generates boxplots and summary tables into concise visualization for one-way ANOVA
- **duplicated data**: displays duplicated rows by specified columns
- **extract**: saves data generated from iterative, independent model builds and groups common data outputs for easy extraction
- **mult_merge**: merge multiple data frames using data.table merging
- **rdata**: generates a random data frame with options to specify a variety of column types
- **refine**: wrapper for Filter. Applies a function (a function that returns a boolean) and returns the values which evaluate to TRUE
- **reorder_cols**: extension of dplyr::select. Reorders columns based on selection syntax provided by dplyr::select and also appends unselected columns. 
- **shortcuts**: import libraries, wipe environment / blank slate, string concatenation shortcut
- **str_scan**: splits string by specified delimiter and obtains the ith result
- **to be or not to be**: returns the subset and anti-subset, supply own subsetting function or pre-existing functions (subset, filter, distinct, etc). Useful for splitting data sets and running analyses in parallel on both sets.
