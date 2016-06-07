# jn.general

## Installation
`devtools::install_github("jnguyen92/jn.general")`

## Functions include:

**Working with Lists**
- **rename_list**: adds an identifier column to (nested) data frames inside a list with the names of the list elements
- **extract_list**: saves data generated from iterative, independent model builds and groups common data outputs for easy extraction

**Working with Data Frames**
- **view_duplicated**: displays duplicated rows by specified columns
- **remove_duplicated**: removes duplicated rows by specified columns
- **merge_mult**: merge multiple data frames using data.table merging
- **to be or not to be**: returns the subset and anti-subset, supply own subsetting function or pre-existing functions (subset, filter, distinct, etc). Useful for splitting data sets and running analyses in parallel on both sets.
- **reorder_cols**: extension of `dplyr::select`. Reorders columns based on selection syntax provided by `dplyr::select` and also appends unselected columns. 

**Misc Data Wrangling**
- **shortcuts**: import libraries, wipe environment / blank slate, string concatenation shortcut
- **refine**: wrapper for Filter. Applies a function (a function that returns a boolean) and returns the values which evaluate to TRUE
- **str_scan**: splits string by specified delimiter and obtains the ith result
- **percentile_bin**: groups a vector by its percentiles

**Plots**
- **anova_plot**: generates boxplots and summary tables into concise visualization for one-way ANOVA
- **table_plot**: generates a plot object from a data frame
- **diagnostic_plots**: generates diagnostic plots for lm and glm objects in ggplot2

**Random Data Generation**
- **rdata**: generates a random data frame with options to specify a variety of column types
- **rdata_helpers**: helper functions to generate random data
