qPCR Analysis
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0             ✔ purrr   0.9000.0.9000
    ## ✔ tibble  3.1.8             ✔ dplyr   1.0.10       
    ## ✔ tidyr   1.2.1.9001        ✔ stringr 1.5.0.9000   
    ## ✔ readr   2.1.3             ✔ forcats 0.5.2        
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(broom)
```

# Ingest data

The qPCR machine outputs its data in excel files. For this experiment,
we can find the files [in our lab google
drive](https://drive.google.com/drive/folders/1W7bcgLTdgLp2NX2_Yl3J17dTZ71Bi6Ib).
Eventually, we might want to automate access to the data, but for now,
we’ll manually download Plates 1, 2, and 3 excel files and save to the
`data/` directory.

## Read qPCR data from excel files

For each plate, we’ll read the “Results” sheet. The first 42 rows of the
sheet are metadata that we don’t need, so we’ll skip them. We’ll also
save the path in a column called “source” so we can look for plate
effects if we’d like after we merge files.

``` r
read_qpcr_excel <- function(path) {
  read_excel(
    path = path,
    sheet = "Results",
    skip = 42
  ) |>
    mutate(source = path)
}
```

``` r
data_dir <- "data/"
filename_pattern <- "Plate [0-9]+[.]xls"
data_raw <- list.files(
  data_dir,
  pattern = filename_pattern,
  full.names = TRUE
  ) |>
  map(read_qpcr_excel) |> 
  list_rbind()
show(data_raw)
```

    ## # A tibble: 198 × 34
    ##     Well Well Positi…¹ Omit  Sampl…² Targe…³ Task  Repor…⁴ Quenc…⁵ CT    Ct Me…⁶
    ##    <dbl> <chr>         <lgl> <chr>   <chr>   <chr> <chr>   <chr>   <chr>   <dbl>
    ##  1     1 A1            FALSE Blank   Blank   UNKN… FAM     NFQ-MGB Unde…    NA  
    ##  2     2 A2            FALSE Blank   Blank   UNKN… FAM     NFQ-MGB Unde…    NA  
    ##  3     3 A3            FALSE Blank   Blank   UNKN… FAM     NFQ-MGB Unde…    NA  
    ##  4    13 B1            FALSE B1_NTC  Barcod… UNKN… FAM     NFQ-MGB 35.9…    36.4
    ##  5    14 B2            FALSE B1_NTC  Barcod… UNKN… FAM     NFQ-MGB Unde…    36.4
    ##  6    15 B3            FALSE B1_NTC  Barcod… UNKN… FAM     NFQ-MGB 36.7…    36.4
    ##  7    25 C1            FALSE A0.1    Barcod… UNKN… FAM     NFQ-MGB 20.3…    20.2
    ##  8    26 C2            FALSE A0.2    Barcod… UNKN… FAM     NFQ-MGB 19.7…    19.9
    ##  9    27 C3            FALSE A0.3    Barcod… UNKN… FAM     NFQ-MGB 18.8…    19.0
    ## 10    28 C4            FALSE A1.1    Barcod… UNKN… FAM     NFQ-MGB 18.8…    18.9
    ## # … with 188 more rows, 24 more variables: `Ct SD` <dbl>, Quantity <lgl>,
    ## #   `Quantity Mean` <lgl>, `Quantity SD` <lgl>, `Y-Intercept` <lgl>,
    ## #   `R(superscript 2)` <lgl>, Slope <lgl>, Efficiency <lgl>,
    ## #   `Automatic Ct Threshold` <lgl>, `Ct Threshold` <dbl>,
    ## #   `Automatic Baseline` <lgl>, `Baseline Start` <dbl>, `Baseline End` <dbl>,
    ## #   `Amp Status` <chr>, Comments <lgl>, `Cq Conf` <dbl>, `Target Color` <chr>,
    ## #   CQCONF <chr>, EXPFAIL <chr>, HIGHSD <chr>, NOAMP <chr>, NOISE <chr>, …

## Tidy the data

We’re only going to keep a subset of columns. In the next block we
extract the columns we want and convert them to the correct datatypes.

- `CT`: the Ct value. Either “Undetermined” or a number. We’ll use
  `as.numeric` to convert from a character string to a number (double).
- `Sample Name` labels the content of the samples
- `Well Position` is the alphanumeric plate well ID (e.g., “A01”). Note:
  this is redundant with `Sample Name` here, but in the future we may
  want to automatically label the samples from `Well Position` using a
  plate layout file.
- `Ct Threshold`. These should all be `0.2`. We will check that for
  quality control.
- `Automatic Ct Threshold`. These should all be `FALSE`
- `Target Name` can be used to distinguish experimental samples
  (“Barcode_1”) from blanks (“Blank”) and NTCs (“Barcode_1\_NTC”). We’ll
  encode these as factors because they have a limited set of values.

We’ll also rename the columns consistently in “snake case” (i.e., all
lowercase with underscores separating the words.)

In the future we may want to redo the baseline subtraction manually, but
for now, we’ll ignore those columns.

``` r
data_extracted <- data_raw |>
  transmute(
    ct = as.numeric(CT),
    sample_name = `Sample Name`,
    well_position = `Well Position`,
    ct_threshold = `Ct Threshold`,
    auto_ct_threshold = `Automatic Ct Threshold`,
    target_name = as.factor(`Target Name`)
  )
```

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

``` r
show(data_extracted)
```

    ## # A tibble: 198 × 6
    ##       ct sample_name well_position ct_threshold auto_ct_threshold target_name  
    ##    <dbl> <chr>       <chr>                <dbl> <lgl>             <fct>        
    ##  1  NA   Blank       A1                     0.2 FALSE             Blank        
    ##  2  NA   Blank       A2                     0.2 FALSE             Blank        
    ##  3  NA   Blank       A3                     0.2 FALSE             Blank        
    ##  4  35.9 B1_NTC      B1                     0.2 FALSE             Barcode_1_NTC
    ##  5  NA   B1_NTC      B2                     0.2 FALSE             Barcode_1_NTC
    ##  6  36.8 B1_NTC      B3                     0.2 FALSE             Barcode_1_NTC
    ##  7  20.3 A0.1        C1                     0.2 FALSE             Barcode_1    
    ##  8  19.8 A0.2        C2                     0.2 FALSE             Barcode_1    
    ##  9  18.8 A0.3        C3                     0.2 FALSE             Barcode_1    
    ## 10  18.8 A1.1        C4                     0.2 FALSE             Barcode_1    
    ## # … with 188 more rows

TODO: It would be nice to specify what values to convert to `NA`
quietly.

For quality control, let’s check assertions our assertions about about
`ct_threshold` and `autothreshold`.

``` r
expected_ct_threshold <- 0.2
data_extracted |> 
  summarize(
    all_auto_ct_theshold_false = all(!auto_ct_threshold),
    all_ct_theshold_expected = all(ct_threshold == expected_ct_threshold)
  )
```

    ## # A tibble: 1 × 2
    ##   all_auto_ct_theshold_false all_ct_theshold_expected
    ##   <lgl>                      <lgl>                   
    ## 1 TRUE                       TRUE

Currently we have the `sample_name` column, which contains three pieces
of information: the experiment (e.g., “A”, “B”, …), the timepoint, and
the technical replicate number. To make the data tidier, we’ll split
this into separate columns.

We will use regular expressions to match the expected pattern, which is:

1.  The start of the string
2.  The treatment group, specified by one or more letters
3.  The timepoint, specified by one or more numbers
4.  A period
5.  The technical replicate, specified by one or more numbers
6.  The end of the string

In our input data, the non-template control was named “B1_NTC”, which
will break our naming scheme. We’ll rename it to “NTC” first.

TODO: Convert the treatment group letters to actual names

``` r
data_tidy <- data_extracted |> 
  mutate(
    sample_name = str_replace(sample_name, "B1_NTC", "NTC")
  ) |> 
  separate_wider_regex(
    sample_name,
    patterns = c(
      "^",
      treatment_group = "[A-Za-z]+",
      timepoint = "[0-9]+",
      "\\.",
      tech_rep = "[0-9]+",
      "$"
    ),
    too_few = "align_start"
  ) |> 
  mutate(
    treatment_group = as.factor(treatment_group),
    timepoint = as.integer(timepoint),
    tech_rep = as.factor(tech_rep)
  )
glimpse(data_tidy)
```

    ## Rows: 198
    ## Columns: 8
    ## $ ct                <dbl> NA, NA, NA, 35.92194, NA, 36.77825, 20.32065, 19.764…
    ## $ treatment_group   <fct> Blank, Blank, Blank, NTC, NTC, NTC, A, A, A, A, A, A…
    ## $ timepoint         <int> NA, NA, NA, NA, NA, NA, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3…
    ## $ tech_rep          <fct> NA, NA, NA, NA, NA, NA, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1…
    ## $ well_position     <chr> "A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"…
    ## $ ct_threshold      <dbl> 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.…
    ## $ auto_ct_threshold <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
    ## $ target_name       <fct> Blank, Blank, Blank, Barcode_1_NTC, Barcode_1_NTC, B…

Let’s make sure we have 9 blanks and NTCs (3 per plate) and 36 of the
other treatments (3 pcr replicates \* 3 technical replicates \* 4
timepoints).

``` r
data_tidy |>
  group_by(treatment_group) |>
  count()
```

    ## # A tibble: 7 × 2
    ## # Groups:   treatment_group [7]
    ##   treatment_group     n
    ##   <fct>           <int>
    ## 1 A                  36
    ## 2 B                  36
    ## 3 Blank               9
    ## 4 C                  36
    ## 5 D                  36
    ## 6 E                  36
    ## 7 NTC                 9

First, let’s take a look at the Blanks and NTCs. These should mostly
have `NA` for their ct value.

``` r
data_tidy |> 
  filter(treatment_group == "Blank" | treatment_group == "NTC")
```

    ## # A tibble: 18 × 8
    ##       ct treatment_group timepoint tech_rep well_posit…¹ ct_th…² auto_…³ targe…⁴
    ##    <dbl> <fct>               <int> <fct>    <chr>          <dbl> <lgl>   <fct>  
    ##  1 NA    Blank                  NA <NA>     A1               0.2 FALSE   Blank  
    ##  2 NA    Blank                  NA <NA>     A2               0.2 FALSE   Blank  
    ##  3 NA    Blank                  NA <NA>     A3               0.2 FALSE   Blank  
    ##  4 35.9  NTC                    NA <NA>     B1               0.2 FALSE   Barcod…
    ##  5 NA    NTC                    NA <NA>     B2               0.2 FALSE   Barcod…
    ##  6 36.8  NTC                    NA <NA>     B3               0.2 FALSE   Barcod…
    ##  7 33.5  Blank                  NA <NA>     A1               0.2 FALSE   Blank  
    ##  8 NA    Blank                  NA <NA>     A2               0.2 FALSE   Blank  
    ##  9  6.36 Blank                  NA <NA>     A3               0.2 FALSE   Blank  
    ## 10 NA    NTC                    NA <NA>     B1               0.2 FALSE   Barcod…
    ## 11 NA    NTC                    NA <NA>     B2               0.2 FALSE   Barcod…
    ## 12 NA    NTC                    NA <NA>     B3               0.2 FALSE   Barcod…
    ## 13 NA    Blank                  NA <NA>     A1               0.2 FALSE   Blank  
    ## 14 NA    Blank                  NA <NA>     A2               0.2 FALSE   Blank  
    ## 15 NA    Blank                  NA <NA>     A3               0.2 FALSE   Blank  
    ## 16 39.7  NTC                    NA <NA>     B1               0.2 FALSE   Barcod…
    ## 17 NA    NTC                    NA <NA>     B2               0.2 FALSE   Barcod…
    ## 18 37.3  NTC                    NA <NA>     B3               0.2 FALSE   Barcod…
    ## # … with abbreviated variable names ¹​well_position, ²​ct_threshold,
    ## #   ³​auto_ct_threshold, ⁴​target_name

We can make a boxplot to compare the distribution of ct across
treatments. To disply the NAs, we’ll set them to one more than the
maximum possible values. We see that the Blanks, NTCs, and treatment E
(which is some sort of negative control?) have CT \> 35, with a few
outliers. We’ll probably want to check those at some point.

``` r
ct_max <- max(data_tidy$ct, na.rm = TRUE) + 1
data_tidy |>
  mutate(ct = replace_na(ct, ct_max)) |>
  ggplot(mapping = aes(ct, treatment_group)) +
  geom_boxplot()
```

![](qpcr_analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

# Convert raw Ct values to concentrations

From Ari’s Excel sheet, we have the following steps:

1.  Use the regression coefficients from the standard to convert Ct
    values to “dilution” (this is a linear relationship $c_t = a d + b$)
2.  Convert dilution to concentration (in copies per microliter) using
    $\text{concentration} = C e^{\log(f_d) d}$, where $f_d$ is the
    fold-dilution at each dilution. In this case, $f_d = 10$.

The variable $d$ labels the level of dilution where 9 is a 10x dilution
of the original sample, 8 is 10x of dilution 9, etc. By implication, the
coefficient $C$ is the expected concentration at dilution 0, i.e. the
original concentration times $10^{10}$.

Note that the coefficients $a$, $b$, and $C$, are phagemid-specific. The
first two are estimated from the standard curve. $C$ is known from the
experimental protocol. In the future we would like estimate these
concentrations directly in this workflow.

We need to specify these coefficients:

``` r
std_curve_slope <- -3.6855
std_curve_intercept <- 43.498
concentration_at_d0 <- 0.9871
dilution_factor <- 10
```

We’ll apply the two equations in separate steps so we can compare
intermediate values with Ari’s spreadsheet. Eventually, we’ll condense
this into one conversion.

We’re also going to filter out the negative controls from now on.

``` r
treatments_to_keep <- c("A", "B", "C", "D")
data_concentration <- data_tidy |>
  filter(treatment_group %in% treatments_to_keep) |>
  mutate(
    dilution = (ct - std_curve_intercept) / std_curve_slope,
    log_concentration = log(concentration_at_d0) + log(dilution_factor) * dilution,
  )
glimpse(data_concentration)
```

    ## Rows: 144
    ## Columns: 10
    ## $ ct                <dbl> 20.32065, 19.76434, 18.81370, 18.81602, 19.44741, 19…
    ## $ treatment_group   <fct> A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A…
    ## $ timepoint         <int> 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 0, 0, 0, 1, 1, 1…
    ## $ tech_rep          <fct> 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3…
    ## $ well_position     <chr> "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9"…
    ## $ ct_threshold      <dbl> 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.…
    ## $ auto_ct_threshold <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
    ## $ target_name       <fct> Barcode_1, Barcode_1, Barcode_1, Barcode_1, Barcode_…
    ## $ dilution          <dbl> 6.288793, 6.439739, 6.697681, 6.697049, 6.525733, 6.…
    ## $ log_concentration <dbl> 14.46750, 14.81506, 15.40900, 15.40754, 15.01307, 14…

Since we have three PCR replicates per technical replicate, we can
summarize our data with min, median, and max without any loss of
information.

``` r
data_concentration |>
  group_by(treatment_group, timepoint, tech_rep) |>
  summarize(
    conc_min = min(log_concentration, na.rm = TRUE),
    conc_med = median(log_concentration, na.rm = TRUE),
    conc_max = max(log_concentration, na.rm = TRUE)
  )
```

    ## `summarise()` has grouped output by 'treatment_group', 'timepoint'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 48 × 6
    ## # Groups:   treatment_group, timepoint [16]
    ##    treatment_group timepoint tech_rep conc_min conc_med conc_max
    ##    <fct>               <int> <fct>       <dbl>    <dbl>    <dbl>
    ##  1 A                       0 1            14.5     14.5     14.6
    ##  2 A                       0 2            14.7     14.8     14.8
    ##  3 A                       0 3            15.0     15.4     15.4
    ##  4 A                       1 1            15.3     15.4     15.4
    ##  5 A                       1 2            15.0     15.1     15.2
    ##  6 A                       1 3            15.0     15.0     15.1
    ##  7 A                       2 1            15.2     15.2     15.3
    ##  8 A                       2 2            15.2     15.3     15.3
    ##  9 A                       2 3            14.9     14.9     14.9
    ## 10 A                       3 1            14.6     14.7     14.7
    ## # … with 38 more rows

# Plot log concentrations vs time for each condition

First, we plot all the treatment groups on the same timecourse to see
differences in absolute as well as relative concentration.

``` r
data_concentration |> 
  ggplot(
    aes(timepoint, log_concentration, shape = treatment_group, color = treatment_group)
    ) +
  geom_jitter(
    height = 0,
    width = 0.1
  )
```

![](qpcr_analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Let’s look at within- vs between-technical replicate variation:

``` r
data_concentration |>
  ggplot(aes(timepoint,log_concentration, group = tech_rep)) +
  stat_summary(
    fun.min = min,
    fun.max = max,
    fun = median,
    position = position_dodge(width = 0.2),
    size = 0.2
    ) +
  facet_wrap(
    facets = ~ treatment_group,
    )
```

![](qpcr_analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

The variation between treatments is swamping the variation between
replicates, so let’s let the y-axis vary:

``` r
data_concentration |>
  ggplot(aes(timepoint,log_concentration, group = tech_rep)) +
  stat_summary(
    fun.min = min,
    fun.max = max,
    fun = median,
    position = position_dodge(width = 0.2),
    size = 0.2
    ) +
  facet_wrap(
    facets = ~ treatment_group,
    scales = "free_y"
    )
```

![](qpcr_analysis_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

It looks like there is sometimes significantly more variation between
technical replicates than PCR replicates. This suggests that we may want
to use a hierarchical model of the error.

# Regression analysis

In this section, we’ll look at the trends in concentration over time.
First, we’ll make the approximation that all of the qPCR measurements
for a `(treatment_group, timepoint)` pair are independent. This is not
exactly true because the qPCR replicates of the same technical replicate
share the noise of the technical replicate. Thus, the error bars on
these estimates will be too optimistic. Next, we’ll make the opposite
approximation: that the mean of the qPCR replicates is a single
observation. In the future, we’ll look at a hierarchical model that
incorporates the dependency structure of the measurements.

## Treating all observations as independent

We can use a Loess curve to see the trend for each treatment:

``` r
data_concentration |>
  ggplot(aes(timepoint, log_concentration, color=treatment_group)) +
  geom_point() +
  geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](qpcr_analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

It’s not really appropriate here since our data is non-linear, but we
can also use a linear model:

``` r
data_concentration |>
  ggplot(aes(timepoint, log_concentration, color=treatment_group)) +
  geom_point() +
  geom_smooth(
    method = "lm"
  )
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](qpcr_analysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
models <- treatments_to_keep |> 
  map(~ filter(data_concentration, treatment_group == .)) |>
  map(~ lm(log_concentration ~ timepoint, .)) |>
  map(tidy) |>
  map2(
    treatments_to_keep,
    ~ mutate(.x, treatment_group=.y, collapsed=FALSE, .before=1)
    ) |> 
  list_rbind()
models
```

    ## # A tibble: 8 × 7
    ##   treatment_group collapsed term        estimate std.error statistic  p.value
    ##   <chr>           <lgl>     <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 A               FALSE     (Intercept) 15.0        0.0784   192.    3.68e-53
    ## 2 A               FALSE     timepoint   -0.00741    0.0419    -0.177 8.61e- 1
    ## 3 B               FALSE     (Intercept) 15.0        0.153     98.5   2.29e-43
    ## 4 B               FALSE     timepoint   -0.523      0.0816    -6.41  2.56e- 7
    ## 5 C               FALSE     (Intercept) 15.8        0.0343   461.    4.08e-66
    ## 6 C               FALSE     timepoint   -0.0776     0.0183    -4.23  1.66e- 4
    ## 7 D               FALSE     (Intercept)  9.98       0.286     34.8   3.44e-28
    ## 8 D               FALSE     timepoint   -1.08       0.153     -7.02  4.18e- 8

## Collapsing qPCR replicates

Now, we create a new table that collapses the qPCR replicates:

``` r
data_collapsed <- data_concentration |> 
  group_by(treatment_group, timepoint) |>
  summarise(log_concentration = mean(log_concentration), .groups="drop")
```

The Loess smoother is uninteresting because it will just draw a curve
through the points with no error.

On the other hand, we now have much wider error bars on our linear
estimates. Just as the independent approximation meant that the errors
were too optimistic, this approximation is too conservative.

``` r
data_collapsed |>
  ggplot(aes(timepoint, log_concentration, color=treatment_group)) +
  geom_point() +
  geom_smooth(
    method = "lm"
  )
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](qpcr_analysis_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

We can fit linear models separately for each timepoint and examine the
coefficients and standard errors:

``` r
models_collapsed <- treatments_to_keep |> 
  map(~ filter(data_collapsed, treatment_group == .)) |>
  map(~ lm(log_concentration ~ timepoint, .)) |>
  map(tidy) |>
  map2(
    treatments_to_keep,
    ~ mutate(.x, treatment_group=.y, collapsed=TRUE, .before=1)
    ) |> 
  list_rbind() 
models_collapsed
```

    ## # A tibble: 8 × 7
    ##   treatment_group collapsed term        estimate std.error statistic   p.value
    ##   <chr>           <lgl>     <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 A               TRUE      (Intercept) 15.0        0.186    80.7    0.000153 
    ## 2 A               TRUE      timepoint   -0.00741    0.0994   -0.0746 0.947    
    ## 3 B               TRUE      (Intercept) 15.0        0.473    31.8    0.000985 
    ## 4 B               TRUE      timepoint   -0.523      0.253    -2.07   0.174    
    ## 5 C               TRUE      (Intercept) 15.8        0.0995  159.     0.0000396
    ## 6 C               TRUE      timepoint   -0.0776     0.0532   -1.46   0.282    
    ## 7 D               TRUE      (Intercept)  9.98       1.10      9.08   0.0119   
    ## 8 D               TRUE      timepoint   -1.08       0.587    -1.83   0.209

Collapsing the qPCR replicates increases the standard error of the
regression coefficients:

``` r
rbind(models, models_collapsed) |> 
  filter(term == "timepoint") |> 
  ggplot(aes(collapsed, std.error, group=treatment_group)) +
  geom_line(aes(linetype = treatment_group)) +
  geom_point()
```

![](qpcr_analysis_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

## Hierarchical model \[TODO\]
