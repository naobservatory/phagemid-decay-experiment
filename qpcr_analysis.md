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
library(knitr)
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
kable(head(data_raw, n = 10))
```

| Well | Well Position | Omit  | Sample Name | Target Name    | Task    | Reporter | Quencher | CT                 |  Ct Mean |     Ct SD | Quantity | Quantity Mean | Quantity SD | Y-Intercept | R(superscript 2) | Slope | Efficiency | Automatic Ct Threshold | Ct Threshold | Automatic Baseline | Baseline Start | Baseline End | Amp Status   | Comments |   Cq Conf | Target Color   | CQCONF | EXPFAIL | HIGHSD | NOAMP | NOISE | source            | BADROX |
|-----:|:--------------|:------|:------------|:---------------|:--------|:---------|:---------|:-------------------|---------:|----------:|:---------|:--------------|:------------|:------------|:-----------------|:------|:-----------|:-----------------------|-------------:|:-------------------|---------------:|-------------:|:-------------|:---------|----------:|:---------------|:-------|:--------|:-------|:------|:------|:------------------|:-------|
|    1 | A1            | FALSE | Blank       | Blank          | UNKNOWN | FAM      | NFQ-MGB  | Undetermined       |       NA |        NA | NA       | NA            | NA          | NA          | NA               | NA    | NA         | FALSE                  |          0.2 | TRUE               |              3 |           15 | No Amp       | NA       | 0.0000000 | RGB(0,139,69)  | Y      | N       | N      | N     | N     | data//Plate 1.xls | NA     |
|    2 | A2            | FALSE | Blank       | Blank          | UNKNOWN | FAM      | NFQ-MGB  | Undetermined       |       NA |        NA | NA       | NA            | NA          | NA          | NA               | NA    | NA         | FALSE                  |          0.2 | TRUE               |              3 |           22 | No Amp       | NA       | 0.0000000 | RGB(0,139,69)  | Y      | N       | N      | N     | Y     | data//Plate 1.xls | NA     |
|    3 | A3            | FALSE | Blank       | Blank          | UNKNOWN | FAM      | NFQ-MGB  | Undetermined       |       NA |        NA | NA       | NA            | NA          | NA          | NA               | NA    | NA         | FALSE                  |          0.2 | TRUE               |              3 |           39 | Inconclusive | NA       | 0.0000000 | RGB(0,139,69)  | Y      | Y       | N      | N     | Y     | data//Plate 1.xls | NA     |
|   13 | B1            | FALSE | B1_NTC      | Barcode_1\_NTC | UNKNOWN | FAM      | NFQ-MGB  | 35.921939849853516 | 36.35009 | 0.6055012 | NA       | NA            | NA          | NA          | NA               | NA    | NA         | FALSE                  |          0.2 | TRUE               |              3 |           30 | Inconclusive | NA       | 0.9320759 | RGB(0,139,69)  | N      | N       | Y      | N     | N     | data//Plate 1.xls | NA     |
|   14 | B2            | FALSE | B1_NTC      | Barcode_1\_NTC | UNKNOWN | FAM      | NFQ-MGB  | Undetermined       | 36.35009 | 0.6055012 | NA       | NA            | NA          | NA          | NA               | NA    | NA         | FALSE                  |          0.2 | TRUE               |              3 |           39 | No Amp       | NA       | 0.0000000 | RGB(0,139,69)  | Y      | Y       | N      | Y     | N     | data//Plate 1.xls | NA     |
|   15 | B3            | FALSE | B1_NTC      | Barcode_1\_NTC | UNKNOWN | FAM      | NFQ-MGB  | 36.778247833251953 | 36.35009 | 0.6055012 | NA       | NA            | NA          | NA          | NA               | NA    | NA         | FALSE                  |          0.2 | TRUE               |              3 |           31 | Inconclusive | NA       | 0.9147403 | RGB(0,139,69)  | N      | N       | Y      | N     | N     | data//Plate 1.xls | NA     |
|   25 | C1            | FALSE | A0.1        | Barcode_1      | UNKNOWN | FAM      | NFQ-MGB  | 20.320652008056641 | 20.23325 | 0.0774648 | NA       | NA            | NA          | NA          | NA               | NA    | NA         | FALSE                  |          0.2 | TRUE               |              3 |           16 | Amp          | NA       | 0.9699352 | RGB(176,23,31) | N      | N       | N      | N     | N     | data//Plate 1.xls | NA     |
|   26 | C2            | FALSE | A0.2        | Barcode_1      | UNKNOWN | FAM      | NFQ-MGB  | 19.764341354370117 | 19.85664 | 0.0884714 | NA       | NA            | NA          | NA          | NA               | NA    | NA         | FALSE                  |          0.2 | TRUE               |              3 |           14 | Amp          | NA       | 0.9626578 | RGB(176,23,31) | N      | N       | N      | N     | N     | data//Plate 1.xls | NA     |
|   27 | C3            | FALSE | A0.3        | Barcode_1      | UNKNOWN | FAM      | NFQ-MGB  | 18.813695907592773 | 19.04667 | 0.3385253 | NA       | NA            | NA          | NA          | NA               | NA    | NA         | FALSE                  |          0.2 | TRUE               |              3 |           13 | Amp          | NA       | 0.9706940 | RGB(176,23,31) | N      | N       | N      | N     | N     | data//Plate 1.xls | NA     |
|   28 | C4            | FALSE | A1.1        | Barcode_1      | UNKNOWN | FAM      | NFQ-MGB  | 18.816024780273438 | 18.87321 | 0.0788805 | NA       | NA            | NA          | NA          | NA               | NA    | NA         | FALSE                  |          0.2 | TRUE               |              3 |           13 | Amp          | NA       | 0.9645014 | RGB(176,23,31) | N      | N       | N      | N     | N     | data//Plate 1.xls | NA     |

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
kable(head(data_extracted, n = 10))
```

|       ct | sample_name | well_position | ct_threshold | auto_ct_threshold | target_name    |
|---------:|:------------|:--------------|-------------:|:------------------|:---------------|
|       NA | Blank       | A1            |          0.2 | FALSE             | Blank          |
|       NA | Blank       | A2            |          0.2 | FALSE             | Blank          |
|       NA | Blank       | A3            |          0.2 | FALSE             | Blank          |
| 35.92194 | B1_NTC      | B1            |          0.2 | FALSE             | Barcode_1\_NTC |
|       NA | B1_NTC      | B2            |          0.2 | FALSE             | Barcode_1\_NTC |
| 36.77825 | B1_NTC      | B3            |          0.2 | FALSE             | Barcode_1\_NTC |
| 20.32065 | A0.1        | C1            |          0.2 | FALSE             | Barcode_1      |
| 19.76434 | A0.2        | C2            |          0.2 | FALSE             | Barcode_1      |
| 18.81370 | A0.3        | C3            |          0.2 | FALSE             | Barcode_1      |
| 18.81602 | A1.1        | C4            |          0.2 | FALSE             | Barcode_1      |

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

Finally, we’ll rename the single-letter treatment groups with more
informative names and mark the negative controls so we can separate them
out later.

``` r
coding = list(
  "A" = "WW + phagemid",
  "B" = "WW + phagemid + detergent",
  "C" = "TBS + phagemid",
  "D" = "WW + phagemid + bleach",
  "E" = "Just WW"
)
controls <-  c("Blank", "NTC", "Just WW")

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
    treatment_group = recode(treatment_group, !!!coding),
    control = treatment_group %in% controls,
    timepoint = as.integer(timepoint),
    tech_rep = as.factor(tech_rep)
  )
kable(head(data_tidy, n = 10))
```

|       ct | treatment_group | timepoint | tech_rep | well_position | ct_threshold | auto_ct_threshold | target_name    | control |
|---------:|:----------------|----------:|:---------|:--------------|-------------:|:------------------|:---------------|:--------|
|       NA | Blank           |        NA | NA       | A1            |          0.2 | FALSE             | Blank          | TRUE    |
|       NA | Blank           |        NA | NA       | A2            |          0.2 | FALSE             | Blank          | TRUE    |
|       NA | Blank           |        NA | NA       | A3            |          0.2 | FALSE             | Blank          | TRUE    |
| 35.92194 | NTC             |        NA | NA       | B1            |          0.2 | FALSE             | Barcode_1\_NTC | TRUE    |
|       NA | NTC             |        NA | NA       | B2            |          0.2 | FALSE             | Barcode_1\_NTC | TRUE    |
| 36.77825 | NTC             |        NA | NA       | B3            |          0.2 | FALSE             | Barcode_1\_NTC | TRUE    |
| 20.32065 | WW + phagemid   |         0 | 1        | C1            |          0.2 | FALSE             | Barcode_1      | FALSE   |
| 19.76434 | WW + phagemid   |         0 | 2        | C2            |          0.2 | FALSE             | Barcode_1      | FALSE   |
| 18.81370 | WW + phagemid   |         0 | 3        | C3            |          0.2 | FALSE             | Barcode_1      | FALSE   |
| 18.81602 | WW + phagemid   |         1 | 1        | C4            |          0.2 | FALSE             | Barcode_1      | FALSE   |

Let’s make sure we have 9 blanks and NTCs (3 per plate) and 36 of the
other treatments (3 pcr replicates \* 3 technical replicates \* 4
timepoints).

``` r
data_tidy |>
  group_by(treatment_group) |>
  count() |> 
  kable()
```

| treatment_group           |   n |
|:--------------------------|----:|
| Blank                     |   9 |
| Just WW                   |  36 |
| NTC                       |   9 |
| TBS + phagemid            |  36 |
| WW + phagemid             |  36 |
| WW + phagemid + bleach    |  36 |
| WW + phagemid + detergent |  36 |

First, let’s take a look at the Blanks and NTCs. These should mostly
have `NA` for their ct value.

``` r
data_tidy |> 
  filter(treatment_group == "Blank" | treatment_group == "NTC") |> 
  kable()
```

|        ct | treatment_group | timepoint | tech_rep | well_position | ct_threshold | auto_ct_threshold | target_name    | control |
|----------:|:----------------|----------:|:---------|:--------------|-------------:|:------------------|:---------------|:--------|
|        NA | Blank           |        NA | NA       | A1            |          0.2 | FALSE             | Blank          | TRUE    |
|        NA | Blank           |        NA | NA       | A2            |          0.2 | FALSE             | Blank          | TRUE    |
|        NA | Blank           |        NA | NA       | A3            |          0.2 | FALSE             | Blank          | TRUE    |
| 35.921940 | NTC             |        NA | NA       | B1            |          0.2 | FALSE             | Barcode_1\_NTC | TRUE    |
|        NA | NTC             |        NA | NA       | B2            |          0.2 | FALSE             | Barcode_1\_NTC | TRUE    |
| 36.778248 | NTC             |        NA | NA       | B3            |          0.2 | FALSE             | Barcode_1\_NTC | TRUE    |
| 33.499622 | Blank           |        NA | NA       | A1            |          0.2 | FALSE             | Blank          | TRUE    |
|        NA | Blank           |        NA | NA       | A2            |          0.2 | FALSE             | Blank          | TRUE    |
|  6.357395 | Blank           |        NA | NA       | A3            |          0.2 | FALSE             | Blank          | TRUE    |
|        NA | NTC             |        NA | NA       | B1            |          0.2 | FALSE             | Barcode_1\_NTC | TRUE    |
|        NA | NTC             |        NA | NA       | B2            |          0.2 | FALSE             | Barcode_1\_NTC | TRUE    |
|        NA | NTC             |        NA | NA       | B3            |          0.2 | FALSE             | Barcode_1\_NTC | TRUE    |
|        NA | Blank           |        NA | NA       | A1            |          0.2 | FALSE             | Blank          | TRUE    |
|        NA | Blank           |        NA | NA       | A2            |          0.2 | FALSE             | Blank          | TRUE    |
|        NA | Blank           |        NA | NA       | A3            |          0.2 | FALSE             | Blank          | TRUE    |
| 39.660065 | NTC             |        NA | NA       | B1            |          0.2 | FALSE             | Barcode_1\_NTC | TRUE    |
|        NA | NTC             |        NA | NA       | B2            |          0.2 | FALSE             | Barcode_1\_NTC | TRUE    |
| 37.288032 | NTC             |        NA | NA       | B3            |          0.2 | FALSE             | Barcode_1\_NTC | TRUE    |

We can make a boxplot to compare the distribution of ct across
treatments. To disply the NAs, we’ll set them to one more than the
maximum possible values. We see that the Blanks, NTCs, and Just WW have
CT \> 35, with a few outliers. We’ll probably want to check those at
some point.

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
data_concentration <- data_tidy |>
  filter(!control) |>
  mutate(
    dilution = (ct - std_curve_intercept) / std_curve_slope,
    log10_concentration = log10(concentration_at_d0) + log10(dilution_factor) * dilution,
  )
kable(head(data_concentration, n = 10))
```

|       ct | treatment_group | timepoint | tech_rep | well_position | ct_threshold | auto_ct_threshold | target_name | control | dilution | log10_concentration |
|---------:|:----------------|----------:|:---------|:--------------|-------------:|:------------------|:------------|:--------|---------:|--------------------:|
| 20.32065 | WW + phagemid   |         0 | 1        | C1            |          0.2 | FALSE             | Barcode_1   | FALSE   | 6.288793 |            6.283155 |
| 19.76434 | WW + phagemid   |         0 | 2        | C2            |          0.2 | FALSE             | Barcode_1   | FALSE   | 6.439739 |            6.434100 |
| 18.81370 | WW + phagemid   |         0 | 3        | C3            |          0.2 | FALSE             | Barcode_1   | FALSE   | 6.697681 |            6.692042 |
| 18.81602 | WW + phagemid   |         1 | 1        | C4            |          0.2 | FALSE             | Barcode_1   | FALSE   | 6.697049 |            6.691410 |
| 19.44741 | WW + phagemid   |         1 | 2        | C5            |          0.2 | FALSE             | Barcode_1   | FALSE   | 6.525733 |            6.520094 |
| 19.54228 | WW + phagemid   |         1 | 3        | C6            |          0.2 | FALSE             | Barcode_1   | FALSE   | 6.499993 |            6.494354 |
| 18.98299 | WW + phagemid   |         2 | 1        | C7            |          0.2 | FALSE             | Barcode_1   | FALSE   | 6.651747 |            6.646108 |
| 18.98078 | WW + phagemid   |         2 | 2        | C8            |          0.2 | FALSE             | Barcode_1   | FALSE   | 6.652345 |            6.646706 |
| 19.66541 | WW + phagemid   |         2 | 3        | C9            |          0.2 | FALSE             | Barcode_1   | FALSE   | 6.466582 |            6.460943 |
| 20.02242 | WW + phagemid   |         3 | 1        | C10           |          0.2 | FALSE             | Barcode_1   | FALSE   | 6.369714 |            6.364075 |

Since we have three PCR replicates per technical replicate, we can
summarize our data with min, median, and max without any loss of
information.

``` r
data_concentration |>
  group_by(treatment_group, timepoint, tech_rep) |>
  summarize(
    conc_min = min(log10_concentration, na.rm = TRUE),
    conc_med = median(log10_concentration, na.rm = TRUE),
    conc_max = max(log10_concentration, na.rm = TRUE),
    .groups="drop"
  ) |> 
  kable()
```

| treatment_group           | timepoint | tech_rep | conc_min | conc_med | conc_max |
|:--------------------------|----------:|:---------|---------:|---------:|---------:|
| TBS + phagemid            |         0 | 1        | 6.768568 | 6.831878 | 6.850705 |
| TBS + phagemid            |         0 | 2        | 6.792533 | 6.837952 | 6.873783 |
| TBS + phagemid            |         0 | 3        | 6.685031 | 6.888131 | 6.901394 |
| TBS + phagemid            |         1 | 1        | 6.847352 | 6.850737 | 6.868744 |
| TBS + phagemid            |         1 | 2        | 6.861691 | 6.877239 | 6.901834 |
| TBS + phagemid            |         1 | 3        | 6.847496 | 6.853513 | 6.869517 |
| TBS + phagemid            |         2 | 1        | 6.788213 | 6.803730 | 6.811093 |
| TBS + phagemid            |         2 | 2        | 6.839702 | 6.845553 | 6.872789 |
| TBS + phagemid            |         2 | 3        | 6.838104 | 6.838262 | 6.846207 |
| TBS + phagemid            |         3 | 1        | 6.719192 | 6.735834 | 6.743106 |
| TBS + phagemid            |         3 | 2        | 6.727269 | 6.733463 | 6.760825 |
| TBS + phagemid            |         3 | 3        | 6.682930 | 6.705331 | 6.709381 |
| WW + phagemid             |         0 | 1        | 6.283155 | 6.314260 | 6.323196 |
| WW + phagemid             |         0 | 2        | 6.386246 | 6.406821 | 6.434100 |
| WW + phagemid             |         0 | 3        | 6.523466 | 6.670980 | 6.692042 |
| WW + phagemid             |         1 | 1        | 6.651478 | 6.684796 | 6.691410 |
| WW + phagemid             |         1 | 2        | 6.520094 | 6.568016 | 6.598399 |
| WW + phagemid             |         1 | 3        | 6.494354 | 6.505343 | 6.557952 |
| WW + phagemid             |         2 | 1        | 6.611019 | 6.615790 | 6.646108 |
| WW + phagemid             |         2 | 2        | 6.594325 | 6.646706 | 6.657891 |
| WW + phagemid             |         2 | 3        | 6.459444 | 6.460943 | 6.491859 |
| WW + phagemid             |         3 | 1        | 6.354533 | 6.362967 | 6.364075 |
| WW + phagemid             |         3 | 2        | 6.402617 | 6.406639 | 6.427699 |
| WW + phagemid             |         3 | 3        | 6.533085 | 6.557352 | 6.557984 |
| WW + phagemid + bleach    |         0 | 1        | 4.787610 | 4.799978 | 4.822240 |
| WW + phagemid + bleach    |         0 | 2        | 4.699030 | 4.711744 | 4.803847 |
| WW + phagemid + bleach    |         0 | 3        | 4.641492 | 4.839197 | 4.869774 |
| WW + phagemid + bleach    |         1 | 1        | 3.120579 | 3.140393 | 3.146548 |
| WW + phagemid + bleach    |         1 | 2        | 3.147717 | 3.160030 | 3.182765 |
| WW + phagemid + bleach    |         1 | 3        | 3.422252 | 3.494258 | 3.521124 |
| WW + phagemid + bleach    |         2 | 1        | 3.097185 | 3.135286 | 3.164708 |
| WW + phagemid + bleach    |         2 | 2        | 3.374681 | 3.393167 | 3.401482 |
| WW + phagemid + bleach    |         2 | 3        | 3.322434 | 3.332246 | 3.405861 |
| WW + phagemid + bleach    |         3 | 1        | 3.213033 | 3.281060 | 3.388114 |
| WW + phagemid + bleach    |         3 | 2        | 2.670230 | 3.001952 | 3.080523 |
| WW + phagemid + bleach    |         3 | 3        | 3.373299 | 3.426159 | 3.436528 |
| WW + phagemid + detergent |         0 | 1        | 6.380314 | 6.387247 | 6.394428 |
| WW + phagemid + detergent |         0 | 2        | 6.487272 | 6.506201 | 6.508716 |
| WW + phagemid + detergent |         0 | 3        | 6.353761 | 6.415534 | 6.464407 |
| WW + phagemid + detergent |         1 | 1        | 6.280190 | 6.300694 | 6.310621 |
| WW + phagemid + detergent |         1 | 2        | 6.267391 | 6.308052 | 6.327616 |
| WW + phagemid + detergent |         1 | 3        | 6.243073 | 6.404747 | 6.442623 |
| WW + phagemid + detergent |         2 | 1        | 6.273753 | 6.300157 | 6.303576 |
| WW + phagemid + detergent |         2 | 2        | 6.253449 | 6.314859 | 6.339616 |
| WW + phagemid + detergent |         2 | 3        | 6.440390 | 6.464820 | 6.490194 |
| WW + phagemid + detergent |         3 | 1        | 5.816892 | 5.828643 | 5.853913 |
| WW + phagemid + detergent |         3 | 2        | 5.866608 | 5.878782 | 5.933759 |
| WW + phagemid + detergent |         3 | 3        | 5.231195 | 5.287707 | 5.290168 |

# Plot log concentrations vs time for each condition

First, we plot all the treatment groups on the same timecourse to see
differences in absolute as well as relative concentration.

``` r
data_concentration |> 
  ggplot(
    aes(timepoint, log10_concentration, shape = treatment_group, color = treatment_group)
    ) +
  geom_point(
    position = position_jitter(height = 0, width = 0.1, seed = 3579237)
  )
```

![](qpcr_analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Let’s look at within- vs between-technical replicate variation:

``` r
data_concentration |>
  ggplot(aes(timepoint,log10_concentration, group = tech_rep)) +
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
  ggplot(aes(timepoint,log10_concentration, group = tech_rep)) +
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
  ggplot(aes(timepoint, log10_concentration, color=treatment_group)) +
  geom_point() +
  geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](qpcr_analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

It’s not really appropriate here since our data is non-linear, but we
can also use a linear model:

``` r
data_concentration |>
  ggplot(aes(timepoint, log10_concentration, color=treatment_group)) +
  geom_point() +
  geom_smooth(
    method = "lm"
  )
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](qpcr_analysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
treatments_to_keep <- unique(data_concentration$treatment_group) 
models <- treatments_to_keep |> 
  map(~ filter(data_concentration, treatment_group == .)) |>
  map(~ lm(log10_concentration ~ timepoint, .)) |>
  map(tidy) |>
  map2(
    treatments_to_keep,
    ~ mutate(.x, treatment_group=.y, collapsed=FALSE, .before=1)
    ) |> 
  list_rbind()
kable(models)
```

| treatment_group           | collapsed | term        |   estimate | std.error |   statistic |   p.value |
|:--------------------------|:----------|:------------|-----------:|----------:|------------:|----------:|
| WW + phagemid             | FALSE     | (Intercept) |  6.5175269 | 0.0340310 | 191.5175973 | 0.0000000 |
| WW + phagemid             | FALSE     | timepoint   | -0.0032189 | 0.0181903 |  -0.1769585 | 0.8605909 |
| WW + phagemid + detergent | FALSE     | (Intercept) |  6.5336741 | 0.0662993 |  98.5481520 | 0.0000000 |
| WW + phagemid + detergent | FALSE     | timepoint   | -0.2270536 | 0.0354385 |  -6.4069822 | 0.0000003 |
| WW + phagemid + bleach    | FALSE     | (Intercept) |  4.3339145 | 0.1243730 |  34.8461059 | 0.0000000 |
| WW + phagemid + bleach    | FALSE     | timepoint   | -0.4668962 | 0.0664802 |  -7.0230909 | 0.0000000 |
| TBS + phagemid            | FALSE     | (Intercept) |  6.8619033 | 0.0148942 | 460.7106971 | 0.0000000 |
| TBS + phagemid            | FALSE     | timepoint   | -0.0336933 | 0.0079613 |  -4.2321566 | 0.0001657 |

## Collapsing qPCR replicates

Now, we create a new table that collapses the qPCR replicates:

``` r
data_collapsed <- data_concentration |> 
  group_by(treatment_group, timepoint, tech_rep) |>
  summarise(log10_concentration = mean(log10_concentration), .groups="drop")
```

With the collapse, we have wider error bars on our linear estimates.
Just as the independent approximation meant that the errors were too
optimistic, this approximation is too conservative.

``` r
data_collapsed |>
  ggplot(aes(timepoint, log10_concentration, color=treatment_group)) +
  geom_point() +
  geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](qpcr_analysis_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
data_collapsed |>
  ggplot(aes(timepoint, log10_concentration, color=treatment_group)) +
  geom_point() +
  geom_smooth(
    method = "lm"
  )
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](qpcr_analysis_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

We can fit linear models separately for each timepoint and examine the
coefficients and standard errors:

``` r
models_collapsed <- treatments_to_keep |> 
  map(~ filter(data_collapsed, treatment_group == .)) |>
  map(~ lm(log10_concentration ~ timepoint, .)) |>
  map(tidy) |>
  map2(
    treatments_to_keep,
    ~ mutate(.x, treatment_group=.y, collapsed=TRUE, .before=1)
    ) |> 
  list_rbind() 
kable(models_collapsed)
```

| treatment_group           | collapsed | term        |   estimate | std.error |  statistic |   p.value |
|:--------------------------|:----------|:------------|-----------:|----------:|-----------:|----------:|
| WW + phagemid             | TRUE      | (Intercept) |  6.5175269 | 0.0608769 | 107.060722 | 0.0000000 |
| WW + phagemid             | TRUE      | timepoint   | -0.0032189 | 0.0325401 |  -0.098922 | 0.9231551 |
| WW + phagemid + detergent | TRUE      | (Intercept) |  6.5336741 | 0.1208882 |  54.047265 | 0.0000000 |
| WW + phagemid + detergent | TRUE      | timepoint   | -0.2270536 | 0.0646174 |  -3.513814 | 0.0055959 |
| WW + phagemid + bleach    | TRUE      | (Intercept) |  4.3339145 | 0.2265403 |  19.130880 | 0.0000000 |
| WW + phagemid + bleach    | TRUE      | timepoint   | -0.4668962 | 0.1210909 |  -3.855751 | 0.0031822 |
| TBS + phagemid            | TRUE      | (Intercept) |  6.8619033 | 0.0210399 | 326.137321 | 0.0000000 |
| TBS + phagemid            | TRUE      | timepoint   | -0.0336933 | 0.0112463 |  -2.995946 | 0.0134364 |

Collapsing the qPCR replicates increases the standard error of the
regression coefficients:

``` r
rbind(models, models_collapsed) |> 
  filter(term == "timepoint") |> 
  ggplot(aes(collapsed, std.error, group=treatment_group)) +
  geom_line(aes(linetype = treatment_group)) +
  geom_point()
```

![](qpcr_analysis_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

## Hierarchical model \[TODO\]
