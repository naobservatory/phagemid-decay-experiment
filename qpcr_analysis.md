---
title: "qPCR Analysis"
output: github_document
---


```r
library(tidyverse)
library(readxl)
library(broom)
library(knitr)
```

# Ingest data

The qPCR machine outputs its data in excel files.
For this experiment, we can find the files [in our lab google drive](https://drive.google.com/drive/folders/1PntkFBFwNZfeXndBdq1k72ilLebwPbQd?usp=share_link).
Eventually, we might want to automate access to the data, but for now, we'll manually download excel files and save to the `data/` directory.
In this document, we'll be looking at the 11-day-qpcr experiment.

## Read qPCR data from excel files

For each plate, we'll read the "Results" sheet.
The first 42 rows of the sheet are metadata that we don't need, so we'll skip them.
We'll also save the path in a column called "source_file" so we can look for plate effects if we'd like after we merge files.

We're only going to keep a subset of columns.
In the next block we extract the columns we want and convert them to the correct datatypes.

* `CT`: the Ct value. Either "Undetermined" or a number. We'll use `as.numeric` to convert from a character string to a number (double).
* `Sample Name` labels the content of the samples
* `Well Position` is the alphanumeric plate well ID (e.g., "A01"). Note: this is redundant with `Sample Name` here, but in the future we may want to automatically label the samples from `Well Position` using a plate layout file.
* `Ct Threshold`. These should all be `0.2`. We will check that for quality control.
* `Automatic Ct Threshold`. These should all be `FALSE`
* `Target Name` can be used to distinguish experimental samples ("Barcode_1") from blanks ("Blank") and NTCs ("Barcode_1_NTC"). We'll encode these as factors because they have a limited set of values.

We'll also rename the columns consistently in "snake case" (i.e., all lowercase with underscores separating the words.)

In the future we may want to redo the baseline subtraction manually, but for now, we'll ignore those columns.


```r
read_qpcr_excel <- function(path) {
  read_excel(
    path = path,
    sheet = "Results",
    skip = 42
  ) |>
  transmute(
    source_file = path,
    ct = as.numeric(CT),
    sample_name = `Sample Name`,
    well_position = `Well Position`,
    ct_threshold = `Ct Threshold`,
    auto_ct_threshold = `Automatic Ct Threshold`,
    target_name = as.factor(`Target Name`)
  )
}
```
TODO: It would be nice to specify what values to convert to `NA` quietly.


```r
data_dir <- "data/11-day-qpcr"
filename_pattern <- "2023-01-21_degradation_plate[0-9]+[.]xls"
data_raw <- list.files(
  data_dir,
  pattern = filename_pattern,
  full.names = TRUE
  ) |>
  map(read_qpcr_excel) |>
  list_rbind()
```

```
## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
```

```r
kable(head(data_raw, n = 10))
```



|source_file                                        |       ct|sample_name |well_position | ct_threshold|auto_ct_threshold |target_name |
|:--------------------------------------------------|--------:|:-----------|:-------------|------------:|:-----------------|:-----------|
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls |       NA|Blank       |A1            |          0.2|FALSE             |Blank       |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls |       NA|NTC         |A2            |          0.2|FALSE             |NTC         |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.34180|A0-1        |A3            |          0.2|FALSE             |Phagemid    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.29534|A0-1        |A4            |          0.2|FALSE             |Phagemid    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.55094|A0-1        |A5            |          0.2|FALSE             |Phagemid    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.35072|A0-2        |A6            |          0.2|FALSE             |Phagemid    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.34296|A0-2        |A7            |          0.2|FALSE             |Phagemid    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.40829|A0-2        |A8            |          0.2|FALSE             |Phagemid    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.42208|A0-3        |A9            |          0.2|FALSE             |Phagemid    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.30456|A0-3        |A10           |          0.2|FALSE             |Phagemid    |

For quality control, let's check assertions our assertions about about `ct_threshold` and `autothreshold`.


```r
expected_ct_threshold <- 0.2
data_raw |>
  summarize(
    all_auto_ct_theshold_false = all(!auto_ct_threshold),
    all_ct_theshold_expected = all(ct_threshold == expected_ct_threshold)
  )
```

```
## # A tibble: 1 × 2
##   all_auto_ct_theshold_false all_ct_theshold_expected
##   <lgl>                      <lgl>                   
## 1 TRUE                       TRUE
```

## Tidy the data

Currently we have the `sample_name` column, which contains three pieces of information: the experiment (e.g., "A", "B", ...), the timepoint, and the technical replicate number.
To make the data tidier, we'll split this into separate columns.

We will use regular expressions to match the expected pattern, which is:

1. The start of the string
2. The treatment group, specified by one or more letters
3. The timepoint, specified by one or more numbers
4. A dash
5. The technical replicate, specified by one or more numbers
6. The end of the string

Finally, we'll rename the single-letter treatment groups with more informative names and mark the negative controls so we can separate them out later.


```r
coding = list(
  "A" = "wastewater + phagemid",
  "B" = "wastewater + phagemid + bleach",
  "C" = "TBS + phagemid",
  "D" = "wastewater"
)

controls <-  c("Blank", "NTC", "wastewater")

sample_name_pattern = c(
  "^",
  treatment_group = "[A-Za-z]+",
  timepoint = "[0-9]+",
  "\\-",
  tech_rep = "[0-9]+",
  "$"
)

data_tidy <- data_raw |>
  mutate(
    sample_name = str_replace(sample_name, "B1_NTC", "NTC")
  ) |>
  separate_wider_regex(
    sample_name,
    patterns = sample_name_pattern,
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



|source_file                                        |       ct|treatment_group       | timepoint|tech_rep |well_position | ct_threshold|auto_ct_threshold |target_name |control |
|:--------------------------------------------------|--------:|:---------------------|---------:|:--------|:-------------|------------:|:-----------------|:-----------|:-------|
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls |       NA|Blank                 |        NA|NA       |A1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls |       NA|NTC                   |        NA|NA       |A2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.34180|wastewater + phagemid |         0|1        |A3            |          0.2|FALSE             |Phagemid    |FALSE   |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.29534|wastewater + phagemid |         0|1        |A4            |          0.2|FALSE             |Phagemid    |FALSE   |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.55094|wastewater + phagemid |         0|1        |A5            |          0.2|FALSE             |Phagemid    |FALSE   |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.35072|wastewater + phagemid |         0|2        |A6            |          0.2|FALSE             |Phagemid    |FALSE   |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.34296|wastewater + phagemid |         0|2        |A7            |          0.2|FALSE             |Phagemid    |FALSE   |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.40829|wastewater + phagemid |         0|2        |A8            |          0.2|FALSE             |Phagemid    |FALSE   |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.42208|wastewater + phagemid |         0|3        |A9            |          0.2|FALSE             |Phagemid    |FALSE   |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.30456|wastewater + phagemid |         0|3        |A10           |          0.2|FALSE             |Phagemid    |FALSE   |

Let's make sure we have 24 blanks, 23 NTCs, 45 wastewater + phagemid and TBS + phagement (3 pcr replicates * 3 technical replicates * 5 timepoints), 63 bleach and wastewater (7 timepoints).

```r
data_tidy |>
  group_by(treatment_group) |>
  count() |>
  kable()
```



|treatment_group                |  n|
|:------------------------------|--:|
|Blank                          | 24|
|NTC                            | 23|
|TBS + phagemid                 | 45|
|wastewater                     | 69|
|wastewater + phagemid          | 45|
|wastewater + phagemid + bleach | 63|

TODO: Relabel the missing wastewater wells.

First, let's take a look at the Blanks and NTCs. These should mostly have `NA` for their ct value.

```r
data_tidy |>
  filter(treatment_group == "Blank" | treatment_group == "NTC") |>
  kable()
```



|source_file                                        |        ct|treatment_group | timepoint|tech_rep |well_position | ct_threshold|auto_ct_threshold |target_name |control |
|:--------------------------------------------------|---------:|:---------------|---------:|:--------|:-------------|------------:|:-----------------|:-----------|:-------|
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls |        NA|Blank           |        NA|NA       |A1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls |        NA|NTC             |        NA|NA       |A2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls |        NA|Blank           |        NA|NA       |B1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls |        NA|NTC             |        NA|NA       |B2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls |  7.122666|Blank           |        NA|NA       |C1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls |        NA|NTC             |        NA|NA       |C2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls |  1.101108|Blank           |        NA|NA       |D1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls |        NA|NTC             |        NA|NA       |D2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 37.704498|Blank           |        NA|NA       |E1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls |        NA|NTC             |        NA|NA       |E2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls |  8.145022|Blank           |        NA|NA       |F1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls |        NA|NTC             |        NA|NA       |F2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 31.199942|Blank           |        NA|NA       |G1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 23.488712|Blank           |        NA|NA       |H1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls |        NA|NTC             |        NA|NA       |H2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate2.xls | 16.789751|Blank           |        NA|NA       |A1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate2.xls |        NA|NTC             |        NA|NA       |A2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate2.xls | 18.323210|Blank           |        NA|NA       |B1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate2.xls |        NA|NTC             |        NA|NA       |B2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate2.xls |        NA|Blank           |        NA|NA       |C1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate2.xls |        NA|NTC             |        NA|NA       |C2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate2.xls | 35.004986|Blank           |        NA|NA       |D1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate2.xls |        NA|NTC             |        NA|NA       |D2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate2.xls | 36.372150|Blank           |        NA|NA       |E1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate2.xls |        NA|NTC             |        NA|NA       |E2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate2.xls | 25.604013|Blank           |        NA|NA       |F1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate2.xls |        NA|NTC             |        NA|NA       |F2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate2.xls |        NA|Blank           |        NA|NA       |G1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate2.xls |        NA|NTC             |        NA|NA       |G2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate2.xls |        NA|Blank           |        NA|NA       |H1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate2.xls |        NA|NTC             |        NA|NA       |H2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate3.xls | 13.887089|Blank           |        NA|NA       |A1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate3.xls |        NA|NTC             |        NA|NA       |A2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate3.xls | 18.587942|Blank           |        NA|NA       |B1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate3.xls |        NA|NTC             |        NA|NA       |B2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate3.xls | 15.852350|Blank           |        NA|NA       |C1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate3.xls |        NA|NTC             |        NA|NA       |C2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate3.xls | 28.758543|Blank           |        NA|NA       |D1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate3.xls |        NA|NTC             |        NA|NA       |D2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate3.xls |        NA|Blank           |        NA|NA       |E1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate3.xls |        NA|NTC             |        NA|NA       |E2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate3.xls |        NA|Blank           |        NA|NA       |F1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate3.xls |        NA|NTC             |        NA|NA       |F2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate3.xls | 13.545769|Blank           |        NA|NA       |G1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate3.xls |        NA|NTC             |        NA|NA       |G2            |          0.2|FALSE             |NTC         |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate3.xls | 35.200001|Blank           |        NA|NA       |H1            |          0.2|FALSE             |Blank       |TRUE    |
|data/11-day-qpcr/2023-01-21_degradation_plate3.xls |        NA|NTC             |        NA|NA       |H2            |          0.2|FALSE             |NTC         |TRUE    |

Note that many of the Blanks have low CT values.
TODO: investigate these by looking at the raw data.

We can make a boxplot to compare the distribution of ct across treatments.
To disply the NAs, we'll set them to one more than the maximum possible values.


```r
ct_max <- max(data_tidy$ct, na.rm = TRUE) + 1
data_tidy |>
  mutate(ct = replace_na(ct, ct_max)) |>
  ggplot(mapping = aes(ct, treatment_group)) +
  geom_boxplot()
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

We see that the wastewater and NTC have high CT values, mostly NA, as expected.
Again we see that the Blanks have lower than expected CT, which we should investigate.

# Convert raw Ct values to concentrations

We would like to transform our `c_t` values to concentrations (or more specifically log concentrations).
We expect `c_t` to depend on log concentration according to the linear relationship $c_t = a \log(\text{conc}) + b$.
To estimate the coefficients $a$ and $b$, we use a *standard curve*, a series of qPCR measurements on sequentially diluted samples.
Note that we expect these coefficients to be phagemid-specific, so we will need to do a standard curve for each phagemid.

## Make the standard curve

Ari did a single dilution series with 10x dilution at each step and three qPCR measurements of each dilution.
First, we'll read the excel file produced by the qPCR machine.
We want barcode 18-04, which is "B2" in the file.

The samples in this file are labeled by their level of dilution.
We convert this label to log10 concentration.
In Ari's coding, dilution $d$ is 10x more concentrated than dilution $d-1$.
We also need to know the expected concentration at $d=0$.
In this case it's 0.9871 copies per microliter.
In the future, we should probably include uncertainty in this value.


```r
standard_curve_file <- paste("data/", "T1_QS2_018_BC49 and T1_QS2_018_BC04.xls", sep="")
concentration_at_d0 <- 0.9871
dilution_factor <- 10

sample_name_pattern_standard_curve <- c(
  "^",
  group = "[A-Za-z0-9]+",
  "_D",
  dilution = "[0-9]+",
  "$"
)

data_standard_curve <- read_qpcr_excel(standard_curve_file) |>
  separate_wider_regex(
    sample_name,
    patterns = sample_name_pattern_standard_curve,
    too_few = "align_start"
  ) |>
  mutate(dilution = as.integer(dilution)) |>
  filter(group == "B2") |>
  drop_na(dilution) |>
  mutate(log10_concentration = log10(concentration_at_d0) + log10(dilution_factor) * dilution)
```

```
## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
```

```r
kable(head(data_standard_curve, n = 10))
```



|source_file                                  |       ct|group | dilution|well_position | ct_threshold|auto_ct_threshold |target_name | log10_concentration|
|:--------------------------------------------|--------:|:-----|--------:|:-------------|------------:|:-----------------|:-----------|-------------------:|
|data/T1_QS2_018_BC49 and T1_QS2_018_BC04.xls |       NA|B2    |        1|E3            |          0.2|FALSE             |Barcode_2   |           0.9943612|
|data/T1_QS2_018_BC49 and T1_QS2_018_BC04.xls | 36.90071|B2    |        2|E4            |          0.2|FALSE             |Barcode_2   |           1.9943612|
|data/T1_QS2_018_BC49 and T1_QS2_018_BC04.xls | 31.61678|B2    |        3|E5            |          0.2|FALSE             |Barcode_2   |           2.9943612|
|data/T1_QS2_018_BC49 and T1_QS2_018_BC04.xls | 28.23690|B2    |        4|E6            |          0.2|FALSE             |Barcode_2   |           3.9943612|
|data/T1_QS2_018_BC49 and T1_QS2_018_BC04.xls | 26.06779|B2    |        5|E7            |          0.2|FALSE             |Barcode_2   |           4.9943612|
|data/T1_QS2_018_BC49 and T1_QS2_018_BC04.xls | 22.05504|B2    |        6|E8            |          0.2|FALSE             |Barcode_2   |           5.9943612|
|data/T1_QS2_018_BC49 and T1_QS2_018_BC04.xls | 17.27207|B2    |        7|E9            |          0.2|FALSE             |Barcode_2   |           6.9943612|
|data/T1_QS2_018_BC49 and T1_QS2_018_BC04.xls |       NA|B2    |        1|F3            |          0.2|FALSE             |Barcode_2   |           0.9943612|
|data/T1_QS2_018_BC49 and T1_QS2_018_BC04.xls | 36.73648|B2    |        2|F4            |          0.2|FALSE             |Barcode_2   |           1.9943612|
|data/T1_QS2_018_BC49 and T1_QS2_018_BC04.xls | 31.35515|B2    |        3|F5            |          0.2|FALSE             |Barcode_2   |           2.9943612|


```r
data_standard_curve |>
  ggplot(mapping = aes(log10_concentration, ct)) +
  geom_point() +
  geom_smooth(method = "lm")
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

```
## Warning: Removed 4 rows containing non-finite values (`stat_smooth()`).
```

```
## Warning: Removed 4 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)
The common deviations from linearity at each dilution suggests pipetting error in the dilution series in addition to qPCR variation.

Fit a linear model and save the coefficients.

```r
standard_curve <- lm(ct ~ log10_concentration, data_standard_curve)
summary(standard_curve)
```

```
## 
## Call:
## lm(formula = ct ~ log10_concentration, data = data_standard_curve)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.3603 -0.5192  0.2738  0.6095  0.9971 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          43.4772     0.5286   82.25  < 2e-16 ***
## log10_concentration  -3.6855     0.1074  -34.33 1.13e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.7272 on 15 degrees of freedom
##   (4 observations deleted due to missingness)
## Multiple R-squared:  0.9874,	Adjusted R-squared:  0.9866 
## F-statistic:  1178 on 1 and 15 DF,  p-value: 1.132e-15
```

```r
std_curve_slope  <- standard_curve$coefficients["log10_concentration"]
std_curve_intercept  <- standard_curve$coefficients["(Intercept)"]
```

Note: a correct model would take into account that errors in dilution propagate to all future dilutions.

## Apply standard curve

Now we can apply the standard curve to our data.
We're also going to filter out the negative controls from now on.


```r
data_concentration <- data_tidy |>
  filter(!control) |>
  mutate(log10_concentration = (ct - std_curve_intercept) / std_curve_slope)
kable(head(data_concentration, n = 10))
```



|source_file                                        |       ct|treatment_group                | timepoint|tech_rep |well_position | ct_threshold|auto_ct_threshold |target_name |control | log10_concentration|
|:--------------------------------------------------|--------:|:------------------------------|---------:|:--------|:-------------|------------:|:-----------------|:-----------|:-------|-------------------:|
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.34180|wastewater + phagemid          |         0|1        |A3            |          0.2|FALSE             |Phagemid    |FALSE   |            6.820144|
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.29534|wastewater + phagemid          |         0|1        |A4            |          0.2|FALSE             |Phagemid    |FALSE   |            6.832752|
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.55094|wastewater + phagemid          |         0|1        |A5            |          0.2|FALSE             |Phagemid    |FALSE   |            6.763397|
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.35072|wastewater + phagemid          |         0|2        |A6            |          0.2|FALSE             |Phagemid    |FALSE   |            6.817725|
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.34296|wastewater + phagemid          |         0|2        |A7            |          0.2|FALSE             |Phagemid    |FALSE   |            6.819831|
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.40829|wastewater + phagemid          |         0|2        |A8            |          0.2|FALSE             |Phagemid    |FALSE   |            6.802104|
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.42208|wastewater + phagemid          |         0|3        |A9            |          0.2|FALSE             |Phagemid    |FALSE   |            6.798361|
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.30456|wastewater + phagemid          |         0|3        |A10           |          0.2|FALSE             |Phagemid    |FALSE   |            6.830250|
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.38781|wastewater + phagemid          |         0|3        |A11           |          0.2|FALSE             |Phagemid    |FALSE   |            6.807662|
|data/11-day-qpcr/2023-01-21_degradation_plate1.xls | 18.67497|wastewater + phagemid + bleach |         0|1        |B3            |          0.2|FALSE             |Phagemid    |FALSE   |            6.729744|

Since we have three PCR replicates per technical replicate, we can summarize our data with min, median, and max without any loss of information.


```r
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



|treatment_group                | timepoint|tech_rep | conc_min| conc_med| conc_max|
|:------------------------------|---------:|:--------|--------:|--------:|--------:|
|TBS + phagemid                 |         0|1        | 6.848491| 6.874437| 6.890585|
|TBS + phagemid                 |         0|2        | 6.690239| 6.696246| 6.727786|
|TBS + phagemid                 |         0|3        | 6.836330| 6.845710| 6.866186|
|TBS + phagemid                 |         3|1        | 6.821579| 6.832730| 6.850520|
|TBS + phagemid                 |         3|2        | 6.921391| 6.936402| 6.959049|
|TBS + phagemid                 |         3|3        | 6.881070| 6.912953| 6.936426|
|TBS + phagemid                 |         4|1        | 6.697814| 6.701416| 6.723007|
|TBS + phagemid                 |         4|2        | 6.697952| 6.744917| 6.747592|
|TBS + phagemid                 |         4|3        | 6.831916| 6.872476| 6.911891|
|TBS + phagemid                 |         8|1        | 6.820742| 6.851124| 6.875485|
|TBS + phagemid                 |         8|2        | 6.768991| 6.776604| 6.793206|
|TBS + phagemid                 |         8|3        | 6.676517| 6.699110| 6.725580|
|TBS + phagemid                 |        11|1        | 6.624997| 6.630496| 6.641820|
|TBS + phagemid                 |        11|2        | 6.592235| 6.656606| 6.666144|
|TBS + phagemid                 |        11|3        | 6.661934| 6.677105| 6.682441|
|wastewater + phagemid          |         0|1        | 6.763397| 6.820144| 6.832752|
|wastewater + phagemid          |         0|2        | 6.802104| 6.817725| 6.819831|
|wastewater + phagemid          |         0|3        | 6.798361| 6.807662| 6.830250|
|wastewater + phagemid          |         3|1        | 6.685651| 6.712316| 6.753321|
|wastewater + phagemid          |         3|2        | 6.618200| 6.635098| 6.670514|
|wastewater + phagemid          |         3|3        | 6.675817| 6.720248| 6.735693|
|wastewater + phagemid          |         4|1        | 6.610231| 6.665555| 6.699638|
|wastewater + phagemid          |         4|2        | 6.616483| 6.656540| 6.657536|
|wastewater + phagemid          |         4|3        | 6.687298| 6.726550| 6.743163|
|wastewater + phagemid          |         8|1        | 6.467016| 6.560424| 6.571577|
|wastewater + phagemid          |         8|2        | 6.498120| 6.508134| 6.529151|
|wastewater + phagemid          |         8|3        | 6.348102| 6.386062| 6.403592|
|wastewater + phagemid          |        11|1        | 6.480610| 6.529886| 6.557219|
|wastewater + phagemid          |        11|2        | 6.593274| 6.593362| 6.593814|
|wastewater + phagemid          |        11|3        | 6.583409| 6.617205| 6.634971|
|wastewater + phagemid + bleach |         0|1        | 6.729744| 6.746106| 6.781763|
|wastewater + phagemid + bleach |         0|2        | 6.730216| 6.739548| 6.756291|
|wastewater + phagemid + bleach |         0|3        | 6.749397| 6.763163| 6.767459|
|wastewater + phagemid + bleach |         1|1        | 6.616427| 6.708162| 6.714910|
|wastewater + phagemid + bleach |         1|2        | 6.587873| 6.625963| 6.631138|
|wastewater + phagemid + bleach |         1|3        | 6.700366| 6.705834| 6.707775|
|wastewater + phagemid + bleach |         2|1        | 6.655631| 6.673966| 6.718287|
|wastewater + phagemid + bleach |         2|2        | 6.514317| 6.583239| 6.660965|
|wastewater + phagemid + bleach |         2|3        | 6.645268| 6.699751| 6.721067|
|wastewater + phagemid + bleach |         3|1        | 6.665023| 6.686003| 6.744820|
|wastewater + phagemid + bleach |         3|2        | 6.652584| 6.659688| 6.692501|
|wastewater + phagemid + bleach |         3|3        | 6.668292| 6.668701| 6.810810|
|wastewater + phagemid + bleach |         4|1        | 6.539586| 6.542637| 6.572873|
|wastewater + phagemid + bleach |         4|2        | 6.703937| 6.790166| 6.794634|
|wastewater + phagemid + bleach |         4|3        | 6.598908| 6.628981| 6.656334|
|wastewater + phagemid + bleach |         8|1        | 6.614022| 6.625182| 6.703118|
|wastewater + phagemid + bleach |         8|2        | 6.494957| 6.498736| 6.510038|
|wastewater + phagemid + bleach |         8|3        | 6.425300| 6.427169| 6.448430|
|wastewater + phagemid + bleach |        11|1        | 6.397518| 6.403579| 6.404978|
|wastewater + phagemid + bleach |        11|2        | 6.289515| 6.320439| 6.357832|
|wastewater + phagemid + bleach |        11|3        | 6.403920| 6.432470| 6.440736|

# Plot log concentrations vs time for each condition

First, we plot all the treatment groups on the same timecourse to see differences in absolute as well as relative concentration.


```r
data_concentration |>
  ggplot(
    aes(timepoint, log10_concentration, shape = treatment_group, color = treatment_group)
    ) +
  geom_point(
    position = position_jitter(height = 0, width = 0.1, seed = 3579237)
  )
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)

Let's look at within- vs between-technical replicate variation:


```r
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

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)

# Regression analysis

In this section, we'll look at the trends in concentration over time.
First, we'll make the approximation that all of the qPCR measurements for a `(treatment_group, timepoint)` pair are independent.
This is not exactly true because the qPCR replicates of the same technical replicate share the noise of the technical replicate.
Thus, the error bars on these estimates will be too optimistic.
Next, we'll make the opposite approximation: that the mean of the qPCR replicates is a single observation.
In the future, we'll look at a hierarchical model that incorporates the dependency structure of the measurements.

## Treating all observations as independent

We can use a Loess curve to see the trend for each treatment:

```r
data_concentration |>
  ggplot(aes(timepoint, log10_concentration, color=treatment_group)) +
  geom_point() +
  geom_smooth()
```

```
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)

The Loess is clearly overfitting to the noise between timepoints. We can also use a linear model:

```r
data_concentration |>
  ggplot(aes(timepoint, log10_concentration, color=treatment_group)) +
  geom_point() +
  geom_smooth(
    method = "lm"
  )
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)


```r
# TODO: Make this function more generic
fit_lm_by_treatment <- function(data){
  treatments <- unique(data$treatment_group)
  treatments |>
    map(~ filter(data, treatment_group == .)) |>
    map(~ lm(log10_concentration ~ timepoint, .)) |>
    map(tidy) |>
    map2(
      treatments,
      ~ mutate(.x, treatment_group=.y, .before=1)
      ) |>
    list_rbind()
}

models <- fit_lm_by_treatment(data_concentration) |>
    mutate(collapsed=FALSE, .before=1)
kable(models)
```



|collapsed |treatment_group                |term        |   estimate| std.error|  statistic|  p.value|
|:---------|:------------------------------|:-----------|----------:|---------:|----------:|--------:|
|FALSE     |wastewater + phagemid          |(Intercept) |  6.7747858| 0.0193852| 349.482634| 0.00e+00|
|FALSE     |wastewater + phagemid          |timepoint   | -0.0249887| 0.0029912|  -8.354077| 0.00e+00|
|FALSE     |wastewater + phagemid + bleach |(Intercept) |  6.7411412| 0.0136427| 494.122350| 0.00e+00|
|FALSE     |wastewater + phagemid + bleach |timepoint   | -0.0298194| 0.0024617| -12.113503| 0.00e+00|
|FALSE     |TBS + phagemid                 |(Intercept) |  6.8603451| 0.0204201| 335.960076| 0.00e+00|
|FALSE     |TBS + phagemid                 |timepoint   | -0.0155439| 0.0031509|  -4.933183| 1.26e-05|

## Collapsing qPCR replicates

Now, we create a new table that collapses the qPCR replicates:

```r
data_collapsed <- data_concentration |>
  group_by(treatment_group, timepoint, tech_rep) |>
  summarise(log10_concentration = mean(log10_concentration), .groups="drop")
```

With the collapse, we have wider error bars on our linear estimates.
Just as the independent approximation meant that the errors were too optimistic, this approximation is too conservative.


```r
data_collapsed |>
  ggplot(aes(timepoint, log10_concentration, color=treatment_group)) +
  geom_point() +
  geom_smooth()
```

```
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png)


```r
data_collapsed |>
  ggplot(aes(timepoint, log10_concentration, color=treatment_group)) +
  geom_point() +
  geom_smooth(
    method = "lm"
  )
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21-1.png)

We can fit linear models separately for each timepoint and examine the coefficients and standard errors:


```r
models_collapsed <- fit_lm_by_treatment(data_collapsed) |>
    mutate(collapsed=TRUE, .before=1)
models_all <- rbind(models, models_collapsed)
kable(models_all)
```



|collapsed |treatment_group                |term        |   estimate| std.error|  statistic|   p.value|
|:---------|:------------------------------|:-----------|----------:|---------:|----------:|---------:|
|FALSE     |wastewater + phagemid          |(Intercept) |  6.7747858| 0.0193852| 349.482634| 0.0000000|
|FALSE     |wastewater + phagemid          |timepoint   | -0.0249887| 0.0029912|  -8.354077| 0.0000000|
|FALSE     |wastewater + phagemid + bleach |(Intercept) |  6.7411412| 0.0136427| 494.122350| 0.0000000|
|FALSE     |wastewater + phagemid + bleach |timepoint   | -0.0298194| 0.0024617| -12.113503| 0.0000000|
|FALSE     |TBS + phagemid                 |(Intercept) |  6.8603451| 0.0204201| 335.960076| 0.0000000|
|FALSE     |TBS + phagemid                 |timepoint   | -0.0155439| 0.0031509|  -4.933183| 0.0000126|
|TRUE      |TBS + phagemid                 |(Intercept) |  6.8603451| 0.0360482| 190.310215| 0.0000000|
|TRUE      |TBS + phagemid                 |timepoint   | -0.0155439| 0.0055624|  -2.794484| 0.0151892|
|TRUE      |wastewater + phagemid          |(Intercept) |  6.7747858| 0.0332204| 203.934392| 0.0000000|
|TRUE      |wastewater + phagemid          |timepoint   | -0.0249887| 0.0051260|  -4.874874| 0.0003034|
|TRUE      |wastewater + phagemid + bleach |(Intercept) |  6.7411412| 0.0220541| 305.663907| 0.0000000|
|TRUE      |wastewater + phagemid + bleach |timepoint   | -0.0298194| 0.0039794|  -7.493409| 0.0000004|

Collapsing the qPCR replicates increases the standard error of the regression coefficients:

```r
models_all |>
  filter(term == "timepoint") |>
  ggplot(aes(collapsed, std.error, group=treatment_group)) +
  geom_line(aes(linetype = treatment_group)) +
  geom_point()
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23-1.png)

# Hierarchical model with error propagation [TODO]
