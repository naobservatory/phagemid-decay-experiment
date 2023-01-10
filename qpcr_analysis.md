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
For this experiment, we can find the files [in our lab google drive](https://drive.google.com/drive/folders/1W7bcgLTdgLp2NX2_Yl3J17dTZ71Bi6Ib).
Eventually, we might want to automate access to the data, but for now, we'll manually download Plates 1, 2, and 3 excel files and save to the `data/` directory.

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
data_dir <- "data/"
filename_pattern <- "Plate [0-9]+[.]xls"
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



|source_file       |       ct|sample_name |well_position | ct_threshold|auto_ct_threshold |target_name   |
|:-----------------|--------:|:-----------|:-------------|------------:|:-----------------|:-------------|
|data//Plate 1.xls |       NA|Blank       |A1            |          0.2|FALSE             |Blank         |
|data//Plate 1.xls |       NA|Blank       |A2            |          0.2|FALSE             |Blank         |
|data//Plate 1.xls |       NA|Blank       |A3            |          0.2|FALSE             |Blank         |
|data//Plate 1.xls | 35.92194|B1_NTC      |B1            |          0.2|FALSE             |Barcode_1_NTC |
|data//Plate 1.xls |       NA|B1_NTC      |B2            |          0.2|FALSE             |Barcode_1_NTC |
|data//Plate 1.xls | 36.77825|B1_NTC      |B3            |          0.2|FALSE             |Barcode_1_NTC |
|data//Plate 1.xls | 20.32065|A0.1        |C1            |          0.2|FALSE             |Barcode_1     |
|data//Plate 1.xls | 19.76434|A0.2        |C2            |          0.2|FALSE             |Barcode_1     |
|data//Plate 1.xls | 18.81370|A0.3        |C3            |          0.2|FALSE             |Barcode_1     |
|data//Plate 1.xls | 18.81602|A1.1        |C4            |          0.2|FALSE             |Barcode_1     |

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
4. A period
5. The technical replicate, specified by one or more numbers
6. The end of the string

In our input data, the non-template control was named "B1_NTC", which will break our naming scheme. We'll rename it to "NTC" first.

Finally, we'll rename the single-letter treatment groups with more informative names and mark the negative controls so we can separate them out later.


```r
coding = list(
  "A" = "WW + phagemid",
  "B" = "WW + phagemid + detergent",
  "C" = "TBS + phagemid",
  "D" = "WW + phagemid + bleach",
  "E" = "Just WW"
)

controls <-  c("Blank", "NTC", "Just WW")

sample_name_pattern = c(
  "^",
  treatment_group = "[A-Za-z]+",
  timepoint = "[0-9]+",
  "\\.",
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



|source_file       |       ct|treatment_group | timepoint|tech_rep |well_position | ct_threshold|auto_ct_threshold |target_name   |control |
|:-----------------|--------:|:---------------|---------:|:--------|:-------------|------------:|:-----------------|:-------------|:-------|
|data//Plate 1.xls |       NA|Blank           |        NA|NA       |A1            |          0.2|FALSE             |Blank         |TRUE    |
|data//Plate 1.xls |       NA|Blank           |        NA|NA       |A2            |          0.2|FALSE             |Blank         |TRUE    |
|data//Plate 1.xls |       NA|Blank           |        NA|NA       |A3            |          0.2|FALSE             |Blank         |TRUE    |
|data//Plate 1.xls | 35.92194|NTC             |        NA|NA       |B1            |          0.2|FALSE             |Barcode_1_NTC |TRUE    |
|data//Plate 1.xls |       NA|NTC             |        NA|NA       |B2            |          0.2|FALSE             |Barcode_1_NTC |TRUE    |
|data//Plate 1.xls | 36.77825|NTC             |        NA|NA       |B3            |          0.2|FALSE             |Barcode_1_NTC |TRUE    |
|data//Plate 1.xls | 20.32065|WW + phagemid   |         0|1        |C1            |          0.2|FALSE             |Barcode_1     |FALSE   |
|data//Plate 1.xls | 19.76434|WW + phagemid   |         0|2        |C2            |          0.2|FALSE             |Barcode_1     |FALSE   |
|data//Plate 1.xls | 18.81370|WW + phagemid   |         0|3        |C3            |          0.2|FALSE             |Barcode_1     |FALSE   |
|data//Plate 1.xls | 18.81602|WW + phagemid   |         1|1        |C4            |          0.2|FALSE             |Barcode_1     |FALSE   |

Let's make sure we have 9 blanks and NTCs (3 per plate) and 36 of the other treatments (3 pcr replicates * 3 technical replicates * 4 timepoints).

```r
data_tidy |>
  group_by(treatment_group) |>
  count() |>
  kable()
```



|treatment_group           |  n|
|:-------------------------|--:|
|Blank                     |  9|
|Just WW                   | 36|
|NTC                       |  9|
|TBS + phagemid            | 36|
|WW + phagemid             | 36|
|WW + phagemid + bleach    | 36|
|WW + phagemid + detergent | 36|

First, let's take a look at the Blanks and NTCs. These should mostly have `NA` for their ct value.

```r
data_tidy |>
  filter(treatment_group == "Blank" | treatment_group == "NTC") |>
  kable()
```



|source_file       |        ct|treatment_group | timepoint|tech_rep |well_position | ct_threshold|auto_ct_threshold |target_name   |control |
|:-----------------|---------:|:---------------|---------:|:--------|:-------------|------------:|:-----------------|:-------------|:-------|
|data//Plate 1.xls |        NA|Blank           |        NA|NA       |A1            |          0.2|FALSE             |Blank         |TRUE    |
|data//Plate 1.xls |        NA|Blank           |        NA|NA       |A2            |          0.2|FALSE             |Blank         |TRUE    |
|data//Plate 1.xls |        NA|Blank           |        NA|NA       |A3            |          0.2|FALSE             |Blank         |TRUE    |
|data//Plate 1.xls | 35.921940|NTC             |        NA|NA       |B1            |          0.2|FALSE             |Barcode_1_NTC |TRUE    |
|data//Plate 1.xls |        NA|NTC             |        NA|NA       |B2            |          0.2|FALSE             |Barcode_1_NTC |TRUE    |
|data//Plate 1.xls | 36.778248|NTC             |        NA|NA       |B3            |          0.2|FALSE             |Barcode_1_NTC |TRUE    |
|data//Plate 2.xls | 33.499622|Blank           |        NA|NA       |A1            |          0.2|FALSE             |Blank         |TRUE    |
|data//Plate 2.xls |        NA|Blank           |        NA|NA       |A2            |          0.2|FALSE             |Blank         |TRUE    |
|data//Plate 2.xls |  6.357395|Blank           |        NA|NA       |A3            |          0.2|FALSE             |Blank         |TRUE    |
|data//Plate 2.xls |        NA|NTC             |        NA|NA       |B1            |          0.2|FALSE             |Barcode_1_NTC |TRUE    |
|data//Plate 2.xls |        NA|NTC             |        NA|NA       |B2            |          0.2|FALSE             |Barcode_1_NTC |TRUE    |
|data//Plate 2.xls |        NA|NTC             |        NA|NA       |B3            |          0.2|FALSE             |Barcode_1_NTC |TRUE    |
|data//Plate 3.xls |        NA|Blank           |        NA|NA       |A1            |          0.2|FALSE             |Blank         |TRUE    |
|data//Plate 3.xls |        NA|Blank           |        NA|NA       |A2            |          0.2|FALSE             |Blank         |TRUE    |
|data//Plate 3.xls |        NA|Blank           |        NA|NA       |A3            |          0.2|FALSE             |Blank         |TRUE    |
|data//Plate 3.xls | 39.660065|NTC             |        NA|NA       |B1            |          0.2|FALSE             |Barcode_1_NTC |TRUE    |
|data//Plate 3.xls |        NA|NTC             |        NA|NA       |B2            |          0.2|FALSE             |Barcode_1_NTC |TRUE    |
|data//Plate 3.xls | 37.288032|NTC             |        NA|NA       |B3            |          0.2|FALSE             |Barcode_1_NTC |TRUE    |

We can make a boxplot to compare the distribution of ct across treatments.
To disply the NAs, we'll set them to one more than the maximum possible values.
We see that the Blanks, NTCs, and Just WW have CT > 35, with a few outliers.
We'll probably want to check those at some point.


```r
ct_max <- max(data_tidy$ct, na.rm = TRUE) + 1
data_tidy |>
  mutate(ct = replace_na(ct, ct_max)) |>
  ggplot(mapping = aes(ct, treatment_group)) +
  geom_boxplot()
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

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
standard_curve_file <- paste(data_dir, "T1_QS2_018_BC49 and T1_QS2_018_BC04.xls", sep="")
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



|source_file       |       ct|treatment_group | timepoint|tech_rep |well_position | ct_threshold|auto_ct_threshold |target_name |control | log10_concentration|
|:-----------------|--------:|:---------------|---------:|:--------|:-------------|------------:|:-----------------|:-----------|:-------|-------------------:|
|data//Plate 1.xls | 20.32065|WW + phagemid   |         0|1        |C1            |          0.2|FALSE             |Barcode_1   |FALSE   |            6.283210|
|data//Plate 1.xls | 19.76434|WW + phagemid   |         0|2        |C2            |          0.2|FALSE             |Barcode_1   |FALSE   |            6.434158|
|data//Plate 1.xls | 18.81370|WW + phagemid   |         0|3        |C3            |          0.2|FALSE             |Barcode_1   |FALSE   |            6.692102|
|data//Plate 1.xls | 18.81602|WW + phagemid   |         1|1        |C4            |          0.2|FALSE             |Barcode_1   |FALSE   |            6.691471|
|data//Plate 1.xls | 19.44741|WW + phagemid   |         1|2        |C5            |          0.2|FALSE             |Barcode_1   |FALSE   |            6.520152|
|data//Plate 1.xls | 19.54228|WW + phagemid   |         1|3        |C6            |          0.2|FALSE             |Barcode_1   |FALSE   |            6.494412|
|data//Plate 1.xls | 18.98299|WW + phagemid   |         2|1        |C7            |          0.2|FALSE             |Barcode_1   |FALSE   |            6.646168|
|data//Plate 1.xls | 18.98078|WW + phagemid   |         2|2        |C8            |          0.2|FALSE             |Barcode_1   |FALSE   |            6.646766|
|data//Plate 1.xls | 19.66541|WW + phagemid   |         2|3        |C9            |          0.2|FALSE             |Barcode_1   |FALSE   |            6.461001|
|data//Plate 1.xls | 20.02242|WW + phagemid   |         3|1        |C10           |          0.2|FALSE             |Barcode_1   |FALSE   |            6.364132|

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



|treatment_group           | timepoint|tech_rep | conc_min| conc_med| conc_max|
|:-------------------------|---------:|:--------|--------:|--------:|--------:|
|TBS + phagemid            |         0|1        | 6.768629| 6.831939| 6.850767|
|TBS + phagemid            |         0|2        | 6.792594| 6.838014| 6.873845|
|TBS + phagemid            |         0|3        | 6.685091| 6.888193| 6.901456|
|TBS + phagemid            |         1|1        | 6.847414| 6.850799| 6.868806|
|TBS + phagemid            |         1|2        | 6.861752| 6.877301| 6.901897|
|TBS + phagemid            |         1|3        | 6.847557| 6.853574| 6.869579|
|TBS + phagemid            |         2|1        | 6.788273| 6.803791| 6.811155|
|TBS + phagemid            |         2|2        | 6.839764| 6.845615| 6.872852|
|TBS + phagemid            |         2|3        | 6.838165| 6.838324| 6.846268|
|TBS + phagemid            |         3|1        | 6.719253| 6.735894| 6.743167|
|TBS + phagemid            |         3|2        | 6.727330| 6.733523| 6.760886|
|TBS + phagemid            |         3|3        | 6.682990| 6.705391| 6.709441|
|WW + phagemid             |         0|1        | 6.283210| 6.314316| 6.323252|
|WW + phagemid             |         0|2        | 6.386303| 6.406878| 6.434158|
|WW + phagemid             |         0|3        | 6.523524| 6.671040| 6.692102|
|WW + phagemid             |         1|1        | 6.651537| 6.684856| 6.691471|
|WW + phagemid             |         1|2        | 6.520152| 6.568075| 6.598458|
|WW + phagemid             |         1|3        | 6.494412| 6.505401| 6.558010|
|WW + phagemid             |         2|1        | 6.611078| 6.615849| 6.646168|
|WW + phagemid             |         2|2        | 6.594384| 6.646766| 6.657950|
|WW + phagemid             |         2|3        | 6.459501| 6.461001| 6.491916|
|WW + phagemid             |         3|1        | 6.354589| 6.363023| 6.364132|
|WW + phagemid             |         3|2        | 6.402674| 6.406696| 6.427756|
|WW + phagemid             |         3|3        | 6.533144| 6.557411| 6.558043|
|WW + phagemid + bleach    |         0|1        | 4.787649| 4.800018| 4.822280|
|WW + phagemid + bleach    |         0|2        | 4.699068| 4.711782| 4.803886|
|WW + phagemid + bleach    |         0|3        | 4.641530| 4.839236| 4.869814|
|WW + phagemid + bleach    |         1|1        | 3.120599| 3.140413| 3.146569|
|WW + phagemid + bleach    |         1|2        | 3.147737| 3.160051| 3.182787|
|WW + phagemid + bleach    |         1|3        | 3.422276| 3.494283| 3.521149|
|WW + phagemid + bleach    |         2|1        | 3.097206| 3.135306| 3.164729|
|WW + phagemid + bleach    |         2|2        | 3.374704| 3.393190| 3.401506|
|WW + phagemid + bleach    |         2|3        | 3.322457| 3.332269| 3.405885|
|WW + phagemid + bleach    |         3|1        | 3.213055| 3.281083| 3.388138|
|WW + phagemid + bleach    |         3|2        | 2.670246| 3.001971| 3.080543|
|WW + phagemid + bleach    |         3|3        | 3.373322| 3.426183| 3.436552|
|WW + phagemid + detergent |         0|1        | 6.380370| 6.387304| 6.394484|
|WW + phagemid + detergent |         0|2        | 6.487330| 6.506259| 6.508774|
|WW + phagemid + detergent |         0|3        | 6.353817| 6.415591| 6.464465|
|WW + phagemid + detergent |         1|1        | 6.280246| 6.300750| 6.310677|
|WW + phagemid + detergent |         1|2        | 6.267446| 6.308108| 6.327672|
|WW + phagemid + detergent |         1|3        | 6.243128| 6.404803| 6.442681|
|WW + phagemid + detergent |         2|1        | 6.273809| 6.300213| 6.303632|
|WW + phagemid + detergent |         2|2        | 6.253504| 6.314915| 6.339672|
|WW + phagemid + detergent |         2|3        | 6.440447| 6.464878| 6.490252|
|WW + phagemid + detergent |         3|1        | 5.816943| 5.828693| 5.853964|
|WW + phagemid + detergent |         3|2        | 5.866659| 5.878833| 5.933811|
|WW + phagemid + detergent |         3|3        | 5.231239| 5.287751| 5.290213|

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

The variation between treatments is swamping the variation between replicates, so let's let the y-axis vary:


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
    scales = "free_y"
    )
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)

It looks like there is sometimes significantly more variation between technical replicates than PCR replicates.
This suggests that we may want to use a hierarchical model of the error.

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

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)

It's not really appropriate here since our data is non-linear, but we can also use a linear model:

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

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png)


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



|collapsed |treatment_group           |term        |   estimate| std.error|   statistic|   p.value|
|:---------|:-------------------------|:-----------|----------:|---------:|-----------:|---------:|
|FALSE     |WW + phagemid             |(Intercept) |  6.5175850| 0.0340313| 191.5171920| 0.0000000|
|FALSE     |WW + phagemid             |timepoint   | -0.0032190| 0.0181905|  -0.1769585| 0.8605909|
|FALSE     |WW + phagemid + detergent |(Intercept) |  6.5337324| 0.0663000|  98.5479440| 0.0000000|
|FALSE     |WW + phagemid + detergent |timepoint   | -0.2270561| 0.0354389|  -6.4069822| 0.0000003|
|FALSE     |WW + phagemid + bleach    |(Intercept) |  4.3339485| 0.1243744|  34.8459951| 0.0000000|
|FALSE     |WW + phagemid + bleach    |timepoint   | -0.4669014| 0.0664809|  -7.0230909| 0.0000000|
|FALSE     |TBS + phagemid            |(Intercept) |  6.8619652| 0.0148943| 460.7097711| 0.0000000|
|FALSE     |TBS + phagemid            |timepoint   | -0.0336937| 0.0079614|  -4.2321566| 0.0001657|

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

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21-1.png)


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

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22-1.png)

We can fit linear models separately for each timepoint and examine the coefficients and standard errors:


```r
models_collapsed <- fit_lm_by_treatment(data_collapsed) |>
    mutate(collapsed=TRUE, .before=1)
models_all <- rbind(models, models_collapsed)
kable(models_all)
```



|collapsed |treatment_group           |term        |   estimate| std.error|   statistic|   p.value|
|:---------|:-------------------------|:-----------|----------:|---------:|-----------:|---------:|
|FALSE     |WW + phagemid             |(Intercept) |  6.5175850| 0.0340313| 191.5171920| 0.0000000|
|FALSE     |WW + phagemid             |timepoint   | -0.0032190| 0.0181905|  -0.1769585| 0.8605909|
|FALSE     |WW + phagemid + detergent |(Intercept) |  6.5337324| 0.0663000|  98.5479440| 0.0000000|
|FALSE     |WW + phagemid + detergent |timepoint   | -0.2270561| 0.0354389|  -6.4069822| 0.0000003|
|FALSE     |WW + phagemid + bleach    |(Intercept) |  4.3339485| 0.1243744|  34.8459951| 0.0000000|
|FALSE     |WW + phagemid + bleach    |timepoint   | -0.4669014| 0.0664809|  -7.0230909| 0.0000000|
|FALSE     |TBS + phagemid            |(Intercept) |  6.8619652| 0.0148943| 460.7097711| 0.0000000|
|FALSE     |TBS + phagemid            |timepoint   | -0.0336937| 0.0079614|  -4.2321566| 0.0001657|
|TRUE      |TBS + phagemid            |(Intercept) |  6.8619652| 0.0210402| 326.1366660| 0.0000000|
|TRUE      |TBS + phagemid            |timepoint   | -0.0336937| 0.0112464|  -2.9959457| 0.0134364|
|TRUE      |WW + phagemid             |(Intercept) |  6.5175850| 0.0608776| 107.0604957| 0.0000000|
|TRUE      |WW + phagemid             |timepoint   | -0.0032190| 0.0325404|  -0.0989220| 0.9231551|
|TRUE      |WW + phagemid + bleach    |(Intercept) |  4.3339485| 0.2265427|  19.1308196| 0.0000000|
|TRUE      |WW + phagemid + bleach    |timepoint   | -0.4669014| 0.1210922|  -3.8557511| 0.0031822|
|TRUE      |WW + phagemid + detergent |(Intercept) |  6.5337324| 0.1208895|  54.0471506| 0.0000000|
|TRUE      |WW + phagemid + detergent |timepoint   | -0.2270561| 0.0646181|  -3.5138139| 0.0055959|

Collapsing the qPCR replicates increases the standard error of the regression coefficients:

```r
models_all |>
  filter(term == "timepoint") |>
  ggplot(aes(collapsed, std.error, group=treatment_group)) +
  geom_line(aes(linetype = treatment_group)) +
  geom_point()
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24-1.png)

# Hierarchical model with error propagation [TODO]
