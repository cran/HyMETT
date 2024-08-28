# HyMETT Function Reference

-   [benchmark\_KGE\_DOY](#benchmark_kge_doy)
-   [calc\_annual\_flow\_stats](#calc_annual_flow_stats)
-   [calc\_annual\_stat\_trend](#calc_annual_stat_trend)
-   [calc\_logistic\_regression](#calc_logistic_regression)
-   [calc\_qlpearsonIII](#calc_qlpearsoniii)
-   [calc\_qpearsonIII](#calc_qpearsoniii)
-   [censor\_values](#censor_values)
-   [example\_annual](#example_annual)
-   [example\_mod](#example_mod)
-   [example\_mod\_zf](#example_mod_zf)
-   [example\_obs](#example_obs)
-   [example\_obs\_zf](#example_obs_zf)
-   [example\_preproc](#example_preproc)
-   [GOF\_correlation\_tests](#gof_correlation_tests)
-   [GOF\_kling\_gupta\_efficiency](#gof_kling_gupta_efficiency)
-   [GOF\_mean\_absolute\_error](#gof_mean_absolute_error)
-   [GOF\_mean\_error](#gof_mean_error)
-   [GOF\_nash\_sutcliffe\_efficiency](#gof_nash_sutcliffe_efficiency)
-   [GOF\_percent\_bias](#gof_percent_bias)
-   [GOF\_rmse](#gof_rmse)
-   [GOF\_summary](#gof_summary)
-   [GOF\_volumetric\_efficiency](#gof_volumetric_efficiency)
-   [HyMETT-package](#hymett-package)
-   [POR\_apply\_annual\_hiflow\_stats](#por_apply_annual_hiflow_stats)
-   [POR\_apply\_annual\_lowflow\_stats](#por_apply_annual_lowflow_stats)
-   [POR\_calc\_amp\_and\_phase](#por_calc_amp_and_phase)
-   [POR\_calc\_AR1](#por_calc_ar1)
-   [POR\_calc\_lp3\_quantile](#por_calc_lp3_quantile)
-   [POR\_deseasonalize](#por_deseasonalize)
-   [POR\_distribution\_metrics](#por_distribution_metrics)
-   [preproc\_audit\_data](#preproc_audit_data)
-   [preproc\_fill\_daily](#preproc_fill_daily)
-   [preproc\_main](#preproc_main)
-   [preproc\_precondition\_data](#preproc_precondition_data)
-   [preproc\_validate\_daily](#preproc_validate_daily)

# DESCRIPTION

    Package: HyMETT
    Type: Package
    Title: Hydrologic Model Evaluation and Time-Series Tools
    Version: 1.1.2
    Date: 2023-11-21
    Authors@R: c(
        person(family = "Penn",
               given = "Colin",
               role = c("aut","cre"),
               email = "cpenn@usgs.gov",
               comment = c(ORCID = "0000-0002-5195-2744")),
        person(family = "Simeone",
               given = "Caelan",
               role = c("aut"),
               email = "csimeone@usgs.gov",
               comment = c(ORCID = "0000-0003-3263-6452")),
        person(family = "Levin",
               given = "Sara",
               role = c("aut"),
               email = "slevin@usgs.gov",
               comment = c(ORCID = "0000-0002-2448-3129")),
        person(family = "Saxe",
               given = "Samuel",
               role = c("aut"),
               email = "ssaxe@usgs.gov",
               comment = c(ORCID = "0000-0003-1151-8908")),
        person(family = "Foks",
               given = "Sydney",
               role = c("aut"),
               email = "sfoks@usgs.gov",
               comment = c(ORCID = "0000-0002-7668-9735")),
        person(family = "Dudley",
               given = "Robert",
               role = c("dtc"),
               email = "rwdudley@usgs.gov",
               comment = c(ORCID = "0000-0002-0934-0568")),
        person(family = "Hodgkins",
               given = "Glenn",
               role = c("dtc"),
               email = "gahodgki@usgs.gov",
               comment = c(ORCID = "0000-0002-4916-5565")),
        person(family = "Hodson",
               given = "Timothy",
               role = c("aut"),
               email = "thodson@usgs.gov",
               comment = c(ORCID = "0000-0003-0962-5130")),
        person(family = "Over",
               given = "Thomas",
               role = c("dtc"),
               email = "@usgs.gov",
               comment = c(ORCID = "0000-0001-8280-4368")),
        person(family = "Russell",
               given = "Amy",
               role = c("dtc"),
               email = "arussell@usgs.gov",
               comment = c(ORCID = "0000-0003-0582-0094")))
    Description: Facilitates the analysis and evaluation of hydrologic model output and 
        time-series data with functions focused on comparison of modeled (simulated) and observed data, 
        period-of-record statistics, and trends.  
    URL: https://code.usgs.gov/hymett/hymett
    BugReports: https://code.usgs.gov/hymett/hymett/-/issues
    Depends: R (>= 3.6.0)
    Imports:
        checkmate,
        dplyr,
        EnvStats,
        lmomco,
        lubridate,
        plyr,
        rlang,
        stats,
        tibble,
        zoo
    Suggests:
        knitr,
        rmarkdown,
        roxygen2,
        testthat
    License: CC0
    LazyLoad: yes
    LazyData: yes
    VignetteBuilder: knitr
    BuildVignettes: true
    Copyright: This software is in the public domain because it contains materials
        that originally came from the U.S. Geological Survey, an agency of
        the U.S. Department of Interior. For more information, see the
        official USGS copyright policy at
        http://www.usgs.gov/visual-id/credit_usgs.html#copyright
    Encoding: UTF-8
    Roxygen: list(markdown = TRUE)
    RoxygenNote: 7.2.3
    NeedsCompilation: no

# `benchmark_KGE_DOY`

## Calculate benchmark Kling–Gupta efficiency (KGE) values from day-of-year (DOY) observations

### Description

Calculate benchmark Kling–Gupta efficiency (KGE) values from daily
observed time-series data

### Usage

    benchmark_KGE_DOY(obs_preproc)

### Arguments

<table>
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code>obs_preproc</code></td>
<td>'data.frame' of daily observational data, preprocessed as output
from<br />
<code>preproc_precondition_data</code> or <code>preproc_main</code>
<code>"daily"</code>.</td>
</tr>
</tbody>
</table>

### Details

This function calculates a "benchmark" KGE value (see Knoben and others,
2020) from a daily observed data time-series. First, the interannual
mean and median is calculated for each day of the calendar year. Next,
the interannual mean and median values are joined to each corresponding
day in the observation time series. Finally, a KGE value
(`GOF_kling_gupta_efficiency`) is calculated comparing the mean or
median value repeated time series to the daily observational time
series. These benchmark KGE values can be used as comparisons for
modeled (simulated) calibration results.

### Value

A data.frame with columns `"KGE_DOY_mean"` and `"KGE_DOY_median"`.

### References

Knoben, W.J.M, Freer, J.E., Peel, M.C., Fowler, K.J.A, Woods, R.A.,
2020. A Brief Analysis of Conceptual Model Structure Uncertainty Using
36 Models and 559 Catchments: Water Resources Research, v. 56.  
\[Also available at <https://doi.org/10.1029/2019WR025975>.\]

### Examples

    benchmark_KGE_DOY(obs_preproc = example_preproc)

# `calc_annual_flow_stats`

## Calculate annual flow statistics from daily data

### Description

Calculate annual flow statistics from daily data

### Usage

    calc_annual_flow_stats(
      data = NULL,
      Date,
      year_group,
      Q,
      Q3 = NA_real_,
      Q7 = NA_real_,
      Q30 = NA_real_,
      jd = NA_integer_,
      calc_high = FALSE,
      calc_low = FALSE,
      calc_percentiles = FALSE,
      calc_monthly = FALSE,
      calc_WSCVD = FALSE,
      longitude = NA,
      calc_ICVD = FALSE,
      zero_threshold = 33,
      quantile_type = 8,
      na.action = c("na.omit", "na.pass")
    )

### Arguments

<table>
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code>data</code></td>
<td>'data.frame'. Optional data.frame input, with columns containing
<code>Date</code>,<br />
<code>year_group</code>, <code>Q</code>, and <code
style="white-space: pre;">⁠Q3, Q7, Q3 0, jd⁠</code> (if required). Column
names are specified as strings in the corresponding parameter. Default
is <code>NULL</code>.</td>
</tr>
<tr class="even">
<td><code>Date</code></td>
<td>'Date' or 'character' vector when <code>data = NULL</code>, or
character' string identifying Date column name when <code>data</code> is
specified. Date associated with each value in <code>Q</code>
parameter.</td>
</tr>
<tr class="odd">
<td><code>year_group</code></td>
<td>'numeric' vector when <code>data = NULL</code>, or 'character'
string identifying grouping column name when <code>data</code> is
specified. Year grouping for each daily value in <code>Q</code>
parameter. Must be same length as <code>Q</code> parameter. Often
<code>year_group</code> is water year or climate year.</td>
</tr>
<tr class="even">
<td><code>Q</code></td>
<td>'numeric' vector when <code>data = NULL</code>, or 'character'
string identifying streamflow values column name when <code>data</code>
is specified. Daily streamflow data. Must be same length as
<code>year_group</code>.</td>
</tr>
<tr class="odd">
<td><code>Q3</code></td>
<td>'numeric' vector when <code>data = NULL</code>, or 'character'
string identifying Q3 column name when <code>data</code> is specified.
3-day moving average of daily streamflow data <code>Q</code> parameter,
often returned from <code>preproc_precondition_data</code>. Default is
<code>NA_real_</code>, required if <code>calc_high</code> or
<code>calc_low = TRUE</code>. If specified, must be same length as
<code>Q</code> parameter.</td>
</tr>
<tr class="even">
<td><code>Q7</code></td>
<td>'numeric' vector when <code>data = NULL</code>, or 'character'
string identifying Q7 column name when <code>data</code> is specified.
7-day moving average of daily streamflow data <code>Q</code> parameter,
often returned from <code>preproc_precondition_data</code>. Default is
<code>NA_real_</code>, required if <code>calc_high</code> or
<code>calc_low = TRUE</code>. If specified, must be same length as
<code>Q</code> parameter.</td>
</tr>
<tr class="odd">
<td><code>Q30</code></td>
<td>'numeric' vector when <code>data = NULL</code>, or 'character'
string identifying Q30 column name when <code>data</code> is specified.
30-day average of daily streamflow data <code>Q</code> parameter, often
returned from <code>preproc_precondition_data</code>. Default is
<code>NA_real_</code>, required if <code>calc_high</code> or
<code>calc_low = TRUE</code>. If specified, must be same length as
<code>Q</code> parameter.</td>
</tr>
<tr class="even">
<td><code>jd</code></td>
<td>'numeric' vector when <code>data = NULL</code>, or 'character'
string identifying jd column name when <code>data</code> is specified.
Calendar Julian day of daily streamflow data <code>Q</code> parameter,
often returned from <code>preproc_precondition_data</code>. Default is
<code>NA_integer_</code>, required if <code>calc_high</code>,
<code>calc_low</code>, <code>calc_WSCVD</code> or
<code>calc_ICVD = TRUE</code>. If specified, must be same length as
<code>Q</code> parameter.</td>
</tr>
<tr class="odd">
<td><code>calc_high</code></td>
<td>'boolean' value. Calculate high flow statistics for years in
<code>year_group</code>. Default is <code>FALSE</code>. See
<strong>Details</strong> for more information.</td>
</tr>
<tr class="even">
<td><code>calc_low</code></td>
<td>'boolean' value. Calculate low flow statistics for years in
<code>year_group</code>. Default is <code>FALSE</code>. See
<strong>Details</strong> for more information.</td>
</tr>
<tr class="odd">
<td><code>calc_percentiles</code></td>
<td>'boolean' value. Calculate percentiles for years in
<code>year_group</code>. Default is <code>FALSE</code>. See
<strong>Details</strong> for more information.</td>
</tr>
<tr class="even">
<td><code>calc_monthly</code></td>
<td>'boolean' value. Calculate monthly statistics for years in
<code>year_group</code>. Default is <code>FALSE</code>. See
<strong>Details</strong> for more information.</td>
</tr>
<tr class="odd">
<td><code>calc_WSCVD</code></td>
<td>'boolean' value. Calculate winter-spring center volume date for
years in <code>year_group</code>. Default is <code>FALSE</code>. See
<strong>Details</strong> for more information.</td>
</tr>
<tr class="even">
<td><code>longitude</code></td>
<td>'numeric' value. Site longitude in North American Datum of 1983
(NAD83), required in WSCVD calculation. Default is <code>NA</code>. See
<strong>Details</strong> for more information.</td>
</tr>
<tr class="odd">
<td><code>calc_ICVD</code></td>
<td>'boolean' value. Calculate inverse center volume date for years in
<code>year_group</code>. Default is <code>FALSE</code>. See
<strong>Details</strong> for more information.</td>
</tr>
<tr class="even">
<td><code>zero_threshold</code></td>
<td>'numeric' value as percentage. The percentage of years of a
statistic that need to be zero in order for it to be deemed a zero flow
site for that statistic. For use in trend calculation. See
<strong>Details</strong> on attributes. Default is <code>33</code> (33
percent) of the annual statistic values.</td>
</tr>
<tr class="odd">
<td><code>quantile_type</code></td>
<td>'numeric' value. The distribution type used in the
<code>stats::quantile</code> function. Default is <code>8</code>
(median-unbiased regardless of distribution). Other types common in
hydrology are <code>6</code> (Weibull) or <code>9</code> (unbiased for
normal distributions).</td>
</tr>
<tr class="even">
<td><code>na.action</code></td>
<td>'character' string indicating na.action passed to
<code>stats::aggregate</code> <code>na.action</code> parameter. Default
is <code>"na.omit"</code>, which removes <code>NA</code> values before
aggregating statistics, or <code>"na.pass"</code>, which will pass
<code>NA</code> values and return <code>NA</code> in the grouped
calculation if any <code>NA</code> values are present.</td>
</tr>
</tbody>
</table>

### Details

`year_group` is commonly water year, climate year, or calendar year.  

Default annual statistics returned:

`annual_mean`  
annual mean in `year_group`

`annual_sd`  
annual standard deviation in `year_group`

`annual_sum`  
annual sum in `year_group`

If `calc_high/low` are selected, annual statistics returned:  
1-, 3-, 7-, and 30-day high/low and Julian date (jd) of n-day high/low.

`high_q`*n*  
where *n* = 1, 3, 7, and 30

`high_q`*n*`⁠_jd⁠`  
where *n* = 1, 3, 7, and 30

`low_q`*n*  
where *n* = 1, 3, 7, and 30

`low_q`*n*`⁠_jd⁠`  
where *n* = 1, 3, 7, and 30

If `calc_percentiles` is selected, annual statistics returned:  
1, 5, 10, 25, 50, 75, 90, 95, 99 percentile based on daily streamflow.

`annual_`*n*`⁠_percentile⁠`  
where *n* = 1, 5, 10, 25, 50, 75, 90, 95, and 99

If `calc_monthly` is selected, annual statistics returned:  
Monthly mean, standard deviation, max, min, percent of annual for each
month in `year_group`.

*month*`⁠_mean⁠`  
monthly mean, where *month* = `month.abb`

*month*`⁠_sd⁠`  
monthly standard deviation, where *month* = `month.abb`

*month*`⁠_max⁠`  
monthly maximum, where *month* = `month.abb`

*month*`⁠_min⁠`  
monthly minimum, where *month* = `month.abb`

*month*`⁠_percent_annual⁠`  
monthly percent of annual, where *month* = `month.abb`

If `calc_WSCVD` is selected, Julian date of annual winter-spring center
volume date is returned.  
Longitude (in NAD83 datum) is used to determine the ending month of
spring. July for longitudes West of `-`95 degrees, May for longitudes
east of `-`95 degrees. See **References** Dudley and others, 2017.
Commonly calculated when `year_group` is water year.

`WSCVD`  
Julian date of winter-spring center volume

If `calc_ICVD` is selected, Julian date of annual inverse center volume
date is returned.  
Commonly calculated when `year_group` is climate year.

`ICVD`  
Julian date of inverse center volume date

**Attribute:** `zero_flow_years`  
A data.frame with each annual statistic calculated, the percentage of
years where the statistic = 0, a flag indicating if the percentage is
over the `zero_threshold` parameter, and the number of years with a zero
value. Columns in `zero_flow_years`:

`annual_stat`  
annual statistic

`percent_zeros`  
percentage of years with 0 statistic value

`over_threshold`  
boolean if percentage is over threshold

`number_years`  
number of years with 0 value statistic

The `zero_flow_years` attribute can be useful in trend calculation,
where a trend may not be appropriate to calculate with many zero flow
years.

### Value

A tibble (see `tibble::tibble`) with annual statistics depending on
options selected. See **Details**.

### References

Dudley, R.W., Hodgkins, G.A, McHale, M.R., Kolian, M.J., Renard, B.,
2017, Trends in snowmelt-related streamflow timing in the conterminous
United States: Journal of Hydrology, v. 547, p. 208-221. \[Also
available at <https://doi.org/10.1016/j.jhydrol.2017.01.051>.\]

### See Also

`preproc_precondition_data`

### Examples

    calc_annual_flow_stats(data = example_preproc, Date = "Date", year_group = "WY", Q = "value")

# `calc_annual_stat_trend`

## Calculate trend in annual statistics

### Description

Calculate trend in annual statistics

### Usage

    calc_annual_stat_trend(data = NULL, year, value, ...)

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>data</code></td>
<td style="text-align: left;">'data.frame'. Optional data.frame input,
with columns containing <code>year</code> and <code>value</code>. Column
names are specified as strings in the corresponding parameter. Default
is <code>NULL</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>year</code></td>
<td style="text-align: left;">'numeric' vector when
<code>data = NULL</code>, or 'character' string identifying year column
name when <code>data</code> is specified. Year of each value in
<code>value</code> parameter.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>value</code></td>
<td style="text-align: left;">'numeric' vector when
<code>data = NULL</code>, or 'character' string identifying value column
name when <code>data</code> is specified. Values to calculate trend
on.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>...</code></td>
<td style="text-align: left;">further arguments to be passed to or from
<code>EnvStats::kendallTrendTest</code>.</td>
</tr>
</tbody>
</table>

### Details

This function is a wrapper for `EnvStats::kendallTrendTest` with the
passed equation `value ~ year`. The returned values include Mann-Kendall
test statistic and p-value, Theil-Sen slope and intercept values, and
trend details (Millard, 2013; Helsel and others, 2020).

`z_stat`  
Mann-Kendall test statistic, returned directly from
`EnvStats::kendallTrendTest`

`p_value`  
`z_stat` p-value, returned directly from `EnvStats::kendallTrendTest`

`sen_slope`  
Sen slope in units value per year, returned directly from
`EnvStats::kendallTrendTest`

`intercept`  
Sen slope intercept, returned directly from `EnvStats::kendallTrendTest`

`trend_mag`  
Trend magnitude over entire period, in units of `value`, calculated as
`⁠sen_slope * (max(year)⁠` `-` `⁠min(year))⁠`

`val_beg/end`  
Calculated value at beginning or end of period, calculated as
`sen_slope * year + intercept`

`val_perc_change`  
Percentage change over period, calculated as
`(val_end - val_beg) / val_beg * 100`

### Value

A tibble (see `tibble::tibble`) with test statistic, p-value, trend
coefficients, and trend calculations. See **Details**.

### References

Millard, S.P., 2013, EnvStats: An R Package for Environmental
Statistics: New York, New York, Springer, 291 p. \[Also available at
<https://doi.org/10.1007/978-1-4614-8456-1>.\]

Helsel, D.R., Hirsch, R.M., Ryberg, K.R., Archfield, S.A., and Gilroy,
E.J., 2020, Statistical methods in water resources: U.S. Geological
Survey Techniques and Methods, book 4, chap. A3, 458 p. \[Also available
at <https://doi.org/10.3133/tm4a3>.\]

### See Also

`kendallTrendTest`

### Examples

    calc_annual_stat_trend(data = example_annual, year = "WY", value = "annual_mean")

# `calc_logistic_regression`

## Calculate logistic regression in annual statistics with zero values

### Description

Calculate logistic regression (Everitt and Hothorn, 2009) in annual
statistics with zero values. A model fit to compute the probability of a
zero flow annual statistic.

### Usage

    calc_logistic_regression(data = NULL, year, value, ...)

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>data</code></td>
<td style="text-align: left;">'data.frame'. Optional data.frame input,
with columns containing <code>year</code> and <code>value</code>. Column
names are specified as strings in the corresponding parameter. Default
is <code>NULL</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>year</code></td>
<td style="text-align: left;">'numeric' vector when
<code>data = NULL</code>, or 'character' string identifying year column
name when <code>data</code> is specified. Year of each value in
<code>value</code> parameter.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>value</code></td>
<td style="text-align: left;">'numeric' vector when
<code>data = NULL</code>, or 'character' string identifying value column
name when <code>data</code> is specified. Values to calculate logistic
regression on.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>...</code></td>
<td style="text-align: left;">further arguments to be passed to or from
<code>stats::glm</code>.</td>
</tr>
</tbody>
</table>

### Details

This function is a wrapper for
`⁠stats::glm(y ~ year, family = stats::binomial(link="logit")⁠` with
`y = 1` when `value = 0` (for example a zero flow annual statistic) and
`y = 0` otherwise. The returned values include

`p_value`  
Probability value of the explanatory (`year`) variable in the logistic
model

`stdErr_slope`  
Standard error of the regression slope (log odds per year)

`odds_ratio`  
Exponential of the explanatory coefficient (year coefficient)

`prob_beg/end`  
Logistic regression predicted (fitted) values at the beginning and
ending year.

`prob_change`  
Change in probability from beginning to end.

Example, an odds ratio of 1.05 represents the odds of a zero-flow year
(versus non-zero) increase by a factor of 1.05 (or 5 percent).

### Value

A tibble (see `tibble::tibble`) with logistic regression p-value,
standard error of slope, odds ratio, beginning and ending probability,
and probability change. See **Details**.

### References

Everitt, B. S. and Hothorn T., 2009, A Handbook of Statistical Analyses
Using R, 2nd Ed. Boca Raton, Florida, Chapman and Hall/CRC, 376p.

### See Also

`glm`

### Examples

    calc_logistic_regression(data = example_annual, year = "WY", value = "annual_mean")

# `calc_qlpearsonIII`

## Quantile of Pearson Type III distribution for log-transformed data

### Description

Quantile of Pearson Type III distribution for log-transformed data

### Usage

    calc_qlpearsonIII(p, meanlog = 0, sdlog = 1, skew = 0)

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>p</code></td>
<td style="text-align: left;">Vector of non-exceedance probabilities,
between 0 and 1, to calculate quantiles.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>meanlog</code></td>
<td style="text-align: left;">Vector of mean of the distribution of the
log-transformed data.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>sdlog</code></td>
<td style="text-align: left;">Vector of standard deviation of the
distribution of the log-transformed data.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>skew</code></td>
<td style="text-align: left;">Vector of skewness of the distribution of
the log-transformed data.</td>
</tr>
</tbody>
</table>

### Details

`calc_qpearsonIII` and `calc_qlpearsonIII` are functions to fit a
log-Pearson type III distribution from a given mean, standard deviation,
and skew. This source code is replicated, unchanged, from the `swmrBase`
package in order to reduce the dependency on that package.

### Value

Quantiles for the described distribution

### References

Asquith, W.H., Kiang, J.E., and Cohn, T.A., 2017, Application of at-site
peak-streamflow frequency analyses for very low annual exceedance
probabilities: U.S. Geological Survey Scientific Investigation Report
2017–5038, 93 p. \[Also available at
<https://doi.org/10.3133/sir20175038>.\]

Lorenz, D.L., 2015, smwrBase—An R package for managing hydrologic data,
version 1.1.1: U.S. Geological Survey Open-File Report 2015–1202, 7 p.  
\[Also available at <https://doi.org/10.3133/ofr20151202>.\]

### See Also

`calc_qpearsonIII`

### Examples

    calc_qlpearsonIII(0.1)

# `calc_qpearsonIII`

## Quantile of Pearson Type III distribution

### Description

Quantile of Pearson Type III distribution

### Usage

    calc_qpearsonIII(p, mean = 0, sd = 1, skew = 0)

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>p</code></td>
<td style="text-align: left;">Vector of non-exceedance probabilities,
between 0 and 1, to calculate quantiles.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>mean</code></td>
<td style="text-align: left;">Vector of means of the distribution of the
data.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>sd</code></td>
<td style="text-align: left;">Vector of standard deviation of the
distribution of the data.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>skew</code></td>
<td style="text-align: left;">Vector of skewness of the distribution of
the data.</td>
</tr>
</tbody>
</table>

### Details

`calc_qpearsonIII` and `calc_qlpearsonIII` are functions to fit a
log-Pearson type III distribution from a given mean, standard deviation,
and skew. This source code is replicated, unchanged, from the `swmrBase`
package in order to reduce the dependency on that package.

### Value

Quantiles for the described distribution

### References

Asquith, W.H., Kiang, J.E., and Cohn, T.A., 2017, Application of at-site
peak-streamflow frequency analyses for very low annual exceedance
probabilities: U.S. Geological Survey Scientific Investigation Report
2017–5038, 93 p. \[Also available at
<https://doi.org/10.3133/sir20175038>.\]

Lorenz, D.L., 2015, smwrBase—An R package for managing hydrologic data,
version 1.1.1: U.S. Geological Survey Open-File Report 2015–1202, 7 p.  
\[Also available at <https://doi.org/10.3133/ofr20151202>.\]

### Examples

    calc_qpearsonIII(0.1)

# `censor_values`

## Censor values above or below a threshold

### Description

Replaces values in a vector with `NA` when above or below a censor
level.  
Censoring is `⁠values censor_symbol censor_threshold⁠` are censored, for
example with the defaults (values lte 0 set to `NA`) all values &lt;= 0
are replaced with `NA`.

### Usage

    censor_values(
      value,
      censor_threshold = 0,
      censor_symbol = c("lte", "lt", "gt", "gte")
    )

### Arguments

<table>
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code>value</code></td>
<td>'numeric' vector. Values to censor.</td>
</tr>
<tr class="even">
<td><code>censor_threshold</code></td>
<td>'numeric' value. Threshold to censor values on. Default is 0.</td>
</tr>
<tr class="odd">
<td><code>censor_symbol</code></td>
<td>'character' string.<br />
Inequality symbol to censor values based on censor_threshold.<br />
Accepted values are <code>"gt"</code> (greater than),<br />
<code>"gte"</code> (greater than or equal to),<br />
<code>"lt"</code> (less than),<br />
or <code>"lte"</code> (less than or equal to).<br />
Default is <code>"lte"</code>.</td>
</tr>
</tbody>
</table>

### Value

'numeric' vector with censored values replaced with `NA`

### Examples

    censor_values(value = seq.int(1, 10, 1), censor_threshold = 5)

# `example_annual`

## Example Annual Observations

### Description

An example dataset with daily observed streamflow processed to annual
water year values.

### Usage

    example_annual

### Format

A data.frame with the following variables:

`WY`  
water year

`annual_mean`  
annual mean

`annual_sd`  
annual standard deviation

`annual_sum`  
annual sum

`high_q1`  
annual maximum of daily mean

`high_q3`  
annual maximum of 3-day mean

`high_q7`  
annual maximum of 7-day mean

`high_q30`  
annual maximum of 30-day mean

`high_q1_jd`  
Julian day of annual maximum of daily mean

`high_q3_jd`  
Julian day of annual maximum of 3-day mean

`high_q7_jd`  
Julian day of annual maximum of 7-day mean

`high_q30_jd`  
Julian day of annual maximum of 30-day mean

`low_q7`  
annual minimum of 7-day mean

`low_q30`  
annual minimum of 30-day mean

`low_q3`  
annual minimum of 3-day mean

`low_q1`  
annual minimum of daily mean

`low_q7_jd`  
Julian day of annual minimum of 7-day mean

`low_q30_jd`  
Julian day of annual minimum of 30-day mean

`low_q3_jd`  
Julian day of annual minimum of 3-day mean

`low_q1_jd`  
Julian day of annual minimum of daily mean

`annual_1_percentile`  
annual first percentile

`annual_5_percentile`  
annual 5th percentile

`annual_10_percentile`  
annual 10th percentile

`annual_25_percentile`  
annual 25th percentile

`annual_50_percentile`  
annual 50th percentile

`annual_75_percentile`  
annual 75th percentile

`annual_90_percentile`  
annual 90th percentile

`annual_95_percentile`  
annual 95th percentile

`annual_99_percentile`  
annual 99th percentile

`Jan_mean`  
annual January mean

`Jan_sd`  
annual January standard deviation

`Jan_max`  
annual January maximum

`Jan_min`  
annual January minimum

`Jan_percent_annual`  
annual January percentage of annual sum

`Feb_mean`  
annual February mean

`Feb_sd`  
annual February standard deviation

`Feb_max`  
annual February maximum

`Feb_min`  
annual February minimum

`Feb_percent_annual`  
annual February percentage of annual sum

`Mar_mean`  
annual March mean

`Mar_sd`  
annual March standard deviation

`Mar_max`  
annual March maximum

`Mar_min`  
annual March minimum

`Mar_percent_annual`  
annual March percentage of annual sum

`Apr_mean`  
annual April mean

`Apr_sd`  
annual April standard deviation

`Apr_max`  
annual April maximum

`Apr_min`  
annual April minimum

`Apr_percent_annual`  
annual April percentage of annual sum

`May_mean`  
annual May mean

`May_sd`  
annual May standard deviation

`May_max`  
annual May maximum

`May_min`  
annual May minimum

`May_percent_annual`  
annual May percentage of annual sum

`Jun_mean`  
annual June mean

`Jun_sd`  
annual June standard deviation

`Jun_max`  
annual June maximum

`Jun_min`  
annual June minimum

`Jun_percent_annual`  
annual June percentage of annual sum

`Jul_mean`  
annual July mean

`Jul_sd`  
annual July standard deviation

`Jul_max`  
annual July maximum

`Jul_min`  
annual July minimum

`Jul_percent_annual`  
annual July percentage of annual sum

`Aug_mean`  
annual August mean

`Aug_sd`  
annual August standard deviation

`Aug_max`  
annual August maximum

`Aug_min`  
annual August minimum

`Aug_percent_annual`  
annual August percentage of annual sum

`Sep_mean`  
annual September mean

`Sep_sd`  
annual September standard deviation

`Sep_max`  
annual September maximum

`Sep_min`  
annual September minimum

`Sep_percent_annual`  
annual September percentage of annual sum

`Oct_mean`  
annual October mean

`Oct_sd`  
annual October standard deviation

`Oct_max`  
annual October maximum

`Oct_min`  
annual October minimum

`Oct_percent_annual`  
annual October percentage of annual sum

`Nov_mean`  
annual November mean

`Nov_sd`  
annual November standard deviation

`Nov_max`  
annual November maximum

`Nov_min`  
annual November minimum

`Nov_percent_annual`  
annual November percentage of annual sum

`Dec_mean`  
annual December mean

`Dec_sd`  
annual December standard deviation

`Dec_max`  
annual December maximum

`Dec_min`  
annual December minimum

`Dec_percent_annual`  
annual December percentage of annual sum

`WSV`  
winter-spring volume

`wscvd`  
Julian date of winter-spring center volume

### Details

Generated with `example_obs` from

    HyMETT::preproc_main(data = example_obs, 
                         Date = "Date", value = "streamflow_cfs", longitude = -68)$annual

### See Also

`example_obs`, `preproc_main`

### Examples

    str(example_annual)

# `example_mod`

## Example Model Output

### Description

An example dataset with daily modeled (simulated) streamflow.

### Usage

    example_mod

### Format

A data.frame with the following variables:

`date`  
date as 'character' column class.

`streamflow_cfs`  
modeled streamflow in units of feet^3/second.

`Date`  
date as 'Date' column class.

### Details

Generated from example data available at
`system.file("extdata", "01013500_MOD.csv", package = "HyMETT")`

### References

Johnson, M., D. Blodgett, 2020, NOAA National Water Model Reanalysis
Data at RENCI, HydroShare, accessed September 17, 2020 at  
<https://doi.org/10.4211/hs.89b0952512dd4b378dc5be8d2093310f>

Johnson, M., 2021, nwmHistoric: National Water Model Historic Data. R
package version 0.0.0.9000, accessed September 17, 2020 at
<https://github.com/mikejohnson51/nwmHistoric>

### Examples

    str(example_mod)

# `example_mod_zf`

## Example Model Output with zero flows

### Description

An example dataset with daily modeled (simulated) streamflow that
includes zero flows.

### Usage

    example_mod_zf

### Format

A data.frame with the following variables:

`date`  
date as 'character' column class.

`streamflow_cfs`  
modeled streamflow in units of feet^3/second.

`Date`  
date as 'Date' column class.

### Details

Generated from example data available at
`system.file("extdata", "08202700_MOD.csv", package = "HyMETT")`

### References

Johnson, M., D. Blodgett, 2020, NOAA National Water Model Reanalysis
Data at RENCI, HydroShare, accessed September 17, 2020 at  
<https://doi.org/10.4211/hs.89b0952512dd4b378dc5be8d2093310f>

Johnson, M., 2021, nwmHistoric: National Water Model Historic Data. R
package version 0.0.0.9000, accessed September 17, 2020 at
<https://github.com/mikejohnson51/nwmHistoric>

### Examples

    str(example_mod_zf)

# `example_obs`

## Example Observations

### Description

An example dataset with daily observed streamflow.

### Usage

    example_obs

### Format

A data.frame with the following variables:

`date`  
date as 'character' column class.

`streamflow_cfs`  
observed streamflow in units of feet^3/second.

`quality_cd`  
qualifier for value in `streamflow_cfs` (U.S. Geological Survey, 2020b)

`Date`  
date as 'Date' column class.

### Details

Generated from example data available at
`system.file("extdata", "01013500_OBS.csv", package = "HyMETT")`

### References

De Cicco, L.A., Hirsch, R.M., Lorenz, D., and Watkins, W.D., 2021,
dataRetrieval: R packages for discovering and retrieving water data
available from Federal hydrologic web services, accessed September 16,
2020 at <https://doi.org/10.5066/P9X4L3GE>.

U.S. Geological Survey, 2020a, USGS water data for the Nation: U.S.
Geological Survey National Water Information System database, accessed
September 16, 2020, at  
<https://doi.org/10.5066/F7P55KJN>.

U.S. Geological Survey, 2020b, Instantaneous and Daily Data-Value
Qualification Codes, in USGS water data for the Nation: U.S. Geological
Survey National Water Information System database, accessed September
16, 2020, at <https://doi.org/10.5066/F7P55KJN>. \[information directly
accessible at
<https://help.waterdata.usgs.gov/codes-and-parameters/instantaneous-value-qualification-code-uv_rmk_cd>.\]

### Examples

    str(example_obs)

# `example_obs_zf`

## Example Observations with zero flows

### Description

An example dataset with daily observed streamflow that includes zero
flows.

### Usage

    example_obs_zf

### Format

A data.frame with the following variables:

`date`  
date as 'character' column class.

`streamflow_cfs`  
observed streamflow in units of feet^3/second.

`quality_cd`  
qualifier for value in `streamflow_cfs` (U.S. Geological Survey, 2020b)

`Date`  
date as 'Date' column class.

### Details

Generated from example data available at
`system.file("extdata", "08202700_OBS.csv", package = "HyMETT")`

### References

De Cicco, L.A., Hirsch, R.M., Lorenz, D., and Watkins, W.D., 2021,
dataRetrieval: R packages for discovering and retrieving water data
available from Federal hydrologic web services, accessed September 16,
2020 at <https://doi.org/10.5066/P9X4L3GE>.

U.S. Geological Survey, 2020a, USGS water data for the Nation: U.S.
Geological Survey National Water Information System database, accessed
September 16, 2020, at  
<https://doi.org/10.5066/F7P55KJN>.

U.S. Geological Survey, 2020b, Instantaneous and Daily Data-Value
Qualification Codes, in USGS water data for the Nation: U.S. Geological
Survey National Water Information System database, accessed September
16, 2020, at <https://doi.org/10.5066/F7P55KJN>. \[information directly
accessible at
<https://help.waterdata.usgs.gov/codes-and-parameters/instantaneous-value-qualification-code-uv_rmk_cd>.\]

### Examples

    str(example_obs_zf)

# `example_preproc`

## Example Observations prepocessed

### Description

An example dataset with daily observed streamflow preprocessed to
include additional timing and n-day moving averages.

### Usage

    example_preproc

### Format

A data.frame with the following variables:

`Date`  

`value`  

`year`  

`month`  

`day`  

`decimal_date`  

`WY`  
Water Year: October 1 - September 30

`CY`  
Climate Year: April 1 - March 30

`Q3`  
3-Day Moving Average: computed at end of moving interval

`Q7`  
7-Day Moving Average: computed at end of moving interval

`Q30`  
30-Day Moving Average: computed at end of moving interval

`jd`  
Julian date

### Details

Generated with `example_obs` from

    HyMETT::preproc_main(data = example_obs, 
                         Date = "Date", value = "streamflow_cfs", longitude = -68)$daily`

### See Also

`example_obs`, `preproc_main`

### Examples

    str(example_preproc)

# `GOF_correlation_tests`

## Calculates Kendall's Tau, Spearman's Rho, Pearson Correlation

### Description

Calculates Kendall's Tau, Spearman's Rho, Pearson Correlation, and
p-values as a wrapper to the `stats::cor.test` function. Output is
tidy-style data.frame.

### Usage

    GOF_correlation_tests(mod, obs, na.rm = TRUE, ...)

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>mod</code></td>
<td style="text-align: left;">'numeric' vector. Modeled or simulated
values. Must be same length as <code>obs</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>obs</code></td>
<td style="text-align: left;">'numeric' vector. Observed or comparison
values. Must be same length as <code>mod</code>.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>na.rm</code></td>
<td style="text-align: left;">'boolean' <code>TRUE</code> or
<code>FALSE</code>. Should <code>NA</code> values be removed before
computing. If any <code>NA</code> values are present in <code>mod</code>
or <code>obs</code>, the <em>i</em>th position from each will be removed
before calculating. If <code>NA</code> values are present and
<code>na.rm = FALSE</code>, then function will return <code>NA</code>.
Default is <code>TRUE</code></td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>...</code></td>
<td style="text-align: left;">Further arguments to be passed to or from
<code>stats::cor.test</code>.</td>
</tr>
</tbody>
</table>

### Details

See `stats::cor.test` for more details and further arguments to be
passed to or from methods. Defaults are used.

### Value

A tibble (`tibble::tibble`) with test statistic values and p-values.

### See Also

`cor.test`

### Examples

    GOF_correlation_tests(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)

# `GOF_kling_gupta_efficiency`

## Calculate Kling–Gupta Efficiency (KGE)

### Description

Calculate Kling–Gupta Efficiency (KGE) (or modified KGE ('KGE)) between
modeled (simulated) and observed values.

### Usage

    GOF_kling_gupta_efficiency(mod, obs, modified = FALSE, na.rm = TRUE)

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>mod</code></td>
<td style="text-align: left;">'numeric' vector. Modeled or simulated
values. Must be same length as <code>obs</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>obs</code></td>
<td style="text-align: left;">'numeric' vector. Observed or comparison
values. Must be same length as <code>mod</code>.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>modified</code></td>
<td style="text-align: left;">'boolean' <code>TRUE</code> or
<code>FALSE</code>. Should the KGE calculation use the original
variability ratio in the standard deviations (see Gupta and others,
2009) (<code>modified = FALSE</code>) or the modified variability ratio
in the coefficient of variations (see Kling and others, 2012)
(<code>modified = TRUE</code>). Default is <code>FALSE</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>na.rm</code></td>
<td style="text-align: left;">'boolean' <code>TRUE</code> or
<code>FALSE</code>. Should <code>NA</code> values be removed before
computing. If any <code>NA</code> values are present in <code>mod</code>
or <code>obs</code>, the <em>i</em>th position from each will be removed
before calculating. If <code>NA</code> values are present and
<code>na.rm = FALSE</code>, then function will return <code>NA</code>.
Default is <code>TRUE</code>.</td>
</tr>
</tbody>
</table>

### Value

Value of computed KGE or 'KGE.

### References

Kling, H., Fuchs, M. and Paulin, M., 2012. Runoff conditions in the
upper Danube basin under an ensemble of climate change scenarios:
Journal of Hydrology, v. 424-425, p. 264-277.  
\[Also available at <https://doi.org/10.1016/j.jhydrol.2012.01.011>.\]

Gupta, H.V., Kling, H., Yilmaz, K.K., and Martinez, G.G., 2009.
Decomposition of the mean squared error and NSE performance criteria:
Implications for improving hydrological modelling: Journal of Hydrology,
v. 377, no.1-2, p. 80-91.  
\[Also available at <https://doi.org/10.1016/j.jhydrol.2009.08.003>.\]

### Examples

    GOF_kling_gupta_efficiency(
      mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs
    )

# `GOF_mean_absolute_error`

## Calculates mean absolute error (MAE).

### Description

Calculates mean absolute error (MAE) between modeled (simulated) and
observed values. Error is defined as modeled minus observed.

### Usage

    GOF_mean_absolute_error(mod, obs, na.rm = TRUE)

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>mod</code></td>
<td style="text-align: left;">'numeric' vector. Modeled or simulated
values. Must be same length as <code>obs</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>obs</code></td>
<td style="text-align: left;">'numeric' vector. Observed or comparison
values. Must be same length as <code>mod</code>.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>na.rm</code></td>
<td style="text-align: left;">'boolean' <code>TRUE</code> or
<code>FALSE</code>. Should <code>NA</code> values be removed before
computing. If any <code>NA</code> values are present in <code>mod</code>
or <code>obs</code>, the <em>i</em>th position from each will be removed
before calculating. If <code>NA</code> values are present and
<code>na.rm = FALSE</code>, then function will return <code>NA</code>.
Default is <code>TRUE</code>.</td>
</tr>
</tbody>
</table>

### Details

The absolute value of each modeled-observed pair error is calculated,
then the mean of those values taken. Values returned are in units of
input data.

### Value

Value of calculated mean absolute error (MAE).

### Examples

    GOF_mean_absolute_error(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)

# `GOF_mean_error`

## Calculates mean error.

### Description

Calculates mean error between modeled (simulated) and observed values.
Error is defined as modeled minus observed.

### Usage

    GOF_mean_error(mod, obs, na.rm = TRUE)

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>mod</code></td>
<td style="text-align: left;">'numeric' vector. Modeled or simulated
values. Must be same length as <code>obs</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>obs</code></td>
<td style="text-align: left;">'numeric' vector. Observed or comparison
values. Must be same length as <code>mod</code>.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>na.rm</code></td>
<td style="text-align: left;">'boolean' <code>TRUE</code> or
<code>FALSE</code>. Should <code>NA</code> values be removed before
computing. If any <code>NA</code> values are present in <code>mod</code>
or <code>obs</code>, the <em>i</em>th position from each will be removed
before calculating. If <code>NA</code> values are present and
<code>na.rm = FALSE</code>, then function will return <code>NA</code>.
Default is <code>TRUE</code>.</td>
</tr>
</tbody>
</table>

### Details

Values returned are in units of input data.

### Value

Value of calculated mean error.

### Examples

    GOF_mean_error(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)

# `GOF_nash_sutcliffe_efficiency`

## Calculate Nash–Sutcliffe Efficiency (NSE)

### Description

Calculate Nash–Sutcliffe Efficiency (NSE) (with options for modified
NSE) between modeled (simulated) and observed values.

### Usage

    GOF_nash_sutcliffe_efficiency(mod, obs, j = 2, na.rm = TRUE)

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>mod</code></td>
<td style="text-align: left;">'numeric' vector. Modeled or simulated
values. Must be same length as <code>obs</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>obs</code></td>
<td style="text-align: left;">'numeric' vector. Observed or comparison
values. Must be same length as <code>mod</code>.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>j</code></td>
<td style="text-align: left;">'numeric' value. Exponent value for
modified NSE (mNSE) equation. Default value is <code>j = 2</code>, which
is traditional NSE equation.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>na.rm</code></td>
<td style="text-align: left;">'boolean' <code>TRUE</code> or
<code>FALSE</code>. Should <code>NA</code> values be removed before
computing. If any <code>NA</code> values are present in <code>mod</code>
or <code>obs</code>, the <em>i</em>th position from each will be removed
before calculating. If <code>NA</code> values are present and
<code>na.rm = FALSE</code>, then function will return <code>NA</code>.
Default is <code>TRUE</code>.</td>
</tr>
</tbody>
</table>

### Value

Value of computed NSE or mNSE.

### References

Krause, P., Boyle, D.P., and Base, F., 2005. Comparison of different
efficiency criteria for hydrological model assessment: Advances in
Geosciences, v. 5, p. 89-97.  
\[Also available at <https://doi.org/10.5194/adgeo-5-89-2005>.\]

Legates D.R and McCabe G.J., 1999, Evaluating the use of
"goodness-of-fit" measures in hydrologic and hydroclimatic model
validation: Water Resources Research. v. 35, no. 1, p. 233-241. \[Also
available at <https://doi.org/10.1029/1998WR900018>.\]

Nash, J.E. and Sutcliffe, J.V., 1970, River flow forecasting through
conceptual models part I: A discussion of principles: Journal of
Hydrology, v. 10, no. 3, p. 282-290. \[Also available at
<https://doi.org/10.1016/0022-1694(70)90255-6>.\]

### Examples

    GOF_nash_sutcliffe_efficiency(
      mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs
    )

# `GOF_percent_bias`

## Calculates percent bias.

### Description

Calculates percent bias between modeled (simulated) and observed values.

### Usage

    GOF_percent_bias(mod, obs, na.rm = TRUE)

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>mod</code></td>
<td style="text-align: left;">'numeric' vector. Modeled or simulated
values. Must be same length as <code>obs</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>obs</code></td>
<td style="text-align: left;">'numeric' vector. Observed or comparison
values. Must be same length as <code>mod</code>.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>na.rm</code></td>
<td style="text-align: left;">'boolean' <code>TRUE</code> or
<code>FALSE</code>. Should <code>NA</code> values be removed before
computing. If any <code>NA</code> values are present in <code>mod</code>
or <code>obs</code>, the <em>i</em>th position from each will be removed
before calculating. If <code>NA</code> values are present and
<code>na.rm = FALSE</code>, then function will return <code>NA</code>.
Default is <code>TRUE</code>.</td>
</tr>
</tbody>
</table>

### Details

Values returned are in percent.

### Value

Value of calculated percent bias as percent.

### Examples

    GOF_percent_bias(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)

# `GOF_rmse`

## Calculate root-mean-square error with options to normalize

### Description

Calculate root-mean-square error (RMSE) between modeled (simulated) and
observed values. Error is defined as modeled minus observed.

### Usage

    GOF_rmse(
      mod,
      obs,
      normalize = c("none", "mean", "range", "stdev", "iqr", "iqr-1", "iqr-2", "iqr-3",
        "iqr-4", "iqr-5", "iqr-6", "iqr-7", "iqr-8", "iqr-9", NULL),
      na.rm = TRUE
    )

### Arguments

<table>
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code>mod</code></td>
<td>'numeric' vector. Modeled or simulated values. Must be same length
as <code>obs</code>.</td>
</tr>
<tr class="even">
<td><code>obs</code></td>
<td>'numeric' vector. Observed or comparison values. Must be same length
as <code>mod</code>.</td>
</tr>
<tr class="odd">
<td><code>normalize</code></td>
<td>'character' value. Option to normalize the root-mean-square error
(NRMSE) by several normalizing options. Default is
<code>'none'</code>(no normalizing). RMSE is returned.<br />
<code>'mean'</code>. RMSE is normalized by the mean of
<code>obs</code>.<br />
<code>'range'</code>. RMSE is normalized by the range
<code>(max - min)</code> of <code>obs</code>.<br />
<code>'stdev'</code>. RMSE is normalized by the standard deviation of
<code>obs</code>.<br />
<code>'iqr-#'</code>. RMSE is normalized by the inter-quartile range of
<code>obs</code>, with distribution type (see
<code>stats::quantile</code> function) indicated by integer number (for
example <code>"iqr-8"</code>). If no type specified, default type is
<code>iqr-7</code>, the quantile function default.</td>
</tr>
<tr class="even">
<td><code>na.rm</code></td>
<td>'boolean' <code>TRUE</code> or <code>FALSE</code>. Should
<code>NA</code> values be removed before computing. If any
<code>NA</code> values are present in <code>mod</code> or
<code>obs</code>, the <em>i</em>th position from each will be removed
before calculating. If <code>NA</code> values are present and
<code>na.rm = FALSE</code>, then function will return <code>NA</code>.
Default is <code>TRUE</code>.</td>
</tr>
</tbody>
</table>

### Value

'numeric' value of computed root-mean-square error (RMSE) or normalized
root-mean-square error (NRMSE)

### Examples

    # RMSE
    GOF_rmse(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)
    # NRMSE
    GOF_rmse(
      mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs, normalize = 'stdev'
    )

# `GOF_summary`

## Calculate Goodness-of-fit metrics and output into table

### Description

Calculate Goodness-of-fit (GOF) metrics for correlation, Kling–Gupta
efficiency, mean absolute error, mean error, Nash–Sutcliffe efficiency,
percent bias, root-mean-square error, normalized root-mean-square error,
and volumetric efficiency, and output into a table.

### Usage

    GOF_summary(
      mod,
      obs,
      metrics = c("cor", "kge", "mae", "me", "nse", "pb", "rmse", "nrmse", "ve"),
      censor_threshold = NULL,
      censor_symbol = NULL,
      na.rm = TRUE,
      kge_modified = FALSE,
      nse_j = 2,
      rmse_normalize = c("mean", "range", "stdev", "iqr", "iqr-1", "iqr-2", "iqr-3", "iqr-4",
        "iqr-5", "iqr-6", "iqr-7", "iqr-8", "iqr-9", NULL),
      ...
    )

### Arguments

<table>
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code>mod</code></td>
<td>'numeric' vector. Modeled or simulated values. Must be same length
as <code>obs</code>.</td>
</tr>
<tr class="even">
<td><code>obs</code></td>
<td>'numeric' vector. Observed or comparison values. Must be same length
as <code>mod</code>.</td>
</tr>
<tr class="odd">
<td><code>metrics</code></td>
<td>'character' vector. Which GOF metrics should be computed and output.
Default is
<code>c ("cor", "kge", "mae", "me", "nse" , "pb", "rmse", "nrmse", "ve")</code>.<br />
<code>"cor"</code>. Correlation tests computed from
<code>GOF_correlation_tests</code>.<br />
<code>"kge"</code>. Kling–Gupta efficiency computed from
<code>GOF_kling_gupta_efficiency</code>.<br />
<code>"mae"</code>. Mean absolute error computed from
<code>GOF_mean_absolute_error</code>.<br />
<code>"me"</code>. Mean error computed from
<code>GOF_mean_error</code>.<br />
<code>"nse"</code>. Nash–Sutcliffe efficiency computed from<br />
<code>GOF_nash_sutcliffe_efficiency</code> with option for modified NSE
specified by parameter <code>nse_j</code>.<br />
<code>"pb"</code>. Percent bias computed from
<code>GOF_percent_bias</code>.<br />
<code>"rmse"</code>. Root-mean-square error computed from
<code>GOF_rmse</code>.<br />
<code>"nrmse"</code>. Normalized root-mean-square error computed from
<code>GOF_rmse</code> and "normalize" option specified in parameter
<code>rmse_normalize</code>.<br />
<code>"ve"</code>. Volumetric efficiency computed from
<code>GOF_volumetric_efficiency</code>.<br />
</td>
</tr>
<tr class="even">
<td><code>censor_threshold</code></td>
<td>'numeric' value. Threshold to censor values on utilizing
<code>censor_values</code> function. Default is <code>NULL</code>, no
censoring. If level specified, must also specify<br />
<code>censor_symbol</code>.</td>
</tr>
<tr class="odd">
<td><code>censor_symbol</code></td>
<td>'character' string. Inequality symbol to censor values based on
<code>censor_threshold</code> utilizing <code>censor_values</code>
function. Accepted values are<br />
<code>"gt"</code> (greater than),<br />
<code>"gte"</code> (greater than or equal to),<br />
<code>"lt"</code> (less than),<br />
or <code>"lte"</code> (less than or equal to).<br />
Default is <code>NULL</code>, no censoring. If symbol specified, must
also specify <code>censor_value</code>.</td>
</tr>
<tr class="even">
<td><code>na.rm</code></td>
<td>'boolean' <code>TRUE</code> or <code>FALSE</code>. Should
<code>NA</code> values be removed before computing. If any
<code>NA</code> values are present in <code>mod</code> or
<code>obs</code>, the <em>i</em>th position from each will be removed
before calculating. If <code>NA</code> values are present and
<code>na.rm = FALSE</code>, then function will return <code>NA</code>.
Default is <code>TRUE</code>.</td>
</tr>
<tr class="odd">
<td><code>kge_modified</code></td>
<td>'boolean' <code>TRUE</code> or <code>FALSE</code>. Should the KGE
calculation use the original variability ratio in the standard
deviations (<code>kge_modified = FALSE</code>) or the modified
variability ratio in the coefficient of variations
(<code>kge_modified = TRUE</code>). Default is <code>FALSE</code>.</td>
</tr>
<tr class="even">
<td><code>nse_j</code></td>
<td>'numeric' value. Exponent value for modified NSE (mNSE) equation,
utilized if <code>"nse"</code> option is in parameter
<code>metrics</code>. Default value is <code>nse_j = 2</code>, which is
traditional NSE equation.</td>
</tr>
<tr class="odd">
<td><code>rmse_normalize</code></td>
<td>'character' value. Normalize option for NRMSE, utilized if "nrmse"
option is in paramter <code>metrics</code>. Default is
<code>"mean"</code>. Options are<br />
<code>'mean'</code>. RMSE is normalized by the mean of
<code>obs</code>.<br />
<code>'range'</code>. RMSE is normalized by the range
<code>(max - min)</code> of <code>obs</code>.<br />
<code>'stdev'</code>. RMSE is normalized by the standard deviation of
<code>obs</code>.<br />
<code>'iqr-#'</code>. RMSE is normalized by the inter-quartile range of
<code>obs</code>, with distribution type (see
<code>stats::quantile</code> function) indicated by integer number (for
example <code>"iqr-8"</code>). If no type specified, default type is
<code>iqr-7</code>, the quantile function default.</td>
</tr>
<tr class="even">
<td><code>...</code></td>
<td>Further arguments to be passed to or from
<code>stats::cor.test</code> if <code>"cor"</code> is in
<code>metrics</code>.</td>
</tr>
</tbody>
</table>

### Details

See `GOF_correlation_tests`, `GOF_kling_gupta_efficiency`,  
`GOF_mean_absolute_error`, `GOF_mean_error`,  
`GOF_nash_sutcliffe_efficiency`, `GOF_percent_bias`, `GOF_rmse`,  
and `GOF_volumetric_efficiency`.

### Value

A tibble (see `tibble::tibble`) with GOF metrics

### See Also

`censor_values`, `GOF_correlation_tests`, `GOF_kling_gupta_efficiency`,
`GOF_mean_absolute_error`, `GOF_mean_error`,  
`GOF_nash_sutcliffe_efficiency`, `GOF_percent_bias`, `GOF_rmse`,  
`GOF_volumetric_efficiency`

### Examples

    GOF_summary(mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs)

# `GOF_volumetric_efficiency`

## Calculate Volumetric Efficiency

### Description

Calculate Volumetric efficiency (VE) between modeled (simulated) and
observed values. VE is defined as the fraction of water delivered at the
proper time (Criss and Winston, 2008).

### Usage

    GOF_volumetric_efficiency(mod, obs, na.rm = TRUE)

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>mod</code></td>
<td style="text-align: left;">'numeric' vector. Modeled or simulated
values. Must be same length as <code>obs</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>obs</code></td>
<td style="text-align: left;">'numeric' vector. Observed or comparison
values. Must be same length as <code>mod</code>.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>na.rm</code></td>
<td style="text-align: left;">'boolean' <code>TRUE</code> or
<code>FALSE</code>. Should <code>NA</code> values be removed before
computing. If any <code>NA</code> values are present in <code>mod</code>
or <code>obs</code>, the <em>i</em>th position from each will be removed
before calculating. If <code>NA</code> values are present and
<code>na.rm = FALSE</code>, then function will return <code>NA</code>.
Default is <code>TRUE</code>.</td>
</tr>
</tbody>
</table>

### Details

Volumetric efficiency was proposed in order to circumvent some problems
associated to the Nash–Sutcliffe efficiency. It ranges from `0` to `1`
and represents the fraction of water delivered at the proper time; its
compliment represents the fractional volumetric mismatch (Criss and
Winston, 2008).

### Value

Value of computed Volumetric efficiency.

### References

Criss, R.E. and Winston, W.E., 2008, Do Nash values have value?
Discussion and alternate proposals: Hydrological Processes, v. 22, p.
2723-2725.  
\[Also available at <https://doi.org/10.1002/hyp.7072>.\]

Zambrano-Bigiarini, M., 2020, hydroGOF: Goodness-of-fit functions for
comparison of simulated and observed hydrological time series R package
version 0.4-0. accessed September 16, 2020, at
<https://github.com/hzambran/hydroGOF>. \[Also available at
<https://doi.org/10.5281/zenodo.839854>.\]

### Examples

    GOF_volumetric_efficiency(
      mod = example_mod$streamflow_cfs, obs = example_obs$streamflow_cfs
    )

# `HyMETT-package`

## Hydrologic Model Evaluation and Time-series Tools

### Description

This package facilitates the analysis and evaluation of hydrologic model
output and time-series data with functions focused on comparison of
modeled (simulated) and observed data, period-of-record statistics, and
trends.

### Details

Please see
\Sexpr\[results=rd\]{tools:::Rd\_expr\_doi("10.5066/P9FNXEWI")} for more
details.

# `POR_apply_annual_hiflow_stats`

## Calculate the 50th and 90th percentiles of a streamflow time series

### Description

This function computes the 50th and 90th percentiles of a streamflow
time series from annual n-day high flow values and returns a data.frame
in the format of other period-of-record (POR) metrics.

### Usage

    POR_apply_annual_hiflow_stats(annual_max, quantile_type = 8)

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>annual_max</code></td>
<td style="text-align: left;">'numeric' vector or data.frame. Vector or
data.frame with columns of annual n-day maximum streamflows.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>quantile_type</code></td>
<td style="text-align: left;">'numeric' value. The distribution type
used in the <code>stats::quantile</code> function. Default is
<code>8</code> (median-unbiased regardless of distribution). Other types
common in hydrology are <code>6</code> (Weibull) or <code>9</code>
(unbiased for normal distributions).</td>
</tr>
</tbody>
</table>

### Details

annual maximum of n-day moving averages can be computed during
pre-processing step using  
`preproc_precondition_data` and `calc_annual_flow_stats`, or
`preproc_main` for both observed and modeled data.

### Value

Data.frame of 0.5 and 0.9 non-exceedance probabilities (50th and 90th
percentiles), with metric names if `annual_max` is a data.frame with
columns named by metric.

### See Also

`quantile`, `preproc_precondition_data`, `calc_annual_flow_stats`,
`preproc_main`

### Examples

    POR_apply_annual_hiflow_stats(annual_max = example_annual[ , c("high_q1", "high_q30")])

# `POR_apply_annual_lowflow_stats`

## Calculate 10-year and 2-year return periods of a streamflow time series

### Description

Calculates 10-year and 2-year return periods of a streamflow time series
from annual n-day low streamflow values and returns a data.frame in the
format of other period-of-record (POR) metrics.

### Usage

    POR_apply_annual_lowflow_stats(annual_min)

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>annual_min</code></td>
<td style="text-align: left;">'numeric' vector or data.frame. Vector or
data.frame with columns of annual n-day minimum streamflows.</td>
</tr>
</tbody>
</table>

### Details

`POR_apply_POR_lowflow_metrics` is a helper function that applies the
`POR_calc_lp3_quantile` function to the data.frame of n-day moving
averages, which can be computed during pre-processing step using
`preproc_precondition_data` and `calc_annual_flow_stats`, or
`preproc_main` for both observed and modeled data. This function returns
a data.frame with the 10-year and 2-year return period streamflows for
each n-day low streamflow in the input data.frame.

### Value

data.frame with 10-year and 2-year return period of n-day streamflows.

### See Also

`POR_calc_lp3_quantile`, `preproc_precondition_data`,
`calc_annual_flow_stats`,  
`preproc_main`

### Examples

    POR_apply_annual_lowflow_stats(annual_min = example_annual[ , c("low_q1", "low_q30")])

# `POR_calc_amp_and_phase`

## Calculate the seasonal amplitude and phase of a daily time series

### Description

Calculates the seasonal amplitude and phase of a daily time series.

### Usage

    POR_calc_amp_and_phase(
      data = NULL,
      Date,
      value,
      time_step = c("daily", "monthly")
    )

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>data</code></td>
<td style="text-align: left;">'data.frame'. Optional data.frame input,
with columns containing <code>Date</code> and <code>value</code>. Column
names are specified as strings in the corresponding parameter. Default
is <code>NULL</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>Date</code></td>
<td style="text-align: left;">'numeric' vector of Dates corresponding to
each <code>value</code> when <code>data = NULL</code>, or 'character'
string identifying Date column name when <code>data</code> is
specified.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>value</code></td>
<td style="text-align: left;">'numeric' vector of values (often
streamflow) when <code>data = NULL</code>, or 'character' string
identifying value column name when <code>data</code> is specified.
Assumed to be daily or monthly.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>time_step</code></td>
<td style="text-align: left;">'character' value. Either
<code>"daily"</code> or <code>"monthly"</code>, Default is
<code>"daily"</code>.</td>
</tr>
</tbody>
</table>

### Value

A data.frame with calculated seasonal amplitude and phase

### References

Farmer, W.H., Archfield, S.A., Over, T.M., Hay, L.E., LaFontaine, J.H.,
and Kiang, J.E., 2014, A comparison of methods to predict historical
daily streamflow time series in the southeastern United States: U.S.
Geological Survey Scientific Investigations Report 2014–5231, 34 p.
\[Also available at <https://doi.org/10.3133/sir20145231>.\]

### Examples

    POR_calc_amp_and_phase(data = example_obs, Date = "Date", value = "streamflow_cfs")

# `POR_calc_AR1`

## calculates lag-one autocorrelation (AR1) coefficient for a time series

### Description

calculates lag-one autocorrelation (AR1) coefficient for a time series

### Usage

    POR_calc_AR1(data = NULL, Date, value, time_step = c("daily", "monthly"))

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>data</code></td>
<td style="text-align: left;">'data.frame'. Optional data.frame input,
with columns containing <code>Date</code> and <code>value</code>. Column
names are specified as strings in the corresponding parameter. Default
is <code>NULL</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>Date</code></td>
<td style="text-align: left;">'numeric' vector of Dates corresponding to
each <code>value</code> when <code>data = NULL</code>, or 'character'
string identifying Date column name when <code>data</code> is
specified.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>value</code></td>
<td style="text-align: left;">'numeric' vector of values (often
streamflow) when <code>data = NULL</code>, or 'character' string
identifying value column name when <code>data</code> is specified.
Assumed to be daily or monthly.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>time_step</code></td>
<td style="text-align: left;">'character' value. Either
<code>"daily"</code> or <code>"monthly"</code>.</td>
</tr>
</tbody>
</table>

### Details

The function calculates lag-one autocorrelation (AR1) coefficient for a
time series using the  
`stats::ar` function. When applied to an observed or modeled time series
of streamflow, the  
`POR_deseasonalize` function can be applied to the raw data prior to
running the `POR_calc_AR1` function.

### Value

A data.frame with calculated seasonal amplitude and phase.

### References

Farmer, W.H., Archfield, S.A., Over, T.M., Hay, L.E., LaFontaine, J.H.,
and Kiang, J.E., 2014, A comparison of methods to predict historical
daily streamflow time series in the southeastern United States: U.S.
Geological Survey Scientific Investigations Report 2014–5231, 34 p.
\[Also available at <https://doi.org/10.3133/sir20145231>.\]

### See Also

`POR_deseasonalize`, `ar`

### Examples

    POR_calc_AR1(data = example_obs, Date = "Date", value = "streamflow_cfs")

# `POR_calc_lp3_quantile`

## Calculate quantile from fitted log-Pearson type III distribution

### Description

Calculate the specified flow quantile from a fitted log-Pearson type III
distribution from a time series of n-day low flows.

### Usage

    POR_calc_lp3_quantile(annual_min, p)

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>annual_min</code></td>
<td style="text-align: left;">'numeric' vector. Vector of minimum annual
n-day mean flows.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>p</code></td>
<td style="text-align: left;">'numeric' value of exceedance
probabilities. Quantile of fitted distribution that is returned
(<code>p=0.1</code> for 10-year return period, <code>p=0.5</code> for
2-year return period)</td>
</tr>
</tbody>
</table>

### Details

`POR_calc_lp3_quantile` fits an log-Pearson type III distribution to a
series of annual n-day flows and returns the quantile of a
user-specified probability using `calc_qlpearsonIII`. This represents a
theoretical return period for than n-day flow.

### Value

Specified quantile from the fitted log-Pearson type 3 distribution.

### References

Asquith, W.H., Kiang, J.E., and Cohn, T.A., 2017, Application of at-site
peak-streamflow frequency analyses for very low annual exceedance
probabilities: U.S. Geological Survey Scientific Investigation Report
2017–5038, 93 p. \[Also available at
<https://doi.org/10.3133/sir20175038>.\]

### See Also

`calc_qlpearsonIII`

### Examples

    POR_calc_lp3_quantile(annual_min = example_annual$low_q1, p = 0.1)

# `POR_deseasonalize`

## Removes seasonal trends from a daily or monthly time series.

### Description

Removes seasonal trends from a daily or monthly time series. Daily data
are deseasonalized by subtracting monthly mean values. Monthly data are
deseasonalized by subtracting mean monthly values.

### Usage

    POR_deseasonalize(data = NULL, Date, value, time_step = c("daily", "monthly"))

### Arguments

<table>
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code>data</code></td>
<td>'data.frame'. Optional data.frame input, with columns containing
<code>Date</code> and <code>value</code>. Column names are specified as
strings in the corresponding parameter. Default is
<code>NULL</code>.</td>
</tr>
<tr class="even">
<td><code>Date</code></td>
<td>'numeric' vector of Dates corresponding to each <code>value</code>
when <code>data = NULL</code>, or<br />
'character' string identifying Date column name when <code>data</code>
is specified.</td>
</tr>
<tr class="odd">
<td><code>value</code></td>
<td>'numeric' vector of values (often streamflow) when
<code>data = NULL</code>, or<br />
'character' string identifying value column name when <code>data</code>
is specified.<br />
(assumed to be daily or monthly).</td>
</tr>
<tr class="even">
<td><code>time_step</code></td>
<td>'character' value. Either <code>"daily"</code> or
<code>"monthly"</code>.</td>
</tr>
</tbody>
</table>

### Details

The deseasonalize function removes seasonal trends from a daily or
monthly time series and returns a deseasonalized time series, which can
be used in the `POR_calc_AR1` function.

### Value

Deseasonalized values.

### See Also

`POR_calc_AR1`

### Examples

    POR_deseasonalize(data = example_obs, Date = "Date", value = "streamflow_cfs")

# `POR_distribution_metrics`

## Calculates various metrics that describe the distribution of a time series of streamflow

### Description

Calculates various metrics that describe the distribution of a time
series of streamflow, which can be of any time step.

### Usage

    POR_distribution_metrics(value, quantile_type = 8, na.rm = TRUE)

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>value</code></td>
<td style="text-align: left;">'numeric' vector of values (assumed to be
streamflow) at any time step.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>quantile_type</code></td>
<td style="text-align: left;">'numeric' value. The distribution type
used in the <code>stats::quantile</code> function. Default is
<code>8</code> (median-unbiased regardless of distribution). Other types
common in hydrology are <code>6</code> (Weibull) or <code>9</code>
(unbiased for normal distributions).</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>na.rm</code></td>
<td style="text-align: left;">'boolean' <code>TRUE</code> or
<code>FALSE</code>. Should <code>NA</code> values be removed before
computing. If <code>NA</code> values are present and
<code>na.rm = FALSE</code>, then function will return <code>NA</code>s.
Default is <code>TRUE</code>.</td>
</tr>
</tbody>
</table>

### Details

Metrics computed include:

`p_`*n*  
Flow-duration curve (FDC) percentile where *n* = 1, 5, 10, 25, 50, 75,
90, 95, and 99

`POR_mean`  
Period of record mean

`POR_sd`  
Period of record standard deviation

`POR_cv`  
Period of record coefficient of variation

`POR_min`  
Period of record minimum

`POR_max`  
Period of record maximum

`LCV`  
L-moment coefficient of variation

`Lskew`  
L-moment skewness

`Lkurtosis`  
L-moment kurtosis

### Value

A data.frame with FDC quantiles, and distribution metrics. See
**Details**. This function calculates various metrics that describe the
distribution of a time series of streamflow, which can be of any time
step.

### References

Farmer, W.H., Archfield, S.A., Over, T.M., Hay, L.E., LaFontaine, J.H.,
and Kiang, J.E., 2014, A comparison of methods to predict historical
daily streamflow time series in the southeastern United States: U.S.
Geological Survey Scientific Investigations Report 2014–5231, 34 p.
\[Also available at <https://doi.org/10.3133/sir20145231>.\]

Asquith, W.H., Kiang, J.E., and Cohn, T.A., 2017, Application of at-site
peak-streamflow frequency analyses for very low annual exceedance
probabilities: U.S. Geological Survey Scientific Investigation Report
2017–5038, 93 p. \[Also available at
<https://doi.org/10.3133/sir20175038>.\]

Asquith, W.H., 2021, lmomco—L-moments, censored L-moments, trimmed
L-moments,  
L-comoments, and many distributions. R package version 2.3.7, Texas Tech
University, Lubbock, Texas.

### See Also

`lmoms`, `quantile`

### Examples

    POR_distribution_metrics(value = example_obs$streamflow_cfs)
     

# `preproc_audit_data`

## Audit daily data for total days in year

### Description

Audit daily data for total days in year. An audit is performed to
inventory and flag missing days in daily data and help determine if
further analyses are appropriate.

### Usage

    preproc_audit_data(
      data = NULL,
      Date,
      value,
      year_group,
      use_specific_years = FALSE,
      begin_year = NULL,
      end_year = NULL,
      days_cutoff = 360,
      date_format = "%Y-%m-%d"
    )

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>data</code></td>
<td style="text-align: left;">'data.frame'. Optional data.frame input,
with columns containing <code>Date</code> and <code>value</code>. Column
names are specified as strings in the corresponding parameter. Default
is <code>NULL</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>Date</code></td>
<td style="text-align: left;">'Date' or 'character' vector when
<code>data = NULL</code>, or 'character' string identifying Date column
name when <code>data</code> is specified. Dates associated with each
value in value parameter.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>value</code></td>
<td style="text-align: left;">'numeric' vector when
<code>data = NULL</code>, or 'character' string identifying year column
name when <code>data</code> is specified. Values to audit, must be daily
data.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>year_group</code></td>
<td style="text-align: left;">'numeric' vector when
<code>data = NULL</code>, or 'character' string identifying grouping
column name when <code>data</code> is specified. Year grouping for each
daily value in <code>value</code> parameter. Must be same length as
<code>value</code>.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>use_specific_years</code></td>
<td style="text-align: left;">'boolean' value. Flag to clip data to a
certain set of years in <code>year_group</code>. Default is
<code>FALSE</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>begin_year</code></td>
<td style="text-align: left;">'numeric' value. If
<code>use_specific_years = TRUE</code>, beginning year to clip value.
Default is <code>NULL</code>.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>end_year</code></td>
<td style="text-align: left;">'numeric' value. If
<code>use_specific_years = TRUE</code>, ending year to clip value.
Default is <code>NULL</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>days_cutoff</code></td>
<td style="text-align: left;">'numeric' value. Designating the number of
days required for a year to be counted as full. Default is
<code>360</code>.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>date_format</code></td>
<td style="text-align: left;">'character' string. Format of Date.
Default is <code>"%Y-%m-%d"</code>.</td>
</tr>
</tbody>
</table>

### Details

Year grouping is commonly water year, climate year, or calendar year.

### Value

A data.frame with `year_group`, count (n, excluding `NA` values) of days
in each `year_group`, and a complete years 'boolean' flag.

### See Also

`preproc_fill_daily`, `preproc_precondition_data`

### Examples

    preproc_audit_data(
      data = example_preproc, Date = "Date", value = "value", year_group = "WY"
    )

# `preproc_fill_daily`

## Fills daily data with missing dates as `NA` values

### Description

Fills daily data with missing dates as `NA` values. Days that are absent
from the daily time series are inserted with a corresponding value of
`NA`.

### Usage

    preproc_fill_daily(
      data = NULL,
      Date,
      value,
      POR_start = NA,
      POR_end = NA,
      date_format = "%Y-%m-%d"
    )

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>data</code></td>
<td style="text-align: left;">'data.frame'. Optional data.frame input,
with columns containing <code>Date</code> and <code>value</code>. Column
names are specified as strings in the corresponding parameter. Default
is <code>NULL</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>Date</code></td>
<td style="text-align: left;">'Date' or 'character' vector when
<code>data = NULL</code>, or 'character' string identifying Date column
name when <code>data</code> is specified. Date associated with each
value in <code>value</code> parameter.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>value</code></td>
<td style="text-align: left;">'numeric' vector when
<code>data = NULL</code>, or 'character' string identifying values
column name when <code>data</code> is specified.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>POR_start</code></td>
<td style="text-align: left;">'character' value. Optional period of
record start. If not specified, defaults to <code>min(Date)</code>.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>POR_end</code></td>
<td style="text-align: left;">'character' value. Optional period of
record end. If not specified, defaults to <code>max(Date)</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>date_format</code></td>
<td style="text-align: left;">'character' string. Format of Date.
Default is <code>"%Y-%m-%d"</code>.</td>
</tr>
</tbody>
</table>

### Details

Can be used prior to `preproc_precondition_data` to fill daily data
before computation of n-day moving averages, or prior to
`preproc_audit_data`.

### Value

A data.frame with `Date` and `value`, sequenced from `POR_start` to
`POR_end` by 1 day.

### See Also

`preproc_audit_data`, `preproc_precondition_data`

### Examples

    Dates = c(seq.Date(as.Date("2020-01-01"), as.Date("2020-01-10"), by = "1 day"),
              seq.Date(as.Date("2020-01-20"), as.Date("2020-01-31"), by = "1 day"))
    values = c(seq.int(1, 22, 1))
    preproc_fill_daily(Date = Dates, value = values)

# `preproc_main`

## A wrapper function for preproc\_precondition\_data, preproc\_audit\_data, and calc\_annual\_flow\_stats

### Description

A wrapper function for `preproc_precondition_data`,
`preproc_audit_data`, and  
`calc_annual_flow_stats`

### Usage

    preproc_main(
      data = NULL,
      Date,
      value,
      date_format = "%Y-%m-%d",
      year_group = c("WY", "CY", "year"),
      use_specific_years = FALSE,
      begin_year = NULL,
      end_year = NULL,
      days_cutoff = 360,
      calc_high = TRUE,
      calc_low = TRUE,
      calc_percentiles = TRUE,
      calc_monthly = TRUE,
      calc_WSCVD = TRUE,
      longitude = NA,
      calc_ICVD = FALSE,
      zero_threshold = 33,
      quantile_type = 8,
      na.action = c("na.omit", "na.pass")
    )

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>data</code></td>
<td style="text-align: left;">'data.frame'. Optional data.frame input,
with columns containing <code>Date</code> and <code>value</code>. Column
names are specified as strings in the corresponding parameter. Default
is <code>NULL</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>Date</code></td>
<td style="text-align: left;">'Date' or 'character' vector when
<code>data = NULL</code>, or 'character' string identifying Date column
name when <code>data</code> is specified. Dates associated with each
value in <code>value</code> parameter.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>value</code></td>
<td style="text-align: left;">'numeric' vector when
<code>data = NULL</code>, or 'character' string identifying year column
name when <code>data</code> is specified. Values to precondition and
calculate n-day moving averages from. N-day moving averages only
calculated for daily data.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>date_format</code></td>
<td style="text-align: left;">'character' string. Format of Date.
Default is <code>"%Y-%m-%d"</code>.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>year_group</code></td>
<td style="text-align: left;">'character' value. Specify either
<code>"year"</code> for calendar year, <code>"WY"</code> for water year,
or <code>"CY"</code> for climate year. Used to select data after
preconditioning for audit and annual statistics. Default is
<code>"WY"</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>use_specific_years</code></td>
<td style="text-align: left;">'boolean' value. Flag to clip data to a
certain set of years in <code>year_group</code>. Default is
<code>FALSE</code>.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>begin_year</code></td>
<td style="text-align: left;">'numeric' value. If
<code>use_specific_years = TRUE</code>, beginning year to clip
<code>value</code>. Default is <code>NULL</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>end_year</code></td>
<td style="text-align: left;">'numeric' value. If
<code>use_specific_years = TRUE</code>, ending year to clip
<code>value</code>. Default is <code>NULL</code>.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>days_cutoff</code></td>
<td style="text-align: left;">'numeric' value. Designating the number of
days required for a year to be counted as full. Default is
<code>360</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>calc_high</code></td>
<td style="text-align: left;">'boolean' value. Calculate high streamflow
statistics for years in <code>year_group</code>. Default is
<code>TRUE</code>. See <strong>Details</strong> for more
information.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>calc_low</code></td>
<td style="text-align: left;">'boolean' value. Calculate low streamflow
statistics for years in <code>year_group</code>. Default is
<code>TRUE</code>. See <strong>Details</strong> for more
information.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>calc_percentiles</code></td>
<td style="text-align: left;">'boolean' value. Calculate percentiles for
years in <code>year_group</code>. Default is <code>TRUE</code>. See
<strong>Details</strong> for more information.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>calc_monthly</code></td>
<td style="text-align: left;">'boolean' value. Calculate monthly
statistics for years in <code>year_group</code>. Default is
<code>TRUE</code>. See <strong>Details</strong> for more
information.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>calc_WSCVD</code></td>
<td style="text-align: left;">'boolean' value. Calculate winter-spring
center volume date for years in <code>year_group</code>. Default is
<code>TRUE</code>. See <strong>Details</strong> for more
information.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>longitude</code></td>
<td style="text-align: left;">'numeric' value. Site longitude in NAD83,
required in WSCVD calculation. Default is <code>NA</code>. See
<strong>Details</strong> for more information.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>calc_ICVD</code></td>
<td style="text-align: left;">'boolean' value. Calculate inverse center
volume date for years in <code>year_group</code>. Default is
<code>FALSE</code>. See <strong>Details</strong> for more
information.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>zero_threshold</code></td>
<td style="text-align: left;">'numeric' value as percentage. The
percentage of years of a statistic that need to be zero in order for it
to be deemed a zero streamflow site for that statistic. For use in trend
calculation. See <strong>Details</strong> on attributes. Default is
<code>33</code> (33 percent) of the annual statistic values.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>quantile_type</code></td>
<td style="text-align: left;">'numeric' value. The distribution type
used in the <code>stats::quantile</code> function. Default is
<code>8</code> (median-unbiased regardless of distribution). Other types
common in hydrology are <code>6</code> (Weibull) or <code>9</code>
(unbiased for normal distributions).</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>na.action</code></td>
<td style="text-align: left;">'character' string indicating na.action
passed to <code>stats::aggregate</code> <code>na.action</code>
parameter. Default is <code>"na.omit"</code>, which removes
<code>NA</code> values before aggregating statistics, or
<code>"na.pass"</code>, which will pass <code>NA</code> values and
return <code>NA</code> in the grouped calculation if any <code>NA</code>
values are present.</td>
</tr>
</tbody>
</table>

### Details

This is a wrapper function of `preproc_precondition_data`,
`preproc_audit_data`, and  
`calc_annual_flow_stats`. Data are first passed to the precondition
function, then audited, then annual statistics are computed.  
It also checks the timestep of the data to make sure that it is daily
timestep. Other time steps are currently not supported and will return
the data.frame without moving averages computed.

### Value

A list of three data.frames: 1 of preconditioned data, 1 data audit, and
1 annual statistics.

### See Also

`preproc_audit_data`, `preproc_precondition_data`,
`calc_annual_flow_stats`

### Examples

    preproc_main(data = example_obs, Date = "Date", value = "streamflow_cfs", longitude = -68)

# `preproc_precondition_data`

## Pre-conditions data with time information and n-day moving averages

### Description

Pre-conditions data with time information and n-day moving averages,
with options to fill missing days with `NA` values.

### Usage

    preproc_precondition_data(
      data = NULL,
      Date,
      value,
      date_format = "%Y-%m-%d",
      fill_daily = TRUE
    )

### Arguments

<table>
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code>data</code></td>
<td>'data.frame'. Optional data.frame input, with columns containing
<code>Date</code> and <code>value</code>. Column names are specified as
strings in the corresponding parameter. Default is
<code>NULL</code>.</td>
</tr>
<tr class="even">
<td><code>Date</code></td>
<td>'Date' or 'character' vector when <code>data = NULL</code>, or
'character' string identifying Date column name when <code>data</code>
is specified. Dates associated with each value in <code>value</code>
parameter.</td>
</tr>
<tr class="odd">
<td><code>value</code></td>
<td>'numeric' vector when <code>data = NULL</code>, or 'character'
string identifying year column name when <code>data</code> is specified.
Values to precondition and calculate n-day moving averages from. N-day
moving averages only calculated for daily data.</td>
</tr>
<tr class="even">
<td><code>date_format</code></td>
<td>'character' string. Format of <code>Date</code>. Default is
<code>"%Y-%m-%d"</code>.</td>
</tr>
<tr class="odd">
<td><code>fill_daily</code></td>
<td>'logical' value. Should gaps in <code>Date</code> and
<code>value</code> be filled using<br />
<code>preproc_fill_daily</code>. Default is <code>TRUE</code>.</td>
</tr>
</tbody>
</table>

### Details

These columns are added to the data:

`year`  

`month`  

`day`  

`decimal_date`  

`WY`  
Water Year: October 1 to September 30

`CY`  
Climate Year: April 1 to March 30

`Q3`  
3-Day Moving Average: computed at end of moving interval

`Q7`  
7-Day Moving Average: computed at end of moving interval

`Q30`  
30-Day Moving Average: computed at end of moving interval

`jd`  
Julian date

This function also checks the time step of the data to make sure that it
is daily time step. Daily values with gaps are important to fill with
`NA` to ensure proper calculation of n-day moving averages. Use
`fill_daily = TRUE` or `preproc_fill_daily`. Other time steps are
currently not supported and will return the data.frame without moving
averages computed.

### Value

A data.frame with Date, value, and additional columns with time and
n-day moving average information.

### See Also

`preproc_fill_daily`, `rollmean`

### Examples

    preproc_precondition_data(data = example_obs, Date = "Date", value = "streamflow_cfs")

# `preproc_validate_daily`

## Validates that daily data do not contain gaps

### Description

Validates that daily data do not contain gaps

### Usage

    preproc_validate_daily(
      data = NULL,
      Date = "Date",
      value = "value",
      date_format = "%Y-%m-%d"
    )

### Arguments

<table>
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>data</code></td>
<td style="text-align: left;">'data.frame'. Optional data.frame input,
with columns containing <code>Date</code> and <code>value</code>. Column
names are specified as strings in the corresponding parameter. Default
is <code>NULL</code>.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>Date</code></td>
<td style="text-align: left;">'Date' or 'character' vector when
<code>data = NULL</code>, or 'character' string identifying Date column
name when <code>data</code> is specified. Dates associated with each
value in <code>value</code> parameter.</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>value</code></td>
<td style="text-align: left;">'numeric' vector when
<code>data = NULL</code>, or 'character' string identifying year column
name when <code>data</code> is specified. Values to precondition and
calculate n-day moving averages from. N-day moving averages only
calculated for daily data.</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>date_format</code></td>
<td style="text-align: left;">'character' string. Format of
<code>Date</code>. Default is <code>"%Y-%m-%d"</code>.</td>
</tr>
</tbody>
</table>

### Details

Used to validate there are no gaps in the daily record before computing
n-day moving averages in `preproc_precondition_data` or lag-1
autocorrelation in `POR_calc_AR1`. If gaps are present,
`preproc_fill_daily` can be used to fill them with `NA` values.

### Value

An error message with missing dates, otherwise nothing.

### Examples

    preproc_validate_daily(data = example_obs, Date = "Date", value = "streamflow_cfs")
