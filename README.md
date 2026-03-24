
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Physical Activity in 47,XXY Compared to Matched Controls

## Study Summary

Klinefelter syndrome (KS), 47,XXY, affects 1 in 600 males and is
characterized by an additional X chromosome. Men with KS have altered
testicular development, deficient testosterone levels, and infertility.
They are less physically active than other men, have higher BMIs on
average, and report higher levels of fatigue. However, little research
has explored the relationship between sleep and activity in KS. This
analysis analyzes differences in activity and sleep outcomes between KS
cases and non-KS age- and BMI-matched controls.

KS males and non-KS controls aged 15 - 27 were recruited into the Lipids
to Energy (LTE) clinical trial. Groups were age- and BMI- matched.
Participants wore ActiGraph accelerometers and ActiWare sleep watches
for one continuous week while completing daily surveys on activity,
sleep, and diet. ActiGraph accelerometers monitor activity including
measures of body position and movement. ActiWare sleep watches monitor
sleep and wake time as well as light exposure.

## Data Processing

The first and last day of wear for each participant were excluded for
analysis.

Data were extracted from ActivPal in but summarized within minutes.

Entries where the watch was indicated to not have been worn were
removed. Only days with \>70% of wear time were included.

## Statistical Methods

We model minute-level step count data using a functional regression
framework (function-on-scalar regression) with subject-specific random
functional intercepts.

Let $Y_i(t)$ denote the step count for subject $i = 1, ..., N$ at time
$t$ (15 minute interval of the day). The mean structure was modeled with
smooth functions of time and group differences:

$$Y_i(t) = \beta_0(t) + \beta_{group}(t) \textbf{1}_{group = KS \hspace{1mm} Case} + b_i(t) + \epsilon_i(t)$$,

where
