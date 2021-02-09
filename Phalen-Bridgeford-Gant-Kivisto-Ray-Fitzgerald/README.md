R script for "Baltimore Ceasefire 365: Estimated impact of a recurring community-led ceasefire on gun violence"
---------------------------------------------------------------------

This folder contains the complete code and data necessary to reproduce the [associated paper](https://peterphalen.github.io/publications/baltimore_ceasefire_365_ajph.pdf) published in the *American Journal of Public Health*. View the annotated script with embedded output at [ceasefire_R_script_markdown_output.pdf](ceasefire_R_script_markdown_output.pdf).

The script pulls from  `BPD_Part_1_Victim_Based_Crime_Data.csv` which contains every victim-based crime reported by the City of Baltimore between January 1, 2012 and July 6, 2019, and uses it to fit a Bayesian multilevel model to estimate the effect of ceasefires on gun violence in Baltimore after accounting for yearly seasonality, weekday effects, calendar day effects, and overall trends.

[ceasefire_R_script_markdown.Rmd](ceasefire_R_script_markdown.Rmd) contains the associated markdown script.
