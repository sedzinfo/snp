# Standard Normal Plot

Draws the standard normal curve in R with score scales lined up under
it: z, T, cumulative percentage, percentile, sten, stanine, grade and
IQ, so that a score on one scale can be read off on the others. Colour
shows how common a score is, in blues, reds or greens: the more cases an
interval under the curve, or a band of the scales, holds, the darker it
is. A diverging palette colours the bands blue below the mean and red
above it instead. Each palette has a light and a dark mode.

# Installation Instructions

## install R

installation instructions can be found here:
<https://cran.r-project.org/>\
\## install RStudio IDE (optional but a good idea)\
installation instructions can be found here:
<https://posit.co/downloads/>

open RStudio and type in the console:

    install.packages("devtools")
    library(devtools)
    install_github("sedzinfo/snp")

Without devtools, install the source tarball of the [latest
release](https://github.com/sedzinfo/snp/releases/tag/latest), which is
rebuilt from `master` on every change, together with the PDF manual:

[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(`` `` ``"https://github.com/sedzinfo/snp/releases/download/latest/snp_0.0.0.9000.tar.gz"``,`` `` repos ``=`` ``NULL``, type ``=`` ``"source"`` ``)`

# Usage

[`library`](https://rdrr.io/r/base/library.html)`(`[`snp`](https://sedzinfo.github.io/snp/)`)`` `[`standard_normal_scales`](https://sedzinfo.github.io/snp/reference/standard_normal_scales.md)`(``)`` ``# for figures about 7 inches wide`` `[`standard_normal_scales`](https://sedzinfo.github.io/snp/reference/standard_normal_scales.md)`(``size ``=`` ``5``)`` ``# for figures about 12 inches wide`` `[`standard_normal_scales`](https://sedzinfo.github.io/snp/reference/standard_normal_scales.md)`(``palette ``=`` ``"greens"``, mode ``=`` ``"dark"``)`` ``# blues, reds, greens or diverging; light or dark`` `[`ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html)`(``"snp.png"``, `[`standard_normal_scales`](https://sedzinfo.github.io/snp/reference/standard_normal_scales.md)`(``)``, width ``=`` ``7``, height ``=`` ``3.2``)`` `` ``# your own scales: tick marks for a test score and bands for performance levels`` ``my_scales`` ``<-`` `[`rbind`](https://rdrr.io/r/base/cbind.html)`(`[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``scale ``=`` ``"SCORE"``, z ``=`` ``-``3``:``3``, z_end ``=`` ``NA``, label ``=`` `[`seq`](https://rdrr.io/r/base/seq.html)`(``20``, ``80``, ``10``)``)``,`` `` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``scale ``=`` ``"LEVEL"``, z ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``4``, ``-``1``, ``1``)``, z_end ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``1``, ``1``, ``4``)``, label ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Low"``, ``"Average"``, ``"High"``)``)``)`` `[`standard_normal_custom`](https://sedzinfo.github.io/snp/reference/standard_normal_custom.md)`(``my_scales``)`

[`standard_normal_scales_data()`](https://sedzinfo.github.io/snp/reference/standard_normal_scales_data.md)
returns the standard scales as a data frame, to start from when changing
them.

# Screenshot

![The standard normal curve with the share of cases in each standard
deviation band (34.13%, 13.59%, 2.14%, 0.13%), above aligned scales of z
scores, T scores, cumulative percentages, percentiles, stens, stanines,
grades and IQ, all in blues, darker where more cases
fall](https://raw.githubusercontent.com/sedzinfo/snp/master/snp.png)

# Palettes

`palette` sets the colours, `"blues"` (the default), `"reds"`,
`"greens"` or `"diverging"`, and `mode` the background, `"light"` (the
default) or `"dark"`:

![The chart in every palette and mode: blues, reds and greens, darker
where more cases fall, and the diverging palette, with the bands blue
below the mean and red above it, each on a light and on a dark
background](https://raw.githubusercontent.com/sedzinfo/snp/master/palettes.png)

------------------------------------------------------------------------

![Stars](https://img.shields.io/github/stars/sedzinfo/snp)![Watchers](https://img.shields.io/github/watchers/sedzinfo/snp)![Repo
Size](https://img.shields.io/github/repo-size/sedzinfo/snp)![Open
Issues](https://img.shields.io/github/issues/sedzinfo/snp)![Forks](https://img.shields.io/github/forks/sedzinfo/snp)![Last
Commit](https://img.shields.io/github/last-commit/sedzinfo/snp)![Contributors](https://img.shields.io/github/contributors/sedzinfo/snp)![License](https://img.shields.io/github/license/sedzinfo/snp)![Workflow
Status](https://img.shields.io/github/actions/workflow/status/sedzinfo/snp/build-tarball.yml)
