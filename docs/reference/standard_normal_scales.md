# Plot the standard normal curve with score scales

Draws the standard normal curve, divided at every standard deviation and
marked with the percentage of cases in each interval, with the z, T,
cumulative percentage, percentile, sten, stanine, grade and IQ scales
lined up under it, so that a score on one scale can be read off on the
others. In blues, reds or greens, colour shows how common a score is;
the diverging palette colours the bands blue below the mean and red
above it. Each palette has a light and a dark mode.

## Usage

``` r
standard_normal_scales(
  size = 3,
  palette = c("blues", "reds", "greens", "diverging"),
  mode = c("light", "dark")
)
```

## Arguments

- size:

  Text size of all labels, in millimetres (ggplot2 text size). The
  default of 3 suits figures around 7 inches wide; about 5 suits figures
  around 12 inches wide.

- palette:

  The colours of the chart:

  "blues", "reds" or "greens"

  : One colour, which shows how common a score is: under the curve, the
    intervals that hold more cases are darker, so 34.13% is the darkest
    and 0.13% the lightest, and the bands are darker the nearer the mean
    they are.

  "diverging"

  : A neutral gray under the curve, and bands blue below the mean and
    red above it, darker the further from the mean, with gray for a band
    centred on the mean.

- mode:

  `"light"` for dark text on a white background, or `"dark"` for light
  text on a near-black background. In the dark mode the colours that are
  darker in the light mode are brighter instead, so that they still
  stand out from the background.

## Value

A ggplot object. Print it to draw the chart, or save it with
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).
The chart keeps a fixed aspect ratio and is about 2.2 times as wide as
it is tall, so a figure with those proportions, such as 7 by 3.2 inches,
has no empty margins.

## Details

All scales are placed on the z axis:

- **z**: standard score, mean 0 and standard deviation 1.

- **T**: \\50 + 10z\\.

- **Cumulative %**: the percentage of cases below each standard
  deviation, from \\z = -3\\ to \\z = 3\\: 0.1, 2.3, 15.9, 50, 84.1,
  97.7 and 99.9.

- **Percentile**: the z values of the 1st, 5th, 10th, 25th, 50th, 75th,
  90th, 95th and 99th percentiles.

- **Sten**: ten bands half a standard deviation wide, sten 1 below \\z =
  -2\\ and sten 10 above \\z = 2\\ (mean 5.5, standard deviation 2).

- **Stanine**: nine bands half a standard deviation wide, stanine 5 from
  \\z = -0.25\\ to \\z = 0.25\\ (mean 5, standard deviation 2).

- **Grade**: groups of stanines, E (1-2), D (3), C (4-6), B (7) and A
  (8-9).

- **IQ**: \\100 + 15z\\.

The chart is
[`standard_normal_custom`](https://sedzinfo.github.io/snp/reference/standard_normal_custom.md)
drawn with
[`standard_normal_scales_data()`](https://sedzinfo.github.io/snp/reference/standard_normal_scales_data.md):
start from those to change the scales.

## See also

[`standard_normal_custom`](https://sedzinfo.github.io/snp/reference/standard_normal_custom.md),
[`standard_normal_scales_data`](https://sedzinfo.github.io/snp/reference/standard_normal_scales_data.md)

## Examples

``` r
standard_normal_scales()

standard_normal_scales(palette="reds")

standard_normal_scales(palette="diverging",mode="dark")

standard_normal_scales(size=5)

# save it with the chart's proportions
ggsave(file.path(tempdir(),"standard_normal_scales.png"),standard_normal_scales(),
       width=7,height=3.2)
```
