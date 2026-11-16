# snp 0.0.0.9000

## Breaking changes

* `standard_normal_scales()`, `standard_normal_custom()` and `standard_normal_plot()` are replaced
  by one function, `plot_normal()`. It draws the standard scales by default, and scales of your own
  when given a data frame:

  ```r
  # before
  standard_normal_scales(palette = "reds")
  standard_normal_custom(my_scales)

  # now
  plot_normal(palette = "reds")
  plot_normal(my_scales)
  ```

  The first argument of `plot_normal()` is `scales`, where it was `size` in
  `standard_normal_scales()` and `standard_normal_plot()`, so give the size by name:
  `plot_normal(size = 5)`, not `plot_normal(5)`.

  `standard_normal_scales_data()` keeps its name.

## The chart

* The percentages under the curve are on one line, each over its interval.
* Palettes: `"blues"`, `"reds"`, `"greens"` and `"diverging"`, each with a light and a dark mode.
* A cumulative percentage scale, the percentage of cases below each standard deviation.

## Documentation

* Two articles: "Examples", from palettes and sizes to scales of your own, and "Saving as vector
  graphics", on saving the chart as PDF, SVG, EMF or EPS.
