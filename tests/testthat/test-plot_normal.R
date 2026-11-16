# draws a chart into a null device, which runs every layer, the bands and the labels included
draw <- function(p) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  ggplot2::ggplotGrob(p)
}

test_that("plot_normal() returns a ggplot that draws, in every palette and mode", {
  for (palette in c("blues", "reds", "greens", "diverging")) {
    for (mode in c("light", "dark")) {
      p <- plot_normal(palette = palette, mode = mode)
      expect_true(inherits(p, "ggplot"))
      expect_no_error(draw(p))
    }
  }
  expect_no_error(draw(plot_normal(size = 5)))
})

test_that("plot_normal() draws scales of your own", {
  # tick marks only, with neither z_end nor colour
  expect_no_error(draw(plot_normal(data.frame(scale = "SCORE", z = -3:3, label = seq(20, 80, 10)))))
  # bands only, some coloured
  bands <- data.frame(scale = "LEVEL", z = c(-4, -1, 1), z_end = c(-1, 1, 4),
                      label = c("Low", "Average", "High"), colour = c("#c74845", NA, NA))
  expect_no_error(draw(plot_normal(bands)))
  # a band given from its end to its start, and a tick mark beyond z = 4
  expect_no_error(draw(plot_normal(data.frame(scale = "WIDE", z = c(1, 5), z_end = c(-1, NA), label = c("band", "5")))))
})

test_that("plot_normal() rejects scales it cannot draw", {
  expect_error(plot_normal(1:3), "data frame")
  expect_error(plot_normal(data.frame()), "data frame")
  expect_error(plot_normal(data.frame(scale = "A", z = 0)), "data frame")
  expect_error(plot_normal(data.frame(scale = "A", z = "0", label = "x")), "numeric")
  expect_error(plot_normal(data.frame(scale = "A", z = 0, z_end = "1", label = "x")), "numeric")
  expect_error(plot_normal(palette = "purples"))
  expect_error(plot_normal(mode = "dim"))
})

test_that("standard_normal_scales_data() has the standard scales, in order", {
  scales <- standard_normal_scales_data()
  expect_s3_class(scales, "data.frame")
  expect_named(scales, c("scale", "z", "z_end", "label", "colour"))
  expect_equal(unique(scales$scale),
               c("z", "T", "CUMULATIVE %", "PERCENTILE", "STEN", "STANINE", "GRADE", "IQ"))
  expect_true(all(is.na(scales$colour)))
})

test_that("the tick marks of the standard scales are where their labels say", {
  scales <- standard_normal_scales_data()
  ticks <- function(name) scales[scales$scale == name, ]
  expect_true(all(is.na(scales$z_end[scales$scale %in% c("z", "T", "CUMULATIVE %", "PERCENTILE", "IQ")])))
  expect_equal(as.numeric(ticks("T")$label), 50 + 10 * ticks("T")$z)
  expect_equal(as.numeric(ticks("IQ")$label), 100 + 15 * ticks("IQ")$z)
  expect_equal(as.numeric(ticks("CUMULATIVE %")$label), round(100 * pnorm(ticks("CUMULATIVE %")$z), 1))
  expect_equal(ticks("PERCENTILE")$z, qnorm(as.numeric(ticks("PERCENTILE")$label) / 100), tolerance = 1e-3)
})

test_that("the bands of the standard scales tile the z axis from -4 to 4", {
  scales <- standard_normal_scales_data()
  for (name in c("STEN", "STANINE", "GRADE")) {
    bands <- scales[scales$scale == name, ]
    expect_equal(bands$z[1], -4)
    expect_equal(bands$z_end[nrow(bands)], 4)
    expect_equal(bands$z[-1], bands$z_end[-nrow(bands)])
  }
  expect_equal(nrow(scales[scales$scale == "STEN", ]), 10)
  expect_equal(nrow(scales[scales$scale == "STANINE", ]), 9)
  expect_equal(scales$label[scales$scale == "GRADE"], c("E", "D", "C", "B", "A"))
})

test_that("bands mirrored about the mean take the same colour, except in the diverging palette", {
  for (palette in c("blues", "reds", "greens")) {
    colours <- band_colours(c(-2, 1), c(-1, 2), palette, "light")
    expect_equal(colours[1], colours[2])
  }
  colours <- band_colours(c(-2, 1), c(-1, 2), "diverging", "light")
  expect_false(colours[1] == colours[2])
  expect_length(band_colours(numeric(0), numeric(0), "blues", "light"), 0)
})

test_that("labels on a fill are white or near-black, whichever stands out more", {
  expect_equal(label_ink(c("#ffffff", "#000000")), c(snp_inks$light$text, "#ffffff"))
})
