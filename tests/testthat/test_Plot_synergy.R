test_that("color_ranges_from_green_to_white_to_red", {
  expected_colors <-
    c("#00FF00", "#3FFF3F", "#7FFF7F", "#BFFFBF", # 00ff00 is green
      "#FFFFFF", "#FFBFBF", "#FF7F7F", "#FF3F3F", # ffffff is white
      "#FF0000")                                  # ff0000 is red
  expect_equal(expected_colors, .GetColors(-8,8))
})

test_that("slice_at_least_first_works", {
  expect_equal(-1, .SliceAtLeastFirst(NULL))
  expect_equal(2:5, .SliceAtLeastFirst(c(-1,5)))  # pathological case
  expect_equal(2:5, .SliceAtLeastFirst(c(0,5)))   # degenerate case
  expect_equal(2:5, .SliceAtLeastFirst(c(1,5)))
  expect_equal(2:5, .SliceAtLeastFirst(c(2,5)))
  expect_equal(3:5, .SliceAtLeastFirst(c(3,5)))
  expect_equal(3:5, .SliceAtLeastFirst(c(3,5,10)))  # spurious val 10
})

test_that("numeric_apply_range_works", {
  val1 <- c("1", 2, 3)
  val2 <- c('2', 4, '6', 8, '10')
  expect_equal(c(1,2,3), .NumericApplyRange(val1, NULL))
  expect_equal(c(1,2,3), .NumericApplyRange(val1, c(1,3)))
  expect_equal(c(1,2,3), .NumericApplyRange(val1, c(1,3,5))) # spurious val 5
  expect_equal(c(2,4,6,8,10), .NumericApplyRange(val2, NULL))
  expect_equal(c(4,6,8), .NumericApplyRange(val2, c(2,4)))
})

test_that("apply_ranges_works", {
  data <- matrix(c(1,2,3,
                   4,5,6,
                   7,8,9), nrow = 3, ncol = 3, byrow=TRUE)

  # Various ways to apply ranges that change nothing.
  expect_equal(data, .ApplyRanges(data, NULL, NULL))    # no ranges
  expect_equal(data, .ApplyRanges(data, c(1,3), NULL))  # full row range
  expect_equal(data, .ApplyRanges(data, c(1,3,99), NULL))  # spurious
  expect_equal(data, .ApplyRanges(data, NULL, c(1,3)))  # full col range
  expect_equal(data, .ApplyRanges(data, c(1,3), c(1,3))) # both full ranges

  # Trim just rows or just columns.
  expect_equal(matrix(c(1,2,3,
                        4,5,6), nrow=2, ncol=3, byrow=TRUE),
               .ApplyRanges(data, c(1,2), NULL))
  expect_equal(matrix(c(1,2,
                        4,5,
                        7,8), nrow=3, ncol=2, byrow=TRUE),
               .ApplyRanges(data, NULL, c(1,2)))

  # Trim both rows and columns.
  expect_equal(matrix(c(1,2,
                        4,5), nrow=2, ncol=2, byrow=TRUE),
               .ApplyRanges(data, c(1,2), c(1,2)))
})

test_that("extended_scores_works", {
  # Note that we expect the kriging in this simple case to preserve
  # the corner values 1,2,3,4.  In between, we expect to see additional
  # interpolated data points that show a progression from one original
  # value to the next.
  input_data1 <- matrix(c(1,2,
                          3,4), nrow=2, ncol=2, byrow=TRUE)
  expected_data1 <- matrix(c( 1.00, 1.33, 1.69, 2.00,
                              1.65, 1.98, 2.33, 2.65,
                              2.36, 2.69, 3.05, 3.36,
                              3.00, 3.32, 3.68, 4.00),
                           nrow=4, ncol=4, byrow=TRUE)
  actual_data1 <- .ExtendedScores(input_data1, 2)

  input_data2 <- matrix(c(1.0,2.0,
                          1.5,2.5,
                          3.0,4.0), nrow=3, ncol=2, byrow=TRUE)
  expected_data2 <- matrix(c(1.00, 1.29, 1.64, 2.00,
                             1.10, 1.39, 1.75, 2.09,
                             1.25, 1.55, 1.90, 2.24,
                             1.50, 1.84, 2.19, 2.50,
                             1.98, 2.30, 2.65, 2.96,
                             2.51, 2.84, 3.19, 3.50,
                             3.00, 3.32, 3.67, 4.00),
                           nrow=7, ncol=4, byrow=TRUE)
  actual_data2 = .ExtendedScores(input_data2,2)

  expect_equal(expected_data1, actual_data1, tolerance=1e-02)
  expect_equal(expected_data2, actual_data2, tolerance=1e-02)
})

# Hardcode some data, taken from the documented examples, to help test our
# computation of values fed to plotting functions.
.GetResponseAndScoreRowNames <- function() {
  c(0, 9.7656, 39.0626, 156.250, 625.0, 2500)
}

.GetResponseAndScoreColumnNames <- function() {
  return(c(0, 0.1954,  0.7812,   3.125,  12.5,   50))
}

.GetResponses1 <- function() {
  responses <- matrix(
    c(-22.95618, -3.76006, -18.13851, 41.17312, 53.33300, 71.30486,
       59.22438, 63.62513,  71.96303, 88.23972, 85.00458, 91.05190,
       60.24569, 63.12837,  70.89699, 86.48881, 92.93511, 93.72343,
       60.76255, 66.48256,  74.06599, 87.57480, 92.38781, 94.27577,
       60.62738, 61.94793,  76.73887, 85.90397, 93.44831, 94.06661,
       54.20937, 61.95692,  75.49612, 84.91041, 93.16868, 92.19736),
    nrow=6, ncol=6, byrow=TRUE)

  dimnames(responses) <- list(
    .GetResponseAndScoreRowNames(),
    .GetResponseAndScoreColumnNames()
  )

 return(responses)
}

.GetResponses2 <- function() {
  responses  <- matrix(
    c(-15.03824,   4.75548,  10.42966,  59.68784,  68.311079,  76.97270,
      -53.87510, -36.33353, -35.28140,   2.93297,  55.305805,  73.43772,
      -87.23946, -78.45438, -56.93535, -18.55597,  34.892080,  72.83862,
      -81.18400, -42.65184, -35.38275, -48.33612,  -0.395256,  59.19825,
      -51.47575, -40.63225, -48.97930, -58.40780, -21.649030,  36.12166,
       53.01917,  58.95660,  64.31239,  67.29226,  66.191418,  86.71329),
    nrow=6, ncol=6, byrow=TRUE)

  dimnames(responses) <- list(
    .GetResponseAndScoreRowNames(),
    .GetResponseAndScoreColumnNames()
  )

  return(responses)
}

.GetScores1 <- function () {
  scores <- matrix(
    c(0,  0.000000,  0.00000,  0.00000,  0.00000, 0.000000,
      0, 10.192587, 14.67011, 27.47910, 12.08755, 5.526368,
      0,  5.586906, 11.69542, 34.82529, 24.55566, 9.158482,
      0,  8.458418, 14.96854, 36.53241, 25.40108, 9.456784,
      0,  8.772475, 15.88465, 36.98242, 26.60314, 9.690151,
      0,  8.536783, 18.14888, 37.28297, 28.90676, 9.654226),
    nrow=6, ncol=6, byrow=TRUE)

  dimnames(scores) <- list(
    .GetResponseAndScoreRowNames(),
    .GetResponseAndScoreColumnNames()
  )

  return(scores)
}

.GetScores2 <- function() {
  scores <- matrix(
    c(0,   0.000000,   0.00000,   0.00000,   0.00000,   0.000000,
      0,  -9.147316, -16.23292, -19.87011,  -3.06092,   5.994880,
      0, -15.630544, -23.30691, -29.43377, -10.30092,   6.000334,
      0, -11.452155, -19.23089, -26.34151, -28.46625,  -6.057822,
      0, -14.793930, -22.50455, -29.86705, -33.87227, -14.592597,
      0, -24.888500, -21.73267, -18.37694, -14.59779,  -6.714009),
    nrow=6, ncol=6, byrow=TRUE)

  dimnames(scores) <- list(
    .GetResponseAndScoreRowNames(),
    .GetResponseAndScoreColumnNames()
  )

  return(scores)
}

.GetDrugPairs <- function() {
  return(
    data.frame(
      'drug.row' = c('ispinesib', 'canertinib'),
      'drug.col' = c('ibrutinib', 'ibrutinib'),
      'concUnit' = c(       'nM',        'nM'),
      'blockIDs' = c(          1,           2)))
}

test_that("compute_title_works", {
  expect_equal(18.04229, mean(.GetScores1()[-1,-1]), tolerance=1e-05)
  expect_equal("Average synergy: 18.042 (ZIP)",
               .ComputePlotTitle(.GetScores1(), NULL, NULL, 'ZIP'))

  expect_equal(-16.33909, mean(.GetScores2()[-1,-1]), tolerance=1e-05)
  expect_equal("Average synergy: -16.339 (ZIP)",
               .ComputePlotTitle(.GetScores2(), NULL, NULL, 'ZIP'))
})

test_that("round_up_to_ten_works", {
  expect_equal(10, .RoundUpToNextTen(1.5))
  expect_equal(10, .RoundUpToNextTen(-1.5))
  expect_equal(10, .RoundUpToNextTen(5))
  expect_equal(10, .RoundUpToNextTen(5.1))
  expect_equal(10, .RoundUpToNextTen(-9.9))
  expect_equal(20, .RoundUpToNextTen(10))
  expect_equal(40, .RoundUpToNextTen(.GetScores1()))
  expect_equal(40, .RoundUpToNextTen(.GetScores2()))
})

test_that("get_plot_data_works", {
  input_data <- list(
    dose.response.mats = list(.GetResponses1(), .GetResponses2()),
    scores = list(.GetScores1(), .GetScores2()),
    drug.pairs = .GetDrugPairs(),
    method = "ZIP"
  )

  plot_data1 <- .GetPlotData(input_data, 1, NULL, NULL, NULL, NULL, 3)
  plot_data2 <- .GetPlotData(input_data, 2, NULL, NULL, NULL, NULL, 3)

  expect_equal("Average synergy: 18.042 (ZIP)", plot_data1$data$plot.title)
  expect_equal("Average synergy: -16.339 (ZIP)", plot_data2$data$plot.title)
  expect_equal("ispinesib.ibrutinib", plot_data1$metadata$file.name.prefix)
  expect_equal("canertinib.ibrutinib", plot_data2$metadata$file.name.prefix)
  # TODO: make some kind of assertions about kriging output in mat.tmp
  expect_equal(3, plot_data1$data$len)
  expect_equal(3, plot_data2$data$len)
  expect_equal(.GetResponseAndScoreRowNames(), plot_data1$data$row.conc)
  expect_equal(.GetResponseAndScoreRowNames(), plot_data2$data$row.conc)
  expect_equal(.GetResponseAndScoreColumnNames(), plot_data1$data$col.conc)
  expect_equal(.GetResponseAndScoreColumnNames(), plot_data2$data$col.conc)
  expect_equal("ispinesib (nM)", plot_data1$data$drug.row)
  expect_equal("canertinib (nM)", plot_data2$data$drug.row)
  expect_equal("ibrutinib (nM)", plot_data1$data$drug.col)
  expect_equal("ibrutinib (nM)", plot_data2$data$drug.col)
  expect_equal(1, plot_data1$metadata$drug.blockID)
  expect_equal(2, plot_data2$metadata$drug.blockID)
  expect_equal(-40, plot_data1$data$start.point)
  expect_equal(-40, plot_data1$data$start.point)
  expect_equal(40, plot_data2$data$end.point)
  expect_equal(40, plot_data2$data$end.point)
})
