context("data_preparation")

prep <- data_preparation(
  site = "barro colorado island",
  stem = TRUE,
  taper_correction = TRUE,
  fill_missing = TRUE,
  use_palm_allometry = TRUE,
  flag_stranglers = TRUE,
  dbh_stranglers = 500,
  maxrel = 0.2,
  write_errors_to = NULL,
  DATA_path = NULL,
  exclude_interval = NULL,
  graph_problems_to = NULL
)

nms <- c(
  "treeID", "dbh1", "dbhc1", "status1", "code1", "hom1", "sp", "wsg",
  "agb1", "date1", "dbh2", "dbhc2", "status2", "code2", "hom2", "agb2",
  "date2", "broken", "agbl", "agb1.surv", "interval", "year", "gx", "gy",
  "quadrat", "name", "ID", "int", "code", "dHOM", "prod.g", "prod.r",
  "loss", "ficus", "prod.rel", "error", "error.loss"
)

describe("data_preparation", {
  it("outputs known output", {
    expect_known_output(
      as.data.frame(prep), "ref-data_preparation",
      update = FALSE, print = TRUE
    )
  })

  it("outputs expected structure", {
    expect_is(prep, "data.table")
    expect_is(prep, "data.frame")

    expect_named(prep, nms)
  })

  it("works with minimum mandatory argumets", {
    out <- data_preparation(site = "barro colorado island", stem = TRUE)
    expect_is(out, "data.frame")
    expect_named(out, nms)
  })
})
