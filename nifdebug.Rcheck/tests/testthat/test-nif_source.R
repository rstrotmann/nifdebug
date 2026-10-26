test_that("find_ex_source maps admin to EX by interval, EXTRT, and DOSE", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE, ~EXTESTCD,
    "S1", 1, "DRUG", "2000-01-01T08:00:00", "2000-01-03T20:00:00", 100, "DOSE"
  )
  dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~RFENDTC,
    "S1", "2000-01-01T00:00:00", "2000-12-31T00:00:00"
  )
  sdtm <- nif::sdtm(list(ex = ex, dm = dm))
  ni <- nif::nif(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~USUBJID, ~ANALYTE, ~DOSE, ~DTC,
    1, 0, 100, 1, 1, NA, "S1", "DRUG", 100, as.POSIXct("2000-01-02 12:00:00", tz = "UTC")
  ))
  out <- find_ex_source(ni, sdtm)
  adm <- out$EVID == 1
  expect_identical(out$SRC_DOMAIN[adm], "EX")
  expect_identical(out$SRC_SEQ[adm], 1)
  expect_identical(out$SRC_TESTCD[adm], "DOSE")
})

test_that("analyte_extrt maps ANALYTE to EXTRT", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE, ~EXTESTCD,
    "S1", 1, "DRUGX", "2000-01-01T08:00:00", "2000-01-03T20:00:00", 50, "DOSE"
  )
  dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~RFENDTC,
    "S1", "2000-01-01T00:00:00", "2000-12-31T00:00:00"
  )
  sdtm <- nif::sdtm(list(ex = ex, dm = dm))
  ni <- nif::nif(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~USUBJID, ~ANALYTE, ~DOSE, ~DTC,
    1, 0, 50, 1, 1, NA, "S1", "A_INTERNAL", 50, as.POSIXct("2000-01-02 12:00:00", tz = "UTC")
  ))
  out <- find_ex_source(ni, sdtm, analyte_extrt = c(A_INTERNAL = "DRUGX"))
  adm <- out$EVID == 1
  expect_identical(out$SRC_SEQ[adm], 1)
})

test_that("multiple EX matches set SRC fields to NA", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE, ~EXTESTCD,
    "S1", 1, "DRUG", "2000-01-01T08:00:00", "2000-01-05T20:00:00", 100, "A",
    "S1", 2, "DRUG", "2000-01-02T08:00:00", "2000-01-06T20:00:00", 100, "B"
  )
  dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~RFENDTC,
    "S1", "2000-01-01T00:00:00", "2000-12-31T00:00:00"
  )
  sdtm <- nif::sdtm(list(ex = ex, dm = dm))
  ni <- nif::nif(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~USUBJID, ~ANALYTE, ~DOSE, ~DTC,
    1, 0, 100, 1, 1, NA, "S1", "DRUG", 100, as.POSIXct("2000-01-03 12:00:00", tz = "UTC")
  ))
  expect_warning(
    out <- find_ex_source(ni, sdtm),
    "multiple EX rows"
  )
  adm <- out$EVID == 1
  expect_true(is.na(out$SRC_SEQ[adm]))
})

test_that("DOSE must match EXDOSE", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE, ~EXTESTCD,
    "S1", 1, "DRUG", "2000-01-01T08:00:00", "2000-01-03T20:00:00", 100, "DOSE"
  )
  dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~RFENDTC,
    "S1", "2000-01-01T00:00:00", "2000-12-31T00:00:00"
  )
  sdtm <- nif::sdtm(list(ex = ex, dm = dm))
  ni <- nif::nif(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~USUBJID, ~ANALYTE, ~DOSE, ~DTC,
    1, 0, 99, 1, 1, NA, "S1", "DRUG", 99, as.POSIXct("2000-01-02 12:00:00", tz = "UTC")
  ))
  expect_warning(
    out <- find_ex_source(ni, sdtm),
    "no matching EX row"
  )
  adm <- out$EVID == 1
  expect_true(is.na(out$SRC_SEQ[adm]))
})

test_that("existing SRC columns trigger overwrite warning", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE, ~EXTESTCD,
    "S1", 1, "DRUG", "2000-01-01T08:00:00", "2000-01-03T20:00:00", 100, "DOSE"
  )
  dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~RFENDTC,
    "S1", "2000-01-01T00:00:00", "2000-12-31T00:00:00"
  )
  sdtm <- nif::sdtm(list(ex = ex, dm = dm))
  ni <- nif::nif(tibble::tribble(
    ~ID, ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~USUBJID, ~ANALYTE, ~DOSE, ~DTC, ~SRC_SEQ,
    1, 0, 100, 1, 1, NA, "S1", "DRUG", 100, as.POSIXct("2000-01-02 12:00:00", tz = "UTC"), 99
  ))
  expect_warning(
    find_ex_source(ni, sdtm),
    "overwritten"
  )
})
