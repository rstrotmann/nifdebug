#' Map EX domain rows onto NIF administration records
#'
#' For each row with `EVID == 1`, finds the corresponding SDTM **EX** record using
#' Option A: subject, calendar interval (`EXSTDTC`--`EXENDTC` with imputed end when
#' needed), `EXTRT` (optionally mapped from `ANALYTE`), and `EXDOSE` matching `DOSE`.
#'
#' Administration time is taken from `DTC` when present; otherwise `TRTDY` and
#' `RFSTDTC` from **DM** are used to approximate the calendar day (midnight in the
#' timezone of `RFSTDTC`).
#'
#' If `SRC_DOMAIN`, `SRC_SEQ`, and/or `SRC_TESTCD` already exist, they are
#' overwritten on administration rows and a warning is issued.
#'
#' @param ni A `nif` object.
#' @param sdtm An `sdtm` object with an `ex` domain. If a `dm` domain is present,
#'   `RFENDTC` / `RFSTDTC` are used when `EXENDTC` or administration `DTC` must be
#'   imputed.
#' @param analyte_extrt Optional named character vector: names are `ANALYTE` values
#'   in `ni`, values are the corresponding `EXTRT` in **EX** (e.g. `c(RS2023 =
#'   "RS2023")`). If `NULL`, `ANALYTE` is compared to `EXTRT` case-insensitively
#'   after trimming whitespace.
#'
#' @return `ni` with `SRC_DOMAIN` (`"EX"`), `SRC_SEQ` (`EXSEQ`), and `SRC_TESTCD`
#'   (`EXTESTCD` if present in **EX**, otherwise `NA`) set on administration rows.
#'   Non-administration rows are left unchanged for these columns (except new
#'   columns are added as `NA` where missing).
#'
#' @importFrom dplyr case_when left_join mutate rename select
#'
#' @import nif
#' @export
find_ex_source <- function(ni, sdtm, analyte_extrt = NULL) {
  nif:::validate_nif(ni)
  nif:::validate_sdtm(sdtm, "ex")
  if (!nif::has_domain(sdtm, "ex")) {
    stop("sdtm must contain domain 'ex'.")
  }

  src_cols <- c("SRC_DOMAIN", "SRC_SEQ", "SRC_TESTCD")
  overwritten <- intersect(src_cols, names(ni))
  if (length(overwritten) > 0) {
    warning(
      "Existing source columns will be overwritten on administration rows: ",
      paste(overwritten, collapse = ", "),
      call. = FALSE
    )
  }

  ex <- nif::domain(sdtm, "ex")
  ex <- nif:::lubrify_dates(ex)

  required_ex <- c("USUBJID", "EXTRT", "EXSTDTC", "EXSEQ", "EXDOSE")
  miss_ex <- setdiff(required_ex, names(ex))
  if (length(miss_ex) > 0) {
    stop("EX domain is missing required column(s): ", paste(miss_ex, collapse = ", "))
  }

  dm <- NULL
  if (nif::has_domain(sdtm, "dm")) {
    dm <- nif:::lubrify_dates(nif::domain(sdtm, "dm"))
  }

  if (!"EXENDTC" %in% names(ex)) {
    ex$EXENDTC <- as.POSIXct(NA_real_, origin = "1970-01-01")
  }

  if (!is.null(dm) && "RFENDTC" %in% names(dm)) {
    dm_rf <- dplyr::rename(
      dplyr::select(dm, "USUBJID", "RFENDTC"),
      RFENDTC_dm = "RFENDTC"
    )
    ex <- dplyr::left_join(ex, dm_rf, by = "USUBJID")
  } else {
    ex$RFENDTC_dm <- as.POSIXct(NA_real_, origin = "1970-01-01")
  }

  ex <- dplyr::mutate(
    ex,
    EXEND_eff = dplyr::case_when(
      !is.na(.data$EXENDTC) ~ .data$EXENDTC,
      !is.na(.data$RFENDTC_dm) ~ .data$RFENDTC_dm,
      TRUE ~ .data$EXSTDTC
    )
  )

  extrt_matches <- function(a, b) {
    tolower(trimws(as.character(a))) == tolower(trimws(as.character(b)))
  }

  dose_matches_vec <- function(dose, exdose) {
    !is.na(dose) &
      !is.na(exdose) &
      dplyr::near(as.numeric(dose), as.numeric(exdose), tol = 1e-8)
  }

  resolve_extrt <- function(analyte) {
    if (!is.null(analyte_extrt) && length(analyte_extrt) > 0L) {
      nm <- names(analyte_extrt)
      if (!is.null(nm) && analyte %in% nm) {
        return(unname(analyte_extrt[[analyte]]))
      }
    }
    analyte
  }

  admin_datetime <- function(row) {
    if ("DTC" %in% names(row) && length(row$DTC) && !is.na(row$DTC[1])) {
      return(as.POSIXct(row$DTC[1]))
    }
    if (is.null(dm) || !nrow(dm)) {
      return(as.POSIXct(NA_real_, origin = "1970-01-01"))
    }
    if (!all(c("TRTDY", "USUBJID") %in% names(row))) {
      return(as.POSIXct(NA_real_, origin = "1970-01-01"))
    }
    uid <- row$USUBJID[1]
    tr <- row$TRTDY[1]
    srow <- dm[dm$USUBJID == uid, , drop = FALSE]
    if (!nrow(srow) || !"RFSTDTC" %in% names(srow) || is.na(srow$RFSTDTC[1])) {
      return(as.POSIXct(NA_real_, origin = "1970-01-01"))
    }
    rf <- srow$RFSTDTC[1]
    tz <- attr(rf, "tzone", exact = TRUE)
    if (length(tz) != 1L || is.na(tz) || !nzchar(tz)) {
      tz <- "UTC"
    }
    bd <- as.Date(rf) + (tr - 1)
    as.POSIXct(paste0(bd, " 00:00:00"), tz = tz)
  }

  out <- ni
  n <- nrow(out)
  if (!"SRC_DOMAIN" %in% names(out)) {
    out$SRC_DOMAIN <- rep(NA_character_, n)
  }
  if (!"SRC_SEQ" %in% names(out)) {
    out$SRC_SEQ <- rep(NA_real_, n)
  }
  if (!"SRC_TESTCD" %in% names(out)) {
    out$SRC_TESTCD <- rep(NA_character_, n)
  }

  admin_idx <- which(out$EVID == 1)
  issues <- character(0)

  for (idx in admin_idx) {
    row <- out[idx, , drop = FALSE]
    uid <- row$USUBJID[1]
    if (is.na(uid)) {
      issues <- c(issues, sprintf("Row %s: missing USUBJID.", idx))
      next
    }
    analyte <- row$ANALYTE[1]
    extrt_want <- resolve_extrt(analyte)
    dose <- row$DOSE[1]
    if (is.null(dose) || (length(dose) && is.na(dose))) {
      issues <- c(issues, sprintf("Row %s: missing DOSE.", idx))
      next
    }

    t_admin <- admin_datetime(row)
    if (is.na(t_admin)) {
      issues <- c(
        issues,
        sprintf(
          "Row %s: cannot derive administration datetime (set DTC or TRTDY + DM$RFSTDTC).",
          idx
        )
      )
      next
    }

    sub_ex <- ex[ex$USUBJID == uid, , drop = FALSE]
    if (!nrow(sub_ex)) {
      issues <- c(issues, sprintf("Row %s: no EX rows for subject.", idx))
      next
    }

    cand <- sub_ex[
      extrt_matches(sub_ex$EXTRT, extrt_want) &
        dose_matches_vec(dose, sub_ex$EXDOSE) &
        !is.na(sub_ex$EXSTDTC) &
        t_admin >= sub_ex$EXSTDTC &
        t_admin <= sub_ex$EXEND_eff,
      ,
      drop = FALSE
    ]

    if (nrow(cand) == 1L) {
      out$SRC_DOMAIN[idx] <- "EX"
      out$SRC_SEQ[idx] <- cand$EXSEQ[1]
      if ("EXTESTCD" %in% names(cand)) {
        out$SRC_TESTCD[idx] <- as.character(cand$EXTESTCD[1])
      } else {
        out$SRC_TESTCD[idx] <- NA_character_
      }
    } else if (nrow(cand) == 0L) {
      issues <- c(
        issues,
        sprintf("Row %s: no matching EX row (interval, EXTRT, EXDOSE).", idx)
      )
      out$SRC_DOMAIN[idx] <- NA_character_
      out$SRC_SEQ[idx] <- NA_real_
      out$SRC_TESTCD[idx] <- NA_character_
    } else {
      issues <- c(
        issues,
        sprintf("Row %s: multiple EX rows match; SRC fields set to NA.", idx)
      )
      out$SRC_DOMAIN[idx] <- NA_character_
      out$SRC_SEQ[idx] <- NA_real_
      out$SRC_TESTCD[idx] <- NA_character_
    }
  }

  if (length(issues) > 0L) {
    warning(paste(issues, collapse = "\n"), call. = FALSE)
  }

  out
}
