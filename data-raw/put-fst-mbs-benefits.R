library(data.table)
library(magrittr)
library(hutils)

options("datatable.integer64" = "integer64")
options("datatable.showProgress" = FALSE)

fread <- function(...) {
  cat(crayon::blue(..1), "\n",
      "\t", crayon::green("start"), "\t", as.character(Sys.time()), "\n",
      sep = "")
  out <- data.table::fread(...)
  cat("\t", crayon::green("  end"), "\t", as.character(Sys.time()), "\n",
      sep = "")
  out
}

patient <- fread("~/../MBS/SAMPLE_PIN_LOOKUP.csv")

month2int <- function(x) {
  # source("~/hutils/R/Switch.R")
  the_months <- c("JAN", "FEB", "MAR",
                  "APR", "MAY", "JUN",
                  "JUL", "AUG", "SEP",
                  "OCT", "NOV", "DEC")
  fastmatch::fmatch(substr(x, 3L, 5L),
                    the_months)
}

set_year_month <- function(DT) {
  START <- Sys.time()
  stopifnot(is.data.table(DT))
  if (hasName(DT, "DOS")) {
    DT[, c("YOS", "MOS") := list(as.integer(substr(.BY[[1L]], 6, 9)),
                                 month2int(.BY[[1L]])),
       keyby = "DOS"]
    DT[, "DOS" := NULL]
  } else if (hasName(DT, "SPPLY_DT")) {
    DT[, c("YOS", "MOS") := list(as.integer(substr(.BY[[1L]], 6, 9)),
                                 month2int(.BY[[1L]])),
       keyby = "SPPLY_DT"]
    DT[, "SPPLY_DT" := NULL]
  } else {
    stop("No date column in DT.")
  }
  elapsed <- round(as.double(difftime(Sys.time(), START, units = "sec")) * 1000)

  cat("set_year_month: ", elapsed, "ms\n")
  DT
}



MBS <-
  lapply(dir("~/../MBS/",
             pattern = "MBS.*[12][0-9]{3}\\.csv$",
             full.names = TRUE),
         function(filename) {
           mbs <- fread(filename,
                        na.strings = c("", "NA"),
                        select = c("PIN", "DOS", "BENPAID", "FEECHARGED", "SCHEDFEE",
                                   "BILLTYPECD",
                                   "SAMPLEWEIGHT"))
           set_year_month(mbs)
         }) %>%
  rbindlist(use.names = TRUE, fill = TRUE)
setkey(MBS, PIN, YOS)
fst::write_fst(MBS, "data-raw/fst/mbs-benefits.fst")


PBS <-
  lapply(dir("~/../PBS/",
             pattern = "^PBS_SAMPLE.*[12][0-9]{3}\\.CSV$",
             full.names = TRUE),
         function(filename) {
           mbs <- fread(filename,
                        na.strings = c("", "NA"),
                        select = c("PTNT_ID", "SPPLY_DT", "BNFT_AMT", "PTNT_CNTRBTN_AMT"))
           set_year_month(mbs)
         }) %>%
  rbindlist(use.names = TRUE, fill = TRUE)
setnames(PBS, "PTNT_ID", "PIN")
setkey(PBS, PIN, YOS)
fst::write_fst(PBS, "data-raw/fst/pbs-benefits.fst")

