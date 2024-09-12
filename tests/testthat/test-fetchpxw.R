library(testthat)
library(fetchpxw)


test_that("pxw_skapa_api_url() SCB fungerar", {
    expect_equal(pxw_skapa_api_url("https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/"),
    "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0210/AM0210A/ArbStatusM")
})

