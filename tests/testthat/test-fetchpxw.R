library(testthat)
library(fetchpxw)


test_that("pxw_skapa_api_url() SCB fungerar", {
  expect_equal(
    pxw_create_api_url("https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/"),
    "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0210/AM0210A/ArbStatusM"
  )
})


test_that("pxw_skapa_api_url() Tillvaxtanalys fungerar", {
  expect_equal(
    pxw_create_api_url("https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/Tillv%C3%A4xtanalys%20statistikdatabas/Tillv%C3%A4xtanalys%20statistikdatabas__Konkurser%20och%20offentliga%20ackord/konk_ack_ar_1996.px/"),
    "https://statistik.tillvaxtanalys.se:443/PxWeb/api/v1/sv/Tillväxtanalys statistikdatabas/Konkurser och offentliga ackord/konk_ack_ar_1996.px"
  )
})

test_that("pxw_skapa_api_url() FoHM fungerar", {
  expect_equal(
    pxw_create_api_url("http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/pxweb/sv/A_Folkhalsodata/A_Folkhalsodata__Z_ovrigdata__Arbete/Arbetsloshet_LanUTBNIVA.px/"),
    "http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/v1/sv/A_Folkhalsodata/Z_ovrigdata/Arbete/Arbetsloshet_LanUTBNIVA.px"
  )
})

test_that("pxw_skapa_api_url() Jordbruksverket fungerar", {
  expect_equal(
    pxw_create_api_url("https://statistik.sjv.se/PXWeb/pxweb/sv/Jordbruksverkets%20statistikdatabas/Jordbruksverkets%20statistikdatabas__Sysselsattning/JO1101Q01.px/?rxid=5adf4929-f548-4f27-9bc9-78e127837625"),
    "https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets statistikdatabas/Sysselsattning/JO1101Q01.px"
  )
})

test_that("pxw_skapa_api_url() Konjinst statistikdatabas fungerar", {
  expect_equal(
    pxw_create_api_url("https://statistik.konj.se/PxWeb/pxweb/sv/KonjBar/KonjBar__ftgregional/Bartotreg.px/"),
    "https://statistik.konj.se:443/PxWeb/api/v1/sv/KonjBar/ftgregional/Bartotreg.px"
  )
})

test_that("pxw_skapa_api_url() Konjinst prognosdatabas fungerar", {
  expect_equal(
    pxw_create_api_url("https://prognos.konj.se/PxWeb/pxweb/sv/SenastePrognosen/SenastePrognosen__f03_internationellbnpochkonsumentpriser/F0301.px/"),
    "https://prognos.konj.se:443/PxWeb/api/v1/sv/SenastePrognosen/f03_internationellbnpochkonsumentpriser/F0301.px"
  )
})

test_that("pxw_get_region_codes() TVA levererar rätt antal regioner", {
  expect_equal(length(pxw_get_region_codes("https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/Tillv%C3%A4xtanalys%20statistikdatabas/Tillv%C3%A4xtanalys%20statistikdatabas__Konkurser%20och%20offentliga%20ackord/konk_ar_lan_bransch_2009.px/")), 21)
})


link_scb <- "https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/"

link_tva <- "https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/Tillv%C3%A4xtanalys%20statistikdatabas/Tillv%C3%A4xtanalys%20statistikdatabas__Konkurser%20och%20offentliga%20ackord/konk_ar_kommun_1996.px/"

link_konj_stat <- "https://statistik.konj.se/PxWeb/pxweb/sv/KonjBar/KonjBar__ftgregional/Bartotreg.px/"

link_konj_progn <- "https://prognos.konj.se/PxWeb/pxweb/sv/SenastePrognosen/SenastePrognosen__f03_internationellbnpochkonsumentpriser/F0301.px/"

link_jordbruksv <- "https://statistik.sjv.se/PXWeb/pxweb/sv/Jordbruksverkets%20statistikdatabas/Jordbruksverkets%20statistikdatabas__Sysselsattning/JO1101Q01.px/?rxid=5adf4929-f548-4f27-9bc9-78e127837625"

link_foh <- "http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/pxweb/sv/A_Folkhalsodata/A_Folkhalsodata__Z_ovrigdata__Arbete/Arbetsloshet_LanUTBNIVA.px/"


#  SCB
test_that("pxw_skapa_api_url() SCB fungerar", {
    expect_equal(
        pxw_create_api_url("https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/"),
        "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0210/AM0210A/ArbStatusM"
    )
})

test_that("pxw_get_periods() SCB fungerar", {
    expect_equal(
        pxw_get_periods(link_scb)[1],
        "2020M01"
    )
})

test_that("pxw_get_region_codes() SCB fungerar", {
    expect_equal(
        pxw_get_region_codes(link_scb)[1],
        "00"
    )
})

test_that("pxw_variables_list() SCB fungerar", {
    expect_equal(
        pxw_variables_list(link_scb)[1,1] %>% pull(),
        "Region"
    )
})

test_that("pxw_parameters_list() SCB fungerar", {
    expect_equal(
        pxw_parameters_list(link_scb) %>% nrow(),
        6
    )
})

test_that("pxw_parameters_list() SCB fungerar", {
    expect_equal(
        pxw_fetch(link_scb, filters_list = list(Region = "1280", Alder = "20-64", Tid = "2021M05"),
                  kod_kolumn = c("region", "kön")) %>% ncol(),
        18
    )
})


# Tillväxtanalys

test_that("pxw_get_periods() TVA fungerar", {
    expect_equal(
        pxw_get_periods(link_tva)[1],
        "1996"
    )
})

test_that("pxw_get_region_codes() TVA fungerar", {
    expect_equal(
        pxw_get_region_codes(link_tva)[1],
        "0114"
    )
})

test_that("pxw_variables_list() TVA fungerar", {
    expect_equal(
        pxw_variables_list(link_tva)[1,7] %>% pull(),
        "0"
    )
})

test_that("pxw_parameters_list() TVA fungerar", {
    expect_equal(
        pxw_parameters_list(link_tva) %>% nrow(),
        3
    )
})

test_that("pxw_parameters_list() TVA fungerar", {
    expect_equal(
        pxw_fetch(link_tva, filters_list = list(år = "2023", kommun = "0114")) %>% nrow(),
        2
    )
})


# Konjunkturistitutet_prognos

# test_that("pxw_get_region_codes() KI Prognos fungerar", {
#     expect_equal(
#         pxw_get_region_codes(link_konj_progn)[1],
#         "0114"
#     )
# })

test_that("pxw_variables_list() KI Prognos fungerar", {
    expect_equal(
        pxw_variables_list(link_konj_progn)[1,3] %>% pull(),
        "F0301Worldbnp"
    )
})

test_that("pxw_parameters_list() KI Prognos fungerar", {
    expect_equal(
        pxw_parameters_list(link_konj_progn) %>% nrow(),
        3
    )
})

test_that("pxw_parameters_list() KI Prognos fungerar", {
    expect_equal(
        pxw_fetch(link_konj_progn, filters_list = list(variabel = "F0301Worldbnp", period = "1994"))[4] %>% pull(),
        3.2
    )
})

# Konjunkturistitutet_Statistik



test_that("pxw_get_region_codes() KI Statistik fungerar", {
    expect_equal(
        pxw_get_region_codes(link_konj_stat)[1],
        "SE11"
    )
})

test_that("pxw_variables_list() KI Statistik fungerar", {
    expect_equal(
        pxw_variables_list(link_konj_stat)[1,3] %>% pull(),
        "SE11"
    )
})

test_that("pxw_parameters_list() KI Statistik fungerar", {
    expect_equal(
        pxw_parameters_list(link_konj_stat) %>% nrow(),
        4
    )
})

test_that("pxw_parameters_list() KI Statistik fungerar", {
    expect_equal(
        pxw_fetch(link_konj_stat, filters_list = list(Region = "SE12", Period = "2024Q2", Serie = "S"))[5] %>% pull(),
        91
    )
})

# Jordbruksverket

test_that("pxw_get_region_codes() Jordbruksverket fungerar", {
    expect_equal(
        pxw_get_region_codes(link_jordbruksv)[1],
        "00"
    )
})

test_that("pxw_variables_list() Jordbruksverket fungerar", {
    expect_equal(
        pxw_variables_list(link_jordbruksv)[1,3] %>% pull(),
        "00"
    )
})

test_that("pxw_parameters_list() Jordbruksverket fungerar", {
    expect_equal(
        pxw_parameters_list(link_jordbruksv) %>% nrow(),
        7
    )
})

test_that("pxw_parameters_list() Jordbruksverket fungerar", {
    expect_equal(
        pxw_fetch(link_jordbruksv, filters_list = list(Län = "00", Företagsform = "Enskilt företag",
                                                       År = "2023", Företagsroll = "Företagare",
                                                       Kön = "Totalt",
                                                       Tabelluppgift = "Värde"))[8] %>% pull(),
        19202
    )
})

# FoH

test_that("pxw_get_region_codes() FoH fungerar", {
    expect_equal(
        pxw_get_region_codes(link_foh)[1],
        "00"
    )
})

test_that("pxw_variables_list() FoH fungerar", {
    expect_equal(
        pxw_variables_list(link_foh)[1,3] %>% pull(),
        "00"
    )
})

test_that("pxw_parameters_list() FoH fungerar", {
    expect_equal(
        pxw_parameters_list(link_foh) %>% nrow(),
        4
    )
})

test_that("pxw_parameters_list() FoH fungerar", {
    expect_equal(
        pxw_fetch(link_foh, filters_list = list(Region = "00", Utbildningsnivå = "1",
                                                År = "2023", Kön = "1"))[5] %>% pull(),
        23.1
    )
})

