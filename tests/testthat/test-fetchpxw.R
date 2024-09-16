library(testthat)
library(fetchpxw)


test_that("pxw_skapa_api_url() SCB fungerar", {
    expect_equal(pxw_create_api_url("https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/"),
    "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0210/AM0210A/ArbStatusM")
})


test_that("pxw_skapa_api_url() Tillvaxtanalys fungerar", {
        expect_equal(pxw_create_api_url("https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/Tillv%C3%A4xtanalys%20statistikdatabas/Tillv%C3%A4xtanalys%20statistikdatabas__Konkurser%20och%20offentliga%20ackord/konk_ack_ar_1996.px/"),
                     "https://statistik.tillvaxtanalys.se:443/PxWeb/api/v1/sv/Tillväxtanalys statistikdatabas/Konkurser och offentliga ackord/konk_ack_ar_1996.px")

    })

test_that("pxw_skapa_api_url() FoHM fungerar", {
        expect_equal(pxw_create_api_url("http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/pxweb/sv/A_Folkhalsodata/A_Folkhalsodata__Z_ovrigdata__Arbete/Arbetsloshet_LanUTBNIVA.px/"),
                     "http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/v1/sv/A_Folkhalsodata/Z_ovrigdata/Arbete/Arbetsloshet_LanUTBNIVA.px")

    })

test_that("pxw_skapa_api_url() Jordbruksverket fungerar", {
    expect_equal(pxw_create_api_url("https://statistik.sjv.se/PXWeb/pxweb/sv/Jordbruksverkets%20statistikdatabas/Jordbruksverkets%20statistikdatabas__Sysselsattning/JO1101Q01.px/?rxid=5adf4929-f548-4f27-9bc9-78e127837625"),
                 "https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets statistikdatabas/Sysselsattning/JO1101Q01.px")

})

test_that("pxw_skapa_api_url() Konjinst statistikdatabas fungerar", {
    expect_equal(pxw_create_api_url("https://statistik.konj.se/PxWeb/pxweb/sv/KonjBar/KonjBar__ftgregional/Bartotreg.px/"),
                 "https://statistik.konj.se:443/PxWeb/api/v1/sv/KonjBar/ftgregional/Bartotreg.px")

})

test_that("pxw_skapa_api_url() Konjinst prognosdatabas fungerar", {
    expect_equal(pxw_create_api_url("https://prognos.konj.se/PxWeb/pxweb/sv/SenastePrognosen/SenastePrognosen__f03_internationellbnpochkonsumentpriser/F0301.px/"),
                 "https://prognos.konj.se:443/PxWeb/api/v1/sv/SenastePrognosen/f03_internationellbnpochkonsumentpriser/F0301.px")

})

test_that("pxw_get_region_codes() TVA levererar rätt antal regioner", {
    expect_equal(length(pxw_get_region_codes("https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/Tillv%C3%A4xtanalys%20statistikdatabas/Tillv%C3%A4xtanalys%20statistikdatabas__Konkurser%20och%20offentliga%20ackord/konk_ar_lan_bransch_2009.px/")), 21)
})
