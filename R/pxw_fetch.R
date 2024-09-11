
library(dplyr)
library(pxweb)
library(purrr)
library(stringr)
library(pxweb)


#' Hämta data från en pxweb-databas
#'
#' @param url Url till en pxwebtabell. Om tabellen tillhör SCB, Konjunkturinstitutet.
#' Jordbruksverket eller Tillväxtanalys omvandlas url:en till tabellens webbsida
#' till en api-url.
#' @param params_lista en lista med vilka urval som ska göras från olika parametrar.
#' Om inget urval görs hämtas alla variabler. Om en variabel ska tas bort anges det med variabelnamn = "e", till exempel Kön = "e".
#' @param kod_kolumn En vektor med namn på de kolumner man även vill ha koder för.
#' @param lopnr_istallet_for_koder Vissa pxwebdatabaser är felgjorda så att man
#' har koder som identifierare, utan löpnummer. Ange till TRUE om inte dessa identifieras
#'  korrekt
#'
#' @return En tibble med tabellinnehållet i vald pxweb-tabell
#'
#' @export
#'
#' @examples
#' url = pxw_skapa_api_url("https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003B/MotFlyktLanKon/")
#' dfscb_urval <- pxw_fetch(url, params_lista = list(Region = "12", Tid = "2022"), kod_kolumn = "region")
pxw_fetch <- function(url, params_lista = list(), kod_kolumn = NULL, lopnr_istallet_for_koder = FALSE) {

    # Gör om url:en om den inte är en api-url
    url = pxw_skapa_api_url(url)


    # Hänta en lista över alla parametrar och variabler i databasen
    dfvariabellista <- pxw_variablellista(url)

    # Byter ut kommunkoder, år m.m. till löpnummer i t.ex. Tillväxtanalys konstiga API
    params_lista <- pxw_param_till_lopnr(dfvariabellista, params_lista)

    # Hämta metadata om struktur och variabler
    pxvariabels <- pxweb_get(url)$variables

    # Skapa en tom lista för att bygga frågan till pxweb
    pxweb_query_list <- list()

    # Ta reda på hur många parametrar databasen har
    n_params <- length(pxvariabels)


    # Loopa igenom parameterlistan och bygg frågan till pxweb en parameter i taget
    for (i in 1:n_params) {

        # Kolla om parametern definiterats av användaren.
        # Om parametern både finns i de användardefinierade parametrarna och i pxwebs
        # parameterlista är värdet TRUE
        logical_anv_def_param <- !is.null(params_lista[[pxvariabels[[i]]$code]])

        # Hoppa över parametern om användaren angett att den ska elimineras
        if (logical_anv_def_param) {
            if (params_lista[[pxvariabels[[i]]$code]][[1]] == "e") next
        }

        # Om användaren definierat ett parametervärde, använd det. I annat fall hämta
        # alla variabler genom att sätta parametern = "*"
        if (logical_anv_def_param) {
            pxweb_query_list[[i]] <- params_lista[[pxvariabels[[i]]$code]]
        } else {
            pxweb_query_list[[i]] <- "*"
        }

        # Namnge elementen i listan till pxweb
        names(pxweb_query_list)[[i]] <-  pxvariabels[[i]]$code
    }

    # Ta bort tomma element ur listan (variabler som ska elimineras)
    pxweb_query_list <- Filter(Negate(is.null), pxweb_query_list)


    # FELKONTROLL

    # Kolla om de parametervärden som angivits finns med i variabellistan

    var_namn_egna_params <- names(params_lista)

    for (i in seq_along(var_namn_egna_params)) {
        parameter_namn <- var_namn_egna_params[i]

        varden_egen_par <- params_lista[[parameter_namn]]

        # Kolla om eliminering är tillåten
        # if (varden_egen_par[1] == "e") {
        dfvars_px <- dfvariabellista %>%
            filter(code == parameter_namn)

        eliminering_tillaten <- head(dfvars_px, 1) %>%
            pull(elimination)

        ska_elimineras <- varden_egen_par[1] == "e"

        if (!isTRUE(eliminering_tillaten) & isTRUE(ska_elimineras))  {
            error_mess <- paste0("Fel! Parametern ", parameter_namn, " får inte elimineras")
            stop(error_mess)
        }

        # Kolla om den av användaren angivna parametern finns och om värdena stämmer

        varden_param <- dfvariabellista %>%
            filter(code == parameter_namn) %>%
            pull(values)

        if ((length(varden_param) == 0)) {
            error_msg <- paste0("Fel! Parametern ", parameter_namn, " finns inte!")
            stop(error_msg)
        }

        felaktiga_variabelvarden <- setdiff(varden_egen_par, varden_param)

        if (!is_empty(felaktiga_variabelvarden)) {
            if (!isTRUE(ska_elimineras)) {
                error_msg <- paste0("Fel!  Felaktiga variabelvärden angivna i parametern ", parameter_namn, ". \n", "Variablerna ", paste(felaktiga_variabelvarden, collapse=" ")  , " finns inte!")
                stop(error_msg)
            }
        }

    }

    # Hämta data
    px_data <-
        pxweb_get(url = url,
                  query = pxweb_query_list)



    # Convert to data.frame
    px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text") %>%
        tibble()

    # Om databasen är korrekt formatterad med koder i values och inte sammanblandning
    # av namn och koder ska den här koden användas för att lägga till kolumner med koder

    # Lägg på koder för de kolumner som finns angivna i parametern kod_kolumn

    if (!is.null(kod_kolumn)) {
        dfcodes <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "code") %>%
            select(any_of(kod_kolumn))  %>%
            tibble()

        if (ncol(dfcodes) > 0) {

            names_df_codes <- names(dfcodes)
            names(dfcodes) <- paste0(names_df_codes, "_kod")

            px_data_frame <- bind_cols(dfcodes, px_data_frame)

        }


        # Hantera siter som inte följer korrekt formatering och har löpnr i values
        # istället för koder

        if (str_detect(url, "sjv|tillvaxtanalys") | lopnr_istallet_for_koder)  {
            for (index in seq_along(kod_kolumn)) {

                df <- dfvariabellista %>%
                    filter(text == kod_kolumn[index]) %>%
                    select(-values) %>%
                    mutate(values = valueText) %>%
                    mutate(values = if_else(str_detect(values, "\\b(?=\\w*\\d)\\w+\\b\\s"),
                                            str_extract(values, "\\b(?=\\w*\\d)\\w+\\b\\s"), values)) %>%
                    mutate(values = str_trim(values)) %>%
                    select(values, valueText)

                names(df)[1] <- paste0(kod_kolumn[index], "_kod")
                names(df)[2] <- kod_kolumn[index]

                px_data_frame <- px_data_frame %>%
                    select(-any_of(paste0(kod_kolumn[index], "_kod")))

                px_data_frame <- df %>%
                    right_join(px_data_frame, by = kod_kolumn[index])

            }

        }

    }

    return(px_data_frame)
}



# --------------------------------- HJÄLPFUNKTIONER ------------------------




# ------------ - Hämta regionkoder
#' Om tabellen har en geografisk variabel hämtas koder för denna, till exempel kommunkoder
#'
#' @param url URL till tabellen, antingen på webbsajt eller api-URL
#' @param niva_nchar Om tabellen innehåller blandade geografiska nivåer så kan man ange att enbart koder med en viss längd ska hämtas. niva_nchar = 2 hämtar till exempel till exempel enbart länskoder.
#' @param lan_kod Vektor med länskoder. Om lan_kod = c("10", "12") så hämtas koder enbart för Blekinge och Skåne.
#'
#' @return En vektor med kommunkoder eller dylikt
#' @export
#'
#' @examples
#' url <- "https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/"
#' pxw_get_region_codes(url, niva_nchar = 4, lan_kod = "12")
pxw_get_region_codes = function(url, niva_nchar = NULL, lan_kod = NULL) {

    url = pxw_skapa_api_url(url)

    if (str_detect(url, "statistik.tillvaxtanalys.se|statistik.sjv.se/")) {
        dfregionkod <- pxw_variablellista(url) %>%
            filter(code %in% c("Kommun", "kommun", "Län", "Län", "Riket", "riket",
                               "Region", "region", "Land", "land", "Country", "country", "reporting country")) %>%
            separate(col = "valueText", into = c("region_kod", "region"),
                     sep = " ", extra = "merge", remove = FALSE) %>%
            select(reg_koder = region_kod)
    } else {
        dfregionkod <- pxw_variablellista(url) %>%
            filter(code %in% c("Kommun", "kommun", "Län", "Län", "Riket", "riket",
                               "Region", "region", "Land", "land", "Country", "country", "reporting country")) %>%
            select(reg_koder = values)
    }

    region_kod <- dfregionkod %>% pull(reg_koder)

    if (length(region_kod) > 0 ) {


        if (!is.null(niva_nchar)) {
            region_kod <- region_kod[nchar(region_kod) == niva_nchar]
            if (niva_nchar == 2 & !is.null(region_kod)) region_kod <- region_kod[region_kod != "00"]
        }

        if (!is.null(lan_kod)) region_kod <- region_kod[substr(region_kod, 1, 2) %in% lan_kod]

        if (is.null(region_kod) | purrr::is_empty(region_kod) | length(region_kod) == 0) {
            stop("Inga regionkoder kunde hittas. Finns inte angiven nivå?")
        }

        return(region_kod)
    } else {
        stop("Inga regioner kan hittas")
    }
}


# --------- Hämta lista över alla parametrar och variabler
#' Skapa en lista över tabellens alla parametrar och deras variabler
#'
#' @param url URL till tabellens webbsida eller API.
#'
#' @return En tibble med alla parametrar och variabler som kan användas.
#' @export
#'
#' @examples
#' url <- "https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/"
#' pxw_variablellista(url)
pxw_variablellista <- function(url) {


    url = pxw_skapa_api_url(url)

    px_levels <- pxweb_get(url)

    langd_var_list <- length(px_levels$variables)

    fvariabellista <- function(index) {

        df <- tibble(code =  px_levels$variables[[index]]$code,
                     text = px_levels$variables[[index]]$text,
                     values = px_levels$variables[[index]]$values,
                     valueText =  px_levels$variables[[index]]$valueTexts,
                     elimination =  px_levels$variables[[index]]$elimination,
                     time =  px_levels$variables[[index]]$time
        )


    }


    df <- map(.x = 1:langd_var_list, ~fvariabellista(index = .x)) %>%
        list_rbind()

    return(df)
}


# ------- Hämta lista över parametrar och om de kan elimineras eller inte
#' Visa tabellens alla parametrare och queryns struktur
#'
#' @param url
#'
#' @return
#' @export
#'
#' @examples
#' url <- "https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/"
#' pxw_visa_parameterlista(url)
pxw_visa_parameterlista <- function(url) {

    url = pxw_skapa_api_url(url)

    pxw_variablellista(url) %>%
        select(code, text, elimination) %>%
        distinct()
}

# -------- Hämta senaste tidpunkt
#' Hämta senaste tidpunkt i tabellen (år, månad etc)
#'
#' @param url URL till tabellen webbsida eller API.
#'
#' @return En textsträng med senaste tidpunkt som finns i tabellen
#' @export
#'
#' @examples
#' url <- "https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/"
#' pxw_get_last_period(url)
pxw_get_last_period = function(url) {


    url = pxw_skapa_api_url(url)

    return(max(pxw_get_periods(url = url)))
}

#' Hämta första tidpunkt i tabellen (år, månad etc)
#'
#' @param url URL till tabellen webbsida eller API.
#'
#' @return En textsträng med den tidigaste tidpunkten som finns i tabellen
#' @export
#'
#' @examples
#' url <- "https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/"
#' pxw_get_first_period(url)
pxw_get_first_period = function(url) {

    url = pxw_skapa_api_url(url)

    return(min(pxw_get_periods(url = url)))
}


# ---------- Skapa periodintervall
#' Skapa ett periodintervall för de tidpunkter som finns i tabelan
#'
#' @param url URL till tabellen webbsida eller API.
#' @param from_per Ange den tidigaste tidpunkten i intervallet
#' @param to_per Ange senaste tidpunkten i intervallet.
#'
#' @return En vektor med alla de tider som finns mellan from_per och to_per.
#' @export
#'
#' @examples
#' url <- "https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/"
#' pxw_get_period_intervall(url, from_per = NULL, to_per = NULL)
pxw_get_period_intervall = function(url, from_per = "2020M06", to_per = "2024M01") {

    url = pxw_skapa_api_url(url)

    periods <- pxw_get_periods(url = url)

    urval_periods <- periods[which(periods == from_per):which(periods == to_per)]

    return(urval_periods)

}


# ------------- Hämta alla tillgängliga perioder

#' Hämta alla tillgängliga perioder
#'
#' @param url URL till tabellen webbsida eller API.
#'
#' @return En vektor med alla tider som finns i databasen, till exempel alla år det finns data för
#' @export
#'
#' @examples
#' url <- "https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/"
#' pxw_get_periods(url, from_per = NULL, to_per = NULL)
pxw_get_periods = function(url) {

    url = pxw_skapa_api_url(url)

    tider <- pxw_variablellista(url) %>%
        filter(code %in% c("År", "år", "Ar", "ar", "Tid", "tid", "Månad", "månad", "Kvartal", "kvartal", "Year", "year", "Month",  "month", "Period") | isTRUE(.data$time)) %>%
        select(valueText) %>%
        distinct() %>%
        arrange(valueText) %>%
        pull(valueText)

    if (length(tider) > 0 ) {
        return(tider)
    } else {
        stop("Inga perioder kan hittas")
    }
}

# Gör om en länk till databastabellens hemsida till en api-url
#' Gör om en länk till databastabellens hemsida till en api-url
#'
#' @param url
#'
#' @return
#' @export
#'
#' @examples
#' #' url <- "https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/"
#' pxw_skapa_api_url(url)
pxw_skapa_api_url <- function(url) {

    if (str_detect(url, "statistikdatabasen.scb.se")) {

        txt_amnetab <- str_remove(url, "https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__") %>%
            str_replace_all("__", "/")

        start_api <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/"

        url <- paste0(start_api, txt_amnetab)

        url <- str_remove(url, "/$")
    }


    if (str_detect(url, "statistik.tillvaxtanalys.se/")) {
        url <- str_replace(URLdecode(url), "https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/Tillväxtanalys statistikdatabas/Tillväxtanalys", "https://statistik.tillvaxtanalys.se:443/PxWeb/api/v1/sv/Tillväxtanalys") %>%
            str_replace_all(pattern = "__", replacement = "/")
    }

    if (str_detect(url, "statistik.sjv.se/PXWeb/pxweb/")) {

        url <- str_extract(url, ".*px")

        txt_amnetab <- str_remove(url, "https://statistik.sjv.se/PXWeb/pxweb/.*__") %>%
            str_replace_all("__", "/")

        start_api <- "https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets statistikdatabas/"

        url <- paste0(start_api, txt_amnetab)

        url <- str_remove(url, "/$")
    }

    if (str_detect(url, "http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/pxweb/sv/A_Folkhalsodata/")) {

        txt_amnetab <- str_remove(url, "http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/pxweb/sv/A_Folkhalsodata/") %>%
            str_replace_all("__", "/")

        start_api <- "http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/v1/sv/"

        url <- paste0(start_api, txt_amnetab)

        url <- str_remove(url, "/$")
    }

    if (str_detect(url, "konj.se/PxWeb/pxweb")) {

        txt_amnetab <- str_remove(url, "https://statistik.konj.se/PxWeb/pxweb/sv/") %>%
            str_replace_all("__", "/")

        start_api <- "https://statistik.konj.se:443/PxWeb/api/v1/sv/"

        url <- paste0(start_api, txt_amnetab)

        url <- str_remove(url, "/$")
    }

    return(url)

}





# Gör om koder till löpnummer
#' Gör om koder till löpnummer i de fall databasen är felkonfigurerad och använder löpnummer istället för koder, till exempel istället för kommunkoder.
#'
#' @param dfvariabellista En lista på variabler som hämtats med pwx_variabellista(url)
#' @param params_lista En lista på de parametrar som ingår i tabellen.
#'
#' @return
#' @export
#'
#' @examples
#'
pxw_param_till_lopnr <- function(dfvariabellista, params_lista) {

    if (length(params_lista) == 0) return(params_lista)

    if (str_detect(url,"tillvaxtanalys.se|statistik.sjv.se")) {

        param_namn <- names(params_lista)

        for (i in 1:length(params_lista)) {

            paramvarde <- params_lista[[i]]


            if (param_namn[[i]] %in% c("Kommun", "kommun", "Län","län")) {
                lopnr_motsv_parmvarde <- dfvariabellista %>%
                    filter(code == param_namn[[i]]) %>%
                    separate(col = "valueText", into = c("region_kod", "region"),
                             sep = " ", extra = "merge", remove = FALSE) %>%
                    filter(region_kod %in% paramvarde) %>%
                    pull(values)
            } else {
                lopnr_motsv_parmvarde <- dfvariabellista %>%
                    filter(code == param_namn[[i]]) %>%
                    filter(valueText %in% paramvarde) %>%
                    pull(values)
            }

            params_lista[[i]] <- lopnr_motsv_parmvarde
        }
    }

    return(params_lista)
}


