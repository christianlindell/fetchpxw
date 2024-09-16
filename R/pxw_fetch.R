
#' Retrieve data from a pxweb database
#'
#' @param url_text Url to a pxweb table. If the table belongs to Statistics
#' Sweden, the Institute of Economic Research. Jordbruksverket or Tillväxtanalys
#'  the url will be coverted to an api url.
#' @param filters_list A list of which selections should be made from various
#' parameters. If no selection is made, all variables are retrieved. If a variable
#'  is to be removed, it is specified with variable name = "e", for example Region = "e".
#' @param kod_kolumn A vector with names of the columns you also want codes for.
#' @param lopnr_istallet_for_koder Some pxweb databases are misconfigured so that
#' they don't have codes as identifiers, but insted uses serial numbers.
#' Set to TRUE if these are not identified correctly
#'
#' @return A tibble with the contents of the pxweb table
#'
#' @export
#'
#' @examples
#' url_text = pxw_create_api_url(paste0("https://www.statistikdatabasen.scb.se/",
#' "pxweb/sv/ssd/START__AA__AA0003__AA0003B/MotFlyktLanKon/"))
#' pxw_fetch(url_text = url_text, filters_list = list(Region = "12", Tid = "2022"),
#'     kod_kolumn = "region")
pxw_fetch <- function(url_text = url_text, filters_list = list(), kod_kolumn = NULL, lopnr_istallet_for_koder = FALSE) {

    # Gör om url:en om den inte är en api-url
    url_text = pxw_create_api_url(url_text = url_text)


    # Hänta en lista över alla parametrar och variabler i databasen
    dfvariabellista <- pxw_variables_list(url_text)

    # Byter ut kommunkoder, år m.m. till löpnummer i t.ex. Tillväxtanalys konstiga API
    filters_list <- pxw_param_till_lopnr(url_text, dfvariabellista, filters_list)

    # Hämta metadata om struktur och variabler
    pxvariabels <- pxweb_get(url_text)$variables

    # Skapa en tom lista för att bygga frågan till pxweb
    pxweb_query_list <- list()

    # Ta reda på hur många parametrar databasen har
    n_params <- length(pxvariabels)


    # Loopa igenom parameterlistan och bygg frågan till pxweb en parameter i taget
    for (i in 1:n_params) {

        # Kolla om parametern definiterats av användaren.
        # Om parametern både finns i de användardefinierade parametrarna och i pxwebs
        # parameterlista är värdet TRUE
        logical_anv_def_param <- !is.null(filters_list[[pxvariabels[[i]]$code]])

        # Hoppa över parametern om användaren angett att den ska elimineras
        if (logical_anv_def_param) {
            if (filters_list[[pxvariabels[[i]]$code]][[1]] == "e") next
        }

        # Om användaren definierat ett parametervärde, använd det. I annat fall hämta
        # alla variabler genom att sätta parametern = "*"
        if (logical_anv_def_param) {
            pxweb_query_list[[i]] <- filters_list[[pxvariabels[[i]]$code]]
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

    var_namn_egna_params <- names(filters_list)

    for (i in seq_along(var_namn_egna_params)) {
        parameter_namn <- var_namn_egna_params[i]

        varden_egen_par <- filters_list[[parameter_namn]]

        # Kolla om eliminering är tillåten
        # if (varden_egen_par[1] == "e") {
        dfvars_px <- dfvariabellista %>%
            filter(code == parameter_namn)

        eliminering_tillaten <- head(dfvars_px, 1) %>%
            pull(elimination)

        ska_elimineras <- varden_egen_par[1] == "e"

        if (!isTRUE(eliminering_tillaten) & isTRUE(ska_elimineras))  {
            error_mess <- paste0("Fel! Parametern ", parameter_namn, " f\u00E5r inte elimineras")
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
                error_msg <- paste0("Fel!  Felaktiga variabelv\u00E4rden angivna i parametern ", parameter_namn, ". \n", "Variablerna ", paste(felaktiga_variabelvarden, collapse=" ")  , " finns inte!")
                stop(error_msg)
            }
        }

    }

    # Hämta data
    px_data <-
        pxweb_get(url = url_text,
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

        if (str_detect(url_text, "sjv|tillvaxtanalys") | lopnr_istallet_for_koder)  {
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
#' If the table has a geographical variable, codes for this are retrieved, for
#' example municipality codes
#'
#'
#' @param url_text UURL to the tables web site or API.
#' @param niva_nchar If the table contains mixed geographic levels, it can
#' specify that only codes with a certain length should be retrieved.
#' level_nchar = 2 fetches for example county codes only.
#' @param lan_kod Vector of county codes. If lan_kod = c("10", "12") then
#' codes are retrieved only for Blekinge and Skåne.
#'
#' @return A vector with municipality codes or similar
#'
#' @export
#'
#' @examples
#' url_text <- paste0("https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/",
#' "START__AM__AM0210__AM0210A/ArbStatusM/")
#' pxw_get_region_codes(url_text, niva_nchar = 4, lan_kod = "12")
pxw_get_region_codes = function(url_text, niva_nchar = NULL, lan_kod = NULL) {

    url_text = pxw_create_api_url(url_text = url_text)

    if (str_detect(url_text, "statistik.tillvaxtanalys.se|statistik.sjv.se/")) {
        dfregionkod <- pxw_variables_list(url_text) %>%
            filter(code %in% c("Kommun", "kommun", "L\u00E4n", "l\u00E4n", "Riket", "riket",
                               "Region", "region", "Land", "land", "Country", "country", "reporting country")) %>%
            separate(col = "valueText", into = c("region_kod", "region"),
                     sep = " ", extra = "merge", remove = FALSE) %>%
            select(reg_koder = region_kod)
    } else {
        dfregionkod <- pxw_variables_list(url_text) %>%
            filter(code %in% c("Kommun", "kommun", "L\u00E4n", "l\u00E4n", "Riket", "riket",
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
            stop("Inga regionkoder kunde hittas. Finns inte angiven niv\u00E5?")
        }

        return(region_kod)
    } else {
        stop("Inga regioner kan hittas")
    }
}


# --------- Hämta lista över alla parametrar och variabler
#' Get a tibble with all parameters and variables in the table
#'
#' @param url_text URL to the tables web site or API.
#'
#' @return A tibble with all parameters and variables that can be used in the query
#' @export
#'
#' @examples
#' url_text <- paste0("https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/",
#' "START__AM__AM0210__AM0210A/ArbStatusM/")
#' pxw_variables_list(url_text)
pxw_variables_list <- function(url_text) {


    url_text = pxw_create_api_url(url_text = url_text)

    px_levels <- pxweb_get(url_text)

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
#' Get a tibble with all parameters in the table
#'
#' @param url_text URL to the tables web site or API.
#'
#' @return A tibble with all parameters
#' @export
#'
#' @examples
#' url_text <- paste0("https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/",
#' "START__AM__AM0210__AM0210A/ArbStatusM/")
#' pxw_parameters_list(url_text)
pxw_parameters_list <- function(url_text) {

    url_text = pxw_create_api_url(url_text = url_text)

    pxw_variables_list(url_text) %>%
        select(code, text, elimination) %>%
        distinct()
}

# -------- Hämta senaste tidpunkt
#' Get the latest time in the table (year, month, etc.)
#'
#' @param url_text URL to the tables web site or API.
#'
#' @return A text string with the last time found in the table
#' @export
#'
#' @examples
#' url_text <- paste0("https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/",
#' "START__AM__AM0210__AM0210A/ArbStatusM/")
#' pxw_get_last_period(url_text)
pxw_get_last_period = function(url_text) {


    url_text = pxw_create_api_url(url_text = url_text)

    return(max(pxw_get_periods(url_text = url_text)))
}

#' Get the first time in the table (year, month, etc.)
#'
#' @param url_text URL to the tables web site or API.
#'
#' @return A text string with the earliest time found in the table
#' @export
#'
#' @examples
#' url_text <- paste0("https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/",
#' "START__AM__AM0210__AM0210A/ArbStatusM/")
#' pxw_get_first_period(url_text)
pxw_get_first_period = function(url_text) {

    url_text = pxw_create_api_url(url_text = url_text)

    return(min(pxw_get_periods(url_text = url_text)))
}


# ---------- Skapa periodintervall
#' Create a period range for the times in the table
#'
#' @param url_text URL to the tables web site or API.
#' @param from_per Enter the earliest time in the range
#' @param to_per Enter the latest time in the range.
#'
#' @return A vector with all the times between from_per and to_per.
#' @export
#'
#' @examples
#' url_text <- paste0("https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/",
#' "START__AM__AM0210__AM0210A/ArbStatusM/")
#' pxw_get_period_intervall(url_text, from_per = "2024M01", to_per = "2024M03")
pxw_get_period_intervall = function(url_text, from_per = NULL, to_per = NULL) {

    url_text = pxw_create_api_url(url_text = url_text)

    periods <- pxw_get_periods(url_text = url_text)

    urval_periods <- periods[which(periods == from_per):which(periods == to_per)]

    return(urval_periods)

}


# ------------- Hämta alla tillgängliga perioder

#' Get all available periods
#'
#' @param url_text URL to the tables web site or API.
#'
#' @return A vector of all times available in the database, for example all
#' years for which there is data
#' @export
#'
#' @examples
#' url_text <- paste0("https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/",
#' "START__AM__AM0210__AM0210A/ArbStatusM/")
#' pxw_get_periods(url_text)
pxw_get_periods = function(url_text) {

    url_text = pxw_create_api_url(url_text = url_text)

    tider <- pxw_variables_list(url_text) %>%
        filter(code %in% c("\u00C5r", "\u00E5r", "Ar", "ar", "Tid", "tid", "M\u00E5nad", "m\u00E5nad", "Manad", "manad","Kvartal", "kvartal", "Year", "year", "Month",  "month", "Period") | isTRUE(.data$time)) %>%
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

# Convert a link to a database tables home page to an api url
#' Convert a link to a database tables home page to an api url
#'
#' @param url_text An URL to a tables as a character string.
#'
#' @return A URL to a API
#' @export
#'
#' @examples
#' url_text <- paste0("https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/",
#' "START__AM__AM0210__AM0210A/ArbStatusM/")
#' pxw_create_api_url(url_text)
pxw_create_api_url <- function(url_text) {

    if (str_detect(url_text, "statistikdatabasen.scb.se")) {

        txt_amnetab <- str_remove(url_text, "https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__") %>%
            str_replace_all("__", "/")

        start_api <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/"

        url_text <- paste0(start_api, txt_amnetab)

        url_text <- str_remove(url_text, "/$")
    }


    if (str_detect(url_text, "statistik.tillvaxtanalys.se/")) {
        url_text <- str_replace(URLdecode(url_text), "https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/Tillv\u00E4xtanalys statistikdatabas/Tillv\u00E4xtanalys", "https://statistik.tillvaxtanalys.se:443/PxWeb/api/v1/sv/Tillv\u00E4xtanalys") %>%
            str_replace_all(pattern = "__", replacement = "/")

        url_text <- str_remove(url_text, "/$")
    }

    if (str_detect(url_text, "statistik.sjv.se/PXWeb/pxweb/")) {

        url_text <- str_extract(url_text, ".*px")

        txt_amnetab <- str_remove(url_text, "https://statistik.sjv.se/PXWeb/pxweb/.*__") %>%
            str_replace_all("__", "/")

        start_api <- "https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets statistikdatabas/"

        url_text <- paste0(start_api, txt_amnetab)

        url_text <- str_remove(url_text, "/$")
    }

    if (str_detect(url_text, "http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/pxweb/sv/A_Folkhalsodata/")) {

        txt_amnetab <- str_remove(url_text, "http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/pxweb/sv/A_Folkhalsodata/") %>%
            str_replace_all("__", "/")

        start_api <- "http://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/v1/sv/"

        url_text <- paste0(start_api, txt_amnetab)

        url_text <- str_remove(url_text, "/$")
    }

    if (str_detect(url_text, "konj.se/PxWeb/pxweb")) {

        if (str_detect(url_text, "prognos")) {
            txt_amnetab <- str_remove(url_text, "https://prognos.konj.se/PxWeb/pxweb/sv/.*?/") %>%
                str_replace_all("__", "/")
        } else if (str_detect(url_text, "statistik")) {
            txt_amnetab <- str_remove(url_text, "https://statistik.konj.se/PxWeb/pxweb/sv/.*?/") %>%
                str_replace_all("__", "/")
        }


        if (str_detect(url_text, "prognos")) {
            start_api <- "https://prognos.konj.se:443/PxWeb/api/v1/sv/"
        } else if (str_detect(url_text, "statistik")) {
            start_api <- "https://statistik.konj.se:443/PxWeb/api/v1/sv/"
        }
        url_text <- paste0(start_api, txt_amnetab)

        url_text <- str_remove(url_text, "/$")
    }



    return(url_text)

}






# Convert codes to serial numbers in cases where the database is misconfigured
# and uses serial numbers instead of codes, for example instead of municipality
# codes.

pxw_param_till_lopnr <- function(url_text, dfvariabellista, filters_list) {

    if (length(filters_list) == 0) return(filters_list)

    if (str_detect(url_text,"tillvaxtanalys.se|statistik.sjv.se")) {

        param_namn <- names(filters_list)

        for (i in 1:length(filters_list)) {

            paramvarde <- filters_list[[i]]


            if (param_namn[[i]] %in% c("Kommun", "kommun", "L\u00E4n","l\u00E4n")) {
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

            filters_list[[i]] <- lopnr_motsv_parmvarde
        }
    }

    return(filters_list)
}


