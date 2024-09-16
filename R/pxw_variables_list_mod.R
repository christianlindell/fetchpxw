#' Adjust variable lists from misconfigured pxweb servers so that they are
#' following SCB standard
#'
#' @param url_str string, URL to an pxweb API
#' @param lopnr_in_values logical, TRUE if the server don't have categories in
#' the value param.
#'
#' @return A dataframe with all parameters and variables in use
#' @export
#'
#' @examples
#' link <- paste0("https://statistik.tillvaxtanalys.se/PxWeb/pxweb/sv/",
#' "Tillv%C3%A4xtanalys%20statistikdatabas/Tillv%C3%A4xtanalys",
#' "%20statistikdatabas__Konkurser%20och%20offentliga%20ackord/",
#' "konk_ar_lan_bransch_2009.px/")
#' pxw_variables_list_mod()
pxw_variables_list_mod <- function(url_str, lopnr_in_values = FALSE) {

    tider <- c("\u00C5r","\u00E5r", "Ar", "ar",  "Tid","tid",  "M\u00C5nad",
               "m\u00E5nad", "Manad", "manad", "Kvartal","kvartal", "year",
               "Month", "month", "Period", "period", "Time", "time")

    dfvar <- pxw_variables_list(url_str) %>%
        mutate(values_org = values)

    #  Om servern är rätt konfigurerad
    if (str_detect(url_str, "scb|folkhalsomyndigheten|konj.se")) {
        return(dfvar)
        # Om servern är felkonfigurerad och använder löpnr i values
    } else if (str_detect(url_str, "tillvaxtanalys|sjv.se") | isTRUE(lopnr_in_values)){
        dfvar <- dfvar %>%
            mutate(time = if_else(text %in% tider,
                                  TRUE, FALSE)) %>%
            mutate(values = if_else(str_detect(valueText, "\\b(?=\\w*[0-9])[\\w.+-]+\\b\\s"),
                                    str_extract(valueText, "\\b(?=\\w*[0-9])[\\w.+-]+\\b\\s"),
                                    valueText),
                   values = str_trim(values),
                   valueText = str_remove(valueText, "\\b(?=\\w*[0-9])[\\w.+-]+\\b\\s"))
    }

    return(dfvar)
}
