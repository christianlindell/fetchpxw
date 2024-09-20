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
#' pxw_variables_list_mod(link)
pxw_variables_list_mod <- function(url_str, lopnr_in_values = FALSE) {

    tider <- c("\u00C5r","\u00E5r", "Ar", "ar",  "Tid","tid",  "M\u00C5nad",
               "m\u00E5nad", "Manad", "manad", "Kvartal","kvartal", "year",
               "Month", "month", "Period", "period", "Time", "time")

    korrekt_konfig_sajter <- paste(c("scb", "folkhalsomyndigheten", "konj.se"), collapse = "|")

    felkonfig_sajter <- paste(c("tillvaxtanalys", "sjv.se"), collapse = "|")

    # Funktion för att om första ordet kan vara en kod och antal ord
    # En kod definieras av att den inte enbart består av bokstäver utan av enbart
    # siffror, en blandning av siffror och bokstäver. Den får även innehålla ".+-_/".
    # För att första ordet ska kunna vara en kod måste den följas av minst ett ord till
    check_first_word <- function(text) {
        words <- str_split(text, "\\s+")[[1]]
        if (length(words) < 2) {
            return(FALSE)
        }
        first_word <- words[1]
        return(str_detect(first_word, "^[0-9a-zA-Z\u00e5\u00e4\u00f6\u00c5\u00c4\u00d6\\-\\+_/]+$") && str_detect(first_word, "[0-9]"))
    }

    dfvar <- pxw_variables_list(url_str) %>%
        mutate(values_org = values,
               valueText_org = valueText)




    #  Om servern är rätt konfigurerad
    if (str_detect(url_str, korrekt_konfig_sajter)) {
        dfvar <- dfvar %>%
            mutate(is_lopnr = FALSE)
        return(dfvar)


        # Om servern är felkonfigurerad och använder löpnr i values
        # } else if (str_detect(url_str, felkonfig_sajter) | isTRUE(lopnr_in_values)){
    } else {
        dfvar <- dfvar %>%
            group_by(code) %>%
            mutate(values = as.numeric((values))) %>%
            mutate(is_lopnr = if_else(max(values, na.rm = TRUE) + 1 ==  n(),

                                      true = TRUE,
                                      false =  FALSE)) %>%
            ungroup() %>%
            group_by(code) %>%
            mutate(values = as.numeric((values))) %>%
            mutate(ar_kod = sapply(valueText, check_first_word)) %>%
            ungroup() %>%
            mutate(values = if_else(ar_kod,
                                    str_extract(valueText, "(?<!\\S)-?\\b(?=\\w*[0-9])[-\\w.+]*\\b-?"),
                                    valueText),
                   valueText = str_remove(valueText, "(?<!\\S)-?\\b(?=\\w*[0-9])[-\\w.+]*\\b-?")) %>%
            select(-ar_kod) %>%
            mutate(valueText = str_trim(valueText))
    }

    # Om värdena i valuText inte är unika efter att den inledande koddelen raderats så
    # ersätt dem med orginalvärdena som finns sparade i kolumnen valueText_org
    dfvar <- dfvar %>%
        group_by(code) %>%
        mutate(valueText = case_when(
            n_distinct(valueText) == n() ~ valueText,
            TRUE ~ valueText_org
        )) %>%
        ungroup() %>%
        mutate(valueText = str_trim(valueText))

    return(dfvar)
}
