#' @export

load_words <- function(path_to_excel_list){
        words <- readxl::read_excel(path_to_excel_list)
        words <- as.matrix(words) %>% as.character()
        words <- words[!(words=="?")]

        most_probable_encoding <- rvest::guess_encoding(x = words)[1, 1]
        Encoding(words)        <- most_probable_encoding
        words <- iconv(words,
                       from = most_probable_encoding,
                       to = "ASCII//TRANSLIT")

        words <- tolower(words)

        words
}
