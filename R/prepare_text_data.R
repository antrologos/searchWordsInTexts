#' @export

prepare_text_data <- function(text_data_frame, doc_id_column_name, text_column_name){

        text_data_frame$doc_id <- text_data_frame[[doc_id_column_name]]
        text_data_frame$text   <- text_data_frame[[text_column_name]]

        most_probable_encoding <- rvest::guess_encoding(x = text_data_frame$text)[1, 1]
        Encoding(text_data_frame$text)    <- most_probable_encoding
        text_data_frame$text <- iconv(text_data_frame$text,
                           from = most_probable_encoding,
                           to = "ASCII//TRANSLIT")

        text_data_frame$text = tolower(text_data_frame$text)

        text_data_frame[ , c("doc_id", "text")]
}


