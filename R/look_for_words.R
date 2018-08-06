#' @export

look_for_words <- function(text_data_frame, words_to_search_for){

        if(!is.data.frame(text_data_frame)){
                stop("The object 'text_data_frame' must be a data.frame (or similar...)")
        }

        if(!is.character(words_to_search_for)){
                stop("The object 'words_to_search_for' must be a character vector")
        }

        if(is.null(text_data_frame$doc_id) | is.null(text_data_frame$text)){
                stop("There must be at least these two columns in your text data.frame: 'doc_id' and 'text'")
        }

        if(!is.character(text_data_frame$text)){
                stop("The column 'text' of your text data.frame must be a character vector")
        }

        data_words        <- data.frame(t(as.matrix(words_to_search_for)),
                                        stringsAsFactors = F)
        names(data_words) <- words_to_search_for

        counts <- purrr::map_df(data_words, .f = function(x){
                purrr::map(text_data_frame[, "text"], stringr::str_count, x) %>%
                        unlist()
        })

        doc_id <- text_data_frame$doc_id
        counts <- cbind(doc_id, counts)

        class(counts) <- c("data.frame", "wordSearchResults_data_frame")

        counts
}
