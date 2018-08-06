#' @export

wordList_by_document <- function(wordSearchResults){

        if(!("wordSearchResults_data_frame" %in% class(wordSearchResults))){
                stop("The object 'wordSearchResults' must be a result of the function 'look_for_words()'.")
        }

        wordListData <- tibble::data_frame(doc_id   = wordSearchResults$doc_id,
                                           wordList = as.character(NA))

        for(i in 2:ncol(wordSearchResults)){
                word      <- names(wordSearchResults)[i]
                word_freq <- wordSearchResults[ ,i]

                doc_lines <- which(word_freq > 0)

                for(j in doc_lines){
                        doc_wordList <- wordListData[j, 'wordList']

                        if(is.na(doc_wordList)){
                                doc_wordList <- word
                        }else{
                                doc_wordList <- paste(c(doc_wordList, word), collapse = "; ")
                        }

                        wordListData[j, 'wordList'] <- doc_wordList
                }
        }
        wordListData
}
