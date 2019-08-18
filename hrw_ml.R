# Word2vec for menu description -------------------------------------------
library(h2o)
library(tm)

all_menus <- read_csv("~/Box Sync/hrw_menus.csv")

all_menus <- all_menus %>%
    mutate(item_desc = paste0(tolower(item),", ", description),
           item_desc = gsub(", ","_", item_desc),
           item_desc = gsub(" ", "_", tolower(item_desc)),
           item_desc = chartr("èûéí", "euei", item_desc))

h2o.init()

STOP_WORDS <- c(stopwords("en"), "na")
string_to_tokenize <- as.h2o(all_menus$item_desc[!is.na(all_menus$item_desc)])
tokenize_string <- h2o.tokenize(as.character(string_to_tokenize), ", ")

tokenize <- function(string_to_tokenize, stop.words = STOP_WORDS) {
    tokenized <- h2o.tokenize(as.character(string_to_tokenize), "_")

    # remove short words (less than 2 characters)
    tokenized.lengths <- h2o.nchar(tokenized.lower)
    tokenized.filtered <- tokenized.lower[is.na(tokenized.lengths) || tokenized.lengths >= 2,]
    # remove words that contain numbers
    tokenized.words <- tokenized.filtered[h2o.grep("[0-9]", tokenized.filtered, invert = TRUE, output.logical = TRUE),]

    # remove stop words
    tokenized.words[is.na(tokenized.words) || (! tokenized.words %in% STOP_WORDS),]
}

words <- tokenize(string_to_tokenize = string_to_tokenize)
print("Build word2vec model")
w2v.model <- h2o.word2vec(words,
                          model_id = "w2v_model",
                          vec_size = 20,
                          min_word_freq = 1,
                          window_size = 5,
                          init_learning_rate = 0.025,
                          sent_sample_rate = 0,
                          epochs = 300)

print("Sanity check - find synonyms for the word 'mango'")
print(h2o.findSynonyms(w2v.model, "pasta", count = 10))

item_desc_vecs <- h2o.transform(w2v.model, words, aggregate_method = "AVERAGE")


# `predict` conflicts with generic fn defined in R.stats
.predict <- function(item, w2v, gbm) {
    words <- tokenize(as.character(as.h2o(item)))
    item.vec <- h2o.transform(w2v, words, aggregate_method = "AVERAGE")
    h2o.predict(gbm, item.vec)
}




