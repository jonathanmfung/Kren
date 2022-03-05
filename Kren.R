library(Unicode)
library(syllable) # https://github.com/trinker/syllable, installed via R's pacman

library(ggplot2)
library(dplyr)

# Utilities ------------------------------

rm_punc <- function (string) {
    clean <- gsub("[[:punct:]]", "", string)
    return(clean)
}

count_syllable <- function (row) {
    # Assume that all punctuation is at beg or end
    word <- rm_punc(row$words)
    if (row$hangulp){
        # KR: Consider each Block as a syllable
        nchar(word)
    } else {
        # EN: Use syllable library algorithm
        count_string(word)
    }
}

hangul_range <- as.u_char_range("AC00..D7AF")

detect_lang <- function (chunk) {
    #' Takes in string composed of hangul and engligh (latin),
    #' returns 1 for hangul syllable code point
    #' returns NA for non-hangul syllable

    # Genius seems to use syllable blocks, not jamo
    # So don't have to worry about syllable de/composition
    uchar   <- as.u_char(utf8ToInt(chunk))
    matches <- u_char_match(uchar,hangul_range)
    return(list(chunk,matches))
}

centered_range <- function (len) {
    #' len cannot be negative, as it is a measure
    if (len == 0 | len == 1) {
        cen_seq <- 0
    }
    else if (len %% 2 == 0) {
        end <- len / 2 - 0.5
        cen_seq <- -end:end
    } else {
        end <- len %/% 2
        cen_seq <- -end:end
    }
    return(cen_seq)
}


# Processing ------------------------------

construct_hangulp <- function (line_str) {
    #' Separates a line into words and hangul boolean
    detected_list <- lapply(strsplit(line_str, " ")[[1]], detect_lang) # Words are space delimitted chunks
    len           <- length(detected_list)

    words         <- lapply(1:len, \(i) detected_list[[i]][[1]])
    hangulp       <- lapply(1:len, \(i) 1 %in% detected_list[[i]][[2]])
    list          <- rbind(words, hangulp) |> t()

    return(data.frame(list))
}

clean_section <- function (section) {
    #' Extracts section title from first line of lyrics verse
    lbrack    <- regexpr("\\[", section, perl=TRUE)[1]
    colon     <- regexpr("\\:", section, perl=TRUE)[1]

    title     <- substring(section, lbrack+1, colon-1)
    clean     <- rm_punc(title)

    return(clean)
}

parse_lyrics <- function (fileName, rm.adlibs = FALSE) {
    #' Parses a Genius lyrics txt file into sections and lines
    ## https://stackoverflow.com/questions/9068397/import-text-file-as-single-character-string
    paras        <- readChar(fileName, file.info(fileName)$size) |> strsplit("\n\n")
    paras_lines        <- sapply(paras[[1]], \(x) strsplit(x, "\n"))
    if (rm.adlibs) {
        ## adlibs are anything enclosed in parentheses, also trim leading/trailing whitespace
        paras_lines <- sapply(paras_lines, \(x) sapply(x, \(y) trimws(gsub("\\s*\\([^\\)]+\\)","", y )), USE.NAMES = FALSE ))
        ## Some lines are only adlibs, need to clean empty lines
        paras_lines <- sapply(paras_lines, \(x) x[ x != ""])
    }
    names(paras_lines) <- sapply(names(paras_lines), \(x) clean_section(x))
    names(paras_lines) <- make.names(names(paras_lines), unique = TRUE) # Allows unique Chorus etc.
    lyrx        <- sapply(paras_lines, \(x) x[-1]) # Remove section line

    return(lyrx)
}

extract_features <- function (words_hangulp_df) {
    #' words_hangulp_df is a df with words and hangulp from one line,
    #' which comes from construct_hangulp()
    # Lists
    kr_dist      <- unlist(words_hangulp_df$hangulp)
    syllables    <- apply(words_hangulp_df, 1, \(x) count_syllable(x))

    # Individual
    kr_word_prop      <- mean(kr_dist)
    kr_cnt       <- sum(kr_dist) # 1- kr_cnt == en_cnt
    word_cnt     <- length(kr_dist) # spaces delim words

    feature_df   <- data.frame(cbind(
        kr_dist, syllables
    ))
    feature_list <- list(kr_word_prop=kr_word_prop,
                         kr_cnt=kr_cnt,
                         word_cnt=word_cnt)

    return(list(feature_df, feature_list))
}

lyrics_to_data <- function (parsed) {
    raw_data   <- lapply(parsed,
                       \(x) lapply(x, construct_hangulp))
    features   <- lapply(raw_data,
                       \(x) lapply(x, extract_features))
    return(features)
}
# Main ------------------------------

lyrics_tree_data <- function (filename, rm.adlibs = FALSE) {
    p           <- parse_lyrics(filename, rm.adlibs = rm.adlibs)
    lyrics_data <- lyrics_to_data(p)
    return(lyrics_data)
}

parse_metadata <- function (symbol) {
    sym  <- enexpr(symbol)
    str  <- rlang::as_string(sym)
    meta <- strsplit(str, "\\.\\_\\.")[[1]]
    paste(meta, collapse = " - ")
}

theme_kren <- function () {
    theme(
        text = element_text(family = "Source Sans Pro"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20, family = "Source Code Pro"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(vjust=1,hjust=1, angle = 0),
        plot.title = element_text(hjust=0.5, vjust=0.5),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.title.align = 0.5)
}

section_tree_to_df <- function (section) {
    #' Converts a raw lyric section data to lyric section df
    #' Each Row represents one word (space delim)
    #' Refer to extract_features
    l   <- length(section)
    vec <- list()
    for (line in 1:l) {
        # Lists - word-based
        violin_xs    <- centered_range(section[[line]][[2]]$word_cnt)
        hangulp      <- section[[line]][[1]]$kr_dist
        syllabs      <- section[[line]][[1]]$syllables

        # Individual - line-based
        line_rel     <- line
        ## Including proportion and total count is a bit inefficient
        ## But proprtion seems most important, so should be readilyy available
        kr_word_prop      <- section[[line]][[2]]$kr_word_prop
        word_cnt    <- section[[line]][[2]]$word_cnt
        syllab_cnt    <- sum(syllabs)
        kr_syllab_prop <- sum(ifelse(hangulp, syllabs, 0))/syllab_cnt
        # https://stackoverflow.com/a/37238415
        kr_line_begp <- hangulp[1]
        kr_line_endp <- hangulp[length(hangulp)]

        vec[[line]]  <- data.frame(cbind(violin_xs, hangulp, syllabs,
                                        line_rel, kr_word_prop, word_cnt, kr_syllab_prop, syllab_cnt, kr_line_begp, kr_line_endp))
    }
    return(vec)
}

lyrics_tree_to_df <- function (lyrics_data, by_line = FALSE) {
    raw_list    <- lapply(lyrics_data,
                       \(x) x |> section_tree_to_df() |> bind_rows())
    df_raw      <- raw_list |> bind_rows(.id="sect") # sect is line-based data
    df_raw$sect <- factor(df_raw$sect,
                          levels = unique(df_raw$sect)) # Ensures correct section order in facets
    if (by_line) {df_raw <- df_raw %>%
                   ## unselect all word-based data, so that user can select line-based data
                   select(-violin_xs, -hangulp, -syllabs) %>% # line_rel ensures distinct() doesn't trim too much
                   distinct() %>%
                   mutate(line_tot=1:nrow(.)) }
    return(df_raw)
}

lyrics_violin <- function (lyrics_data) {
    # Maybe in Future: Put correspondying lyrics on each/across point
    meta_list     <- parse_metadata(!!enexpr(lyrics_data))
    raw_points_df <- lyrics_data %>%
        lyrics_tree_to_df() %>%
        select(violin_xs, syllabs, line_rel, hangulp, sect)

    xl <- max(raw_points_df$violin_xs) + 0.5
    yl <- max(raw_points_df$line_rel)

    ggplot(raw_points_df, aes(x=violin_xs, y=line_rel, color = as.factor(hangulp), size = syllabs)) +
        geom_point(shape = "square") +
        scale_size_area("Syllables") +
        facet_wrap(vars(sect), dir = "v") +
        xlim(-xl, xl) +
        labs(title = meta_list, col = "Language", x = "Words", y = "Line") +
        # scale_y_reverse does not work with breaks = breaks_width(1)
        # https://stackoverflow.com/a/39877048
        scale_y_continuous(trans = "reverse",
                           breaks = \(x) seq(1, max(x)), limits = c(yl+0.5,0.5)) +
        scale_color_manual(labels = c("English", "Korean"),
                           values = c("#FF7C00", "#00BFE0")) +
        guides(color = guide_legend(override.aes = list(size=5))) +
        theme_kren()
}

lyrics_chist <- function (lyrics_data) {
    meta_list    <- parse_metadata(!!enexpr(lyrics_data))
    raw_props_df <- lyrics_data %>%
        lyrics_tree_to_df(by_line = TRUE)

    xs     <- (\(v) tapply(seq_along(v), v, min)) (raw_props_df$sect) - 0.5
    l      <- nrow(xs)
    seg_df <- data.frame(
        sect = unique(as.character(raw_props_df$sect)),
        x = xs, xend = xs, y = rep(-1.3, l), yend = rep(-1.1, l))

    # https://stackoverflow.com/a/62995858
    ggplot(raw_props_df, aes(x=line_tot, y=kr_word_prop)) +
        geom_col(aes(fill=as.factor("Korean"))) +
        geom_col(aes(y = -(1-kr_word_prop), fill=as.factor("English"))) +

        geom_segment(data = seg_df,
                 aes(x = x, xend = xend, y= y, yend = yend), size = 0.5) +
        geom_text(data = seg_df,
                  aes(label = sect, x=x, y=(y + yend)/2),
                  vjust=0, hjust=0.5, nudge_x=0.2, size = 5) +

        labs(title = meta_list,
             fill = "Language", x = "Line Number", y = "En-Kr Word Proportion") +
        scale_fill_manual(values = c("#FF7C00", "#00BFE0")) +
        scale_x_reverse() +
        coord_flip() +
        theme_kren()
}

lyrics_series <- function (lyrics_data) {
    meta_list    <- parse_metadata(!!enexpr(lyrics_data))
    raw_props_df <- lyrics_data %>%
        lyrics_tree_to_df(by_line = TRUE)

    ggplot(raw_props_df, aes(x=line_tot, y=kr_word_prop)) +
        geom_step(direction = "mid") +
#    geom_linerange(aes(ymin = 0, ymax = kr_word_prop), size = 15) +
        labs(title = meta_list) +
        ylim(0, 1) +
        theme_kren()
}

## ----------------------------------------------------------------------------------------------------

fName1 <- 'lyrics/fromis_9-DM.txt'
fName2 <- 'lyrics/aespa-Savage.txt'
fName3 <- 'lyrics/STAYC-STEREOTYPE.txt'
fName4 <- 'lyrics/BLACKPINK-PrettySavage.txt'
fName <- fName1
dat <- lyrics_tree_data(fName)
fromis_9._.DM <- lyrics_tree_data(fName1)
aespa._.Savage <- lyrics_tree_data(fName2)
aespa._.Savage1 <- lyrics_tree_data(fName2, rm.adlibs = TRUE)
STAYC._.STEREOTYPE <- lyrics_tree_data(fName3)
BLACKPINK._.PrettySavage <- lyrics_tree_data(fName4)

fNameList <- c(fName1, fName2, fName3)
datList <- sapply(fNameList, lyrics_tree_data)
# datList[[2]] == lyrics_tree_data(fName2)
# datList[[N]][[2]] == song name

d <- lyrics_tree_to_df(dat)

# TODO: Analysis
# Calculate overall proportion of ENsyllable-KRword, ENword-KRword

## binomual regression
binom <- dat %>% lyrics_tree_to_df()  %>%
    select(sect, kr_word_prop, line_rel) %>% # line_rel ensures distinct() doesn't trim too much
    distinct() %>%
    mutate(line_tot=1:nrow(.))

glm(cbind(kr_word_prop, 1-kr_word_prop) ~ line_tot, family=binomial, data=binom)
## i dont have many variables to work with,


chisq.test(table(d$hangulp, d$syllabs))

glm(d$hangulp ~ d$syllabs, family = "binomial")

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

ggplot(d,aes(x=syllabs, y = hangulp, col = syllabs)) +
    geom_point() + binomial_smooth() + geom_jitter(width=0.1, height=0.1)

## time series
## library(fpp3)
## ## ma (moving average) is not in fpp3
## ## dat %>% data_to_df(by_line = TRUE)  %>%
## ##     mutate(ma = ma(kr_word_prop, n = 3)) %>%
## ##     ggplot(aes(x=line_tot, y=kr_word_prop)) +
## ##         geom_line() +
## ##     geom_line(aes(y=ma, col="red"))

## dat %>% lyrics_tree_to_df(by_line = TRUE)  %>%
##     tsibble(index=line_tot) %>%
##     model(ARIMA(kr_word_prop)) %>%
##     report()




lyrics_smry <- function(lyrics_data, stat = kr_word_prop, smry_f = mean) {
    #' stat should be any output of section_tree_to_df()
    #' smry_f is any one dimensional statistic, mean, median, mode, max, min, etc.
    f_str <- enexpr(smry_f)
    smry_df <- lyrics_data %>%
        lyrics_tree_to_df(by_line = TRUE)  %>% # removes duplicates
        group_by(sect) %>%
        summarize("{f_str}({{stat}})" := smry_f({{stat}}))
    return(smry_df)
}
## Contrast between without and with adlibs
lyrics_smry(aespa._.Savage1)$mean - lyrics_smry(aespa._.Savage)$mean

## non-parametrics
## http://www.mit.edu/~6.s085/notes/lecture5.pdf

# These compare song distributions, across sections
# maybe better to be across line_tot
q <- lyrics_smry(fromis_9._.DM, kr_word_prop)$mean
w <- lyrics_smry(aespa._.Savage, kr_word_prop)$mean
ks.test(q,w)
plot(ecdf(q), xlim = range(c(q,w)))
plot(ecdf(w), add = TRUE, col = 2)

t.test(q,w)
wilcox.test(q,w)

## these are across line_tot
e <- fromis_9._.DM %>% lyrics_tree_to_df(by_line = TRUE) %>% getElement("kr_word_prop")
r <- aespa._.Savage %>% lyrics_tree_to_df(by_line = TRUE) %>% getElement("kr_word_prop")
c <- STAYC._.STEREOTYPE %>% lyrics_tree_to_df(by_line = TRUE) %>% getElement("kr_word_prop")
v <- BLACKPINK._.PrettySavage %>% lyrics_tree_to_df(by_line = TRUE) %>% getElement("kr_word_prop")

e1 <- fromis_9._.DM %>% lyrics_tree_to_df(by_line = TRUE) %>% getElement("kr_syllab_prop")
r1 <- aespa._.Savage %>% lyrics_tree_to_df(by_line = TRUE) %>% getElement("kr_syllab_prop")
c1 <- STAYC._.STEREOTYPE %>% lyrics_tree_to_df(by_line = TRUE) %>% getElement("kr_syllab_prop")
v1 <- BLACKPINK._.PrettySavage %>% lyrics_tree_to_df(by_line = TRUE) %>% getElement("kr_syllab_prop")

comp_dist <- function(a, b) {
    print(ks.test(a,b))
    ggplot(data.frame(
        prop = c(a,b),
        song = c(rep(1,length(a)), rep(2,length(b)))
    ), aes(x=prop, col = as.factor(song))) +
        stat_ecdf(pad = FALSE, geom = "point") +
        stat_ecdf(pad = FALSE) +
        geom_density() +
        geom_dotplot(aes(x=prop,
                         fill = as.factor(song)),
                     stackgroups = TRUE,
                     binwidth = 1/75)
}

hist(e, breaks = c(seq(0,1,by = 1/16)), axes = FALSE)
axis(side = 1 , at = seq(0,1,by = 1/16))
axis(side = 2 , at = seq(0,25,by = 5))
box()

comp_dist(e, r)
comp_dist(e1, r1)

## trying to be pairwise
prop_list <- list(e=e,r=r,c=c,v=v)
sapply(prop_list, \(x) sapply(prop_list, \(y) list(ks.test(x,y)$p.value)))
sapply(prop_list, \(x) sapply(prop_list, \(y) list(t.test(x,y)$p.value)))
prop_list1 <- list(e1=e1,r1=r1,c1=c1,v1=v1)
sapply(prop_list1, \(x) sapply(prop_list, \(y) list(ks.test(x,y)$p.value)))

t.test(e,c)




beg <- fromis_9._.DM %>% lyrics_tree_to_df(by_line = TRUE) %>% getElement("kr_line_begp")
end <- fromis_9._.DM %>% lyrics_tree_to_df(by_line = TRUE) %>% getElement("kr_line_endp")
t <- table(beg, end)
chisq.test(t) # due to testing only goodness of fit
mcnemar.test(t)
fisher.test(t)

## don't think wilcox works because these are bools/counts
wilcox.test(t)
wilcox.test(beg,end)
wilcox.test(beg,end, paired = TRUE)

## simple paired t-test
## not sure if beg/end can be considered treatments
t.test(beg, end, paired = TRUE)

glm(end ~ beg, family = "binomial") %>% summary()
