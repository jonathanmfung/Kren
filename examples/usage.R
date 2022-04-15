source("../Kren.R")

Artist._.Song <- lyrics_tree_data("dummy_lyrics.txt")

lyrics_violin(Artist._.Song)
ggsave("violin.jpg", width = 1500, height = 1800, units = "px", dpi = 200)

lyrics_chist(Artist._.Song)
ggsave("chist.jpg", width = 1700, height = 2000, units = "px", dpi = 200)

lyrics_series(Artist._.Song)
ggsave("series.jpg", width = 1600, height = 1000, units = "px", dpi = 200)

lyrics_smry(Artist._.Song)
lyrics_begend(Artist._.Song)

## Use with two different sets of lyrics
#lyrics_comp(Artist._.Song, Artist._.Song)
