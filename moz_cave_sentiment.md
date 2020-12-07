Natural Language Processing with Moz and Cave: Sentiment Analysis
================
Hilary Dotson
Updated December 6, 2020

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(magrittr)
library(readr)
library(stringr)
library(tidytext)
```

# Introduction

As a sociologist by training with a background in both quantitative and
quantitative methods, I have been curious about what natural language
processing can do (and *can’t* do) to inform the social word. I have
some excellent colleagues who are great with MLP, and I’d like to have a
better understanding of the kinds of work they do.

In reading through publicly available information on natural language
processing, I found Silge and Robinson’s text, *[Text Mining with R: A
Tidy Approach](https://www.tidytextmining.com/)*, where the authors work
through several elementary examples to help readers grasp how to use R
to conduct sentiment analysis, analyze word and document frequencies,
and conduct topic modeling, primarily by using old books (both fiction
and non-fiction). Cool stuff.

So, it would be fun to do something similar But, as some of you might
now, I spent many years cultivating an interesting/unique/little strange
taste in music. And two of my favorite artists are [Nick
Cave](https://en.wikipedia.org/wiki/Nick_Cave) and
[Morrissey](https://en.wikipedia.org/wiki/Morrissey). I’ve seen both
perform live in the last few years and those are two major highlights of
my life (say what you will about what that means about me). And as a
heavily tattooed woman, two of my tattoos are of them. So, clearly
influential to some degree.

So who are Nick Cave and Steven Patrick Morrissey? Let’s just talk about
their commonalities. Both are around 60 years old. Both have careers
spanning 35+ years. Both have been involved in at least one other major
project besides their current work (Morrissey as the front-man of The
Smiths; Nick Cave in The Boys Next Door, The Birthday Party, and
Grinderman). Both are still active musicians and have achieved success
across mainstream audiences, despite their more niche roots in the 1970s
and 1980s.

Both are a little weird, a little dark, and vegetarians/vegans – so we
have a lot in common (in my head, at least). Though, I’ll certainly
distance myself from one of their political beliefs. You can guess which
one.

# Why Bring NLP to Morrissey and Nick Cave?

Well for one, because both would probably dislike it. Maybe they’ll
write songs about it.

Seriously though, I thought it would be fun to do a very basic sentiment
analysis using their songs. *In the coming weeks, I’ll perform topic
modeling on the same corpus–stay tuned.*

From my personal take, Morrissey tends to write songs that are sad,
witty, and sound much happier when performed than when read. And in my
read, Nick Cave tends to write tragic stories in songs, which sound sad
when performed and read. Morrissey’s lyrics are imbued with literature,
politics, and opinion; Nick Cave’s with religious imagery, love, and
death.

And since this is just for fun, I figured I’d settle the debate I’ve had
internally for 15+ years once and for all:

**Who’s gloomier? Morrissey or Nick Cave?**

In support of this effort, **I’ll test a hypothesis using sentiment
analysis.** My hypothesis is that *Nick Cave’s songs will be more
negative, in general, than Morrissey’s.*

I will mostly be following along with the process detailed by Silge and
Robinson in *Tidy Text Mining*.

# Objectives

  - Successfully create a catalog of songs that includes a unique ID,
    artist name, album name, song name, and lyrics for each song.
  - Import .csv file into R which contains song lyrics for the selected
    songs.
  - Completely some very basic data cleaning for text analysis.
  - Conduct a rudimentary sentiment analysis on the two artists’ songs.

# Data

I manually created a .csv file that contained all the songs and lyrics
from both artists’ studio album discographies (excluding albums that
only included covers). Note that I did not QC the lyrics and took them
as they appeared on available lyric websites. At some point, I’ll do a
similar project using webscraping to collect the data. *Note that the
lyrics are property of their owners, not not not me.*

Nick Cave and The Bad Seeds have 16 studio albums that met the
aforementioned criteria, containing a total of 160 songs. Morrissey has
12 studio solo albums, containing a total of 132 songs. Note that not
all songs were written by Cave and Morrissey, respectively, though
nearly all were. Certainly a project for another day, though.

``` r
songlist <- readr::read_csv("moz_cave_songlist.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   id = col_double(),
    ##   artist = col_character(),
    ##   album_name = col_character(),
    ##   song_name = col_character(),
    ##   lyrics = col_character()
    ## )

``` r
colnames(songlist)
```

    ## [1] "id"         "artist"     "album_name" "song_name"  "lyrics"

Just to get an idea as to what the lyrics wind up looking like, let’s
take a look at the 149th row’s song lyrics (Nick Cave and the Bad Seeds
- Skeleton Tree). Yes, I deliberately picked it.

``` r
# Wrapping the songlist object with as.data.frame so the entire lyrics print.  
# Since the songlist object is a tibble, it otherwise would only print the
# first few words.

as.data.frame(songlist[149,5])
```

    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   lyrics
    ## 1 Sunday morning, skeleton tree\nOh, nothing is for free\nIn the window, a candle\nWell, maybe you can see\nFallen leaves thrown across the sky\nA jittery TV\nGlowing white like fire\nNothing is for free\nI called out, I called out\nRight across the sea\nBut the echo comes back empty\nAnd nothing is for free\n\nSunday morning, skeleton tree\nPressed against the sky\nThe jittery TV\nGlowing white like fire\nAnd I called out, I called out\nRight across the sea\nI called out, I called out\nThat nothing is for free\n\nAnd it's alright now\nAnd it's alright now\nAnd it's alright now

Those are the full lyrics to the song. You’ll notice indications of line
breaks (“\\n”), words starting with uppercase characters, and other
forms of punctuation. So, we need to clean this up.

# Basic Data Cleaning

From here, I used dplyr and tidytext packages to do some basic data
cleaning and transformation.

First, I transformed the data set so that each observation corresponds
to a singular word, rather than an entire song:

``` r
tidy_songlist <- songlist %>%
    tidytext::unnest_tokens(word, lyrics)

head(tidy_songlist)
```

    ## # A tibble: 6 x 5
    ##      id artist                    album_name           song_name word     
    ##   <dbl> <chr>                     <chr>                <chr>     <chr>    
    ## 1  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche i        
    ## 2  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche stepped  
    ## 3  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche into     
    ## 4  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche an       
    ## 5  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche avalanche
    ## 6  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche it

``` r
str(tidy_songlist)
```

    ## tibble [68,988 x 5] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ id        : num [1:68988] 1001 1001 1001 1001 1001 ...
    ##  $ artist    : chr [1:68988] "Nick Cave & The Bad Seeds" "Nick Cave & The Bad Seeds" "Nick Cave & The Bad Seeds" "Nick Cave & The Bad Seeds" ...
    ##  $ album_name: chr [1:68988] "From her to eternity" "From her to eternity" "From her to eternity" "From her to eternity" ...
    ##  $ song_name : chr [1:68988] "Avalanche" "Avalanche" "Avalanche" "Avalanche" ...
    ##  $ word      : chr [1:68988] "i" "stepped" "into" "an" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   id = col_double(),
    ##   ..   artist = col_character(),
    ##   ..   album_name = col_character(),
    ##   ..   song_name = col_character(),
    ##   ..   lyrics = col_character()
    ##   .. )

As we can see, the tidy\_songlist data set contains 68,988 words.

Next, I looked at the most frequently occurring words to help inform
decisions on removing stopwords.

``` r
tidy_songlist %>%
  dplyr::count(word, sort = TRUE)
```

    ## # A tibble: 6,458 x 2
    ##    word      n
    ##    <chr> <int>
    ##  1 the    3520
    ##  2 and    2499
    ##  3 i      1904
    ##  4 you    1780
    ##  5 a      1420
    ##  6 to     1315
    ##  7 in     1069
    ##  8 of      968
    ##  9 my      904
    ## 10 me      875
    ## # ... with 6,448 more rows

Words like these top ones (articles, pronouns, etc.) are probably safe
to remove. Notice that there are 6,458 unique words in these data. Let’s
remove them and see what we are left with.

``` r
tidy_rm_stopwords <- tidy_songlist %>%
  dplyr::anti_join(stop_words)
```

    ## Joining, by = "word"

``` r
head(tidy_rm_stopwords)
```

    ## # A tibble: 6 x 5
    ##      id artist                    album_name           song_name word     
    ##   <dbl> <chr>                     <chr>                <chr>     <chr>    
    ## 1  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche stepped  
    ## 2  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche avalanche
    ## 3  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche covered  
    ## 4  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche soul     
    ## 5  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche hunchback
    ## 6  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche sleep

``` r
str(tidy_rm_stopwords)
```

    ## tibble [23,873 x 5] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ id        : num [1:23873] 1001 1001 1001 1001 1001 ...
    ##  $ artist    : chr [1:23873] "Nick Cave & The Bad Seeds" "Nick Cave & The Bad Seeds" "Nick Cave & The Bad Seeds" "Nick Cave & The Bad Seeds" ...
    ##  $ album_name: chr [1:23873] "From her to eternity" "From her to eternity" "From her to eternity" "From her to eternity" ...
    ##  $ song_name : chr [1:23873] "Avalanche" "Avalanche" "Avalanche" "Avalanche" ...
    ##  $ word      : chr [1:23873] "stepped" "avalanche" "covered" "soul" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   id = col_double(),
    ##   ..   artist = col_character(),
    ##   ..   album_name = col_character(),
    ##   ..   song_name = col_character(),
    ##   ..   lyrics = col_character()
    ##   .. )

``` r
tidy_rm_stopwords %>%
  count(word, sort = TRUE)
```

    ## # A tibble: 5,931 x 2
    ##    word      n
    ##    <chr> <int>
    ##  1 love    369
    ##  2 la      282
    ##  3 time    167
    ##  4 life    133
    ##  5 baby    119
    ##  6 day     118
    ##  7 babe    115
    ##  8 eyes    114
    ##  9 hand    105
    ## 10 yeah    100
    ## # ... with 5,921 more rows

Removing the stopwords reduced the total word count to 23,873 and the
number of unique words to 5,931.

For fun, let’s look at all the lyrics in the song Skeleton Tree again –
in both the tidy\_songlist version of the corpus, and the version where
we removed stopwords.

``` r
print(tidy_songlist %>%
  dplyr::filter(song_name == "Skeleton Tree") %>%
    dplyr::select(word), 
  n = Inf) 
```

    ## # A tibble: 106 x 1
    ##     word    
    ##     <chr>   
    ##   1 sunday  
    ##   2 morning 
    ##   3 skeleton
    ##   4 tree    
    ##   5 oh      
    ##   6 nothing 
    ##   7 is      
    ##   8 for     
    ##   9 free    
    ##  10 in      
    ##  11 the     
    ##  12 window  
    ##  13 a       
    ##  14 candle  
    ##  15 well    
    ##  16 maybe   
    ##  17 you     
    ##  18 can     
    ##  19 see     
    ##  20 fallen  
    ##  21 leaves  
    ##  22 thrown  
    ##  23 across  
    ##  24 the     
    ##  25 sky     
    ##  26 a       
    ##  27 jittery 
    ##  28 tv      
    ##  29 glowing 
    ##  30 white   
    ##  31 like    
    ##  32 fire    
    ##  33 nothing 
    ##  34 is      
    ##  35 for     
    ##  36 free    
    ##  37 i       
    ##  38 called  
    ##  39 out     
    ##  40 i       
    ##  41 called  
    ##  42 out     
    ##  43 right   
    ##  44 across  
    ##  45 the     
    ##  46 sea     
    ##  47 but     
    ##  48 the     
    ##  49 echo    
    ##  50 comes   
    ##  51 back    
    ##  52 empty   
    ##  53 and     
    ##  54 nothing 
    ##  55 is      
    ##  56 for     
    ##  57 free    
    ##  58 sunday  
    ##  59 morning 
    ##  60 skeleton
    ##  61 tree    
    ##  62 pressed 
    ##  63 against 
    ##  64 the     
    ##  65 sky     
    ##  66 the     
    ##  67 jittery 
    ##  68 tv      
    ##  69 glowing 
    ##  70 white   
    ##  71 like    
    ##  72 fire    
    ##  73 and     
    ##  74 i       
    ##  75 called  
    ##  76 out     
    ##  77 i       
    ##  78 called  
    ##  79 out     
    ##  80 right   
    ##  81 across  
    ##  82 the     
    ##  83 sea     
    ##  84 i       
    ##  85 called  
    ##  86 out     
    ##  87 i       
    ##  88 called  
    ##  89 out     
    ##  90 that    
    ##  91 nothing 
    ##  92 is      
    ##  93 for     
    ##  94 free    
    ##  95 and     
    ##  96 it's    
    ##  97 alright 
    ##  98 now     
    ##  99 and     
    ## 100 it's    
    ## 101 alright 
    ## 102 now     
    ## 103 and     
    ## 104 it's    
    ## 105 alright 
    ## 106 now

``` r
print(tidy_rm_stopwords %>%
  dplyr::filter(song_name == "Skeleton Tree") %>%
    dplyr::select(word), 
  n = Inf) 
```

    ## # A tibble: 43 x 1
    ##    word    
    ##    <chr>   
    ##  1 sunday  
    ##  2 morning 
    ##  3 skeleton
    ##  4 tree    
    ##  5 free    
    ##  6 window  
    ##  7 candle  
    ##  8 fallen  
    ##  9 leaves  
    ## 10 thrown  
    ## 11 sky     
    ## 12 jittery 
    ## 13 tv      
    ## 14 glowing 
    ## 15 white   
    ## 16 fire    
    ## 17 free    
    ## 18 called  
    ## 19 called  
    ## 20 sea     
    ## 21 echo    
    ## 22 empty   
    ## 23 free    
    ## 24 sunday  
    ## 25 morning 
    ## 26 skeleton
    ## 27 tree    
    ## 28 pressed 
    ## 29 sky     
    ## 30 jittery 
    ## 31 tv      
    ## 32 glowing 
    ## 33 white   
    ## 34 fire    
    ## 35 called  
    ## 36 called  
    ## 37 sea     
    ## 38 called  
    ## 39 called  
    ## 40 free    
    ## 41 alright 
    ## 42 alright 
    ## 43 alright

The complete song contains 106 words, while the version with stopwords
removed contains only 43. More importantly, the flow is completely off
in the second version\! However, the version without stopwords is much
more appropriate for NLP. So, we continue.

# Splitting the Data

From here, I created two subsets of the data; one for Nick Cave and the
Bad Seeds songs (tidy\_nickcave) and one for just Morrissey songs
(tidy\_moz).

``` r
# Nick Cave and the Bad Seeds transformation and summary
tidy_nickcave <- dplyr::filter(tidy_rm_stopwords, artist == "Nick Cave & The Bad Seeds")

head(tidy_nickcave)
```

    ## # A tibble: 6 x 5
    ##      id artist                    album_name           song_name word     
    ##   <dbl> <chr>                     <chr>                <chr>     <chr>    
    ## 1  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche stepped  
    ## 2  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche avalanche
    ## 3  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche covered  
    ## 4  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche soul     
    ## 5  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche hunchback
    ## 6  1001 Nick Cave & The Bad Seeds From her to eternity Avalanche sleep

``` r
str(tidy_nickcave)
```

    ## tibble [17,191 x 5] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ id        : num [1:17191] 1001 1001 1001 1001 1001 ...
    ##  $ artist    : chr [1:17191] "Nick Cave & The Bad Seeds" "Nick Cave & The Bad Seeds" "Nick Cave & The Bad Seeds" "Nick Cave & The Bad Seeds" ...
    ##  $ album_name: chr [1:17191] "From her to eternity" "From her to eternity" "From her to eternity" "From her to eternity" ...
    ##  $ song_name : chr [1:17191] "Avalanche" "Avalanche" "Avalanche" "Avalanche" ...
    ##  $ word      : chr [1:17191] "stepped" "avalanche" "covered" "soul" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   id = col_double(),
    ##   ..   artist = col_character(),
    ##   ..   album_name = col_character(),
    ##   ..   song_name = col_character(),
    ##   ..   lyrics = col_character()
    ##   .. )

``` r
tidy_nickcave %>%
  dplyr::count(word, sort = TRUE)
```

    ## # A tibble: 4,729 x 2
    ##    word      n
    ##    <chr> <int>
    ##  1 la      244
    ##  2 love    219
    ##  3 babe    115
    ##  4 baby    113
    ##  5 fire     98
    ##  6 eyes     87
    ##  7 time     80
    ##  8 yeah     79
    ##  9 black    78
    ## 10 hand     77
    ## # ... with 4,719 more rows

``` r
unique_tidy_nickcave <- tidy_nickcave %>%
  dplyr::count(word, sort = TRUE)

n_unique_tidy_nickcave <- nrow(unique_tidy_nickcave)
tot_tidy_nickcave <- nrow(tidy_nickcave)


# Morrissey transformation and summary
tidy_moz <- dplyr::filter(tidy_rm_stopwords, artist == "Morrissey")

head(tidy_moz)
```

    ## # A tibble: 6 x 5
    ##      id artist    album_name song_name       word       
    ##   <dbl> <chr>     <chr>      <chr>           <chr>      
    ## 1  2001 Morrissey Viva Hate  Alsatian Cousin lovers     
    ## 2  2001 Morrissey Viva Hate  Alsatian Cousin forecourt  
    ## 3  2001 Morrissey Viva Hate  Alsatian Cousin friday     
    ## 4  2001 Morrissey Viva Hate  Alsatian Cousin passing    
    ## 5  2001 Morrissey Viva Hate  Alsatian Cousin lovers     
    ## 6  2001 Morrissey Viva Hate  Alsatian Cousin groundsheet

``` r
str(tidy_moz)
```

    ## tibble [6,682 x 5] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ id        : num [1:6682] 2001 2001 2001 2001 2001 ...
    ##  $ artist    : chr [1:6682] "Morrissey" "Morrissey" "Morrissey" "Morrissey" ...
    ##  $ album_name: chr [1:6682] "Viva Hate" "Viva Hate" "Viva Hate" "Viva Hate" ...
    ##  $ song_name : chr [1:6682] "Alsatian Cousin" "Alsatian Cousin" "Alsatian Cousin" "Alsatian Cousin" ...
    ##  $ word      : chr [1:6682] "lovers" "forecourt" "friday" "passing" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   id = col_double(),
    ##   ..   artist = col_character(),
    ##   ..   album_name = col_character(),
    ##   ..   song_name = col_character(),
    ##   ..   lyrics = col_character()
    ##   .. )

``` r
tidy_moz %>%
  dplyr::count(word, sort = TRUE)
```

    ## # A tibble: 2,315 x 2
    ##    word       n
    ##    <chr>  <int>
    ##  1 love     150
    ##  2 life      97
    ##  3 time      87
    ##  4 day       67
    ##  5 gonna     49
    ##  6 heart     48
    ##  7 people    39
    ##  8 la        38
    ##  9 born      37
    ## 10 exit      35
    ## # ... with 2,305 more rows

``` r
unique_tidy_moz <- tidy_moz %>%
  dplyr::count(word, sort = TRUE)

n_unique_tidy_moz <- nrow(unique_tidy_moz)
tot_tidy_moz <- nrow(tidy_moz)
```

Notice that with stopwords removed, Nick Cave uses more than twice as
many unique words in his songs, compared to Morrissey (4729 vs. 2315).
Nick Cave also uses almost three times as many non-stopword words in his
lyrics, overall, compared to Morrissey (17191 vs. 6682).

# Sentiment Analysis

The tidytext package comes with three sentiment dictionaries: AFINN,
Bing, and NRC. The Bing lexicon is the most simplistic of the three; it
assigns sentiments of either “positive” or “negative” to a list of about
\~6,800 words. For the purposes of this exercise, I’ll use the Bing
lexicon. *Later on, I might compare the three just to see, but that will
be for another day.*

``` r
bing_sent <- tidytext::get_sentiments("bing")

# Nick Cave and the Bad Seeds 
tidy_nickcave_bing <- tidy_nickcave %>%
  dplyr::inner_join(bing_sent) %>%
  dplyr::mutate(method = "Bing et al.")
```

    ## Joining, by = "word"

``` r
# Proportion positive/negative sentiment for NicK Cave & The Bad Seeds
tidy_nickcave_bing %>%
  dplyr::count(sentiment, sort = TRUE) %>%
  dplyr::mutate(prop = n / sum(n))
```

    ## # A tibble: 2 x 3
    ##   sentiment     n  prop
    ##   <chr>     <int> <dbl>
    ## 1 negative   1857 0.649
    ## 2 positive   1005 0.351

``` r
per_sent_nickcave <- (round(nrow(tidy_nickcave_bing)/nrow(tidy_nickcave), digits = 3)*100)

# Morrissey
tidy_moz_bing <- tidy_moz %>%
  dplyr::inner_join(bing_sent) %>%
  dplyr::mutate(method = "Bing et al.")
```

    ## Joining, by = "word"

``` r
# Proportion positive/negative sentiment for NicK Cave & The Bad Seeds
tidy_moz_bing %>%
  dplyr::count(sentiment, sort = TRUE) %>%
  dplyr::mutate(prop = n / sum(n))
```

    ## # A tibble: 2 x 3
    ##   sentiment     n  prop
    ##   <chr>     <int> <dbl>
    ## 1 negative   1005 0.640
    ## 2 positive    566 0.360

``` r
per_sent_moz <- (round(nrow(tidy_moz_bing)/nrow(tidy_moz), digits = 3)*100)
```

A few points to keep in mind:

  - Less than one-quarter (23.5) of Morrissey’s non-stopword lyrics had
    any sentiment.
  - Only 16.6 of Nick Cave’s non-stopword lyrics had any sentiment.

From here, let’s take a look at each artist’s most commonly used words,
and the sentiment applied to each:

``` r
# Nick Cave and the Bad Seeds
tidy_nickcave_bing_counts <- tidy_nickcave_bing %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  dplyr::ungroup() 

tidy_nickcave_bing_counts
```

    ## # A tibble: 754 x 3
    ##    word  sentiment     n
    ##    <chr> <chr>     <int>
    ##  1 love  positive    219
    ##  2 lie   negative     46
    ##  3 hard  negative     44
    ##  4 pain  negative     40
    ##  5 dead  negative     39
    ##  6 die   negative     39
    ##  7 poor  negative     34
    ##  8 death negative     32
    ##  9 cry   negative     31
    ## 10 bad   negative     29
    ## # ... with 744 more rows

``` r
# Morrissey
tidy_moz_bing_counts <- tidy_moz_bing %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  dplyr::ungroup() 

tidy_moz_bing_counts 
```

    ## # A tibble: 495 x 3
    ##    word   sentiment     n
    ##    <chr>  <chr>     <int>
    ##  1 love   positive    150
    ##  2 kill   negative     24
    ##  3 wrong  negative     23
    ##  4 dead   negative     21
    ##  5 killed negative     19
    ##  6 pain   negative     19
    ##  7 sick   negative     19
    ##  8 poor   negative     17
    ##  9 cry    negative     16
    ## 10 die    negative     16
    ## # ... with 485 more rows

For both artists, the word “love” was the most commonly used word and
the only word with positive sentiment. Adorable\!

At this point, it’s helpful to consider the percentage of negative v.
positive sentiment words in both corpi.

``` r
tidy_nickcave_bing_counts %<>%
  dplyr::mutate(sentiment_binary = case_when(
    sentiment == "positive" ~ 1, 
    sentiment == "negative" ~ 0
  )) %>%
  dplyr::relocate(sentiment_binary, .after = sentiment)

mean_nickcave_pos <- (round(mean(tidy_nickcave_bing_counts$sentiment_binary), 
                            digits = 3)*100)

tidy_moz_bing_counts %<>%
  dplyr::mutate(sentiment_binary = case_when(
    sentiment == "positive" ~ 1, 
    sentiment == "negative" ~ 0
  )) %>%
  dplyr::relocate(sentiment_binary, .after = sentiment)


mean_moz_pos <- (round(mean(tidy_moz_bing_counts$sentiment_binary), 
                            digits = 3)*100)
```

About one-quarter of Nick Cave’s non-stopword lyrics had positive
sentiment (24.8), compared to shy of one-third for Morrissey (31.7).
Hmph\!

# Visualizing Sentiment

At this point, let’s visually compare the two and settle this once and
for all (until one of them comes out with another album).

![](moz_cave_sentiment_files/figure-gfm/sent%20plots-1.png)<!-- -->

# Conclusion

Look at all that love\! And just how similar the sentiment holding words
are for both artists\! That said though, it’s evident both from the
graphs and summary statistics that the sentiment of Nick Cave’s lyrics
is more negative than Morrissey’s. Context is important though. Any
listener of Morrissey can tell you that when he refers to love or is
singing of love, he’s primarily discussing unrequited love or the fact
(?) that he has never experienced love – not exactly a happy story.
While with Nick Cave, the love “feels” more positive. If I had used more
than unigrams in this study, perhaps that would have emerged more
organically.

Overall though, these findings are consistent with what I thought would
happen going into it. Many of Nick Cave’s older songs are epic stories
involving death. And in his more recent music, he tackles enormous grief
and loss in his songs.
