library(Hmisc)
library(gutenbergr)
library(tidytext)
library(stringr)
library(tidyverse)
options(stringsAsFactors = F)

all_works = gutenberg_works()

# get all Oscar Wilde
oscar_wilde = all_works %>%
  filter(author == 'Wilde, Oscar',
         title != 'The Happy Prince, and Other Tales',  # there are two "The Happy Prince, and Other Tales"
         title != 'A Woman of No Importance\nAudio performance')

table(oscar_wilde$title)

# get Jane Austen
jane_austen = all_works %>%
  filter(author == 'Austen, Jane',
         title != 'Mansfield Park',  # there are some invalid UTF-8 characters in the book
         !grepl('The Complete Project Gutenberg Works of Jane Austen|The Letters of Jane Austen', title))

table(jane_austen$title)

# get F. Scott Fitzgerald
f_scott_fitzgerald = all_works %>%
  filter(grepl('Fitzgerald, F. Scott', author))

table(f_scott_fitzgerald$title)

# combine
works_to_download = oscar_wilde %>%
  bind_rows(jane_austen) %>%
  bind_rows(f_scott_fitzgerald)

# download
works = gutenberg_download(works_to_download$gutenberg_id, meta_fields = 'title')

write_csv(works, 'works.csv')

# break into paragraphs
works_paragraphs = works %>%
  unnest_tokens(paragraph, text, token = 'paragraphs')

# remove paragraphs that are titles or chapter names
works_paragraphs = works_paragraphs %>%
  filter(gsub('[^a-z]', '', paragraph) != gsub('[^a-z]', '', tolower(title)),
         !grepl('chapter', paragraph))

# remove paragraphs without any English character
works_paragraphs = works_paragraphs %>%
  filter(grepl('[a-z]', paragraph))

# remove paragraphs that have too few words (these are usually out-of-context paragraphs)
works_paragraphs = works_paragraphs %>%
  mutate(paragraph = gsub('^ +| +$', '', paragraph),
         space_cnt = str_count(paragraph, ' +')) %>%
  filter(space_cnt >= 2)

# seperate paragraphs into training, validation, and testing on a per-title basis
works_paragraphs = works_paragraphs %>%
  group_by(title) %>%
  mutate(rn = row_number(),
         n = n(),
         grp = ifelse(rn / n <= .8, 'train', ifelse(rn / n <= .9, 'valid', 'test')))

describe(works_paragraphs$grp)

# break into words
## first replace underscores (used for emphasis)
works_paragraphs = works_paragraphs %>%
  mutate(paragraph = gsub('_', '', paragraph))

## then replace punctuations with words (e.g., "_comma_")
### find common punctuations (except apostrophies, which `unnest_token` keeps)
all_paragraphs = paste(works_paragraphs$paragraph, collapse = ' ')
punctuations = str_extract_all(all_paragraphs, "[^a-z ']")
sort(table(unlist(punctuations)), decreasing = T)

works_paragraphs = works_paragraphs %>%
  mutate(paragraph = gsub(",+", " _comma_ ", paragraph),
         paragraph = gsub("\\.+", " _period_ ", paragraph),
         paragraph = gsub("\"+", " _quote_ ", paragraph),
         paragraph = gsub("\\-+", " _dash_ ", paragraph),
         paragraph = gsub(";+", " _semicolon_ ", paragraph),
         paragraph = gsub("!+", " _exclamation_ ", paragraph),
         paragraph = gsub("\\?+", " _question_ ", paragraph),
         paragraph = gsub(":+", " _colon_ ", paragraph),
         paragraph = gsub("‘", " _quote_ ", paragraph),
         paragraph = gsub("’", " _quote_ ", paragraph),
         paragraph = gsub("\\(+", " _leftparenthesis_ ", paragraph),
         paragraph = gsub("\\)+", " _rightparenthesis_ ", paragraph),
         paragraph = gsub("\\[+", " _leftbracket_ ", paragraph),
         paragraph = gsub("\\]+", " _rightbracket_ ", paragraph))

## finally, break into words
works_words = works_paragraphs %>%
  ungroup() %>%
  unnest_tokens(word, paragraph)

n_distinct(works_words$word)  # 38864

# examine infrequent words
works_words_infreq = works_words %>%
  count(word) %>%
  arrange(nn)

# based onthe examination, remove words without any English characters
works_words = works_words %>%
  filter(grepl('[a-z]', word))

write_csv(works_words, 'works_words.csv')
