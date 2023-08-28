# 19th-20th Century British Literature Prose Analysis
Literature helps paint a picture of the lives of people at the time of writing. In addition to illustrating ideology and everyday life, literature is a quintessential vessel in understanding the evolution in language over a given time period. Language is constantly changing, and discovering a [dataset on British Literature from the 14th to 21st centuries](https://www.kaggle.com/datasets/ahmadalijamali/british-literature-nlp-labeld-phrase), I became curious about exactly what linguistic changes were occurring in the past. 

For this project I used R, as R has a number of straightforward tools for data analysis. I used the tidyverse and tidytext libraries. 

I began by organizing the data into a table. Each row had a single sentence from a novel, the author, the novel title, and the century. This gave me an overview of the dataset and features to consider. I also mended an error in the dataset as Virginia Woolf's *To The Lighthouse* was listed as being written in the 19th century when it was truly written in the 20th. 
```
brit_lit <- read_csv("British-literature.csv", col_names = FALSE, show_col_types = FALSE)
brit_lit <- brit_lit %>%
  separate(col = "X1", into = c(NA, "Sentence", "Other"), sep = "\\[|\\]\",|\\]") %>% 
  separate(col = "Other", into = c("Author", "Novel", "Century"), sep = "\\w,") %>%
  mutate(Author = gsub(",", "", Author))
brit_lit$Century[brit_lit$Author == "Virginia Wool"] <- 20
```
| Index | Sentence | Author | Century |
| --- | --- | ---| --- |
| 1 | 'Even the Cock-lane ghost had been laid only a round dozen of years, after rapping out its messages, as the spirits of this very year last past (supernaturally deficient in originality) rapped out theirs.' | Charles Dicken|A tale of two citie | 19 |
| 2 | ' It is likely enough that in the rough outhouses of some tillers of the heavy lands adjacent to Paris, there were sheltered from the weather that very day, rude carts, bespattered with rustic mire, snuffed about by pigs, and roosted in by poultry, which the Farmer, Death, had already set apart to be his tumbrils of the - 5 Revolution.' | Charles Dicken | A tale of two citie | 19 |
| 3 | ' Daring burglaries by armed men, and highway robberies, took place in the capital itself every night; families were publicly cautioned not to go out of town without removing their furniture to... | Charles Dicken | A tale of two citie | 19 |

Next, I split each sentence into a table of individual word tokens and their corresponding centuries. I removed stopwords from the dataset because these are unlikely to show significant change. 
```
words <- (brit_lit[c("Sentence", "Century")])
words <- words %>%
  unnest_tokens(Word, Sentence)
words <- words %>% 
  na.omit() 
words <- words %>%
  filter(grepl("^[a-z]+$", Word)) %>%
  filter(Century > 16) %>%
  anti_join(stop_words, by = join_by(Word == word))
```

| Index | Century | Word |
| --- | --- | --- |
| 1 | 19 | cock |
| 2 | 19 | lane |
| 3 | 19 | ghost |
| 4 | 19 | laid |
| 5 | 19 | round |
| 6 | 19 | dozen |
| 7 | 19 | rapping |
| 8 | 19 | messages |
| 9 | 19 | spirits |
| 10 | 19 | past |

I then grouped words by century into a separate table that listed the number of times the word appeared in the cited literature within the century and filtered out any data point that contained digits. It was here that I had to make certain decisions concerning what data would be useful for analysis. Other than the 19th and 20th centuries, each century contained two or fewer works. As a result, I decided to only analyze data points spanning from the 19th to 20th century as having data from several works by different authors would help to center the analysis on the language of a century rather than the word choice of an individual author. I also chose to manually remove certain words from the dataset that were story-specific, mainly proper nouns. Words such as names or cities are mentioned many times in a single novel, but are words that pertain to a given story rather than a time period (though name choices do change over time), so they distract from overall analysis of the period. I further removed the word "don", as this was an error when extracting word tokens caused by the word "don't". A final consideration I had to make concerned George Orwell. Orwell often centered political issues in his novels and so words such as fascist and communist appear frequently. Even though these words appeared in the dataset mainly exclusively in Orwell's work, I chose to leave them in the data as they refer to political ideologies of Orwell's era rather than groups or characters contained within the bounds of the story. 
```
words_count <- words %>%
  group_by(Century) %>%
  count(Word) %>%
  arrange(desc(n)) %>%
  arrange(Century)
words_count

words_count <- words_count[-c(1, 3, 5, 6, 7, 8, 12, 17, 22, 23, 29, 36, 38, 40, 42, 49, 51, 56, 60, 63, 66, 71, 11267, 13266, 11272, 11275, 11284, 11285, 11287, 11293, 11304, 11305, 11319, 11325, 11331, 11332, 11333, 11345, 11346),]
```
Finally, I made graphs of the 30 most frequently used words in each century. 
```
words_count %>%
  filter(Century == 19) %>%
  top_n(30, wt = n) %>%
ggplot(aes(x=n, y=reorder(Word, n))) +
  geom_bar(stat="identity")

words_count %>%
  filter(Century == 20) %>%
  top_n(30, wt = n) %>%
ggplot(aes(x=n, y=reorder(Word, n))) +
  geom_bar(stat="identity")
```
![Graph of 30 most frequent 19th century words](https://github.com/fua-bot/Brit-Lit/assets/78167892/e4ea0cd1-eb8b-43cf-aa34-0b70677d77b4)
![Graph of 30 most frequent 20th century words](https://github.com/fua-bot/Brit-Lit/assets/78167892/f5c86c41-d1fe-4604-aa7f-083213dff494)

Using these graphs, we can get a basic sense for common language of the two centuries. In both centuries the words "time", "miss", "people", "looked, "lady", "house", "head", "night", "eyes", "round", "life", "lord", "hand", "door", "left", "sir", "day", "woman", "friend", "told", and "hand\[s\]" all appear in the 30 most frequent words. The words "dear", "father", "mind", "heard", "cried", "half", "found", "love", and "sister" appeared only in the 30 most frequent words of the 19th century. The words "street", "war", "party", "moment", and "fascist" appear only in the 30 most frequent words of the 20th century. 

The shared words are primarily words pertaining to common human actions such as "looked", "people", "life", "time", which shows a prevailing focus on humanity in the literature of both centuries. For words exclusive to each century, I wanted to view two things more in-depth: their frequency in the other century and use by an individual author. 30 words is not many when concerning full works of literature, so I wanted to examine if these words exclusive to the 30 most frequent of each century were also common in the other century. Additionally, I wanted to check if the words were being used in several works rather than by a single author, as such words may not give clues concerning the time, but merely the author's personal writing style. 

The words most interesting to me that were exclusive to the top 30 of the 19th century were "dear", "father, and "sister". Quickly scanning across the 19th century data reveals these words are all used several times by multiple authors. Since there are six works in the 19th century data and four in the 20th, it is not appropriate to directly compare the word frequencies. Instead I chose to compare the word frequencies to the most frequent word of the century. For the 19th century this is "miss" with 342 uses and for the 20th it is "time" with 337 uses. "Dear" is used 170 times in the 19th century, 49.71% as much as "miss". "Dear" is used 29 times in the 20th century, 8.61% as much as "time". For "father" and "sister", 45.03% and 31.88% as many uses as "miss" respectively in the 19th century. "Father" and "sister" are used 19.29% and 4.15% as many times as "time" in the 20th century. This reveals a large decrease in the usage of these three words from the 19th to the 20th century. This change in language potentially speaks to a cultural shift between the periods. Literature of the 20th century seems to shift away from a focus on the family unit as the words "father" and "sister" appear much less often. In fact, within the 30 most frequent words of the 20th century, no words relate to family. The decline of the use "dear" can indicate a general shift away from certain terms of endearment or honorifics. 

The word "fascist" is a word exclusive to the most frequent 30 of the 20th century. It only appears in the cited Orwell works, however I included it as it is relevant to the historical era. According to the [Merriam-Webster Dictionary](https://www.merriam-webster.com/wordplay/fascism-meaning-and-history#:~:text=Rise%20of%20Mussolini&text=Mussolini's%20fascisti%20made%20the%20stronger,and%20their%20fasces%2C%20precede%20him.), "fascism" only became a word in the early 20th century during the reign of Mussolini. Along with "war" and "party", the frequent usage of "fascist" reveals a concern for political upheaval that was occurring throughout the 20th century. While it is impossible to properly analyze this to the full extent without a textual analysis of all works in the dataset, it is possible that it also indicates that the general concern of people was moving away from interpersonal conflict in the 19th century, as shown by the use of "father" and "sister", toward social issues in the 20th century. It also demonstrates how words are created and incorporated into common vernacular. 

Across a decade, language and relevant issues can shift greatly. Over two centuries, this shift is even more dramatic. Literature reveals what common people are thinking about as well as what thoughts are allowed to be expressed in the public sphere. The literary data from the 19th to 20th century indicates as such. As focus moved away from familial conflict in the 19th century, writers concerned themselves with greater social upheaval in the 20th century. 
