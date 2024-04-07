library(RedditExtractoR)
library(dplyr)
library(stringr)
library(openxlsx)

#Threads in the conservative subreddit
threads_conservative <- find_thread_urls(
  sort_by = "top",
  subreddit = "Conservative", 
  period = "all"
)

#Threads in the liberal subreddit
threads_liberal <- find_thread_urls(
  sort_by = "top",
  subreddit = "neoliberal", 
  period = "all"
)

# 30 URLs and extract the content for conservatives
threads_urls_conservative <- threads_conservative$url[1:30]

threads_content_conservative <- get_thread_content(
  urls = threads_urls_conservative) 

# 30 URLs and extract the content for liberal
threads_urls_liberal <- threads_liberal$url[1:30]

threads_content_liberal <- get_thread_content(
  urls = threads_urls_liberal) 

#Conservative comments dataframe
threads_comments_conservative <- threads_content_conservative$comments
user_count_before_Con <- threads_comments_conservative %>% 
  distinct(author) %>% 
  nrow()

#Liberal comments dataframe
threads_comments_liberal <- threads_content_liberal$comments
user_count_before_Lib <- threads_comments_liberal %>% 
  distinct(author) %>% 
  nrow()
#Filtering

#Deleting comments that have a deleted author, filtering by date and deleting hyperlinks
threads_comments_conservative <- threads_comments_conservative %>%
    filter(author != "[deleted]") %>% 
    filter(!grepl("https?://[\\S]+", comment)) %>% 
    arrange(desc(upvotes))

threads_comments_liberal <- threads_comments_liberal %>%
    filter(author != "[deleted]") %>% 
    filter(!grepl("https?://[\\S]+", comment))%>% 
    arrange(desc(upvotes))

#Finding keywords

threads_comments_conservative_w <- threads_comments_conservative %>% 
  mutate(comment2 = tolower(comment)) 

#Ingroup/loyality

threads_comments_conservative_w <- threads_comments_conservative_w %>% 
  mutate(ingroup = str_detect(
    string = threads_comments_conservative_w$comment2,
    pattern =  c ("together | nation | homeland | family | families | familial | group | loyal | abuse | damag | ruin | ravage | detriment | crush | attack | annipatriot | communal | commune | communit | communis | comrad | hilate | destroy | stomp | abandon | spurn | impair | exploit | exploits | cadre | collectiv | joint | unison | unite | fellow | guild | solidarity | devot | member | cliqu | cohort | ally | insider | foreign | enem | betray | treason | traitor | treacher | disloyal | individual | apostasy | apostate | deserted | deserter | deserting | deceiv | jilt | imposter | miscreant | spy | sequester | renegade | terroris | immigra")))

sum(threads_comments_conservative_w$ingroup)

#Authority/respect

threads_comments_conservative_w <- threads_comments_conservative_w %>% 
  mutate(authority = str_detect(
    string = threads_comments_conservative_w$comment2,
    pattern =  c ("obey | obedien | duty | law | lawful | legal | duti | honor | respect | respectful | respected | respects | order | father | mother | motherl | mothering | mothers | tradition | hierarch | authorit | permit | permission | status | rank | leader | class | bourgeoisie | cast | position | complian | command | supremacy | control | submi | allegian | serve | abide | defere | defer | revere | venerat | comply | defian | rebel | dissent | subver | disrespect | disobe | sediti | agitat | insubordinat | illegal | lawless | insurgent | mutinous | defy | dissident | unfaithful | alienate | defector | heretic | nonconformist | oppose | protest | refuse | denounce | remonstrate | riot | obstruct")))

sum(threads_comments_conservative_w$authority)

#Purity/sanctity

threads_comments_conservative_w <- threads_comments_conservative_w %>% 
  mutate(purity = str_detect(
    string = threads_comments_conservative_w$comment2,
    pattern =  c ("piety | pious | purity | pure | clean | steril | sacred | chast | holy | holiness | saint | wholesome | celiba | abstention | virgin | virgins | virginity | virginal | austerity | integrity | modesty | abstinen | abstemiousness | upright | limpid | unadulterated | maiden | virtuous | refined | intemperate | decen | immaculate | innocent | pristine | humble | disgust | deprav | disease | unclean | contagio | indecen | sin | sinful | sinner | sins | sinned | sinning | slut | whore | dirt | impiety | impious | profan | gross | repuls | sick | promiscu | lewd | adulter | debauche | defile | tramp | prostitut | unchaste | wanton | profligate | filth | trashy | obscen | lax | taint | stain | tarnish | debase | desecrat | wicked | blemish | exploitat | pervert | wretched")))

sum(threads_comments_conservative_w$purity) 

#LIBERALS

threads_comments_liberal_w <- threads_comments_liberal %>% 
  mutate(comment2 = tolower(comment)) 

#Ingroup/loyality

threads_comments_liberal_w <- threads_comments_liberal_w %>% 
  mutate(ingroup = str_detect(
    string = threads_comments_liberal_w$comment2,
    pattern =  c ("together | nation | homeland | family | families | familial | group | loyal | abuse | damag | ruin | ravage | detriment | crush | attack | annipatriot | communal | commune | communit | communis | comrad | hilate | destroy | stomp | abandon | spurn | impair | exploit | exploits | cadre | collectiv | joint | unison | unite | fellow | guild | solidarity | devot | member | cliqu | cohort | ally | insider | foreign | enem | betray | treason | traitor | treacher | disloyal | individual | apostasy | apostate | deserted | deserter | deserting | deceiv | jilt | imposter | miscreant | spy | sequester | renegade | terroris | immigra")))

sum(threads_comments_liberal_w$ingroup)

#Authority/respect

threads_comments_liberal_w <- threads_comments_liberal_w %>% 
  mutate(authority = str_detect(
    string = threads_comments_liberal_w$comment2,
    pattern =  c ("obey | obedien | duty | law | lawful | legal | duti | honor | respect | respectful | respected | respects | order | father | mother | motherl | mothering | mothers | tradition | hierarch | authorit | permit | permission | status | rank | leader | class | bourgeoisie | cast | position | complian | command | supremacy | control | submi | allegian | serve | abide | defere | defer | revere | venerat | comply | defian | rebel | dissent | subver | disrespect | disobe | sediti | agitat | insubordinat | illegal | lawless | insurgent | mutinous | defy | dissident | unfaithful | alienate | defector | heretic | nonconformist | oppose | protest | refuse | denounce | remonstrate | riot | obstruct")))

sum(threads_comments_liberal_w$authority)

#Purity/sanctity

threads_comments_liberal_w <- threads_comments_liberal_w %>% 
  mutate(purity = str_detect(
    string = threads_comments_liberal_w$comment2,
    pattern =  c ("piety | pious | purity | pure | clean | steril | sacred | chast | holy | holiness | saint | wholesome | celiba | abstention | virgin | virgins | virginity | virginal | austerity | integrity | modesty | abstinen | abstemiousness | upright | limpid | unadulterated | maiden | virtuous | refined | intemperate | decen | immaculate | innocent | pristine | humble | disgust | deprav | disease | unclean | contagio | indecen | sin | sinful | sinner | sins | sinned | sinning | slut | whore | dirt | impiety | impious | profan | gross | repuls | sick | promiscu | lewd | adulter | debauche | defile | tramp | prostitut | unchaste | wanton | profligate | filth | trashy | obscen | lax | taint | stain | tarnish | debase | desecrat | wicked | blemish | exploitat | pervert | wretched")))

sum(threads_comments_liberal_w$purity) 

# Calculate the total number of words in the liberal comments 
total_words_liberal <- threads_comments_liberal_w %>%
  dplyr::mutate(words = strsplit(as.character(comment2), "\\s+")) %>%
  dplyr::mutate(n_words = sapply(words, length)) %>%
  dplyr::summarise(total = sum(n_words))

print(total_words_liberal)

# Calculate the total number of words in the conservative comments 
total_words_conservative <- threads_comments_conservative_w %>%
  dplyr::mutate(words = strsplit(as.character(comment2), "\\s+")) %>%
  dplyr::mutate(n_words = sapply(words, length)) %>%
  dplyr::summarise(total = sum(n_words))

print(total_words_conservative)

# T-test
word_counts <- matrix(c(344, 239, 422, 223, 80, 68), nrow = 3, byrow = TRUE,
                      dimnames = list(c("ingroup", "authority", "purity"),
                                      c("conservative", "liberal")))
total_words <- c(conservative = 209546, liberal = 157207)
t_test_results <- lapply(1:nrow(word_counts), function(i) {
  prop.test(word_counts[i,], total_words)
})
names(t_test_results) <- rownames(word_counts)

t_test_results



#Filtering and creating final dataframes
final_conservative <- threads_comments_conservative_w %>% 
  filter(ingroup =="TRUE" | authority == "TRUE" | purity == "TRUE")

user_count_after_Con <- final_conservative %>% 
  distinct(author) %>% 
  nrow()

final_liberal <- threads_comments_liberal_w %>% 
  filter(ingroup =="TRUE" | authority == "TRUE" | purity == "TRUE")

user_count_after_Lib <- final_liberal %>% 
  distinct(author) %>% 
  nrow()
#Sampling

sample_conservative <- final_conservative %>% 
  slice_head(n=50)

sample_liberal <- final_liberal %>% 
  slice_head(n=50)

toxsample_conservative <- sample_n(threads_comments_conservative, 50)
comments_con <- data.frame(comment = toxsample_conservative$comment)

toxsample_liberal <- sample_n(threads_comments_liberal, 50)
comments_lib <- data.frame(comment = toxsample_liberal$comment)

#load excel

library(readxl)

Sample_100_dat <- read_excel("Sample 100.xlsx")  #here you have to run the excel

Sys.setenv(perspective_api_key = "your key")




library(peRspective)

##prsp_stream requires an arguments called text_id, where unique_id for each comment is needed. 
##The Reddit API did not give us this. 
##Let's create one with mutate: 

Sample_100_dat_w<- Sample_100_dat %>% 
  mutate(our_comment_id = row_number())




##Now we can pass 'our_comment_id' to the text_id argument:
##CommentsConservatives
tox_scores_Conservatives <- prsp_stream(
  .data = Sample_100_dat_w,
  text = CommentsConservatives,
  text_id = our_comment_id,
  score_model = "TOXICITY",
  verbose = T)

##CommentsLiberals
tox_scores_Liberals <- prsp_stream(
  .data = Sample_100_dat_w,
  text = CommentsLiberals,
  text_id = our_comment_id,
  score_model = "TOXICITY",
  verbose = T)


#We create a new column in base R:

Sample_100_dat_w$Toxicity_Score_Conservatves <- tox_scores_Conservatives$TOXICITY

Sample_100_dat_w$Toxicity_Score_Liberals <- tox_scores_Liberals$TOXICITY


#mean toxicity

mean_tox_Conservatives <- mean(Sample_100_dat_w$Toxicity_Score_Conservatves)

mean_tox_Liberals <- mean(Sample_100_dat_w$Toxicity_Score_Liberals)




##takes a long time to run! 
#general toxicity

threads_comments_conservative<- threads_comments_conservative %>% 
  mutate(our_comment_id = row_number())

threads_comments_liberal<- threads_comments_liberal %>% 
  mutate(our_comment_id = row_number())

##CommentsConservatives
tox_scores_final_Conservatives <- prsp_stream(
  .data = final_conservative,
  text = comment,
  text_id = our_comment_id,
  score_model = c("TOXICITY",
                  "IDENTITY_ATTACK",
                  "INSULT",
                  "PROFANITY",
                  "THREAT",
                  "SEXUALLY_EXPLICIT"),
  verbose = T)

##CommentsLiberals
tox_scores_final_Liberals <- prsp_stream(
  .data = final_liberal,
  text = comment,
  text_id = our_comment_id,
  score_model = c("TOXICITY",
                  "IDENTITY_ATTACK",
                  "INSULT",
                  "PROFANITY",
                  "THREAT",
                  "SEXUALLY_EXPLICIT"),
  verbose = T)





#We create a new column in base R:

tox_scores_final_Conservatives$Toxicity_Score_Conservatves <- tox_scores_Conservatives$TOXICITY

tox_scores_final_Liberals$Toxicity_Score_Liberals <- tox_scores_Liberals$TOXICITY


#mean toxicity

mean_tox_Conservatives_final <- mean(tox_scores_final_Conservatives$TOXICITY, na.rm = TRUE)

mean_tox_Liberal_final <- mean(tox_scores_final_Liberals$TOXICITY, na.rm = TRUE)



###result for median comments\raply ratio\time range of both subreddit
summary(final_conservative)
summary(final_liberal)
library(stringr)

##median
#conservative
distinct_users_con <- final_conservative %>% 
  distinct(author)
comments_per_user_table_con <- final_conservative %>% 
  group_by(author) %>% 
  summarize(comments_per_user_con = n()) 
comments_per_user_table_con <- comments_per_user_table_con %>% 
  arrange(desc(comments_per_user_con))

user_comments_histogram_con <- hist(comments_per_user_table_con$comments_per_user_con)
median(comments_per_user_table_con$comments_per_user_con)
mean(comments_per_user_table_con$comments_per_user_con)
#liberal
distinct_users_lib <- final_liberal %>% 
  distinct(author)
comments_per_user_table_lib <- final_liberal %>% 
  group_by(author) %>% 
  summarize(comments_per_user_lib = n()) 
comments_per_user_table_lib <- comments_per_user_table_lib %>% 
  arrange(desc(comments_per_user_lib))

user_comments_histogram_lib <- hist(comments_per_user_table_lib$comments_per_user_lib)
median(comments_per_user_table_lib$comments_per_user_lib)
mean(comments_per_user_table_lib$comments_per_user_lib)

##reply ratio
#conservative
final_conservative_w <- final_conservative %>% 
  add_count(author, name = "user_total_comments")
final_conservative_w <- final_conservative_w %>% 
  mutate(reply = str_detect(
    string = final_conservative_w$comment_id,
    pattern = "_",
    negate = F
  ))

reply_table_con <- final_conservative_w %>% 
  group_by(reply) %>% 
  summarize(reply_count = n())
reply_ratio_con <- sum(final_conservative_w$reply) / nrow(final_conservative_w)
reply_ratio_con

#liberal
final_liberal_w <- final_liberal %>% 
  add_count(author, name = "user_total_comments")
final_liberal_w <- final_liberal_w %>% 
  mutate(reply = str_detect(
    string = final_liberal_w$comment_id,
    pattern = "_",
    negate = F
  ))

reply_table_lib <- final_liberal_w %>% 
  group_by(reply) %>% 
  summarize(reply_count = n())
reply_ratio_lib <- sum(final_liberal_w$reply) / nrow(final_liberal_w)
reply_ratio_lib

#time range of 100 sample
earliest_date_con <- min(sample_conservative$date)
latest_date_con <- max(sample_conservative$date)
earliest_date_lib <- min(sample_liberal$date)
latest_date_lib <- max(sample_liberal$date)

##plot the 100 sample
Final_Sample <- read_excel("figure.xlsx")
library(ggplot2)
library(tidyr)
percentage_data <- Final_Sample %>%
  group_by(subreddit) %>%
  summarise(
    Authority = mean(authority == 1) * 100,
    Ingroup = mean(ingroup == 1) * 100,
    Purity = mean(purity == 1) * 100
  ) %>%
  pivot_longer(cols = -subreddit, names_to = "Variable", values_to = "Percentage")

ggplot(percentage_data, aes(x = Variable, y = Percentage, fill = subreddit)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_dodge(width = 0.9), vjust = -0.25, size = 3) +
  labs(title = "  ",
       x = " ",
       y = "Percentage (%)") +
  scale_fill_manual(values = c("Conservative" = "#3498db", "neoliberal" = "#e74c3c")) +
theme_minimal() +
  scale_x_discrete(labels = c("Authority" = "Authority", "Ingroup" = "Ingroup", "Purity" = "Purity"))
