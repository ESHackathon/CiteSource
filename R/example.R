# example workflow

devtools::load_all() # load CiteSource package
library(dplyr)
library(ggplot2)

# step 1: bring in RIS files-----

# listRIS citation files in folder
citation_files <- list.files(path= "tests/testthat/data/new2", pattern = "\\.ris", full.names = TRUE)

# print to console
citation_files


# read in citations and specify sources, labels (no strings relevant here)
citations <- read_citations(citation_files,
                            cite_sources = c("DIM", NA,"LENS", NA, "WOS"),
                            cite_labels = c("search",  "final", "search", "screen", "search"),
                            tag_naming = "best_guess")

# FIX for blank spaces in dimensions file 
citations <- citations %>%
  mutate(delete = ifelse(is.na(year) & is.na(title) & is.na(author), "yes", "no")) %>%
  filter(!delete == "yes") %>% 
  select(-delete)

# step 2: deduplication-----

# deduplicate citations 
dedup_results <- dedup_citations(citations, merge_citations = TRUE)

# get unique citations
unique_citations <- dedup_results$unique

# step 3: compare sources / labels/ strings

# classify unique citations across difference sources/ labels/ strings
n_unique <- count_unique(unique_citations)

# for each unique citation, which sources are present
source_comparison <- compare_sources(unique_citations, comp_type = "sources")
label_comparison <- compare_sources(unique_citations, comp_type = "labels")

# step 4: visualise comparisons

# making cite_label a factor in a certain order to reflect SR process - search, screen, final
n_unique$cite_label = factor(n_unique$cite_label, levels=c('search','screen','final'))

# bar plot unique labels PER database
ggplot(n_unique, aes(fill=unique, x=cite_label)) + 
  geom_bar(position="stack", stat="count") +
  facet_grid(vars(cite_source)) +
  xlab("") + ylab("Number of citations")

# bar plot unique labels PER database (flipped)
ggplot(n_unique, aes(fill=unique, x=cite_label)) + 
  geom_bar(position="stack", stat="count") +
  facet_grid(~cite_source) +
  xlab("") + ylab("Number of citations")

# bar plot unique citations PER database as PERCENTAGE
n_unique %>% 
  select(cite_source, duplicate_id, unique) %>%
  unique() %>%
  group_by(cite_source, unique) %>%
  count(unique, cite_source) %>%
  group_by(cite_source) %>%
  mutate(perc = n / sum(n)) %>%
  ggplot(aes(fill=unique, x=cite_source, y=perc)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("") + ylab("Number of citations")

# bar plot database PER label
ggplot(n_unique, aes(fill=unique, x=cite_source)) + 
  geom_bar(position="stack", stat="count") +
  facet_grid(~cite_label) +
  xlab("") + ylab("Number of citations")


# stacked bar labels PER database
ggplot(n_unique, aes(fill=cite_label, x=cite_source)) + 
  geom_bar(position="stack", stat="count") +
  xlab("") + ylab("Number of citations")

# stacked bar database PER label
ggplot(n_unique, aes(fill=cite_source, x=cite_label)) + 
  geom_bar(position="stack", stat="count") +
  xlab("") + ylab("Number of citations")

# heatmap of sources
plot_source_overlap_heatmap(data2)

# upset plot of sources
plot_source_overlap_upset(source_comparison)
plot_source_overlap_upset(source_comparison, decreasing = c(TRUE, TRUE))

