
#Install CiteSource
#remotes::install_github("ESHackathon/CiteSource")

#Load the necessary libraries
library(CiteSource)
library(ggplot2)
library(dplyr)
library(knitr)

citation_files <- list.files(path= "shinytest", pattern = "\\.ris", full.names = TRUE)
citation_files

#Read in citations and specify sources. Note that labels and strings are not relevant for this use case.
citations <- read_citations(citation_files,
                            cite_sources = c("one", "two", "three"),
                            cite_labels = c("search", "search","search"),
                            tag_naming = "best_guess")

#other option to read in citations that helps with keeping from making mistakes
# file_path <- "../testing/tempNE/"
# metadata_tbl <- tibble::tribble(
# ~files,           ~cite_sources, ~cite_labels, 
# "AG.ris",      "AGRIS",       "search",    
# "ASFA.ris",        "CAB",         "search",    
# "DIM.ris",    "EconLit",     "search",    
# "WOS.ris",       "WOS",           "search",     



#Deduplicate citations. This yields a dataframe of all records with duplicates merged, but the originating source information maintained in a new variable called cite_source. 
unique_citations <- dedup_citations(citations)

#Count number of unique and non-unique citations from different sources and labels. 
n_unique <- count_unique(unique_citations)

#For each unique citation, determine which sources were present
source_comparison <- compare_sources(unique_citations, comp_type = "sources")

#create a table to view internal deduplication
initial_counts<-record_counts(unique_citations, citations, "cite_source")
record_counts_table(initial_counts)


## Plot heatmap to compare source overlap

### Heatmap by number of records
#Generate source comparison heatmap
plot_source_overlap_heatmap(source_comparison)

### Heatmap by percentage of records
#Generate heatmap with percent overlap
plot_source_overlap_heatmap(source_comparison, plot_type = "percentages")

## Plot an upset plot to compare source overlap
#Generate a source comparison upset plot.
plot_source_overlap_upset(source_comparison, decreasing = c(TRUE, TRUE))


### Bar plot of numbers of records

#Generate bar plot of unique citations PER database as NUMBER
n_unique %>% 
  select(cite_source, duplicate_id, unique) %>% #remove label /other cols to prevent duplicated rows
  ggplot(aes(fill=unique, x=cite_source)) + 
  geom_bar(position="stack", stat="count") +
  xlab("") + ylab("Number of citations")

### Bar plot of percentages of records

#Generate bar plot of unique citations PER database as PERCENTAGE
n_unique %>% 
  select(cite_source, duplicate_id, unique) %>% #remove label /other cols to prevent duplicated rows
  unique() %>% 
  group_by(cite_source, unique) %>%
  count(unique, cite_source) %>%
  group_by(cite_source) %>%
  mutate(perc = n / sum(n)) %>%
  ggplot(aes(fill=unique, x=cite_source, y=perc)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("") + ylab("Number of citations")


### Bar plot with labels

#Generate dodged bar plot Unique/Crossover PER Source with labels
n_unique %>%
  select(cite_source, unique) %>% 
  ggplot(aes(fill=unique, x=cite_source)) + 
  geom_bar(position=position_dodge(width=0.5)) +
  xlab("") + ylab("Number of citations") +
  geom_text(stat="count", aes(label=..count..))


#Get unique records from each source and add bibliographic data
unique_WOS <- n_unique %>% filter(cite_source=="WOS", unique == TRUE) %>% inner_join(unique_citations, by = "duplicate_id")

unique_DIM <- n_unique %>% 
  filter(cite_source=="DIM", unique == TRUE) %>%
  inner_join(unique_citations, by = "duplicate_id")

unique_ASFA <- n_unique %>% 
  filter(cite_source=="ASFA", unique == TRUE) %>%
  inner_join(unique_citations, by = "duplicate_id")

unique_LENS2 <- n_unique %>% 
  filter(cite_source=="LENS2", unique == TRUE) %>%
  inner_join(unique_citations, by = "duplicate_id")

unique_LENS <- n_unique %>% 
  filter(cite_source=="LENS", unique == TRUE) %>%
  inner_join(unique_citations, by = "duplicate_id")

### Analyze journal titles 

#Analyze journal titles for unique records
journals_WOS <- unique_WOS %>% 
  group_by(journal) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

#Use the knitr:kable function to print a nice looking table of the top 10 journals
kable(journals_WOS[1:10, ])


## Analyze publication years


#Group by year, count and produced a line graph
unique_WOS %>% group_by(year) %>% 
  summarise(count = n()) %>%  
  ggplot(aes(year, count, group=1)) +
  geom_line() +
  geom_point() +
  xlab("Publication year") + ylab("Unique records")



#Combine all unique record dataframes into a single dataframe. Note that we'll leave Criminal Justice Abstracts out since there is only one unique record.
all_unique <- bind_rows(unique_WOS,unique_DIM,unique_ASFA,unique_LENS2, unique_LENS)

#Group by year and source, count and produced a faceted line graph
all_unique %>% group_by(cite_source.x, year) %>% 
  summarise(count = n()) %>%  
  ggplot(aes(year, count, group=1)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ cite_source.x) +
  xlab("Publication year") + ylab("Unique records")

calculated_counts<-calculate_record_counts(unique_citations, citations, n_unique, "cite_source")
record_summary_table(calculated_counts)

unique_citations %>%
  dplyr::filter(stringr::str_detect(cite_label, "search")) %>%
  record_level_table(return = "DT")



