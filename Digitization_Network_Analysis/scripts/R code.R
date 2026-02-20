
rm(list = ls())
gc()

# Load libraries 
packages <- c("haven", "dplyr", "tidyr", "fixest", "tidyverse", 
              "httr", "jsonlite", "stringr", "igraph", "plm", 
              "marginaleffects", "ggplot2", "ggraph", "tidygraph", "here")

installed_packages <- packages %in% rownames(installed.packages())

# Install missing packages (if any)
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

for (pkg in packages) {
  library(pkg, character.only = TRUE)
}


#######################
# Reproducibility note !!!!!!!!!!!!!!!!!!!!
#######################

# For reproducibility, please open the RStudio Project file inside
# the "Coding_task_Mack_tosend" folder before running this script.
#
# All paths below are defined relative to the project root using `here()`,
# so the code should run on any machine without needing to change paths.

here()


data_raw   <- here("data_raw")
data_clean <- here("data_clean")
scripts    <- here("scripts")
output     <- here("output")
figure    <- here("output", "figure")

raw_data <- read_dta(here("data_raw", "loans_merged.dta"))

#######################
# Quick look at the data
#######################

View(raw_data)

# How many unique books are in the data?
n_book <- length(unique(raw_data$bib_doc_id))
n_book

# Sort by book ID, loan year and scanned year
# and move the key identifiers to the front
raw_data <- raw_data %>%
  arrange(bib_doc_id, year_loaned,year_scanned) %>%
  relocate(bib_doc_id, year_loaned,year_scanned)

#check year_loaned
summary(raw_data$year_loaned)

#check year_scanned
summary(raw_data$year_scanned)

# For each book, how many years appear?
checking <- raw_data %>%
  group_by(bib_doc_id) %>%
  summarise(
    scan_year = min(year_scanned, na.rm = FALSE),
    min = min(year_loaned, na.rm = TRUE),
    max = max(year_loaned, na.rm = TRUE), 
    n_event = length(year_loaned) #at least one loan in the sample period
  )%>%
  arrange(n_event)

head(checking) 

# Look at one example book that has multiple loans in the same year,
# just to understand the event-level structure
raw_data %>%
  filter(bib_doc_id == 3127) #having more than one loan in the same period


#######################
# Answer 1
#######################

# Build a balanced book–year panel from 2003 to 2011,
# and count how many loan events happen in each book-year
master_dta <- raw_data %>%
  group_by(bib_doc_id, year_loaned) %>%
  summarise(
    n_loan = n(),
    .groups = "drop"
  ) %>%
  group_by(bib_doc_id) %>%
  complete(year_loaned = 2003:2011, fill = list(n_loan = 0)) %>%
  ungroup()

head(master_dta) #only have bib_doc_id, year_loaned, n_loan

#----------------------------
# Adding location (for later analysis)
#----------------------------

# get a unique mapping between each book and its location
location_data <- raw_data %>%
  distinct(bib_doc_id, location)

# check if any book is linked to more than one location
location_check <- location_data %>%
  group_by(bib_doc_id) %>%
  summarise(n_loc = n_distinct(location), .groups = "drop") %>%
  filter(n_loc != 1)

# take a look at problematic cases (should be empty)
location_check

# merge the location info into the master dataset by book ID
master_dta <- master_dta %>%
  left_join(location_data, by = "bib_doc_id")

head(master_dta)

#----------------------------
# Adding scanned year (for later analysis)
#----------------------------

# get a unique mapping between each book and its scanned year
year_scanned_data <- raw_data %>%
  distinct(bib_doc_id, year_scanned)

# merge the scanned year info into the master dataset by book ID
master_dta <- master_dta %>%
  left_join(year_scanned_data, by = "bib_doc_id")

head(master_dta)

#----------------------------
# Adding post_scanned (0 or 1)(for later analysis)
#----------------------------

scan_year <- raw_data %>%
  filter(!is.na(year_scanned)) %>%
  group_by(bib_doc_id) %>%
  summarise(first_scan_year = min(year_scanned), .groups = "drop")

master_dta <- master_dta %>%
  left_join(scan_year, by = "bib_doc_id")

master_dta <- master_dta %>%
  mutate(
    post_scanned = ifelse(
      is.na(first_scan_year), 
      0,                              
      as.integer(year_loaned >= first_scan_year)  
    )
  )

head(master_dta)

# should be 792,054 = 88,006 books * 9 years
nrow(master_dta)

# should be 88,006 each year
master_dta %>% count(year_loaned)

# each book should appear 9 times
master_dta %>% 
  count(bib_doc_id) %>% 
  summarise(min=n_distinct(n), 
            max=max(n)
  )

# save master_dta
write_dta(master_dta, file.path(data_clean, "master.dta"))


#######################
# Answer 2 — TWFE main effect
#######################

table5_1 <- feols(
  log(n_loan + 1) ~ post_scanned |
    bib_doc_id + year_loaned^location,
  data = master_dta,
  cluster = ~bib_doc_id
)

master_dta <- master_dta %>%
  mutate(any_loan = as.integer(n_loan > 0))



table5_2 <- feols(
  any_loan ~ post_scanned |
    bib_doc_id + year_loaned^location,
  data = master_dta,
  cluster = ~bib_doc_id
)

# Labels for LaTeX table
setFixest_dict(c(
  post_scanned           = "Post-Scanned",
  "log(n_loan + 1)"      = "Log(Loans+1)",
  any_loan               = "Any Loan",
  bib_doc_id             = "Book FE",
  "year_loaned^location" = "Year-Location FE"
))



etable(
  table5_1, table5_2,
  fitstat   = ~ n,
  title     = "Replication of Table 5 (Columns 1--2)",
  label     = "tab:table5",
  notes     = "\\vspace{0.5em} This table replicates Table~5, columns~(1) and~(2) of Nagaraj and Reimers (2021). The dependent variables are $\\log(\\text{Loans}+1)$ and an indicator for any loan. All specifications include book and year-by-location fixed effects. Standard errors, clustered at the book level, are reported in parentheses.",
  tex       = TRUE,
  file      = file.path(output, "table_5.tex"),
  replace   = TRUE
)

setFixest_dict()



#######################
# Answer 3 — Sun & Abraham robustness
#######################

table5_1_robust <- feols(
  log(n_loan + 1) ~ sunab(year_scanned, year_loaned) |
    bib_doc_id + year_loaned^location,
  data = master_dta,
  cluster = ~bib_doc_id
)


table5_2_robust <- feols(
  any_loan ~ sunab(year_scanned, year_loaned) |
    bib_doc_id + year_loaned^location,
  data = master_dta,
  cluster = ~bib_doc_id
)

setFixest_dict(c(
  "log(n_loan + 1)"      = "Log(Loans+1)",
  any_loan               = "Any Loan",
  bib_doc_id             = "Book FE",
  "year_loaned^location" = "Year-Location FE"
))

etable(
  table5_1_robust, table5_2_robust,
  agg       = "att",
  fitstat   = ~ n,
  title     = "Robustness Check: Sun \\& Abraham (2021) Estimator",
  label     = "tab:table5robust",
  notes     = "\\vspace{0.5em} This table re-estimates Table~\\ref{tab:table5} using the Sun and Abraham (2021) heterogeneity-robust estimator. Reported coefficients correspond to aggregated average treatment effects on the treated (ATT) across cohorts. All specifications include book and year-by-location fixed effects, with standard errors clustered at the book level.",
  tex       = TRUE,
  file      = file.path(output, "table_5_robust.tex"),
  replace   = TRUE
)

setFixest_dict()


#######################
# Answer 4
#######################

# ==========================================
# Step1:Retrieves book metadata from Open Library API
# ==========================================

# Step1 for Answer 4 script retrieves book metadata from Open Library API
# and merges it with the main dataset to look at how digitization
# affects different types of books differently.

# Some of the API stuff and error handling was cleaned up with help from
# ChatGPT and Gemini to make it more robust. But all the actual research
# decisions, what data to use, and how to analyze it - that's all me.

# References:
# - Tidyverse: https://www.tidyverse.org/
# - httr package: https://httr.r-lib.org/
# - jsonlite: https://arxiv.org/abs/1403.2803
# - Open Library API docs: https://openlibrary.org/developers/api
# - R Base Documentation


save_file <- file.path(output, "original_data", "open_library_data.rds")


#----------------------------
# Function to fetch book data :
# Note: The API querying structure was tweaked with some AI assistance
# to handle edge cases better. Variable choices and analysis logic are mine.
#----------------------------

fetch_open_library <- function(oclc) {
  # Skip if no OCLC number provided
  if (is.na(oclc) || oclc == "") return(NULL)
  
  # Build the API request URL - using OCLC identifier
  url <- paste0("https://openlibrary.org/api/books?bibkeys=OCLC:", 
                oclc, 
                "&format=json&jscmd=data")
  
  # Try to fetch data, return NULL if anything goes wrong
  tryCatch({
    res <- GET(url, timeout(10))  # 10 second timeout 
    
    if (status_code(res) == 200) {
      content_data <- fromJSON(content(res, "text", encoding = "UTF-8"))
      
      # Extract book info using the OCLC key
      book_key <- paste0("OCLC:", oclc)
      book_info <- content_data[[book_key]]
      
      # If actually got book data back, parse it
      if (!is.null(book_info)) {
        
        # Extract title - pretty straightforward
        title_val <- ifelse(!is.null(book_info$title), 
                            book_info$title, 
                            NA)
        
        # For authors, combine multiple if there are any
        author_val <- ifelse(!is.null(book_info$authors), 
                             paste(book_info$authors$name, collapse = "|"), 
                             NA)
        
        # Keep author keys too - might be useful later
        author_key_val <- ifelse(!is.null(book_info$authors), 
                                 paste(book_info$authors$key, collapse = "|"), 
                                 NA)
        
        # Subjects are interesting for categorization
        subjects_val <- ifelse(!is.null(book_info$subjects), 
                               paste(book_info$subjects$name, collapse = "|"), 
                               NA)
        
        # Publishing date - converting to character to be safe
        pub_date_val <- ifelse(!is.null(book_info$publish_date), 
                               as.character(book_info$publish_date), 
                               NA)
        
        # Page count - could be useful for analysis
        pages_val <- ifelse(!is.null(book_info$number_of_pages), 
                            book_info$number_of_pages, 
                            NA)
        
        # Publisher info
        publisher_val <- ifelse(!is.null(book_info$publishers), 
                                paste(book_info$publishers$name, collapse = "|"), 
                                NA)
        

        return(data.frame(
          api_title      = title_val,
          api_author     = author_val,
          author_keys    = author_key_val,
          subjects       = subjects_val,
          publish_date   = pub_date_val,
          page_count     = pages_val,
          publisher      = publisher_val,
          stringsAsFactors = FALSE
        ))
      }
    }
  }, error = function(e) {
    # Silently fail and return NULL 
    return(NULL)
  })
  
  # Default return if nothing worked
  return(NULL)
}


# Get the first 40k OCLC numbers 
targets_oclc <- raw_data %>%
  select(bib_doc_id, title_display, `_oclc`) %>%
  distinct(bib_doc_id, .keep_all = TRUE) %>%
  mutate(clean_oclc = readr::parse_number(`_oclc`)) %>%
  filter(!is.na(clean_oclc)) %>%
  head(40000)  # Just taking the top 40k for now


# Initialize 
remaining_targets <- targets_oclc
results_list <- list()


# --------------------------------------------------
# Reproducible API retrieval with caching
# --------------------------------------------------
# Note: I have pre-included the cached RDS file in the output 
# folder to save you hours of API calling time.

if (file.exists(save_file)) {
  
  cat("Cached Open Library data found. Loading from disk...\n")
  openlib_df <- readRDS(save_file)
  
} else {
  
  cat("No cached data found. Querying Open Library API (this may take a while)...\n")
  
  results_list <- list()
  pb <- txtProgressBar(min = 0, max = nrow(targets_oclc), style = 3)
  
  for (i in 1:nrow(targets_oclc)) {
    
    res <- fetch_open_library(targets_oclc$clean_oclc[i])
    
    if (!is.null(res)) {
      res$bib_doc_id <- targets_oclc$bib_doc_id[i]
      results_list[[as.character(res$bib_doc_id)]] <- res
    }
    
    setTxtProgressBar(pb, i)
    
    if (i %% 50 == 0 || i == nrow(targets_oclc)) {
      saveRDS(bind_rows(results_list), save_file)
    }
    
    Sys.sleep(0.1)
  }
  
  close(pb)
  
  openlib_df <- bind_rows(results_list)
  saveRDS(openlib_df, save_file)
  
  cat("API retrieval complete. Data cached for future runs.\n")
}




# ==========================================
# Step2: doing analysis
# ==========================================

targets_oclc <- readRDS(
  here::here("output", "original_data", "open_library_data.rds")
)

head(targets_oclc)
names(targets_oclc)

#----------------------------
# Get valid author–topic pairs
#----------------------------

# Turn the "subjects" column into a clean list!!
# Some rows separate subjects with "|", and some rows are just empty
data <- targets_oclc %>%
  mutate(subjects_list = map(subjects, ~ {
    # First, check if it's empty or NA — if so, return an empty vector
    if (is.na(.x) || .x == "") return(character(0))
    str_trim(strsplit(as.character(.x), "\\|")[[1]])
  }))


# Keep only rows where have an author
# and at least one topic in the subjects list
edges_author_topic <- data %>%
  filter(!is.na(api_author) & map_lgl(subjects_list, ~ length(.x) > 0)) %>%
  select(api_author, subjects_list) %>%
  # Critical step: break down subject lists into single rows to build the network connections.
  unnest_longer(subjects_list) %>%
  rename(author = api_author, topic = subjects_list) %>%
  filter(!is.na(topic) & topic != "") %>%
  distinct()

head(edges_author_topic)

# To prevent authors and topics with the same name from being merged
# adding suffix: "_author" for authors and "_topic" for topics
edges_author_topic <- edges_author_topic %>%
  mutate(
    author_node = paste0(author, "_author"),
    topic_node  = paste0(topic, "_topic")
  )


all_nodes <- unique(c(edges_author_topic$author_node,
                      edges_author_topic$topic_node))

head(all_nodes)


# Mark which nodes are authors
# If a node name ends with "_author", it belongs to the author side
node_type <- str_ends(all_nodes, "_author")
head(node_type)

# Build the bipartite graph
g_bipartite <- graph_from_data_frame(
  edges_author_topic[, c("author_node", "topic_node")],
  directed = FALSE,
  vertices = data.frame(
    name = all_nodes,
    type = node_type
  )
)

g_bipartite

# See which nodes have the highest degree centrality
head(sort(degree(g_bipartite), decreasing = TRUE), 5)


# --------------------------------------------------
# Visualization:
# This part was not required for the assignment,
# but I still tried to visualize the network structure,
# which I think is interesting.
# Since it was not required, I only included these plots
# in the report, not in the main submission file.
# --------------------------------------------------

# Take the 50 most connected nodes (just to make the graph readable)
top_nodes <- names(sort(degree(g_bipartite), decreasing = TRUE))[1:50]

sub_bi <- induced_subgraph(g_bipartite, top_nodes)

# Clean the names a little bit
V(sub_bi)$label <- V(sub_bi)$name
V(sub_bi)$label <- str_remove(V(sub_bi)$label, "_author$|_topic$")
V(sub_bi)$label <- str_trunc(V(sub_bi)$label, 15)
V(sub_bi)$node_category <- ifelse(V(sub_bi)$type, "Author", "Topic")

ggraph(sub_bi, layout = "stress") +
  geom_edge_link(alpha = 0.2, colour = "grey70") +
  geom_node_point(aes(color = node_category), size = 3.5) + 
  geom_node_text(aes(label = label),
                 repel = TRUE,
                 size = 2.5) +
  labs(title = "Key Authors and Topics (Top 50 Nodes)",
       color = "Node Category") +
  scale_color_manual(values = c("Author" = "#00BFC4", "Topic" = "#F8766D")) + 
  theme_void()


# Also looked at author network only
# Focus only on the authors to simplify the math
g_authors <- bipartite_projection(g_bipartite, which = "true")

top_authors <- names(sort(degree(g_authors), decreasing = TRUE))[1:30]

sub_auth <- induced_subgraph(g_authors, top_authors)

V(sub_auth)$label <- str_remove(V(sub_auth)$name, "_author$")
V(sub_auth)$label <- str_trunc(V(sub_auth)$label, 20)

ggraph(sub_auth, layout = "kk") +
  geom_edge_link(aes(alpha = weight),
                 colour = "steelblue",
                 width = 0.4) +   
  scale_edge_alpha(range = c(0.1, 0.6)) +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = label),
                 repel = TRUE,
                 size = 3) +
  labs(title = "Author Network (Top 30 by Degree)") +
  theme_graph()


# how many different topics each author covers (this is "Centrality" score)
author_degree <- degree(g_authors)
head(author_degree)

# Clean up the names and put the scores into a nice table 
# so using it in the regression
author_cent_df <- data.frame(
  api_author = str_remove(names(author_degree), "_author$"),
  author_centrality = as.numeric(author_degree),
  stringsAsFactors = FALSE
)

head(author_cent_df)
# --------------------------------------------------
# Give each book the author's network score
# 
# Idea:
# I take the author's centrality score from the network
# and attach it to every book written by that author.
# 
# Intuition:
# Well-known authors are more visible, so their books
# are probably more likely to be borrowed.
# --------------------------------------------------

book_author_cent <- data %>%
  select(bib_doc_id, api_author) %>%
  mutate(bib_doc_id = as.character(bib_doc_id)) %>%  
  left_join(author_cent_df, by = "api_author") %>%
  distinct(bib_doc_id, .keep_all = TRUE)  

# Merge the author score into the main analysis dataset

master <- master_dta %>%
  mutate(bib_doc_id = as.character(bib_doc_id)) %>% 
  left_join(
    book_author_cent %>% select(bib_doc_id, author_centrality),
    by = "bib_doc_id"
  )

#Clean up missing values and scale the centrality variable
master <- master %>%
  mutate(
    author_centrality = ifelse(is.na(author_centrality), 0, author_centrality),
    centrality_scaled = author_centrality / 100,
    # log1p is safer than log() because it handles zero loans correctly
    log_n_loan = log1p(n_loan)
  )

head(master)

# If a book has no author information, treat the score as 0
# (meaning: no observable network prominence)

master$author_centrality[is.na(master$author_centrality)] <- 0


# --------------------------------------------------
# Panel Fixed Effects Model
# --------------------------------------------------

# Estimate the model:
# n_loan is explained by digitization (post_scanned),
# author centrality, and their interaction
# I control for book fixed effects and year fixed effects
# I've added 'cluster = ~bib_doc_id' to handle serial correlation within books.

model<- feols(
  log_n_loan ~ post_scanned * centrality_scaled | bib_doc_id + year_loaned,
  data = master,
  cluster = ~bib_doc_id
)

summary(model)


model_sunab <- feols(
  log_n_loan ~ sunab(first_scan_year, year_loaned) | bib_doc_id + year_loaned,
  data = master,
  cluster = ~bib_doc_id
)

summary(model_sunab)


iplot(model_sunab) 



# --------------------------------------------------
# Grouping authors based on their centrality scores
# --------------------------------------------------

master <- master %>%
  mutate(cent_level = ifelse(
    author_centrality > median(author_centrality[author_centrality > 0], na.rm = TRUE), 
    "High Centrality", "Low Centrality"
  ))

# Estimate the treatment effect for each group
model_group <- feols(
  log_n_loan ~ i(cent_level, post_scanned) | bib_doc_id + year_loaned,
  data = master,
  cluster = ~bib_doc_id
)

summary(model_group )


iplot(model_group, 
      main = "Effect of Digitization by Author Centrality Group",
      dict = c("High Centrality" = "High Centrality Authors", 
               "Low Centrality" = "Low Centrality Authors"))



# export both panels as a single high-res PNG for the report
png(here("output", "figure", "event_study.png"),
    width = 8, height = 3.8, units = "in", res = 300)

par(mfrow = c(1, 2), mar = c(4, 4, 2.5, 1))

iplot(model_sunab,
      main = "Effect on log(Loans+1)",
      xlab = "Years relative to digitization",
      ylab = "Change in log borrowing")

iplot(model_group,
      main = "Effect by Author Centrality",
      xlab = "",
      ylab = "Change in log borrowing")

dev.off()
par(mfrow = c(1, 1))

#######################
# Answer 5
#######################


# These files are ready to be compiled into the final
# `Lu_Pei-Chin_output.tex` document for presentation.
