library(dplyr)
library(stringr)
library(jsonlite)
library(tidyr)

setwd("~/Yelp")

#convert json file to df
print("Converting business data json to data frame...")
df_bus <- stream_in(file("yelp_businesses.json"))

# convert to tibble for better readbility
df_bus <- as_data_frame(df_bus)

# Detect whether hipster true: true is part of attributes string
print("Adding label columns to business data frame...")
df_bus <- df_bus %>% mutate(hipster = str_detect(attributes, "'hipster': True") * 1)
df_bus <- df_bus %>% mutate(trendy = str_detect(attributes, "'trendy': True") * 1)
df_bus <- df_bus %>% mutate(classy = str_detect(attributes, "'classy': True") * 1)
df_bus <- df_bus %>% mutate(divey = str_detect(attributes, "'divey': True") * 1)
df_bus <- df_bus %>% mutate(touristy = str_detect(attributes, "'touristy': True") * 1)
df_bus <- df_bus %>% mutate(casual = str_detect(attributes, "'casual': True") * 1)
df_bus <- df_bus %>% mutate(intimate = str_detect(attributes, "'intimate': True") * 1)
df_bus <- df_bus %>% mutate(romantic = str_detect(attributes, "'romantic': True") * 1)
df_bus <- df_bus %>% mutate(upscale = str_detect(attributes, "'upscale': True") * 1)

df_bus <- df_bus %>% mutate(dessert = str_detect(attributes, "'dessert': True") * 1)
df_bus <- df_bus %>% mutate(latenight = str_detect(attributes, "'latenight': True") * 1)
df_bus <- df_bus %>% mutate(lunch = str_detect(attributes, "'lunch': True") * 1)
df_bus <- df_bus %>% mutate(dinner = str_detect(attributes, "'dinner': True") * 1)
df_bus <- df_bus %>% mutate(breakfast = str_detect(attributes, "'breakfast': True") * 1)
df_bus <- df_bus %>% mutate(brunch = str_detect(attributes, "'brunch': True") * 1)

# Read in reviews data
print("Converting reviews json to data frame...")
df_rev <- stream_in(file("yelp_reviews.json"))
df_rev <- as_data_frame(df_rev)

# Left join business first because we filtered to only restaurants ealier and we only want business ID's that are still in df_bus
print("Joining business and review data by business id...")
data <- df_bus %>% left_join(df_rev, by="business_id")

#Create rows that indicate whether it has a Restaurant/Nightlife category. These words are unique to the category so it's okay to just detect the string, though we may want to find exact match in future if data changes.
print("Filtering for only Resaturant/Nightlife/Food categories and US states...")
data <- data %>% mutate(restaurant = str_detect(categories, "Restaurant")) 
data <- data %>% mutate(nightlife = str_detect(categories, "Nightlife"))
# Create row that indicates whether it has a Food category. We use regex and gsub to detect an exact "Food" match with no other things, e.g. "Fast Food"
data <- data %>% mutate(food = str_detect(gsub(" ", "", categories, fixed = TRUE), "\\bFood\\b"))
data <- data %>% filter(restaurant == TRUE | food == TRUE | nightlife == TRUE)

# For labelling problem we keep all US states
state_keeps = c("AZ", "NC", "NV", "OH", "WI", "PA", "IL")
data <- data %>% filter(state %in% state_keeps)

# Keeping only relevant rows
print("Keeping only relevant rows...")
keeps <- c("business_id", "name", "city", "state", "review_count", "hipster", "trendy", "classy", "divey", "touristy", "casual", "intimate", "romantic", "upscale", "dessert", "latenight", "lunch", "dinner", "breakfast", "brunch", "text", "restaurant", "nightlife", "food")
data <- data[keeps]

# Group by business ID and consolidate data for a single business. Aggregates all reviews into one data point, one doc
print("Collapsing all reviews and aggregate data by business...")
data <- data %>% group_by(business_id) %>% summarize(name=first(name), review_count=mean(review_count), state=first(state), city=first(city), hipster=first(hipster), trendy=first(trendy), classy=first(classy), divey=first(divey), touristy=first(touristy), casual=first(casual), intimate=first(intimate), romantic=first(romantic), upscale=first(upscale), dessert=first(dessert), latenight=first(latenight), lunch=first(lunch), dinner=first(dinner), brunch=first(brunch), breakfast=first(breakfast), restaurant=first(restaurant), food=first(food), nightlife=first(nightlife), text=paste(text, collapse=" "))

# Only keep businesses that have more than 20 reviews
data <- data %>% filter(review_count > 19)

# Only keep data that is labeled
# Split into goodfor and ambience datasets to be modelled separately
data_goodfor <- data %>% filter(dessert+latenight+lunch+dinner+brunch+breakfast > 0)

data_ambience <- data %>% filter(hipster+trendy+classy+divey+touristy+casual+intimate+romantic+upscale > 0)

# Create goodfor corpus and dfm

corpus_goodfor <- corpus(data_goodfor$text)

docvars(corpus_goodfor, "business_id") <- data_goodfor$business_id
docvars(corpus_goodfor, "dessert") <- data_goodfor$dessert
docvars(corpus_goodfor, "latenight") <- data_goodfor$latenight
docvars(corpus_goodfor, "lunch") <- data_goodfor$lunch
docvars(corpus_goodfor, "dinner") <- data_goodfor$dinner
docvars(corpus_goodfor, "brunch") <- data_goodfor$brunch
docvars(corpus_goodfor, "breakfast") <- data_goodfor$breakfast

tags_goodfor <- c("dessert", "latenight", "lunch", "dinner", "brunch", "breakfast")

dfm_goodfor <- dfm(corpus_goodfor, remove = stopwords("english"), stem = TRUE, removePunct = TRUE)
dfm_goodfor <- dfm_trim(dfm_goodfor, sparsity = .9)

# Entire data corpus and dfm

corpus <- corpus(data$text)

docvars(corpus, "business_id") <- data$business_id
docvars(corpus, "hipster") <- data$hipster
docvars(corpus, "trendy") <- data$trendy
docvars(corpus, "classy") <- data$classy
docvars(corpus, "touristy") <- data$touristy
docvars(corpus, "divey") <- data$divey
docvars(corpus, "casual") <- data$casual
docvars(corpus, "intimate") <- data$intimate
docvars(corpus, "romantic") <- data$romantic
docvars(corpus, "upscale") <- data$upscale
docvars(corpus, "dessert") <- data$dessert
docvars(corpus, "latenight") <- data$latenight
docvars(corpus, "lunch") <- data$lunch
docvars(corpus, "dinner") <- data$dinner
docvars(corpus, "brunch") <- data$brunch
docvars(corpus, "breakfast") <- data$breakfast

tags <- c("hipster", "trendy", "classy", "divey", "touristy", "casual", "intimate", "romantic", "upscale", "dessert", "latenight", "lunch", "dinner", "brunch", "breakfast")

dfm <- dfm(corpus, remove = stopwords("english"), stem = TRUE, removePunct = TRUE)
dfm <- dfm_trim(dfm, sparsity = .9)
