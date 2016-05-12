# --------------------------------------------------------
# Preprocess the Yelp business data for use in random forest model
# -Marilyn
# --------------------------------------------------------

# Download the Yelp CSV files from Blackboard, under Resources

setwd("~/0 Projects/0 Data Science/2016 EDSO Program/Capstone")
business <- read.csv("yelp_csv/yelp_academic_dataset_business.csv")
#View(business)

#user <- read.csv("yelp_csv/yelp_academic_dataset_user.csv")
#tip <- read.csv("yelp_csv/yelp_academic_dataset_tip.csv")
#review <- read.csv("yelp_csv/yelp_academic_dataset_review.csv")
#checkin <- read.csv("yelp_csv/yelp_academic_dataset_checkin.csv")



# -- functions --

stop.pos <- function(start.pos, string, delimiter) {
  return(c(regexpr(delimiter, substr(string, start.pos, nchar(string))))+start.pos-2*nchar(delimiter))
}

find.nth <- function(n, string, starts, stops) {
  return(substr(string, starts[n], stops[n]))
}

delimited_items <- function(delimiter, string) {
  starts <- c(gregexpr(paste("u",delimiter,sep=""), string)[[1]])
  if (starts[1] == -1) {
    return(NULL)
  } 
  starts <- starts + nchar(delimiter) + 1
  stops <- sapply(starts, stop.pos, string, delimiter) 
  items <- c()
  n <- 1:length(starts)
  items <- c(items, sapply(n, find.nth, string, starts, stops))
  return(items)
}

list_items <- function(string, delimiters) {
  items <- c()
  for (delimiter in delimiters) {
    items <- c(items, delimited_items(delimiter, string))
  }
  return(items)
}


# -- main --

# read the categories column
# Break out all the listed items in a string with format:
# "[u\"Women's Clothing\", u\"Men's Clothing\", u'Fashion', u'Shopping', u\"Children's Clothing\"]"

delimiters <- c("'", '\"')

# create a new column with separated business categories
stringlist <- as.character(business$categories)
business$categories.separated <- sapply(stringlist, list_items, delimiters)
#examples of accessing the categories in the reformatted column
#business$categories.separated[[1]]
#business$categories.separated[[1]][1]
#business$categories.separated[[1]][2]


# Make a new dataframe with just the category columns
business_categories <- business[, c("business_id", "name", "categories", "categories.separated")]
View(business_categories)

# expand the categories to individual columns
for (index in 1:nrow(business_categories)) {
  for (category in business_categories$categories.separated[[index]]) {
    business_categories[index, category] <- 1
  }
}
View(business_categories)
#write.csv(business_categories[, -4], file = "business_categories.csv")

# Select just the restaurants
restaurants <- subset(business_categories, business_categories$Restaurants == 1) 

business.restaurant <- subset(business, business$business_id %in% restaurants$business_id)
write.csv(business.restaurant[,-106], file = "business.restaurant.csv")
b2 <- read.csv("business.restaurant.csv")
#View (b2) 

# Handle NA values

for (feature in colnames(b2)) {
  na_count <- 0
  for(row in 1:nrow(b2)) {
    na_count <- na_count + is.na(b2[row,feature])
  }
  print(paste(na_count, "NA in", feature))
}
# The Hair Types columns are all NA values, let's remove them

dim(b2)  #21892 106
hair_col <- c()
for(colnum in 1:ncol(b2)) {
  if(substr(names(b2)[colnum], 12, 21) == "Hair.Types") {
    hair_col <- c(hair_col, names(b2)[colnum])
  }
}
hair_col
b3 <- b2[ , !(names(b2) %in% hair_col)]
dim(b3)    #21892 98
names(b3)


# The remaining columns look applicable to restaurants.

for (feature in colnames(b3)) {
  na_count <- 0
  for(row in 1:nrow(b3)) {
    na_count <- na_count + is.na(b3[row,feature])
  }
  if (na_count > 0) {
    print(paste(na_count, "NA in", feature))
  }
}
# NA remain only in 1462 rows of attributes.Price.Range
# That's not too many. Drop them.

unique(b3$attributes.Price.Range)
# 1 2 NA 3 4

b4 <- subset(b3, !is.na(b3[, "attributes.Price.Range"]))
dim (b4)  #20430 98
# 21892 - 1462 = 20430, check. :)


View(b4)

# Condense the operating hours to an indication if they are open weekends
b4$attributes.Open.Weekends <- (b4$hours.Sunday.open != "" | b4$hours.Saturday.open != "")

names(b4)
summary(b4$attributes.Open.Weekends)
## Mode   FALSE    TRUE    NA's 
## logical    7964   12466       0 


# now remove the hours columns
dim(b4)  #20430 99
hours <- c()
for(colnum in 1:ncol(b4)) {
  if(substr(names(b4)[colnum], 1, 5) == "hours") {
    hours <- c(hours, names(b4)[colnum])
  }
}
hours
b5 <- b4[ , !(names(b4) %in% hours)]
dim(b5)    #20430 85
names(b5)
str(b5)

b5$attributes.Open.Weekends <- as.factor(b5$attributes.Open.Weekends)
b5$attributes.Price.Range <- as.factor(b5$attributes.Price.Range)

str(b5)

hist(b5$longitude) # interesting. 3 clumps. West Coast, East Cost, Europe?
hist(b5$latitude) # range 35 to 55, northern hemisphere, more in warm climates
# but we already knew most records are from NV and AZ.
# phoenix 33
# bangor, ME 45
# london 51

for (r in 1:nrow(b5)){
  if (b5[r, "longitude"] < -100) { b5[r, "region"] <- "US West"}
  else if (b5[r, "longitude"] < -40) { b5[r, "region"] <- "US East"}
  else b5[r, "region"] <- "Europe"
}
b5$region <- as.factor(b5$region)
summary(b5$region)

summary(b5$review_count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.00    8.00   18.00   52.71   52.00 4578.00 

b5$log_review_count <- (log10(b5$review_count))
summary(b5$log_review_count)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4771  0.9031  1.2550  1.3220  1.7160  3.6610 

# require at least 10 reviewers
b6 <- subset(b5, b5[,"review_count"] >= 10)
dim(b6)  #13966 87
summary(b6$log_review_count)
b6$log_review_count <- as.factor(floor(b6$log_review_count))
summary(b6$log_review_count)
#     1     2     3   (10-100, 100-1000, or >1000 reviews)
# 11331  2587    48 

hist(b6$stars)
summary(b6$stars)

b6$high.rating <- b6$stars > 3.5
summary(b6$high.rating)

dim(b6) # 13966 88
removecols <- c("stars", "latitude", "longitude", "review_count",
                "X", "categories", "business_id", "name", "state",
                "full_address", "city", "neighborhoods", "type")
b6 <- b6[, !(names(b6) %in% removecols)]
dim(b6) # 13966 75

write.csv(b6, file = "business.preprocessed.csv")




