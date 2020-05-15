# Install and load the packages from tidyverse and stringR; 

install.packages("tidyverse")
install.packages("stringr")
library(tidyverse)
library(stringr)

# Download and unzip the files from Coursera Site

if(!file.exists("./data")){
        dir.create("./data")
        }

if(!file.exists("./data/UCI HAR Dataset")){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        destfile <- "./data/dataset.zip"
        download.file(fileUrl, destfile = destfile, method = "curl" )
        unzip(zipfile = destfile, exdir = "./data")
        }


# Load files into R using readR package

x_test <- read_table("./data/UCI HAR Dataset/test/X_test.txt", col_names = FALSE)

x_train <- read_table("./data/UCI HAR Dataset/train/X_train.txt", col_names = FALSE)

y_test <- read_table("./data/UCI HAR Dataset/test/y_test.txt", col_names = "Code", col_types = cols(Code = col_factor(levels = c("1","2","3","4","5","6"))))

y_train <- read_table("./data/UCI HAR Dataset/train/y_train.txt", col_names = "Code",  col_types = cols(Code = col_factor(levels = c("1","2","3","4","5","6"))))

features <- read_table("./data/UCI HAR Dataset/features.txt", col_names = FALSE)

subject_test <- read_table("./data/UCI HAR Dataset/test/subject_test.txt", col_names = "Subject", col_types = cols(.default = col_number()))

subject_train <- read_table("./data/UCI HAR Dataset/train/subject_train.txt", col_names = "Subject", col_types = cols(.default = col_number()))

activities_labels <- read_table("./data/UCI HAR Dataset/activity_labels.txt", col_names = c("Code","Activity"), col_types = cols(Code = col_factor(levels = c("1","2","3","4","5","6" ))))

# Preparing the labels and renaming the collumns with the actual variables being measured 

features <- separate(features,X1, sep = " ", into = c("Rank","Feature"))

colnames(x_test) <- features$Feature 

colnames(x_train) <- features$Feature

# Binding the colluns with the subject identification (subject_test), the activity (y_test) and the data collected (x_test)

test_full <- bind_cols(subject_test,y_test,x_test) %>% 
        mutate(Code = factor(Code, levels = c("1","2","3","4","5","6" )))

train_full <- bind_cols(subject_train,y_train,x_train) %>% 
        mutate(Code = factor(Code, levels = c("1","2","3","4","5","6" )))

# Recode the activity collumn to express the actual activity being measured

test_full <- left_join(test_full, activities_labels, by = "Code" )

train_full <- left_join(train_full, activities_labels, by = "Code" )


# Creating an index to filter the variables that registered means and standard deviation acoording to instructions. I decided only to consider the variables which std() and mean() were found at the end of the variable lalel. So I created a regular expression ("((M|m)ean|(S|s)td)(\\(\\)-(X|Y|Z))$") to extrat this data through stringR:str_subset command.

pattern <- "((M|m)ean|(S|s)td)(\\(\\)-(X|Y|Z))$"

index_test <- str_subset(names(test_full), pattern = pattern)

index_train <-  str_subset(names(train_full), pattern = pattern)


# Create final test table with identification of the subject in the first column, activity being measured in the second collumn and the rest os the collumns providing the data measured during the experiment

test <- test_full %>% select(Subject, Activity, all_of(index_test))

train <- train_full %>% select(Subject, Activity, all_of(index_train))

joined_table <- test %>%
        bind_rows(train) %>%
        arrange(Subject) %>% 
        mutate(Activity = parse_factor(Activity))

# Rename variables with descriptive variable names

names(joined_table) <- str_replace(names(joined_table), "^t", "Time")
names(joined_table) <- str_replace(names(joined_table), "^f", "Frequency")
names(joined_table) <- str_replace(names(joined_table), "Acc", "Accelerometer")
names(joined_table) <- str_replace(names(joined_table), "Gyro", "Gyroscope")
names(joined_table) <- str_replace(names(joined_table), "Mag", "Magnitude")
names(joined_table) <- str_replace(names(joined_table), "BodyBody", "Body")

# Create a second independent data set with the average of each variable for each activity and each subject.

tidy_means <- joined_table %>%
        group_by(Subject,Activity) %>% summarise_all(.funs = mean)

write.table(tidy_means, file = "./data/tidy.txt", quote = FALSE, row.names = FALSE)
        
