CodeBook
================

# 1\. Code Description

First stage of this project was to install and load the necessary
packages:  
`dplyr`  
`readR`  
`stringR`

``` r
install.packages("tidyverse")
install.packages("stringr")
library(tidyverse)
library(stringr)
```

It was necessary to download, save and unzip the files in a data
directory:

``` r
if(!file.exists("./data")){
        dir.create("./data")
        }

if(!file.exists("./data/UCI HAR Dataset")){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        destfile <- "./data/dataset.zip"
        download.file(fileUrl, destfile = destfile, method = "curl" )
        unzip(zipfile = destfile, exdir = "./data")
        }
```

All the necessary files were loaded into R using `readR` package. Each
experiment information was separated in 6 files divided in 2 sets (test
and train) - two files with the activities information (`y_test` and
`y_train`), two files with the measured data (`x_test` and `x_train`)
and two files with the subject identification (`subject_test` and
`subject_train`). I’ve also loaded a third file with the variables
labels `features`, that was later used to name the measured
variables:

``` r
x_test <- read_table("./data/UCI HAR Dataset/test/X_test.txt", col_names = FALSE)

x_train <- read_table("./data/UCI HAR Dataset/train/X_train.txt", col_names = FALSE)

y_test <- read_table("./data/UCI HAR Dataset/test/y_test.txt", col_names = "Code", col_types = cols(.default = col_factor(Code = col_factor(levels = c("1","2","3","4","5","6" )))))

y_train <- read_table("./data/UCI HAR Dataset/train/y_train.txt", col_names = "Code",  col_types = cols(.default = col_factor(Code = col_factor(levels = c("1","2","3","4","5","6" )))))

features <- read_table("./data/UCI HAR Dataset/features.txt", col_names = FALSE)

subject_test <- read_table("./data/UCI HAR Dataset/test/subject_test.txt", col_names = "Subject", col_types = cols(.default = col_number()))

subject_train <- read_table("./data/UCI HAR Dataset/train/subject_train.txt", col_names = "Subject", col_types = cols(.default = col_number()))

activities_labels <- read_table("./data/UCI HAR Dataset/activity_labels.txt", col_names = c("Code","Activity"), col_types = cols(Code = col_factor(levels = c("1","2","3","4","5","6" ))))
```

I’ve serarate each row in the feature dataset to extract only the
variable name without the number preecededing it.

``` r
features <- separate(features,X1, sep = " ", into = c("Rank","Feature"))

colnames(x_test) <- features$Feature 

colnames(x_train) <- features$Feature
```

With all the files loaded and prepared, the next step was to bind the
files together in 2 datasets. One with the `train` data and the other
with the
`test`data.

``` r
test_full <- bind_cols(subject_test,y_test,x_test) %>% mutate(Code = factor(Code, levels = c("1","2","3","4","5","6" )))

train_full <- bind_cols(subject_train,y_train,x_train) %>% mutate(Code = factor(Code, levels = c("1","2","3","4","5","6" )))
```

I’ve recoded the activity collumn with descriptive data instead of
numbers

``` r
test_full <- left_join(test_full, activities_labels, by = "Code" )

train_full <- left_join(train_full, activities_labels, by = "Code" )
```

I’ve created an index (`pattern`) to filter the variables that
registered means and standard deviation acoording to instructions. I
decided only to consider the variables which std() and mean() were found
at the end of the variable label. So I created a regular expression
(“((M|m)ean|(S|s)td)(\\(\\)-(X|Y|Z))$”) to extract this data through
`stringR`:str\_subset command. Then train and test files were bind
together and I renamed all the variables with descriptive (a kind long)
labels.

``` r
pattern <- "((M|m)ean|(S|s)td)(\\(\\)-(X|Y|Z))$"

index_test <- str_subset(names(test_full), pattern = pattern)

index_train <-  str_subset(names(train_full), pattern = pattern)

test <- test_full %>%
        select(Subject, Activity, all_of(index_test))

train <- train_full %>%
        select(Subject, Activity, all_of(index_train))

joined_table <- test %>%
        bind_rows(train) %>%
        arrange(Subject) %>% 
        mutate(Subject = Activity = parse_factor(Activity))

names(joined_table) <- str_replace(names(joined_table), "^t", "Time")

names(joined_table) <- str_replace(names(joined_table), "^f", "Frequency")

names(joined_table) <- str_replace(names(joined_table), "Acc", "Accelerometer")

names(joined_table) <- str_replace(names(joined_table), "Gyro", "Gyroscope")

names(joined_table) <- str_replace(names(joined_table), "Mag", "Magnitude")

names(joined_table) <- str_replace(names(joined_table), "BodyBody", "Body")
```

And finally, I’ve created a second, independent tidy data set with the
average of each variable for each activity and each subject and saved it
as a txt file, following projest’s step 5.

``` r
tidy_means <- joined_table %>%
        group_by(Subject,Activity) %>% summarise_all(.funs = mean)

write.table(tidy_means, file = "./data/tidy.txt", quote = FALSE, row.names = FALSE)
```

# 2\. Variables description

| Variable                       | Description                                                     |
| ------------------------------ | --------------------------------------------------------------- |
| Subject                        | Each participan numbered from 1 to 30 \[number\]                |
| Activity                       | Activity during experiment (standing, walking, etc.) \[factor\] |
| TimeBodyAccelerometer          | Mean and Standad deviation in X,Y,Z axis \[number\]             |
| TimeGravityAccelerometer       | Mean and Standad deviation in X,Y,Z axis \[number\]             |
| TimeBodyAccelerometerJerk      | Mean and Standad deviation in X,Y,Z axis \[number\]             |
| TimeBodyGyroscope              | Mean and Standad deviation in X,Y,Z axis \[number\]             |
| TimeBodyGyroscopeJerk          | Mean and Standad deviation in X,Y,Z axis \[number\]             |
| FrequencyBodyAccelerometer     | Mean and Standad deviation in X,Y,Z axis \[number\]             |
| FrequencyBodyAccelerometerJerk | Mean and Standad deviation in X,Y,Z axis \[number\]             |
| FrequencyBodyGyroscope         | Mean and Standad deviation in X,Y,Z axis \[number\]             |

-----
