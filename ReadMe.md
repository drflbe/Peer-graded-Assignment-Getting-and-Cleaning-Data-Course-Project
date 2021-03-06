Introduction
------------

This is my submission for the peer graded assignment: Getting and
Cleaning Data Course Project. The purpose of this project is to
demonstrate the ability to collect, work with, and clean a data set. Its
specific objectives are:

##### Create one r script called run\_analysis.R that does the following:

1.  Merges the training and the test sets to create one data set.
2.  Extracts only the measurements on the mean and standard deviation
    for each measurement.
3.  Uses descriptive activity names to name the activities in the data
    set
4.  Appropriately labels the data set with descriptive variable names.
5.  From the data set in step 4, creates a second, independent tidy data
    set with the average of each variable for each activity and each
    subject.

#### Review Criteria:

1.  The submitted data set is tidy.
2.  The Github repo contains the required scripts.
3.  GitHub contains a code book that modifies and updates the available
    codebooks with the data to indicate all the variables and summaries
    calculated, along with units, and any other relevant information.
4.  The README that explains the analysis files is clear and
    understandable.
5.  The work submitted for this project is the work of the student who
    submitted it.

#### The dataset

The data used in this study was obtained from
<a href="http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones#" class="uri">http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones#</a><sup>1</sup>,
and consists of experiments carried out with a group of 30 volunteers
within an age bracket of 19-48 years. Each person performed six
activities (WALKING, WALKING\_UPSTAIRS, WALKING\_DOWNSTAIRS, SITTING,
STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the
waist. Using its embedded accelerometer and gyroscope, we captured
3-axial linear acceleration and 3-axial angular velocity at a constant
rate of 50Hz. The experiments have been video-recorded to label the data
manually. The obtained dataset has been randomly partitioned into two
sets, where 70% of the volunteers was selected for generating the
training data and 30% the test data. The sensor signals (accelerometer
and gyroscope) were pre-processed by applying noise filters and then
sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128
readings/window). The sensor acceleration signal, which has
gravitational and body motion components, was separated using a
Butterworth low-pass filter into body acceleration and gravity. The
gravitational force is assumed to have only low frequency components,
therefore a filter with 0.3 Hz cutoff frequency was used. From each
window, a vector of features was obtained by calculating variables from
the time and frequency domain.

#### The Script

The run\_analysis.R script downloads and unzip the dataset files and
merges the training and the test sets to create one data set. After
that, I’ve created a index to extract only the variables that contains
Mean and Standard Deviation (STD) data through the stringR pacakge. I
also recoded the activity collunm to be more descriptive of the data
collected and the variables names as well. Considering that in the final
tibble: each collumn represents a variable, each observation forms a row
and each type of observational unit forms a table, it can be considered
as tidy according to Wickham criteria<sup>2</sup>. From this last tibble
I’ve created a second one, independent tidy data set with the average of
each variable for each activity and each subject. The `tidy_data.txt` in
this repository was created by running the `run_analysis.R` script using
R version 4.0.0 on Windows 10 64-bit edition, and requires the following
packages:  
- `dplyr`  
- `stringR`  
- `readR`

##### Bibliography

1.  Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and
    Jorge L. Reyes-Ortiz. A Public Domain Dataset for Human Activity
    Recognition Using Smartphones. 21th European Symposium on Artificial
    Neural Networks, Computational Intelligence and Machine Learning,
    ESANN 2013. Bruges, Belgium 24-26 April 2013.

2.  Wickham, H. (2014). Tidy Data. Journal of Statistical Software,
    59(10), 1 - 23.
    <a href="doi:http://dx.doi.org/10.18637/jss.v059.i10" class="uri">doi:http://dx.doi.org/10.18637/jss.v059.i10</a>
