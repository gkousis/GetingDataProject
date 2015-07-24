## Getting and Cleaning Data Course Project

### Johns Hopkins University on Coursera 

*The purpose of the project is to merge the datasets provided and  create a tidy data set based on the data provided at:*

[UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones )

### The data used in this project is available at:
[Cloudfront.net (zip format)](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

## Instructions
* Download and unzip the zip file in a data folder in your working directory. **NOTE:** The unzip will create by default a subfolder *UCI HAR Dataset*
* Run the script *run_analysis.R*

## Codebook
- Detailed original codebook information is provided in the zip file.  Please see README.txt in the data folder
- Additional notes:
  * Variable names are created automatically using the features.txt file
  * Because the file includes parenthesis (), to improve readability of variables the empty parenthesis are removed
  * Other special characters are automatically adjusted by col.names
- The merged data drops several of the original variables.  Only mean and std variables are kept plus activity and subject id
- see Codebook.MD provided in this repo for more details.

## Output
The final tidy data set is extracted in text format using `write.table` with `row.name=FALSE`