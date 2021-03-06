Getting-and-cleaning-data 
=========================

*Course Project for data science getting and cleaning data below text explains pre-requitsites for running [run_analysis.R](https://github.com/girishgupta/Getting-and-cleaning-data/blob/master/run_analysis.R) file.*

#Points-to-note#
- The script has been written for windows operating system.
- We need following packages installed and loaded for script to run **plyr**, **reshape2**, **stringr** and **Rcurl**.
- People with Mac may be required to download put zip file in working directory.
- Script will download the zip file [getdata-projectfiles-UCI HAR Dataset](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip) to your current working directory if it is already not present there.
- Script will start with unzipping the zip file even though the unzipped folder may be present to ensure the correct folder structure and untouched data.
- Working directory will be changed to UCI HAR Dataset folder.

#Steps involved in data manipulation
1. Zip file is downloaded and unzipped.
2. Working directory changed to unzipped folder **UCI HAR Dataset**.
3. Read in activity and feature files into **activityLabels** and **featureLabel**.
4. Acitivity and feature dataset given proper column names to ensure a good join later.
5. Read in test and training data such that feature values is in **X** and activity code in **Y** dataset.
6. Read in subject identifiers file for test and training data.
7. Combine test data for feature value **X** with activity code **Y** and subject identifier column-wise using ```cbind```.
8. Repeat *step 7* for training data.
9. Finally combine test data from *step 7* and training data from *step 8* together in **completeData** using  ```rbind```.
10. **completeData** still doesnot have activity description so we perform left join between **completeData** and **activityLabels** to get activity description. 
11. Join is made easier by naming the columns as in *step 4*.
12. Now we start with adjusting the feature labels to give descriptive names.
13. The adjustment is mainly focussed on removing special characters and assining proper casing of names.
  1. Changing angle (x,y) to angle_between_x_and_y.
  2. Changing abc-mean to abcMean.
  3. Changing Mean()-1,2 to MeanFor1,2.
  4. Changing For1,16 to For1To16.
  5. Finally removing all other special charachters and replacing with nothing.
14. Now **featureLabel** dataset contains adjusted feature names, we also add additional names to **featureLabel** dataset which are present in **completeData** but not in **featureLable** like *activityCode*, *subject*, and *activityDes*.
15. Assign the adjusted feature lables to column names **completeData**.
16. Remove columns and variables not required after this step.
17. Extract **tidydataset1:**
   1. Extract all the columns whose name contain mean and std.
   2. Here we are working with the assumption that any name containing mean and std is a required column. There are few columns with meanFreq in column header and we will including those as well in our **tidaydataset1**.
   3. Attach column corresponding to **activityDes** and **subject** to mean & std columns.
   4. Extract the **tidydataset1** using subsetting which gives us one the output required in assignment.
18. Extract **tidydataset2:**
  1. Use ```aggregate``` function to calculate mean of features based on **subject** and **activityDes**.
  2. Remove additional columns generated by ```aggregate```.
  3. Final **tidydataset2** contains 180 ```30 subjects * six activities``` rows. 2 addtional columns for subject and activity description along with mean values.
19. Export **tidydataset2** to a text file using ```write.table```.

#####For any clarification please contact [girishgupta@outlook.com](girishgupta@outlook.com)#####


