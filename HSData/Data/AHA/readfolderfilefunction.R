# combine data part ----------------------------------------------------------------
# Function to combine desired sheets in all .xlsx or cvs files in one folder
combine_data_from_csvxlsx <- function(
  # This function is used to extract certain sheets in all excel files in one folder,
  # and combine them into one dataframe
  # Please make sure the sheet names in all xlsx are exactly same
  # Args:
  folderpath,      # path of the folder, example: "./Top100data"
  filetype,        # csv or xlsx
  namepattern,     # pattern of the name of .xlsx or .csv files, 
  # example: "Top_100_Contractors_Report_Fiscal_Year.*\\.xlsx"
  sheetname        # name of the sheet you want to extract from xlsx
  #
  # Returns:
  # a dataframe
){
  library(dplyr)
  library(tidyverse)
  ##Read xlsx files named in certain pattern
  filenames <- list.files(path = folderpath,
                          pattern = namepattern)
  
  ##Create list of data frame names without the ".xlsx" part
  if(filetype == "csv") {
    dfnames <-substr(filenames,1,nchar(filenames) - 4)
  } else {
    dfnames <-substr(filenames,1,nchar(filenames) - 5)
  }
  
  print("Ready to read files.")
  ##Create a list of all data frame
  alldata <- list()
  ##Load all files
  if(filetype == "csv") {
    for(i in dfnames){
      library(rlist)
      filepath <- file.path(folderpath,paste(i,".csv",sep=""))
      print(filepath)
      i<- read.csv(filepath)
      alldata <- list.append(alldata,i)
    }
  } else {
    for(i in dfnames){
      library(rlist)
      filepath <- file.path(folderpath,paste(i,".xlsx",sep=""))
      print(filepath)
      library(openxlsx)
      i<- read.xlsx(filepath, sheet = sheetname)
      alldata <- list.append(alldata,i)
    }
  }
  print("Complete reading all files.")
  ##Rename column names and assign segment column to data frame
  index = 1
  fulldata <- data.frame()
  for(df in alldata){
    library(tibble)
    # Rename colnames
    names(df) <- names(i)
    # Add a new column - Year
    df <- add_column(df, segment = dfnames[index])
    # Combine all tables
    fulldata <- bind_rows(fulldata,df)
    index = index + 1
  }
  
  ##Save data to csv
  #write_csv(fulldata, csvfilename)
  #print("Complete saving csv file.")
  return(fulldata)
}

mydata <- combine_data_from_csvxlsx(folderpath = "./data/InsideView_raw/0416level1withoutemail",
                                    filetype = "csv",
                                    namepattern = "level.*\\.csv")