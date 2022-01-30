library(stringr)

allFiles = list.files(path = ".", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

for(i in 1:length(allFiles)) {
    df <- read.csv(str_interp("${allFiles[i]}"),
               header = TRUE,
               sep = ",")

    rightMse = round(df$RMSE ^ 2, 2)

    df["RightMSE"] = rightMse

    mean = (df$RightMSE + df$MAE + df$RAE + df$RMSE)/4

    df["mean"] = mean
    
    write.csv(df, str_interp("${allFiles[i]}"))
}