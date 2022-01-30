library(stringr)

allFiles = list.files(path = ".", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
errorDf <- data.frame(matrix(ncol = 5, nrow = 0))
names(errorDf)<-c("Name", "First_Smaller", "F_NN", "Second_Smaller", "S_NN")

for(i in 1:length(allFiles)) {
    df <- read.csv(str_interp("${allFiles[i]}"),
               header = TRUE,
               sep = ",")
    print(str_interp("laço: ${i} / ${allFiles[i]}"))
    smallerIndex = which.min(df$mean)
    secondSmallerIndex = which.min(df$mean[df$mean!=min(df$mean)])

    errorDf[nrow(errorDf) + 1,] = c(allFiles[i],
                                            round(df$mean[smallerIndex], 2),
                                            df["Hidden.neurons"][smallerIndex, 1],
                                            round(df$mean[secondSmallerIndex], 2),
                                            df["Hidden.neurons"][secondSmallerIndex, 1])
    write.csv(errorDf, str_interp("../smallerErro.csv"))
}
