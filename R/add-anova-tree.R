library("rpart.plot")

anovaTree <- function(outputExpected, df) {
    outputExpected = gsub("-", ".", outputExpected, fixed = TRUE)
    # Keep only the RES column of outputExpected
    outputResult = df[, outputExpected]
    df <- df[, -grep("RES", colnames(df))]
    df[outputExpected] <- outputResult
    # Names of the attributes from csv
    dfNames <- names(df)
    # NA coluns receive 0
    df[is.na(df)] = 0
    dataset <- df
    formula = str_c(dfNames[!dfNames %in% outputExpected],
                    collapse = "+")
    foramTree <- rpart(str_c(outputExpected, " ~ ", formula), data = df, method='anova')
    #rpart.plot(foramTree, type = 3, digits = 2)
    return(foramTree)
}