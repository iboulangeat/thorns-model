simu.names = c("test1", "test2") # here put the names of the params sets

file_list = paste(simu.names, "_result_average.RData", sep="")

# load the result for each scenario
getResults <- function(file){
	load(file)
	df = data.frame(av = result.av, sd = result.sd)
	return(df)
}

# gather the results from all scenarios
result.list = lapply(file_list, getResults)
result.df = do.call(rbind, result.list)
result.df$scenarios = simu.names

# save the data frame
write.table(result.df[, c(3,1,2)], file = "result_scenarios.csv", quote = FALSE, sep=";", row.names = FALSE)
