simu.name = "test" # here put the name of the param set
repetitions = c(1,2,3,4,5,6,7)  # here put the names (numbers) of the repetitions

file_list = paste(simu.name, "_", repetitions, ".RData" , sep="")

# define how to load the result for each repetition
getResponse <- function(file){
	load(file)
	# calculate the result you are interested in :
	result = mean(landscape[,"thorn.density",dim(landscape)[3]])
	return(result)
}

# compute the result for all repetitions
result.list = lapply(file_list, getResponse)

# calculate the average result for the set of params
result.av = mean(unlist(result.list))
result.sd = sd(unlist(result.list))

# save the result
save(result.av, result.sd, file = paste(simu.name, "_result_average.RData", sep=""))

