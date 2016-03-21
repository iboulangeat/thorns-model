args <- commandArgs(trailingOnly = TRUE)

simu.name = as.character(args[1])

source(paste(simu.name, "_paramFile.r", sep=""))

# random seed
rnd.seed = as.numeric(args[2])
simu.name = paste(simu.name, rnd.seed, sep="_")

#========================
# build parameter table
#========================

# trees
#-----------

tree.params = data.frame(sp=1:nbspecies, woodiness=NA, init.thorn.density=NA)
if(!woodiness.distribution %in% c("even", "pos.skew", "neg.skew")) stop("wrong woodiness.distribution option")
require(fGarch)
if(woodiness.distribution == "even") {
	set.seed(rnd.seed)
	tree.params$woodiness = runif(nbspecies, min = mean.woodiness - 3*sd.woodiness, max = mean.woodiness + 3*sd.woodiness)
}
if(woodiness.distribution == "pos.skew") {
	set.seed(rnd.seed)
	tree.params$woodiness = rsnorm(nbspecies, mean.woodiness, sd.woodiness, skewness.factor.woodiness)
}
if(woodiness.distribution == "neg.skew") {
	set.seed(rnd.seed)
	tree.params$woodiness = rsnorm(nbspecies, mean.woodiness, sd.woodiness, -skewness.factor.woodiness)
}

set.seed(rnd.seed)
tree.params$init.thorn.density = 0
tree.params$init.thorn.density[sample(1:nbspecies, as.integer(init.thorny.tree.proportion*nbspecies))] = init.thorn.density

tree.params$av.growth = mean.growth + scale(1/tree.params$woodiness)
tree.params$sd.growth = rep(sd.growth, nbspecies)


print("===========tree parameters===========")
print(tree.params)

# herbivory
#----------------

# per unit of time (scenarios)
herbivore.biomass.removal = herbivory*mean.growth*nbindiv


#========================
# initialize landscape
#========================


init.random = function(ntrees, tree.params, init.size, sd){
	set.seed(rnd.seed)
	rnd = sample(1:nrow(tree.params), replace=TRUE)
	trees = tree.params$sp[rnd]
	set.seed(rnd.seed)
	size = rnorm(ntrees, init.size, sd)
	init.thorn = tree.params$init.thorn.density[rnd]
	return(data.frame(treesp= trees, size = size, thorn.density = init.thorn))
}


init.even = function(ntrees, tree.params, init.size, sd){
	set.seed(rnd.seed)
	nbindiv_by_sp = as.integer(nbindiv / nrow(tree.params))
	rnd = rep(1:nrow(tree.params), each = nbindiv_by_sp)
	trees = tree.params$sp[rnd]
	set.seed(rnd.seed)
	size = rnorm(ntrees, init.size, sd)
	init.thorn = tree.params$init.thorn.density[rnd]
	return(data.frame(treesp= trees, size = size, thorn.density = init.thorn))
}

if(!ind_by_species %in% c("random", "even")) stop("wrong ind_by_species option")

if(ind_by_species=="random") init.forest = init.random(nbindiv, tree.params, mean.init.tree.size, sd.init.tree.size)

if(ind_by_species=="even") init.forest = init.even(nbindiv, tree.params, mean.init.tree.size, sd.init.tree.size)


init.forest$growth = rep(0, nbindiv)

print("======= initial landscape ========")
print(init.forest)


#============================================================================
# FUNCTIONS
#============================================================================


# growth function 
#----------------------------------------------------------

growth = function(tree.name, treepars, rnseed){
	tree.sd = treepars$sd.growth[treepars$sp==tree.name] 
	tree.av = treepars$av.growth[treepars$sp==tree.name]
	set.seed(rnseed)
	gr = ifelse(tree.sd==0, tree.av, rnorm(1, tree.av, tree.sd))
	return(gr)
	}


# thorn density affected by tree growth
#-----------------------------------------

calc.thorn = function(tree.thorn, tree.growth){
	thorn.density = tree.thorn / tree.growth
	return(thorn.density)
}

vthorn = Vectorize(calc.thorn)

#========================
# simulation model 
#========================


# initialize landscape object : tree (name), size, thorn
# ---------------------------------------------------------
landscape = array(NA, dim = c(nrow(init.forest), ncol(init.forest), simulation.time+1), dimnames=list(c(1:nrow(init.forest)), colnames(init.forest), 0:(simulation.time)) )
landscape[,,1] = as.matrix(init.forest)
landscape[,1,-1] = landscape[,1,1]

# START LOOP #=======================================================================

for (i in 1:simulation.time)
{

j=i+1 # corresponding index in the array
# i = current time step
# j = index of the array corresponding to current time step

# tree growth
# ---------------------------------------------

for(indiv in 1:nbindiv){
	landscape[indiv,"growth",j]  = growth(landscape[indiv,"treesp",j-1], treepars = tree.params, rnseed = rnd.seed+i+indiv)
}
	
forest.growth = landscape[,"growth",j]
landscape[,"size",j] = landscape[,"size",j-1] + forest.growth

# modification of thorns density
# ---------------------------------------------

landscape[,"thorn.density",j] = vthorn(thorn.amount,  tree.growth = forest.growth)

# rank trees by preference / priority of consumption (softness * thorn density) + weigthing (herbivore type)
# ------------------------------------------------------------------------------

#initial ranking

woodiness = tree.params$woodiness[unlist(lapply(landscape[,"treesp",j], function(x){which(tree.params$sp==x)}))]
tdens = landscape[,"thorn.density",j]
tdens.index = rep(j, dim(landscape)[1])
forest.growth.index = rep(j, dim(landscape)[1])

# herbivory (biomass removal distribution)
# ------------------------------------------

toRemove = herbivore.biomass.removal
biomass.removal = rep(0, dim(landscape)[1])

# first : the better
# then : the old growth is availble from this tree but with a softness *.5 / woodiness *2

while(toRemove>0)
{
	if(sum(woodiness==1000)==nbindiv) {
		print("not enough ressources")
		break;
	}
	best = preference = order(woodiness, tdens)[1]

	selectedBiomass = landscape[best,"growth",forest.growth.index[best]]
	# check if there is enough biomass
	if(selectedBiomass > (landscape[best,"size",j] - biomass.removal[best])){
		selectedBiomass = landscape[best,"size",j] - biomass.removal[best]
		woodiness[best]=10000 #won't be grazed anymore at this time step
	}

	if(selectedBiomass<toRemove){
		# update biomass removal to the selected tree
		biomass.removal[best] = biomass.removal[best] + selectedBiomass
		# update herbivore needs
		toRemove = toRemove - selectedBiomass
		# apply coefficient to woodiness of the last time step for the browsed tree
		woodiness[best] = 2*woodiness[best]
		# update indices for new available part of this tree
		tdens.index[best] = tdens.index[best] - 1
		forest.growth.index[best] = forest.growth.index[best] -1
		# update indices for the new available part of this tree
		if(landscape[best,"growth",forest.growth.index[best]]==0) woodiness[best]=10000 #won't be grazed anymore at this time step
		if(tdens.index[best]>=0) {
			tdens[best] = landscape[best,"thorn.density",tdens.index[best]]
		}else{
		# if the tree went back to its size at the begining of the simulation, it won't be grazed anymore at this time step
			woodiness[best]=10000
		}
	}else{
		# if the biomass to remove is smaller that the new part of the selected tree
		biomass.removal[best] = biomass.removal[best] + toRemove
		toRemove = 0 
		# we are done in this case
		break;
	}

}

# bilan of new tree state (biomass or height)
#-----------------------------------------

landscape[,"size",j] = landscape[,"size",j] - biomass.removal

# update thorn density
# --------------------

landscape[,"thorn.density",j] = tdens


# END LOOP #============================================================================
}

#========================
# output of the simulation
#========================

print("=========final landscape===============")
print(landscape[,,simulation.time])


#========================
# save
#========================

save(tree.params, init.forest, landscape, file = paste(simu.name, ".RData", sep=""))
write.table(tree.params, file = paste(simu.name, "_treeParams.txt", sep=""), sep="\t", quote=FALSE)
write.table(init.forest, file = paste(simu.name, "_initalLandscape.txt", sep=""), sep="\t", quote=FALSE)
write.table(landscape[,,simulation.time], file = paste(simu.name, "_finalLandscape.txt", sep=""), sep="\t", quote=FALSE)

