

#========================
# parameters 
#========================

# trees
#-----------

# species name
# type = soft/hard (a value to be able to rank softness)
# thorn density will give thorn number per unit of time according to growth average

# tree.params = read.table...

tree.params = data.frame(sp=NA, woodiness=NA, thorn.density=NA)
tree.params$sp = 
tree.params$woodiness = c()
tree.params$thorn.density = c()


# growth functions / climate
#---------------------------

# biomass or height = average + variability

# growth.params = read.table...

growth.params = data.frame(sp=NA, clim=NA, av.growth=NA, sd.growth=NA)
growth.params$sp = 
growth.params$clim = 
growth.params$av.growth =
growth.params$sd.growth =


# herbivore
#----------------

# per unit of time (scenarios)
herbivore.biomass.removal = 


# simulation
#--------------------

rnd.seed = 123
simulation.time = 100 # time steps # = nrow climate if climate is not constant
climate = "" # name of the climate from growth parameters

# initialisation:

init = "random"
init.tree.size = # size average
init.tree.sd =  # size sd for random initialization
ntrees = 

init="zero.size"

#========================
# initialize landscape
#========================

#landscape has individuals with 4 attributes (at each time step) : name, size, thorn.density, growth of the time step


init.random = function(ntrees, tree.params, init.size, sd=1){
	set.seed(rnd.seed)
	rnd = sample(1:nrow(tree.params), replace=TRUE)
	trees = tree.params$sp[rnd]
	set.seed(rnd.seed)
	size = rnorm(ntrees, init.size, sd)
	init.thorn = tree.params$thorn.density[rnd]
	return(data.frame(tree= trees, size = size, thorn.density = init.thorn))
}

init.zero= function(ntrees, tree.params, init.size, sd=1){
	set.seed(rnd.seed)
	rnd = sample(1:nrow(tree.params), replace=TRUE)
	trees = tree.params$sp[rnd]
	init.thorn = tree.params$thorn.density[rnd]
	return(data.frame(tree= trees, size = rep(0,ntrees), thorn.density = init.thorn),  = rep(0,ntrees))
}


if(init=="random") init.forest = init.random(ntrees, tree.params, init.tree.size, init.tree.sd)

if(init=="zero") init.forest = init.zero(ntrees, tree.params)

init.forest$growth = rep(0, ntrees)

# climate sequence
#-------------------

set.seed(rnd.seed)
climate.sequence = rnorm(simulation.time)

# derivate thorn amount per unit of time depending on the choosen climate
#---------------------------------------------------------------------------

for (s in unique(tree.params$sp)){
	tree.params$thorn[tree.params$sp==s] = growth.params$av.growth[growth.params$sp==s & growth.params$clim == climate] * tree.params$thorn.density[tree.params$sp==s] 
}


# preference option
#-------------------

# preference = "determinist"
# # preference = "stochastic"


#========================
# simulation model 
#========================


# growth function : translate climate into specific growth
#----------------------------------------------------------

growth = function(tree.name, env, growth.params){
	tree.growth = env * growth.params$sd.growth[growth.params$sp=tree.name] 
	 + growth.params$av.growth[growth.params$sp=tree.name] 
	}

vgrowth = Vectorize(growth, tree.name)


# env will be a number from random sequence (mean 0, sd = 1)


# thorn density affected by tree growth
#-----------------------------------------

calc.thorn = function(tree.thorn, tree.growth){
	thorn.density = tree.thorn / tree.growth
	return(thorn.density)
}

vthorn = Vectorize(calc.thorn)


# initialize landscape object : tree (name), size, thorn
# ---------------------------------------------------------
landscape = array(NA, dim = c(nrow(init.forest), ncol(init.forest), simulation.time+1), dimnames=list(c(1:nrow(init.forest)), colnames(init.forest), 0:(simulation.time+1) )
landscape[,,1] = init.forest

# START LOOP #=======================================================================

for (i in 1:simulation.time)
{

j=i+1 # corresponding index in the array
# i = current time step
# j = index of the array corresponding to current time step

# tree growth
# ---------------------------------------------

forest.growth = unlist(lapply(landscape[,"tree",j-1], growth, env=climate.sequence[i], growth.params = growth.params))

landscape[,"size",j] = landscape[,"size",j-1] + forest.growth
landscape[,"growth",j] = forest.growth

# modification of thorns density
# ---------------------------------------------
thorn = tree.params$thorn[unlist(lapply(landscape[,"tree",j-1], grep, x = tree.params$sp))]

landscape[,"thorn.density",j] = vthorns(thorn,  tree.growth = forest.growth)

# rank trees by preference / priority of consumption (softness * thorn density) + weigthing (herbivore type)
# ------------------------------------------------------------------------------

#initial ranking

woodiness = tree.params$woodiness[unlist(lapply(landscape[,"tree",j], grep, x = tree.params$sp))]
tdens = landscape[,"thorn.density",j]
tdens.index = rep(j, dim(landscape)[1])

# herbivory (biomass removal distribution)
# ------------------------------------------

toRemove = herbivore.biomass.removal
biomass.removal = rep(0, dim(landscape)[1])

# first : the better
# then : the old growth is availble from this tree but with a softness *.5 / woodiness *2

while(toRemove>0)
{
	best = preference = order(woodiness, tdens)[1]
	if(forest.growth[best]<toRemove){
		# update biomass removal to the selected tree
		biomass.removal[best] = biomass.removal[best] + forest.growth[best]
		# update herbivore needs
		toRemove = toRemove - forest.growth[best]
		# apply coefficient to woodiness of the last time step for the browsed tree
		woodiness[best] = 2*woodiness[best]
		# update index for new available part of this tree
		tdens.index[best] = tdens.index[best] - 1
		# update thorn density for the new available part of this tree
		if(tdens.index[best]>=0) {
			tdens[best] = landscape[best,"thorn.density",tdens.index[best]]
		}else{
		# if the tree went baxk to its size at the begening of the simulation, it won't be grazed anymore at this time step
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
tdens.index[tdens.index<0]=0

landscape[,"thorn.density",j] = tdens


# END LOOP #============================================================================
}

#========================
# output of the simulation
#========================

landscape


