
#========================
# simulation parameters 
#========================

# nb species: e.g 10-50
nbspecies = 10

# nb individuals 
nbindiv = 50 
# random selection versus even selection of species
ind_by_species = "random" # choose betwee "random" or "even"
 
# woodiness: 
# N.B. it will influence growth (growth = mean.growth + scale(1/woodiness)
#- even distribution across species (from mean-3*sd to mean+3*sd)
#- skewed distribution towards both sides
woodiness.distribution = "even" # choose between "even" and "pos.skew" and neg.skew"
mean.woodiness = 1 
sd.woodiness = 0.3
skewness.factor.woodiness = 2

# growth rate (biomass per time step)
mean.growth = 10
sd.growth = 0 

# thorns number (number per time step)
thorn.amount = 20 # same for all trees (density will vary according to the growth)

# init thorn density
init.thorny.tree.proportion = 0.5
init.thorn.density = thorn.amount / mean.growth


# herbivory:
# proportion of [mean growth * nb indiv.] that will be removed each time step)
herbivory = 0.5

# inital tree sizes
#- skewed small trees
#- normal distribution
init.tree.size.distribution = "normal" # choose between "normal" and "skew"
mean.init.tree.size = 100
sd.init.tree.size =  20
skewness.factor.tree.size = 2

# simulation
simulation.time = 100 # time steps 

# random seed
rnd.seed = 123

