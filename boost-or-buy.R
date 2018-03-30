# boost-or-buy-v1.0.R
# Quick-reference cards for statistical decisions in the Warmachine/Hordes tabletop game
# Script for use with R Environment for Statistical Computing v3.3.1
# by NephMakes, 2018
# 
# Creative Commons Attribution 4.0 International License 
# CC-BY-4.0, http://creativecommons.org/licenses/by/4.0/

# Load packages
library(distr)      # v2.6.2, for operations on probability distributions
library(ggplot2)    # v2.2.1, for nice plots
library(gridExtra)  # v2.3, for multiple plots in one window

# Define function to expand data frames
# 	Thanks to "Y T" on Stack Overflow for this function
# 	Arguments must be data frames or lists for proper column names
expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))

# Define dice-roll probability distributions
oneD6   <- DiscreteDistribution(supp = 1:6)
twoD6   <- oneD6 + oneD6
threeD6 <- twoD6 + oneD6
fourD6  <- twoD6 + twoD6
# 	d(distribution)(values) is probability density of values
# 	p(distribution)(values) is cumulative probability of values


# ============
# ATTACK ROLLS
# ============

# Calculate probability to hit vs roll needed for 2d6, 3d6, and 2x 2d6 attempts
roll.scope <- 2:18
data.hit <- data.frame(
	# Wide format for clarity
	roll.needed = roll.scope, 
	# Probability at least one hit
	prob.hit.2d6 = 1 - p(twoD6)(roll.scope - 1),
	prob.hit.3d6 = 1 - p(threeD6)(roll.scope - 1), 
	prob.hit.buy = 1 - (p(twoD6)(roll.scope - 1))^2
)
data.hit <- within(data.hit, {
	# All-ones always misses
	prob.hit.2d6[roll.needed < 3] <- prob.hit.2d6[roll.needed == 3]
	prob.hit.3d6[roll.needed < 4] <- prob.hit.3d6[roll.needed == 4]
	prob.hit.buy[roll.needed < 3] <- prob.hit.buy[roll.needed == 3]
	# 
	# All-sixes always hits
	prob.hit.2d6[roll.needed > 12] <- prob.hit.2d6[roll.needed == 12]
	prob.hit.3d6[roll.needed > 18] <- prob.hit.3d6[roll.needed == 18]
	prob.hit.buy[roll.needed > 12] <- prob.hit.buy[roll.needed == 12]
	# 
	# Mean number of hits
	mean.hits.2d6 <- prob.hit.2d6
	mean.hits.3d6 <- prob.hit.3d6
	mean.hits.buy <- 2 * prob.hit.2d6
})
data.hit <- reshape(data.hit, 
	# Long format for plots
	direction = "long", 
	idvar = "roll.needed", 
	varying = list(
		c("prob.hit.2d6", "prob.hit.3d6", "prob.hit.buy"), 
		c("mean.hits.2d6", "mean.hits.3d6", "mean.hits.buy")
	), 
	v.names = c("prob.hit", "mean.hits"), 
	timevar = "roll.type", times = c("2d6", "3d6", "2x 2d6")
)
# Save results as function for later: 
ProbToHit <- function(roll.needed, roll.type) {
	data.hit$prob.hit[(data.hit$roll.needed == roll.needed) & (data.hit$roll.type == roll.type)]
}

# Define shared plot elements
plot.hit.base <- ggplot(data.hit) + 
	aes(x = roll.needed, color = roll.type, fill = roll.type) + 
	scale_x_continuous(
		name = "Roll needed to hit", limits = c(2, 18), breaks = seq(2, 18, by = 4)
	) +
	geom_point(shape = 21) + 
	scale_color_grey(start = 0.2, end = 0.5) + 
	scale_fill_grey(start = 0.2, end = 1) + 
	theme(
		legend.title = element_blank(), 
		legend.justification = c(1, 1), 
		legend.position = c(0.96, 0.96), 
		plot.margin = unit(rep(0.2, 4), "inches"), 
		plot.background = element_rect(
			fill = "white", color = grey(0.92), size = 1, linetype = "solid"
		)
	)

# Plot mean number of hits
plot.mean.hits <- plot.hit.base + aes(y = mean.hits) + 
	scale_y_continuous(
		name = "Mean number of hits", 
		limits = c(0, 2), breaks = seq(0, 2, by = 0.5)
	)
# dev.new(width = 3.5, height = 2.5)
# plot.mean.hits 
# 	Boosting to hit and buying another attack give you the same number of hits 
# 	on average when you need to roll an eight. Above eight, boosting will get you 
# 	more hits. Below eight, buying will get you more. 

# Plot probability at least one hit
plot.prob.hit <- plot.hit.base + aes(y = prob.hit) + 
	scale_y_continuous(
		name = "Probability at least one hit", 
		limits = c(0, 1), breaks = seq(0, 1, by = 0.2)
	)
# dev.new(width = 3.5, height = 2.5)
# plot.prob.hit
# 	When you need a specific attack to hit, you want to boost it -- maybe even 
# 	when you only need to roll a five. When you need to land at least one hit 
# 	to trigger some effect, boosting the hit roll is substantially better than 
# 	buying attacks when you need to roll an eight or more.  

# 	Possible shorthand: "Eight on the die, boost over buy" 


# ============
# DAMAGE ROLLS
# ============

# Calculate mean damage for different boost/buy options 
# vs damage roll modifier and roll needed to hit again
roll.mod.scope <- -8:8
data.dmg.mean <- expand.grid(dmg.roll.mod = roll.mod.scope, hit.roll.needed = 2:12)
data.dmg.mean <- within(data.dmg.mean, {
	# 
	# Single focus to spend
	# 
	mean.dmg.2d6 <- sapply(dmg.roll.mod, function(roll.mod) {
		weighted.mean(pmax(2:12 + roll.mod, 0), d(twoD6)(2:12))
	})
	mean.dmg.3d6 <- sapply(dmg.roll.mod, function(roll.mod) {
		weighted.mean(pmax(3:18 + roll.mod, 0), d(threeD6)(3:18))
	})
	mean.dmg.buy <- mapply(
		function(roll.mod, roll.needed) {
			weighted.mean(pmax(2:12 + roll.mod, 0), d(twoD6)(2:12)) + 
			ProbToHit(roll.needed, "2d6") * 
			weighted.mean(pmax(2:12 + roll.mod, 0), d(twoD6)(2:12))
		}, dmg.roll.mod, hit.roll.needed
	)
	best.single <- NA
	best.single[mean.dmg.3d6 > mean.dmg.buy] <- "3d6"
	best.single[mean.dmg.3d6 < mean.dmg.buy] <- "2x 2d6"
	# 
	# Two or more focus to spend	
	# 
	mean.dmg.DB <- mapply( 
		function(roll.mod, roll.needed) {
			 # DB = Boost damage then buy another
			weighted.mean(pmax(3:18 + roll.mod, 0), d(threeD6)(3:18)) + 
			ProbToHit(roll.needed, "2d6") * 
			weighted.mean(pmax(2:12 + roll.mod, 0), d(twoD6)(2:12))
		}, dmg.roll.mod, hit.roll.needed
	)
	mean.dmg.BH <- mapply(  
		function(roll.mod, roll.needed) {
			# BH = Buy another then boost to hit
			weighted.mean(pmax(2:12 + roll.mod, 0), d(twoD6)(2:12)) + 
			ProbToHit(roll.needed, "3d6") * 
			weighted.mean(pmax(2:12 + roll.mod, 0), d(twoD6)(2:12))
		}, dmg.roll.mod, hit.roll.needed
	)
	mean.dmg.BB <- mapply(  
		function(roll.mod, roll.needed) {
			# BB = Buy 2x more
			weighted.mean(pmax(2:12 + roll.mod, 0), d(twoD6)(2:12)) + 
			2 * ProbToHit(roll.needed, "2d6") * 
			weighted.mean(pmax(2:12 + roll.mod, 0), d(twoD6)(2:12))
		}, dmg.roll.mod, hit.roll.needed
	)
	# Buy-Dmg always between Dmg-Buy and Buy-Buy b/c misses
	best.double <- NA
	best.double[(mean.dmg.DB > mean.dmg.BH) & (mean.dmg.DB > mean.dmg.BB)] <- "DB"
	best.double[(mean.dmg.BH > mean.dmg.DB) & (mean.dmg.BH > mean.dmg.BB)] <- "BH"
	best.double[(mean.dmg.BB > mean.dmg.DB) & (mean.dmg.BB > mean.dmg.BH)] <- "BB"
})
data.dmg.mean$best.single <- factor(data.dmg.mean$best.single, levels = c("2x 2d6", "3d6"))
data.dmg.mean$best.double <- factor(data.dmg.mean$best.double, levels = c("BB", "BH", "DB"))
# subset(data.dmg.mean, is.na(best.single)); subset(data.dmg.mean, is.na(best.double)) 
# 	Boosting and buying equivalent at dice -1, 7 to hit

# Plot option with highest mean damage for given dmg and hit roll
plot.dmg.choice.base <- ggplot(data = data.dmg.mean) + 
	aes(x = dmg.roll.mod, y = hit.roll.needed) + 
	geom_point(shape = 21, size = 2.5) + 
	scale_color_grey(start = 0.4, end = 0.5) + 
	scale_fill_grey(start = 0.5, end = 1) + 
	scale_x_continuous(
		name = "Damage roll modifier", 
		limits = c(-8, 8), breaks = seq(-8, 8, by = 2)
	) + 
	scale_y_continuous(
		name = "Roll needed to hit again", 
		limits = c(2, 12), breaks = seq(2, 12, by = 2)
	) + 
	theme(
		legend.position = "none", 
		plot.margin = unit(rep(0.2, 4), "inches"), 
		plot.background = element_rect(
			fill = "white", color = grey(0.92), size = 1, linetype = "solid"
		)
	)
plot.dmg.choice.single <- plot.dmg.choice.base %+% 
	subset(data.dmg.mean, !is.na(best.single)) + 
	aes(color = best.single, fill = best.single) + 
	annotate(
		geom = "label", label = "Boost damage", x = -4, y = 9, 
		size = 3.5, color = grey(0.3), fill = grey(0.92), label.size = NA
	) + 
	annotate(
		geom = "label", label = "Buy attack", x = 4, y = 5, 
		size = 3.5, color = grey(0.3), fill = grey(0.92), label.size = NA
	) + 
	geom_text(label = "=", x = -1, y = 7, color = grey(0.5))
plot.dmg.choice.double <- plot.dmg.choice.base %+% 
	subset(data.dmg.mean, !is.na(best.double)) + 
	aes(color = best.double, fill = best.double) + 
	annotate(
		geom = "label", label = "Boost damage\nthen buy attack", x = -4.5, y = 9.5, 
		size = 3, color = grey(0.3), fill = grey(0.92), label.size = NA
	) + 
	annotate(
		geom = "label", label = "Buy unboosted attacks", x = 3, y = 4, 
		size = 3, color = grey(0.3), fill = grey(0.92), label.size = NA
	) + 
	annotate(
		geom = "label", label = "Buy attack\nthen boost hit", x = 5, y = 9.5, 
		size = 3, color = grey(0.3), fill = grey(0.92), label.size = NA
	) + 
	geom_text(label = "=", x = -1, y = 7, color = grey(0.5))
# dev.new(width = 3.5, height = 5)
# grid.arrange(plot.dmg.choice.single, plot.dmg.choice.double, ncol = 1)
# 	Best to buy attacks when easier to hit and favorable damage roll modifier. 
# 	With 2+ focus, it's better to buy a boosted attack when hit probability is low 
# 	but favorable dmg roll modifier. 


# ==================================
# MAKE PDF FOR QUICK-REFERENCE CARDS
# ==================================

pdf(file = "boost-or-buy-output.pdf", width = 7, height = 5)
grid.arrange(
	plot.prob.hit, plot.mean.hits, 
	plot.dmg.choice.single, plot.dmg.choice.double, 
	ncol = 2
)
dev.off()

