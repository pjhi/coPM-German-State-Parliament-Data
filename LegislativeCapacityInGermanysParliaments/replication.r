## appeldorn and fortunato lsq replication ##
## this script reads in the raw data used in the manuscript ##
## (plus values for most variables back to reunificiation) ##
## and creates the tables and figures in the article ##
## clean up, load libraries, set seed, set directory, and load data ##
	rm(list = ls())
	library(stargazer)
	library(matrixStats)
	library(MCMCpack)
	
	set.seed(1)
	
	setwd('')

	data <- read.table('replication.txt', sep = '|', header = TRUE)
	summary(data)

## first, we'll specify a squire-type scale ##
	data$squire <- 1/3 * (data$salary / data$fp_mp_salary_2015) + 
		1/3 * (data$staff / data$fp_staffmp_2015) + 
		1/6 * (data$commDays / data$fp_com_sessions) + 
		1/6 * (data$plenDays / data$fp_plenary_sessions)

## and decompose the data with a scaling model ##
## save the full frame ##
	data1 <- data

## break out the article's description period ##
	data <- data[data$year > 1999 & data$year < 2020, ]	

## estiamate the model, constraining all factors to be positive ##
	model <- MCMCmixfactanal(~ salary + staff + commDays + plenDays, data = data, factors = 1,
		lambda.constraints = list(salary = list(2, '+'), staff = list(2, '+'), commDays = list(2, '+'), plenDays = list(2, '+')),
			burnin = 1000, mcmc = 100000, thin = 100, seed = 1, verbose = 10000, 
			store.scores = TRUE, tune = 0.8)
	summary(model)
		
## this is table 1 ##
	loadMean <- summary(model)$statistics[1:4, 1]
	loadSd <- summary(model)$statistics[1:4, 2]
	round(loadMean, 3)
	round(loadSd, 3)
	varMean <- summary(model)$statistics[345:348, 1]
	varSd <- summary(model)$statistics[345:348, 2]
	round(varMean, 3)
	round(varSd, 3)

## now we can splice the results into the main data ##
	data$factMean <- summary(model)$statistics[5:344, 1]
	data$factSd <- summary(model)$statistics[5:344, 2]	
	data$factLo <- summary(model)$quantiles[5:344, 1]
	data$factHi <- summary(model)$quantiles[5:344, 5]	

## let's assess the stability of recovered rank-orderings ##
## with pearson and spearman correlations ##
	corS <- c()
	pS <- c()
	corP <- c()
	pP <- c()
	for(i in 1:10000){
		x <- rank(runif(1000))[1:2]
		corP[i]	<- cor(model[x[1], 5:344], model[x[2], 5:344], method = 'pearson')
		pP[i] <- cor.test(model[x[1], 5:344], model[x[2], 5:344], method = 'pearson')$p.value
		corS[i]	<- cor(model[x[1], 5:344], model[x[2], 5:344], method = 'spearman')
		pS[i] <- cor.test(model[x[1], 5:344], model[x[2], 5:344], method = 'spearman')$p.value
	
	}
	summary(corP)
	summary(pP)
	summary(corS)
	summary(pS)

## plot all of the components ##
## this is figure 1 ##
	pdf('fig1.pdf', height = 10, width = 10)
	par(mfrow = c(2, 2))
	plot(data$year, data$salary, type = 'n',
		xlab = 'Year',
		ylab = 'Thousands of Euros',
		ylim = c(0, 10),
		xlim = c(2000, 2020),
		main = 'MP Monthly Compensation',
		axes = FALSE)
	axis(1)
	axis(2)
	lander <- unique(data$land)
	for(i in lander){
		if(i == 'Bundesrepublik'){
			lines(data$year[data$land == i], data$salary[data$land == i]/1000, col = 'black', lwd = 2)
		}else{
			lines(data$year[data$land == i], data$salary[data$land == i]/1000, col = rgb(0, 0, 0, 0.2), lwd = 2)
		}
	}
	legend('topleft', legend = c('Bundestag', 'Landtage'), col = c('black', rgb(0, 0, 0, 0.2)),
		lwd = 3, bty = 'n')
	
	plot(data$year, data$staff, type = 'n',
		xlab = 'Year',
		xlim = c(2000, 2020),
		ylim = c(0, 800),
		ylab = 'Thousands of Euros',
		main = 'Annual Spending on MP Staff Support',
		axes = FALSE)
	axis(1)
	axis(2)
	lander <- unique(data$land)
	for(i in lander){
		if(i == 'Bundesrepublik'){
			lines(data$year[data$land == i], data$staff[data$land == i]/1000, col = 'black', lwd = 2)
		}else{
			lines(data$year[data$land == i], data$staff[data$land == i]/1000, col = rgb(0, 0, 0, 0.2), lwd = 2)
		}
	}
	legend('topleft', legend = c('Bundestag', 'Landtage'), col = c('black', rgb(0, 0, 0, 0.2)),
		lwd = 3, bty = 'n')

	plot(data$year, data$commDays, type = 'n',
		xlab = 'Year',
		ylab = 'Total Committee Meetings',
		ylim = c(0, 850),
		xlim = c(2000, 2020),
		main = 'Annual Committee Meetings',
		axes = FALSE)
	axis(1)
	axis(2)
	lander <- unique(data$land)
	for(i in lander){
		if(i == 'Bundesrepublik'){
			lines(data$year[data$land == i], data$commDays[data$land == i], col = 'black', lwd = 2)
		}else{
			lines(data$year[data$land == i], data$commDays[data$land == i], col = rgb(0, 0, 0, 0.2), lwd = 2)
		}
	}
	legend('topleft', legend = c('Bundestag', 'Landtage'), col = c('black', rgb(0, 0, 0, 0.2)),
		lwd = 3, bty = 'n')
		
	plot(data$year, data$plenDays, type = 'n',
		xlab = 'Year',
		ylab = 'Days in Plenary',
		ylim = c(0, 80),
		xlim = c(2000, 2020),
		main = 'Annual Plenary Session Days',
		axes = FALSE)
	axis(1)
	axis(2)
	lander <- unique(data$land)
	for(i in lander){
		if(i == 'Bundesrepublik'){
			lines(data$year[data$land == i], data$plenDays[data$land == i], col = 'black', lwd = 2)
		}else{
			lines(data$year[data$land == i], data$plenDays[data$land == i], col = rgb(0, 0, 0, 0.2), lwd = 2)
		}
	}
	legend('topleft', legend = c('Bundestag', 'Landtage'), col = c('black', rgb(0, 0, 0, 0.2)),
		lwd = 3, bty = 'n')
	dev.off()

## and now plot the summaries ##
## this is figure 3 ##
	hold <- data[data$land != 'Bundesrepublik', ]
	lander <- unique(data$land)
	pdf('fig3.pdf', width = 10, height = 10)
	par(mfrow = c(2, 2))
	plot(data$year, data$squire, type = 'n',
		xlab = 'Year',
		ylab = 'Squire-modeled Index',
		xlim = c(2000, 2020),
		main = 'Squire-modeled Index over Time',
		axes = FALSE)
	axis(1)
	axis(2)
	for(i in lander){
		if(i == 'Bundesrepublik'){
			lines(data$year[data$land == i], data$squire[data$land == i], col = 'black', lwd = 2)
		}else{
			lines(data$year[data$land == i], data$squire[data$land == i], col = rgb(0, 0, 0, 0.2), lwd = 2)
		}
	}
	legend(2000, 1, legend = c('Bundestag', 'Landtage'), col = c('black', rgb(0, 0, 0, 0.2)),
		lwd = 3, bty = 'n')
	
	plot(data$year, data$factMean, type = 'n',
		xlab = 'Year',
		ylab = 'Factor Analytic AF Scores',
		xlim = c(2000, 2020),
		main = 'AF Scores over Time',
		axes = FALSE)
	axis(1)
	axis(2)
	for(i in lander){
		if(i == 'Bundesrepublik'){
			lines(data$year[data$land == i], data$factMean[data$land == i], col = 'black', lwd = 2)
		}else{
			lines(data$year[data$land == i], data$factMean[data$land == i], col = rgb(0, 0, 0, 0.2), lwd = 2)
		}
	}
	legend('topleft', legend = c('Bundestag', 'Landtage'), col = c('black', rgb(0, 0, 0, 0.2)),
		lwd = 3, bty = 'n')
	
	plot(hold$squire, hold$factMean, type = 'n',
		main = 'Comparing Summary Scores (Landtage only)',
		xlab = 'Squire-modeled Index',
		ylim = c(-1, 1),
		xlim = c(0, 0.6),
		ylab = 'Factor Analytic AF Scores',
		axes = FALSE)
	axis(1)
	axis(2)
	points(hold$squire, hold$factMean, col = rgb(0, 0, 0, 0.3), pch = 1)
	lines(lowess(hold$squire[is.na(hold$squire) == FALSE], hold$factMean[is.na(hold$squire) == FALSE]), col = rgb(0, 0, 0, 0.5), lwd = 3)

	hold <- hold[order(hold$factMean), ]
	plot(hold$factMean, hold$factMean, type = 'n',
		main = 'Uncertainty in AF Scores (Landtage only)',
		xlab = 'Factor Analytic AF Scores',
		ylab = '',
		ylim = c(1, 320),
		xlim = c(-2, 2),
		axes = FALSE)
	axis(1)
	for(i in 1:320){
		lines(c(hold$factHi[i], hold$factLo[i]), c(i, i), col = rgb(0, 0, 0, 0.2))
		points(hold$factMean[i], i, col = rgb(0, 0, 0, 0.2), pch = 16)
	}
	lines(c(median(hold$factMean), median(hold$factMean)), c(0, 321), lwd = 2)	
	lines(c(quantile(hold$factMean, 0.25), quantile(hold$factMean, 0.25)), c(0, 321), lwd = 2, lty = 3)
	lines(c(quantile(hold$factMean, 0.75), quantile(hold$factMean, 0.75)), c(0, 321), lwd = 2, lty = 3)
	legend('bottomright', legend = c('Median', '1st and 3rd Quartile'), lwd = 2, lty = c(1, 3), bty = 'n')
	dev.off()
	
## and now the bw plots ##
## this is figure 2 ##

bw <- data[data$land == 'Baden-Wurttemberg', ]

	pdf('fig2.pdf', height = 10, width = 10)
	par(mfrow = c(2, 2))
	plot(bw$year, bw$salary/1000, type = 'n',
		xlab = 'Year',
		ylab = 'Thousands of Euros',
		xlim = c(2000, 2020),
		main = 'MP Monthly Compensation',
		axes = FALSE)
	axis(1)
	axis(2)
	points(bw$year[bw$year != 2011], bw$salary[bw$year != 2011]/1000, col = rgb(0, 0, 0, 0.5), pch = 16)
	lines(lowess(bw$year[bw$year < 2011], bw$salary[bw$year < 2011]/1000), col = rgb(0, 0, 0, 0.5), lwd = 3)
	lines(lowess(bw$year[bw$year > 2011], bw$salary[bw$year > 2011]/1000), col = rgb(0, 0, 0, 0.5), lwd = 3)
	lines(c(2011, 2011), c(min(bw$salary/1000), max(bw$salary/1000)), col = rgb(0, 0, 0, 0.5), lty = 3, lwd = 2)
	text(2007, 6, 'Pre-reform period')
	arrows(2010, 5.75, 2006, 5.75, length = 0.125)
	text(2015, 6.5, 'Post-reform period')
	arrows(2012, 6.75, 2016, 6.75, length = 0.125)
	text(2015, 5.5, paste('t=', 
		round(t.test(bw$salary[bw$year > 2011], bw$salary[bw$year < 2011])$statistic, 2), sep = ''))

	plot(bw$year, bw$staff/1000, type = 'n',
		xlab = 'Year',
		ylab = 'Thousands of Euros',
		xlim = c(2000, 2020),
		main = 'Annual Spending on MP Staff Support',
		axes = FALSE)
	axis(1)
	axis(2)
	points(bw$year[bw$year != 2011], bw$staff[bw$year != 2011]/1000, col = rgb(0, 0, 0, 0.5), pch = 16)
	lines(lowess(bw$year[bw$year < 2011], bw$staff[bw$year < 2011]/1000), col = rgb(0, 0, 0, 0.5), lwd = 3)
	lines(lowess(bw$year[bw$year > 2011], bw$staff[bw$year > 2011]/1000), col = rgb(0, 0, 0, 0.5), lwd = 3)
	lines(c(2011, 2011), c(min(bw$staff/1000), max(bw$staff/1000)), col = rgb(0, 0, 0, 0.5), lty = 3, lwd = 2)
	text(2007, 100, 'Pre-reform period')
	arrows(2010, 92.5, 2006, 92.5, length = 0.125)
	text(2015, 120, 'Post-reform period')
	arrows(2012, 112.5, 2016, 112.5, length = 0.125)
	text(2015, 70, paste('t=', 
		round(t.test(bw$staff[bw$year > 2011], bw$staff[bw$year < 2011])$statistic, 2), sep = ''))

	plot(bw$year, bw$commDays, type = 'n',
		xlab = 'Year',
		ylab = 'Total Committee Meetings',
		xlim = c(2000, 2020),
		main = 'Annual Committee Meetings',
		axes = FALSE)
	axis(1)
	axis(2)
	points(bw$year[bw$year != 2011], bw$commDays[bw$year != 2011], col = rgb(0, 0, 0, 0.5), pch = 16)
	lines(lowess(bw$year[bw$year < 2011], bw$commDays[bw$year < 2011]), col = rgb(0, 0, 0, 0.5), lwd = 3)
	lines(lowess(bw$year[bw$year > 2011], bw$commDays[bw$year > 2011]), col = rgb(0, 0, 0, 0.5), lwd = 3)
	lines(c(2011, 2011), c(min(bw$commDays), max(bw$commDays)), col = rgb(0, 0, 0, 0.5), lty = 3, lwd = 2)
	text(2007, 105, 'Pre-reform period')
	arrows(2010, 102, 2006, 102, length = 0.125)
	text(2015, 110, 'Post-reform period')
	arrows(2012, 107, 2016, 107, length = 0.125)
	text(2015, 97, paste('t=', 
		round(t.test(bw$commDays[bw$year > 2011], bw$commDays[bw$year < 2011])$statistic, 2), sep = ''))

	plot(bw$year, bw$plenDays, type = 'n',
		xlab = 'Year',
		ylab = 'Days in Plenary',
		xlim = c(2000, 2020),
		main = 'Annual Plenary Session Days',
		axes = FALSE)
	axis(1)
	axis(2)
	points(bw$year[bw$year != 2011], bw$plenDays[bw$year != 2011], col = rgb(0, 0, 0, 0.5), pch = 16)
	lines(lowess(bw$year[bw$year < 2011], bw$plenDays[bw$year < 2011]), col = rgb(0, 0, 0, 0.5), lwd = 3)
	lines(lowess(bw$year[bw$year > 2011], bw$plenDays[bw$year > 2011]), col = rgb(0, 0, 0, 0.5), lwd = 3)
	lines(c(2011, 2011), c(min(bw$plenDays), max(bw$plenDays)), col = rgb(0, 0, 0, 0.5), lty = 3, lwd = 2)
	text(2007, 20, 'Pre-reform period')
	arrows(2010, 19, 2006, 19, length = 0.125)
	text(2015, 22, 'Post-reform period')
	arrows(2012, 21, 2016, 21, length = 0.125)
	text(2015, 17.3, paste('t=', 
		round(t.test(bw$plenDays[bw$year > 2011], bw$plenDays[bw$year < 2011])$statistic, 2), sep = ''))
	dev.off()

## now the simple turnover model ##
## this will use the whole sample, back to reunification ##
## so we estimate another fa for our central measure ##

	model <- MCMCmixfactanal(~ salary + staff + commDays + plenDays, data = data1, factors = 1,
		lambda.constraints = list(salary = list(2, '+'), staff = list(2, '+'), commDays = list(2, '+'), plenDays = list(2, '+')),
			burnin = 10000, mcmc = 100000, thin = 100, seed = 1,
			store.scores = TRUE, tune = 0.8)
	summary(model)
	
	loadMean <- colMeans(model[1:1000, 1:4])
	loadSd <- colSds(model[1:1000, 1:4])
	round(loadMean, 3)
	round(loadSd, 3)

## now we can splice the results into the main data ##

	data1$factMean <- colMeans(model[1:1000, 5:531])

	mod1 <- lm(turnover ~ factMean + as.factor(land), data = data1)
	summary(mod1)
	mod2 <- lm(turnover ~ factMean + as.factor(year), data = data1)
	summary(mod2)
	mod3 <- lm(turnover ~ factMean + as.factor(land)+ as.factor(year), data = data1)
	summary(mod3)
	stargazer(mod1, mod2, mod3)

## this is table 2 ##
	stargazer(mod3)	

## and we're donezo ##