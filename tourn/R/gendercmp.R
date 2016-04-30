#The user needs to specify: 
	#merged dataframe that includes prelim and elim 
	#starting column of elim rounds
	#ending column of elim rounds
	#variable we compare graphs e.g. gender

#we assume that in the dataframe the following variables exist
	#trips
	#doubs
	#seed	
	#gender

gendercmp <- function(all, startc, endc)
{
	sumbna <- function(rw)sum(rw, na.rm = TRUE)
	all$totwins <- apply(all[,startc:endc], 1, sumbna)

	whichelims <- is.na(all$trips) & !is.na(all$doubs) & all$seed <= 32
	all <- all[whichelims,]
	
	allmale <- all[all$gender == 'male',]
	allfemale <- all[all$gender == 'female',]
	
	avgmale <- tapply(allmale$totwins, allmale$seed, mean)
	avgfemale <- tapply(allfemale$totwins, allfemale$seed, mean)

	dev.new()

	plot(lowess(1:32, avgmale), 
	col = 'blue', type = 'l', main = 'Smoothed Mean Elim Wins vs. Seed', 
	xlab = 'seed', ylab = 'mean elim wins')

	points(lowess(c(1:2, 4:32), avgfemale), col = 'red', type = 'l')

	legend("topright", inset = .03, c('Male', 'Female'), lty = c(1,1),
	lwd = c(2.5, 2.5), col = c('blue', 'red'))

	return('A screen should pop up with a graph!')
}
