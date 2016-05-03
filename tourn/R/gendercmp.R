#The user needs to specify: 
	#merged dataframe that includes prelim and elim 
	#starting column of elim rounds
	#ending column of elim rounds
	#variable we compare graphs e.g. gender
	#the seed cutoff -- the default is 32

#we assume that in the dataframe the following variables exist
	#trips
	#doubs
	#seed
	#gender

gendercmp <- function(all, startc, endc, seedc = 32)
{
	sumbna <- function(rw)sum(rw, na.rm = TRUE)

	all[,startc:endc][all[,startc:endc] == -1] <- NA 
	#changes all -1 to NA
	all$totwins <- apply(all[,startc:endc], 1, sumbna)

	if(seedc == 32)
	{
	  whichelims <- is.na(all$trips) & !is.na(all$doubs) & all$seed <= 32
	  all <- all[whichelims,]
	}
	else if (seedc == 64)
	{
	  whichelims <- !is.na(all$trips) & all$seed <= 64
	  all <- all[whichelims,]
	}
	
	allmale <- all[all$gender == 'male',]
	allfemale <- all[all$gender == 'female',]
	
	avgmale <- tapply(allmale$totwins, allmale$seed, mean)
	avgfemale <- tapply(allfemale$totwins, allfemale$seed, mean)

	dev.new()

	mnames <- as.numeric(names(avgmale))
	plot(lowess(mnames, avgmale), #1:seedc is incorrect 
	col = 'blue', type = 'l', main = 'Smoothed Mean Elim Wins vs. Seed', 
	xlab = 'seed', ylab = 'mean elim wins')
	
	fnames <- as.numeric(names(avgfemale))
	points(lowess(fnames, avgfemale), col = 'red', type = 'l')
	#1:seedc is incorrect

	legend("topright", inset = .03, c('Male', 'Female'), lty = c(1,1),
	lwd = c(2.5, 2.5), col = c('blue', 'red'))

	return('A screen should pop up with a graph!')
}
