#This functopm takes a dataframe with at least 3 columns -- gender (male or female), powerhouse (1 or 0), and an extra variable. It creates two new dataframe by filtering males and females. Then it creates 4 new dataframes by filtering males by powerhouse and non-powerhouse and females by powerhouse andnon-powerhouse. Then, a new plot is created of all 4 graphs combined and saved. Then, 4 separate graphs are created and saved. 
 
#The user needs to specify:
	#df: data frame
	#gen: gender variable
	#pow: phouse variable
	#var2: variable to compare

#this would output 4 graphs
	#power male vs. var2
	#nonpower male vs. var2
	#power female vs. var2
	#nonpower female vs. var2

	#these four will be graphed separately
	#these four will all be combined on one graph

var2 <- function(df, gen, pow, var2)
{
	var2.name <- deparse(substitute(var2))

	gen <- eval(substitute(gen),df,parent.frame())

	gmale <- df[gen == "male",]
	gfemale <- df[gen == "female",]

	powm <- eval(substitute(pow),gmale,parent.frame())
	powf <- eval(substitute(pow),gfemale,parent.frame())
	
	powmale <- gmale[powm == 1,]
	nonpowmale <- gmale[powm == 0,]
	powfemale <- gfemale[powf == 1,]
	nonpowfemale <- gfemale[powf == 0,]

	vpowmale <- eval(substitute(var2),powmale,parent.frame())
	vnonpowmale <- eval(substitute(var2),nonpowmale,parent.frame())
	vpowfemale <- eval(substitute(var2),powfemale,parent.frame())
	vnonpowfemale <- eval(substitute(var2),nonpowfemale,parent.frame())

	dev.new()
	aplot <- density(vpowmale)
	bplot <- density(vnonpowmale)
	cplot <- density(vpowfemale)
	dplot <- density(vnonpowfemale)

	a <- max(aplot$y)
	b <- max(bplot$y)
	c <- max(cplot$y)
	d <- max(dplot$y)

	max = a
	if (b > max) max = b
	if (c > max) max = c
	if (d > max) max = d

	if (a == max)
	{
		plot(aplot, col = 'darkslategrey',
		main = paste("Powerhouse and gender frequency by ",
		var2.name), lwd = 2)

		points(bplot, col = 'firebrick4', type = 'l', lwd = 2)
		points(cplot, col = 'deepskyblue', type = 'l', lwd = 2)
		points(dplot, col = 'gold', type = 'l', lwd = 2)
	}

	else if (b == max)
	{
		plot(bplot, col = 'firebrick4',
		main = paste("Powerhouse and gender frequency by ", 
		var2.name), lwd = 2)

		points(aplot, col = 'darkslategrey', type = 'l', lwd = 2)
		points(cplot, col = 'deepskyblue', type = 'l', lwd = 2)
		points(dplot, col = 'gold', type = 'l', lwd = 2)
	}
	
	else if (c == max)
	{
		plot(cplot, col = 'deepskyblue',
		main = paste("Powerhouse and gender frequency by ", 
		var2.name), lwd = 2)

		points(aplot, col = 'darkslategrey', type = 'l', lwd = 2)
		points(bplot, col = 'firebrick4', type = 'l', lwd = 2)
		points(dplot, col = 'gold', type = 'l', lwd = 2)
	}

	else if (d == max)
	{
		plot(dplot, col = 'gold',
		main = paste("Powerhouse and gender frequency by ", 
		var2.name), lwd = 2)

		points(aplot, col = 'darkslategrey', type = 'l', lwd = 2)
		points(bplot, col = 'firebrick4', type = 'l', lwd = 2)
		points(cplot, col = 'deepskyblue', type = 'l', lwd = 2)
	}

	legend("topright", inset = .01, c("Phouse male", "nonPhouse male", "Phouse female", "nonPhouse female"), lty = c(1,1,1,1), lwd = c(1.5, 1.5, 1.5, 1.5), col = c("darkslategrey", "firebrick4", "deepskyblue", "gold"))

	dev.copy(png,'4plot.png')
	dev.off()

	dev.new()
	plot(aplot, col = 'darkslategrey',
	main = paste('Powerhouse male and',
	var2.name), lwd = 2)
	dev.copy(png,'plota.png')
	dev.off()

	dev.new()
	plot(bplot, col = 'firebrick4',
	main = paste('non-Powerhouse male and',
	var2.name), lwd = 2)
	dev.copy(png,'plotb.png')
	dev.off()

	dev.new()
	plot(cplot, col = 'deepskyblue',
	main = paste('Powerhouse female and',
	var2.name), lwd = 2)
	dev.copy(png,'plotc.png')
	dev.off()

	dev.new()
	plot(dplot, col = 'gold',
	main = paste('non-Powerhouse female and',
	var2.name), lwd = 2)
	dev.copy(png,'plotd.png')
	dev.off()

	return("You should see 5 graphs")
}

