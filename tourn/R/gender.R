#The user needs to specify:
	#df: gender data frame
	#gvar: gender variable
	#var1: variable to compare

gender <- function(df, gvar, var1)
{
	var1.name <- deparse(substitute(var1))
	gvar <- eval(substitute(gvar),df,parent.frame())

	gmale <- df[gvar == "male",]
	gfemale <- df[gvar == "female",]

	varmale <- eval(substitute(var1),gmale,parent.frame())
	varfemale <- eval(substitute(var1),gfemale,parent.frame())

	dev.new()
	aplot <- density(varmale)
	bplot <- density(varfemale)

	if (max(aplot$y) >= max(bplot$y))
	{
		plot(density(varmale), col = 'blue',
		main = paste("Gender frequency by ",var1.name), lwd = 2)

		points(density(varfemale), col = 'red', type = 'l', lwd = 2)
	}

	else if(max(aplot$y) < max(bplot$y))
	{
		plot(density(varfemale), col = 'red',
		main = paste("Gender frequency by ",var1.name), lwd = 2)

		points(density(varmale), col = 'blue', type = 'l', lwd = 2)
	}

	legend("topright", inset = .02, c("Male", "Female"), lty = c(1,1), 
	lwd = c(2.5,2.5), col = c("Blue", "Red"))

	return("A graph should pop up on the screen")
}
