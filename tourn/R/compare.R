#The user needs to specify:
	#df: gender data frame
	#bivar: binary variable -- 1 or 0 e.g. gender and phouse
	#var1: variable to compare

compare <- function(df, bivar, var1)
{
        var1.name <- var1

	if (bivar == "gender")
	{
		bivar1 <- df[df[[bivar]] == "male",]
		bivar0 <- df[df[[bivar]] == "female",]
	}

	else
	{
		bivar1 <- df[df[[bivar]] == 1,]
		bivar0 <- df[df[[bivar]] == 0,]
	}

        varmale <- bivar1[[var1]]
        varfemale <- bivar0[[var1]]

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
