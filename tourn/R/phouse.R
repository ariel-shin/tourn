#The user needs to specify:
	#df: phouse data frame 
	#pvar: phouse variable
	#var1: variable to compare

phouse <- function(df, pvar, var1)
{
	var1.name <- deparse(substitute(var1))
	pvar <- eval(substitute(pvar),df,parent.frame())

	pyes <- df[pvar == 1,]
	pno <- df[pvar == 0,]

	varyes <- eval(substitute(var1),pyes,parent.frame())
	varno <- eval(substitute(var1),pno,parent.frame())

	dev.new()
	aplot <- density(varyes)
	bplot <- density(varno)

	if (max(aplot$y) >= max(bplot$y))
	{
		plot(density(varyes), col = 'darkgreen', 
		main = paste("Powerhouse frequency by ",var1.name), lwd = 2)

		points(density(varno), col = 'purple', type = 'l', lwd = 2)
	}
	
	else if(max(aplot$y) < max(bplot$y))
	{
		plot(density(varno), col = 'purple', 
		main = paste("Powerhouse frequency by ",var1.name), lwd = 2)

		points(density(varyes), col = 'darkgreen', type = 'l', 
		lwd = 2)
	}

	legend("topright", inset = .02, c("Powerhouse school", 
	"Non-Powerhouse school"), lty = c(1,1), lwd = c(2.5,2.5), 
	col = c("darkgreen", "purple"))
}
