######## load data #######

stockList <- as.vector(read.table("constituents1.csv", header = TRUE, sep=",", quote="")[,1])
tenstocks <- as.vector(stockList[1:10])

########################### with packages ############################
require(quantmod)
require(PerformanceAnalytics)

getBetas <- function ( symbvec, bench, date = "2014-1-1" ) {
	df <- data.frame(NULL)
	for ( i in symbvec ) {
		if (nrow(df)==0) { tryCatch( df <- getBeta ( i, bench, date ), error = function (e) NULL) } # Creates df if no data
		else { tryCatch( df <- rbind( df, getBeta ( i, bench, date ) ), error = function (e) NULL) }
	} # rbinds data to master data frame
	return (df)
}



getBeta <- function ( symb, bench, date = "2014-1-1" ) {
	d <- getSymbols ( symb, src="google", auto.assign=FALSE) # Use google for most symbols because of errors
	b <- getSymbols ( bench, src="yahoo", auto.assign=FALSE)
		

	d <- d[index(d) > date]
	b <- b[index(b) > date]

	dr <- monthlyReturn ( d[,4] )
	br <- monthlyReturn ( b[,6] )
	
	drd <- dailyReturn ( d[,4] )
	brd <- dailyReturn ( b[,6] )
		
	beta <- round( CAPM.beta ( dr, br ), digits = 3 )
	beta.bull <- round( CAPM.beta.bull ( dr, br ), digits = 3 ) 
	beta.bear <- round( CAPM.beta.bear ( dr, br ), digits = 3 )
	beta.daily <- round( CAPM.beta ( dr, br ), digits = 3 )
	beta.bull.daily <- round( CAPM.beta.bull ( drd, brd ), digits = 3 ) 
	beta.bear.daily <- round( CAPM.beta.bear ( drd, brd ), digits = 3 )

	betaVec <- c(beta, beta.bull, beta.bear, beta.bull/beta.bear, beta.daily, beta.bull.daily, beta.bear.daily, beta.bull.daily/beta.bear.daily)

	names (betaVec) <- c( "Beta", "Beta Bull", "Beta Bear", "Bull/Bear Ratio", "Beta Daily", "Beta Bull Daily", "Beta Bear Daily", "Bull/Bear Daily (Ratio)")
	betaVec <- as.data.frame ( t ( betaVec ) )
	rownames ( betaVec ) <- symb
	return (betaVec)
}

spybetas <- getBetas ( stockList, "^GSPC")
monthlyRatioOrder <- spybetas[order(spybetas$"Bull/Bear Ratio"),]
dailyRatioOrder <- spybetas[order(spybetas$"Bull/Bear Daily (Ratio)"),]
dailyBearOrder <- spybetas[order(spybetas$"Beta Bear Daily"), ]
write.table(monthlyRatioOrder, "TopBullBearBetaRatioMonthly.csv", sep=",")
write.table(dailyRatioOrder, "TopBullBearBetaRatioDaily.csv", sep=",")
write.table(dailyBearOrder, "TopBullBearBetaRatioDaily.csv", sep=",")

########################### Started without packages. Then discovered packages ######################

getUrl <- function (symbol) {
	url <- paste("http://api.kibot.com/?action=history&symbol=", symbol, "&interval=daily&period=5000", sep = "")NYSE:KO
	return(url)
}

downloadData <- function ( listOfStocks ) { 
	#This is the master function that downloads all relevant data and applies transformations
}

percentChange <- function ( list ) {
	return ( ( list[, 5] - list[, 2] ) / list [, 2] * 100 )
}