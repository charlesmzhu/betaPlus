######## load data #######

stockList <- as.vector(read.csv("constituents.csv", header = TRUE)[,1])
tenstocks <- as.vector(stockList[1:10])

########################### with packages ############################
require(quantmod)
require(PerformanceAnalytics)

getBetas <- function ( symbvec, bench, from = "2007-01-01", to = Sys.Date(), period = "both" ) {
	df <- data.frame(NULL)
	for ( i in symbvec ) {
		if (nrow(df)==0) { tryCatch( df <- getBeta ( i, bench, from, to, period ), error = function (e) NULL) }
		else { tryCatch( df <- rbind( df, getBeta ( i, bench, from, to , period ) ), error = function (e) NULL) }
	}
	return (df)
}

getBeta <- function ( symb, bench, from = "2007-01-01", to = Sys.Date(), period = "both" ) {
	d <- getSymbols.google ( symb, from = from , to = to, auto.assign=FALSE)
	b <- getSymbols.yahoo ( bench, from = from, to = to, auto.assign=FALSE)
		
	dr <- monthlyReturn ( d[,4] )
	br <- monthlyReturn ( b[,6] )
	
	drd <- dailyReturn ( d[,4] )
	brd <- dailyReturn ( b[,6] )
	
	if (period == "both") {
		
		beta_month <- round( CAPM.beta ( dr, br ), digits = 3 )
		bull_month <- round( CAPM.beta.bull ( dr, br ), digits = 3 ) 
		bear_month <- round( CAPM.beta.bear ( dr, br ), digits = 3 )
		
		bull_bear_ratio_month <- round( bull_month / bear_month, digits = 3 )
		
		beta_day <- round( CAPM.beta ( drd, brd ), digits = 3 )
		bull_day <- round( CAPM.beta.bull ( drd, brd ), digits = 3 ) 
		bear_day <- round( CAPM.beta.bear ( drd, brd ), digits = 3 )
		
		bull_bear_ratio_day <- round( bull_day / bear_day , digits = 3 )
		
		betaVec <- c(beta_day, bull_day, bear_day, bull_bear_ratio_day, beta_month, bull_month, bear_month, bull_bear_ratio_month)
		
		names (betaVec) <- c( "Beta Day", "Bull Day", "Bear Day", "Bull:Bear Day", "Beta Month", "Beta Bull Month", "Beta Bear Month", "Bull:Bear Month")
		
	} else if ( period == "daily") {
		
		beta_day <- round( CAPM.beta ( drd, brd ), digits = 3 )
		bull_day <- round( CAPM.beta.bull ( drd, brd ), digits = 3 ) 
		bear_day <- round( CAPM.beta.bear ( drd, brd ), digits = 3 )
		
		bull_bear_ratio_day <- round( bull_day / bear_day , digits = 3 )
		
		betaVec <- c(beta_day, bull_day, bear_day, bull_bear_ratio_day)

		names (betaVec) <- c( "Beta", "Bull", "Bear", "Bull:Bear")
				
	} else if ( period == "monthly") {

		beta_month <- round( CAPM.beta ( dr, br ), digits = 3 )
		bull_month <- round( CAPM.beta.bull ( dr, br ), digits = 3 ) 
		bear_month <- round( CAPM.beta.bear ( dr, br ), digits = 3 )
		
		bull_bear_ratio_month <- round( bull_month / bear_month, digits = 3 )
		
		betaVec <- c(beta_month, bull_month, bear_month, bull_bear_ratio_month)
		names (betaVec) <- c( "Beta", "Bull", "Bear", "Bull:Bear")
	
	}
	
	betaVec <- as.data.frame ( t ( betaVec ) )
	rownames(betaVec) <- symb
	return ( betaVec )
	
}

####################### Commands

write.csv(getBetas ( stockList, "^GSPC", from = "2005-01-01" ), "AllBetasTo2005.csv" )
write.csv(getBetas ( stockList, "^GSPC", from = Sys.Date()-365*2, period="daily" ), "DailyBetaForTwoYears.csv" )

########################### Started without packages. Then discovered packages. IGNORE BELOW! ######################

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