
stockList <- read.table("constituents.csv", header = TRUE, sep=",")[,1]

getUrl <- function (symbol) {
	url <- paste("http://api.kibot.com/?action=history&symbol=", symbol, "&interval=daily&period=5000", sep = "")
	return(url)
}

downloadData <- function ( listOfStocks ) { 
	#This is the master function that downloads all relevant data and applies transformations
}

percentChange <- function ( list ) {
	return ( ( list[, 5] - list[, 2] ) / list [, 2] * 100 )
}

########################### SANDBOX ##############################

SPX <- read.csv ( getUrl ( "$SPX") )
table <- read.csv ( getUrl ( toString(stockList[1]) ) )
testSet <- percentChange ( SPX )
sum ( testSet > 0 )