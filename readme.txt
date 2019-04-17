Twitter analysis and Technical analysis for stock price prediction in R

	This project comprises of two phases, (i)To find correlation between tweet sentiments and stock value variation (find more in documents). (ii) To build an application which can extract tweets in real time through Twitter API and sentimentally analyse. Also it fetches live stock price using Yahoo financial API and forecasting model is built using ARIMA model (find more in documents). 

	The project was built in R's Shiny application and consists of four tabs; (1) Fetching tweets through Twitter API and forming word cloud by text mining, (2) Using NRC lexicon, tweet sentiment is analysed, (3) Using Rbokeh package, time graph for tweets is developed, (4) Yahoo financial API is used to display live stock data and also forecasted model(built using ARIMA).

Use Case:
	Let’s look at one instance of usage of the application. Company XYZ is conducting a twitter analysis on their latest product ABC. So, company will search for a tweets with say “#ABC”, 1000 tweets will be retrieved. In sentiment tab, it is found that among 1000 tweets, anger emotion has majority tweets with a count of 400 tweets, so it means that the new product launched by the company received too many critics on anger note. Now company should analyse the tweets of anger, which can be done in time graph tab of the application and finally can come up with a solution to satisfy its customer.
