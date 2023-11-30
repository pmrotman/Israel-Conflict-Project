# Israel-Conflict-Project
Description:
R-shiny app that allows a user to filter data on the Israel-Palestine conflict since 2016. The dataset is taken from "The Armed Conflict Location & Event Data Project" (ACLED, https://acleddata.com/data-export-tool/) and contains a large amount of information (such as fatalities, violence type, and location data) about violent events in Israel, Palestine, and Lebanon since 2016. The query for the data: Date Range = 01-01-2016 to 27-11-2023; Country = Israel, Palestine, Lebanon; Keyword = Israel. The data was then further filtered to exclude all non-Hezbollah events in Lebanon and combine all regions in Lebanon into one, Lebanon. Additionally, an unidentified region "" containing 3 total events was removed. The app allows the user to filter the data, plot the data points in a map (and select for more information), and perform a principal component analysis on the filtered data, based on location and event type. 

Usage:
To run the app in an R environment, run the command: runGitHub("Israel-Conflict-Project", "pmrotman", subdir = "IP.R"). 
To run the app on the web, utilize the following link: https://pmrotman.shinyapps.io/israel-conflict-project/. 
In three tabs, the app allows the user to view data, plot a map of the data, and visualize the data in a bar chart and perform a PCA on region and event type.
On the Data Selection tab, the user filters the data, selecting a date range, event types, and regions. The user can view several columns of the data and select a plot button.
Once the plot button has been selected, a map with all selected data points is generated in the Map tab. The user can select points on the map to display data. If the user then selects a row, the "notes" column of the data set will be output below.
In the PCA tab, a bar plot of counts vs event type and region is produced, and a principal component analysis is performed on the filtered data. The output is a biplot and a summary of the data.

