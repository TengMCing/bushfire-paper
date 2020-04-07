# reading list

1. A new tidy data structure to support exploration and modeling of temporal data
2. Enabling interactivity on displays of multivariate time series and longitudinal data
3. CRAN Task View: Handling and Analyzing Spatio-Temporal Data
4. CRAN Task View: Time Series Analysis
5. Map Plots Created With R And Ggmap
6. tidyverts - Tidy tools for time series
7. plotly-r - Interactive web-based data visualization with R, plotly, and shiny
8. http://socviz.co/ Data Visualization A practical introduction
9. Analyzing spatio-temporal data with R: Everything you always wanted to know – but were afraid to ask
10. Geocomputation with R

# Thoughts & Notes

1. A new tidy data structure to support exploration and modeling of temporal data

	Problem: inconsistent formats of wild temporal data

	Inspiration: tidy data workflow proposed by Wickham and Grolemund 

    Package: tsibble

    A new type of data structure

    Fields: index key measurements

    index + key = PK

    index: explicit

    Automatically check PK when construct a tsibble 

    Interval selection: 
    	a. Compute GCD as fixed interval
    	b. irregular interval
    	c. unknown interval

    provide tools for handling missing value 
    adopt dplyr's tools and modify for ts data

    A workflow for dealing with temporal data 
    Can be used in the project to improve the workflow

2. Enabling interactivity on displays of multivariate time series and longitudinal data

    
