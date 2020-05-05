# reading list

1. A new tidy data structure to support exploration and modeling of temporal data
2. Enabling interactivity on displays of multivariate time series and longitudinal data
3. CRAN Task View: Handling and Analyzing Spatio-Temporal Data
4. CRAN Task View: Time Series Analysis
5. Map Plots Created With R And Ggmap
6. tidyverts - Tidy tools for time series
7. plotly-r - Interactive web-based data visualization with R, plotly, and shiny
8. http://socviz.co/ Data Visualization A practical introduction
10. Geocomputation with R
11. A general science-based framework for dynamical spatio-temporal models
12. Spatio-Temporal Statistics with R

# Thoughts & Notes

## 1. A new tidy data structure to support exploration and modeling of temporal data

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

Thoughts: 

may or may not need to use tsibble to imporve the workflow considering time has been spent on data wrangling. 

I am more interested in the plots Earo generated in the example XD

## 2. Enabling interactivity on displays of multivariate time series and longitudinal data

Making comparisons between many time series is a challenge.

This paper introduces many ways to make graphics, especially interactive graphics

packages: cranvas

only put init state and operations into memory

Thoughts:

Reading the horizon plot is confusing, perhaps it is because of the baseline.

I am not very understand y-wrapping plot 

This package can be used in the final shiny app

    
## 3. CRAN Task View: Handling and Analyzing Spatio-Temporal Data

Thoughts:

It's a long list of packages in R community related to spatio-temporal data analysis

I have little knowledge in this field so far, so I don't know what's the purpose of many of those packages

It's too long so that I think I'd better read the books/articles first and see what packages they are using. 
Perhaps I will check the packages for visualization later since there are only 6 of them in the list.

## 4. CRAN Task View: Time Series Analysis

Thoughts:

Comparing to the previous list of package, I feel better with this one.
There are serveral packages I have touched before
Many terms in this list remind of the forecasting book written by Rob

Ok, turns out the maintainer of this webpage is Rob XD

Time to read that book again I guess, I didn't finish it last time. 
It's necessary because I haven't enrolled in the forecasting unit.

I will check this list later when I finish reading other books

## 5. Map Plots Created With R And Ggmap

An introduction of ggmap with an application about 911 incidents

Thoughts:

It uses Google map as background, which may be different with my idea.
I want to use OSM or Stamen as background, since ggmap also has built-in method to get OSM static map.

It illustrates some interesting techniques:

	a. geom_label_repel for labelling those important points
	this technique can be used to mark the severe bushfire

	b. density plot for visulizing dangerous area
	adding density lines on plot is wonderful



