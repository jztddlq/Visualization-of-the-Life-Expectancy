# Visualization-of-the-Life-Expectancy
Visualization of the Life Expectancy &amp; Healthcare Expenditure Indicators in WHO Dataset

==Background introduction==

Life expectancy, often abbreviated to LEB (for Life expectancy at birth), is a statistical measure of the average time an organism is expected to live, based on the year of its birth, its current age and other demographic factors including gender.[1] The World Health Organization (WHO) is a specialized agency of the United Nations that is concerned with international public health. It was established on 7 April 1948, and is headquartered in Geneva, Switzerland.[2] WHO collects the data related to the life expectancy, like the infant mortality rate, alcohol consumption, for countries through out the world every year since 1960s. Besides, in its official website, we also found the data related to healthcare expenditure, for example, the total health expenditure per capita of different countries for the past 50 years. In total, there are more 150 indicators including all the aspects related to life expectancy and health spending, and most of them are numerical format. It’s far too much for reviewers and organizers to utilize the information.

Motivations and objectives==

To help the WHO better understand the data they collected, we plan to use this geographical-referenced data to design an interactive and dynamic application to help the authority facilitate the usage of the data so that they can provide valuable recommendations to countries facing severe situations. We will visualize the indicators so that users can see the distributions and features of these factors in our app. Besides, we will use the DTW(Dynamic Time Warping) cluster to classify countries sharing the similar changing patterns over time or in different time period, which can support the organization simplify their management as well as increase resource availability.
R shiny is a very powerful R package that makes it easy to build interactive web apps straight from R. And the capability of Shiny and rich R packages make R an excellent choice to help us perform visualization analysis.

==Data Structure==

The dataset consists of three parts, first part is the geographical indicators, countries, it contains more 200 countries and areas all around the world. Second part is the topics that we are interested in and planning to conduct further analysis. Specifically, the topics also divided into three segments, Demographical indicators, including the Population, Infant death rate, Adult Mortality death rate etc.. Health indicators, most are diseases containing several common and deathful illness like HIV/Aids and the consumption of unhealthy product like alcohol, and the last topic is about the Economics variables to measure the development level and health expenditure of the country. The third field of our data is time series related data from 2000 to 2016. The table below shows the overall structure of our data.
