# Sentiment Analysis Webscraping Project
## Project Purpose
Welcome! The purpose of this project is to compare coverage of the war in Ukraine and the Hamas attacks on Israel from the Associated Press. Specfically I decided to do a senitment analysis of the body text of articles from right after the beginning of each of these conflcits (10/7/23 for Israel and 2/24/22 for Ukraine). The unit of analysis of the project is at the sentence level, isolating sentences that contain the names of leaders involved in the conflict, which is Netanyahu, Putin, and Biden for both conflicts.

## Methodology
First the articles were scraped for body text, stuck in a excel sheet, and descriptive stats were performed on the body text. 
Then with LDAvis package, an intertopic distance map was created for each conflict.
Finally, sentences containing the names of the leaders were isolated, sentiment analysis was performed with the bing dictionary, and results were tested for significance and modeled.

## Findings will be detailed in the pdf attached
