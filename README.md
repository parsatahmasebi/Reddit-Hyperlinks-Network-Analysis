# Reddit-Hyperlinks-Network-Analysis

What is Reddit?
Reddit is a social news aggregation, web content rating and discussion site. It is also known as “the front page of the internet”. Anyone can create an account and post contents to share, such as links, texts, images and videos. It has grown to become the seventh most visited sites in the United States, and the 18th most visited sites in the world. 


The objective for this project is to use the title and body portions of subreddit hyperlinks to first analyze the sentiment of hyperlinks. This will be done using a previously assigned properties column that was categorized using Linguistic Inquiry and Word Count(LIWC) to determine the sentiment of the text in the subreddit posts. Then, we will calculate eigen centralities of both title and body to attempt to analyze the influencers in the network of subreddit hyperlinks, and to analyze the text that they have shared in their subsequent hyperlinks to see the relationship between text and sentiment. Lastly, we are using logistic regression to predict how the different words that are used in the body of the hyperlinks affect the sentiment of the post, for example having a word more related to family or home will lead to increase in the likelihood that the sentiment of the post will be positive.
