setwd("~/Desktop/Winter Quarter/Customer & Social Analytics/Final Project Reddit")
library("dplyr")
library("tidyverse")
library("igraph")
library("readr")
library("tidyr")
install.packages("threejs")
library("threejs")
#install.packages("writexl")
#importing the data
df_body <- read.csv("soc-redditHyperlinks-body.tsv", sep = "\t") 

head(df_body)
colnames(df_body)

#Assigning a new column to each one of the unique properties 

df_body %>%
  separate(PROPERTIES , c( "Char_num" , "char_num_no_space", "alpha_char_frac" , "digits_frac" , "upper_frac" , "whitespace_frac",
                           "special_char_frac" , "word_cnt" , "unique_word_cnt" , "longword_cnt" , "avg_word_length" , "unique_stopword_cnt",
                           "Fraction_of_stopwords",
                           "sentences_cnt" , "longsentences_cnt" , "avg_char_persentence" , "avg_word_sentence" , "auto_readable_index",
                           "pos_sent_vad" , "neg_sent_vad" , "component_sent_vad", "LIWC_Funct",	"LIWC_Pronoun",	"LIWC_Ppron",	
                           "LIWC_I",	"LIWC_We",	"LIWC_You",	"LIWC_SheHe",	"LIWC_They",	
                           "LIWC_Ipron",	"LIWC_Article",	"LIWC_Verbs",	"LIWC_AuxVb",	
                           "LIWC_Past",	"LIWC_Present",	"LIWC_Future",	"LIWC_Adverbs",	"LIWC_Prep",	
                           "LIWC_Conj",	"LIWC_Negate",	
                           "LIWC_Quant",	"LIWC_Numbers",	"LIWC_Swear",	"LIWC_Social",	"LIWC_Family",	
                           "LIWC_Friends",	"LIWC_Humans",	"LIWC_Affect",	"LIWC_Posemo",	"LIWC_Negemo",	"LIWC_Anx",
                           "LIWC_Anger",	"LIWC_Sad",	"LIWC_CogMech",	"LIWC_Insight",	"LIWC_Cause",	"LIWC_Discrep",	
                           "LIWC_Tentat",	"LIWC_Certain",	"LIWC_Inhib",	"LIWC_Incl",	"LIWC_Excl",	"LIWC_Percept",	
                           "LIWC_See",	"LIWC_Hear",	"LIWC_Feel",	"LIWC_Bio",	"LIWC_Body",	"LIWC_Health",	
                           "LIWC_Sexual",	"LIWC_Ingest",	"LIWC_Relativ",	"LIWC_Motion",	"LIWC_Space",	"LIWC_Time",	
                           "LIWC_Work",	"LIWC_Achiev",	"LIWC_Leisure",	"LIWC_Home",	"LIWC_Money",	"LIWC_Relig",	
                           "LIWC_Death",	"LIWC_Assent",	"LIWC_Dissent",	"LIWC_Nonflu", "LIWC_Filler")) -> df_body
head(df_body)

#Exporting the dataframe to excel in order to create visuals in tableau
library("writexl")
write_xlsx(df_body,"/Users/parsat8/Desktop/Winter Quarter/Customer & Social Analytics/Final Project Reddit/\\datavisualization_body.xlsx")

# Let's do some exploratory data analysis 

# Data types

str(df_body)
summary(df_body)
glimpse(df_body)
table(df_body$LINK_SENTIMENT)

# any nulls values? No.

apply(df_body, 2, function(x) any(is.null(x)))

# Subsetting the data for creating a graph

graph_from_df1_main <- df_body[,1:2]

#Creating the graph
g <- graph_from_data_frame(graph_from_df1_main, directed = T)

# number of vertexes in the graph
V(g)
# Number of edges in the graph
E(g)

#Number of vertices
gorder(g)
#Number of edges
gsize(g)

#checking if the graph is directed
is.directed(g)

#checking too see if the graph is weighted
is.weighted(g)

head_of(g,E(g))


# Diameter ( longest path in the network)
get_diameter(g)


### Let's find the important verteces

graph_degree_all<-degree(g,mode = c("all"))
max(graph_degree_all)
graph_degree_all[graph_degree_all=="8667"]
#askreddit
df.degree.all<-as.data.frame(graph_degree_all)

graph_degree_in<-degree(g,mode = c("in"))
max(graph_degree_in)
graph_degree_in[graph_degree_in=="7329"]
#askreddit
df.degree.in<-as.data.frame(graph_degree_in)

graph_degree_out<-degree(g,mode = c("out"))
max(graph_degree_out)
graph_degree_out[graph_degree_out=="4665"]
#subredditdrama
df.degree.out<-as.data.frame(graph_degree_out)

# Find the vertices with max out degree

which.max(graph_degree)

#closeness: How quickly can a node interact with other nodes?

c<-closeness(g , mode = c("all") , weights = NA)
df.c<-as.data.frame(c)

# Eigenvector (centrality proportional to the sum of connection centralities)
# Values of the first eigenvector of the graph adjacency matrix
ec<-eigen_centrality(g, directed=T, weights=NA)
df.ec<-as.data.frame(ec)

# Betweenness (centrality based on a broker position connecting others)
# (Number of geodesics that pass through the node or the edge)
b<-betweenness(g, directed=T, weights=NA)
df.b<-as.data.frame(b)

#edge_betweenness(g, directed=T, weights=NA)



#Measures of network structure

density <- edge_density(g)
density


# Average path length 
# The mean of the shortest distance between each pair of nodes in the network 
# (in both directions for directed graphs). 
mean_distance(g, directed=F)
mean_distance(g, directed=T)

# We can also find the length of all shortest paths in the graph:
#distances(g) # with edge weights
#distances(g, weights=NA) # ignore weights

# Hub and Authority Score
hs <- hub.score(g)$vector
df.hs<-as.data.frame(hs)
as <- authority.score(g)$vector
df.as<-as.data.frame(as)

#plot(g,
#     vertex.size=hs*30,
#    main = 'Hubs',
#    vertex.color = rainbow(52),
#    edge.arrow.size=0.1,
#     layout = layout.kamada.kawai)



#plot(g,
#     vertex.size=as*30,
#     main = 'Authorities',
#     vertex.color = rainbow(52),
#     edge.arrow.size=0.1,
#    layout = layout.kamada.kawai)


tra<-transitivity(g)

#this metric calculates the proportion of closed triangles that a vertex is a part of
# out of theoretical numbers that could have been a part of.
tra_local<- transitivity(g,type="local")

triangles(g)


# How many triangles each vertex is a part of
count_triangles(g)

#
largest_cliques(g)

max_cliques(g)

#The below metrci measures if vertices with high degree also are connected to other high degree vertices or not
assortativity.degree(g,directed=F)


reciprocity(g)


#Let's visualize the complete network 
sentiment = factor(df_body$LINK_SENTIMENT)
ColorList = c("yellow", "darkblue")
V(g)$color = ColorList[sentiment]

graphjs(g , vertex.size = 0.3 , vertex.label = T , main = "Complete Network" , opacity = 1)
plot(g)

### Create a data frame for all of the centrality metrics and also degrees

centrality_degree_df<-data.frame(df.b,df.ec$vector,df.c,df.degree.out,df.degree.all,df.degree.in,df.as,df.hs)
# Make the row index a column of the dataset
centrality_degree_df$names <- rownames(centrality_degree_df)

colnames(centrality_degree_df) <- c("betweeness", "eigencentrality", "closeness", "degreeout",
                                    "degreeall", "degreein" , "authorityscore" ,"hubscore","verteces" )
head(centrality_degree_df)

# Transform the row index to 1,2....
rownames(centrality_degree_df) <- 1:nrow(centrality_degree_df)
 head(centrality_degree_df)

centrality_degree_df$betweeness <- as.numeric( centrality_degree_df$betweeness )
centrality_degree_df$closeness <- as.numeric( centrality_degree_df$closeness )
centrality_degree_df$eigencentrality <- as.numeric( centrality_degree_df$eigencentrality )
centrality_degree_df$degreeout <- as.numeric( centrality_degree_df$degreeout )
centrality_degree_df$degreein <- as.numeric( centrality_degree_df$degreein )
centrality_degree_df$degreeall <- as.numeric( centrality_degree_df$degreeall )
centrality_degree_df$authorityscore <- as.numeric( centrality_degree_df$authorityscore )
centrality_degree_df$hubscore <- as.numeric( centrality_degree_df$hubscore )


str(centrality_degree_df)
summary(centrality_degree_df)

df[which.max(centrality_degree_df$betweeness),]
df[which.max(centrality_degree_df$closeness),]
df[which.max(centrality_degree_df$eigencentrality),]
df[which.max(centrality_degree_df$degreeout),]
df[which.max(centrality_degree_df$degreein),]
df[which.max(centrality_degree_df$degreeall),]
df[which.max(centrality_degree_df$authorityscore),]
df[which.max(centrality_degree_df$hubscore),]


#Let's extract the askreddit subcommiunity

sub_co<-subcomponent(g, "askreddit" , "all")

graph <- induced.subgraph(g,sub_co)

# number of vertexes in the graph
V(graph)
# Number of edges in the graph
E(graph)


V(graph)$label <- V(graph)$name
V(graph)$degree <- degree(graph)
sentiment = factor(df_body$LINK_SENTIMENT)
ColorList = c("yellow", "darkblue")
V(graph)$color = ColorList[sentiment]

#Visualizing the subgraph
graphjs(graph, vertex.size = 0.5, main = "AskReddit Subcommunity Network")


#Diameter
diameter(graph, directed = T, weights = NA)
dia <- get_diameter(graph, weights = NULL)
dia

V(graph)$color[dia]<-"red"

# Let's visualize the diameter in our sub graph
graphjs(graph, vertex.size = 0.5,main = "AskReddit Subcommunity Network" )


##################### Now let's do the same analysis for title data frame
df_title <- read.csv("soc-redditHyperlinks-title.tsv", sep = "\t") 

df_title %>%
  separate(PROPERTIES , c( "Char_num" , "char_num_no_space", "alpha_char_frac" , "digits_frac" , "upper_frac" , "whitespace_frac",
                           "special_char_frac" , "word_cnt" , "unique_word_cnt" , "longword_cnt" , "avg_word_length" , "unique_stopword_cnt",
                           "sentences_cnt" , "longsentences_cnt" , "avg_char_persentence" , "avg_word_sentence" , "auto_readable_index",
                           "pos_sent_vad" , "neg_sent_vad" , "component_sent_vad", "LIWC_Funct",	"LIWC_Pronoun",	"LIWC_Ppron",	
                           "LIWC_I",	"LIWC_We",	"LIWC_You",	"LIWC_SheHe",	"LIWC_They",	
                           "LIWC_Ipron",	"LIWC_Article",	"LIWC_Verbs",	"LIWC_AuxVb",	
                           "LIWC_Past",	"LIWC_Present",	"LIWC_Future",	"LIWC_Adverbs",	"LIWC_Prep",	
                           "LIWC_Conj",	"LIWC_Negate",	
                           "LIWC_Quant",	"LIWC_Numbers",	"LIWC_Swear",	"LIWC_Social",	"LIWC_Family",	
                           "LIWC_Friends",	"LIWC_Humans",	"LIWC_Affect",	"LIWC_Posemo",	"LIWC_Negemo",	"LIWC_Anx",
                           "LIWC_Anger",	"LIWC_Sad",	"LIWC_CogMech",	"LIWC_Insight",	"LIWC_Cause",	"LIWC_Discrep",	
                           "LIWC_Tentat",	"LIWC_Certain",	"LIWC_Inhib",	"LIWC_Incl",	"LIWC_Excl",	"LIWC_Percept",	
                           "LIWC_See",	"LIWC_Hear",	"LIWC_Feel",	"LIWC_Bio",	"LIWC_Body",	"LIWC_Health",	
                           "LIWC_Sexual",	"LIWC_Ingest",	"LIWC_Relativ",	"LIWC_Motion",	"LIWC_Space",	"LIWC_Time",	
                           "LIWC_Work",	"LIWC_Achiev",	"LIWC_Leisure",	"LIWC_Home",	"LIWC_Money",	"LIWC_Relig",	
                           "LIWC_Death",	"LIWC_Assent",	"LIWC_Dissent",	"LIWC_Nonflu", "LIWC_Filler")) -> df_title
head(df_title)

library("writexl")
write_xlsx(df_title,"/Users/parsat8/Desktop/Winter Quarter/Customer & Social Analytics/Final Project Reddit/\\datavisualization_title.xlsx")

#Lets do some exploratory data analysis 

# Data types

str(df_title)
summary(df_title)
glimpse(df_title)

# any nulls values? No.

apply(df_title, 2, function(x) any(is.null(x)))

# Subsetting the data for creating a graph

graph_from_df2_main <- df_title[,1:2]

#Creating the graph
g2 <- graph_from_data_frame(graph_from_df2_main, directed = T)

# number of vertexes in the graph
V(g2)
# Number of edges in the graph
E(g2)

#Number of vertices: 54075
gorder(g2)
#Number of edges: 571927
gsize(g2)

#checking if the graph is directed
is.directed(g2)

#checking too see if the graph is weighted
is.weighted(g2)

head_of(g2,E(g2))


# Diameter ( longest path in the network)
get_diameter(g2)



### Let's find the important verteces

graph_degree_all2<-degree(g2,mode = c("all"))
max(graph_degree_all2)
graph_degree_all2[graph_degree_all2=="25425"]
#subredditdrama
df.degree.all2<-as.data.frame(graph_degree_all2)

graph_degree_in2<-degree(g2,mode = c("in"))
max(graph_degree_in2)
graph_degree_in2[graph_degree_in2=="19293"]
#askreddit
df.degree.in2<-as.data.frame(graph_degree_in2)

graph_degree_out2<-degree(g2,mode = c("out"))
max(graph_degree_out2)
graph_degree_out2[graph_degree_out2=="22971"]
#subredditdrama
df.degree.out2<-as.data.frame(graph_degree_out2)

# Find the vertices with max out degree

#which.max(graph_degree2)

#closeness: How quickly can a node interact with other nodes?

c2<-closeness(g2 , mode = c("all") , weights = NA)
df.c2<-as.data.frame(c2)

# Eigenvector (centrality proportional to the sum of connection centralities)
# Values of the first eigenvector of the graph adjacency matrix
ec2<-eigen_centrality(g2, directed=T, weights=NA)
df.ec2<-as.data.frame(ec2)

# Betweenness (centrality based on a broker position connecting others)
# (Number of geodesics that pass through the node or the edge)
b2<-betweenness(g2, directed=T, weights=NA)
df.b2<-as.data.frame(b2)

#The edge betweenness centrality is defined as the number of the shortest paths that go through an edge in a graph or network (Girvan and Newman 2002). 
#Each edge in the network can be associated with an edge betweenness centrality value. An edge with a high edge betweenness centrality score represents a bridge-like connector between two parts of a network, and the removal 
#of which may affect the communication between many pairs of nodes through the shortest paths between them.
#edge_betweenness(g2, directed=T, weights=NA)



#Measures of network structure

density2 <- edge_density(g2)
density2


# Average path length 
# The mean of the shortest distance between each pair of nodes in the network 
# (in both directions for directed graphs). 
mean_distance(g2, directed=F)
mean_distance(g2, directed=T)

# We can also find the length of all shortest paths in the graph:
#distances(g2) # with edge weights
#distances(g2, weights=NA) # ignore weights

# Hub and Authority Score
hs2 <- hub.score(g2)$vector
df.hs2<-as.data.frame(hs2)
as2 <- authority.score(g2)$vector
df.as2<-as.data.frame(as2)


### Create a data frame for all of the centrality metrics and also degrees

centrality_degree_df2<-data.frame(df.b2,df.ec2$vector,df.c2,df.degree.out2,df.degree.all2,df.degree.in2,df.as2,df.hs2)
# Make the row index a column of the dataset
centrality_degree_df2$names <- rownames(centrality_degree_df2)

colnames(centrality_degree_df2) <- c("betweeness", "eigencentrality", "closeness", "degreeout",
                                    "degreeall", "degreein" , "authorityscore" ,"hubscore","verteces" )
head(centrality_degree_df2)

# Transform the row index to 1,2....
rownames(centrality_degree_df2) <- 1:nrow(centrality_degree_df2)
head(centrality_degree_df2)

centrality_degree_df2$betweeness <- as.numeric( centrality_degree_df2$betweeness )
centrality_degree_df2$closeness <- as.numeric( centrality_degree_df2$closeness )
centrality_degree_df2$eigencentrality <- as.numeric( centrality_degree_df2$eigencentrality )
centrality_degree_df2$degreeout <- as.numeric( centrality_degree_df2$degreeout )
centrality_degree_df2$degreein <- as.numeric( centrality_degree_df2$degreein )
centrality_degree_df2$degreeall <- as.numeric( centrality_degree_df2$degreeall )
centrality_degree_df2$authorityscore <- as.numeric( centrality_degree_df2$authorityscore )
centrality_degree_df2$hubscore <- as.numeric( centrality_degree_df2$hubscore )


str(centrality_degree_df2)
summary(centrality_degree_df2)

centrality_degree_df2[which.max(centrality_degree_df2$betweeness),]
centrality_degree_df2[which.max(centrality_degree_df2$closeness),]
centrality_degree_df2[which.max(centrality_degree_df2$eigencentrality),]
centrality_degree_df2[which.max(centrality_degree_df2$degreeout),]
centrality_degree_df2[which.max(centrality_degree_df2$degreein),]
centrality_degree_df2[which.max(centrality_degree_df2$degreeall),]
centrality_degree_df2[which.max(centrality_degree_df2$authorityscore),]
centrality_degree_df2[which.max(centrality_degree_df2$hubscore),]

#Let's extract the askreddit

sub_co2<-subcomponent(g2, "subredditdrama" , "all")

graph2 <- induced.subgraph(g2,sub_co2)

# number of vertexes in the graph
V(graph2)
# Number of edges in the graph
E(graph2)


V(graph2)$label <- V(graph2)$name
V(graph2)$degree <- degree(graph2)
sentiment = factor(df_title$LINK_SENTIMENT)
ColorList = c("yellow", "darkblue")
V(graph2)$color = ColorList[sentiment]

#Visualizing the subgraph
G<-graphjs(graph2, vertex.size = 0.5, main = "SubredditDrama Subcommiunity Network")


#Diameter
diameter(graph2, directed = T, weights = NA)
dia2 <- get_diameter(graph2, weights = NULL)
dia



V(graph2)$color[dia]<-"red"

# Let's visualize the diameter in our sub graph
graphjs(graph2, vertex.size = 0.5, main = "SubredditDrama Subcommiunity Network" )




#plot(graph2,
#    vertex.color=rainbow(52),
#     vertex.size=V(graph2)$degree*0.4,
#     edge.arrow.size=0.1,
#     vertex.label.cex=0.8,
#     layout=layout.kamada.kawai)

### Community detection in complete network (body)

#un_sub_co2<-as.undirected(sub_co2 , mode = "each")

#x<-edge.betweenness.community(un_sub_co2)

#sizes(x)

#membership(x)

install.packages("networktools")
library(networktools)
bridge(graph2, communities = NULL, useCommunities = "all",
       directed = NULL, nodes = NULL, normalize = FALSE)



# We can also easily identify the immediate neighbors of a vertex, say WSJ.
# The 'neighbors' function finds all nodes one step out from the focal actor.
# To find the neighbors for multiple nodes, use 'adjacent_vertices()'.
V(g)$label <- V(g)$name
neigh.nodes.askreddit <- neighbors(g,"askreddit", mode="out")
neigh.nodes.circlebroke <-  neighbors(g,"circlebroke", mode="out")
neigh.nodes.circlejerkcopypasta <-  neighbors(g,"circlejerkcopypasta", mode="out")
neigh.nodes.funny <-  neighbors(g,"funny", mode="out")
neigh.nodes.news <-  neighbors(g,"news", mode="out")
neigh.nodes.subredditdrama <-  neighbors(g,"subredditdrama", mode="out")
neigh.nodes.writingprompts <-  neighbors(g,"writingprompts", mode="out")
neigh.nodes.iama <-  neighbors(g,"iama", mode="out")
#Let's Plot them
#Askreddit
vcol <- rep("gray40", vcount(g))
vcol[neigh.nodes.askreddit ] <- "#ff9d00"
graphjs(neigh.nodes.askreddit, vertex.color=vcol, main = "Askreddit Neighbors", vertex.size = 0.5)

#Circlebroke
vcol <- rep("gray40", vcount(g))
vcol[neigh.nodes.circlebroke ] <- "seagreen"
graphjs(g, vertex.color=vcol, main = "circlebroke Neighbors")

#circlejerkcopypasta
vcol <- rep("gray40", vcount(g))
vcol[neigh.nodes.circlejerkcopypasta] <- "slateblue1"
graphjs(g, vertex.color=vcol, main = "circlejerkcopypasta Neighbors")

#funny
vcol <- rep("gray40", vcount(g))
vcol[neigh.nodes.circlebroke ] <- "orchide64"
graphjs(g, vertex.color=vcol, main = "Funny Neighbors") 

#News
vcol <- rep("gray40", vcount(g))
vcol[neigh.nodes.news ] <- "orangered1"
graphjs(g, vertex.color=vcol, main = "News Neighbors") 

#Subredditdrama
vcol <- rep("gray40", vcount(g))
vcol[neigh.nodes.Subredditdrama ] <- "orangered4"
graphjs(g, vertex.color=vcol, main = "Subredditdrama Neighbors") 

#writingprompts
vcol <- rep("gray40", vcount(g))
vcol[neigh.nodes.writingprompts ] <- "salmon2"
graphjs(g, vertex.color=vcol, main = "Writingprompts Neighbors") 

#iama
vcol <- rep("gray40", vcount(g))
vcol[neigh.nodes.iama] <- "plum4"
graphjs(g, vertex.color=vcol, main = "iama Neighbors") 


