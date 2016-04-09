############################################################
#               Facebook Network Analysis                  #
############################################################

rm (list = ls())

require("Rfacebook")||install.packages("Rfacebook")
require("Rook")||install.packages("Rook")
require("qgraph")||install.packages("qgraph")
require("igraph")||install.packages("igraph")

library("Rfacebook")
library("Rook")
library("qgraph")
library("igraph")

# Please change the token before running this code

token = "CAACEdEose0cBAF1syJkWqw5H485VuzvXyfwgjQetpvHLUQZC6ZAxw2miBwQci3CpxhYhre4bl98kCUtnlK2UDNc6Ds0N88SWZBLLdMXLMoLmyjuADomykQvdnM4zA9sQNNmiszHr5yNo0LYZC9QTtooa0fjm4l3zFnAtn7e1oYLSr68ZCh8NfWfNYeDYFzV2DMRb5zfSAbGmaxpYt5dRWYrMjBSiSCy4ZD"

#############################################################
#         Read Network Matrix and Create Adjacency Matrix   #
#############################################################

user = getUsers("me",token=token)

mat = getNetwork(token, format="adj.matrix")

filename = paste(user$name)

############################################################
#         Plot Graph  and Calculate Network Matrices

graph <- graph.adjacency(mat, mode = "undirected",weighted=T) # Create Graph Object

graph = simplify(graph)   # remove selfloops

col.names <- make.names(V(graph)$name, unique = TRUE)   # lable vertices

wc = walktrap.community(graph)                                     # Walktrap algorithm Communities detection

table(wc$membership)                                               # Get Size of each community

###############################################################
# Plot Network and save the JPEG file in High resolution format
# To create high resolution image increase width, hight and quality accordingly  
# Modify parameters to plot your graph neatly
# This high resilution image will be stored in your working directory (Mostly My Documents folder) 
# To check your working directory use -- getwd()

mypath = paste(filename,"_friends_by_name.jpeg")
jpeg(file = mypath, pointsize = 12,  width = 6500, height = 6500, quality=2000) 
par(mai=c(0,0,0,0))     	#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
plot( wc , graph,			#the graph to be plotted
      layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
      #main='Organizational network example',	#specifies the title
      #vertex.label.dist=0.5,			#puts the name labels slightly off the dots
      vertex.frame.color='blue', 		#the color of the border of the dots 
      vertex.label.color='black',		#the color of the name labels
      vertex.label.font=1,			#the font of the name labels
      vertex.size = 2,
      vertex.label=col.names,		#specifies the lables of the vertices. in this case the 'name' attribute is used
      vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary
)
dev.off()


##################### plot 1st community #################

test = mat[match(wc$names[wc$membership == 1],rownames(mat)),match(wc$names[wc$membership == 1],rownames(mat))]  # Adjacency Matrix for first community
g = graph.adjacency(test, mode=c("directed"), weighted=T)        # Create graph object
g = simplify(g)                                                  # Remove Loops    
pct = round(length(wc$names[wc$membership == 1])/length(wc$names)*100,2)  # Percent of population

windows()
plot(  g ,			#the graph to be plotted
       layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
       vertex.frame.color='blue', 		#the color of the border of the dots 
       vertex.label.color='black',		#the color of the name labels
       vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary
)
title(paste("Community : ",1), sub = paste("Population share - ",pct,"%"))


########################################################
#  Calculate centarality metrices and other properties
########################################################

# transitivity in Network :
(transitivity.graph <- transitivity(graph, type = "localaverage",isolates = "zero"))

# Average path length in Network :
(averagePathLength.graph <- average.path.length(graph))

# Density in Network :
(density.graph = graph.density(graph, loops=FALSE))

# Total Communities in Network :
(size = length(wc))

# Size of communities in Network:
table(wc$members)

# Centrality Metrices in Network
metrics <- data.frame(deg=degree(graph), out.deg =degree(graph, v=V(graph), mode=c("out")),in.deg =degree(graph, v=V(graph), mode=c("in")), bet=betweenness(graph), clo = closeness(graph), eigraph = evcent(graph)$vector, cor = graph.coreness(graph))
rownames(metrics) = col.names   # Assign row names
metrics = metrics[(order(metrics[,1],metrics[,2],metrics[,3],metrics[,4],metrics[,5],metrics[,6],metrics[,7], decreasing= T)),]  # Sort by degree etc.
head(metrics)


############## Subnetwork properties  #################
# transitivity in Subnetwork :
(transitivity.g <- transitivity(g, type = "localaverage",isolates = "zero"))

# Average path length in Subnetwork :
(averagePathLength.g <- average.path.length(g))

# Density in Subnetwork :
(density.g = graph.density(g, loops=FALSE))

# Centrality Metrices in Subnetwork
metrics.g <- data.frame(deg=degree(g), out.deg =degree(g, v=V(g), mode=c("out")),in.deg =degree(g, v=V(g), mode=c("in")), bet=betweenness(g), clo = closeness(g), eigraph = evcent(g)$vector, cor = graph.coreness(g))
rownames(metrics.g) = make.names(V(g)$name, unique = TRUE) # Assign row names
metrics.g = metrics.g[(order(metrics.g[,1],metrics.g[,2],metrics.g[,3],metrics.g[,4],metrics.g[,5],metrics.g[,6],metrics.g[,7], decreasing= T)),]  # Sort by degree etc.
head(metrics.g)

