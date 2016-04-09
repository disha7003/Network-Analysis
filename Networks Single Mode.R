############################################################
#               Single Mode Network Analysis               #
############################################################
rm (list = ls())

require("igraph")||install.packages("igraph")
library(igraph)


############################################################
#         Read Network Matrix and Create Adjacency Matrix
############################################################

connec.mat  = read.table(file.choose())  # Select cba_section_a_interaction_matrix.txt file

head(connec.mat)  # view first few rows

names.list1 = as.matrix(connec.mat[,1])  # first col is surveytakers' names

input.mat = connec.mat[, 2:ncol(connec.mat)]	# other cols are survey takers' connections

n = length(names.list1);	n

adj.mat = matrix(0, n, n)	# build adjacency matrix of dimn n x n

for (i1 in 1:n){
  
  a1 = match(as.matrix(input.mat[i1,]), names.list1)
  
  for (i2 in 1:length(a1)){
    
    adj.mat[i1, a1[i2]] = length(a1) +1 - i2	} # i2 ends
  
} # i1 loop ends

rownames(adj.mat) = names.list1
colnames(adj.mat) = names.list1

adj.mat[1:10,1:10]	# view part of adjacency matrix


############################################################
#         Plot Graph  and Calculate Network Matrices

graph <- graph.adjacency(adj.mat, mode = "directed",weighted=T)    # Create Network object
graph = simplify(graph)                                            # remove selfloops

wc = walktrap.community(graph)                                     # Walktrap algorithm Communities detection

table(wc$membership)                                               # Get Size of each community

###############################################################
# Plot Network and save the JPEG file in High resolution format
# To create high resolution image increase width, hight and quality accordingly  
# Modify parameters to plot your graph neatly

# mypath = paste("Network.jpeg")         
# jpeg(file = mypath, pointsize = 12,  width = 1000, height = 800, quality=200) 
# par(mai=c(0,0,0,0)) 			#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
windows()
plot( wc , graph,			#the graph to be plotted
      layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
      #main='Title of the graph',	#specifies the title
      #vertex.label.dist=0.5,			#puts the name labels slightly off the dots
      vertex.frame.color='blue', 	#the color of the border of the dots 
      vertex.label.color='black',	#the color of the name labels
      #vertex.label.font=1,			  #the font of the name labels
      vertex.size = 5,
#       edge.width = 1,
      edge.arrow.size=0.1,
      vertex.label=V(graph)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
      vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary
      
)
# dev.off()


##################### plot 1st community #################

test = adj.mat[match(wc$names[wc$membership == 1],rownames(adj.mat)),match(wc$names[wc$membership == 1],rownames(adj.mat))]  # Adjacency Matrix for first community
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
metrics.g = metrics.g[(order(metrics.g[,1],metrics.g[,2],metrics.g[,3],metrics.g[,4],metrics.g[,5],metrics.g[,6],metrics.g[,7], decreasing= T)),]  # Sort by degree etc.
head(metrics.g)


