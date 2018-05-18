## Clear workspace
rm(list=ls())
setwd(system("pwd", intern = T) )
#-------0. Packages---------

## Download and install the package
#install.packages("igraph")

## Load package
library(igraph)
library(ggplot2)
library(sand)
library(ergm)
#library(network)
#library(sna)

#-------I. Import Data---------
#from https://linkedjazz.org/api/relationships/all/nt
jazz_music_network=read.csv("data/clean_triples_network.csv")

#as graph object
jazz_graph=graph(as.character(jazz_music_network[,2]), directed=F)

#remove multiple
jazz_graph=simplify(jazz_graph,remove.loops=T,remove.multiple=T)

#------II. Descriptive Analysis --------

#DESCRIBE THE NETWORK
length(E(jazz_graph))
length(V(jazz_graph))
set.seed(123)
plot(jazz_graph, vertex.label=NA, vertex.size=5,vertex.color="lightblue")

#DECOMPOSE THE GRAPH 
table(sapply(decompose.graph(jazz_graph),vcount))
#Only one graph with all components


#DEGREE

#Histogram
degree_jazz_graph=data.frame(degree(jazz_graph))
ggplot(data=degree_jazz_graph, aes(degree_jazz_graph$degree.jazz_graph.)) + 
  geom_histogram(breaks=seq(0, 100, by = 1), 
                 col="blue", 
                 fill="lightblue", 
                 alpha = .2)+ 
  labs(title="Histogram for Degree Distribution") +
  labs(x="Degree", y="Count")

#Average degree
mean(degree_jazz_graph$degree.jazz_graph.)
sd(degree_jazz_graph$degree.jazz_graph.)

#Scatter plot
degree_distrib <- degree.distribution(jazz_graph)
d <- 1:max(degree_jazz_graph)-1
ind <- (degree_distrib != 0)
plot(d[ind], degree_distrib[ind], log="xy", col="blue",
     xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution")
#linear relation between frequency and degree 

#who are the most connected
most_connected_music=degree(jazz_graph)[order(-degree(jazz_graph))]
names(most_connected_music[1:10])

#graph of 10 most connected within all network

most_connected_graph = subgraph.edges(jazz_graph,
                                     E(jazz_graph)[from(names(most_connected_music[1:10]))])
plot(most_connected_graph,vertex.label=NA)
V(jazz_graph)$size=3
V(jazz_graph)$color="lightblue"
V(jazz_graph)[names(V(most_connected_graph))]$color="orange"
V(jazz_graph)[names(most_connected_music[1:10])]$color="red"
V(jazz_graph)[names(most_connected_music[1:10])]$size=8
set.seed(123)
plot(jazz_graph,vertex.label=NA)



#DENSITY
graph.density(jazz_graph,loop=FALSE)

#DIAMETER

#The value of the longest distance in a graph is called the diameter of the graph.
diameter(jazz_graph)
#it is only 5
#who are they?
get_diameter(jazz_graph)

#average path length
average.path.length(jazz_graph)
#it is about 3

#CUT VERTICES

#If the removal of a particular set of vertices (edges) in a graph disconnects the
#graph, that set is called a vertex-cut (edge-cut).
cut.vertices <- articulation.points(jazz_graph)
length(cut.vertices)
#they are only 44

#CENTRALITY

#Closeness centrality
#Cloness centrality measures how many steps is required to access every other vertex from a given vertex.
#loseness centrality measures attempt to capture the notion that a vertex is 'central'
#if it is 'close' to many other vertices.
closeness_jazz_graph=closeness(jazz_graph,mode="in")
hist(closeness_jazz_graph)

#Most influential jazz musician
most_influential_music=closeness_jazz_graph[order(-closeness_jazz_graph)]
#10 most influential
names(most_influential_music[1:10])


#graph of most influential artists

#graph of 10 most connected within all network
V(jazz_graph)$color="lightblue"
V(jazz_graph)[names(most_influential_music[1:10])]$color="red"
V(jazz_graph)$size=3
V(jazz_graph)[names(most_influential_music[1:10])]$size=7
set.seed(123)
plot(jazz_graph,vertex.label=NA)

#graph of 5 most connected with their network
most_influent_graph = subgraph.edges(jazz_graph,
                                     E(jazz_graph)[from(names(most_influential_music[1:10]))])
plot(most_influent_graph,vertex.label=NA)

#graph of all network highlighting network of 5 most connected
V(jazz_graph)$size=3
V(jazz_graph)$color="lightblue"
V(jazz_graph)[names(V(most_influent_graph))]$color="orange"
V(jazz_graph)[names(most_influential_music[1:10])]$color="red"
V(jazz_graph)[names(most_influential_music[1:10])]$size=8
set.seed(123)
plot(jazz_graph,vertex.label=NA)

#graph of 10 most influential only
most_influent_graph <- induced.subgraph(jazz_graph, 
                                        V(jazz_graph)[names(most_influential_music[1:10])])
plot(most_influent_graph,vertex.color="lightblue")

graph_influent=make_ego_graph(jazz_graph, order = 1, nodes = V(jazz_graph)[names(most_influential_music[1:10])],
                              mode = c("all", "out","in"), mindist = 0)

#Most influential 1
V(graph_influent[[1]])$color="lightblue"
V(graph_influent[[1]])[names(most_influential_music[1])]$color="red"
V(graph_influent[[1]])[names(V(graph_influent[[1]])) %in% names(most_influential_music[2:10])]$color="orange"
plot(graph_influent[[1]], vertex.label.cex=1,vertex.label.font=2, vertex.size=8)

#Most influential 2
V(graph_influent[[2]])$color="blue"
V(graph_influent[[2]])[names(most_influential_music[2])]$color="red"
V(graph_influent[[2]])[names(V(graph_influent[[2]])) %in% names(most_influential_music[c(1,3:10)])]$color="orange"
plot(graph_influent[[2]], vertex.label.cex=0.5,vertex.label.font=2)

#Most influential 3
V(graph_influent[[3]])$color="blue"
V(graph_influent[[3]])[names(most_influential_music[3])]$color="red"
V(graph_influent[[3]])[names(V(graph_influent[[3]])) %in% names(most_influential_music[c(1:2,4:10)])]$color="orange"
plot(graph_influent[[3]], vertex.label.cex=0.5,vertex.label.font=2)

#Most influential 4
V(graph_influent[[4]])$color="blue"
V(graph_influent[[4]])[names(most_influential_music[4])]$color="red"
V(graph_influent[[4]])[names(V(graph_influent[[4]])) %in% names(most_influential_music[c(1:3,5:10)])]$color="orange"
plot(graph_influent[[4]], vertex.label.cex=0.5,vertex.label.font=2)

#Most influential 5
V(graph_influent[[5]])$color="blue"
V(graph_influent[[5]])[names(most_influential_music[5])]$color="red"
V(graph_influent[[5]])[names(V(graph_influent[[5]])) %in% names(most_influential_music[c(1:4,6:10)])]$color="orange"
plot(graph_influent[[5]], vertex.label.cex=0.5,vertex.label.font=2)



#Betweenness centrality measures are aimed at summarizing the extent to which a
#vertex is located 'between' other pairs of vertices

#DENSITY

#The density of a graph is the ratio of the number of edges and the number of possible edges. 
edge_density(jazz_graph, loops = FALSE)

#average degree
mean(degree_jazz_graph$degree.jazz_graph.)
#in average almost 6 connections

#median degree
median(degree_jazz_graph$degree.jazz_graph.)


#TRANSITIVITY (or clustering coefficient)

#Transitivity measures the probability that the adjacent vertices of a vertex 
#are connected. This is sometimes also called the clustering coefficient. 
transitivity(jazz_graph, type=list("globalundirected"))

#CLIQUES

table(sapply(cliques(jazz_graph), length))





#CLUSTERING

cluster <- fastgreedy.community(jazz_graph)
set.seed(123)
plot(cluster,jazz_graph, vertex.label=NA, vertex.size=5)
sizes(cluster)

#finds 11 clusters
#gives membership : membership(cluster)
#descriptive statistics within the clusters


#------II. Descriptive Analysis With Attributes --------

#Import attributes
attribut = read.csv("data/metadata_dataframe_clean.csv",header = T)
rownames(attribut)=as.character(attribut$Name)
# Add instruments to graph 
# Place
V(jazz_graph)$area=as.character(attribut[names(V(jazz_graph)),"Area"])
V(jazz_graph)$area[V(jazz_graph)$area==""]=NA
#Style
V(jazz_graph)$style=as.character(attribut[names(V(jazz_graph)),"Main_style"])
V(jazz_graph)$style[V(jazz_graph)$style==""]=NA
#Instrument
V(jazz_graph)$instrument=as.character(attribut[names(V(jazz_graph)),"Main_instrument"])
V(jazz_graph)$instrument[V(jazz_graph)$instrument==""]=NA
#Birth decade
V(jazz_graph)$birth_decade=as.character(attribut[names(V(jazz_graph)),"Birth_decade"])
V(jazz_graph)$birth_decade[V(jazz_graph)$birth_decade==""]=NA

#Calculate percentage for each instrument
#Place
area_prop=data.frame(prop.table(table(V(jazz_graph)$area)))
rownames(area_prop)=area_prop$Var1

most_common_area=data.frame(paste(area_prop[order(-area_prop[,2]),][1:3,1]," (",
                                  round(area_prop[order(-area_prop[,2]),][1:3,2],3)*100,"%)",sep=""))
rownames(most_common_area)=c("Area 1","Area 2","Area 3")
colnames(most_common_area)=c("Total")

for (i in 1:length(sizes(cluster))){
  cluster_number=paste("cluster",i,sep="")
  table_prop=data.frame(prop.table(table(V(jazz_graph)[membership(cluster)==i]$area)))
  table_prop=table_prop[order(-table_prop[,2]),]
  area_prop[[cluster_number]]=rep(NA,7)
  area_prop[as.character(table_prop$Var1),][[cluster_number]]=table_prop$Freq
  
  most_common_area=data.frame(most_common_area,paste(table_prop$Var1[1:3]," (",round(table_prop$Freq[1:3],3)*100,"%)",sep=""))
  colnames(most_common_area)[i+1]=c(cluster_number)
}
write.csv(area_prop,"data/area_prop.csv")

#Style
style_prop=data.frame(prop.table(table(V(jazz_graph)$style)))
rownames(style_prop)=style_prop$Var1

most_common_style=data.frame(paste(style_prop[order(-style_prop[,2]),][1:3,1]," (",
                                   round(style_prop[order(-style_prop[,2]),][1:3,2],3)*100,"%)",sep=""))
rownames(most_common_style)=c("Style 1","Style 2","Style 3")
colnames(most_common_style)=c("Total")

for (i in 1:length(sizes(cluster))){
  cluster_number=paste("cluster",i,sep="")
  table_prop=data.frame(prop.table(table(V(jazz_graph)[membership(cluster)==i]$style)))
  table_prop=table_prop[order(-table_prop[,2]),]
  style_prop[[cluster_number]]=rep(NA,length(style_prop$Var1))
  style_prop[as.character(table_prop$Var1),][[cluster_number]]=table_prop$Freq
  
  most_common_style=data.frame(most_common_style,paste(table_prop$Var1[1:3]," (",round(table_prop$Freq[1:3],3)*100,"%)",sep=""))
  colnames(most_common_style)[i+1]=cluster_number
}
write.csv(style_prop,"data/style_prop.csv")

#Instrument
instrument_prop=data.frame(prop.table(table(V(jazz_graph)$instrument)))
rownames(instrument_prop)=instrument_prop$Var1

most_common_instrument=data.frame(paste(instrument_prop[order(-instrument_prop[,2]),][1:3,1]," (",
                                        round(instrument_prop[order(-instrument_prop[,2]),][1:3,2],3)*100,"%)",sep=""))
rownames(most_common_instrument)=c("Instrument 1","Instrument 2","Instrument 3")
colnames(most_common_instrument)=c("Total")

for (i in 1:length(sizes(cluster))){
  cluster_number=paste("cluster",i,sep="")
  table_prop=data.frame(prop.table(table(V(jazz_graph)[membership(cluster)==i]$instrument)))
  table_prop=table_prop[order(-table_prop[,2]),]
  instrument_prop[[cluster_number]]=rep(NA,length(instrument_prop$Var1))
  instrument_prop[as.character(table_prop$Var1),][[cluster_number]]=table_prop$Freq
  
  most_common_instrument=data.frame(most_common_instrument,paste(table_prop$Var1[1:3]," (",round(table_prop$Freq[1:3],3)*100,"%)",sep=""))
  colnames(most_common_instrument)[i+1]=cluster_number
}
write.csv(instrument_prop,"data/instrument_prop.csv")

#Birth decade
birth_decade_prop=data.frame(prop.table(table(V(jazz_graph)$birth_decade)))
rownames(birth_decade_prop)=birth_decade_prop$Var1

most_common_birth_decade=data.frame(paste(birth_decade_prop[order(-birth_decade_prop[,2]),][1:3,1]," (",
                                          round(birth_decade_prop[order(-birth_decade_prop[,2]),][1:3,2],3)*100,"%)",sep=""))
rownames(most_common_birth_decade)=c("Birth Decade 1","Birth Decade 2","Birth Decade 3")
colnames(most_common_birth_decade)=c("Total")

area=NULL
for (i in 1:length(sizes(cluster))){
  cluster_number=paste("cluster",i,sep="")
  table_prop=data.frame(prop.table(table(V(jazz_graph)[membership(cluster)==i]$birth_decade)))
  table_prop=table_prop[order(-table_prop[,2]),]
  birth_decade_prop[[cluster_number]]=rep(NA,length(birth_decade_prop$Var1))
  birth_decade_prop[as.character(table_prop$Var1),][[cluster_number]]=table_prop$Freq
  
  most_common_birth_decade=data.frame(most_common_birth_decade,paste(table_prop$Var1[1:3]," (",round(table_prop$Freq[1:3],3)*100,"%)",sep=""))
  colnames(most_common_birth_decade)[i+1]=cluster_number
}
write.csv(birth_decade_prop,"data/birth_decade_prop.csv")

write.csv(rbind(most_common_area,most_common_style,most_common_birth_decade,
                most_common_instrument),"data/most_common_attributes.csv")

#-----III. Inference -------

#-----III.1 ASSORTATIVITY AND MIXING ====

#Selective linking among vertices, according to a certain characteristic(s), is termed
#assortative mixing in the social network literature. Measures that quantify the extent
#of assortative mixing in a given network have been referred to as assortativity
#coefficients, and are essentially variations on the concept of correlation coefficients.

#it is equal to one when there is perfect assortativemixing
#(i.e., when edges only connect vertices of the same category).
assortativity.nominal(jazz_graph, (V(jazz_graph)$instrument=="Piano")+1,
                      directed=FALSE)
assortativity.nominal(jazz_graph, (V(jazz_graph)$area=="New-Orleans")+1,
                      directed=FALSE)


assortativity.nominal(jazz_graph, (V(jazz_graph)$birth_decade=="1920s")+1,
                      directed=FALSE)



assortativity.degree(jazz_graph)

instrument_corr=NULL
for (i in as.character(data.frame(table(V(jazz_graph)$instrument))$Var1)){
  instrument_corr[[i]]=assortativity.nominal(jazz_graph, (V(jazz_graph)$instrument==i)+1,
                                             directed=FALSE)
}

area_corr=NULL
for (i in as.character(data.frame(table(V(jazz_graph)$area))$Var1)){
  area_corr[[i]]=assortativity.nominal(jazz_graph, (V(jazz_graph)$area==i)+1,
                                       directed=FALSE)
}

style_corr=NULL
for (i in as.character(data.frame(table(V(jazz_graph)$style))$Var1)){
  style_corr[[i]]=assortativity.nominal(jazz_graph, (V(jazz_graph)$style==i)+1,
                                        directed=FALSE)
}

birth_decade_corr=NULL
for (i in as.character(data.frame(table(V(jazz_graph)$birth_decade))$Var1)){
  birth_decade_corr[[i]]=assortativity.nominal(jazz_graph, (V(jazz_graph)$birth_decade==i)+1,
                                               directed=FALSE)
}


#-----III.2 KNN TO PREDICT MISSING LABELS ====

V(jazz_graph)$instrument[V(jazz_graph)$instrument=="Other"]=NA
V(jazz_graph)$style[V(jazz_graph)$style=="Jazz"]=NA

get_most_common_attr <- function(n, type){
  true_instrument = vertex_attr(jazz_graph, type, index = n)
  l= c()
  for (i in ego(jazz_graph, 2, n)){
    l = c(l, vertex_attr(jazz_graph, type, index = i))
  }
  predicted_instrument = names(which.max(table(l)))

  if (is.na(true_instrument)){ return(c(0, 0)) }
  else if (is.null(predicted_instrument)){ return(c(0, 0)) }
  else if (true_instrument == predicted_instrument){ return(c(1, 1))}
  else if (true_instrument != predicted_instrument){return(c(0, 1))}
  }

get_accuracy <- function(type){
  total_non_na = 0
  total_well_predicted = 0
  for (i in 1:length(V(jazz_graph))){
    total_non_na = total_non_na + get_most_common_attr(i, type)[2]
    total_well_predicted = total_well_predicted + get_most_common_attr(i, type)[1]
  }
  return(total_well_predicted/total_non_na)
}


