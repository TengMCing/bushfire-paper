# Clustering Algorithm

## V1.0

Demo: VIC_from_Oct.Rmd

For first hour data:

	1. connect every pair of points that close to each other (within 3km)
	2. build a graph based on the adjacency matrix
	3. give a fire_id to each component in the graph
	4. compute the centroid for each hotspots group
	5. set the 'active' variable to 0 for each hotspots group

For 2-n hour data:

	1. minus one from 'active' variable for each hotspots group
	2. for each hotspots group, if its 'active' variable is greater than -24, it will be consider as a active group
	3. combine points with the active groups' centroid
	3. connect every pair of points that close to each other (within 3km)
	4. build a graph based on the adjacency matrix
	5. if any point that share a same membership as active groups' centroid, it will be assigned a previous fire_id
	6. otherwise, it will be given a new fire_id
	7. update the hotspots groups' centroid using only the points in this iteration
	8. reset the hotspots groups' active variable to zero if they appear in this iteration

Advantage:
	
1. Only trace the centroid for each hotspots group
2. Only compute the distance matrix for small size of data in each iteration
3. Using a single threshold (3km) to determine the edges between points, which can be a potential hyperparameter to control the total number of fire_id

Disadvantage:

1. If the fire getting larger, this algorithm will label the hotspots inside one fire area with different fire_id. An algorithm for merging clusters is needed to combine the fire that close to each other and overlap on timeline.