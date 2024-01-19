library(data.table)
library(igraph)
library(scales)

'fulldata3.csv'->ff
fread(ff)->info
info[,.N,URL][order(-N)]->urls


getSummary<-function(inter.url='https://www.altmetric.com/details/8375434'){
      info[URL==inter.url]->inter1
      ##############graph construction
      graph_from_data_frame(inter1[,.(from=Target,to=Source,weight)],TRUE)->gg
      
      
      ########################################nodes info
      as_data_frame(gg,'vertices')->nn
      setDT(nn)
      nn[,degree.in.centro:=centr_degree(gg,mode='in')$res]
      nn[,degree.out.centro:=centr_degree(gg,mode='out')$res] 
      nn[,degree.all.centro:=centr_degree(gg,mode='all')$res]
      nn[,betweenness.centro:=centr_betw(gg,directed=TRUE)$res]
      
      ####################
      lapply(nn$name,function(inter.node)
      {
      dfs(gg,root=inter.node, neimode='out',unreachable=F,dist=T)$dist->dd
      dd[!is.na(dd)]
      })->resu.dfs
      
      nn[,dfs.detail:=sapply(resu.dfs,function(x) paste(names(x),x,sep="=",collapse=";"))]
      nn[,dfs.max:=sapply(resu.dfs,function(x) max(x))]

      
      ####################
      lapply(nn$name,function(inter.node)
      {
      bfs(gg,root=inter.node, neimode='out',unreachable=F,dist=T)$dist->dd
      dd[!is.na(dd)]
      })->resu.bfs
      
      nn[,bfs.detail:=sapply(resu.bfs,function(x) paste(names(x),x,sep="=",collapse=";"))]
      nn[,bfs.max:=sapply(resu.bfs,function(x) max(x))]
      
      
      ########################edges info
      as_data_frame(gg,'edges')->ee
      setDT(ee)
      ee[,betweenness.edges:=edge_betweenness(gg)]
      
      
      ####################
      table(nn$bfs.max)/nrow(nn)->bfs.summary
      paste(names(bfs.summary),percent(as.numeric(bfs.summary),0.01),sep="_",collapse=";")->bfs.summary
      
      table(nn$dfs.max)/nrow(nn)->dfs.summary
      paste(names(dfs.summary),percent(as.numeric(dfs.summary),0.01),sep="_",collapse=";")->dfs.summary
      
      
      ######################distance between nodes pairs
      distances(gg,mode='out')->dd
      reshape2::melt(dd)->dd
      setDT(dd)
      dd[Var1!=Var2]->dd
      dd[is.finite(value)]->dd
	  
	  
	  ###clique/triangle
	  sapply(cliques(gg),length)->cliks
      #transitivity(gg)->trans
      
      summaryInfo<-data.table('URL'=inter.url,
                              'N.nodes'=vcount(gg),
                              #'N.edges'=ecount(gg),
							  'N.edges'=sum(ee$weight),
                              'diameter'=diameter(gg),
      		                'Max.degree.out.centro'=max(nn$degree.out.centro),
      		                'Max.betweenness.centro'=max(nn$betweenness.centro),
                              'Max.BFS'=max(nn$bfs.max),
                              'Max.DFS'=max(nn$dfs.max),
							  'Mean.BFS'=mean(nn$bfs.max),
							  'Mean.DFS'=mean(nn$dfs.max),
                              'BFS.distribute'=bfs.summary,
                              'DFS.distribute'=dfs.summary,
      		                'Number.of.pairsAcc'=NROW(dd),
      		                'Sum.shortest.Paths'=sum(dd$value),
      		                'Mean.shortest.Paths'=mean(dd$value),
							'cliques'=list(cliks),
							'Ntriangle'=sum(cliks==3),
							'density.Graph'=edge_density(gg),
							'density.undirectedGraph'=edge_density(as.undirected(gg))
      		                )
      


       
      return(list(graph=gg,nodes=nn,edges=ee,dist=dd,summ=summaryInfo))
}


lapply(urls$URL, function(x)
{
cat(x,'==\n')
getSummary(x)
})->resu


save(resu,info,file='totalResults.RData')

