library(sbm)
data('war')
library(igraph)

A = as.matrix(get.adjacency(war$alliance))
A = A[1:83,1:83]
B = as.matrix(get.adjacency(war$belligerent))

netA = defineSBM(A,model="bernoulli",dimLabels = "country") 
netB = defineSBM(B,model="bernoulli",dimLabels = "country")
plotMyMultiplexMatrix(list(netA,netB))

MultiplexFitIndep = estimateMultiplexSBM(list(netA,netB), dependent = FALSE)
plot(MultiplexFitIndep)
plot(MultiplexFitIndep,type="expected")