###################################################
### chunk number 1: Setup
###################################################
library("RBGL")
library("Biobase")
require("Rgraphviz", quietly=TRUE)
library("RColorBrewer")
library("RbcBook1")
library("yeastExpData")
data(ccyclered)

library("annotate")
library("GOstats")
library("xtable")
library("multtest")
library("hgu95av2")
library("genefilter")


##################################################
### chunk number 2: makeClusterGraph
###################################################

clusts <-  split(ccyclered[["Y.name"]],
                 ccyclered[["Cluster"]])

cg1 <- new("clusterGraph", clusters = clusts)

ccClust <- connectedComp(cg1)


###################################################
### chunk number 3: litGraph
###################################################
 data(litG)
 ccLit  <- connectedComp(litG)
 cclens <- listLen(ccLit)
 #table(cclens)


###################################################
### chunk number 4: litGraph.undercover
###################################################
ord <- order(cclens, decreasing=TRUE)
nrSingletons <- table(cclens)["1"]

ord <- order(cclens, decreasing=TRUE)
sG1 <- subGraph(ccLit[[ord[1]]], litG)
sG2 <- subGraph(ccLit[[ord[2]]], litG)

plot(sG2)

nodePerm <- function (g1, g2, B=1000) {
 n1 <- nodes(g1)
 sapply(1:B, function(i) {
    nodes(g1) <- sample(n1)
    numEdges(intersection(g1, g2))
  })
}
set.seed(123)

data(nPdist)



###################################################
### chunk number 15: Example
###################################################
## subset of interest: 37+42 samples
require(ALL)
data(ALL)
eset <- ALL[, intersect(grep("^B", as.character(ALL$BT)),
          which(as.character(ALL$mol) %in% c("BCR/ABL","NEG")))]

## intensities above 100 in at least 25% of the samples
f1 <- pOverA(.25, log2(100))
f2 <- function(x)(IQR(x)>0.5)
ff <- filterfun(f1, f2)


###################################################
### chunk number 34: pathstats
###################################################
library("hgu95av2")
library("annotate")
genel <- unlist(eapply(hgu95av2PATH, length))
#table(genel)


###################################################
### chunk number 35: uniqLL
###################################################
pathLL <- eapply(hgu95av2PATH2PROBE, function(x) {
    LLs <- getLL(x, "hgu95av2")
    unique(LLs) })

pLens <- sapply(pathLL, length)
range(pLens)

uniqLL <- unique(unlist(pathLL,use.names=FALSE))


###################################################
### chunk number 36: LLpathwBG
###################################################

Amat <- sapply(pathLL, function(x) {
    mtch <- match(x, uniqLL)
    zeros <- rep(0, length(uniqLL))
    zeros[mtch] <- 1
    zeros})


###################################################
### chunk number 37: pathwayGraph
###################################################

pwGmat <- t(Amat) %*% Amat
diag(pwGmat) <- 0
pwG <- as(pwGmat, "graphNEL")

###################################################
### chunk number 38: connComppwG
###################################################
 ccpwG <- connectedComp(pwG)
 sapply(ccpwG, length)



###################################################
### chunk number 40: pwNames
###################################################
library("KEGG")
for(i in ccpwG) {
   if(length(i) == 1)
    cat(get(i, KEGGPATHID2NAME), "\n")
    }


###################################################
### chunk number 42: setup
###################################################

data(integrinMediatedCellAdhesion)

##nodes that do not correspond to genes have no LocusLink ID
llLens <- sapply(IMCAAttrs$LocusLink, length)
numLL <- sum(llLens > 0)

#hsa04510 <- hgu95av2PATH2PROBE$"04510"
hsapathgenes <- hgu95av2PATH2PROBE$"00626"
hsaLLs <- getLL(hsapathgenes, "hgu95av2")
LLs <- unlist(sapply(IMCAAttrs$LocusLink, function(x) x[1]))
whProbe <- match(LLs, hsaLLs)
probeNames <- names(hsaLLs)[whProbe]
names(probeNames)<-names(LLs)
pN <- probeNames[!is.na(probeNames)]

###################################################
### chunk number 45: imca.do1
###################################################
nodeA <- makeNodeAttrs(IMCAGraph, width=1, height=.75,
           label=sub("^cell ", "", nodes(IMCAGraph)),
           shape="ellipse")

## the big node in the middle
j <- which( names(nodeA$label) ==  "Phosphatidylinositol signaling system" )
nodeA$label[j]  <- "Phosphatidyl-"
nodeA$shape[j]  <- "ellipse"
nodeA$width[j]  <- 4
nodeA$height[j] <- 2
nodeA$fillcolor[j] <- "white"

## some of the longer names
nc  <- nchar(nodeA$label)
sel <- (nc>4) & (seq(along=nc)!=j)
nodeA$width[sel]  <- nc[sel]/4

## overall attributes
attrs <- IMCAAttrs$defAttrs
attrs$graph$nodesep <- "0.1"
attrs$graph$ranksep <- "0.3"
attrs$graph$margin <- "0"

## all other nodes
nodeA$fillcolor[IMCAAttrs$nodeAttrs$fillcolor=="green"] <- "gray"#brewer.pal(9, "YlGn")[3]
nodeA$fillcolor[!IMCAAttrs$nodeAttrs$fillcolor=="red"] <- "gray"#brewer.pal(9, "YlGn")[3]
hitnodes <- c("SRC","FAK","BCAR1","CAPNS","CSK","p110","p85","CAPN")
nodeA$fillcolor[hitnodes] <- "red"
#nodeA$fillcolor[-hitnodes] <- "gray"

IMCg <- agopen(IMCAGraph, "", attrs=attrs,
     nodeAttrs=nodeA, subGList=IMCAAttrs$subGList)

IMCg@AgNode[[j]]@txtLabel@labelText <- ""

png("PPInet.png")
plot(IMCg)
text( getX(getNodeCenter( AgNode(IMCg)[[j]])),
      getY(getNodeCenter( AgNode(IMCg)[[j]])) - seq(-2,2,length=4)*strheight("P", cex=.4),
      c("Phosphatidyl-", "inositol", "signaling", "system"), adj=c(0.5,1), cex=.4)
dev.off()

