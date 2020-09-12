##################################################################################################################
#MUHAMMAD AHSAN ASIF
#218606833

#QUESTION 1

#1.5

library(igraph) 
library(ggm)

dag <- DAG(M ~ H, W ~ H, N ~ W + H, R ~ H + N, S ~ N)
plotGraph(dag, nodesize=20, tcltk=FALSE, vc="white")
dSep(dag, first="M", second="S", cond=NULL)
dSep(dag, first="W", second="S", cond=c("N", "H"))
dSep(dag, first=c("R","S"), second="W", cond="H")



##################################################################################################################

#QUESTION 2 

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.11")
BiocManager::install(c("gRain", "RBGL", "gRbase")) 
BiocManager::install(c("Rgraphviz")) 

library("Rgraphviz") 
library(RBGL)
library(gRbase)
library(gRain)
library(ggedit)

# Conditional Probability Tabels
neta <- cptable(~season, values=c(0.3,0.7),levels=c("wet","dry"))
netb <- cptable(~riverflowrate, values=c(0.8,0.2),levels=c("low","high"))
netc.ab <- cptable(~species|season:riverflowrate, values=c(0.6, 0.4, 0.3, 0.7, 0.4, 0.6, 0.5, 0.5), levels=c("bass","cod"))
netd.c <- cptable(~colour|species, values=c(0.2, 0.4, 0.4, 0.5, 0.3, 0.2), levels=c("light","medium","dark"))
nete.c <- cptable(~size|species, values=c(0.6, 0.4, 0.4, 0.6), levels=c("wide","thin"))

#a)

plist <- compileCPT(list(neta,netb,netc.ab,netd.c,nete.c))
plist

#Plotting the belief network

net1 <- grain(plist)
plot(net1$dag)

#b)

plist$season
plist$riverflowrate
plist$species
plist$colour
plist$size

#Getting marginal probability of all nodes 

querygrain(net1)


b)

# a) P(riverflowlate = low, size = thin)
net1WithEvidence_A <- setEvidence(net1, evidence=list(riverflowrate="low"))
querygrain(net1WithEvidence_A, nodes=c("size"))


# b) P(species=cod,season=dry, color=dark)
net1WithEvidence_B <- setEvidence(net1, evidence=list(season="dry",colour="dark"))
querygrain(net1WithEvidence_B, nodes=c("species"))

# c) Joint probability distribution of colour and fish
querygrain(net1,nodes=c("colour","species"), type="joint")

# d) Marginal distribution of fish species
querygrain(net1, nodes=c("species"), type="marginal")

##################################################################################################################


#Q.4

library (bnlearn) 
data(hailfinder) 
summary(hailfinder)

# create and plot the network structure.
modelstring = paste0("[N07muVerMo][SubjVertMo][QGVertMotion][SatContMoist][RaoContMoist]","[VISCloudCov][IRCloudCover][AMInstabMt][WndHodograph][MorningBound][LoLevMoistAd][Date]","[MorningCIN][LIfr12ZDENSd][AMDewptCalPl][LatestCIN][LLIW]","[CombVerMo|N07muVerMo:SubjVertMo:QGVertMotion][CombMoisture|SatContMoist:RaoContMoist]","[CombClouds|VISCloudCov:IRCloudCover][Scenario|Date][CurPropConv|LatestCIN:LLIW]","[AreaMesoALS|CombVerMo][ScenRelAMCIN|Scenario][ScenRelAMIns|Scenario][ScenRel34|Scenario]","[ScnRelPlFcst|Scenario][Dewpoints|Scenario][LowLLapse|Scenario][MeanRH|Scenario]", "[MidLLapse|Scenario][MvmtFeatures|Scenario][RHRatio|Scenario][SfcWndShfDis|Scenario]", "[SynForcng|Scenario][TempDis|Scenario][WindAloft|Scenario][WindFieldMt|Scenario]",  "[WindFieldPln|Scenario][AreaMoDryAir|AreaMesoALS:CombMoisture]",                      "[AMCINInScen|ScenRelAMCIN:MorningCIN][AMInsWliScen|ScenRelAMIns:LIfr12ZDENSd:AMDewptCalPl]",                      "[CldShadeOth|AreaMesoALS:AreaMoDryAir:CombClouds][InsInMt|CldShadeOth:AMInstabMt]",                      "[OutflowFrMt|InsInMt:WndHodograph][CldShadeConv|InsInMt:WndHodograph][MountainFcst|InsInMt]",                      "[Boundaries|WndHodograph:OutflowFrMt:MorningBound][N34StarFcst|ScenRel34:PlainsFcst]",                      "[CompPlFcst|AreaMesoALS:CldShadeOth:Boundaries:CldShadeConv][CapChange|CompPlFcst]",                      "[InsChange|CompPlFcst:LoLevMoistAd][CapInScen|CapChange:AMCINInScen]",                      "[InsSclInScen|InsChange:AMInsWliScen][R5Fcst|MountainFcst:N34StarFcst]",                      "[PlainsFcst|CapInScen:InsSclInScen:CurPropConv:ScnRelPlFcst]") 
dag = model2network(modelstring)
graphviz.plot(dag)

#Loading the data for the first 100 values
data_100 <-head(hailfinder,100)
data_100

bnet_bic=hc(data_100,score="bic")
bnet_bic
score(bnet_bic,data_100,type="bic")
graphviz.plot(bnet_bic)

bnet_bde=hc(data_100,score="bde")
bnet_bde
score(bnet_bde,data_100,type="bde")
graphviz.plot(bnet_bde)


#4.1.b

#Loading the first 1000 data

data_1000 <-head(hailfinder,1000)
data_1000

bnet_bic2=hc(data_1000,score="bic")
bnet_bic2
score(bnet_bic2,data_1000,type="bic")
graphviz.plot(bnet_bic2)

bnet_bde2=hc(data_1000,score="bde")
bnet_bde2
score(bnet_bde2,data_1000,type="bde")
graphviz.plot(bnet_bde2)


#4.1.c

#Loading the first 1000 data

data_10000 <-head(hailfinder,10000)
data_10000
bnet_bic3=hc(data_10000,score="bic")
bnet_bic3
score(bnet_bic3,data_10000,type="bic")
graphviz.plot(bnet_bic3)

bnet_bde3=hc(data_10000,score="bde")
bnet_bde3
score(bnet_bde3,data_10000,type="bde")
graphviz.plot(bnet_bde3)


#4.3.a.Whole dataset
set_full=hailfinder
bnet_bicfull=hc(set_full,score="bic")
bnet_bicfull
score(bnet_bicfull,set_full,type="bic")
graphviz.plot(bnet_bicfull)

bnet_bdefull=hc(set_full,score="bde")
bnet_bdefull
score(bnet_bdefull,set_full,type="bde")
graphviz.plot(bnet_bdefull)


#4.3.b

compare(bnet_bicfull,bnet_bdefull,arcs=TRUE)

#Comparing the true network with network obtained through bic score
compare(dag,bnet_bicfull)
graphviz.compare(bnet_bicfull,dag)

#Comparing the true network with the network obtained through BDe score
compare(dag,bnet_bdefull)
graphviz.compare(bnet_bdefull,dag)


#4.3.c
fitted_params=bn.fit(bnet_bicfull,set_full)
fitted_params$CombClouds

#4.3.d
cpquery(fitted_params, event = (CombClouds=="Cloudy"), evidence = ((MeanRH== "VeryMoist") & (IRCloudCover== "Cloudy")))






