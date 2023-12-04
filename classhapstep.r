###Class Hapstep, functions and methods
##april 2021, cnrakt (caktas.aca at gmail.com)
##will be included in the package haplotypes in future updates.
#This software is distributed under the terms of the GNU General Public License.

#install.packages("haplotypes")
require("haplotypes")


#Class Hapstep

slots<-representation(nhap="numeric"
,npop="numeric"
,mean="numeric"
,harmean="numeric"
,Dm="numeric"
,Dwm="numeric"
,hS="numeric"
,hS_se="numeric"
,hT="numeric"
,hT_se="numeric"
,Gst="numeric"
,Gst_se="numeric"
,vS="numeric"
,vS_se="numeric"
,vT="numeric"
,vT_se="numeric"
,Nst="numeric"
,Nst_se="numeric"
,vS_mper="numeric"
,vT_mper="numeric"
,Nst_mper="numeric"
,vS_pval="numeric"
,vT_pval="numeric"
,Nst_pval="numeric"
,infile="list"
,call="call"
)

setClass(Class="Hapstep", representation=slots)

# Initialize Class Hapstep


setMethod("initialize", "Hapstep", function(.Object
,nhap=numeric(1)
,npop=numeric(1)
,mean=numeric(1)
,harmean=numeric(1)
,Dm=numeric(1)
,Dwm=numeric(1)
,hS=numeric(1)
,hS_se=numeric(1)
,hT=numeric(1)
,hT_se=numeric(1)
,Gst=numeric(1)
,Gst_se=numeric(1)
,vS=numeric(1)
,vS_se=numeric(1)
,vT=numeric(1)
,vT_se=numeric(1)
,Nst=numeric(1)
,Nst_se=numeric(1)
,vS_mper=numeric(1)
,vT_mper=numeric(1)
,Nst_mper=numeric(1)
,vS_pval=1
,vT_pval=1
,Nst_pval=1
,infile=list()
) {
    .Object@nhap<-nhap
    .Object@npop<-npop
    .Object@mean<-mean
    .Object@harmean<-harmean
    .Object@Dm<-Dm
    .Object@Dwm<-Dwm
    .Object@hS<-hS
    .Object@hS_se<-hS_se
    .Object@hT<-hT
    .Object@hT_se<-hT_se
    .Object@Gst<-Gst
    .Object@Gst_se<-Gst_se
    .Object@vS<-vS
    .Object@vS_se<-vS_se
    .Object@vT<-vT
    .Object@vT_se<-vT_se
    .Object@Nst<-Nst
    .Object@Nst_se<-Nst_se
    .Object@vS_mper<-vS_mper
    .Object@vT_mper<-vT_mper
    .Object@Nst_mper<-Nst_mper
    .Object@vS_pval<-vS_pval
    .Object@vT_pval<-vT_pval
    .Object@Nst_pval<-Nst_pval
    .Object@infile=infile
    .Object
})



#Show method for Hapstep objects

setMethod("show","Hapstep", function(object)
{
    if(length(object@vS_pval))
    {
        vSsig<-"ns"
        if(object@vS_pval<=0.1)  vSsig<-"."
        if(object@vS_pval<=0.05)  vSsig<-"*"
        if(object@vS_pval<=0.01)  vSsig<-"**"
        if(object@vS_pval<=0.001)  vSsig<-"***"
    }
    if(length(object@vT_pval))
    {
        vTsig<-"ns"
        if(object@vT_pval<=0.1)  vTsig<-"."
        if(object@vT_pval<=0.05)  vTsig<-"*"
        if(object@vT_pval<=0.01)  vTsig<-"**"
        if(object@vT_pval<=0.001)  vTsig<-"***"
    }
    if(length(object@Nst_pval))
    {
        Nstsig<-"ns"
        if(object@Nst_pval<=0.1)  Nstsig<-"."
        if(object@Nst_pval<=0.05)  Nstsig<-"*"
        if(object@Nst_pval<=0.01)  Nstsig<-"**"
        if(object@Nst_pval<=0.001)  Nstsig<-"***"
    }
    
    cat("*** S4 Object of Class Hapstep ***\n")
    
    cat("\nNumber of haplotypes =", object@nhap)
    cat("\nNumber of populations =", object@npop)
    cat("\nMean =", object@mean)
    cat("\nHarmonic mean =", object@harmean)
    cat("\nMean nb. of differences between haplotypes (Dm) =", object@Dm)
    cat("\nWeighted mean nb. of diff. between haplotypes (Dwm) =", object@Dwm)
    cat("\n\nhS (se) = ", object@hS,paste("(",round(object@hS_se,5),")",sep=""))
    cat("\nhT (se) = ", object@hT,paste("(",round(object@hT_se,5),")",sep=""))
    cat("\nGst (se) = ", object@Gst,paste("(",round(object@Gst_se,5),")",sep=""))
    cat("\n\nvS (se) = ", object@vS,paste("(",round(object@vS_se,5),")",sep=""),", p-value = ",object@vS_pval, " ", vSsig,sep="")
    cat("\nvT (se) = ", object@vT,paste("(",round(object@vT_se,5),")",sep=""),", p-value = ",object@vT_pval, " ",vTsig,sep="")
    cat("\nNst (se) = ", object@Nst,paste("(",round(object@Nst_se,5),")",sep=""),", p-value = ",object@Nst_pval, " ",Nstsig,sep="")
    
    cat("\n\nExpected ", ": vS = ", object@vS_mper," vT = ",object@vT_mper, " Nst = ",object@Nst_mper, sep="")
    cat("\nObserved ", ": vS = ", object@vS," vT = ",object@vT," Nst = ",object@Nst,sep="")
   
     cat("\nNumber of permutations =", as.character(object@call["nperm"]))
     
    cat("\n\nSignif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ns’ 1")
    
    
    cat("\n\nNote: Use slotNames() function to return slot names (use '@' or slot() function to access slots. \n e.g: object@nhap or slot(object,'nhap'):\n")

})



#Generic prep.infile: prepare infile (hapstep/permut) from Dna objects
#populations contained three or more individuals are taken.
setGeneric (
name= "prep.infile",
def=function(x,...)standardGeneric("prep.infile")
)

#prep.infile method for Dna objects, april 21
setMethod(f="prep.infile", signature= "Dna", definition=function(x,file=NULL,indels="sic",populations)
{
    indmet<- c("sic","5th","missing")
    matchmet <- pmatch(indels, indmet)
    if (is.na(matchmet))
    stop("invalid indel coding method", paste("", indels))
    indels<-indmet[matchmet]
    rmsingletons<-names(table(populations))[table(populations)>2]
    rempops<-na.omit(rmsingletons[match(populations,rmsingletons)])
    seq<-as.matrix(x)
    if(!is.null(attr(rempops,"na.action"))) remseq<-as.dna(seq[-attr(rempops,"na.action"),]) else remseq<-x
    h<-haplotypes::haplotype(remseq,indels=indels)
    g<-t(grouping(h,factor=rempops)$hapmat)
    if(indels=="sic")
    {
        hx<-as.dna(h@sequence)
        ind<-indelcoder(hx)$codematrix
        ind[ind==1]<-5
        ind[ind==0]<-1
        ind[ind==-1]<-0
        s<-as.numeric(as.dna(subs(as.dna(h@sequence))$subsmat))
        s[s==5]<-0
        hapcharmat<-cbind(s,ind)
    }
    
    if(indels=="5th")
    {
        hapcharmat<-cbind(as.numeric(as.dna(subs(as.dna(h@sequence),fifth=TRUE)$subsmat)),ind)
    }
    
    if(indels=="missing")
    {

         s<-as.numeric(as.dna(subs(as.dna(h@sequence))$subsmat))
         s[s==5]<-0
        hapcharmat<-s
    }
    
    if(!is.null(file))
    {
        write.table(matrix(c(h@nhap,nrow(g),ncol(hapcharmat)),1,3),file=file,quote=FALSE,sep="\t",col.names=FALSE,row.names=FALSE)
        write.table(g,file=file,append=TRUE,quote=FALSE,sep="\t",col.names=FALSE,row.names=FALSE)
        write.table(hapcharmat,file=file,append=TRUE,quote=FALSE,sep="\t",col.names=FALSE,row.names=FALSE)
    }
    
    if(is.null(file))
    {
        colnames(hapcharmat)<-1:ncol(hapcharmat)
        he<-matrix(c(h@nhap,nrow(g),ncol(hapcharmat)),1,3)
        colnames(he)<-c("nhap","npop","nchar")
        list(head=he, Fm=g,G=hapcharmat)
    }
}
)



# remove excess gaps in Dna matrix
#remove.excess.gaps

#Generic remove.excess.gaps
setGeneric (
name= "remove.excess.gaps",
def=function(x,...)standardGeneric("remove.excess.gaps")
)



#remove.excess.gaps method for Dna objects

setMethod(f="remove.excess.gaps", signature= "Dna", definition=function(x)
{
    seq<-as.numeric(x)
    alltest<-function(x,char="-") all(x==char)
    allindel<-apply(seq,2,alltest,char=5)
    regx<-x[,!allindel,as.matrix=FALSE]
    colnames(regx@sequence)<-1:ncol(regx)
    return(regx)
    
}

)


#Generic har.mean: calculate harmonic mean
setGeneric (
name= "har.mean",
def=function(x,...)standardGeneric("har.mean")
)


#har.mean method for Dna objects
setMethod(f="har.mean", signature= "Haplotype", definition=function(x,factor,minpopsize=3)
{
    
    g<-t(haplotypes::grouping(x,factor=factor)$hapmat)
    rs<- rowSums(g,na.rm = TRUE)
    mrs<-rs[rs>=minpopsize]
    
    harmean<-length(mrs)/sum(1/mrs)
    harmean
    
})



#Generic hapstep:  Based on original Pascal code (Hapstep program) written by Remy PETIT (Petit at pierroton.inra.fr),  April 2000. Licenced under GNU General Public License.

setGeneric (
name= "hapstep",
def=function(x,...)standardGeneric("hapstep")
)



#hapstep method for Dna objects, april 21

setMethod(f="hapstep", signature= "Dna", definition=function(x,indels="sic",populations,skip.se=TRUE,nperm=0,printprog=TRUE)
{
    
    infile<-prep.infile(x=x,file=NULL,indels=indels,populations=populations)
    obj<- hapstep.internal(infile,skip.se=skip.se,nperm=nperm,printprog=printprog)
    obj@call<-match.call()
    return(obj)
    
})

#internal function: hapstep.internal
#This is based on original source code of Hapstep program written by Remy PETIT (Petit at pierroton.inra.fr) April 2000. Licenced under GNU General Public License.

hapstep.internal<-function(infile,skip.se=TRUE, nperm=0,printprog=TRUE)
{
    
    if(printprog)
    {
        
        message('\r',"*** Stepwise Analysis of Diversity for Haploid Data with Distances Between Haplotypes ***\n\n",appendLF = FALSE)
        message('\r'," * This is based on original source code of Hapstep program \nwritten by Remy PETIT (Petit at pierroton.inra.fr),  April 2000.\n * Licenced under GNU General Public License.\n\n",appendLF = FALSE)
        message('\r',"reading data...",appendLF = FALSE)
    }
    .Object<-new("Hapstep")
    
    A<-infile$head[1]
    n<-infile$head[2]
    fr<-infile$head[3]
    F<-t(infile$Fm)
    G<-infile$G
    Nk<-colSums(F)
    Fm<- rowSums(t(t(F)/Nk)/n)
    #calculation of distance matrix
    D<-matrix(0,A,A)
    for (i in 1:A)
    {
        for (j in 1:A)
        {
            for(m in 1:fr)
            {
                if(G[j,m]>0&&G[i,m]>0&&G[j,m]!=G[i,m]) D[i,j]<-D[i,j]+1
            }
        }
    }
    
    D1<-D
    DG<-matrix(0,A,A)
    # by default threshold set to 0
    thresh<-0
    trs<- D1<(thresh+1)
    D1[trs]<-0
    DG[!trs]<-1
    
    Sij<-0
   
    
    Moy<-sum(Nk/n)
    Harm<-n/sum(1/Nk)
    
    Sij<-0
    for(i in 1:A)
    {
        for (j in 1:A)
        {
            if(i!=j) Sij<-Sij+Fm[i]*Fm[j]
            
        }
    }
    if(printprog) message('\r',"Reading data...Ok\n",appendLF = FALSE)
    if(printprog) message('\r',"Calculating hS, hT, Gst...",appendLF = FALSE)
    #Calculate hS, hT, and Gst
    
    Dm1<-0
    Dm2<-0
    hT1<-0
    hS<-0
    hT<-0
    hk<-vector("numeric",n)
    hsvec<-c()
    hsvec1<-c()
    
    for(i in 1:A)
    {
        for (j in 1:A)
        {
            Dm1<-Dm1+Fm[i]*Fm[j]*D[i,j]/Sij
            Dm2<-Dm2+D[i,j]/A/(A-1)
            hT1<-hT1+DG[i,j]*Fm[i]*Fm[j];
            for(k in 1:n)
            {
                hk[k]<-hk[k]+1/(Nk[k]-1)*DG[i,j]*F[i,k]*F[j,k]/Nk[k]
                hS<-hS+1/n/(Nk[k]-1)*DG[i,j]*F[i,k]*F[j,k]/Nk[k]
                hT<-hT+DG[i,j]*(F[i,k]/Nk[k]-Fm[i])*(F[j,k]/Nk[k]-Fm[j])/n/(n-1)
            }
        }
    }
    
    hT<-hT1-hT
    Gst<-1-hS/hT
    
    if(printprog) message('\r',"Calculating hS, hT, Gst...Ok\n",appendLF = FALSE)
    if(printprog) message('\r',"Calculating vS, vT, Nst...",appendLF = FALSE)
    
    #Calculate vS, vT, and  Nst
    vk<-vector("numeric",n)
    vS<-0
    vT<-0
    Nst<-0
    vT1<-0
    Dm<-Dm2
    
    for(i in 1:A)
    {
        for (j in 1:A)
        {
            vT1<-vT1+D1[i,j]/Dm*Fm[i]*Fm[j];
            for(k in 1:n)
            {
                vk[k]<-vk[k]+1/(Nk[k]-1)*D1[i,j]/Dm*F[i,k]*F[j,k]/Nk[k];
                vS<-vS+1/n/(Nk[k]-1)*D1[i,j]/Dm*F[i,k]*F[j,k]/Nk[k];
                vT<-vT+D1[i,j]/Dm*(F[i,k]/Nk[k]-Fm[i])*(F[j,k]/Nk[k]-Fm[j])/n/(n-1);
            }
        }
    }
    
    vT<-vT1-vT;
    Nst<-1-vS/vT;
    
    .Object@nhap<-A
    .Object@npop<-n
    .Object@mean<-Moy
    .Object@harmean<-Harm
    .Object@Dm<-Dm
    .Object@Dwm<-Dm1
    .Object@hS<-hS
    .Object@hT<-hT
    .Object@Gst<-Gst
    .Object@vS<-vS
    .Object@vT<-vT
    .Object@Nst<-Nst
    
    if(printprog) message('\r',"Calculating vS, vT, Nst...OK\n",appendLF = FALSE)
    
    if(!skip.se)
    {
        if(printprog) message('\r',"Calculating SE...",appendLF = FALSE)
        #Calculate se's
        VarhS<-0
        VarvS<-0
        CovhSvS<-0
        for(k in 1:n)
        {
            VarhS<-VarhS+(hk[k]-hS)^2/n/(n-1);
            VarvS<-VarvS+(vk[k]-vS)^2/n/(n-1);
            CovhSvS<-CovhSvS+(vk[k]-vS)*(hk[k]-hS)/n/(n-1);
        }
        
        hS_se<-sqrt(VarhS)
        vS_se<-sqrt(VarvS)
        
        CovvSvT<-0
        CovhSvT<-0
        VarhT<-0
        VarvT<-0
        CovhTvT<-0
        
        for(i in 1:A)
        {
            for (j in 1:A)
            {
                for(k in 1:n)
                {
                    CovvSvT<-CovvSvT+1/(n-1)*D[i,j]/Dm*(Fm[i]-F[i,k]/Nk[k]/n)*vk[k]*F[j,k]/Nk[k];
                    CovhSvT<-CovhSvT+1/(n-1)*D[i,j]/Dm*(Fm[i]-F[i,k]/Nk[k]/n)*hk[k]*F[j,k]/Nk[k];
                    VarhT<-VarhT+4*Fm[i]*Fm[j]*(F[i,k]/Nk[k]-Fm[i])*(F[j,k]/Nk[k]-Fm[j])/n/(n-1);
                    
                    for(ii in 1:A)
                    {
                        for(jj in 1:A)
                        {
                            VarvT<-VarvT+4*D[i,j]/Dm*D[ii,jj]/Dm*Fm[j]*Fm[jj]*(F[i,k]/Nk[k]-Fm[i])*(F[ii,k]/Nk[k]-Fm[ii])/n/(n-1);
                            if (ii!=jj) CovhTvT<-CovhTvT+4*D[i,j]/Dm*Fm[j]*Fm[jj]*(F[i,k]/Nk[k]-Fm[i])*(F[ii,k]/Nk[k]-Fm[ii])/n/(n-1);
                            
                        }
                        
                    }
                    
                }
            }
        }
        
        CovvSvT<-2/(n-2)*(CovvSvT-vS*vT);
        CovhSvT<-2/(n-2)*(CovhSvT-hS*vT);
        CovhShT<-0
        CovhTvS<-0
        VarG<-0
        VarN<-0
        
        for (i in 1:A)
        {
            for(k in 1:n)
            {
                CovhShT<-CovhShT+1/(n-1)*(Fm[i]-F[i,k]/Nk[k]/n)*hk[k]*F[i,k]/Nk[k]
                CovhTvS<-CovhTvS+1/(n-1)*(Fm[i]-F[i,k]/Nk[k]/n)*vk[k]*F[i,k]/Nk[k]
            }
            
        }
        
        CovhShT<-2/(n-2)*(hS*(1-hT)-CovhShT)
        CovhTvS<-2/(n-2)*(vS*(1-hT)-CovhTvS)
        
        VarG<-(VarhS-2*hS*CovhShT/hT+(hS/hT)^2*VarhT)/(hT)^2;
        VarN<-(VarvS-2*vS*CovvSvT/vT+(vS/vT)^2*VarvT)/(vT)^2;
        
        hT_se<-sqrt(VarhT)
        vT_se<-sqrt(VarvT)
       
        if(!is.nan(VarG)&&VarG>0) Gst_se<-sqrt(VarG) else Gst_se<-NaN
        if(!is.nan(VarN)&&VarN>0) Nst_se<-sqrt(VarN) else Nst_se<-NaN
        
        
        .Object@hS_se<-hS_se
        .Object@hT_se<-hT_se
        .Object@Gst_se<-Gst_se
        .Object@vS_se<-vS_se
        .Object@vT_se<-vT_se
        .Object@Nst_se<-Nst_se
        if(printprog) message('\r',"Calculating SE...Ok\n",appendLF = FALSE)
    }
    
    if(nperm>0)
    {
        
        if(printprog) message('\r',"Running permutations...\n",appendLF = FALSE)
        permvS<-vector("numeric",nperm)
        permvT<-vector("numeric",nperm)
        permNst<-vector("numeric",nperm)
        
        #permutations for vS, vT, and  Nst
        for(p in 1:nperm)
        {
            samp<-sample(A)
            F<-t(infile$Fm)
            F<-F[samp,]
            #F<-permut(F)
            #G<-infile$G
            Nk<-colSums(F)
            Fm<- rowSums(t(t(F)/Nk)/n)
            
            
            #Calculate vS, vT, and  Nst
            vk<-vector("numeric",n)
            vS<-0
            vT<-0
            Nst<-0
            vT1<-0
            Dm<-Dm2
            
            for(i in 1:A)
            {
                for (j in 1:A)
                {
                    vT1<-vT1+D1[i,j]/Dm*Fm[i]*Fm[j];
                    for(k in 1:n)
                    {
                        vk[k]<-vk[k]+1/(Nk[k]-1)*D1[i,j]/Dm*F[i,k]*F[j,k]/Nk[k];
                        vS<-vS+1/n/(Nk[k]-1)*D1[i,j]/Dm*F[i,k]*F[j,k]/Nk[k];
                        vT<-vT+D1[i,j]/Dm*(F[i,k]/Nk[k]-Fm[i])*(F[j,k]/Nk[k]-Fm[j])/n/(n-1);
                    }
                }
            }
            
            vT<-vT1-vT;
            Nst<-1-vS/vT;
            permvS[p]<-vS
            permvT[p]<-vT
            permNst[p]<-Nst
            if(printprog) setTxtProgressBar(txtProgressBar(1,nperm,style=3),p)
        }
        
        .Object@vS_mper<-mean(permvS)
        .Object@vT_mper<- mean(permvT)
        .Object@Nst_mper<-mean(permNst)
        vS_pval<-(sum(permvS<=.Object@vS)+1)/(nperm + 1)
        vT_pval<-(sum(permvT<=.Object@vT)+1)/(nperm + 1)
        Nst_pval<-(sum(permNst>=.Object@Nst)+1)/(nperm + 1)
        
        
        .Object@vS_pval<-vS_pval
        .Object@vT_pval<-vT_pval
        .Object@Nst_pval<-Nst_pval
        
    }
   .Object@infile<-infile
   .Object@call<-match.call()
    return(.Object)
}


#Generic getmorethan2:
#populations contained three or more individuals are taken.

setGeneric (
name= "getmorethan2",
def=function(x,...)standardGeneric("getmorethan2")
)

#getmorethan2 method for Dna objects, april 21
setMethod(f="getmorethan2", signature= "Dna", definition=function(x,file=NULL,populations)
{

    rmsingletons<-names(table(populations))[table(populations)>2]
    rempops<-na.omit(rmsingletons[match(populations,rmsingletons)])
    seq<-as.matrix(x)
    if(!is.null(attr(rempops,"na.action"))) remseq<-as.dna(seq[-attr(rempops,"na.action"),]) else remseq<-x
    remseq
})




##### EXAMPLES #####

require(haplotypes)
data(dna.obj)
x<-dna.obj
h<-haplotype(x)

### an extreme example ###

# perfect dissociation
# Gst==Nst==1
populations<-vector("character", nrow(x))
for(i in 1: h@nhap)
{
    indx<- h@hapind[[i]]
    populations[indx]<-paste("pop",i, sep="")
}

# only a single haplotype is fixed in each population and not shared between populations.
# frequency of haplotypes in populations
infile<-prep.infile(x,file=NULL,indels="sic",populations)
Fm<-t(infile$Fm)
rownames(Fm)<-paste("H",rownames(Fm),sep="")
Fm


# hapstep function
# use skip.se=FALSE to calculate standart errors.
hapstep.obj<-hapstep(x,indels="sic",populations=populations,skip.se=TRUE,nperm=999,printprog=TRUE)
hapstep.obj


### a more realistic example ###

# the populations are determined using hclust by clustering the sequences.
# perfect dissociation
hc<-hclust(distance(x))
clline<-4
plot(hc, hang = -1, main='cluster sequences')
abline (h=clline, col='red', lty=2)
ctt<-cutree(hc, h=clline)
populations<-paste("pop",ctt, sep="")

# one or more haplotypes are fixed in single population and not shared between populations
# frequency of haplotypes in populations
infile<-prep.infile(x,file=NULL,indels="sic",populations)
Fm<-t(infile$Fm)
rownames(Fm)<-paste("H",rownames(Fm),sep="")
Fm


# hapstep function
# use skip.se=FALSE to calculate standart errors.
hapstep.obj<-hapstep(x,indels="sic",populations=populations,skip.se=TRUE,nperm=999,printprog=TRUE)
hapstep.obj


### make it more realistic ###

# perturb phylogeographic structure
hc<-hclust(distance(x))
clline<-4
plot(hc, hang = -1, main='cluster sequences')
abline (h=clline, col='red', lty=2)
ctt<-cutree(hc, h=clline)
populations<-paste("pop",ctt, sep="")
populations2<-populations

# randomly shuffle 20% of populations
percent<-20
l<-length(populations2)
set.seed(12)
samp1<-sample(l,round(l*percent/100))
samp2<-sample(l,round(l*percent/100))
populations2[samp1]<-populations2[samp2]

# most of the haplotypes are fixed in a single population but some are shared between populations
# frequency of haplotypes in populations
infile<-prep.infile(x,file=NULL,indels="sic",populations2)
Fm<-t(infile$Fm)
rownames(Fm)<-paste("H",rownames(Fm),sep="")
Fm

# hapstep function
# use skip.se=FALSE to calculate standart errors.

hapstep.obj<-hapstep(x,indels="sic",populations=populations2,skip.se=TRUE,nperm=999,printprog=TRUE)
hapstep.obj






