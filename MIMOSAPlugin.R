library(MIMOSA)
input <- function(inputfile) {
   mydata <<- read.csv(inputfile)
}

run <- function() {
# Note: MIMOSA models are highly specific to input data;
# so at the moment this is tailored to the example provided
# the MIMOSA documentation and in the example/ folder.
# A future goal will be to make it more customizable.
E<-ConstructMIMOSAExpressionSet(ICS,
reference=ANTIGEN%in%'negctrl',measure.columns=c('CYTNUM','NSUB'),
other.annotations=c('CYTOKINE','TCELLSUBSET','ANTIGEN','UID'),
default.cast.formula=component~UID+ANTIGEN+CYTOKINE+TCELLSUBSET,
.variables=.(TCELLSUBSET,CYTOKINE,UID),
featureCols=1,ref.append.replace='_REF')

result<-MIMOSA(NSUB+CYTNUM~UID+TCELLSUBSET+CYTOKINE|ANTIGEN,
data=E, method='EM',
subset=RefTreat%in%'Treatment'&ANTIGEN%in%'ENV',
ref=ANTIGEN%in%'ENV'&RefTreat%in%'Reference')
qvalues <<- fdr(result)
}

output <- function(outputfile) {
   write.csv(qvalues, outputfile)
}
