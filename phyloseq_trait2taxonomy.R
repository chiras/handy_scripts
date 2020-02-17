
trait.natives= read.csv("NativeIntroduced.csv")

map2traits <- function(phyloseq, traits){	
	
	tax_table(phyloseq)[,"species"] 
	trait_dataset = matrix(data=NA, nrow=length(taxa_names(phyloseq)), ncol=length(names(traits))-1)
	rownames(trait_dataset)<-taxa_names(phyloseq)
	colnames(trait_dataset)<-names(traits)[2:length(names(traits))]
	
	for (i in 1: length(taxa_names(phyloseq))){
		if (tax_table(phyloseq)[i,"species"] %in% traits[,1]){ # only first trait column currently considered
			trait_dataset[i,1] <- as.character(traits[which(traits[,1]==as.character(tax_table(phyloseq)[i,"species"]) ),2])
		}
	}
	
	tax_table(phyloseq)<- tax_table(trait_dataset)
	

  return(phyloseq)
}


data.natives <- map2traits(data.renamed, trait.natives)
