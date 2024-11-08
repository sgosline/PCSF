##getnew string interactome

tab<-readr::read_delim('https://stringdb-static.org/download/protein.physical.links.v11.5/9606.protein.physical.links.v11.5.txt.gz')
mapping<-readr::read_delim('https://stringdb-static.org/download/protein.info.v11.5/9606.protein.info.v11.5.txt.gz')|>
  dplyr::select(prot='#string_protein_id',gene='preferred_name')

##now do the join

newtab<-tab|>
  dplyr::rename(prot='protein1')|>
  dplyr::left_join(mapping)|>
  dplyr::select(from='gene',prot='protein2',combined_score)|>
  dplyr::left_join(mapping)|>
  dplyr::select(from,to='gene',combined_score)|>
  dplyr::mutate(cost=1-combined_score/1000)|>
  dplyr::select(from,to,cost)|>
  dplyr::distinct()