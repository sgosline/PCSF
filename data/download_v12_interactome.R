#' Download a new interaction network
#'
#' Given a URL download and save new interactome dataset
#' 
#' @param stringdl A list of edges. A \code{data.frame} composed of three columns, where each
#'  row corresponds to an edge in which the first element is a \code{head}, the second 
#'  element is a \code{tail}, and the last element represents the \code{cost} of the edge.
#'  
#' @return A data object
#' @import readr
#' @import dplyr
#' @export
#' 
#' @examples 
#' \dontrun{
#' library("PCSF")
#' data("STRING")
#' ppi <- construct_interactome(STRING)}
#' 
#' @author Sara Gosline
#' 

download_interactome<-function(url='https://stringdb-downloads.org/download/protein.physical.links.v12.0/9606.protein.physical.links.v12.0.txt.gz'){
  tab<-readr::read_delim(url,delim = ' ')
  
  #map proteins to gene names
  map<-readr::read_delim('https://stringdb-downloads.org/download/protein.info.v12.0/9606.protein.info.v12.0.txt.gz',delim='\t')
  mapred<-map|>dplyr::select(protein1=`#string_protein_id`,gene1='preferred_name')|>
    dplyr::distinct()
  ntab<-tab|>dplyr::left_join(mapred)
  
  mapred<-mapred|>dplyr::rename(protein2='protein1',gene2='gene1')
  
  ntab<-ntab|>
    dplyr::left_join(mapred)
  #are there dupes? - no, but they are directed!
  
  ntab<-ntab|>dplyr::select(from='gene1',to='gene2',combined_score)|>
    dplyr::distinct()|>
    dplyr::mutate(cost=1-(combined_score/1000))|>
    dplyr::select(-combined_score)|>
    as.data.frame()
  

  #store as RDA
  STRINGv12<-ntab
  save(STRINGv12,file='data/STRINGv12.rda')
  
  
}