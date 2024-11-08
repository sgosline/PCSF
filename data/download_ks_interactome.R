#' Download a new kinase substrate network
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
#' data("PSP_KS")
#' ksi <- construct_interactome(PSP_KS)}
#' 
#' @author Sara Gosline
#' 

download_ks_interactome<-function(url='https://raw.githubusercontent.com/PNNL-CompBio/amlresistancenetworks/refs/heads/master/inst/PSP%26NetworKIN_Kinase_Substrate_Dataset_July2016.csv',
                               mval=0.6394096){
  KSDB<-readr::read_delim(url,delim =',')
  
  kdat<-KSDB|>
      dplyr::group_by(GENE)|>
    dplyr::select(SUB_GENE,SUB_MOD_RSD)|>
    dplyr::rowwise()|>
    dplyr::mutate(subval=paste(SUB_GENE,SUB_MOD_RSD,sep='-'))
  
  allvals<-unique(kdat$subval)

  ksi<-apply(kdat,1,function(x)
    #for each substrate interaction, add a link from the kinase gene -> substrate -> substrate gene
    data.frame(from=c(x[['GENE']],x[['subval']]),to=c(x[['subval']],x[['SUB_GENE']]),
               cost=c(mval*1.5,mval/4)))%>%  ##arbitrary costs based on mean cost of edges around network
    do.call(rbind,.)
  ##todo: alter weights based on NK score
  PSP_NK=ksi
  
  #store as RDA
  save(PSP_NK,file='data/PSP_NK.rda')
  
  
}