% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.PCSFe.R
\name{plot.PCSFe}
\alias{plot.PCSFe}
\title{Plot an interactive subnetwork with functional enrichment analysis}
\usage{
\method{plot}{PCSFe}(x, edge_width = 5, node_size = 30,
  node_label_cex = 1, Terminal_node_legend = "Terminal",
  Steiner_node_legend = "Steiner", ...)
}
\arguments{
\item{x}{An output subnetwork provided by the \code{enrichment_analysis}. 
It is "PCSFe" object derived from an \pkg{igraph} class, and it has the edge 
cost and vertex prize attributes.}

\item{edge_width}{A \code{numeric} value to emphasize a maximum edge width. 
A default value is 5. This value must be greater than 1.}

\item{node_size}{A \code{numeric} value to emphasize a maximum node size. 
A default value is 30. This value must be greater than 10.}

\item{node_label_cex}{A \code{numeric} value to set a node label size. 
A default value is 1.}

\item{Terminal_node_legend}{A \code{string} to set a legend for \code{terminal} nodes. 
A default legend is "Terminal".}

\item{Steiner_node_legend}{A \code{string} to set a legend for \code{Steiner} nodes. 
A default legend is "Steiner".}

\item{...}{Ignored.}
}
\description{
\code{plot.PCSFe} plots an interactive figure of the subnetwork 
to display the functionla enrichment analysis, which is obtained by employing 
\code{enrichment_analysis} on the subnetwork.
}
\details{
An enrichment analysis of the final subnetwork obtained by multiple runs of the PCSF 
(with random noise added edge costs) is performed by using \code{\link{enrichment_analysis}}. 
The subnetwork is clustered using an edge betweenness clustering algorithm from the 
\pkg{igraph} package, and for each cluster functional enrichment is done by employing the 
ENRICHR API (Chen \emph{et al.}, 2013). An interactive visualization of the final subnetwork 
is plotted, where the node sizes and edge widths are proportional to the frequency of show 
ups in total randomised runs. Nodes are colored according to the cluster membership, and 
the top 15 functional enrichment terms are displayed in tabular format during the hover-over 
of the node in that cluster. A specific cluster can be displayed separately in the figure 
by selecting from the icon list at the top left side of the figure.
}
\examples{
\dontrun{
library("PCSF")
data("STRING")
data("Tgfb_phospho")
terminals <- Tgfb_phospho
ppi <- construct_interactome(STRING)
subnet <- PCSF_rand(ppi, terminals, n = 10, r = 0.1, w = 2, b = 1, mu = 0.0005)
res <- enrichment_analysis(subnet)
plot(res$subnet)}

}
\references{
Chen E.Y., Christopher M.T., Yan K., Qiaonan D., Zichen W., Gabriela V.M., Neil R.C., and Avi M. (2013) 
Enrichr: Interactive and Collaborative Html5 Gene List Enrichment Analysis Tool. \emph{BMC Bioinformatics} 14 (1). 
BioMed Central: 1.
}
\seealso{
\code{\link{enrichment_analysis}}, \code{\link{PCSF_rand}}, \code{\link{plot.PCSF}}
}
\author{
Murodzhon Akhmedov
}
