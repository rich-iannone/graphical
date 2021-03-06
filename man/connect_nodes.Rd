% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connect_nodes.R
\name{connect_nodes}
\alias{connect_nodes}
\title{Create a connection between nodes}
\usage{
connect_nodes(graph, from, to)
}
\arguments{
\item{graph}{a graph object of class \code{dgr_graph} that is created using
\code{create_graph}.}

\item{from}{the node from which a connection will initiate.}

\item{to}{the node to which a connection will terminate.}
}
\value{
a graph object of class \code{dgr_graph}.
}
\description{
Create a connection between two different nodes in the graph
space.
}

