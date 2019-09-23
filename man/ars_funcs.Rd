\name{arsData}
\alias{arsData}
\alias{arsProducts}

\title{Get analysis ready point and area samples from various geosaptial data}

\description{
Get analysis ready point and area samples from various sptio-temporal datasets primarily accessible through the Land Processes Distributed Active Archive Center (LPDAAC). Function \code{arsProducts} returns the complete list of products available. 

\bold{Immportant:} These methods should be used for smaller spatio-temporal subsets (e.g. point samples of MODIS NDVI over multiple years). For analysis involving large spatial component, other methods such as \code{getModis}, \code{getLandsat} are strongly recommended. 

The functions are user-friendly wrapper around the \href{https://lpdaacsvc.cr.usgs.gov/appeears/}{Application for Extracting and Exploring Analysis Ready Samples (AρρEEARS) API}. Additional information can be found in the \href{https://lpdaacsvc.cr.usgs.gov/appeears/api/?language=R#introduction}{R examples of AρρEEARS API}.


}

\usage{
arsSubmit(task_name, start_date, end_date, product, layer, latitude, longitude, recurring = FALSE,....)
arsDownload(task_response, path, ...)
}

\arguments{
  \item{task_name}{user defined task name to keep track of the processes}
  \item{start_date}{The start of the date range for which to extract data}
  \item{end_date}{The end of the date range for which to extract data}
  \item{product}{The product and version identifier for the product}
  \item{layer}{The name of the layer}
  \item{latitude}{Numeric geographic latitude for the coordinate}
  \item{longitude}{Numeric geographic longitude for the coordinate}
  \item{path}{Character. Path name indicating where to store the data. Default is the current working directory }
  \item{...}{ Additional required (!) parameters. These are data set specific. See Details}   
}
 
\value{
Multiple items
} 

\details{  

\bold{dates}: 

All date formats should be in MM-DD-YYYY format for non-recurring date ranges and MM-DD for recurring date ranges

\bold{yearRange}: 

If \code{recurring = TRUE}, specify the starting and ending years for a recurring date range (e.g. [2010,2015] ).


\bold{product & layers}: 

The product and version identifier for the product and the name of the layer (e.g. MOD11A1.006,LST_Day_1km ). Multiple layers can be specified by using multiple layer parameters (e.g. layer=MOD11A1.006,LST_Day_1km, &layer=MOD11A1.006,LST_Night_1km ). Layer and correspoding product combination if layers are from multiple sources. Not all products are available. See \code{arsProducts} for details. 


\bold{spatial}: 

\code{task_type = "point"}: \code{latitude}, \code{longitude}, \code{id} (optional), \code{category} (optional); coordinate objects can be spatial objects with id and category attributes. Names require for using other attributes as id and/or category. Multiple coordinates can be specified by using multiple coordinate parameters (e.g. coordinate=US-Ha1,DBF,42.5378,-72.1715&coordinate=US-Ha2,DBF,44.5378,-73.1715 ).

\code{task_type = "area"}: 

`bbox`: Extent; A rectangular bounding box in the form of {min_longitude},{min_latitude},{max_longitude},{max_latitude} (lower left, upper right) where the longitude and latitude values are in a geographic projection (e.g. -96.626640,43.735557,-95.624623,44.736689 ). Multiple bounding boxes can be specified by using multiple bbox parameters (e.g. bbox=-96.626640,43.735557,-95.624623,44.736689&bbox=-76.852609,38.991712,-76.851965,38.992137). 

`polygon`: A polygon constructed from geographic coordinate vertices. The value must be in the form of {coordinate_1_longitude},{coordinate_1_latitude}...{coordinate_N_longitude},{coordinate_N_latitude} , where the last coordinate specified is the same as the first coordinate, closing the polygon.

`file_type`: The file format of the output files. It must be one of geotiff or netcdf4.

`projection_name`: The name of the projection of the output files. It must be one of the supported projections (e.g. native , geographic ). The list of supported projections can be generated via an API call described here. Default is `geographic`.

}

\examples{

# show different scenarios with products, layers, point, area, date range, recurring (yearly composite)

}

\references{
\url{https://lpdaacsvc.cr.usgs.gov/appeears/}
}