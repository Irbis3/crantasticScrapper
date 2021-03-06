#' Cloud Monitoring API Objects 
#' Accesses Google Cloud Monitoring data.
#' 
#' Auto-generated code by googleAuthR::gar_create_api_objects
#'  at 2017-03-05 19:32:52
#' filename: /Users/mark/dev/R/autoGoogleAPI/googlecloudmonitoringv2beta2.auto/R/cloudmonitoring_objects.R
#' api_json: api_json
#' 
#' Objects for use by the functions created by googleAuthR::gar_create_api_skeleton

#' DeleteMetricDescriptorResponse Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The response of cloudmonitoring.metricDescriptors.delete.
#' 
#' 
#' 
#' @return DeleteMetricDescriptorResponse object
#' 
#' @family DeleteMetricDescriptorResponse functions
#' @export
DeleteMetricDescriptorResponse <- function() {
    structure(list(kind = `cloudmonitoring#deleteMetricDescriptorResponse`), class = "gar_DeleteMetricDescriptorResponse")
}

#' ListMetricDescriptorsRequest Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The request of cloudmonitoring.metricDescriptors.list.
#' 
#' 
#' 
#' @return ListMetricDescriptorsRequest object
#' 
#' @family ListMetricDescriptorsRequest functions
#' @export
ListMetricDescriptorsRequest <- function() {
    structure(list(kind = `cloudmonitoring#listMetricDescriptorsRequest`), class = "gar_ListMetricDescriptorsRequest")
}

#' ListMetricDescriptorsResponse Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The response of cloudmonitoring.metricDescriptors.list.
#' 
#' @param metrics The returned metric descriptors
#' @param nextPageToken Pagination token
#' 
#' @return ListMetricDescriptorsResponse object
#' 
#' @family ListMetricDescriptorsResponse functions
#' @export
ListMetricDescriptorsResponse <- function(metrics = NULL, nextPageToken = NULL) {
    structure(list(kind = `cloudmonitoring#listMetricDescriptorsResponse`, metrics = metrics, 
        nextPageToken = nextPageToken), class = "gar_ListMetricDescriptorsResponse")
}

#' ListTimeseriesDescriptorsRequest Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The request of cloudmonitoring.timeseriesDescriptors.list
#' 
#' 
#' 
#' @return ListTimeseriesDescriptorsRequest object
#' 
#' @family ListTimeseriesDescriptorsRequest functions
#' @export
ListTimeseriesDescriptorsRequest <- function() {
    structure(list(kind = `cloudmonitoring#listTimeseriesDescriptorsRequest`), class = "gar_ListTimeseriesDescriptorsRequest")
}

#' ListTimeseriesDescriptorsResponse Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The response of cloudmonitoring.timeseriesDescriptors.list
#' 
#' @param nextPageToken Pagination token
#' @param oldest The oldest timestamp of the interval of this query, as an RFC 3339 string
#' @param timeseries The returned time series descriptors
#' @param youngest The youngest timestamp of the interval of this query, as an RFC 3339 string
#' 
#' @return ListTimeseriesDescriptorsResponse object
#' 
#' @family ListTimeseriesDescriptorsResponse functions
#' @export
ListTimeseriesDescriptorsResponse <- function(nextPageToken = NULL, oldest = NULL, 
    timeseries = NULL, youngest = NULL) {
    structure(list(kind = `cloudmonitoring#listTimeseriesDescriptorsResponse`, nextPageToken = nextPageToken, 
        oldest = oldest, timeseries = timeseries, youngest = youngest), class = "gar_ListTimeseriesDescriptorsResponse")
}

#' ListTimeseriesRequest Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The request of cloudmonitoring.timeseries.list
#' 
#' 
#' 
#' @return ListTimeseriesRequest object
#' 
#' @family ListTimeseriesRequest functions
#' @export
ListTimeseriesRequest <- function() {
    structure(list(kind = `cloudmonitoring#listTimeseriesRequest`), class = "gar_ListTimeseriesRequest")
}

#' ListTimeseriesResponse Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The response of cloudmonitoring.timeseries.list
#' 
#' @param nextPageToken Pagination token
#' @param oldest The oldest timestamp of the interval of this query as an RFC 3339 string
#' @param timeseries The returned time series
#' @param youngest The youngest timestamp of the interval of this query as an RFC 3339 string
#' 
#' @return ListTimeseriesResponse object
#' 
#' @family ListTimeseriesResponse functions
#' @export
ListTimeseriesResponse <- function(nextPageToken = NULL, oldest = NULL, timeseries = NULL, 
    youngest = NULL) {
    structure(list(kind = `cloudmonitoring#listTimeseriesResponse`, nextPageToken = nextPageToken, 
        oldest = oldest, timeseries = timeseries, youngest = youngest), class = "gar_ListTimeseriesResponse")
}

#' MetricDescriptor Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A metricDescriptor defines the name, label keys, and data type of a particular metric.
#' 
#' @param description Description of this metric
#' @param labels Labels defined for this metric
#' @param name The name of this metric
#' @param project The project ID to which the metric belongs
#' @param typeDescriptor Type description for this metric
#' 
#' @return MetricDescriptor object
#' 
#' @family MetricDescriptor functions
#' @export
MetricDescriptor <- function(description = NULL, labels = NULL, name = NULL, project = NULL, 
    typeDescriptor = NULL) {
    structure(list(description = description, labels = labels, name = name, project = project, 
        typeDescriptor = typeDescriptor), class = "gar_MetricDescriptor")
}

#' MetricDescriptorLabelDescriptor Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A label in a metric is a description of this metric, including the key of this description (what the description is), and the value for this description.
#' 
#' @param description Label description
#' @param key Label key
#' 
#' @return MetricDescriptorLabelDescriptor object
#' 
#' @family MetricDescriptorLabelDescriptor functions
#' @export
MetricDescriptorLabelDescriptor <- function(description = NULL, key = NULL) {
    structure(list(description = description, key = key), class = "gar_MetricDescriptorLabelDescriptor")
}

#' MetricDescriptorTypeDescriptor Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A type in a metric contains information about how the metric is collected and what its data points look like.
#' 
#' @param metricType The method of collecting data for the metric
#' @param valueType The data type of of individual points in the metric's time series
#' 
#' @return MetricDescriptorTypeDescriptor object
#' 
#' @family MetricDescriptorTypeDescriptor functions
#' @export
MetricDescriptorTypeDescriptor <- function(metricType = NULL, valueType = NULL) {
    structure(list(metricType = metricType, valueType = valueType), class = "gar_MetricDescriptorTypeDescriptor")
}

#' Point Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' Point is a single point in a time series. It consists of a start time, an end time, and a value.
#' 
#' @param boolValue The value of this data point
#' @param distributionValue The value of this data point as a distribution
#' @param doubleValue The value of this data point as a double-precision floating-point number
#' @param end The interval [start, end] is the time period to which the point's value applies
#' @param int64Value The value of this data point as a 64-bit integer
#' @param start The interval [start, end] is the time period to which the point's value applies
#' @param stringValue The value of this data point in string format
#' 
#' @return Point object
#' 
#' @family Point functions
#' @export
Point <- function(boolValue = NULL, distributionValue = NULL, doubleValue = NULL, 
    end = NULL, int64Value = NULL, start = NULL, stringValue = NULL) {
    structure(list(boolValue = boolValue, distributionValue = distributionValue, 
        doubleValue = doubleValue, end = end, int64Value = int64Value, start = start, 
        stringValue = stringValue), class = "gar_Point")
}

#' PointDistribution Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' Distribution data point value type. When writing distribution points, try to be consistent with the boundaries of your buckets. If you must modify the bucket boundaries, then do so by merging, partitioning, or appending rather than skewing them.
#' 
#' @param buckets The finite buckets
#' @param overflowBucket The overflow bucket
#' @param underflowBucket The underflow bucket
#' 
#' @return PointDistribution object
#' 
#' @family PointDistribution functions
#' @export
PointDistribution <- function(buckets = NULL, overflowBucket = NULL, underflowBucket = NULL) {
    structure(list(buckets = buckets, overflowBucket = overflowBucket, underflowBucket = underflowBucket), 
        class = "gar_PointDistribution")
}

#' PointDistributionBucket Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The histogram's bucket. Buckets that form the histogram of a distribution value. If the upper bound of a bucket, say U1, does not equal the lower bound of the next bucket, say L2, this means that there is no event in [U1, L2).
#' 
#' @param count The number of events whose values are in the interval defined by this bucket
#' @param lowerBound The lower bound of the value interval of this bucket (inclusive)
#' @param upperBound The upper bound of the value interval of this bucket (exclusive)
#' 
#' @return PointDistributionBucket object
#' 
#' @family PointDistributionBucket functions
#' @export
PointDistributionBucket <- function(count = NULL, lowerBound = NULL, upperBound = NULL) {
    structure(list(count = count, lowerBound = lowerBound, upperBound = upperBound), 
        class = "gar_PointDistributionBucket")
}

#' PointDistributionOverflowBucket Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The overflow bucket is a special bucket that does not have the upperBound field; it includes all of the events that are no less than its lower bound.
#' 
#' @param count The number of events whose values are in the interval defined by this bucket
#' @param lowerBound The lower bound of the value interval of this bucket (inclusive)
#' 
#' @return PointDistributionOverflowBucket object
#' 
#' @family PointDistributionOverflowBucket functions
#' @export
PointDistributionOverflowBucket <- function(count = NULL, lowerBound = NULL) {
    structure(list(count = count, lowerBound = lowerBound), class = "gar_PointDistributionOverflowBucket")
}

#' PointDistributionUnderflowBucket Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The underflow bucket is a special bucket that does not have the lowerBound field; it includes all of the events that are less than its upper bound.
#' 
#' @param count The number of events whose values are in the interval defined by this bucket
#' @param upperBound The upper bound of the value interval of this bucket (exclusive)
#' 
#' @return PointDistributionUnderflowBucket object
#' 
#' @family PointDistributionUnderflowBucket functions
#' @export
PointDistributionUnderflowBucket <- function(count = NULL, upperBound = NULL) {
    structure(list(count = count, upperBound = upperBound), class = "gar_PointDistributionUnderflowBucket")
}

#' Timeseries Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The monitoring data is organized as metrics and stored as data points that are recorded over time. Each data point represents information like the CPU utilization of your virtual machine. A historical record of these data points is called a time series.
#' 
#' @param points The data points of this time series
#' @param timeseriesDesc The descriptor of this time series
#' 
#' @return Timeseries object
#' 
#' @family Timeseries functions
#' @export
Timeseries <- function(points = NULL, timeseriesDesc = NULL) {
    structure(list(points = points, timeseriesDesc = timeseriesDesc), class = "gar_Timeseries")
}

#' TimeseriesDescriptor Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' TimeseriesDescriptor identifies a single time series.
#' 
#' @param TimeseriesDescriptor.labels The \link{TimeseriesDescriptor.labels} object or list of objects
#' @param labels The label's name
#' @param metric The name of the metric
#' @param project The Developers Console project number to which this time series belongs
#' 
#' @return TimeseriesDescriptor object
#' 
#' @family TimeseriesDescriptor functions
#' @export
TimeseriesDescriptor <- function(TimeseriesDescriptor.labels = NULL, labels = NULL, 
    metric = NULL, project = NULL) {
    structure(list(TimeseriesDescriptor.labels = TimeseriesDescriptor.labels, labels = labels, 
        metric = metric, project = project), class = "gar_TimeseriesDescriptor")
}

#' TimeseriesDescriptor.labels Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The label's name.
#' 
#' 
#' 
#' @return TimeseriesDescriptor.labels object
#' 
#' @family TimeseriesDescriptor functions
#' @export
TimeseriesDescriptor.labels <- function() {
    list()
}

#' TimeseriesDescriptorLabel Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' No description
#' 
#' @param key The label's name
#' @param value The label's value
#' 
#' @return TimeseriesDescriptorLabel object
#' 
#' @family TimeseriesDescriptorLabel functions
#' @export
TimeseriesDescriptorLabel <- function(key = NULL, value = NULL) {
    structure(list(key = key, value = value), class = "gar_TimeseriesDescriptorLabel")
}

#' TimeseriesPoint Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' When writing time series, TimeseriesPoint should be used instead of Timeseries, to enforce single point for each time series in the timeseries.write request.
#' 
#' @param point The data point in this time series snapshot
#' @param timeseriesDesc The descriptor of this time series
#' 
#' @return TimeseriesPoint object
#' 
#' @family TimeseriesPoint functions
#' @export
TimeseriesPoint <- function(point = NULL, timeseriesDesc = NULL) {
    structure(list(point = point, timeseriesDesc = timeseriesDesc), class = "gar_TimeseriesPoint")
}

#' WriteTimeseriesRequest Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The request of cloudmonitoring.timeseries.write
#' 
#' @param WriteTimeseriesRequest.commonLabels The \link{WriteTimeseriesRequest.commonLabels} object or list of objects
#' @param commonLabels The label's name
#' @param timeseries Provide time series specific labels and the data points for each time series
#' 
#' @return WriteTimeseriesRequest object
#' 
#' @family WriteTimeseriesRequest functions
#' @export
WriteTimeseriesRequest <- function(WriteTimeseriesRequest.commonLabels = NULL, commonLabels = NULL, 
    timeseries = NULL) {
    structure(list(WriteTimeseriesRequest.commonLabels = WriteTimeseriesRequest.commonLabels, 
        commonLabels = commonLabels, timeseries = timeseries), class = "gar_WriteTimeseriesRequest")
}

#' WriteTimeseriesRequest.commonLabels Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The label's name.
#' 
#' 
#' 
#' @return WriteTimeseriesRequest.commonLabels object
#' 
#' @family WriteTimeseriesRequest functions
#' @export
WriteTimeseriesRequest.commonLabels <- function() {
    list()
}


#' WriteTimeseriesResponse Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The response of cloudmonitoring.timeseries.write
#' 
#' 
#' 
#' @return WriteTimeseriesResponse object
#' 
#' @family WriteTimeseriesResponse functions
#' @export


WriteTimeseriesResponse <- function() {
    
    
    
    structure(list(kind = `cloudmonitoring#writeTimeseriesResponse`), class = "gar_WriteTimeseriesResponse")
}

