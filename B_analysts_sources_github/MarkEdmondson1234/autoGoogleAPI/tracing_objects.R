#' Google Tracing API Objects 
#' Send and retrieve trace data from Google Stackdriver Trace.
#' 
#' Auto-generated code by googleAuthR::gar_create_api_objects
#'  at 2017-03-05 20:20:46
#' filename: /Users/mark/dev/R/autoGoogleAPI/googletracingv1.auto/R/tracing_objects.R
#' api_json: api_json
#' 
#' Objects for use by the functions created by googleAuthR::gar_create_api_skeleton

#' Trace Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A trace describes how long it takes for an application to perform someoperations. It consists of a set of spans, each of which contains detailsabout an operation with time information and operation details.
#' 
#' @param name ID of the trace which is 'projects/<project_id>/traces/<trace_id>'
#' 
#' @return Trace object
#' 
#' @family Trace functions
#' @export
Trace <- function(name = NULL) {
    structure(list(name = name), class = "gar_Trace")
}

#' Module Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' Binary module.
#' 
#' @param buildId Build_id is a unique identifier for the module,
#' @param module E
#' 
#' @return Module object
#' 
#' @family Module functions
#' @export
Module <- function(buildId = NULL, module = NULL) {
    structure(list(buildId = buildId, module = module), class = "gar_Module")
}

#' Status Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The `Status` type defines a logical error model that is suitable for differentprogramming environments, including REST APIs and RPC APIs. It is used by[gRPC](https://github.com/grpc). The error model is designed to be:- Simple to use and understand for most users- Flexible enough to meet unexpected needs# OverviewThe `Status` message contains three pieces of data: error code, error message,and error details. The error code should be an enum value ofgoogle.rpc.Code, but it may accept additional error codes if needed.  Theerror message should be a developer-facing English message that helpsdevelopers *understand* and *resolve* the error. If a localized user-facingerror message is needed, put the localized message in the error details orlocalize it in the client. The optional error details may contain arbitraryinformation about the error. There is a predefined set of error detail typesin the package `google.rpc` which can be used for common error conditions.# Language mappingThe `Status` message is the logical representation of the error model, but itis not necessarily the actual wire format. When the `Status` message isexposed in different client libraries and different wire protocols, it can bemapped differently. For example, it will likely be mapped to some exceptionsin Java, but more likely mapped to some error codes in C.# Other usesThe error model and the `Status` message can be used in a variety ofenvironments, either with or without APIs, to provide aconsistent developer experience across different environments.Example uses of this error model include:- Partial errors. If a service needs to return partial errors to the client,    it may embed the `Status` in the normal response to indicate the partial    errors.- Workflow errors. A typical workflow has multiple steps. Each step may    have a `Status` message for error reporting purpose.- Batch operations. If a client uses batch request and batch response, the    `Status` message should be used directly inside batch response, one for    each error sub-response.- Asynchronous operations. If an API call embeds asynchronous operation    results in its response, the status of those operations should be    represented directly using the `Status` message.- Logging. If some API errors are stored in logs, the message `Status` could    be used directly after any stripping needed for security/privacy reasons.
#' 
#' @param code The status code, which should be an enum value of google
#' @param message A developer-facing error message, which should be in English
#' @param details A list of messages that carry the error details
#' 
#' @return Status object
#' 
#' @family Status functions
#' @export
Status <- function(code = NULL, message = NULL, details = NULL) {
    structure(list(code = code, message = message, details = details), class = "gar_Status")
}

#' ListTracesResponse Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The response message for the `ListTraces` method.
#' 
#' @param traces List of trace records returned
#' @param nextPageToken If defined, indicates that there are more traces that match the request
#' 
#' @return ListTracesResponse object
#' 
#' @family ListTracesResponse functions
#' @export
ListTracesResponse <- function(traces = NULL, nextPageToken = NULL) {
    structure(list(traces = traces, nextPageToken = nextPageToken), class = "gar_ListTracesResponse")
}

#' Span Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A span represents a single operation within a trace. Spans can be nestedand form a trace tree. Often, a trace contains a root span that describes theend-to-end latency and, optionally, one or more subspans forits sub-operations. Spans do not need to be contiguous. There may be gapsbetween spans in a trace.
#' 
#' @param Span.attributes The \link{Span.attributes} object or list of objects
#' @param localStartTime Local machine clock time from the UNIX epoch,
#' @param hasRemoteParent True if this Span has a remote parent (is an RPC server Span)
#' @param timeEvents A collection of time-stamped events
#' @param parentId ID of parent span
#' @param localEndTime Local machine clock time from the UNIX epoch,
#' @param status The final status of the Span
#' @param name Name of the span
#' @param stackTrace Stack trace captured at the start of the span
#' @param links A collection of links
#' @param attributes Properties of a span
#' @param id Identifier for the span
#' 
#' @return Span object
#' 
#' @family Span functions
#' @export
Span <- function(Span.attributes = NULL, localStartTime = NULL, hasRemoteParent = NULL, 
    timeEvents = NULL, parentId = NULL, localEndTime = NULL, status = NULL, name = NULL, 
    stackTrace = NULL, links = NULL, attributes = NULL, id = NULL) {
    structure(list(Span.attributes = Span.attributes, localStartTime = localStartTime, 
        hasRemoteParent = hasRemoteParent, timeEvents = timeEvents, parentId = parentId, 
        localEndTime = localEndTime, status = status, name = name, stackTrace = stackTrace, 
        links = links, attributes = attributes, id = id), class = "gar_Span")
}

#' Span.attributes Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' Properties of a span. Attributes at the span level.E.g.'/instance_id': 'my-instance''/zone': 'us-central1-a''/grpc/peer_address': 'ip:port' (dns, etc.)'/grpc/deadline': 'Duration''/http/user_agent''/http/request_bytes': 300'/http/response_bytes': 1200'/http/url': google.com/apis'/pid''abc.com/myattribute': 'my attribute value'Maximum length for attribute key is 128 characters, for string attributevalue is 2K characters.
#' 
#' 
#' 
#' @return Span.attributes object
#' 
#' @family Span functions
#' @export
Span.attributes <- function() {
    list()
}

#' Empty Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A generic empty message that you can re-use to avoid defining duplicatedempty messages in your APIs. A typical example is to use it as the requestor the response type of an API method. For instance:    service Foo {      rpc Bar(google.protobuf.Empty) returns (google.protobuf.Empty);    }The JSON representation for `Empty` is empty JSON object `{}`.
#' 
#' 
#' 
#' @return Empty object
#' 
#' @family Empty functions
#' @export
Empty <- function() {
    list()
}

#' AttributeValue Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' Allowed attribute values.
#' 
#' @param intValue An integer value
#' @param stringValue A string value
#' @param boolValue A boolean value
#' 
#' @return AttributeValue object
#' 
#' @family AttributeValue functions
#' @export
AttributeValue <- function(intValue = NULL, stringValue = NULL, boolValue = NULL) {
    structure(list(intValue = intValue, stringValue = stringValue, boolValue = boolValue), 
        class = "gar_AttributeValue")
}

#' BatchUpdateSpansRequest Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The request message for the `BatchUpdateSpans` method.
#' 
#' @param BatchUpdateSpansRequest.spanUpdates The \link{BatchUpdateSpansRequest.spanUpdates} object or list of objects
#' @param spanUpdates A map from trace name to spans to be stored or updated
#' 
#' @return BatchUpdateSpansRequest object
#' 
#' @family BatchUpdateSpansRequest functions
#' @export
BatchUpdateSpansRequest <- function(BatchUpdateSpansRequest.spanUpdates = NULL, spanUpdates = NULL) {
    structure(list(BatchUpdateSpansRequest.spanUpdates = BatchUpdateSpansRequest.spanUpdates, 
        spanUpdates = spanUpdates), class = "gar_BatchUpdateSpansRequest")
}

#' BatchUpdateSpansRequest.spanUpdates Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A map from trace name to spans to be stored or updated.
#' 
#' 
#' 
#' @return BatchUpdateSpansRequest.spanUpdates object
#' 
#' @family BatchUpdateSpansRequest functions
#' @export
BatchUpdateSpansRequest.spanUpdates <- function() {
    list()
}

#' StackTrace Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' StackTrace collected in a trace.
#' 
#' @param stackFrame Stack frames of this stack trace
#' @param stackTraceHashId User can choose to use their own hash function to hash large attributes to
#' 
#' @return StackTrace object
#' 
#' @family StackTrace functions
#' @export
StackTrace <- function(stackFrame = NULL, stackTraceHashId = NULL) {
    structure(list(stackFrame = stackFrame, stackTraceHashId = stackTraceHashId), 
        class = "gar_StackTrace")
}

#' TimeEvent Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A time-stamped annotation in the Span.
#' 
#' @param annotation Optional field for user supplied <string, AttributeValue> map
#' @param localTime The local machine absolute timestamp when this event happened
#' @param networkEvent Optional field that can be used only for network events
#' 
#' @return TimeEvent object
#' 
#' @family TimeEvent functions
#' @export
TimeEvent <- function(annotation = NULL, localTime = NULL, networkEvent = NULL) {
    structure(list(annotation = annotation, localTime = localTime, networkEvent = networkEvent), 
        class = "gar_TimeEvent")
}

#' SpanUpdates Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' Collection of spans to update.
#' 
#' @param spans A collection of spans
#' 
#' @return SpanUpdates object
#' 
#' @family SpanUpdates functions
#' @export
SpanUpdates <- function(spans = NULL) {
    structure(list(spans = spans), class = "gar_SpanUpdates")
}

#' ListSpansResponse Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' The response message for the 'ListSpans' method.
#' 
#' @param spans The requested spans if they are any in the specified trace
#' @param nextPageToken If defined, indicates that there are more spans that match the request
#' 
#' @return ListSpansResponse object
#' 
#' @family ListSpansResponse functions
#' @export
ListSpansResponse <- function(spans = NULL, nextPageToken = NULL) {
    structure(list(spans = spans, nextPageToken = nextPageToken), class = "gar_ListSpansResponse")
}

#' NetworkEvent Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' An event describing an RPC message sent/received on the network.
#' 
#' @param kernelTime If available, this is the kernel time:
#' @param type Type of a NetworkEvent
#' @param messageId Every message has an identifier, which must be different from all the
#' @param messageSize Number of bytes send/receive
#' 
#' @return NetworkEvent object
#' 
#' @family NetworkEvent functions
#' @export
NetworkEvent <- function(kernelTime = NULL, type = NULL, messageId = NULL, messageSize = NULL) {
    structure(list(kernelTime = kernelTime, type = type, messageId = messageId, messageSize = messageSize), 
        class = "gar_NetworkEvent")
}

#' StackFrame Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' Presents a single stack frame in a stack trace.
#' 
#' @param loadModule Binary module the code is loaded from
#' @param columnNumber Column number is important in JavaScript(anonymous functions),
#' @param fileName File name of the frame
#' @param sourceVersion source_version is deployment specific
#' @param originalFunctionName Used when function name is ‘mangled’
#' @param functionName Fully qualified names which uniquely identify function/method/etc
#' @param lineNumber Line number of the frame
#' 
#' @return StackFrame object
#' 
#' @family StackFrame functions
#' @export
StackFrame <- function(loadModule = NULL, columnNumber = NULL, fileName = NULL, sourceVersion = NULL, 
    originalFunctionName = NULL, functionName = NULL, lineNumber = NULL) {
    structure(list(loadModule = loadModule, columnNumber = columnNumber, fileName = fileName, 
        sourceVersion = sourceVersion, originalFunctionName = originalFunctionName, 
        functionName = functionName, lineNumber = lineNumber), class = "gar_StackFrame")
}

#' Link Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' Link one span with another which may be in a different Trace. Used (forexample) in batching operations, where a single batch handler processesmultiple requests from different traces.
#' 
#' @param type The type of the link
#' @param traceId The trace identifier of the linked span
#' @param spanId The span identifier of the linked span
#' 
#' @return Link object
#' 
#' @family Link functions
#' @export
Link <- function(type = NULL, traceId = NULL, spanId = NULL) {
    structure(list(type = type, traceId = traceId, spanId = spanId), class = "gar_Link")
}

#' Annotation Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' Text annotation with a set of attributes.
#' 
#' @param Annotation.attributes The \link{Annotation.attributes} object or list of objects
#' @param description A user-supplied message describing the event
#' @param attributes A set of attributes on the annotation
#' 
#' @return Annotation object
#' 
#' @family Annotation functions
#' @export
Annotation <- function(Annotation.attributes = NULL, description = NULL, attributes = NULL) {
    structure(list(Annotation.attributes = Annotation.attributes, description = description, 
        attributes = attributes), class = "gar_Annotation")
}


#' Annotation.attributes Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' A set of attributes on the annotation.
#' 
#' 
#' 
#' @return Annotation.attributes object
#' 
#' @family Annotation functions
#' @export


Annotation.attributes <- function() {
    
    
    list()
    
}

