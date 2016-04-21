#' @title xml.rpc
#' @description perform an XML-RPC call
xml.rpc =
function(url, method, ..., .args = list(...),
          .opts = list(),
          .defaultOpts = list(httpheader = c('Content-Type' = "text/xml"), followlocation = TRUE, useragent = useragent),
          .convert = TRUE, .curl = getCurlHandle(), useragent = "DeepBlue-R-XMLRPC", verbose=deepblue.debug.VERBOSE)
{
    # Turn the method and arguments to an RPC body.
  body = createBody(method,  .args)
  if(verbose)
    print(body)

    # merge the .defaultOpts and the .opts into one list.
  .defaultOpts[["postfields"]] = saveXML(body)
  if(length(.opts))
     .defaultOpts[names(.opts)] = .opts

  rdr = dynCurlReader(.curl, baseURL = url)
  .defaultOpts[["headerfunction"]] = rdr$update
  postForm(url, .opts = .defaultOpts, style = "POST", curl = .curl)

  hdr = parseHTTPHeader(rdr$header())
  if(as.integer(hdr[["status"]]) %/% 100 !=  2) {
    print(hdr["status"])
       # call an RCurl error generator function.
     stop("Problems")
  }
  ans = rdr$value()
  if (verbose)
    print(ans)

   # Now either convert using the default converter fnction (convertToR)
   # or return as is or allow the caller to specify a function to use for conversion.
  if(is.logical(.convert)) {
    if(.convert){
      convertToR(ans)
    }

    else
      ans
  } else if(is.function(.convert))
          .convert(ans)
  else
      ans
}

#' @title createBody
#' @description Create XML-RPC request body document
#' @param method DeepBlue operation to be executed
#' @param args Operation arguments
createBody =
function(method, args)
{
  top = newXMLNode("methodCall", newXMLNode("methodName", method))
  params = newXMLNode("params", parent = top)
  sapply(args, function(x) newXMLNode("param", rpc.serialize(x), parent = params))
  top
}

#' @title rpc.serialize
#' @description Serialize the R data to XML-RPC
setGeneric("rpc.serialize", function(x, ...) standardGeneric("rpc.serialize"))

setMethod("rpc.serialize", "ANY",
           function(x, ...) {
              if(isS4(x))
                return(rpc.serialize.S4Object(x, ...))

              stop("Not sure how to convert this type of object to XMLRPC format")
           })

#' @title rpc.serialize.S4Object
#' @description Serialize the R data to XML-RPC
rpc.serialize.S4Object =
function(x, ...)
{
  els = slotNames(x)
  rpc.serialize(structure(lapply(els, function(id) slot(x, id)), names = els), ...)
}

#' @title basicTypeMap
#' @description mapping between R types and XML-RPC types.
basicTypeMap =
  c("integer" = "i4",
    "double" = "double",
    "character" = "string",
    "logical" = "boolean",
    "POSIXt" = "dateTime.iso8601",
    "POSIXct" = "dateTime.iso8601",
    "Date" = "dateTime.iso8601",
    "list" = "array",
    "raw" = "base64")

#' @title cast
#' @description Cast to integer value if it is a logical
#' @param x - value to be converted
cast <- function(x) {
  if (is.logical(x))
    as.integer(x)
  else
    x
}

setOldClass("AsIs")

setMethod("rpc.serialize", "AsIs",
           function(x) {
             type = basicTypeMap[typeof(x)]
             vectorArray(x, type)
           })

setMethod("rpc.serialize", "NULL",
           function(x, ...) {
             newXMLNode("value", newXMLNode("nil"))
           })

setMethod("rpc.serialize", "raw",
           function(x, ...) {
#              x = gsub("\\n", "", x)
              val = base64Encode(x)
              newXMLNode("value", newXMLNode("base64", val))
           })


setMethod("rpc.serialize", "Date",
           function(x, ...) {
             val = format(x, "%Y%m%dT%H:%H:%S")
             if(length(x) == 1)
                newXMLNode("value", newXMLNode("dateTime.iso8601", val))
             else
                vectorArray(val, basicTypeMap["Date"])
           })

setMethod("rpc.serialize", "POSIXt",
           function(x, ...) {
             val = format(as.POSIXct(x), "%Y%m%dT%H:%H:%S")
             if(length(x) == 1)
                newXMLNode("value", newXMLNode("dateTime.iso8601", val))
             else
                vectorArray(val, basicTypeMap["POSIXt"])
           })

setMethod("rpc.serialize", "vector",
           function(x, ...) {
              type = basicTypeMap[typeof(x)]
              x = cast(x)

              if(length(names(x))) {
                warning("Skipping names on vector!")
                names(x) = NULL
              }

#              else
              {
                if(length(x) == 1)
                  newXMLNode("value", newXMLNode(type, if(type == "string") newXMLCDataNode(x) else x))
                else {
                  vectorArray(x, type)
                }
              }
           })


#' @title FormatStrings
#' @description Types format
FormatStrings = c(numeric = "%f", integer = "%d", logical = "%s",
                   i4 = "%d", double = "%f",
                  string = "%s", Date = "%s",  POSIXt = "%s", POSIXct = "%s")

#' @title vectorArray
#' @description Convert an vector to XML-RPC array.
vectorArray =
function(x, type)
{
  top = newXMLNode("value")
  a = newXMLNode("array", parent = top)
  data = newXMLNode("data", parent = a)

  tmpl = if(type == "string")  # is.character(x))
            sprintf("<value><%s><![CDATA[%%s]]></%s></value>", type, type)
         else if(type == "dateTime.iso8601") {
            if(is(x, "Date"))
               x = format(x, "%Y%m%dT00:00:00")
            else
               x = format(as.POSIXct(x), "%Y%m%dT%H:%H:%S")
            sprintf("<value><%s>%%s</%s></value>", type, type)
         } else {
           if(type == "double") {
              x = as.character(x)
              pct = "%s"
           } else
             pct = FormatStrings[type]

           if(is.na(pct)) pct = "%s"
           sprintf("<value><%s>%s</%s></value>", type, pct, type)
         }

  txt = sprintf(tmpl, x)
  parseXMLAndAdd(txt, data)

#  sapply(x, function(x)  newXMLNode(type, if(type == "string") newXMLCDataNode(x) else x, parent = data))
  top
}

setMethod("rpc.serialize", "list",
           function(x, ...) {
              if(length(names(x))) {
                  a = newXMLNode("struct")
                  sapply(names(x), function(id) {
                                     newXMLNode("member",
                                        newXMLNode("name", id),
                                        rpc.serialize(x[[id]]), parent = a)
                                   })
                  newXMLNode("value", a)
              } else {
                a = newXMLNode("array")
                data = newXMLNode("data", parent = a)
                v <- sapply(x, function(x) {
                    rpc.serialize(x)
                })
                addChildren(data, v)
                newXMLNode("value", a)
              }
           })

#' @title convertToR
#' @description Convert XML-RPC nodes to R types
#' @param node - XML-RPC XML node to be converted to R

setGeneric('convertToR', function(node) standardGeneric('convertToR'))

setMethod('convertToR', 'XMLInternalDocument', function(node)
{
    fault = getNodeSet(node,path="//methodResponse/fault/value/struct")
    if (length(fault) > 0) {
          fault = xmlRPCToR(fault[[1]])
          e = simpleError(paste("faultCode: ",  fault$faultCode, " faultString: ", fault$faultString))
          class(e) = c("XMLRPCError", class(e))
          stop(e)
    }
    a = xpathApply(node, "//param/value", xmlRPCToR)
    if(length(a) == 1)
      a[[1]]
    else
      a
})

setMethod('convertToR', 'XMLInternalNode',
function(node)
{
   if(length(getNodeSet(node, "./param/value"))) {
     ans = xpathApply(node, "./param/value", xmlRPCToR, simplify = FALSE)
   } else
      xmlToList(node)
})

setMethod('convertToR', 'character',
function(node)
{
  xml = xmlParse(node, asText = TRUE)
  convertToR(xml)
})

#' @title xmlRPCToR
#' @description convert the XML-RPC data to R
xmlRPCToR =
function(node, ...)
{
  if(is.null(node))
    return(NULL)

  if(xmlName(node) == "value") {
    node = node[[1]]
  }

  if(is(node, "XMLInternalTextNode")) {
    return(xmlValue(node))
  }

  type = xmlName(node)
  switch(type,
         'array' = xmlRPCToR.array(node, ...),
         'struct' = xmlRPCToR.struct(node, ...),
         'i4' = as.integer(xmlValue(node)),
         'int' = as.integer(xmlValue(node)),
         'boolean' = if(xmlValue(node) == "1") TRUE else FALSE,
         'double' = as.numeric(xmlValue(node)),
         'string' = xmlValue(node),
         'dateTime.iso8601' = as.POSIXct(strptime(xmlValue(node), "%Y%m%dT%H:%M:%S")),
         'base64' = base64(xmlValue(node), encode = FALSE),
         xmlValue(node)
        )

}
#' @title xmlRPCToR.struct
#' @description convert XML-RPC struct to R data'
#' @param node - XML-RPC node that contains the struct.
xmlRPCToR.struct =
function(node, ...)
{
  ans = xmlApply(node, function(x) xmlRPCToR(x[["value"]][[1]], ...))
  names(ans) = xmlSApply(node, function(x) xmlValue(x[["name"]]))
  ans
}

#' @title xmlRPCToR.struct
#' @description convert XML-RPC array to R data'
#' @param node - XML-RPC node that contains the array.
xmlRPCToR.array =
function(node, ...)
{
  i <- 1
  result <- list()
  while(!is.null(node[["data"]][[i]])){
   result <- append(result, list(xmlRPCToR(node[["data"]][[i]])))
   i <- i + 1
 }
 return(result)
}

#' @title check value
#' @description return the function result and check for error message from DeepBlue server.
#' @param input - DeepBlue XML-RPC return to be checked.
check_value =
function(input)
{
  status = input[[1]]
  print(status)
  if (status == "error") {
    stop(input[[2]])
  }
  value = input[[2]]
  return (value)
}