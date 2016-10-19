#' @title xml.rpc
#' @keywords internal
#' @return XML RPC request data converted to R objects
#' @description perform an XML-RPC call
xml.rpc =
    function(url, method, ..., .args = list(...),
             .opts = list(),
             .defaultOpts = list(httpheader = c('Content-Type' = "text/xml"),
                                 followlocation = TRUE,
                                 useragent = useragent),
             .convert = TRUE,
             .curl = getCurlHandle(),
             useragent = "DeepBlue-R-XMLRPC",
             verbose=deepblue_debug_VERBOSE)
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
        # or return as is or allow the caller to specify a
        # function to use for conversion.
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

createBody =
    function(method, args)
    {
        top = newXMLNode("methodCall", newXMLNode("methodName", method))
        params = newXMLNode("params", parent = top)
        sapply(args, function(x) newXMLNode("param",
                                            rpc.serialize(x),
                                            parent = params))
        top
    }


setGeneric("rpc.serialize", function(x, ...) standardGeneric("rpc.serialize"))

setMethod("rpc.serialize", "ANY",
          function(x, ...) {
              if(isS4(x))
                  return(rpc.serialize.S4Object(x, ...))

              stop("Not sure how to convert this type")
          })

rpc.serialize.S4Object =
    function(x, ...)
    {
        els = slotNames(x)
        rpc.serialize(structure(lapply(els, function(id) slot(x, id)),
                                names = els), ...)
    }

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

              if(length(x) == 1)
                  newXMLNode("value", newXMLNode(type, if(type == "string") {
                      newXMLCDataNode(x)
                  }
                  else x))
              else {
                  vectorArray(x, type)
              }
          }
)


FormatStrings = c(numeric = "%f", integer = "%d", logical = "%s",
                  i4 = "%d", double = "%f",
                  string = "%s", Date = "%s",  POSIXt = "%s", POSIXct = "%s")


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


setGeneric('convertToR', function(node) standardGeneric('convertToR'))

setMethod('convertToR', 'XMLInternalDocument', function(node)
{
    fault = getNodeSet(node,path="//methodResponse/fault/value/struct")
    if (length(fault) > 0) {
        fault = xmlRPCToR(fault[[1]])
        e = simpleError(paste("faultCode: ",  fault$faultCode,
                              " faultString: ", fault$faultString))
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
                  ans = xpathApply(node, "./param/value", xmlRPCToR,
                                   simplify = FALSE)
              } else
                  xmlToList(node)
          })

setMethod('convertToR', 'character',
          function(node)
          {
              xml = xmlParse(node, asText = TRUE)
              convertToR(xml)
          })


xmlRPCToR =
    function(node, ...)
    {
        type = xmlName(node)

        # if the node is a 'value' node, get its child element
        if (type == "value") {
          node = node[[1]]
          type = xmlName(node)
        }

        switch(type,
               'array' = xmlRPCToR.array(node, ...),
               'struct' = xmlRPCToR.struct(node, ...),
               'i4' = as.integer(xmlValue(node)),
               'int' = as.integer(xmlValue(node)),
               'boolean' = if(xmlValue(node) == "1") TRUE else FALSE,
               'double' = as.numeric(xmlValue(node)),
               'string' = xmlValue(node),
               'dateTime.iso8601' = as.POSIXct(strptime(xmlValue(node),
                                                        "%Y%m%dT%H:%M:%S")),
               'base64' = base64(xmlValue(node), encode = FALSE),
               xmlValue(node)
        )

    }

xmlRPCToR.struct =
    function(node, ...)
    {
        #check if our structure is nested
        descendant_struct <- getNodeSet(node, ".//struct")
        
        #case where we have tabular data
        if(length(descendant_struct) == 0){
            strings <- xpathSApply(node, "./member", getChildrenStrings)
            values <- as.list(strings[2,])
            names(values) <- strings[1,]
            return(values)
        }
        
        #further structs means recursive processing
        else{
            ans = xmlApply(node, function(x) xmlRPCToR(x[[2]][[1]], ...))
            names(ans) = xmlSApply(node, function(x) xmlValue(x[[1]]))
            return(ans)
        }
    }

xmlRPCToR.array =
    function(node, status = NULL, ...)
    {
        nodeSize <- xmlSize(node[[1]])
        elements <- xmlChildren(node[[1]])
        
        if(is.null(status)){
            status <- xmlRPCToR(elements[[1]])
            result <- xmlRPCToR(elements[[2]], status)
        }
        else{
            result <- vector("list", nodeSize)
            for(element in 1:nodeSize) {
                result[[element]] <- xmlRPCToR(elements[[element]])
            }
            
            for(r in 1:length(result)){
                test_result <- result[[r]]
                if(is.null(names(test_result))){
                    if(length(test_result) == 2){
                        names(result[[r]]) = c("id", "name")
                        
                        if(length(result[[r]]$name) > 1)
                            result[[r]] <- c(id = result[[r]]$id, result[[r]]$name)
                    } 
                    else if(length(test_result) == 3)
                    {
                        names(result[[r]]) = c("id", "name", "count")
                    }
                }
            }
            if(is.list(result) && length(result) == 1) return(result[[1]])
            
            framed_result <- tryCatch(data.table::rbindlist(result, fill = TRUE),
                                      error = function(e){ return(result)})
            
            return(framed_result)
            
        }
        if(is.null(status)) return(result)
        else return(list(status, result))
    }

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