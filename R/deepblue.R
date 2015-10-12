library(XMLRPC)

#URL = "http://deepblue.mpi-inf.mpg.de/xmlrpc"
URL = "http://localhost:31415"
USER_KEY = "anonymous_key"

ids <- c("me", "a1")
me = xml.rpc(URL, "info", ids, "anonymous_key")
print(me)

print(me[2]$value[1]$value$name)
print(me[2]$value[2]$value$name)

commands = xml.rpc(URL, "commands")

# List all commands
print(t(commands[2]$value))
print(commands[2]$value$add_annotation$description[3])
# [1] "Inserts a new annotation with the given parameters."

# Print command names
print(names(commands[2]$value))

# Each command
print(mapply(print, commands[2]))

commands_list = commands[[2]]
for (i in 1:length(commands_list)) {
	command = commands_list[[i]]
	parameters = command$parameters
	results = command$results
  description = command$description
}

function <- create_deepblue_call()

setGeneric("deepblue.list_experiments",
  function() {
    print("hello")
  }
)

setGeneric("deepblue.list_experiments", c(epigenetic_mark=NULL, sample=NULL, technique=NULL, project=NULL, user_key="anonymous_key"),
           function(object, step, ...) {
             print("hello")
           }
)


setMethod("deepblue_list_experiments", signature(epigenetic_mark=NULL, sample=NULL, technique=NULL, project=NULL, user_key="anonymous_key"),
         function(epigenetic_mark, sample, technique, project, user_key) {
           ret <- xml.rpc(URL, "list_experiments", epigenetic_mark, sample, technique, project, user_key);
           ret
         }
)


setClass("DeepBlue",
         slots = c(url="character", user_key="character"),
         prototype = list(url="http://localhost:31415", user_key="anonymous")
)

setMethod("show", "DeepBlue", function(object) {
  cat(object@url, object@user_key)
})


setGeneric("hi",
           function(object="DeepBlue")
             standardGeneric("hi")
)

setMethod("hi", "DeepBlue", function(object) {
  cat(object@url, object@user_key)
})

setGeneric("DeepBlue.list_experiments",
          function(object="DeepBlue", epigenetic_mark="character", ...)
              standardGeneric("DeepBlue.list_experiments")
)

setMethod("DeepBlue.list_experiments", c("DeepBlue", "character"),
          function(object, epigenetic_mark, ...) {
          }
)
           # sample = ""
          #  technique = ""
          #  project = ""
          #  user_key = object@user_key
          #  ret <- xml.rpc(object@url, "list_experiments", epigenetic_mark, sample, technique, project, user_key);
          #  ret
          #}
#)

functions <- character()
function_name <-  "deepblue.list_experiments"
functions <- c(functions, function_name)

eval("2 + 2")

assign(function_name,  function(genome, epigenetic_mark, sample, technique, project, user_key) {
    xml.rpc(URL,'list_experiments', genome, epigenetic_mark, sample, technique, project, user_key)
}, envir = .GlobalEnv)




deepblue <- new("DeepBlue", user_key=1234567)
deepblue.list_experiments("aaa")

