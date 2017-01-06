import xmlrpclib


export_tmpl = """
#' @export \
"""
title_tmpl = """
#' @title %(name)s \
"""

param_tmpl = """
#' @param %(name)s - A %(type)s%(vector)s (%(description)s)\
"""


result_tmpl = """
#' @description %(description)s\
"""

results_tmpl = """ \
('okay', %(results)s) or ('error', error_message) \
"""

result_tmpl = """\
%(name)s - A %(type)s%(vector)s (%(description)s)\
"""

cmd_documentation_tmpl = """
%(export)s
#' %(title)s
#' @description %(description)s
#' @family %(category)s
#' %(params)s
#'
#' @return %(return)s
#'
#' @examples
%(examples)s
#'\
"""


cmd_tmpl = """
%(documentation)s
deepblue_%(name)s <- function(%(parameter_names)s) {

    previous_commands <- list()
    arg.names <- names(as.list(match.call()))
    for(command_object_name in arg.names[which(arg.names != "")]){
        if(exists(command_object_name)){
            command_object <- get(command_object_name)
            if(is(command_object, "DeepBlueCommand")){
                previous_commands <- append(previous_commands, command_object)
                assign(command_object_name, command_object@query_id)
            }
        }
    }
    value <- xml.rpc(%(url)s, '%(name)s'%(parameter_convertion)s)
    status = value[[1]]
    method_name = as.character(match.call()[[1]])
    message(paste("Called method:", method_name))
    message(paste("Reported status was:", status))
    if (status == "error") {
        stop(value[[2]])
    }
    if (!exists("user_key")) {
        user_key = NULL
    }
    if(length(value) == 1) return(NULL)
    else if(!is.list(value[[2]])){
        DeepBlueCommand(call = sys.call(),
            status = value[[1]],
            query_id = value[[2]],
            previous_commands = previous_commands,
            user_key = user_key
        )
    }

    if(is.data.frame(value[[2]]) && "count" %%in%% colnames(value[[2]])){
        result <- value[[2]]
        result$count <- as.integer(result$count)
        return(result)
    }

    return(value[[2]])
}
"""

def main():

  client = xmlrpclib.Server("http://deepblue.mpi-inf.mpg.de/xmlrpc", allow_none=True)

  #list of commands to ignore
  exclude = ['get_state',
  'add_annotation',
  'add_biosource',
  'add_epigenetic_mark',
  'add_experiment',
  'add_expression',
  'add_gene_model',
  'add_genome',
  'add_project',
  'add_sample',
  'add_sample_from_gsm',
  'add_technique',
  'add_user_to_project',
  'create_experiments_set',
  'set_biosource_parent',
  'set_biosource_synonym',
  'set_project_public',
  'upload_chromosome',
  'change_extra_metadata',
  'clone_dataset',
  'create_column_type_calculated',
  'create_column_type_simple',
  'create_column_type_range',
  'create_column_type_category',
  'find_pattern',
  'remove',
  'extract_ids',
  'extract_names']

  (s, v) = client.echo(None)

  version = v.split()[1][1:-1]

  ok, commands = client.commands()
  if not ok:
    print "unable to retrieve commands"
    return

  categories = {}
  commands_long_doc = ""

  for name in sorted(commands.keys()):

    if name in exclude: continue

    cmd = commands[name]
    desc = cmd["description"]

    category = desc[0]

    html_id = name.replace(' ', '-').lower()

    # generate full description html
    params_s = ""
    param_names = []
    param_names_convertion = []

    examples = open("examples/deepblue."+name+".R").read()
    examples = "#' ".join(examples.splitlines(True))
    examples = "#' " + examples

    #if name.startswith("add_") or name.startswith("set_biosource") or name == "set_project_public" or name == "upload_chromosome" or name.startswith("create_") or name == "cancel_request" or name == "change_extra_metadata" or name == "clone_dataset" or name == "remove" or name == "find_pattern":
    #  examples = "#' \dontrun{\n" + examples + "\n#' }\n"

    params_documentation = []
    titles = []
    for p in cmd["parameters"]:

      if p[0] == "user_key":
        param_names.append("user_key=deepblue_options('user_key')")
      elif p[0] == "extra_metadata":
        param_names.append("extra_metadata=NULL")
      else:
        param_names.append(p[0]+"= NULL")

      if p[1] == "int":
        param_names_convertion.append("if (is.null("+p[0]+")) NULL else as.integer("+p[0]+")")
      else:
        param_names_convertion.append(p[0])

      s_vector = ""
      if (p[2]):
        s_vector = " or a vector of " + p[1]

      params_documentation.append(param_tmpl  % {"name": p[0],
                                                 "type": p[1],
                                                 "vector" : s_vector,
                                                 "description" : p[3].replace('%', '\%')})
    titles.append(title_tmpl % {'name': name})
    results = []
    for r in cmd['results']:
      print r
      r_vector = ""
      if (r[2]):
        r_vector = " or a vector of " + r[1]
      results.append( result_tmpl % {"name" : r[0],
                                     "type" : r[1],
                                     "vector" : r_vector,
                                     "description": r[3]})

    join_results = "".join(results)
    if not join_results:
      join_results = "nothing :-("

    command_description = cmd_documentation_tmpl % {'export': export_tmpl,'title':"".join(titles),
                                                  'description': cmd["description"][2],
                                                  'category' : cmd['description'][1],
                                                  'params' : "".join(params_documentation),
                                                  'examples': examples,
                                                  'return' :  join_results}

    if param_names_convertion:
      parameters_list_convertion = ", " + ', '.join(param_names_convertion)
    else:
      parameters_list_convertion = ""

    commands_long_doc += cmd_tmpl % {"parameters_full": params_s,
                           "parameter_names": ', '.join(param_names),
                           "parameter_convertion": parameters_list_convertion,
                           "name": name,
                           "documentation": command_description,
                           'url': "deepblue_options('url')"}

  print commands_long_doc

  serialize_code = open('serialize.R').read()


  template = ""
  with open("api_auto.template", 'r') as f:
    template = f.read()
    template = template.replace("{{ version }}", version)
    template = template.replace("{{ commands }}", commands_long_doc)
    template = template.replace("{{ serialize_r }}", serialize_code)

  print "'" + template[105029:105089] + "'"
  with open("../R/deepblue.R", 'w') as f:
    f.write(template)



if __name__ == "__main__":
  main()
