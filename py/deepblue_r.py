import xmlrpclib


check_value = """\

"""

param_tmpl = """
#' @param %(name)s - A %(type)s%(vector)s (%(description)s)\
"""

result_tmpl = """\
%(description)s\
"""

results_tmpl = """ \
('okay', %(results)s) or ('error', error_message) \
"""

cmd_documentation_tmpl = """
#' %(description)s
#' %(params)s
#'
#' @return a vector with %(return)s\
"""


cmd_tmpl = """
%(documentation)s
deepblue.%(name)s <- function(%(parameter_names)s) {
  value <- xml.rpc(%(url)s, '%(name)s'%(parameter_convertion)s)
  status = value[[1]]
  if (status == "error") {
    stop(value[[2]])
  }
  value[[2]]
}
"""

def main():

  client = xmlrpclib.Server("http://deepblue.mpi-inf.mpg.de/xmlrpc", allow_none=True)

  (s, v) = client.echo(None)

  version = v.split()[1][1:-1]

  ok, commands = client.commands()
  if not ok:
    print "unable to retrieve commands"
    return

  categories = {}
  commands_long_doc = ""

  for name in sorted(commands.keys()):
    cmd = commands[name]
    desc = cmd["description"]
    category = desc[0]

    html_id = name.replace(' ', '-').lower()

    # generate full description html
    params_s = ""
    param_names = []
    param_names_convertion = []


    params_documentation = []

    for p in cmd["parameters"]:

      if p[0] == "user_key":
        param_names.append("user_key=deepblue.USER_KEY")
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
                                                 "description" : p[3]})
    results = []
    for r in cmd['results']:
      results.append( result_tmpl % {"name" : r[0],
                                     "type" : r[1],
                                     "description": r[3]})

    results_s = results_tmpl % {"results" : "".join(results)}

    if param_names_convertion:
      parameters_list_convertion = ", " + ', '.join(param_names_convertion)
    else:
      parameters_list_convertion = ""

    command_description = cmd_documentation_tmpl % {'description': cmd["description"][2],
                                                  'params' : "".join(params_documentation),
                                                  'return' :  results_s}

    commands_long_doc += cmd_tmpl % {"parameters_full": params_s,
                           "parameter_names": ', '.join(param_names),
                           "parameter_convertion": parameters_list_convertion,
                           "name": name,
                           "documentation": command_description,
                           'url': "deepblue.URL"}

  print commands_long_doc

  serialize_code = open('serialize.R').read()


  template = ""
  with open("api_auto.template", 'r') as f:
    template = f.read()
    template = template.replace("{{ version }}", version)
    template = template.replace("{{ commands }}", commands_long_doc)
    template = template.replace("{{ serialize_r }}", serialize_code)

  with open("deepblue.r", 'w') as f:
    f.write(template)



if __name__ == "__main__":
  main()
