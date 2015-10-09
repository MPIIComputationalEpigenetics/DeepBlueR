import xmlrpclib

header_tmpl = """
# Accessing Deepblue trough R
library(XMLRPC)
"""


cmd_tmpl = """
deepblue.%(name)s <- function(%(parameter_names)s) {
    xml.rpc(URL,'%(name)s', %(parameter_convertion)s)
}
"""

def main():

  client = xmlrpclib.Server("http://localhost:31415", allow_none=True)

  (s, v) = client.echo(None)

  version = v.split()[2]

  ok, commands = client.commands()
  if not ok:
    print "unable to retrieve commands"
    return

  categories = {}

  for name in sorted(commands.keys()):
    cmd = commands[name]
    desc = cmd["description"]
    category = desc[0]

    html_id = name.replace(' ', '-').lower()

    # generate full description html
    params_s = ""
    param_names = []
    param_names_convertion = []
    for p in cmd["parameters"]:
      param_names.append(p[0])
      if p[1] == "int":
        param_names_convertion.append("as.integer("+p[0]+")")
      else:
        param_names_convertion.append(p[0])

    #print param_names
    long_doc = cmd_tmpl % {"parameters_full": params_s,
                           "parameter_names": ', '.join(param_names),
                           "parameter_convertion": ', '.join(param_names_convertion),
                           "name": name,
                           "description": desc[2],
                           "id": html_id}
    print long_doc



if __name__ == "__main__":
  main()
