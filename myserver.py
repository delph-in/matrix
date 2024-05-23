import http.server

PORT = 9000

# Treat everything as a cgi file, i.e.
# `handler.cgi_directories = ["*"]` but that is not defined, so we need
class Handler(http.server.CGIHTTPRequestHandler):
  def __init__(self, *args, **kwargs):
        super().__init__(*args, directory="", **kwargs)
  def is_cgi(self):
    self.cgi_info = '', self.path[1:]
    if "matrix.cgi" in self.path:
      print("This is the self path", self.path)
      return True
    else:
      return False

with http.server.HTTPServer(("", PORT), Handler) as httpd:
    try:
        print("serving at port", PORT)
        httpd.serve_forever()
    except KeyboardInterrupt:
        print("\nclose server")
        httpd.server_close()
