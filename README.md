shrdlite project for AI course at Chalmers University and University of Gothenburg

How to run:
- First start a web server:
  python -m CGIHTTPServer 8000
  from the same folder as the shrdlite.html
- Browse to any of the following addresses:
  http://localhost:8000/shrdlite.html
  http://127.0.0.1:8000/shrdlite.html
  http://0.0.0.0:8000/shrdlite.html

One might have to change the path in cgi-bin/ajaxwrapper.py, depending on the Python version.

Compile with: ghc -O2 -rtsopts Shrdlite.hs