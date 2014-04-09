#!/usr/bin/env python

from __future__ import print_function

import os
import cgi
from subprocess import Popen, PIPE, STDOUT



# # Haskell
SCRIPTDIR = 'haskell'

# To run NON compiled code
# SCRIPT = ['/usr/bin/runhaskell', 'Shrdlite.hs']


# Run compiled code with garbage collection off
# compile with this command: ghc -O2 -rtsopts Shrdlite.hs
SCRIPT = ['./Shrdlite',' +RTS -A2G']



################################################################################

print('Content-type:text/plain')
print()

try:
    while not os.path.isdir(SCRIPTDIR):
        SCRIPTDIR = os.path.join("..", SCRIPTDIR)

    form = cgi.FieldStorage()
    data = form.getfirst('data')
    script = Popen(SCRIPT, cwd=SCRIPTDIR, stdin=PIPE, stdout=PIPE, stderr=PIPE)
    out, err = script.communicate(data)

    print(out)
    if err:
        raise Exception(err)

except:
    import sys, traceback
    print(traceback.format_exc())
    sys.exit(1)
