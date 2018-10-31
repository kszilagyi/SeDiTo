#fifo.py
import os
import sys
import base64

from os.path import expanduser
if __name__ == "__main__":
    fifo_path = expanduser("~/.sedito/startup_pipe")
    # todo setup logging to log file
    print 'path is %s' % fifo_path
    arg_left = sys.argv[1]
    arg_right = sys.argv[2]
    try:
        pipe = os.open(fifo_path, os.O_WRONLY | os.O_NONBLOCK)
        try:
            os.write(pipe, base64.standard_b64encode(arg_left) + ' ' + base64.standard_b64encode(arg_right))
        finally:
            os.close(pipe)
    except Exception as e:
        print 'Starting app because %s(%s) occurred.' % (type(e).__name__, e.args)


