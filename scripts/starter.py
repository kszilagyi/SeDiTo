#fifo.py
import os
import sys
import base64
from subprocess import call, STDOUT
from os.path import expanduser
import logging

logging.basicConfig(filename=expanduser('~/.sedito/starter.log'),level=logging.DEBUG,
                    format='%(asctime)-15s %(message)s')

if __name__ == "__main__":
    try:
        project_dir = expanduser('~/.sedito/')
        fifo_path = project_dir + '/startup_pipe'
        # todo setup logging to log file
        logging.info('path is %s, parameters: %s' % (fifo_path, str(sys.argv)))
        arg_left = sys.argv[1]
        arg_right = sys.argv[2]

        try:
            pipe = os.open(fifo_path, os.O_WRONLY | os.O_NONBLOCK)
            try:
                os.write(pipe, base64.standard_b64encode(arg_left) + ' ' + base64.standard_b64encode(arg_right))
                logging.info('App was already running, passed parameters.')
            finally:
                os.close(pipe)
        except Exception as e:
            logging.info('Starting app because %s(%s) occurred.' % (type(e).__name__, e.args))
            dir = os.path.dirname(os.path.realpath(__file__))
            exe = '%s/../gui/target/universal/stage/bin/gui' % dir
            logging.info('Running %s' % exe)
            log_file = project_dir + '/sedito_stderr_stdout.log'
            with open(log_file, 'a') as f:
                call([exe, arg_left, arg_right], stderr=STDOUT, stdout=f)
    except Exception as e:
        logging.error('Uncaught exception: %s(%s)' % (type(e).__name__, e.args))
