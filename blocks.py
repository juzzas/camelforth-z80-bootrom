#!/usr/bin/env python3

# This is free and unencumbered software released into the public domain.
# 
# Anyone is free to copy, modify, publish, use, compile, sell, or
# distribute this software, either in source code form or as a compiled
# binary, for any purpose, commercial or non-commercial, and by any
# means.
# 
# In jurisdictions that recognize copyright laws, the author or authors
# of this software dedicate any and all copyright interest in the
# software to the public domain. We make this dedication for the benefit
# of the public at large and to the detriment of our heirs and
# successors. We intend this dedication to be an overt act of
# relinquishment in perpetuity of all present and future rights to this
# software under copyright law.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
# OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.
# 
# For more information, please refer to <http://unlicense.org>

"""Forth BLOCK utility tool.
"""

from __future__ import print_function
import argparse
import logging
import os
import signal
import sys

class Processor(object):
    def __init__(self, infile, outfile):
        """Processor class constructor.

        Keyword arguments:
        infile -- File object of the input file
        outfile -- File object of the input file
        """
        self._infile = infile
        self._outfile = outfile

    @property
    def infile(self):
        """Return the filename of the infile File object."""
        if not self._infile:
            return None

        return self._infile.name

    @property
    def outfile(self):
        """Return the filename of the outfile File object."""
        if not self._outfile:
            return None

        return self._outfile.name

    def process(self, logger):
        """ Process infile to outfile."""

        padlines_cnt = 0
        for line in self._infile:
            line = line.rstrip('\n')

            if len(line) > 64:
                padded_line = line[:64]
                logger.warning("line {} too long".format(padlines_cnt+1))
            else:
                padded_line = line.ljust(64, ' ')

            self._outfile.write(padded_line)

            if padlines_cnt % 16 == 0:
                logger.debug("Block {0:d}: ".format(int(padlines_cnt / 16) + 1) + ('-' * 64))

            logger.debug("{} ({:02}): {}".format(padlines_cnt + 1,
                                                 (padlines_cnt % 16) + 1,
                                                 padded_line))

            padlines_cnt += 1

        logger.info("line count: {}".format(padlines_cnt))

        while padlines_cnt % 16 != 0:
            self._outfile.write(' ' * 64)
            padlines_cnt += 1

        logger.info("blocks count: {0:d}".format(int(padlines_cnt / 16)))


def signal_handler(signal, frame):
    logger = logger.getLogger(__name__)
    logger.debug("term received")
    sys.exit(0)


def main(arguments):
    signal.signal(signal.SIGTERM, signal_handler)

    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)

    parser.add_argument('infile', nargs='?', help="input file",
                        default=sys.stdin, type=argparse.FileType('r'))
    parser.add_argument('-o', '--outfile', help="output file",
                        default=sys.stdout, type=argparse.FileType('w'))
    parser.add_argument("-v", "--verbose", action="count",
                        help="increase output verbosity", default=0)

    args = parser.parse_args(arguments)

    logger = logging.getLogger(__name__)
    logger.setLevel(logging.DEBUG)
    logformatter = logging.Formatter('[ %(levelname)s ] %(message)s')

    # create console handler and set level to verbosity count
    ch = logging.StreamHandler()
    ch.setFormatter(logformatter)

    if args.verbose == 1:
        ch.setLevel(logging.INFO)
    elif args.verbose > 1:
        ch.setLevel(logging.DEBUG)
    else:
        ch.setLevel(logging.WARN)

    logger.addHandler(ch)

    # create file handler and set level to INFO
    fh = logging.FileHandler(filename='/tmp/{}.log'.format(os.path.basename(__file__)))
    fh.setLevel(logging.INFO)
    fh.setFormatter(logformatter)
    logger.addHandler(fh)

    try:
        exit_code = 0;

        processor = Processor(args.infile, args.outfile)
        if not processor:
            logger.critical("unable to process")
            exit(2)

        logger.info("Processing {} to {}".format(processor.infile, processor.outfile))

        processor.process(logger)

    except KeyboardInterrupt:
        logger.debug("quit")
        
    except RuntimeError as err:
        logger.error(str(err))
        exit_code = 1
        
    finally:
        logger.debug("done")

    return exit_code

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))

