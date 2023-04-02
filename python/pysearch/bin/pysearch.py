#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# pysearch
#
# The python (3.x) implementation of xsearch
#
###############################################################################
import asyncio
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__))[:-4])

if __name__ == '__main__':
    from pysearch.__main__ import main
    asyncio.run(main())
