# -*- coding: utf-8 -*-
################################################################################
#
# searchexception.py
#
# class SearchException: custom exception
#
################################################################################


class SearchException(Exception):
    def __init__(self, *args):
        Exception.__init__(self, *args)
