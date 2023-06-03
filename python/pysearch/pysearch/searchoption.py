# -*- coding: utf-8 -*-
"""
###############################################################################
#
# searchoption.py
#
# class SearchOption: encapsulates a (command-line) search option
#
###############################################################################
"""


class SearchOption(object):
    """a class to encapsulate a specific command line option"""

    __slots__ = ['short_arg', 'long_arg', 'desc', 'func']

    def __init__(self, short_arg: str, long_arg: str, desc: str, func):
        self.short_arg = short_arg
        self.long_arg = long_arg
        self.desc = desc
        self.func = func

    @property
    def sort_arg(self):
        """Return a value to compare this option for sorting"""
        if self.short_arg:
            return self.short_arg.lower() + 'a' + self.long_arg.lower()
        return self.long_arg.lower()
