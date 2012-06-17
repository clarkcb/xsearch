################################################################################
#
# searchoption.py
#
# class SearchOption: encapsulates a (command-line) search option
#
################################################################################

class SearchOption:
    '''a class to encapsulate a specific command line option'''
    def __init__(self, shortarg, longarg, func, desc):
        self.shortarg = shortarg
        self.longarg = longarg
        self.func = func
        self.desc = desc

    @property
    def sortarg(self):
        if self.shortarg:
            return self.shortarg.lower()
        else:
            return self.longarg.lower()

