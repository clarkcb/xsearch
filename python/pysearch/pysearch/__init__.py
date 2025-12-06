# -*- coding: utf-8 -*-

from .config import XSEARCHPATH, SHAREDPATH
from .searcher import Searcher
from .searchexception import SearchException
from .searchoptions import SearchOptions
from .searchresult import SearchResult, SearchResultFormatter, SearchResultSorter
from .searchsettings import SearchSettings
from pyfind import Color, ConsoleColor, FileResult, FileType

VERSION = '0.1.0'

__version__ = VERSION
__author__ = 'Cary Clark'
