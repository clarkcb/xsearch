# -*- coding: utf-8 -*-

from .searchconfig import SearchConfig
from .searcher import Searcher
from .searchexception import SearchException
from .searchoptions import SearchOptions
from .searchresult import SearchResult, SearchResultFormatter, SearchResultSorter
from .searchsettings import SearchSettings
from pyfind import Color, ConsoleColor, FileResult, FileType, FindConfig

VERSION = '0.1.0'

__version__ = VERSION
__author__ = 'Cary Clark'
