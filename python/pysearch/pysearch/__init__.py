# -*- coding: utf-8 -*-

from .config import XSEARCHPATH, SHAREDPATH
from .searcher import Searcher
from .searchexception import SearchException
from .searchoptions import SearchOptions
from .searchresult import SearchResult, SearchResultFormatter
from .searchsettings import SearchSettings
from pyfind import Color, FileResult, FileType

__version__ = '0.1.0'
__author__ = 'Cary Clark'
