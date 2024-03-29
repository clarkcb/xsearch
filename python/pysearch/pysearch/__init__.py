# -*- coding: utf-8 -*-

from .color import Color
from .config import XSEARCHPATH, SHAREDPATH
from .searcher import Searcher
from .searchexception import SearchException
from .searchoptions import SearchOptions
from .searchresult import SearchResult, SearchResultFormatter
from .searchsettings import SearchSettings
from pyfind import FileResult, FileType

__version__ = '0.1.0'
__author__ = 'Cary Clark'
