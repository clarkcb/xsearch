# -*- coding: utf-8 -*-
"""
###############################################################################
#
# searchconfig.py
#
# Configuration values
#
###############################################################################
"""
import importlib.resources
import os
from pyfind import FindConfig


def get_xsearch_config_dir() -> str:
    _home = os.getenv('HOME', '')
    _default_search_config_dir = os.path.join(_home, '.config', 'xsearch')
    _xsearch_config_dir = os.getenv('XSEARCH_CONFIG_DIR', _default_search_config_dir)
    return _xsearch_config_dir


class SearchConfig(FindConfig):
    """SearchConfig holds basic configuration."""

    __slots__ = ['search_options_path', 'default_search_settings_path']

    def __init__(self):
        """Create a new SearchConfig instance."""

        FindConfig.__init__(self)

        # pysearch data package resources
        _data = importlib.resources.files('pysearch').joinpath('data')
        _search_options_path = _data.joinpath('searchoptions.json')

        _xsearch_config_dir = get_xsearch_config_dir()
        _default_settings_path = os.path.join(_xsearch_config_dir, 'settings.json')

        self.search_options_path = _search_options_path
        self.default_search_settings_path = _default_settings_path
