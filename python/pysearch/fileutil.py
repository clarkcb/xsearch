# -*- coding: utf-8 -*-
################################################################################
#
# fileutil.py
#
# class FileUtil: provides utility functions for getting file extension and
#                 determining file type
#
################################################################################
import os
import xml.dom.minidom as minidom

class FileType:
    Archive = 1
    Binary  = 2
    Text    = 3
    Unknown = 4

class FileUtil:
    """a file helper class"""

    # TODO: move to a config file
    FILETYPESPATH = '~/src/git/xsearch/shared/filetypes.xml'

    def __init__(self, **kargs):
        self.filetypespath = os.path.expanduser(self.FILETYPESPATH)
        self.filetypes = {}
        self.__dict__.update(kargs)
        self.populate_filetypes()

    def get_filetype(self, filename):
        if self.is_text_file(filename):
            return FileType.Text
        elif self.is_binary_file(filename):
            return FileType.Binary
        elif self.is_archive_file(filename):
            return FileType.Archive
        else:
            return FileType.Unknown

    def get_text(self, nodelist):
        rc = []
        for node in nodelist:
            if node.nodeType == node.TEXT_NODE:
                rc.append(node.data)
        return ''.join(rc)

    def populate_filetypes(self):
        types = 'archive binary nosearch text unknown xml'.split()
        filetypedom = minidom.parse(self.filetypespath)
        filetypenodes = filetypedom.getElementsByTagName('filetype')
        for filetypenode in filetypenodes:
            name = filetypenode.getAttribute('name')
            extnode = filetypenode.getElementsByTagName('extensions')[0]
            exts = set(self.get_text(extnode.childNodes).split())
            self.filetypes[name] = exts
        self.filetypes['text'] = \
            self.filetypes['text'].union(self.filetypes['code'],
                self.filetypes['xml'])
        self.filetypes['searchable'] = \
            self.filetypes['binary'].union(self.filetypes['archive'],
                self.filetypes['text'])

    def get_extension(self, filename):
        """Returns the extension for a given filename, if any, else empty 
           string"""
        ext = ''
        if os.path.basename(filename).rfind('.') > 0:
            ext = filename.split('.')[-1]
        return ext.lower()

    def is_archive_file(self, f):
        """Return true if file is of a (known) archive file type"""
        return (self.get_extension(f) in self.filetypes['archive'])

    def is_binary_file(self, f):
        """Return true if file is of a (known) searchable binary file type"""
        return (self.get_extension(f) in self.filetypes['binary'])

    def is_searchable_file(self, f):
        """Return true if file is of a (known) searchable type"""
        return (self.get_extension(f) in self.filetypes['searchable'])

    def is_text_file(self, f):
        """Return true if file is of a (known) text file type"""
        return (self.get_extension(f) in self.filetypes['text'])
