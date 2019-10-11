# -*- coding: utf-8 -*-
###############################################################################
#
# common.py
#
# Common functionality
#
###############################################################################


def log(message: str):
    """log a message (for now just print to stdout)"""
    print(message)


def get_text(nodelist):
    """Get the text from an xml node"""
    rc = []
    for node in nodelist:
        if node.nodeType == node.TEXT_NODE:
            rc.append(node.data)
    return ''.join(rc)
