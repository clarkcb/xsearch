import 'dart:io' show Platform;

// TODO: switch fallback path to invalid path to force setting env var
String xSearchPath = Platform.environment.containsKey('XSEARCH_PATH')
    ? Platform.environment['XSEARCH_PATH']
    : '/Users/cary/src/xsearch';
String sharedPath = '$xSearchPath/shared';
String fileTypesPath = '$sharedPath/filetypes.json';
String searchOptionsPath = '$sharedPath/searchoptions.json';
