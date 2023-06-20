import 'dart:io' show Platform;

String xSearchPath = Platform.environment.containsKey('XSEARCH_PATH')
    ? Platform.environment['XSEARCH_PATH']!
    : '${Platform.environment['HOME']!}/src/xsearch';
String sharedPath = '$xSearchPath/shared';
String searchOptionsPath = '$sharedPath/searchoptions.json';
