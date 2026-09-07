import 'dart:io' show Platform;
import 'package:dartfind/dartfind.dart';

class SearchConfig extends FindConfig {
  String xSearchConfigDir = "";
  String xSearchPath = "";
  String searchOptionsPath = "";
  String defaultSearchSettingsPath = "";

  SearchConfig() {
    xSearchConfigDir = Platform.environment.containsKey('XSEARCH_CONFIG_DIR')
        ? Platform.environment['XSEARCH_CONFIG_DIR']!
        : '${Platform.environment['HOME']!}/.config/xsearch';

    xSearchPath = Platform.environment.containsKey('XSEARCH_PATH')
        ? Platform.environment['XSEARCH_PATH']!
        : '${Platform.environment['HOME']!}/src/xsearch';

    searchOptionsPath = '$xSearchPath/shared/searchoptions.json';
    defaultSearchSettingsPath = '${xSearchConfigDir}/settings.json';
  }
}
