#ifndef SEARCHOPTIONS_H
#define SEARCHOPTIONS_H
#include "SearchOption.h"
#include "SearchSettings.h"

class SearchOptions
{
public:
    SearchOptions();
    void usage();
    
    void searchsettings_from_args(int &argc, char *argv[], SearchSettings &settings);

private:
    SearchOption *arg_options[];
    SearchOption *flag_options[];
};

#endif
