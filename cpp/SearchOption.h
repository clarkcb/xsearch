#ifndef SEARCHOPTION_H
#define SEARCHOPTION_H

#include <cstdlib>
class SearchOption
{
public:
    SearchOption(string shortarg, string longarg, string description);

private:
    string shortarg;
    string longarg;
    string description;
};

#endif
