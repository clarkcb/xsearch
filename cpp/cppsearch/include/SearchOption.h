#ifndef CPPSEARCH_SEARCHOPTION_H
#define CPPSEARCH_SEARCHOPTION_H

using namespace std;

class SearchOption {
public:
    const string* shortarg;
    const string* longarg;
    const string* description;
    const string* sortarg;
    SearchOption(const string* shortarg, const string* longarg, const string* description);
};

#endif //CPPSEARCH_SEARCHOPTION_H
