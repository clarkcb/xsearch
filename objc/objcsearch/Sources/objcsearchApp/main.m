#import <Foundation/Foundation.h>
#import "common.h"
#import "Searcher.h"
#import "SearchOptions.h"
#import "SearchResult.h"
#import "SearchResultFormatter.h"
#import "SearchSettings.h"

NSArray* argvToNSArray(int argc, const char * argv[]) {
    NSMutableArray *args = [NSMutableArray array];
    for (int i = 0; i < argc; i++) {
        NSString *arg = [[NSString alloc] initWithCString:argv[i] encoding:NSUTF8StringEncoding];
        [args addObject:arg];
    }
    return [NSArray arrayWithArray:args];
}

void handleError(NSError *error, BOOL colorize, SearchOptions *options) {
    logMsg(@"");
    logErrorColor(error.domain, colorize);
    [options usage:1];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;

        SearchOptions *options = [[SearchOptions alloc] init];

        NSArray *args = argvToNSArray(argc, argv);

        SearchSettings *settings = [options settingsFromArgs:args error:&error];

        if (error) {
            handleError(error, settings.colorize, options);
        }
        
        if (settings.debug) {
            logMsg([NSString stringWithFormat:@"\nsettings: %@", settings]);
        }

        if (settings.printUsage) {
            [options usage:0];
        }

        Searcher *searcher = [[Searcher alloc] initWithSettings:settings error:&error];

        if (error) {
            handleError(error, settings.colorize, options);
        }

        NSArray<SearchResult *> *results = [searcher search:&error];

        if (error) {
            handleError(error, settings.colorize, options);
        }

        SearchResultFormatter *formatter = [[SearchResultFormatter alloc] initWithSettings:settings];

        if (settings.printResults) {
            [searcher printSearchResults:results formatter:formatter];
        }

        if (settings.printDirs) {
            [searcher printMatchingDirs:results formatter:formatter];
        }

        if (settings.printFiles) {
            [searcher printMatchingFiles:results formatter:formatter];
        }

        if (settings.printLines) {
            [searcher printMatchingLines:results formatter:formatter];
        }
    }
    return 0;
}
