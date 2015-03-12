xsearch
=======

A multilingual file search utility


Overview
--------

This is a command line-based recursive file search utility written in a number
of different languages (for the reason behind this see History / Motivation).

Here's the current usage text for the Haskell version (the same for each
language except for executable name):

<pre>
Usage:
  hssearch [options] &lt;startpath&gt;

Options:
  -1,--firstmatch           Capture only the first file+pattern match
  -a,--allmatches           Capture all matches*
  --archivesonly            Search only archive files (also turns on searcharchives)
  -B,--linesafter           Number of lines to include after every matched line (default: 0)
  -b,--linesbefore          Number of lines to include before every matched line (default: 0)
  -d,--in-dirpattern        Specify name pattern for directories to include in search
  -D,--out-dirpattern       Specify name pattern for directories to exclude from search
  --debug                   Set output mode to debug
  --excludehidden           Exclude hidden files and directories*
  -f,--in-filepattern       Specify name pattern for files to include in search
  -F,--out-filepattern      Specify name pattern for files to exclude from search
  -h,--help                 Print this usage and exit
  --in-archiveext           Specify extension for archive files to include in search
  --in-archivefilepattern   Specify name pattern for archive files to include in search
  --in-linesafterpattern    Specify pattern to search the "lines-after" lines on
  --in-linesbeforepattern   Specify pattern to search the "lines-before" lines on
  --includehidden           Include hidden files and directories
  --linesaftertopattern     Specify pattern to collect lines after up to where pattern matched
  --linesafteruntilpattern  Specify pattern to collect lines after until pattern matched
  --listdirs                Generate a list of the matching directories after searching
  --listfiles               Generate a list of the matching files after searching
  --listlines               Generate a list of the matching lines after searching
  -m,--multilinesearch      Search files as single multi-line content block
  --out-archiveext          Specify extension for archive files to exclude from search
  --out-archivefilepattern  Specify name pattern for archive files to exclude from search
  --out-linesafterpattern   Specify pattern to filter the "lines-after" lines on
  --out-linesbeforepattern  Specify pattern to filter the "lines-before" lines on
  -P,--noprintmatches       Suppress printing of search results to stdout
  -p,--printmatches         Print search results to stdout*
  -R,--norecursive          Do not search recursively
  -r,--recursive            Search recursively through subdirectories*
  -s,--search               Specify search pattern
  -t,--dotiming             Measure search execution duration
  -u,--uniquelines          Save only search results with unique lines from previous results
  -v,--verbose              Set output mode to verbose
  -V,--version              Print version and exit
  -w,--maxlinelength        Max length of line of result to print out (default: 150)
  -x,--in-ext               Specify extension for files to include in search
  -X,--out-ext              Specify extension for files to exclude from search
  -Z,--nosearcharchives     Do not search archive files (bz2, gz, tar, zip)*
  -z,--searcharchives       Search archive files (bz2, gz, tar, zip)
</pre>

For example, say you want to perform a search according to these settings:

* search from the current directory (.)
* C# (.cs extension) files only
* lines matching the regex "\bLogin"
* file names matching the regex "Controller"
* directory names not matching the regex "temp"
* tell how long each part of the search process takes (time it)
* print a list of matching files after


You would use a command like this (using the Python version in this example):

    pysearch.py -x cs -f "Controller" -D "temp" -s "\bLogin" -t --listfiles .

If there were one match, the result output would look something like this:

    Elapsed time for get_search_dirs: 87.359 ms
    Elapsed time for get_search_files: 16.41 ms
    Elapsed time for search_files: 0.003 ms
    Total elapsed time: 103.772 ms

    Search results (1):
    ./path/to/CsController.cs: 99 [21:26]: var loginLabel = "Login";
    1 match for "\bLogin"

    Files with matches (1):
    ./path/to/CsController.cs


The verbosity level can be increased with the `-v/--verbose` option, which will
output the lists of directories and files to be searched, and even more with
the `--debug` option, which will also output the `SearchSettings` and other
debug info.

The completeness of the functionality of the tool varies from language to
language. I have three rough functionality groups:

* __Group I__ - Basic search functionality
  - Search binary files
  - Search text files by line
  - Search text files by content (as multi-line string)
  - Include/exclude hidden files
  - Determine file type as archive, binary, text, or unknown
  - Filter directories based on directory-specific name patterns
  - Filter files based on file extension and/or file-specific name patterns
  - Search single directory or recursively
  - Search a single file
  - Find all matches per file or only the first
  - Operate in normal, verbose or debug mode
  - Print search results
  - Print matching directories
  - Print matching files
  - Print all matching lines
  - Print unique matching lines
  - Ensure valid SearchSettings
  - Time the execution

* __Group II__ - Lines before/after functionality (implemented for searching by
  line and also by content)
  - Capture a specified number of lines before each match
  - Capture a specified number of lines after each match
  - Filter results by specific lines-before patterns
  - Filter results by specific lines-after patterns
  - Print search results in multi-line mode or single-line mode (depending on
    whether there are lines before or lines after in the result)

* __Group II.b__ - Lines after to/until functionality
  - Capture all lines after a match up to and including a line matching a
    specific "linesafterto" pattern, or exclude result if matching line not
    found
  - Capture all lines after a match up to but excluding a line matching a
    specific "linesafteruntil" pattern, or exclude result if matching line not
    found

* __Group III__ - Archive file searching
  - Turn archive file searching on or off
  - Filter archive files based on archive file extension and/or archive
    file-specific name patterns
  - Search regular files only, archive files only, or all matching files
  - Search zip files (also includes jar and war)
  - Search tar files
  - Search gz/tgz files
  - Search bz2 files
  - Search files by file object/handle/source/stream (facilitates searching
    of files contained in archive files)
  - Reference search files as SearchFile instances (allows for capturing
    archive file containers)
  - Print search results with container prefixes in filepath


Here's the breakdown of languages for each group:

* __Group I__: all languages except C++
* __Group II__: all languages except C++ and F#
* __Groups II.b and III__: Go, Python, Scala


Code Structure / Functionality
------------------------------

The basic code structure includes these elements:

* `FileTypes` - helps determine file type (archive, binary or text),
  searchability, etc.
* `FileUtil` - provides file-related functionality (e.g. get file extension,
  check whether file is hidden)
* `Searcher` - executes the file search based on the `SearchSettings`
* `SearchOptions` - loads option data from XML, generates usage text, builds a
  `SearchSettings` instance from command line arguments
* `SearchResult` - encapsulates a single search result
* `SearchSettings` - encapsulates the search settings, including what
  directories or files to include/exclude, search patterns, lines before or
  after, etc.


### Functionality Lifecycle ###

The functionality lifecycle goes something like this:

1. If the command (e.g. `hssearch`) is run by itself or with the `--help`
   argument, print usage text (to STDOUT) and exit.
1. If the command line does not include a `startpath` argument, print an error
   to this effect, followed by the usage text, then exit.
1. If no search patterns are provide in the arguments, print an error to this
   effect, followed by the usage text, then exit.
1. If any invalid/unknown arguments are provided, print an error to this effect,
   followed by the usage text, then exit.
1. If the `startpath` is not found, print an error to this effect, followed by
   the usage text, then exit.
1. If the `--debug` argument was included, print the `SearchSettings` instance.
1. The `startpath` can be a file or a directory. If it is a file, skip directly
   to the `SearchFile` function/method (described further down).
1. If `recursive` is `true` in `SearchSettings` (the default), then find all
   directories under `startpath`, filtering by any directory name patterns that
   were provided as arguments. These are the "search directories".
1. If `verbose` is `true` in `SearchSettings`, then print the list of search
   directories.
1. Find all files under the search directories, filtering by any file extensions
   and/or file name patterns that were provided as arguments. These are the
   "search files".
1. If `verbose` is `true` in `SearchSettings`, then print the list of search
   files.
1. For all search files (or for the single file if `startpath` is a file), call
   the `SearchFile` function/method.
1. The `SearchFile` function/method determines the type of file (archive,
   binary, text, or unknown), and will route to the appropriate file
   type-specific function/method, or do nothing if the file type is unknown.
1. If the file type is binary, route to the `SearchBinaryFile` function/method.
   `SearchBinaryFile` will return a single `SearchResult` for each search
   pattern that has a match anywhere in the binary file's bytes. The
   `SearchResult` will not  include line number or line text, for obvious
   reasons.
1. If the file type is text, route to `SearchTextFile`.
1. If `multilinesearch` is `true` in `SearchSettings`, route to
   `SearchTextFileContents`, otherwise route to the `SearchTextFileLines`.
1. `SearchTextFileContents` slurps the entire contents of the text file into a
   string variable and routes to `SearchMultilineString`.
   `SearchMultilineString` returns a list of `SearchResult` instances for all
   search patterns matches in the string.
1. `SearchTextFileLines` creates a line iterator on the file and routes to
   `SearchLines`. `SearchLines` returns a list of `SearchResult` instances for
   all search patterns matches in the lines.
1. If the file type is archive, and if either `archivesonly` or `searcharchives`
   is `true` in `SearchSettings`, and if the specific language version of the
   tool supports archive file searching, `SearchFile` routes to
   `SearchArchiveFile`. `SearchArchiveFile` in turn routes to the specific
   function/method that handles searching for the specific archive file type.
   `SearchZipFile` will be the example.
1. `SearchZipFile` gets the list of directories and files contained in the zip
   and filters that list based on any provided file extensions and
   directory/file name pattern arguments. The list of search files is then
   iterated through and individual files in the zip are searched using the steps
   described above based on file type and `SearchSettings`.
1. Once searching is complete, if `printresults` is `true` (the default), print
   out the `SearchResult` instances. If `linesbefore` or `linesafter` is greater
   than zero, the `SearchResult` will print out in a multiline mode, otherwise
   it will print out in a single line.
1. If `listdirs` is `true` in `SearchSettings`,
   print the unique list of directories containing files with matches.
1. If `listfiles` is `true` in `SearchSettings`, print the unique list of files
   with matches.
1. If `listlines` is `true` in `SearchSettings`, and if `uniquelines` is `true`,
   print the unique list of lines with matches. If `uniquelines` is `false`,
   print all lines with matches.



Installing / Running
--------------------

I have cloned `xsearch` to this path: _~/src/git/xsearch_. This is important to
know, because although I've tried to limit them, there are places in the code
(e.g. in the shell scripts that run the various versions, and also in the test
code) that reference this path and that will need to be adjusted to match a
different clone location.

To run the various language versions, I compile them (if necessary) and create a
soft link to the executable or script under _~/bin_, which I have included in
the `PATH` environment variable. For example, to run the Haskell version I
compiled it and then created the following soft link:

    $ cd ~/bin
    $ ln -s ~/src/git/xsearch/haskell/hssearch/dist/build/hssearch/hssearch


In some cases I have bash scripts under the root of some of the language
versions to facilitate running that version. I will probably add more of these
for consistency.

For each language version that you want to compile/run, you will need to install
the compilers/interpreters for those languages, unless the system already has
them installed (e.g. Perl, PHP, Python and Ruby on Linux / OSX).

I have a script under the _xsearch/shared_ directory called _build.sh_ that can
be used to compile any or all of the compiled language versions. For example,
you can build the Haskell version (after installing GHC) by running:

    $ cd ~/src/git/xsearch
    $ ./shared/build.sh haskell

You can also run it without a language argument to build all compiled versions
in one run.

Below is some additional info on installing/running some of the specific
language versions.


#### C# / F# ####

For writing the C# and F# code I use MS Visual Studio on a Windows 7 VM. To
compile/run/test them I use the [Mono](http://www.mono-project.com/)
environment installed on my OSX system. If you take a look at the
_shared/build.sh_ script you will see this command to compile the C# version:

    xbuild /p:Configuration=$CONFIGURATION $CSHARP_PATH/CsSearch/CsSearch.sln

Note that the `CONFIGURATION` variable is currently set to `Debug`, but this can
be changed to `Release` to create a release build.

To run `cssearch`, I created a soft link to a bash script under _CsSearch_:

    $ cd ~/bin
    $ ln -s ~/src/git/xsearch/csharp/CsSearch/cssearch.sh cssearch

Note that the defined path in that script will need to be altered if `xsearch`
is cloned to a different location than _~/src/git/xsearch_.

This information is similarly applicable for the F# version.


#### Clojure ####

Although you can download Clojure from http://clojure.org/, you won't need to.
Instead, retrieve the Leiningen build tool script from http://leiningen.org/
and run the `lein` command by itself to have it do a self-install of Clojure
and Leiningen. After the initial install, running the `lein` by itself will
show the options.

Leiningen works similarly to Maven. There's `lein clean` to do clean up,
`lein compile` to compile to class files, and `lein install` to install to the
local _.m2_ repository. To build a jar with dependencies included, use the
command `lein uberjar`.

To run `cljsearch`, I created a soft link to a bash script under _cljsearch_:

    $ cd ~/bin
    $ ln -s ~/src/git/xsearch/clojure/cljsearch/cljsearch

Note that the defined path in that script will need to be altered if `xsearch`
is cloned to a different location than _~/src/git/xsearch_.


#### Go ####

You can download Go from http://golang.org/. After you have installed it you
will have the `go` command available; run it by itself to get basic usage
instructions.

If you look in the _build.sh_ script in the `build_go()` function you will see
this command to build `gosearch`:

    go install elocale.com/clarkcb/xsearch/gosearch

Note also that I set the `GOPATH` environment variable in that function. This
is important for Go because it expects it to be defined and point to a standard
directory structure containing a _src_ directory with project-specific source
directories under that.

Note also that something called `gengosearchcode` is installed and ran before
the `gosearch` install. This executable generates some go code files from xml
that get used in `gosearch`.

The compiled executable gets created under _go/bin_. To run `gosearch` I then
create a soft link to it in my _~/bin_ directory:

    $ cd ~/bin
    $ ln -s ~/src/git/xsearch/go/bin/gosearch

Alternatively, you could add `$GOPATH/bin` to `PATH`.


#### Haskell ####

You can download Haskell from http://www.haskell.org/. After you have run the
installer you will have access to these commands:

* `ghc` - the compiler
* `ghci` - the REPL
* `cabal` - a tool for building, packaging, and managing dependencies

The `hssearch` version has a number of dependencies, which you will use `cabal`
to download and install. To start, I recommend running these commands first:

    $ cd ~/src/git/xsearch/haskell/hssearch
    $ cabal sandbox init

This will create a hidden directory called _.cabal-sandbox_, where the
dependencies will be downloaded to and built. You can then try compiling from
the same directory:

    $ cabal build

`cabal` will complain about missing dependencies. You will need to install
them. For example, for this specified dependency:

    --dependency='timeit=timeit-1.0.0.0-b5d83acfe823666e0ea6354a3ae30d03'

Run this `cabal` command:

    $ cabal install timeit

Once all dependencies satisfied, you should be able to build `hssearch`
successfully with `cabal build`. This will create the `hssearch` executable
under _dist/build/HsSearch/hssearch_, to which I then created a soft link under
_~/bin_.


#### Java / Scala ####

Maven is the tool used to build and manage dependencies for the Java and Scala
versions. You will find _pom.xml_ files at the roots of those source trees.

To build the `javasearch` and `scalasearch` versions, it is recommended to use
the _shared/build.sh_ script, because this script copies needed resource files
(_filetypes.xml_ and _searchoptions.xml_) before running the `mvn clean package`
command, which will create the executable jar under the _target_ directory.

To run these versions I have bash scripts at their project levels - _javasearch_
and _scalasearch_ - to which I then create soft links under _~/bin_. Note that
you will need to edit those scripts to change the path if `xsearch` is not
cloned under _~/src/git/xsearch_.

I do plan to switch the Scala version over to SBT but that hasn't happened yet.


#### Perl / PHP / Python / Ruby ####

Since Perl, PHP, Python and Ruby are interpreted languages, and since their
interpreters are installed by default on most Unix-style systems, there's not
much you will need to do to run these (on Windows you will need to download and
install them from http://perl.org/, http://php.net/, http://www.python.org and
http://www.ruby-lang.org/).

I don't recall having to install any dependencies, it should just be a matter
of running the scripts _plsearch.pl_, _phpsearch.php_,  _pysearch.py_ and
_rbsearch.rb_. I created soft links to each of these in _~/bin_.

I'm starting to work on creating installable packages of these versions, but I
still have work to do on that.

Also note: the python code is in 2.x style, I have not converted it to 3.x yet
but I plan to.


Style Checking
--------------

I have enabled lint-like style checking for some of the languages, and I
created a script under _shared_ called _lint.sh_ that can be used to exercise
the style checking on one or more of the language versions. The script can be
run with a language argument to run the style checker for that language, or
without a language to run it for all languages. For languages that don't
currently have style checking, the script will output a message to that effect.

Running style checking requires installing the tools for most of the languages.
Here's a list of what is currently being used:

* C# - Nothing yet
* Clojure - [eastwood](https://github.com/jonase/eastwood)
* F# - Nothing yet
* Go - The `go vet` command (included in the distribution)
* Haskell - [HLint](http://community.haskell.org/~ndm/darcs/hlint/hlint.htm)
* Java - [Checkstyle](http://checkstyle.sourceforge.net/)
* Node - [JSHint](http://jshint.com/)
* Perl - Nothing yet, although `use strict` and `use warnings` are in all Perl
  source files
* PHP - Nothing yet (I looked at [PHPLint](http://www.icosaedro.it/phplint/)
  but it seemed problematic)
* Python - [Pylint](http://www.pylint.org/)
* Ruby - [ruby-lint](https://github.com/YorickPeterse/ruby-lint)
* Scala - [Scalastyle](http://www.scalastyle.org/)


Testing
-------

I have implemented three different types of testing for almost all language
versions: basic, unit, and benchmark. 


#### Basic Testing ####

The _shared/test.sh_ test script was the first test I put together. I wanted a
way to run multiple versions of the tool and compare outputs, and that is what
this script will do. You can adjust the search settings to compare results for
different scenarios.


#### Unit Testing ####

I have added unit tests for almost all of the languages, and I created the
script _shared/unittest.sh_ to provide a common interface for running the tests
for the various language implementations, or for running all tests for all
languages at once.

Here is some language-specific unit test info:

* C# - Tests use the [`NUnit`](http://www.nunit.org/) framework. The tests are
  in the same solution but different project, called _CsSearchTests_. They are
  run by executing the _CsSearchTests.exe_ executable compiled from that
  project.
* Clojure - Tests use [`clojure.test`](https://clojure.github.io/clojure/clojure.test-api.html)
  and are run using the Leiningen command `lein test`.
* F# - No tests yet
* Go - Test files follow the standard naming convention: _[name]_test.go_. The
  tests use the [`testing`](http://golang.org/pkg/testing/) package. They are
  run using the `go test` command.
* Haskell - Tests use the [`HUnit`](https://hackage.haskell.org/package/HUnit)
  framework, which must be installed using the `cabal install` command. Test
  suite configuration is done in the cabal configuration file. Tests are run
  via the `cabal test` command.
* Java - Java tests use [`JUnit`](http://junit.org/) and are run via the Maven
  command `mvn test`.
* Node - [Nodeunit](https://github.com/caolan/nodeunit) was used to create and
  run tests for Node. You will need to install it.
* Perl - Tests in Perl use the [`Test::Simple`](http://perldoc.perl.org/Test/Simple.html)
  module and are run as regular Perl scripts.
* PHP - [PHPUnit](https://phpunit.de/) was used to create and run tests for PHP.
  You will need to install it.
* Python - Python tests use the [`unittest`](https://docs.python.org/2/library/unittest.html)
  module and are run as regular Python scripts.
* Ruby - Ruby tests use [`test/unit`](http://ruby-doc.org/stdlib-1.8.7/libdoc/test/unit/rdoc/Test/Unit.html)
  and are run as regular Ruby scripts.
* Scala - Scala tests use `JUnit` and [`scalatest`](http://www.scalatest.org/)
  and are run via the Maven command `mvn test`.


#### Benchmark Testing ####

I created the _shared/benchmark.py_ script to compare execution time across
versions. The script defines a number of "scenarios" (sets of arguments to test
different inputs and outputs) and the number of runs to perform. Each scenario
is run the specified number of times for each language version, and the Unix
`time` command is used to measure execution time. After each run, a table of
times and ranks for each version is printed out. At the end of all runs for a
given scenario, a totals and ranks table is printed for that scenario, and an
aggregate totals and ranks table is printed at the end of all runs for all
scenarios.

The results that I have seen so far from this testing have been somewhat
surprising. Here's an example aggregate totals table resulting from 3 scenarios
with 10 runs each:


    xsearch         real r.avg r.rank   sys s.avg s.rank   user u.avg u.rank   total t.avg t.rank
    ---------------------------------------------------------------------------------------------
    cljsearch      47.74  1.59     12  6.95  0.23     12  88.75  2.96     12  143.44  4.78     12
    cssearch        9.74  0.32      6  2.05  0.07      4   7.25  0.24      5   19.04  0.63      6
    fssearch       10.02  0.33      7  1.29  0.04      2   8.58  0.29      7   19.89  0.66      7
    gosearch        7.61  0.25      3  4.39  0.15     11   3.19  0.11      1   15.19  0.51      4
    hssearch        5.70  0.19      1  1.81  0.06      3   3.64  0.12      3   11.15  0.37      1
    javasearch     13.51  0.45      9  3.21  0.11     10  20.55  0.69      9   37.27  1.24      9
    nodesearch     11.59  0.39      8  2.66  0.09      8   8.69  0.29      8   22.94  0.76      8
    plsearch.pl     8.91  0.30      5  1.03  0.03      1   7.59  0.25      6   17.53  0.58      5
    phpsearch.php  23.72  0.79     10  2.77  0.09      9  20.56  0.69     10   47.05  1.57     10
    pysearch.py     5.97  0.20      2  2.12  0.07      5   3.63  0.12      2   11.72  0.39      2
    rbsearch.rb     7.65  0.26      4  2.57  0.09      7   4.80  0.16      4   15.02  0.50      3
    scalasearch    24.26  0.81     11  2.31  0.08      6  32.16  1.07     11   58.73  1.96     11


I should mention some disclaimers at this point:

* The versions vary in their levels of functionality, and that could certainly
  affect overall performance. Ideally all versions should have the same level
  of functionality for this test, and that is a long-term goal.
* This is a command-line tool, and certain usage patterns and results are to be
  expected. Some languages are more suited for this particular scenario than
  others.
* So far I have only run this script on my MacBook Pro running Yosemite.
  Different results are likely on other systems.


Now some brief comments for each version:

* `cljsearch` - I was surprised by how dramatically slower this version is compared
  to the rest. There's an interesting [blog post](https://nicholaskariniemi.github.io/2014/02/25/clojure-bootstrapping.html)
  that seems to give a good explanation of what's going on. I actually tried to
  improve performance by adding some `clojure.async` usage, but it actually made
  things worse, so I removed it. I'm still a big fan of the Clojure language,
  but it would seem that the language is not well suited for these types of
  scenarios.
* `cssearch` - I find it interesting that the CLR versions are quite a bit
  faster than the JVM versions. At some point I will need to search for startup
  time comparisons.
* `fssearch` - This version is dramatically faster than its JVM "counterpart"
  `scalasearch`. The F# version is behind in functionality, and the Scala
  version could probably use some optimization, but there is probably also a
  CLR vs JVM startup time thing going on here.
* `gosearch` - This version is certainly fast, but I expected it to be the
  fastest, or in the top two. I expected this because it is natively compiled,
  and also because I added some concurrency via channels. I can probably do
  more to optimize it, but I also wonder if there is some small amount of
  overhead with using channels.
* `hssearch` - Being that it is natively compiled, I expected this version to
  be one of the fastest, but I did not expect it to be **the** fastest. I have
  done nothing to optimize this version. I seem to recall seeing some mention
  of automatic parallelization, I wonder if that could help explain this.
* `javasearch` - JVM startup time can certainly help to explain why the JVM
  versions are a little slower, but because it is sort-of compiled I expected
  it to be faster than the interpreted languages. I think it probably goes back
  to languages being more or less suited for certain scenarios vs others.
* `nodesearch` - My disclaimer for this version is that I haven't done much to
  make this version asynchronous, even though that is standard practice in Node.
  At some point I will do this, and at that point I will pay more attention to
  its benchmark results.
* `plsearch.pl` - Perl might not be the fastest among the scripting languages,
  but it is faster than all of the CLR and JVM versions! It's clear that
  scripting languages like Perl are well suited to this usage scenario.
* `phpsearch.php` - This is the slowest of the scripting languages, but to be
  fair, PHP wasn't designed for this type of use. I can see why Facebook created
  `Hack`, though, and I'm actually somewhat interested in doing a comparison at
  some point, perhaps by creating a `Hack` version of the tool.
* `pysearch.py` - It turns out that this is not only the fastest of the
  scripting languages, but also the second fastest overall! GIL be damned, I'm
  impressed, and this more-or-less cements in my mind the idea that Python is
  an ideal language for these types of scenarios.
* `rbsearch.rb` - The Ruby version is very nearly as fast as the Python version;
  yet another way that the two are very similar. I want to bring the Ruby
  version up to the same level of functionality and run this again, but I
  expect to see similar results.
* `scalasearch` - JVM startup yadda yadda, but I also need to look at doing some
  optimizations. I have a feeling there are some big opportunities for
  optimization lurking in the scala code, perhaps through forcing laziness in
  some places and not in others. Regardless, I expect to be able to bring this
  version closer to the Java version in terms of performance, but I don't expect
  that this version will ever be one of the fastest.


History / Motivation
--------------------

Before becoming a full-time, "regular" programmer in 2010, I worked for many
years in software localization (L10n) and internationalization (i18n). As an
i18n consultant, I examined clients' source code and came up with
recommendations for changes to the source code to allow the software to become
localizable (translated and built in different language versions). That role
required being able to get up-to-speed on a client's source code, as well as
being able to identify the areas of concern in the code, quickly.

Naturally I used a lot of search tools in that job - find, grep, also text
editor and system search utilities - and was finding that none of those were
giving me exactly what I needed, which was a tool that had these features:

* Recursively search a given directory for files with lines matching a given
  (PCRE) regular expression
* Filter directories to search by name (regex)
* Filter files to search by extension and also by name (regex)
* Allow for capturing a specified number of lines before and/or after each match
* Filter matches based on "lines-before" and/or "lines-after" regex patterns
* Allow for searching archive (zip, tar, gz, etc.) files
* Present single-line results with filepath, line #, match start and end column
  indices, and the line that matched if no lines before or after are captured
* Present multi-line results with filepath, line #, match start and end column
  indices, and all of the lines of the result, including the matching line
  preceded by any lines before and followed by any lines after

I wrote the initial version of the tool in Python in the early 2000's. The
features described above came in piecemeal, with the archive file searching
coming a while later. I found the tool to be very useful for what I did, and I
have used it frequently ever since.

A number of years after I wrote the initial Python version, I got the idea to
rewrite the tool in other languages that I wanted to learn or practice. At the
time I had worked as a Java developer and was working as a C# developer, but
was also getting interested in other languages/platforms, such as Clojure,
Node.js, and F#. I wrote basic versions of the tool in those, and then also
decided to try writing versions in Java and Ruby. At this point I have also
written versions in Go, Haskell, PHP and Perl, and have been working to bring
everything up to more or less the same level of functionality.


Thoughts So Far
---------------

I have found the experience of rewriting the tool in various languages mostly
rewarding, for a number of reasons:

* It seems to be a fairly effective way to learn or improve basic language
  knowledge/skills.

* It provides an interesting opportunity to compare languages in a not entirely
  superficial way.

* I learned something new from each language that helped me find better
  approaches to problems across multiple implementations.

* The experience has helped me to form some opinions on what constitute (for me)
  preferable language features, but it has also given me an appreciation of each
  language that I have worked in.


So how do the different language versions compare? Here's my biased list of
current favorites:

* __hssearch__ - I really like the succintness of the Haskell code, and the
  readability is better than I thought it would be. Also, this version is the
  fastest. Perhaps that isn't too surprising, since Haskell and Go are the two
  natively compiled languages, but I'm a little surprised by how fast it is,
  and especially that is is quite a bit faster than the Go version, even
  without me having done any performance optimizations (concurrency, etc.).

* __gosearch__ - This version is also very fast, but I expected it to be the
  fastest, given that it is natively compiled but also that I added concurrency.
  In fact, it's the only language version that I have added concurrency to (so
  far), mainly because channels make it easy. Also, the available standard
  library packages made it fairly easy to implement some of the more advanced
  features, like archive file searching.

* __scalasearch__ - I first wrote this version when I wanted to learn Scala and
  was looking at a possible Scala development opportunity. I did end up working
  in it professionally for several years, and in that time my functional
  language skills improved a fair amount, and it's been interesting for me to
  see how this version has evolved. I should mention that this version is one of
  the slower ones based on what I've seen so far, which is probably partially
  due to JVM startup time, but I'm also certain that there are a number of
  optimizations that can be done in the code.

* __pysearch.py__ - When I use this tool, which I still do regularly, I usually
  end up using the Python version. That's mostly out of habit, but also because
  as the first language version I still sort of treat it as the reference
  version, and try to keep it up-to-date with new functionality. Also, I
  recently discovered that it is apparently the 2nd fastest version! (See the
  Benchmark Testing section for details.)


Also, here are some thoughts on each language that I have done implementations
for so far:

* [C#](http://en.wikipedia.org/wiki/C_Sharp_%28programming_language%29) -
  C# had the benefit of learning from Java's strengths and weaknesses, but it
  has also evolved beyond Java in a number of ways that I appreciate:
  the incorporation of extension methods, lambdas (which java 8 adds) and LINQ.
  I think the .NET libraries are a little stronger than the Java standard ones
  too, at least in some cases. Although it is an imperative/OO language, it
  does have some nice functional features with lambdas and LINQ.

* [Clojure](http://clojure.org/) -
  Clojure was my first exposure to a language in the Lisp family. It was also my
  first exposure to coding in a predominantly functional language. I found that
  developing in it was very slow for me, especially at first, but I also enjoyed
  it because it really forced me to think about solving problems very
  differently than I was used to, and the end solutions felt succinct and
  elegant. Although I really like Clojure as a language, I have also discovered
  that it takes a long time to start up (seeing the Benchmark Testing section
  for more info), and so I will likely not use it again for this type of project.
  I do think it's a good language choice for other scenarios though, e.g. a
  long-running filtering process of some kind.

* [F#](http://fsharp.org/) -
  I started looking at F# after working for a while in C#. I liked the syntax
  right away, but I had a hard time working in it at first, because I couldn't
  understand the errors that Visual Studio was giving me. The language feels to
  me sort of like a cross between Python and C# with lots of LINQ. In comparison
  with Scala, I think F# is cleaner syntactically, although object-oriented code
  seems to be a little clunkier in it. My personal feeling is that F# *should*
  be the successor to C# on the CLR, but I'm not sure that will happen. I
  believe that the larger programming community will eventually swing away from
  imperative and towards functional, but my impression is that F# doesn't have
  the same momentum in the CLR world as Scala does in the JVM world. Time will
  tell.

* [Go](https://golang.org/) -
  I was really surprised how fast I could write Go code, I reached near Python
  speeds, which I think is in part due to the tools and conventions (e.g.
  standard directory structure) but also the straightforwardness of the
  language; think C/C++ but cleaner and easier to understand. I like the way OO
  is done in Go, it seems like an elegant solution. I also really like channels,
  they are a surprisingly simple way to add concurrency. On the minus side,
  lack of generics is a little disappointing, as is the lack of any functional
  capabilities.

* [Haskell](https://www.haskell.org/) -
  Haskell was (and still is) a mind bender to learn, but I'm becoming more and
  more a fan of functional programming, and Haskell seems to be the king. I
  love the succinctness of the language and the way it forces you to approach
  problems very differently. I'm also impressed with its type system. When I
  first started working in Haskell I found myself missing OO sometimes, but this
  diminished over time as I got more experience in it. I still need to tackle
  the harder category theory-related subjects, but I plan to and will definitely
  do more coding in Haskell.

* [Java](http://en.wikipedia.org/wiki/Java_%28programming_language%29) -
  I wrote the Java version after the Scala version, and frankly it was a little
  hard to work in Java after having had several years of Scala experience. The
  same functionality in Java will often require much more verbosity and end up
  being less readable as well as more error-prone. Java deserves respect for
  being a vast improvement over C/C++ in terms of ease of coding, cross-platform
  deployment, and encoding support, and also for being the giant in the
  enterprise software world, but my personal feeling is that Java will gradually
  lose out to more functional languages like Scala. Java version 8 might delay
  that transition a little, although in the end I think it will actually help
  influence the transition.

* [Node.js](http://nodejs.org/) -
  A platform for running JavaScript "in the wild," I think Node.js is pretty
  neat, and quite a bit faster than I expected it to be, even while not going
  "fully async" (which I currently have not). Node.js and AJAX have helped me
  hate JavaScript less. Also, `npm` seems like a good package manager. In
  theory I also like the idea that a developer can use one language for
  front-end and back-end development of web apps. Still, JavaScript just isn't
  that compelling of a language in my opinion. It's very quirky in places
  (e.g. some strange math behaviors) and it is missing some things that I
  consider standard (e.g. sprintf-like functionality). Sure, you can add what
  you need through prototyping, but that feels a little like reinventing the
  wheel. I do want to do more work with async/non-blocking calls, though.

* [Perl](http://perl.org/) - Perl was my first scripting language, and the
  first language I did any real programming in. It was also the language that
  caused me to learn regular expressions, and I think it is still the language
  that sets the bar for regex implementation. I went away from Perl for a while
  to work in PHP, JavaScript, and later Python and Java. When I came back to it
  I made a decision to switch to Python as my scripting language of choice
  because of what I saw as a major increases in readability and speed of
  development. I still like Perl, and writing the Perl version was pretty quick
  and relatively painless (it helped that I could reference the PHP version) but
  I do find the syntax to be pretty awkward and clunky. I will likely give Perl
  6 a try when it finally arrives, but I have a feeling that I won't be doing
  too much in Perl down the road.

* [PHP](http://php.net/) - I have a fair amount of web dev experience with PHP,
  but had never done any non-web/CLI work in it, and I had also never worked
  with PHP 5 classes. Also, I wanted to see how fast I could put a PHP version
  together. The experience was actually not too painful. PHP is not my favorite
  language for sure - it feels haphazardly designed, the syntax is only slightly
  cleaner than Perl's and the way that namespaces are implemented bugs me - but
  the class implementation is fine, and I discovered that PHP has some
  functional aspects to it, such as first-class functions and some
  functional-style functions (`array_filter`, `array_map`, etc.). I got a basic
  working version (everything except linesafterto/until and archive file
  searching) done in a little over a day.

* [Python](https://www.python.org/) -
  This was my 2nd go-to scripting language (after Perl), and the first language
  that I wrote this tool in. I'm a fan of Python. I find Python code to be
  naturally very clean/readable, it has a good standard library, and it's super
  fast to develop in. That makes it a great language for writing utilities that
  go beyond shell scripting requirements, and it's also great for prototyping.
  Python does have some limited but nice functional capabilities too, such as
  lambdas and for-comprehensions. I find it and Ruby to be very similar, with
  pluses and minuses on both sides (more on that below), but suffice it to say
  that I would probably choose to use Python over Ruby for tools like this one,
  not because I think it's superior, but because I have more experience in it
  and prefer some of its syntax.

* [Ruby](https://www.ruby-lang.org/) -
  I first learned some Ruby when I was taking a look at Ruby On Rails. I was
  already a regular Python user, so a lot of my reactions to it were
  comparitive in nature. Mostly what I found is that it is a nice language that
  is quite similar to Python in a lot of respects, with the obvious superficial
  syntactic differences and different libraries but also with some relative
  strengths and weaknesses. On the strengths side, Ruby is a pure OO language,
  whereas Python is not. I haven't found that to be an issue in Python
  development, but if I had come from Ruby first I might. Also, Ruby's
  functional capabilities seem to be slightly more robust than Python's. Its
  code blocks seem to be more flexible than Python's lambdas, and libraries
  with higher-order function handling seem to be more prevalent. As far as
  relative weaknesses, Ruby's Unicode support is weaker than Python's (last I
  checked), which isn't a big deal for most people but it's an important
  consideration for me. In terms of personal preference, I prefer Python's
  indentation-based nesting over the need to use the "end" token to close
  nested blocks in Ruby.

* [Scala](http://www.scala-lang.org/) -
  I had been a Java and C# developer before getting a job in Scala, and although
  there was a learning curve, I found the transition to Scala to be fairly
  smooth overall. I was able to start with a more Java-like style (OO +
  imperative), but then I could slowly migrate to a more functional style as I
  started to understand it and appreciate it more. This is how the language was
  designed - allow imperative but encourage functional - and in my opinion it
  makes Scala the natural successor to the JVM throne after Java. The tipping
  point will be when enough developers have understood the benefits of
  functional programming that the larger industry starts to push for a shift to
  those languages.


As far as what my ideal language looks like, here's my list of optimal features:

* Functional - Now that I have become familiar with functional programming, I
  find myself wanting to use that style as much as possible (where applicable
  and within reason of course). I don't mean to say that a language has to be
  100% functional, but just provide non-trival "functional functionality".

* Statically-typed - The more I program the more I find myself annoyed when
  working in a dynamically typed language because I can't always immediately
  tell the type of a variable or the return type of a function. Also, dynamic
  typing seems more error-prone.

* Generics - Unless I'm in a dynamically-typed language, I find generics (or
  something that achieves the same result) to be a near must-have. I shouldn't
  have to rewrite the same function each time I want to get the same
  functionality for different input types.

* Object-oriented - Give me functional over imperative, but I would still like
  to be able to create classes, etc. Yes, you can get a lot of that with structs
  and records, and Haskell taught me that OO wasn't absolutely essential, but I
  still like it enough to miss it a little in a non-OO language.

* Robust library / framework - This is probably an obvious point, but I
  think it's still worth mentioning.

* Quick to develop in - Any language will be quick to develop in for the
  developer that has a lot of experience with that language, but I think it
  says a lot about a language when it is quick to develop in with minimal
  knowledge and experience.

* Comfortable - This is just the idea of having a level of familiarity with a
  language that makes the developer feel very comfortable with the language.
  This comes with experience, but depending on the language it can start to feel
  comfortable quickly.

* High-performance - High performance isn't always needed, but when it is, that
  is a point where languages really start to differentiate themselves.


Based on these features, the overall "winners" for me so far are:

1. Haskell
   - Functional - yes
   - Statically-typed - yes
   - Generics - Haskell's type system achieves this result (and more)
   - Object-oriented - no
   - Robust library - not sure yet
   - Quick to develop in - yes, although it took a while to get there
   - Comfortable - getting there
   - High-perormance - yes

2. Go
   - Functional - no
   - Statically-typed - yes
   - Generics - no
   - Object-oriented - yes
   - Robust library - seems to be
   - Quick to develop in - yes, surprisingly so
   - Comfortable - fairly
   - High-perormance - yes


to be continued . . . 


Plans / TODOs
-------------

My current TODOs:

* Rewrite the Python version for Python 3.x
* Rewrite the Node.js version using more async/non-blocking calls
* Switch from Maven to SBT for Scala
* Take a look at performance optimizations in the various languages
* Implement a C++ version (?)


Some longer-term and/or tentative TODOs:

* Implement archive file searching in versions that don't currently have it
* Add new features (ideas pop up periodically but no list exists currently)
* Internationalize the versions (???) -- this used to be my job, so even though
  it seems like overkill in some respects it also feels like something I
  *should* do.


Other than that, I don't have specific plans for other language rewrites, but
I'm sure they will happen. I'm not entirely sure which languages I would do
implementations for yet, but I have several different ideas for ones to choose:

1. ~~Choose a language I (used to) know to see how quickly I can implement that
   version.~~ I think I have run out of languages that I know or used to know
   and haven't implemented the tool in.
2. Choose a language I don't know because it seems to be up-and-coming as well
   as interesting.
3. Choose a language I don't know because it is different from other languages
   I have done and therefore gives new challenges.


Languages that I'm currently considering (listed alphabetically):

* [Common Lisp](http://en.wikipedia.org/wiki/Common_Lisp) [3] - After some
  exposure to Clojure and Scheme/Racket, I've become intrigued with the Lisp
  family. I'm interested in learning about macros, and also interested to see
  how the language's multiparadigm nature plays out.

* [D](http://dlang.org/) [2] - I know next to nothing about D, other than it is
  intended as a replacement for C/C++ and it has a small but loyal following.

* [Erlang](http://www.erlang.org/) [3] - I'm most interested in the message
  passing feature of Erlang. I had a small amount of exposure to a similar
  approach when using Akka with Scala, and I liked how it worked. Other than
  that, it seems vaguely like Haskell but without types, which sounds bad but
  I'd like to try it out before writing it off.

* [OCaml](http://caml.inria.fr/ocaml/) [2] - I'm interested to compare OCaml to
  its close cousin F#, but also compare it to other functional languages that I
  have worked in, such as Haskell, and other functional/OO hybrid languages that
  I have worked in, such as Scala.

* [Rust](http://www.rust-lang.org/) [2,3] - I don't know much about Rust yet,
  but from the little bit I've seen it looks interesting. It seems to be
  positioned as replacement for C/++ with functional aspects, so maybe a little
  like Go + functional? I'm not sure but I'm curious.

* [Squeak](http://en.wikipedia.org/wiki/Squeak) [3] - I have known some
  developers who worked in Smalltalk professionally, and they were huge fans of
  it. As the OO language that influenced the rest I'm pretty interested to see
  what it's like too.

* [Swift](https://developer.apple.com/swift/) [2] - Another functional/OO hybrid,
  Apple's replacement for Objective C isn't quite ready for primetime from what
  I've been hearing, but from the little bit I've read about the language it
  looks like it'll be nice to work in when they work out the kinks.


It is possible there will be other languages besides those, but there are some
languages that I don't currently see myself ever doing or am quite sure I won't:

* Ada
* Bash
* Cobol
* Delphi/Pascal
* Forth
* Fortran
* Groovy
* Lua
* Objective-C
* PowerShell
* Rexx
* Scheme/Racket
* Tcl
* VB

