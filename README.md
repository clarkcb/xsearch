xsearch
=======

A multilingual file search utility


Overview
--------

This is a command line-based recursive file search utility written in a number of
different languages (for the reason behind this see History / Motivation).

Here's the current usage text for the Haskell version (the same for each
language except for executable name):

<pre>
Usage:
  hssearch [options] &lt;startpath&gt;

Options:
  -1,--firstmatch           Capture only the first match for a file+search combination
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
  --in-linesafterpattern    Specify pattern to search the "lines-after" lines on (used with --linesafter)
  --in-linesbeforepattern   Specify pattern to search the "lines-before" lines on (used with --linesbefore)
  --includehidden           Include hidden files and directories
  --linesaftertopattern     Specify pattern to collect lines after up to where pattern is matched (or EOF)
  --linesafteruntilpattern  Specify pattern to collect lines after until pattern is matched (or EOF)
  --listdirs                Generate a list of the matching directories after searching
  --listfiles               Generate a list of the matching files after searching
  --listlines               Generate a list of the matching lines after searching
  -m,--multilinesearch      Search files as single multi-line content block
  --out-archiveext          Specify extension for archive files to exclude from search
  --out-archivefilepattern  Specify name pattern for archive files to exclude from search
  --out-linesafterpattern   Specify pattern to filter the "lines-after" lines on (used with --linesafter)
  --out-linesbeforepattern  Specify pattern to filter the "lines-before" lines on (used with --linesbefore)
  -P,--noprintmatches       Suppress printing of search results to stdout
  -p,--printmatches         Print search results to stdout*
  -R,--norecursive          Do not search recursively
  -r,--recursive            Search recursively through subdirectories*
  -s,--search               Specify search pattern
  -t,--dotiming             Measure search execution duration
  -u,--uniquelines          Save only search results that have unique lines from previous search results
  -v,--verbose              Set output mode to verbose
  -V,--version              Print version and exit
  -w,--maxlinelength        Maximum number of characters to print out per line of result (default: 150)
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


The verbosity level can be increased with the <code>-v/--verbose</code> option,
which will output the lists of directories and files to be searched, and even
more with the <code>--debug</code> option, which will also output the
<code>SearchSettings</code> and other debug info.

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


Here's the breakdown of languages for each group.

* __Group I__: all languages except C++ and F#
* __Group II__: all languages except C++ and F#
* __Group II.b__: Go, Python, Scala
* __Group III__: Go, Python, Scala


Code Structure / Functionality
------------------------------

The basic code structure includes these elements:

* <code>FileTypes</code> - helps determine file type (archive, binary or text),
  searchability, etc.
* <code>FileUtil</code> - provides file-related functionality (e.g. get file
  extension, check whether file is hidden)
* <code>Searcher</code> - executes the file search based on the
  <code>SearchSettings</code>
* <code>SearchOptions</code> - loads option data from XML, generates usage text,
  builds a <code>SearchSettings</code> instance from command line arguments
* <code>SearchResult</code> - encapsulates a single search result
* <code>SearchSettings</code> - encapsulates the search settings, including what
  directories or files to include/exclude, search patterns, lines before or
  after, etc.


### Functionality Lifecycle ###

The functionality lifecycle goes something like this:

1. If the command (e.g. <code>hssearch</code>) is run by itself or with the
   <code>--help</code> argument, print usage text (to STDOUT) and exit.
1. If the command line does not include a <code>startpath</code> argument, print
   an error to this effect, followed by the usage text, then exit.
1. If no search patterns are provide in the arguments, print an error to this
   effect, followed by the usage text, then exit.
1. If any invalid/unknown arguments are provided, print an error to this effect,
   followed by the usage text, then exit.
1. If the <code>startpath</code> is not found, print an error to this effect,
   followed by the usage text, then exit.
1. If the <code>--debug</code> argument was included, print the
   <code>SearchSettings</code> instance.
1. The <code>startpath</code> can be a file or a directory. If it is a file,
   skip directly to the <code>SearchFile</code> function/method (described
   further down).
1. If <code>recursive</code> is <code>true</code> in <code>SearchSettings</code>
   (the default), then find all directories under <code>startpath</code>,
   filtering by any directory name patterns that were provided as arguments.
   These are the "search directories".
1. If <code>verbose</code> is <code>true</code> in <code>SearchSettings</code>,
   then print the list of search directories.
1. Find all files under the search directories, filtering by any file extensions
   and/or file name patterns that were provided as arguments. These are the
   "search files".
1. If <code>verbose</code> is <code>true</code> in <code>SearchSettings</code>,
   then print the list of search files.
1. For all search files (or for the single file if <code>startpath</code> is a
   file), call the <code>SearchFile</code> function/method.
1. The <code>SearchFile</code> function/method determines the type of file
   (archive, binary, text, or unknown), and will route to the appropriate
   file type-specific function/method, or do nothing if the file type is unknown.
1. If the file type is binary, route to the <code>SearchBinaryFile</code>
   function/method. <code>SearchBinaryFile</code> will return a single
   <code>SearchResult</code> for each search pattern that has a match anywhere
   in the binary file's bytes. The <code>SearchResult</code> will not  include
   line number or line text, for obvious reasons.
1. If the file type is text, route to <code>SearchTextFile</code>.
1. If <code>multilinesearch</code> is <code>true</code> in
   <code>SearchSettings</code>, route to <code>SearchTextFileContents</code>,
   otherwise route to the <code>SearchTextFileLines</code>.
1. <code>SearchTextFileContents</code> slurps the entire contents of the text
   file into a string variable and routes to <code>SearchMultilineString</code>.
   <code>SearchMultilineString</code> returns a list of <code>SearchResult</code>
   instances for all search patterns matches in the string.
1. <code>SearchTextFileLines</code> creates a line iterator on the file and
   routes to <code>SearchLines</code>. <code>SearchLines</code> returns a list
   of <code>SearchResult</code> instances for all search patterns matches in
   the lines.
1. If the file type is archive, and if either <code>archivesonly</code> or
   <code>searcharchives</code> is <code>true</code> in <code>SearchSettings</code>,
   and if the specific language version of the tool supports archive file
   searching, <code>SearchFile</code> routes to <code>SearchArchiveFile</code>.
   <code>SearchArchiveFile</code> in turn routes to the specific function/method
   that handles searching for the specific archive file type. We will use
   <code>SearchZipFile</code> as our example.
1. <code>SearchZipFile</code> gets the list of directories and files contained in
   the zip and filters that list based on any provided file extensions and
   directory/file name pattern arguments. The list of search files is then
   iterated through and individual files in the zip are searched using the steps
   described above based on file type and <code>SearchSettings</code>.
1. Once searching is complete, if <code>printresults</code> is <code>true</code>
   (the default), print out the <code>SearchResult</code> instances. If
   <code>linesbefore</code> or <code>linesafter</code> is greater than zero, the
   <code>SearchResult</code> will print out in a multiline mode, otherwise it
   will print out in a single line.
1. If <code>listdirs</code> is <code>true</code> in <code>SearchSettings</code>,
   print the unique list of directories containing files with matches.
1. If <code>listfiles</code> is <code>true</code> in <code>SearchSettings</code>,
   print the unique list of files with matches.
1. If <code>listlines</code> is <code>true</code> in <code>SearchSettings</code>,
   and if <code>uniquelines</code> is <code>true</code>, print the unique list
   of lines with matches. If <code>uniquelines</code> is <code>false</code>,
   print all lines with matches.



Installing / Running
--------------------

I have xsearch cloned to this path: _~/src/git/xsearch_. To run the various
language versions, I compile them (if necessary) and create a soft link to the
executable or script under _~/bin_, which I have included in the <code>PATH</code>
environment variable. For example, to run the Haskell version I compiled it and
then created the following soft link:

    $ cd ~/bin
    $ ln -s ~/src/git/xsearch/haskell/hssearch/dist/build/hssearch/hssearch


In some cases I have bash scripts under the root of some of the language
versions to facilitate running that version. I will probably add more of these
for consistency.

For each language version that you want to compile/run, you will need to install
the compilers/interpreters for those languages, unless that system already has
them installed (e.g. php, python and ruby on Linux / OSX).

I have a script under the _xsearch/shared_ directory called _build.sh_ that can
be used to compile any or all of the compiled language versions. For example,
you can build the Haskell version (after installing GHC) by running:

    $ cd ~/src/git/xsearch
    $ ./shared/build.sh haskell

You can also run it without a language name to build all compiled versions in
one run.

I have also included a basic test script - _.shared/test.sh_ - that can be run
to test an individual language version or test and compare all versions in a
single run. Edit that script to see how to run it and also to change test
parameters.

Below is some additional info for some of the languages.


### C# / F# ###

For writing the C# and F# code I use MS Visual Studio on a Windows 7 VM. To
compile/run/test them I use the [Mono](http://www.mono-project.com/)
environment installed on my OSX system. If you take a look at the
_shared/build.sh_ script you will see this command to compile the C# version:

    xbuild /p:Configuration=Debug $CSHARP_PATH/CsSearch/CsSearch.sln

You can change <code>Configuration</code> to <code>Release</code> if you want to
create a release build.

To run the <code>cssearch</code> version, I created a soft link to a bash script
under _CsSearch_:

    $ cd ~/bin
    $ ln -s ~/src/git/xsearch/csharp/CsSearch/cssearch.sh cssearch

This information is similarly applicable for the F# version.


### Clojure ###

You can download Clojure from http://clojure.org/, but you won't need to.
Instead, retrieve the Leiningen build tool script from http://leiningen.org/
and run the <code>lein</code> command by itself to have it do a self-install of
Clojure and Leiningen. After the initial install, running the <code>lein</code>
by itself will show the options.

Leiningen works similarly to Maven. There's <code>lein clean</code> to do clean
up, <code>lein compile</code> to compile to class files, and <code>lein
install</code> to install to the local _.m2_ repository. To build a jar with
dependencies included, use the command <code>lein uberjar</code>.


### Go ###

You can download Go from this page: http://golang.org/. After you have installed
it you will have the <code>go</code> command available; run it by itself to get
basic usage instructions.

If you look in the _build.sh_ script in the <code>build_go()</code> function you
will see this command to build <code>gosearch</code>:

    go install elocale.com/clarkcb/xsearch/gosearch

Note that I set the <code>GOPATH</code> environment variable. This is important
for Go because it expects it to be defined and point to a standard directory
structure that contains a _src_ directory with project-specific source
directories under that.

Note also that something called <code>gengosearchcode</code> is installed and
ran before the <code>gosearch</code> install. This executable generates some go
code files from xml that get used in <code>gosearch</code>.

The compiled executable gets created under _go/bin_. To run <code>gosearch</code>
I then create a soft link to it in my _~/bin_ directory:

    $ cd ~/bin
    $ ln -s ~/src/git/xsearch/go/bin/gosearch

Alternatively, you could add <code>$GOPATH/bin</code> to <code>PATH</code>.


### Haskell ###

You can download Haskell from this page: https://www.haskell.org/. After you
have run the installer you will have access to these commands:

* <code>ghc</code> - the compiler
* <code>ghci</code> - the REPL
* <code>cabal</code> - a tool for building, packaging, and managing dependencies

The <code>hssearch</code> version has a number of dependencies, which you will
use <code>cabal</code> to download and install. To start, I recommend running
these commands first:

    $ cd ~/src/git/xsearch/haskell/hssearch
    $ cabal sandbox init

This will create a hidden directory called _.cabal-sandbox_, where the
dependencies will be downloaded to and built. You can then try compiling from
the same directory:

    $ cabal build

<code>cabal</code> will complain about missing dependencies. You will need to
install them. For example, for this specified dependency:

    --dependency='timeit=timeit-1.0.0.0-b5d83acfe823666e0ea6354a3ae30d03'

Run this <code>cabal</code> command:

    $ cabal install timeit

Once all dependencies satisfied, you should be able to build <code>hssearch</code> 
successfully with <code>cabal build</code>. This will create the <code>hssearch</code>
executable under _dist/build/HsSearch/hssearch_, to which I then created a soft
link under _~/bin_.


### Java / Scala ###

I use Maven to build and manage dependencies for the Java and Scala versions.
You will find _pom.xml_ files at the roots of those source trees.

To build the <code>javasearch</code> and <code>scalasearch</code> versions I run
this command in those root directories:

    $ mvn clean install

This creates the executable jar under the _target_ directory. To run them I
created bash scripts at their root levels - _javasearch_ and _scalasearch_ - to
which I then created soft links under _~/bin_. You will need to edit those
scripts to change the path to the jar if xsearch is not cloned under
_~/src/git/xsearch_.


### PHP / Python / Ruby ###

Since PHP, Python and ruby are interpreted languages, and since their interpreters
are automatically installed on most Unix-style systems (e.g. Linux and OSX),
there's not much you will need to do to run these (on Windows you will need to
download and install them from http://php.net/, http://www.python.org and
https://www.ruby-lang.org/).

Unless I'm forgetting a dependency that had to be installed, it should just be
a matter of running the scripts _phpsearch.php_,  _pysearch.py_ and
_rbsearch.rb_. I created soft links to each of these in my _~/bin_ to run them
from anywhere.

I'm starting to work on creating installable packages of these versions, but I
still have work to do on that.

Also note: the python code is in 2.x style, I have not converted it to 3.x yet
but I plan to.


History / Motivation
--------------------

This project started as a python implementation of a basic command line-based
recursive file search utility that I wrote for programming practice but also
because I needed a file search utility that did something slightly different
from what I was able to get with other tools or combinations of tools.
Specifically, I needed something that would show me multiline search results
if a line matched a regular expression, but only if another regex was or wasn't
a match in a certain number of lines before or after the matching line.

A number of years after I wrote the initial Python version, I got the idea that
I could rewrite the tool in other languages that I wanted to learn or practice.
At the time I had worked as a java developer and was working as a C# developer,
but was also getting interested in other languages/platforms, such as Clojure,
node.js, F#. I wrote basic versions of the tool in those, and then also decided
to try writing versions in Java and Ruby. At this point I have also done versions
in Go, Haskell and PHP, and have been working to bring everything up to more or
less the same level of functionality.


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

* __hssearch__ - I'm pleasantly surprised by the succintness of the Haskell code
  even after the first version, and the readability is better than I thought
  it would be. Also, this version is the fastest. Perhaps that isn't too
  surprising, since haskell and go are the two natively compiled languages, but
  I'm a little surprised how fast it is, and especially that is is faster than
  the go version, even without me having done any performance optimizations
  (concurrency, etc.).

* __gosearch__ - This is the second fastest version. Frankly, I expected it to
  be the fastest, given the fact that it is natively compiled and also the fact
  that I added concurrency. In fact, it's the only language version that I added
  concurrency to, mainly because channels make it easy. Also, the available
  standard library packages made it fairly easy to implement some of the more
  advanced features, like archive file searching.

* __scalasearch__ - I first wrote this version when I wanted to learn Scala and
  was looking at possibly having a Scala development opportunity. I did end up
  working in it professionally for several years, and in that time my functional
  language skills improved a fair amount, and it's been interesting for me to
  see how this version has evolved.

* __pysearch.py__ - When I use this tool, which I still do regularly, I usually
  end up using the Python version. That's mostly out of habit, but also because
  as the first language version I still sort of treat it as the reference
  version, and try to keep it up-to-date with new functionality.


Also, here are some thoughts on each language that I have done implementations
for so far:

* [C#](http://en.wikipedia.org/wiki/C_Sharp_%28programming_language%29) -
  C# had the benefit of learning from Java's strengths and weaknesses, but it
  has also evolved beyond Java in a number of ways that I appreciate:
  the incorporation of extension methods, lambdas (which java 8 adds) and LINQ.
  I think the .NET libraries are a little stronger than the Java standard ones
  too. Although it is an imperative/OO language, it does have some nice
  functional features with lambdas and LINQ.

* [Clojure](http://clojure.org/) -
  Clojure was my first exposure to a language in the Lisp family. It was also my
  first exposure to coding in a predominantly functional language. I found that
  developing in it was very slow for me, especially at first, but I also enjoyed
  it because it really forced me to think about solving problems very differently
  than I was used to, and the end solutions felt succinct and elegant. Although
  I wouldn't call Clojure my new favorite language, I definitely like it and
  will most likely do more with it.

* [F#](http://fsharp.org/) -
  I haven't done any real work on the F# version in several years, but I
  recall really liking the language. It feels sort of like a cross between
  python and C# with lots of LINQ. In comparison with scala, I think F# is
  cleaner syntactically, although object-oriented code seems to be a little
  clunkier in it. My personal feeling is that F# *should* be the successor to C#
  on the CLI, but I have my doubts that that will happen. I believe that the
  larger programming community will eventually swing away from imperative and
  towards functional, but my impression is that F# doesn't have the same
  momentum in the CLI world as scala does in the JVM world. Hopefully I'm wrong.

* [Go](https://golang.org/) -
  I was really surprised how fast I could write Go code, I reached
  near Python speeds, which I think is in part due to the tools and conventions
  (e.g. standard directory structure) but also the straightforwardness of the
  language; think C/C++ but cleaner and easier to understand. I like the way OO
  is done in Go, it seems like an elegant solution. I also really like channels,
  they are a surprisingly simple way to add concurrency. On the minus side, lack
  of generics is a little disappointing, as is the lack of any functional
  capabilities.

* [Haskell](https://www.haskell.org/) -
  Haskell was a mind bender to learn (still is), but I'm becoming more and more
  a fan of functional programming, and Haskell seems to be the king. I love the
  succinctness of the language and the way it forces you to approach problems
  very differently. I'm also impressed with its type system. I still need to
  tackle the harder category theory-related subjects, but I plan to and will
  definitely do more coding in Haskell.

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
  A platform for running Javascript "in the wild," I think node.js is pretty neat,
  and quite a bit faster than I expected it to be, even when not going "fully
  async" (which I currently am not). Node.js and AJAX have helped me hate
  Javascript less. Also, npm seems like a good package manager. In theory I also
  like the idea that a developer can use one language for front-end and back-end
  development of web apps. Still, Javascript just isn't that compelling of a
  language in my opinion. I will withhold final judgement until I have rewritten
  the node version with async/non-blocking calls.

* [PHP](http://php.net/) [1] - I have a fair amount of web dev experience with
  PHP, but had never done any non-web/CLI work in it, and I had also never
  worked with PHP 5 classes. Also, I wanted to see how fast I could put a PHP
  version together. The experience was actually not too painful. PHP is not
  my favorite language for sure, but the class implementation is fine,
  and I discovered that PHP has some functional aspects to it, such as first-
  class functions and some functional-style functions (array_filter, array_map,
  etc.). I got a basic working version (everything except linesafterto/until
  and archive file searching) done in a little over a day.

* [Python](https://www.python.org/) -
  This was my 2nd go-to scripting language (after Perl), and the first language
  that I wrote this tool in. I'm a fan of Python. Python code is naturally very
  clean/readable, it has a good standard library, and it's super fast to develop
  in. That makes it a great language for writing utilities that go beyond shell
  scripting requirements, and it's also great for prototyping. Python does have
  some limited but nice functional capabilities too, such as lambdas and
  for-comprehensions. I find it and Ruby to be very similar, with pluses and
  minuses on both sides (more on that below), but suffice it to say that I would
  probably pick Python over Ruby, mostly because I have more experience in it
  and prefer some of its syntax, but not because I think it's superior.

* [Ruby](https://www.ruby-lang.org/) -
  I first learned some Ruby when I was taking a look at Ruby On Rails (I suspect
  I'm not the only one). I was already a regular Python user, so a lot
  of my reactions to it were comparitive in nature. Mostly what I found is that
  it is a nice language that is quite similar to Python in a lot of respects,
  with the obvious superficial syntactic differences and different libraries
  but also with some relative strengths and weaknesses. On the strengths side,
  Ruby is a pure OO language, whereas Python is not. I haven't found that to be
  an issue in Python development, but if I had come from Ruby first I might.
  Also, Ruby's functional capabilities seem to be slightly more robust than
  Python's. Its code blocks seem to be more flexible than Python's lambdas, and
  libraries with higher-order function handling seem to be more prevalent. As
  far as relative weaknesses, Ruby's Unicode support is weaker than Python's
  (last I checked), which isn't a big deal for most people but it's an important
  consideration for me. In terms of personal preference, I prefer Python's
  indentation-based nesting over the need to use the "end" token to close nested
  blocks in Ruby.

* [Scala](http://www.scala-lang.org/) -
  I had been a Java and C# developer before getting a job in Scala, and although
  there was a learning curve, I found the transition to Scala to be fairly
  painless overall. I was able to start with a more Java-like style (OO +
  imperative), but then I could slowly migrate to a more functional style as I
  started to understand it better. This is how the language was designed - allow
  imperative but encourage functional - and in my opinion it makes Scala the
  natural successor to the JVM throne after Java. The tipping point will be when
  enough developers have understood the benefits of functional programming that
  the larger industry starts to push for a shift to those languages.

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
  have to rewrite the same function each time I want to get the same functionality
  for different input types.

* Object-oriented - Give me functional over imperative, but I would still like
  to be able to create classes, etc. Yes, you can get a lot of that with structs
  and records, and Haskell taught me that OO wasn't absolutely essential, but I
  still like it enough to miss it a little in a non-OO language.

* Robust library / framework - This is probably an obvious point, but I
  think it's still worth mentioning.

* Quick to develop in - Any language will be quick to develop in for the developer
  that has a lot of experience with that language, but I think it says a lot
  about a language when it is quick to develop in with minimal knowledge and
  experience.

* Comfortable - This is just the idea of having a level of familiarity with a
  language that makes the developer feel very comfortable with the language.
  This comes with experience, but depending on the language it can start to feel
  comfortable quickly.

* High-performance - This is not actually an absolute requirement for me, but
  honestly, who wouldn't prefer higher performance?



Based on these features, the "winners" for me so far are:

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

* Bring the F# version up to speed with the other versions
* Rewrite the Python version for Python 3.x
* Rewrite the node.js version using async/non-blocking calls
* Switch from Maven to SBT for Scala
* Implement a C++ version (?)


Some longer-term and/or tentative TODOs:

* Implement archive file searching in versions that don't currently have it
* Add new features (ideas pop up periodically but no list exists currently)
* Internationalize the versions (???) -- this used to be my job, so even though
  it seems like overkill in some respects it also feels like something I *should*
  do.


Other than that, I don't have specific plans for other language rewrites, but
I'm sure they will happen. I'm not entirely sure which languages I would do
implementations for yet, but I have several different ideas for ones to choose:

1. Choose a language I (used to) know to see how quickly I can implement that
   version.
2. Choose a language I don't know because it seems to be up-and-coming as well
   as interesting.
3. Choose a language I don't know because it is different from other languages
   I have done and therefore gives new challenges.


Languages that I'm currently considering (listed alphabetically):

* [Common Lisp](http://en.wikipedia.org/wiki/Common_Lisp) [3] - After some
  exposure to Clojure and Scheme/Racket, I've become intrigued with the Lisp
  family. I'm especially intrigued by the idea of homoiconicity and macros, but
  also by the language's multiparadigm nature.

* [Erlang](http://www.erlang.org/) [3] - I'm most interested in the message
  passing feature of Erlang. I had a small amount of exposure to a similar
  approach when using Akka with Scala, and I liked how it worked. Other than
  that, it seems vaguely like Haskell but without types, which sounds bad but
  I'd like to try it out before writing it off.

* [OCaml](http://caml.inria.fr/ocaml/) [2] - I'm interested to compare OCaml to
  its close cousin F#, but also compare it to other functional languages that I
  have done, such as Haskell, and other functional/OO hybrid languages that I
  have done, such as Scala.

* [Perl](https://www.perl.org/) [1] - Perl was my first go-to scripting language.
  I would be curious to see how quickly I could implement a Perl version.

* [Rust](http://www.rust-lang.org/) [2,3] - I don't know much about Rust yet,
  but from the little bit I've seen it looks interesting. It seems to be a
  functional language replacement for C/++, so maybe a little like Go +
  functional? I'm not sure but I'm curious.

* [Squeak](http://en.wikipedia.org/wiki/Squeak) [3] - I have known some
  developers who worked in Smalltalk professionally, and they were huge fans of
  it. As the most influential OO language I'm pretty interested to see what it's
  like too.

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
* Lua
* PowerShell
* Rexx
* Scheme/Racket
* Tcl
* VB

