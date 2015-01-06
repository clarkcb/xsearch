xsearch
=======

A multilingual file search utility


Overview
--------

This is a command line-based recursive file search utility written in a number of
different languages (for the reason behind this see History / Motivation).

Here's the current usage text for the haskell version (the same for each
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
  -P,--noprintmatches       Suppress printing of matches to stdout
  -p,--printmatches         Print matches to stdout*
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
  -Z,--nosearcharchives     Do not search archive files (bz2, gz, tar, zip)
  -z,--searcharchives       Search archive files (bz2, gz, tar, zip)*
</pre>

For example, say you want to perform a search according to these settings:

* search from the current directory (.)
* C# (.cs extension) files only
* lines matching the regex "\bLogin"
* file names matching the regex "Controller"
* directory names not matching the regex "temp"
* tell how long each part of the search process takes (time it)
* print a list of matching files after


You would use a command like this (using the python version in this example):

<pre>
  pysearch.py -x cs -f "Controller" -D "temp" -s "\bLogin" -t --listfiles .
</pre>

If there were one match, the result output would look something like this:

<pre>
  Elapsed time for get_search_dirs: 0:00:00.058262
  Elapsed time for get_search_files: 0:00:00.011757
  Elapsed time for search_files: 0:00:00.010897

  Search results (1):
  ./path/to/CsController.cs: 99 [21:26]: var loginLabel = "Login";
  1 match for "\bLogin"

  Files with matches (1):
  ./path/to/CsController.cs
</pre>


The verbosity level can be increased with the -v/--verbose option, which will
output the lists of directories and files to be searched, and even more
with the --debug option, which will also output the SearchSettings and other
debug info.

The completeness of the functionality of the tool varies from language to language.
Currently, there are only two language versions that search archive files: go and
scala. A slightly larger number supports before- and after-line capturing and
matching: go, haskell, python and scala. The rest of the functionality is more
or less implemented in all of the languages. However, it should be noted that
the clojure and C++ versions are very incomplete, and the F# version has
fallen fairly far behind the rest.

The basic code structure includes these elements:

* SearchOptions - transforms option data in XML into the usage text, also builds
  a SearchSettings instance from command line arguments
* SearchSettings - encapsulates the search settings, including what directories
  or files to include/exclude, search patterns, lines before or after, etc.
* Searcher - executes the file search based on the SearchSettings
* FileUtil - helps determine file type (e.g. binary vs. text), searchability,
  etc.
* SearchResult - encapsulates a single search result


Installing / Running
--------------------

I have xsearch cloned to this path: ~/src/git/xsearch. To run the various language
versions I compile them (if necessary) and create a soft link to the executable
or script under ~/bin, which I include in the PATH. For example, to run the
Haskell version I compiled it and then created the following soft link:

    $ cd ~/bin
    $ ln -s ~/src/git/xsearch/haskell/hssearch/dist/build/hssearch/hssearch


In some cases I have bash scripts under the root of some of the language versions
to facilitate running that version. I will probably add more of these for consistency.

For each language version that you want to compile/run, you will need to install
the compilers/interpreters for those languages, unless that system already has
them installed (e.g. python and ruby on Linux / OSX).

I have a script under the xsearch/shared directory called build.sh that can be
used to compile various compiled language versions. For example, you can build
the Haskell version (after installing GHC) by running:

    $ cd ~/src/git/xsearch
    $ ./shared/build.sh haskell

You can also run it without a language to build all compiled versions at once.

Below is some additional info for some of the languages.


### C# / F# ###

For writing the C# and F# code I use MS Visual Studio on a Windows 7 VM. To
compile/run/test them I use the [Mono](http://www.mono-project.com/)
environment installed on my OSX system. If you take a look at the shared/build.sh
script you will see this command to compile the C# version:

    xbuild /p:Configuration=Debug $CSHARP_PATH/CsSearch/CsSearch.sln

You can change Configuration to Release if you want to create a release build.

To run the cssearch version, I created a soft link to a bash script under CsSearch:

    $ cd ~/bin
    $ ln -s ~/src/git/xsearch/csharp/CsSearch/cssearch.sh cssearch

This information is similarly applicable for the F# version.


### Go ###

You can download go from this page: http://golang.org/. After you have installed
it you will have the __go__ command available; run it by itself to see what options
are.

If you look in the build.sh script you will see this command to build gosearch:

    go install elocale.com/clarkcb/xsearch/gosearch

Note that I set the GOPATH environment variable. This is important for go
because it expects it to be defined and point to a standard directory structure
that contains a src directory with project-specific source directories under that.

Note also that something called gengosearchcode is installed and ran before
the gosearch install. This executable generates go code files from xml that get
used in gosearch. 

The compiled executable gets created under go/bin. To run gosearch I then create
a soft link to it in my ~/bin directory:

    $ cd ~/bin
    $ ln -s ~/src/git/xsearch/go/bin/gosearch

Alternatively, you could add $GOPATH/bin to PATH.


### Haskell ###

You can download Haskell from this page: https://www.haskell.org/haskellwiki/Haskell.
After you have run the installer you will have access to these commands:

* __ghc__ - the compiler
* __ghci__ - the REPL
* __cabal__ - a tool for building, packaging, and managing dependencies

The hssearch version has a number of dependencies, which you will use cabal to
download and install. To start, I recommend running these commands first:

    $ cd ~/src/git/xsearch/haskell/hssearch
    $ cabal sandbox init

This will create a hidden directory called .cabal-sandbox, where the dependencies
will be downloaded to and built. You can then try compiling from the same directory:

    $ cabal build

Cabal will complain about missing dependencies. You will need to install them.
For example, for this specified dependency:

    --dependency='timeit=timeit-1.0.0.0-b5d83acfe823666e0ea6354a3ae30d03'

Run this cabal command:

    $ cabal install timeit

Once all dependencies satisfied, you should be able to build hssearch successfully
with cabal build. This will create the hssearch executable under
dist/build/HsSearch/hssearch, to which I then created a soft link under ~/bin.


### Java / Scala ###

I use Maven to build and manage dependencies for the Java and Scala versions.
You will find pom.xml files under the root of those source trees.

To build the javasearch and scalasearch versions I run this command in those
root directories:

    $ mvn clean install

This creates the executable jar under the target directory. To run them I created
bash scripts at their root levels - javasearch and scalasearch - to which I then
created soft links under ~/bin. You will need to edit those scripts to change
the path to the jar if xsearch is not cloned under ~/src/git/xsearch.


History / Motivation
--------------------

This project started as a python implementation of a basic command line-based
recursive file search utility that I wrote for programming practice but also
because I needed a file search utility that did something slightly different
from what I was able to get with other tools or combinations of tools.
Specifically, I needed something that would show me multiline search results
if a line matched a regular expression, but only if another regex was or wasn't
a match in a certain number of lines before or after the matching line.

A number of years after I wrote the initial python version, I got the idea that
I could rewrite the tool in other languages that I wanted to learn or practice.
At the time I had worked as a java developer and was working as a C# developer,
but was also getting interested in other languages/platforms, such as clojure,
node.js, F#. I wrote basic versions of the tool in those, and then also decided
to try writing versions in java and ruby. At this point I have also done versions
in go and haskell, and have been working to bring everything up to more or less
the same level of functionality.


Thoughts So Far
---------------

I have enjoyed the experience of rewriting the tool in various languages
for a number of reasons:

* I've found it to be an effective way to learn or improve language
  knowledge/skills.
* It gives an interesting opportunity to compare languages in a not entirely
  superficial way.
* I learned something new from each language that helped me find better
  approaches to problems and just become a better programmer in general.
* The experience has helped me to form some opinions on what constitute (for me)
  preferable language features, but it has also given me an appreciation of each
  language that I have worked in.



So how do the different language versions compare? Here's my biased list of
current favorites:

* __hssearch__ - It took me a long time to get to the point where I could even
  start to tackle a haskell version - I had to do lots of reading and video
  watching and exercises - but once the light bulbs started to go off things
  started to go surprisingly quickly. I'm pleasantly surprised by the
  succintness and readability of the code even after the first version. Also,
  this version is one of the two fastest. I know it's a compiled language,
  but I'm still impressed with how fast it is, even without me having done any
  performance optimizations (concurrency, etc.).
* __gosearch__ - This is the other of the two fastest versions (I haven't done a
  shootout yet to determine the real winner). It's also the only one that I have
  added concurrency to, mainly because channels make it so easy. Also, the
  available standard library packages made it fairly easy to implement some of 
  the more advanced features, like archive file searching.
* __scalasearch__ - I first wrote this version when I wanted to learn scala and
  was possibly going to get a job developing in it. I did end up working in it 
  professionally for 2.5 years, and in that time my functional skills have
  improved quite a bit, and it's been interesting to see how this version has
  evolved.
* __pysearch.py__ - When I need to use this tool, which I still do regularly, I
  end up using the python version most of the time. That's mostly out of habit,
  but also because as the first language version I still sort of treat it as the
  reference version, although it is currently behind the go and scala versions
  in terms of functionality.


Also, here are some thoughts on each language that I have done implementations
for so far:

* [C#](http://en.wikipedia.org/wiki/C_Sharp_%28programming_language%29) -
  C# had the benefit of learning from java's strengths and weaknesses, but it
  has also evolved beyond java in a number of ways, most noticeably (to me) with
  the incorporation of extension methods, lambdas (which java 8 adds) and LINQ.
  I think the .NET libraries are a little stronger than the java standard ones
  too. Although it is an imperative/OO language, it does have some nice
  functional features with lambdas and LINQ.
* [Clojure](http://clojure.org/) -
  Clojure was my first exposure to a language in the lisp family. It was also my
  first exposure to coding in a predominantly functional language. The clojure
  version of the search tool is very incomplete and untouched in several years,
  but I'm really looking forward to getting back to it. I remember thinking that
  I might eventually conclude that it's the best language of the bunch, but that
  was before I did some of the others. It will be interesting to see if I end up
  feeling the same way or not.
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
  I was really surprised how fast I could write go code, I reached
  near python speeds, which I think is in part due to the conventions (e.g.
  standard directory structure) but also the straightforwardness of the language;
  think C/C++ but cleaner and easier to understand. I like the way OO is done in
  go. I also really like channels, they are an elegant and fairly simple way to
  add concurrency. On the minus side, lack of generics is a little disappointing,
  as is the lack of any functional capabilities.
* [Haskell](https://www.haskell.org/) -
  Haskell was a mind bender to learn (still is), but I'm becoming more and more
  a fan of functional programming, and haskell seems to be the king. I love the
  succinctness of the language and the way it forces you to approach problems
  very differently. I'm also impressed with its type system. I still need to
  tackle the harder category theory-related subjects, but I plan to and will
  definitely do more coding in haskell.
* [Java](http://en.wikipedia.org/wiki/Java_%28programming_language%29) -
  I wrote the java version after the scala version, and frankly it was a little
  hard to work in java after having had several years of scala experience. The
  same functionality in java will often require much more verbosity and end up
  being less readable as well as more error-prone. Java deserves respect for
  being a vast improvement over C/C++ in terms of ease of coding, cross-platform
  deployment, and encoding support, and also for being the giant in the
  enterprise software world, but my personal feeling is that java will gradually
  lose out to more functional languages like scala. Java version 8 might delay
  that transition a little, although in the end I think it will actually help
  influence the transition.
* [Node.js](http://nodejs.org/) -
  A platform for running javascript "in the wild," I think node.js is pretty neat,
  and quite a bit faster than I expected it to be, even when not using the
  non-blocking calls (which I currently am not). Node.js and AJAX have also both
  helped me hate javascript less. Also, npm seems like a good package manager.
  In theory I also like the idea that a developer can use one language for
  frontend and backend development of web apps. Still, javascript just isn't
  that compelling of a language in my opinion. I will withhold final judgement
  until I have rewritten the node version with async/non-blocking calls.
* [Python](https://www.python.org/) -
  This was my 2nd go-to scripting language (after perl), and the first language
  that I wrote this tool in. I'm a fan of python. Python code is naturally very
  clean/readable, has a good standard library, and is super fast to develop in.
  That makes it a great language for writing utilities that go beyond shell
  scripting requirements, and it's also great for prototyping. Python does have
  some limited but nice functional capabilities too, such as lambdas and
  for-comprehensions. I find it and ruby to be very similar, with pluses and
  minuses on both sides (more on that below).
* [Ruby](https://www.ruby-lang.org/) -
  I first learned some ruby when I was taking a look at Ruby On Rails
  (I suspect I'm not the only one). I was already a regular python user, so a lot
  of my reactions to it were comparitive in nature. Mostly what I found is that
  it is a nice language that is quite similar to python in a lot of respects,
  with the obvious superficial syntactic differences and different libraries
  but also with some relative strengths and weaknesses. On the strengths side,
  ruby is a pure OO language, whereas python is not. I haven't found that to be
  an issue in python development, but if I had come from ruby first I might.
  Also, ruby's functional capabilities seem to be slightly more robust than
  python's. Its code blocks seem to be more flexible than python's lambdas, and
  libraries with higher-order function handling seem to be more prevalent. As
  far as relative weaknesses, ruby's Unicode support is weaker than python's
  (last I checked), which isn't a big deal for everyone but it's an important
  consideration for me. In terms of personal preference, I prefer python's
  indentation-based nesting over the need to use the "end" token to close nested
  blocks.
* [Scala](http://www.scala-lang.org/) -
  I had been a java and C# developer before getting a job in scala, and I found
  the transition to scala to be fairly painless. I was able to start with a more
  java-like style (OO + imperative), but then I could slowly migrate to a more
  functional style as I wrapped my head around it more. This is how the
  language was designed - allow imperative but encourage functional - and in my
  opinion it makes scala the natural successor to the JVM throne after java. The
  tipping point will be when enough developers have understood the benefits of
  functional programming that the larger industry starts to push for a shift to
  those languages.


Plans / TODOs
-------------

My current TODOs:

* Bring the F# version up to speed with the other versions
* Rewrite the python version for python 3.x
* Rewrite the node.js version using async/non-blocking calls
* Implement the clojure version
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
   you have done and therefore gives new challenges.


Languages that I'm currently considering (listed alphabetically):

* [Common Lisp](http://en.wikipedia.org/wiki/Common_Lisp) [3] - After some
  exposure to clojure and scheme/racket, I've become intrigued with the lisp
  family. I'm especially intrigued by the idea of homoiconicity and macros, but
  also by the language's multiparadigm nature.
* [Erlang](http://www.erlang.org/) [3] - I'm most interested in the message
  passing feature of erlang. I had a small amount of exposure to a similar
  approach when using akka with scala, and I liked how it worked. Other than
  that, it seems vaguely like haskell but without types, which sounds bad but
  I'd like to try it out before forming any opinions.
* [OCaml](http://caml.inria.fr/ocaml/) [2] - I'm interested to compare OCaml to
  its close cousin F#, but also compare it to other functional languages that I
  have done, such as haskell, and other functional/OO hybrid languages that I
  have done, such as scala.
* [Perl](https://www.perl.org/) [1] - Perl was my first go-to scripting language.
  Related to the first language selection idea above, I would be curious to see
  how quickly I could implement a perl version.
* [PHP](http://php.net/) [1] - I have webdev experience with PHP, but I'm curious
  to see what it's like to write a non-web app in PHP. Also, similarly to perl,
  I'd like to see how quickly I could implement a PHP version.
* [Rust](http://www.rust-lang.org/) [2,3] - I don't know much about rust yet, but
  from the little bit I've seen it looks very interesting. It seems to be a
  functional language replacement for C/++, so maybe a little like go +
  functional? I'm not sure but I'm curious.
* [Squeak](http://en.wikipedia.org/wiki/Squeak) [3] - I have known some developers
  who worked in Smalltalk professionally, and they were huge fans of it. As the
  most influential OO language I'm pretty interested to see what it's like too.
* [Swift](https://developer.apple.com/swift/) [2] - Apple's replacement for
  Objective C isn't quite ready for primetime from what I've been hearing, but
  from the little bit I've read about the language it looks like it'll be nice
  to work in when they work out the kinks.


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

