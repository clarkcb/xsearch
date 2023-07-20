# xsearch

A multilingual file search utility

NOTE: `xsearch` now has [xfind](https://github.com/clarkcb/xfind) as a dependency. More details to follow, but here are the basic instructions:

1. Clone `xfind` and `xsearch` into the same parent directory, e.g.

```
$ mkdir -p ~/src
$ cd ~/src
$ git clone https://github.com/clarkcb/xfind
$ git clone https://github.com/clarkcb/xsearch
```

2. Set the `XFIND_PATH` and `XSEARCH_PATH` environment variables:

```
$ export XFIND_PATH=~/src/xfind
$ export XSEARCH_PATH=~/src/xsearch
```

3. Build `xfind`. The following builds all language versions:

```
$ cd $XFIND_PATH/scripts
$ ./build.sh all
```

4. Now build `xsearch`. The following builds all language versions:

```
$ cd $XSEARCH_PATH/scripts
$ ./build.sh all
```

See the [Wiki](https://github.com/clarkcb/xsearch/wiki) for the rest of the current documentation.

