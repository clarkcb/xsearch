<?php

// the str_replace was necessary when using a namespace because it was using
// a backslash in the path instead of forward slash
spl_autoload_register(function ($class) {
    $include_path = '/Users/cary/src/git/xsearch/php/phpsearch/';
    include $include_path . str_replace("\\", "/", strtolower($class)) . '.php';
});

?>