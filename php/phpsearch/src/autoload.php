<?php
require_once __DIR__ . '/phpsearch/Config.php';

// the str_replace was necessary when using a namespace because it was using
// a backslash in the path instead of forward slash
spl_autoload_register(function ($class) {
    $cls = str_replace("\\", "/", $class);
    if (str_starts_with($class, 'phpfind')) {
        include __DIR__ . '/../../../../xfind/php/phpfind/src/' . $cls . '.php';
    } else {
        include __DIR__ . '/' . $cls . '.php';
    }
});
