<?php
require_once __DIR__ . '/config.php';

// the str_replace was necessary when using a namespace because it was using
// a backslash in the path instead of forward slash
spl_autoload_register(function ($class) {
    $include_path = __DIR__ . '/';
    include $include_path . str_replace("\\", "/", strtolower($class)) . '.php';
});
