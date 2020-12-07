<?php
require_once __DIR__ . '/phpsearch/config.php';

// the str_replace was necessary when using a namespace because it was using
// a backslash in the path instead of forward slash
spl_autoload_register(function ($class) {
    include __DIR__ . '/' . str_replace("\\", "/", $class) . '.php';
});
