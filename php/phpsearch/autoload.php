<?php

// the str_replace was necessary when using a namespace because it was using
// a backslash in the path instead of forward slash
spl_autoload_register(function ($class) {
	$HOME = getenv('HOME');
	if ($HOME === false) {
		$HOME = getenv('USERPROFILE');
	}
    $include_path = "$HOME/src/xsearch/php/phpsearch/";
    include $include_path . str_replace("\\", "/", strtolower($class)) . '.php';
});

?>