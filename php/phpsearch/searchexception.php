<?php

namespace PhpSearch;

class SearchException extends \Exception {
    function __construct($message) {
        $this->message = $message;
    }
}

?>
