<?php

namespace PhpSearch;

class SearchOption {
    function __construct($shortarg, $longarg, $desc, $func) {
        $this->shortarg = $shortarg;
        $this->longarg = $longarg;
        $this->desc = $desc;
        $this->func = $func;
    }
}

?>
