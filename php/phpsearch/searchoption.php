<?php

class SearchOption {
    function __construct($shortarg, $longarg, $desc, $func) {
        $this->shortarg = $shortarg;
        $this->longarg = $longarg;
        $this->desc = $desc;
        $this->func = $func;
        $this->sortarg = $this->__sortarg();
    }

    private function __sortarg() {
        if ($this->shortarg) {
            return strtolower($this->shortarg) . 'a' . $this->longarg;
        }
        return $this->longarg;
    }
}

?>
