<?php

class SearchFile {
    const CONTAINER_SEPARATOR = '!';

    function __construct($path, $filename, $filetype) {
        $this->containers = array();
        $this->path = $path;
        $this->filename = $filename;
        $this->filetype = $filetype;
    }

    public function __toString() {
        $s = "";
        if ($this->containers) {
            $s = join(CONTAINER_SEPARATOR, $this->containers) . CONTAINER_SEPARATOR;
        }
        $s .= FileUtil::join_path($this->path, $this->filename);
        return $s;
    }
}
?>
