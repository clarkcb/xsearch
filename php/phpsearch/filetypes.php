<?php

require_once __DIR__ . '/autoload.php';

class FileTypes {
    const FILETYPESPATH = '~/src/git/xsearch/shared/filetypes.xml';

    function __construct() {
        $this->file_type_map = $this->get_file_type_map();
    }

    private function get_file_type_map() {
        $file_type_map = array();
        $filetypespath = FileUtil::expand_user_home_path(self::FILETYPESPATH);
        if (file_exists($filetypespath)) {
            $filetypes = simplexml_load_file($filetypespath);
            foreach ($filetypes->filetype as $filetype) {
                $name = sprintf($filetype['name']);
                $exts = preg_split("/\s+/", $filetype->extensions);
                $file_type_map[$name] = $exts;
            }
            $file_type_map['text'] = array_merge($file_type_map['text'],
                $file_type_map['code'], $file_type_map['xml']);
            $file_type_map['searchable'] = array_merge($file_type_map['text'],
                $file_type_map['archive'], $file_type_map['binary']);
        } else {
            throw new Exception('File not found: ' . $filetypespath);
        }
        return $file_type_map;
    }

    public function get_filetype($file) {
        if ($this->is_text($file)) {
            return FileType::Text;
        }
        if ($this->is_binary($file)) {
            return FileType::Binary;
        }
        if ($this->is_archive($file)) {
            return FileType::Archive;
        }
        return FileType::Unknown;
    }

    public function is_archive($f) {
        return in_array(FileUtil::get_extension($f), $this->file_type_map['archive']);
    }

    public function is_binary($f) {
        return in_array(FileUtil::get_extension($f), $this->file_type_map['binary']);
    }

    public function is_text($f) {
        return in_array(FileUtil::get_extension($f), $this->file_type_map['text']);
    }

    public function is_searchable($f) {
        return in_array(FileUtil::get_extension($f), $this->file_type_map['searchable']);
    }

    public function is_unknown($f) {
        return !$this->is_searchable($f);
    }
}

?>
