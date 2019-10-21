<?php

require_once __DIR__ . '/autoload.php';

class FileTypes {
    function __construct() {
        $this->file_type_map = $this->get_file_type_map();
    }

    private function get_file_type_map() {
        $file_type_map = array();
        $filetypespath = FileUtil::expand_user_home_path(Config::FILETYPESPATH);
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

    public static function from_name(string $name) {
        $uname = strtoupper($name);
        if ($uname == 'TEXT') {
            return FileType::Text;
        }
        if ($uname == 'BINARY') {
            return FileType::Binary;
        }
        if ($uname == 'ARCHIVE') {
            return FileType::Archive;
        }
        if ($uname == 'CODE') {
            return FileType::Code;
        }
        if ($uname == 'XML') {
            return FileType::Xml;
        }
        return FileType::Unknown;
    }

    public function get_filetype(string $file) {
        if ($this->is_text($file)) {
            return FileType::Text;
        }
        if ($this->is_binary($file)) {
            return FileType::Binary;
        }
        if ($this->is_archive($file)) {
            return FileType::Archive;
        }
        if ($this->is_code($file)) {
            return FileType::Code;
        }
        if ($this->is_xml($file)) {
            return FileType::Xml;
        }
        return FileType::Unknown;
    }

    public function is_archive(string $f): bool {
        return in_array(FileUtil::get_extension($f), $this->file_type_map['archive']);
    }

    public function is_binary(string $f): bool {
        return in_array(FileUtil::get_extension($f), $this->file_type_map['binary']);
    }

    public function is_code(string $f): bool {
        return in_array(FileUtil::get_extension($f), $this->file_type_map['code']);
    }

    public function is_text(string $f): bool {
        return in_array(FileUtil::get_extension($f), $this->file_type_map['text']);
    }

    public function is_xml(string $f): bool {
        return in_array(FileUtil::get_extension($f), $this->file_type_map['xml']);
    }

    public function is_searchable(string $f): bool {
        return in_array(FileUtil::get_extension($f), $this->file_type_map['searchable']);
    }

    public function is_unknown(string $f): bool {
        return !$this->is_searchable($f);
    }
}

?>
