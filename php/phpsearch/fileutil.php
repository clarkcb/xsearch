<?php

class FileUtil {

    const FILETYPESPATH = '~/src/git/xsearch/shared/filetypes.xml';

    public static $DOT_PATHS = array('.', '..');

    public static function expand_user_home_path($path) {
        if (strpos($path, '~') === 0) {
            return str_replace('~', getenv('HOME'), $path);
        }
        return $path;
    }

    public static function get_extension($file) {
        $f = basename($file);
    	$ext = '';
    	$dot_idx = strrpos($f, '.');
    	if ($dot_idx !== false && $dot_idx > 0 && $dot_idx < strlen($f))
    		$ext = substr($f, $dot_idx+1);
        return $ext;
    }

    public static function is_dot_dir($d) {
        return in_array($d, self::$DOT_PATHS);
    }

    public static function is_hidden($file) {
        $f = basename($file);
        return strlen($f) > 1 && $f{0}==='.' && !self::is_dot_dir($f);
    }

    public static function join_path($path, $file) {
        return self::normalize_path($path) . '/' . $file;
    }

    public static function normalize_path($path) {
        return rtrim($path, '/');
    }

    public static function split_path($path) {
        return preg_split("/\//", $path);
    }
}

?>
