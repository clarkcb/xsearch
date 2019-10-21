<?php

class FileUtil {

    public static $DOT_PATHS = array('.', '..');

    public static function expand_user_home_path(string $path): string {
        if (strpos($path, '~') === 0) {
            return str_replace('~', getenv('HOME'), $path);
        }
        return $path;
    }

    public static function get_extension(string $file): string {
        $f = basename($file);
    	$ext = '';
    	$dot_idx = strrpos($f, '.');
    	if ($dot_idx !== false && $dot_idx > 0 && $dot_idx < strlen($f))
    		$ext = substr($f, $dot_idx+1);
        return $ext;
    }

    public static function is_dot_dir(string $d): bool {
        return in_array($d, self::$DOT_PATHS);
    }

    public static function is_hidden(string $file): bool {
        $f = basename($file);
        return strlen($f) > 1 && $f{0}==='.' && !self::is_dot_dir($f);
    }

    public static function join_path(string $path, string $file) {
        return self::normalize_path($path) . self::get_separator($path) . $file;
    }

    public static function get_separator(string $path): string {
        $sep = '/';
        if (strpos($path, $sep) === false) {
            if (strpos($path, '\\') === false) {
                return $sep;
            } else {
                return '\\';
            }
        }
        return $sep;
    }

    public static function normalize_path(string $path): string {
        $sep = self::get_separator($path);
        return rtrim($path, $sep);
    }

    public static function split_path(string $path) {
        $sep = self::get_separator($path);
        if ($sep == '/')
            $sep = '\\/';
        return preg_split("/$sep/", $path);
    }
}

?>
