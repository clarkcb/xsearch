<?php

namespace phpsearch;

class StringUtil
{
    public static function bool_to_string(bool $b): string
    {
        return $b ? 'true' : 'false';
    }

    public static function string_array_to_string(array $arr): string
    {
        if (count($arr)) {
            return '["' . implode('", "', $arr) . '"]';
        }
        return '[]';
    }
}
