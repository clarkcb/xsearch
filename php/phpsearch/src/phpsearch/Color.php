<?php declare(strict_types=1);

namespace phpsearch;

/**
 * Class Color
 */
abstract class Color
{
    const Grey = "\033[30m";
    const Red = "\033[31m";
    const Green = "\033[32m";
    const Yellow = "\033[33m";
    const Blue = "\033[34m";
    const Reset = "\033[0m";
}
