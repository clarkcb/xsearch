<?php

declare(strict_types=1);

namespace phpsearch;

/**
 * Enum Color
 */
enum Color: string
{
    case Grey = "\033[30m";
    case Red = "\033[31m";
    case Green = "\033[32m";
    case Yellow = "\033[33m";
    case Blue = "\033[34m";
    case Reset = "\033[0m";
}
