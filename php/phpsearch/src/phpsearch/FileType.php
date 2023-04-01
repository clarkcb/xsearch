<?php declare(strict_types=1);

namespace phpsearch;

/**
 * Enum FileType
 */
enum FileType
{
    case Unknown;
    case Archive;
    case Binary;
    case Code;
    case Text;
    case Xml;
}
