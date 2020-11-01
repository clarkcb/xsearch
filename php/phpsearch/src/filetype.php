<?php declare(strict_types=1);

/**
 * Class FileType
 */
abstract class FileType
{
    const Unknown = -1;
    const Archive = 1;
    const Binary  = 2;
    const Code    = 3;
    const Text    = 4;
    const Xml     = 5;
}
