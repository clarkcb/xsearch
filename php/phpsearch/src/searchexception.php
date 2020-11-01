<?php declare(strict_types=1);

/**
 * Class SearchException
 */
class SearchException extends Exception
{
    public function __construct(string $message)
    {
        parent::__construct($message);
    }
}
