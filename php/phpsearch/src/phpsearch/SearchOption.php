<?php declare(strict_types=1);

namespace phpsearch;

/**
 * Class SearchOption
 */
class SearchOption
{
    public string $short_arg;
    public string $long_arg;
    public string $desc;
    public ?object $func;
    public string $sort_arg;

    public function __construct(string $short_arg, string $long_arg, string $desc, $func)
    {
        $this->short_arg = $short_arg;
        $this->long_arg = $long_arg;
        $this->desc = $desc;
        $this->func = $func;
        $this->sort_arg = $this->__sort_arg();
    }

    private function __sort_arg()
    {
        if ($this->short_arg) {
            return strtolower($this->short_arg) . 'a' . $this->long_arg;
        }
        return $this->long_arg;
    }
}
