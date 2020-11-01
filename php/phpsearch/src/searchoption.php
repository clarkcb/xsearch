<?php declare(strict_types=1);

/**
 * Class SearchOption
 *
 * @property string shortarg
 * @property string longarg
 * @property string desc
 * @property callable func
 * @property string sortarg
 */
class SearchOption
{
    public function __construct(string $shortarg, string $longarg, string $desc, $func)
    {
        $this->shortarg = $shortarg;
        $this->longarg = $longarg;
        $this->desc = $desc;
        $this->func = $func;
        $this->sortarg = $this->__sortarg();
    }

    private function __sortarg()
    {
        if ($this->shortarg) {
            return strtolower($this->shortarg) . 'a' . $this->longarg;
        }
        return $this->longarg;
    }
}
