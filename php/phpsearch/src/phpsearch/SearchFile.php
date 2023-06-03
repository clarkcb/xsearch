<?php declare(strict_types=1);

namespace phpsearch;

/**
 * Class SearchFile
 *
 * @property array containers
 * @property string path
 * @property string file_name
 * @property FileType file_type
 */
class SearchFile
{
    public array $containers;
    public string $path;
    public string $file_name;
    public FileType $file_type;

    const CONTAINER_SEPARATOR = '!';

    public function __construct(string $path, string $file_name, $file_type)
    {
        $this->containers = array();
        $this->path = $path;
        $this->file_name = $file_name;
        $this->file_type = $file_type;
    }

    public function file_path(): string
    {
        return FileUtil::join_path($this->path, $this->file_name);
    }

    public function __toString(): string
    {
        $s = "";
        if ($this->containers) {
            $s = join(SearchFile::CONTAINER_SEPARATOR, $this->containers) .
                SearchFile::CONTAINER_SEPARATOR;
        }
        $s .= FileUtil::join_path($this->path, $this->file_name);
        return $s;
    }
}
