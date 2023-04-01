<?php declare(strict_types=1);

namespace phpsearch;

/**
 * Class SearchSettings
 *
 * @property bool archivesonly
 * @property bool colorize
 * @property bool debug
 * @property bool excludehidden
 * @property bool firstmatch
 * @property array in_archiveextensions
 * @property array in_archivefilepatterns
 * @property array in_dirpatterns
 * @property array in_extensions
 * @property array in_filepatterns
 * @property array in_filetypes
 * @property array in_linesafterpatterns
 * @property array in_linesbeforepatterns
 * @property int linesafter
 * @property array linesaftertopatterns
 * @property array linesafteruntilpatterns
 * @property int linesbefore
 * @property bool listdirs
 * @property bool listfiles
 * @property bool listlines
 * @property int maxlinelength
 * @property bool multilinesearch
 * @property array out_archiveextensions
 * @property array out_archivefilepatterns
 * @property array out_dirpatterns
 * @property array out_extensions
 * @property array out_filepatterns
 * @property array out_filetypes
 * @property array out_linesafterpatterns
 * @property array out_linesbeforepatterns
 * @property array paths
 * @property bool printresults
 * @property bool printusage
 * @property bool printversion
 * @property bool recursive
 * @property bool searcharchives
 * @property array searchpatterns
 * @property string textfileencoding
 * @property bool uniquelines
 * @property bool verbose
 */
class SearchSettings
{
    public $archivesonly = false;
    public $colorize = true;
    public $debug = false;
    public $excludehidden = true;
    public $firstmatch = false;
    public $linesafter = 0;
    public $linesbefore = 0;
    public $listdirs = false;
    public $listfiles = false;
    public $listlines = false;
    public $maxlinelength = 150;
    public $multilinesearch = false;
    public $printresults = true;
    public $printusage = false;
    public $printversion = false;
    public $recursive = true;
    public $searcharchives = false;
    public $textfileencoding = 'utf-8';
    public $uniquelines = false;
    public $verbose = false;

    public $in_archiveextensions = array();
    public $in_archivefilepatterns = array();
    public $in_dirpatterns = array();
    public $in_extensions = array();
    public $in_filepatterns = array();
    public $in_filetypes = array();
    public $in_linesafterpatterns = array();
    public $in_linesbeforepatterns = array();
    public $linesaftertopatterns = array();
    public $linesafteruntilpatterns = array();
    public $out_archiveextensions = array();
    public $out_archivefilepatterns = array();
    public $out_dirpatterns = array();
    public $out_extensions = array();
    public $out_filepatterns = array();
    public $out_filetypes = array();
    public $out_linesafterpatterns = array();
    public $out_linesbeforepatterns = array();
    public $paths = array();
    public $searchpatterns = array();

    public function add_exts($ext, &$exts)
    {
        if (gettype($ext) == 'string') {
            $xs = explode(',', $ext);
            foreach ($xs as $x) {
                $exts[] = $x;
            }
        } elseif (gettype($ext) == 'array') {
            foreach ($ext as $x) {
                $exts[] = $x;
            }
        }
    }

    public function add_filetypes($filetype, &$filetypes)
    {
        if (gettype($filetype) == 'string') {
            $fts = explode(',', $filetype);
            foreach ($fts as $ft) {
                $filetypes[] = FileTypes::from_name($ft);
            }
        } elseif (gettype($filetype) == 'array') {
            foreach ($filetype as $ft) {
                $filetypes[] = FileTypes::from_name($ft);
            }
        }
    }

    public function add_patterns($pattern, &$patterns)
    {
        if (gettype($pattern) == 'string') {
            $patterns[] = $pattern;
        } elseif (gettype($pattern) == 'array') {
            foreach ($pattern as $p) {
                $patterns[] = $p;
            }
        }
    }

    public function set_archivesonly(bool $b)
    {
        $this->archivesonly = $b;
        if ($b) {
            $this->searcharchives = $b;
        }
    }

    public function set_debug(bool $b)
    {
        $this->debug = $b;
        if ($b) {
            $this->verbose = $b;
        }
    }

    private function arr_to_string(array $arr): string
    {
        $s = '["' . implode('","', $arr) . '"]';
        return $s;
    }

    private function bool_to_string(bool $b): string
    {
        return $b ? 'true' : 'false';
    }

    public function __toString(): string
    {
        return sprintf('FindSettings(' .
            'archivesonly: %s' .
            ', colorize: %s' .
            ', debug: %s' .
            ', excludehidden: %s' .
            ', firstmatch: %s' .
            ', in_archiveextensions: %s' .
            ', in_archivefilepatterns: %s' .
            ', in_dirpatterns: %s' .
            ', in_extensions: %s' .
            ', in_filepatterns: %s' .
            ', in_filetypes: %s' .
            ', in_linesafterpatterns: %s' .
            ', in_linesbeforepatterns: %s' .
            ', linesafter: %d' .
            ', linesaftertopatterns: %s' .
            ', linesafteruntilpatterns: %s' .
            ', linesbefore: %d' .
            ', listdirs: %s' .
            ', listfiles: %s' .
            ', listlines: %s' .
            ', maxlinelength: %d' .
            ', multilinesearch: %s' .
            ', out_archiveextensions: %s' .
            ', out_archivefilepatterns: %s' .
            ', out_dirpatterns: %s' .
            ', out_extensions: %s' .
            ', out_filepatterns: %s' .
            ', out_filetypes: %s' .
            ', out_linesafterpatterns: %s' .
            ', out_linesbeforepatterns: %s' .
            ', paths: %s' .
            ', printresults: %s' .
            ', printusage: %s' .
            ', printversion: %s' .
            ', recursive: %s' .
            ', searcharchives: %s' .
            ', searchpatterns: %s' .
            ', textfileencoding: "%s"' .
            ', uniquelines: %s' .
            ', verbose: %s' .
            ')',
            StringUtil::bool_to_string($this->archivesonly),
            StringUtil::bool_to_string($this->colorize),
            StringUtil::bool_to_string($this->debug),
            StringUtil::bool_to_string($this->excludehidden),
            StringUtil::bool_to_string($this->firstmatch),
            StringUtil::string_array_to_string($this->in_archiveextensions),
            StringUtil::string_array_to_string($this->in_archivefilepatterns),
            StringUtil::string_array_to_string($this->in_dirpatterns),
            StringUtil::string_array_to_string($this->in_extensions),
            StringUtil::string_array_to_string($this->in_filepatterns),
            StringUtil::string_array_to_string($this->in_filetypes),
            StringUtil::string_array_to_string($this->in_linesafterpatterns),
            StringUtil::string_array_to_string($this->in_linesbeforepatterns),
            $this->linesafter,
            StringUtil::string_array_to_string($this->linesaftertopatterns),
            StringUtil::string_array_to_string($this->linesafteruntilpatterns),
            $this->linesbefore,
            StringUtil::bool_to_string($this->listdirs),
            StringUtil::bool_to_string($this->listfiles),
            StringUtil::bool_to_string($this->listlines),
            $this->maxlinelength,
            StringUtil::bool_to_string($this->multilinesearch),
            StringUtil::string_array_to_string($this->out_archiveextensions),
            StringUtil::string_array_to_string($this->out_archivefilepatterns),
            StringUtil::string_array_to_string($this->out_dirpatterns),
            StringUtil::string_array_to_string($this->out_extensions),
            StringUtil::string_array_to_string($this->out_filepatterns),
            StringUtil::string_array_to_string($this->out_filetypes),
            StringUtil::string_array_to_string($this->out_linesafterpatterns),
            StringUtil::string_array_to_string($this->out_linesbeforepatterns),
            StringUtil::string_array_to_string($this->paths),
            StringUtil::bool_to_string($this->printresults),
            StringUtil::bool_to_string($this->printusage),
            StringUtil::bool_to_string($this->printversion),
            StringUtil::bool_to_string($this->recursive),
            StringUtil::bool_to_string($this->searcharchives),
            StringUtil::string_array_to_string($this->searchpatterns),
            $this->textfileencoding,
            StringUtil::bool_to_string($this->uniquelines),
            StringUtil::bool_to_string($this->verbose)
        );
    }
}
