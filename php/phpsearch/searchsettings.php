<?php
/**
 * Class SearchSettings
 *
 * @property bool archivesonly
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
 * @property bool printresults
 * @property bool printusage
 * @property bool printversion
 * @property bool recursive
 * @property bool searcharchives
 * @property array searchpatterns
 * @property string startpath
 * @property string textfileencoding
 * @property bool uniquelines
 * @property bool verbose
 */
class SearchSettings {
    public $archivesonly = false;
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
    public $startpath = NULL;
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
    public $searchpatterns = array();

    public function add_exts($ext, &$exts) {
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

    public function add_filetypes($filetype, &$filetypes) {
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

    public function add_patterns($pattern, &$patterns) {
        if (gettype($pattern) == 'string') {
            $patterns[] = $pattern;
        } elseif (gettype($pattern) == 'array') {
            foreach ($pattern as $p) {
                $patterns[] = $p;
            }
        }
    }

    public function set_archivesonly(bool $b) {
        $this->archivesonly = $b;
        if ($b) {
            $this->searcharchives = $b;
        }
    }

    public function set_debug(bool $b) {
        $this->debug = $b;
        if ($b) {
            $this->verbose = $b;
        }
    }

    private function arr_to_string(array $arr): string {
        $s = '["' . implode('","', $arr) . '"]';
        return $s;
    }

    private function bool_to_string(bool $b): string {
        return $b ? 'true' : 'false';
    }

    public function __toString(): string {
        $s = 'SearchSettings(';
        $s .= 'archivesonly: ' . $this->bool_to_string($this->archivesonly);
        $s .= ', debug: ' . $this->bool_to_string($this->debug);
        $s .= ', excludehidden: ' . $this->bool_to_string($this->excludehidden);
        $s .= ', firstmatch: ' . $this->bool_to_string($this->firstmatch);
        $s .= ', in_archiveextensions: ' . $this->arr_to_string($this->in_archiveextensions);
        $s .= ', in_archivefilepatterns: ' . $this->arr_to_string($this->in_archivefilepatterns);
        $s .= ', in_dirpatterns: ' . $this->arr_to_string($this->in_dirpatterns);
        $s .= ', in_extensions: ' . $this->arr_to_string($this->in_extensions);
        $s .= ', in_filepatterns: ' . $this->arr_to_string($this->in_filepatterns);
        $s .= ', in_filetypes: ' . $this->arr_to_string($this->in_filetypes);
        $s .= ', in_linesafterpatterns: ' . $this->arr_to_string($this->in_linesafterpatterns);
        $s .= ', in_linesbeforepatterns: ' . $this->arr_to_string($this->in_linesbeforepatterns);
        $s .= ', linesafter: ' . $this->linesafter;
        $s .= ', linesaftertopatterns: ' . $this->arr_to_string($this->linesaftertopatterns);
        $s .= ', linesafteruntilpatterns: ' . $this->arr_to_string($this->linesafteruntilpatterns);
        $s .= ', linesbefore: ' . $this->linesbefore;
        $s .= ', listdirs: ' . $this->bool_to_string($this->listdirs);
        $s .= ', listfiles: ' . $this->bool_to_string($this->listfiles);
        $s .= ', listlines: ' . $this->bool_to_string($this->listlines);
        $s .= ', maxlinelength: ' . $this->maxlinelength;
        $s .= ', multilinesearch: ' . $this->bool_to_string($this->multilinesearch);
        $s .= ', out_archiveextensions: ' . $this->arr_to_string($this->out_archiveextensions);
        $s .= ', out_archivefilepatterns: ' . $this->arr_to_string($this->out_archivefilepatterns);
        $s .= ', out_dirpatterns: ' . $this->arr_to_string($this->out_dirpatterns);
        $s .= ', out_extensions: ' . $this->arr_to_string($this->out_extensions);
        $s .= ', out_filepatterns: ' . $this->arr_to_string($this->out_filepatterns);
        $s .= ', out_filetypes: ' . $this->arr_to_string($this->out_filetypes);
        $s .= ', out_linesafterpatterns: ' . $this->arr_to_string($this->out_linesafterpatterns);
        $s .= ', out_linesbeforepatterns: ' . $this->arr_to_string($this->out_linesbeforepatterns);
        $s .= ', printresults: ' . $this->bool_to_string($this->printresults);
        $s .= ', printusage: ' . $this->bool_to_string($this->printusage);
        $s .= ', printversion: ' . $this->bool_to_string($this->printversion);
        $s .= ', recursive: ' . $this->bool_to_string($this->recursive);
        $s .= ', searcharchives: ' . $this->bool_to_string($this->searcharchives);
        $s .= ', searchpatterns: ' . $this->arr_to_string($this->searchpatterns);
        $s .= ', startpath: "' . $this->startpath . '"';
        $s .= ', textfileencoding: "' . $this->textfileencoding . '"';
        $s .= ', uniquelines: ' . $this->bool_to_string($this->uniquelines);
        $s .= ', verbose: ' . $this->bool_to_string($this->verbose);
        $s .= ')';
        return $s;
    }
}

?>
