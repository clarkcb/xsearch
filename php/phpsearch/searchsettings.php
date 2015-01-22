<?php

class SearchSettings {
    public $archivesonly = false;
    public $debug = false;
    public $dotiming = false;
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
    public $uniquelines = false;
    public $verbose = false;

    public $in_archiveextensions = array();
    public $in_archivefilepatterns = array();
    public $in_dirpatterns = array();
    public $in_extensions = array();
    public $in_filepatterns = array();
    public $in_linesafterpatterns = array();
    public $in_linesbeforepatterns = array();
    public $linesaftertopatterns = array();
    public $linesafteruntilpatterns = array();
    public $out_archiveextensions = array();
    public $out_archivefilepatterns = array();
    public $out_dirpatterns = array();
    public $out_extensions = array();
    public $out_filepatterns = array();
    public $out_linesafterpatterns = array();
    public $out_linesbeforepatterns = array();
    public $searchpatterns = array();

    public function add_exts($ext, &$exts) {
        $xs = explode(',', $ext);
        foreach ($xs as $x) {
            $exts[] = $x;
        }
    }

    private function arr_to_string($arr) {
        $s = '["' . implode('","', $arr) . '"]';
        return $s;
    }

    private function bool_to_string($b) {
        return $b ? 'true' : 'false';
    }

    public function __toString() {
        $s = "SearchSettings(startpath: \"{$this->startpath}\"";
        $s .= ', archivesonly: ' . $this->bool_to_string($this->archivesonly);
        $s .= ', debug: ' . $this->bool_to_string($this->debug);
        $s .= ', dotiming: ' . $this->bool_to_string($this->dotiming);
        $s .= ', firstmatch: ' . $this->bool_to_string($this->firstmatch);
        $s .= ', excludehidden: ' . $this->bool_to_string($this->excludehidden);
        $s .= ', in_archiveextensions: ' . $this->arr_to_string($this->in_archiveextensions);
        $s .= ', in_archivefilepatterns: ' . $this->arr_to_string($this->in_archivefilepatterns);
        $s .= ', in_dirpatterns: ' . $this->arr_to_string($this->in_dirpatterns);
        $s .= ', in_extensions: ' . $this->arr_to_string($this->in_extensions);
        $s .= ', in_filepatterns: ' . $this->arr_to_string($this->in_filepatterns);
        $s .= ', in_linesafterpatterns: ' . $this->arr_to_string($this->in_linesafterpatterns);
        $s .= ', in_linesbeforepatterns: ' . $this->arr_to_string($this->in_linesbeforepatterns);
        $s .= ', linesafter: ' . $this->linesafter;
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
        $s .= ', printresults: ' . $this->bool_to_string($this->printresults);
        $s .= ', printusage: ' . $this->bool_to_string($this->printusage);
        $s .= ', printversion: ' . $this->bool_to_string($this->printversion);
        $s .= ', recursive: ' . $this->bool_to_string($this->recursive);
        $s .= ', searcharchives: ' . $this->bool_to_string($this->searcharchives);
        $s .= ', searchpatterns: ' . $this->arr_to_string($this->searchpatterns);
        $s .= ', uniquelines: ' . $this->bool_to_string($this->uniquelines);
        $s .= ', verbose: ' . $this->bool_to_string($this->verbose);
        $s .= ')';
        return $s;
    }
}

?>
