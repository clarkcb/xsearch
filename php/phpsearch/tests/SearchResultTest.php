<?php declare(strict_types=1);
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

class SearchResultTest extends TestCase
{
    const CSSEARCHPATH = '~/src/xsearch/csharp/CsSearch/CsSearch';
    public function test_singleline_searchresult()
    {
        $settings = new SearchSettings();
        $settings->colorize = false;
        $formatter = new SearchResultFormatter($settings);
        $pattern = "Search";
        $file = new SearchFile(self::CSSEARCHPATH, "Searcher.cs", FileType::Code);
        $linenum = 10;
        $match_start_index = 15;
        $match_end_index = 23;
        $line = "\tpublic class Searcher\n";
        $linesbefore = [];
        $linesafter = [];
        $searchresult = new SearchResult(
            $pattern,
            $file,
            $linenum,
            $match_start_index,
            $match_end_index,
            $line,
            $linesbefore,
            $linesafter
        );
        $expectedoutput = sprintf(
            "%s: %d: [%d:%d]: %s",
            $file,
            $linenum,
            $match_start_index,
            $match_end_index,
            trim($line)
        );
        $output = $formatter->format($searchresult);
        $this->assertEquals($expectedoutput, $output);
    }

    public function test_singleline_longer_than_maxlength_searchresult()
    {
        $settings = new SearchSettings();
        $settings->colorize = false;
        $settings->maxlinelength = 100;
        $formatter = new SearchResultFormatter($settings);
        $pattern = "maxlen";
        $file = new SearchFile('.', "maxlen.txt", FileType::Text);
        $linenum = 1;
        $match_start_index = 53;
        $match_end_index = 59;
        $line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
        $linesbefore = [];
        $linesafter = [];
        $searchresult = new SearchResult(
            $pattern,
            $file,
            $linenum,
            $match_start_index,
            $match_end_index,
            $line,
            $linesbefore,
            $linesafter
        );
        $expectedline = '...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901...';
        $expectedoutput = sprintf(
            "%s: %d: [%d:%d]: %s",
            $file,
            $linenum,
            $match_start_index,
            $match_end_index,
            $expectedline
        );
        $output = $formatter->format($searchresult);
        $this->assertEquals($expectedoutput, $output);
    }

    public function test_binaryfile_searchresult()
    {
        $settings = new SearchSettings();
        $formatter = new SearchResultFormatter($settings);
        $pattern = "Search";
        $file = new SearchFile(self::CSSEARCHPATH, "Searcher.exe", FileType::Binary);
        $linenum = 0;
        $match_start_index = 0;
        $match_end_index = 0;
        $line = null;
        $linesbefore = [];
        $linesafter = [];
        $searchresult = new SearchResult(
            $pattern,
            $file,
            $linenum,
            $match_start_index,
            $match_end_index,
            $line,
            $linesbefore,
            $linesafter
        );
        $expectedoutput = sprintf("%s matches at [%d:%d]", $file, $match_start_index, $match_end_index);
        $output = $formatter->format($searchresult);
        $this->assertEquals($expectedoutput, $output);
    }

    public function test_multiline_searchresult()
    {
        $settings = new SearchSettings();
        $settings->colorize = false;
        $formatter = new SearchResultFormatter($settings);
        $pattern = "Search";
        $file = new SearchFile(self::CSSEARCHPATH, "Searcher.cs", FileType::Code);
        $linenum = 10;
        $match_start_index = 15;
        $match_end_index = 23;
        $line = "\tpublic class Searcher\n";
        $linesbefore = ["namespace CsSearch\n", "{\n"];
        $linesafter = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"];
        $searchresult = new SearchResult(
            $pattern,
            $file,
            $linenum,
            $match_start_index,
            $match_end_index,
            $line,
            $linesbefore,
            $linesafter
        );
        $outputtemplate =
            "================================================================================\n" .
            "%s: %d: [%d:%d]\n" .
            "--------------------------------------------------------------------------------\n" .
            "   8 | namespace CsSearch\n" .
            "   9 | {\n" .
            "> 10 | \tpublic class Searcher\n" .
            "  11 | \t{\n" .
            "  12 | \t\tprivate readonly FileTypes _fileTypes;\n";
        $expectedoutput = sprintf(
            $outputtemplate,
            $file,
            $linenum,
            $match_start_index,
            $match_end_index
        );
        $output = $formatter->format($searchresult);
        $this->assertEquals($expectedoutput, $output);
    }
}
